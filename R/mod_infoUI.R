#' @title mod_infoUI and mod_info
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_infoUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # ui skeleton (rows)
  shiny::tagList(
    shiny::fluidRow(
      shiny::br(),
      shiny::plotOutput(ns("info_plot"))
    ),
    shiny::fluidRow(
      shiny::br(),
      formattable::formattableOutput(ns('info_table'), width = "75%")
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_reactives,main_data_reactives,viz_reactives reactives
#' @param nfidb object with the db connection
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang lang reactive
#'
#' @export
mod_info <- function(
  input, output, session,
  map_reactives, main_data_reactives, viz_reactives,
  nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  ## reactives ####
  # table reactive
  info_table_data <- shiny::reactive({
    # validation
    # shiny::req(map_reactives$nfi_map_shape_click)
    # tables to look, for translation
    tables_to_look_at <- c(
      main_table_to_look_at(
        map_reactives$aesthetics$nfi, map_reactives$aesthetics$desglossament,
        map_reactives$aesthetics$diameter_classes
      ),
      ancillary_tables_to_look_at(map_reactives$aesthetics$nfi)
    )
    # get the click
    nfi_map_shape_click <- map_reactives$nfi_map_shape_click

    # table data
    # logic is as follows:
    #   - no need of data_inputs, we have all in map_reactives$aesthetics
    #   - if group of click is plots, then table info with requested data,
    #     unless dom is TRUE, then is rawdata, filter by plot
    #   - if group is another one, then table info with general_summary filter
    #     by poly var
    #   - vars are always the same so all together, with one of and building
    #     the possible options
    table_data <- {
      if (nfi_map_shape_click$group == 'plots') {
        map_reactives$aesthetics$plot_data %>%
          dplyr::as_tibble() %>%
          dplyr::filter(plot_id == nfi_map_shape_click$id)
      } else {
        if (isTRUE(map_reactives$aesthetics$group_by_div)) {
          map_reactives$aesthetics$polygon_data %>%
            dplyr::as_tibble() %>%
            dplyr::filter(
              !! rlang::sym(map_reactives$aesthetics$polygon_join_var) ==
                nfi_map_shape_click$id
            )
        } else {
          main_data_reactives$main_data$general_summary %>%
            dplyr::filter(
              !! rlang::sym(map_reactives$aesthetics$polygon_join_var) ==
                nfi_map_shape_click$id
            ) %>% {
              temp <- .
              if (!is.null(map_reactives$aesthetics$fg_var)) {
                temp %>%
                  dplyr::filter(
                    !! rlang::sym(map_reactives$aesthetics$fg_var) ==
                      viz_reactives$viz_functional_group_value
                  )
              } else {
                temp
              }
            } %>% {
              temp <- .
              if (isTRUE(map_reactives$aesthetics$diameter_classes)) {
                temp %>%
                  dplyr::filter(diamclass_id == viz_reactives$viz_diamclass)
              } else {
                temp
              }
            }
        }
      }
    } %>%
      dplyr::select(dplyr::one_of(
        'plot_id',
        map_reactives$aesthetics$polygon_join_var,
        'diamclass_id',
        map_reactives$aesthetics$fg_var,
        map_reactives$aesthetics$viz_color,
        glue::glue(
          "{map_reactives$aesthetics$viz_color}",
          "{map_reactives$aesthetics$viz_statistic}"
        ),
        map_reactives$aesthetics$viz_size,
        glue::glue(
          "{map_reactives$aesthetics$viz_size}",
          "{map_reactives$aesthetics$viz_statistic}"
        ),
        'topo_altitude_asl', 'topo_fdm_slope_percentage',
        'topo_fdm_aspect_cardinal_8',
        'clim_tmean_year', 'clim_prec_year', 'clim_pet_year',
        'topo_altitude_asl_mean', 'topo_fdm_slope_percentage_mean',
        'clim_tmean_year_mean', 'clim_prec_year_mean', 'clim_pet_year_mean'
      )) %>%
      dplyr::mutate_if(
        is.numeric, round, digits = 2,
      ) %>%
      tidyr::gather('Characteristics', 'Value') %>%
      dplyr::mutate(
        Characteristics = stringr::str_remove(
          Characteristics, '_mean|_se|_max|_min|_n'
        ),
        Characteristics = names(translate_var(
          Characteristics, tables_to_look_at, lang(),
          var_thes, numerical_thes, texts_thes, need_order = FALSE
        ))
      ) %>%
      formattable::formattable(
        list(
          Characteristics = formattable::formatter(
            "span", style = formattable::style(
              "font-family" = "Montserrat", color = "#c8cac8",
              "font-size" = "14pt", "font-weight" = "normal"
            )
          ),
          Value = formattable::formatter(
            "span", style = formattable::style(
              "font-family" = "Montserrat", color = "#c8cac8",
              "font-size" = "16pt", "font-weight" = "bold"
            )
          )
        ),
        align = c('r', 'l'),
        table.attr = "class=\"table table-condensed lfc_formattable\""
      )

    return(table_data)

  }) # end of info table reactive

  # info plot reactive
  info_plot_data <- shiny::reactive({
    # tables to look, for translation
    tables_to_look_at <- c(
      main_table_to_look_at(
        map_reactives$aesthetics$nfi, map_reactives$aesthetics$desglossament,
        map_reactives$aesthetics$diameter_classes
      ),
      ancillary_tables_to_look_at(map_reactives$aesthetics$nfi)
    )
    # get the click
    nfi_map_shape_click <- map_reactives$nfi_map_shape_click

    # plot data
    # logic is as follows:
    #   - no need of data_inputs, we have all in map_reactives$aesthetics
    #   - if group of click is plots, then comparing clicked plot with other
    #     plots in map. That means requested data, no filtering, except if
    #     dom is TRUE that is raw data no filtering
    #   - if group is another one, then table info with general_summary no
    #     filtering
    #   - plot has to be built always the same, so maybe this involves changing
    #     variable names

  })

  ## outputs ####
  output$info_table <- formattable::renderFormattable({
    info_table_data()
  })

}
