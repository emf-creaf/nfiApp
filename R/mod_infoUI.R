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
      shiny::h4(shiny::textOutput(ns('plot_title'))),
      shiny::plotOutput(ns("info_plot"))
    ),
    shiny::fluidRow(
      shiny::br(),
      shiny::column(
        12, align = 'center',
        formattable::formattableOutput(ns('info_table'), width = "90%")
      )
    )
  )
}

#' mod_info server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param map_reactives,main_data_reactives,viz_reactives reactives
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang lang reactive
#'
#' @export
mod_info <- function(
  input, output, session,
  map_reactives, main_data_reactives, viz_reactives,
  var_thes, texts_thes, numerical_thes, lang
) {

  ns <- session$ns

  waiter_plot <- waiter::Waiter$new(
    id = ns('info_plot'),
    html = waiter::spin_timer(),
    color = "#1C1C20"
  )

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
      dplyr::select(tidyselect::any_of(c(
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
      ))) %>%
      dplyr::mutate_if(
        is.numeric, round, digits = 2,
      ) %>%
      tidyr::gather('Characteristics', 'Value') %>%
      dplyr::mutate(
        Characteristics = stringr::str_remove(
          Characteristics, '_mean$|_se$|_max$|_min$|_n$'
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
              "font-size" = "12pt", "font-weight" = "normal"
            )
          ),
          Value = formattable::formatter(
            "span", style = formattable::style(
              "font-family" = "Montserrat", color = "#c8cac8",
              "font-size" = "12pt", "font-weight" = "bold"
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
    summary_on <- any(
      map_reactives$aesthetics$group_by_div,
      map_reactives$aesthetics$group_by_dom
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
    #     filtering, expecpt for the unique case of character variables in
    #     viz_color, in that case is plot data
    #   - plot has to be built always the same, so maybe this involves changing
    #     variable names
    if (nfi_map_shape_click$group == 'plots') {
      data_for_plot <- map_reactives$aesthetics$plot_data %>%
        dplyr::as_tibble() %>%
        dplyr::rename(
          label_var = plot_id
        )
      label_var_chr <- 'plot_id'
    } else {
      if (isTRUE(map_reactives$aesthetics$group_by_div)) {
        data_for_plot <- map_reactives$aesthetics$polygon_data %>%
          dplyr::as_tibble() %>%
          dplyr::rename(
            label_var = !! rlang::sym(
              map_reactives$aesthetics$polygon_join_var
            )
          )
        label_var_chr <- map_reactives$aesthetics$polygon_join_var
      } else {

        data_for_plot <- {
          if (!is.numeric(map_reactives$aesthetics$color_vector)) {
            map_reactives$aesthetics$plot_data
          } else {
            main_data_reactives$main_data$general_summary
          }
        } %>%
          dplyr::as_tibble() %>%
          dplyr::rename(
            label_var = !! rlang::sym(
              map_reactives$aesthetics$polygon_join_var
            )
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
        label_var_chr <- map_reactives$aesthetics$polygon_join_var
      }
    }

    # browser()

    plot_data <- data_for_plot %>%
      # plot. Logic is as follows:
      #   - violin plots
      #   - if plots are clicked:
      #     + we need to compare the clicked plot with the other plots in the
      #       map.
      #     + Variable is viz_color or {viz_color}{viz_statistic}
      #     + Color is click id vs the other ones at plot_id
      #     + If more than 50 points, don't make interactive all, only clicked,
      #       and the 25 highest points and the 25 lowest points
      #   - if polygons are clicked:
      #     + we need to compare polygon clicked with the other polygons in the
      #       map
      #     + Variable is viz_color or {viz_color}{viz_statistic}
      #     + Color is click id vs the other ones at polygon_join_var
      #     + Always interactive
      dplyr::select(tidyselect::any_of(c(
        'label_var',
        map_reactives$aesthetics$polygon_join_var,
        map_reactives$aesthetics$viz_color,
        glue::glue(
          "{map_reactives$aesthetics$viz_color}",
          "{map_reactives$aesthetics$viz_statistic}"
        ),
        map_reactives$aesthetics$viz_size,
        glue::glue(
          "{map_reactives$aesthetics$viz_size}",
          "{map_reactives$aesthetics$viz_statistic}"
        )
      ))) %>% {
        if (map_reactives$aesthetics$viz_color %in% names(.)) {
          dplyr::rename(
            .,
            y_var = !! rlang::sym(map_reactives$aesthetics$viz_color)
          )
        } else {
          dplyr::rename(
            .,
            y_var = !! rlang::sym(
              glue::glue(
                "{map_reactives$aesthetics$viz_color}",
                "{map_reactives$aesthetics$viz_statistic}"
              )
            )
          )
        }
      } %>% {
        shiny::validate(
          shiny::need(
            nfi_map_shape_click$id %in% .[['label_var']], 'no data in clicked'
          ),
          shiny::need(
            nrow(.) > 3,
            text_translate('not_enough_info_plot_warning', lang(), texts_thes)
          )
        )
        .
      } %>% {
        temp_data <- .
        # if character
        if (!is.numeric(map_reactives$aesthetics$color_vector)) {
          # browser()
          # if click poly
          if (nfi_map_shape_click$group != 'plots') {
            temp_data <- temp_data %>%
              dplyr::filter(label_var == nfi_map_shape_click$id)
            palette_colors <- rep(
              '#448714', length(unique(temp_data[['y_var']]))
            ) %>%
              magrittr::set_names(
                stringr::str_sort(unique(temp_data[['y_var']]))
              )
          } else {
            green_value <- temp_data %>%
              dplyr::filter(label_var == nfi_map_shape_click$id) %>%
              dplyr::pull(y_var)
            palette_colors <- rep(
              '#647a8d', length(unique(temp_data[['y_var']]))
            ) %>%
              magrittr::set_names(
                stringr::str_sort(unique(temp_data[['y_var']]))
              )
            palette_colors[[green_value]] <- '#448714'
          }
          temp_plot <-
            temp_data %>%
            ggplot2::ggplot(
              ggplot2::aes(x = y_var, fill = y_var, colour = y_var)
            ) +
            ggplot2::geom_bar(show.legend = FALSE) +
            ggplot2::scale_fill_manual(values = palette_colors) +
            ggplot2::scale_colour_manual(values = palette_colors) +
            ggplot2::labs(
              x = '',
              y = text_translate('info_count', lang(), texts_thes)
            )
        } else {
          temp_plot <-
            temp_data %>%
            ggplot2::ggplot(ggplot2::aes(x = 0, y = y_var)) +
            ggplot2::geom_point(
              data = ~ dplyr::filter(.x, label_var != nfi_map_shape_click$id),
              colour = '#647a8d', size = 4, alpha = 0.5,
              position = ggplot2::position_jitter(
                width = .2, height = 0, seed = 25
              )
            ) +
            ggplot2::geom_violin(fill = 'transparent') +
            ggplot2::geom_point(
              data = ~ dplyr::filter(.x, label_var == nfi_map_shape_click$id),
              colour = '#448714', size = 6
            ) +
            ggplot2::scale_x_continuous(breaks = NULL) +
            ggplot2::labs(
              x = '',
              y = names(translate_var(
                map_reactives$aesthetics$viz_color,
                tables_to_look_at, lang(),
                var_thes, numerical_thes, texts_thes,
                summary_on, need_order = FALSE
              ))
            )
        }
        temp_plot  +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            text = ggplot2::element_text(size = 14, color = '#647a8d'),
            axis.text = ggplot2::element_text(color = '#647a8d'),
            strip.text = ggplot2::element_text(color = '#647a8d'),
            panel.background = ggplot2::element_rect(
              fill = '#c8cac8', colour = NA
            ),
            plot.background = ggplot2::element_rect(
              fill = '#c8cac8', colour = NA
            ),
            strip.background = ggplot2::element_rect(
              fill = '#c8cac8', colour = NA
            ),
            panel.grid = ggplot2::element_line(colour = '#647a8d'),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(
              size = ggplot2::rel(0.5), colour = '#647a8d'
            )
          )
        # return(temp_plot)
      }
    return(list(plot_data = plot_data, label_var_chr = label_var_chr))
  })

  ## outputs ####
  output$info_table <- formattable::renderFormattable({
    info_table_data()
  })

  output$info_plot <- shiny::renderPlot({
    waiter_plot$show()
    info_plot_data()[["plot_data"]]
  })

  output$plot_title <- shiny::renderText({

    tables_to_look_at <- c(
      main_table_to_look_at(
        map_reactives$aesthetics$nfi, map_reactives$aesthetics$desglossament,
        map_reactives$aesthetics$diameter_classes
      ),
      ancillary_tables_to_look_at(map_reactives$aesthetics$nfi)
    )
    summary_on <- any(
      map_reactives$aesthetics$group_by_div,
      map_reactives$aesthetics$group_by_dom
    )

    viz_color <- names(translate_var(
      map_reactives$aesthetics$viz_color,
      tables_to_look_at, lang(),
      var_thes, numerical_thes, texts_thes,
      summary_on, need_order = FALSE
    ))

    # numeric?
    if (is.numeric(map_reactives$aesthetics$color_vector)) {
      return(text_translate(
        glue::glue("{info_plot_data()[['label_var_chr']]}_info_plot_title"),
        lang(), texts_thes
      ))
    } else {
      # categorical
      # polys?
      if (info_plot_data()[['label_var_chr']] != 'plot_id') {
        glue::glue(
          text_translate("cat_admin_info_plot_title", lang(), texts_thes)
        )
        # glue::glue(
        #   "Distribution of plots in {map_reactives$nfi_map_shape_click$id}",
        #   " for {viz_color}"
        # )
      } else {
        # plots
        glue::glue(
          text_translate("cat_plot_info_plot_title", lang(), texts_thes)
        )
        # glue::glue(
        #   "Distribution of plots for {viz_color}"
        # )
      }
    }



  })

}
