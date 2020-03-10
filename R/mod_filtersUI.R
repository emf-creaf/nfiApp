#' @title mod_filtersUI and mod_filters
#'
#' @description A shiny module to generate and process the filters
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_filtersUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_filters_container')
    )
  )
}

#' mod_filters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb object to access the nfi db
#' @param lang lang reactive value
#' @param data_reactives reactives from the dataInput module
#' @param var_thes,numerical_thes,texts_thes thesauruses
#'
#' @importFrom dplyr between
#'
#' @export
#'
#' @rdname mod_filtersUI
mod_filters <- function(
  input, output, session,
  nfidb, lang,
  data_reactives,
  var_thes, numerical_thes, texts_thes
) {

  # variables available ####
  variables_available <- shiny::reactive({
    nfi <- data_reactives$nfi
    desglossament <- data_reactives$desglossament
    diameter_classes <- data_reactives$diameter_classes

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    vars_overall <- var_thes %>%
      dplyr::filter(var_table %in% tables_to_look_at) %>%
      dplyr::pull(var_id) %>%
      unique()

    clim_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^clim_")
    ]

    plot_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^admin_|^feat_|^topo_")
    ]

    removed_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^old_|^coords_|^presence_|plot_id")
    ]

    res_vars <- vars_overall[
      !(vars_overall %in% c(clim_vars, plot_vars, removed_vars))
    ]

    return(list(
      res_vars = translate_var(
        res_vars, tables_to_look_at, lang, var_thes, numerical_thes
      ),
      clim_vars = translate_var(
        clim_vars, tables_to_look_at, lang, var_thes, numerical_thes
      ),
      plot_vars = translate_var(
        plot_vars, tables_to_look_at, lang, var_thes, numerical_thes
      )
    ))
  })

  # renderUI ####
  output$mod_filters_container <- shiny::renderUI({
    # ns
    ns <- session$ns

    fil_res_vars_choices <- variables_available()$res_vars
    fil_clim_vars_choices <- variables_available()$clim_vars
    fil_plot_vars_choices <- variables_available()$plot_vars

    # tagList
    shiny::tagList(
      # filter categories row
      shiny::fluidRow(
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_res_vars'),
            text_translate('fil_res_vars_input', lang, texts_thes),
            choices = fil_res_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        ),
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_clim_vars'),
            text_translate('fil_clim_vars_input', lang, texts_thes),
            choices = fil_clim_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        ),
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_plot_vars'),
            text_translate('fil_plot_vars_input', lang, texts_thes),
            choices = fil_plot_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        )
      ) # end of filter categories row
    ) # end of tagList
  })
}
