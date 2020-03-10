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
  # renderUI ####
  output$mod_filters_container <- shiny::renderUI({
    # ns
    ns <- session$ns

    # tagList
    shiny::tagList(
      # filter categories row
      shiny::fluidRow(
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_res_vars'),
            text_translate('fil_res_vars_input', lang, texts_thes),
            choices = '',
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
              `select-all-text` = text_translate('select-all-text', lang, texts_thes),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
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
            choices = '',
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
              `select-all-text` = text_translate('select-all-text', lang, texts_thes),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
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
            choices = '',
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
              `select-all-text` = text_translate('select-all-text', lang, texts_thes),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate('count-selected-text-var', lang, texts_thes),
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
