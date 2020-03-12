#' @title mod_applyButtonInput and mod_applyButton
#'
#' @description A shiny module to create and populate the buttons inputs
#'
#' @param id shiny id
#'
#' @export
mod_applyButtonInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # tagList
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6, align = 'left',
        shiny::verbatimTextOutput(ns('filter_warning'))
      ),
      shiny::column(
        6, align = 'center',
        shiny::uiOutput(ns("apply_panel"))
      )
    )
  )


}

#' mod_applyButton
#' @param input internal
#' @param output internal
#' @param session internal
#' @param lang language selected
#' @param texts_thes thesauruses
#'
#' @export
#'
#' @rdname mod_applyButtonInput
mod_applyButton <- function(
  input, output, session,
  lang, texts_thes, var_thes, numerical_thes,
  data_reactives, filters_reactives
) {

  # filter_warning ####
  output$filter_warning <- shiny::renderPrint({

    shiny::validate(
      shiny::need(filters_reactives$filter_vars, '')
    )

    # tables to look at for translations
    nfi <- data_reactives$nfi
    desglossament <- data_reactives$desglossament
    diameter_classes <- data_reactives$diameter_classes

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    filter_vars <- filters_reactives$filter_vars
    glue::glue(
      "{text_translate('filter_warning', lang, texts_thes)} ",
      "{names(translate_var(
        filter_vars, tables_to_look_at, lang, var_thes, numerical_thes
      ))}"
    )
  })

  # renderUI ####
  output$apply_panel <- shiny::renderUI({
    ns <- session$ns

    # button
    shiny::fluidRow(
      shiny::actionButton(
        ns('apply'),
        text_translate('apply', lang, texts_thes),
        icon = shiny::icon('check-circle')
      )
    )
  })

  # return reactive ####
  apply_reactives <- shiny::reactiveValues()
  shiny::observe({
    apply_reactives$apply_button <- input$apply
  })
  return(apply_reactives)

}
