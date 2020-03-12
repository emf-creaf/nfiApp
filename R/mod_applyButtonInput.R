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
    shiny::uiOutput(ns("apply_panel"))
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
mod_applyButton <- function(input, output, session, lang, texts_thes) {

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
