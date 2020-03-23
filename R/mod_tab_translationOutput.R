#' @title mod_tab_translateOutput and mod_tab_translation
#'
#' @description A shiny module to translate tab titles
#'
#' @param id shiny id
#'
#' @export
mod_tab_translateOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::uiOutput(ns("tab_title_translated"))
}

#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#' @param var_thes,numerical_thes,texts_thes thesauruses
#'
#' @rdname mod_tab_translateOutput
#'
#' @export
mod_tab_translate <- function(
  input, output, session,
  tab_title, lang, texts_thes
) {
  output$tab_title_translated <- shiny::renderUI({
    translated_title <- text_translate(tab_title, lang(), texts_thes)
    shiny::tagList(translated_title)
  })
}
