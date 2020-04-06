#' @title mod_helpUI and mod_help
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_helpUI <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("help_container"))
}

#' mod_help
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data_reactives,viz_reactives reactives needed
#' @param nfidb object to access the database, with describe_var method
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang language selected
#'
#' @export
#'
#' @rdname mod_helpUI
mod_help <- function(
  input, output, session,
  data_reactives, viz_reactives,
  nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  ## renderUI ####
  output$help_container <- shiny::renderUI({

    ns <- session$ns
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    # translate needed
    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    var_choices <- var_thes %>%
      dplyr::filter(var_table %in% tables_to_look_at) %>%
      dplyr::pull(var_id) %>%
      unique() %>%
      translate_var(
        tables_to_look_at, lang(),
        var_thes, numerical_thes, texts_thes,
        is_summary = FALSE, need_order = TRUE
      )

    selected_choice <- viz_reactives$viz_color

    # tagList
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          8, align = 'center',
          shiny::br(),
          shinyWidgets::pickerInput(
            ns('glossary_var'),
            text_translate('glossary_var_input', lang(), texts_thes),
            choices = var_choices, width = '100%',
            selected = selected_choice,
            options = list(
              `size` = 10,
              `live-search` = TRUE,
              `action-box` = FALSE
            )
          ),
          shiny::br(),
          shiny::textOutput(ns('var_description_panel')),
          shiny::textOutput(ns('var_units_panel'))
        ),
        shiny::column(
          4, align = 'center',
          shiny::br(),
          shiny::tags$a(
            text_translate('link_to_tutorials_text', lang(), texts_thes),
            href = "http://laboratoriforestal.creaf.uab.cat/tutorial/ifn_app/"
          )
        )
      )
    ) # end of tagList
  }) # end of renderUI

  output$var_description_panel <- shiny::renderText({
    shiny::validate(
      shiny::need(input$glossary_var, 'no var selected yet')
    )
    var_description <- var_thes %>%
      dplyr::filter(var_id == input$glossary_var) %>%
      dplyr::select(tidyselect::any_of(
        c(glue::glue("var_description_{lang()}"))
      )) %>%
      purrr::flatten_chr() %>%
      unique()
    c(
      text_translate("var_description_title", lang(), texts_thes),
      var_description
    )
  })

  output$var_units_panel <- shiny::renderText({
    shiny::validate(
      shiny::need(input$glossary_var, 'no var selected yet')
    )
    var_units <- numerical_thes %>%
      dplyr::filter(var_id == input$glossary_var) %>%
      dplyr::select(tidyselect::any_of(c(
        glue::glue("var_units")
      ))) %>%
      purrr::flatten_chr() %>%
      unique()
    if (length(var_units) < 1) {
      var_units <- '-'
    }

    c(
      text_translate("var_units_title", lang(), texts_thes), var_units
    )
  })

}
