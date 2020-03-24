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
        9, align = 'left',
        # data requested summary
        shiny::uiOutput(ns('data_requested_info'))
      ),
      shiny::column(
        3, align = 'center',
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
#' @param texts_thes,var_thes,numerical_thes thesauruses
#' @param data_reactives,filter_reactives reactives needed
#'
#' @export
#'
#' @rdname mod_applyButtonInput
mod_applyButton <- function(
  input, output, session,
  lang, texts_thes, var_thes, numerical_thes,
  data_reactives, filter_reactives
) {

  # data_requested_info ####
  output$data_requested_info <- shiny::renderUI({

    shiny::validate(
      shiny::need(data_reactives$nfi, 'no inputs yet')
    )

    # tables to look at for translations
    nfi <- data_reactives$nfi
    desglossament <- data_reactives$desglossament
    diameter_classes <- data_reactives$diameter_classes

    # grouping
    group_by_div <- data_reactives$group_by_div
    group_by_dom <- data_reactives$group_by_dom

    group_paragraph <- shiny::tagList(
      text_translate("dri_group_by_div", lang(), texts_thes),
      text_translate("dri_group_by_dom", lang(), texts_thes)
    )

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    # browser()

    filter_vars <- filter_reactives$filter_vars
    translated_filter_vars <-
      purrr::map(filter_vars, ~ names(translate_var(
        .x, tables_to_look_at, lang(),
        var_thes, numerical_thes, texts_thes
      )))
    any_empty <- translated_filter_vars %>%
      purrr::map_lgl(rlang::is_empty) %>%
      any()

    if (isTRUE(any_empty)) {
      translated_filter_vars <-
        text_translate("active_filters_warning", lang(), texts_thes)
    }

    shiny::tagList(
      shiny::h4(text_translate("dri_title", lang(), texts_thes)),
      # nfi info
      shiny::p(
        shiny::strong(text_translate("dri_nfi", lang(), texts_thes), ": "),
        text_translate(nfi, lang(), texts_thes)
      ),
      # agrupament info
      shiny::p(
        shiny::strong(
          text_translate("dri_agrupament", lang(), texts_thes), ": "
        ),
        {
          if (!any(group_by_dom, group_by_div)) {
            text_translate("dri_none", lang(), texts_thes)
          } else {
            group_paragraph[c(group_by_div, group_by_dom)]
          }
        }
      ),
      # desglossament
      shiny::p(
        shiny::strong(
          text_translate("dri_desglossament", lang(), texts_thes), ": "
        ),
        {
          if (desglossament == 'plot') {
            if (isTRUE(diameter_classes)) {
              text_translate("dri_diameter_classes", lang(), texts_thes)
            } else {
              text_translate("dri_none", lang(), texts_thes)
            }
          } else {
            if (isTRUE(diameter_classes)) {
              shiny::tagList(
                text_translate(desglossament, lang(), texts_thes),
                text_translate("dri_diameter_classes", lang(), texts_thes)
              )
            } else {
              text_translate(desglossament, lang(), texts_thes)
            }
          }
        }
      ),
      # filters
      shiny::p(
        shiny::strong(
          text_translate("dri_filters", lang(), texts_thes), ": "
        ),
        {
          if (is.null(filter_vars)) {
            text_translate("dri_none", lang(), texts_thes)
          } else {
            shiny::tagList(
              translated_filter_vars
            )
          }
        }
      )
    )
  })

  # renderUI ####
  output$apply_panel <- shiny::renderUI({
    ns <- session$ns

    # button
    shiny::fluidRow(
      shiny::actionButton(
        ns('apply'),
        text_translate('apply', lang(), texts_thes),
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
