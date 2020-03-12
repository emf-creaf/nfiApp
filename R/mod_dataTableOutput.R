#' mod_dataTableOutput and mod_dataTable
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataTableOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    DT::DTOutput(ns('main_table'))
    # shiny::uiOutput(ns('mod_table_panel'))
  )
}

#' mod_dataTable server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives reactives from
#'   other modules
#' @param nfidb pool object to access nfi db
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_dataTableOutput
mod_dataTable <- function(
  input, output, session,
  main_data_reactives, data_reactives, #filter_reactives,
  nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  table_preparation <- shiny::reactive({
    # browser()

    shiny::validate(shiny::need(
      main_data_reactives$main_data, 'no data yet'
    ))

    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    summary_on <- FALSE
    if (any(group_by_div, group_by_dom)) {
      summary_on <- TRUE
    }

    main_data_reactives$main_data$requested_data %>%
      dplyr::as_tibble() %>%
      dplyr::select(-geometry) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = names(
          translate_var(
            names(.), tables_to_look_at, lang, var_thes, numerical_thes,
            texts_thes, is_summary = summary_on, need_order = FALSE
          )
        ),
        # colnames = names(var_names_input_builder(
        #   names(.), lang(), var_thes, texts_thes, tables_to_look_at(),
        #   numerical_thes, summ, ordered = FALSE
        # )),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        options = list(
          pageLength = 15,
          dom = 'tip',
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Montserrat'});",
            "}"
          )
        )
      )# %>%
    # DT::formatRound(
    #   columns = numeric_vars,
    #   digits = 2
    # )
  })

  output$main_table <- DT::renderDT({
      table_preparation()
    })

}
