#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data as tbl_sql
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives reactives from dataInput module
#' @param filters_reactives filter reactives
#' @param nfidb object to access the nfi db#'
#' @param lang lang selected
#' @param texts_thes thesaurus
#'
#' @import tidyNFI
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, filters_reactives, apply_reactives,
  nfidb, lang, texts_thes
) {

  # main data ####
  # we have all we need to retrieve the main data. The map will need other
  # transformations (summarising...), and we need inputs now is convoluted
  # to do (like draw polygons or file inputs). Let's retrieve the main data,
  # and lets delegate the data transformations to the places they are needed
  main_data <- shiny::eventReactive(
    eventExpr = apply_reactives$apply_button,
    valueExpr = {

      # set a progress
      progress <- shiny::Progress$new(session, min = 0, max = 100)
      on.exit(progress$close())
      progress$set(
        message = 'Calculation in progress',
        detail = 'This may take a while...'
      )

      # tables to look at
      nfi <- data_reactives$nfi
      desglossament <- data_reactives$desglossament
      diameter_classes <- data_reactives$diameter_classes
      admin_div <- glue::glue("admin_{data_reactives$admin_div}")

      tables_to_look_at <- c(
        main_table_to_look_at(nfi, desglossament, diameter_classes),
        ancillary_tables_to_look_at(nfi)
      )

      message(as.character(filters_reactives$filter_expressions))

      progress$set(value = 10)

      # get data, join it
      main_data_table <-
        tables_to_look_at %>%
        purrr::map(~ nfidb$get_data(., spatial = FALSE)) %>%
        purrr::walk(
          ~ progress$set(value = 45)
        ) %>%
        purrr::reduce(dplyr::left_join, by = c('plot_id')) %>%
        dplyr::filter(
          !!! filters_reactives$filter_expressions
        ) %>%
        dplyr::left_join(
          nfidb$get_data(tables_to_look_at[1], spatial = TRUE) %>%
            dplyr::select(plot_id, geometry),
          by = 'plot_id'
        ) %>%
        purrr::walk(
          ~ progress$set(value = 85)
        ) %>%
        sf::st_as_sf(sf_column_name = 'geometry')

      progress$set(value = 100)
      return(main_data_table)
    }
  )

  # reactive to return
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$main_data <- main_data()
  })
  return(main_data_reactives)
}
