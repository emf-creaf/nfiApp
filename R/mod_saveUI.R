#' @title mod_saveUI and mod_save
#'
#' @description module for creating the astounding viz when click
#'
#' @param id shiny id
#'
#' @export
mod_saveUI <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::uiOutput(ns("save_container"))
}

#' mod_save
#' @param input internal
#' @param output internal
#' @param session internal
#' @param map_reactives,table_reactives reactives needed
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang language selected
#'
#' @export
#'
#' @rdname mod_saveUI
mod_save <- function(
  input, output, session,
  map_reactives, table_reactives,
  var_thes, texts_thes, numerical_thes, lang
) {

  # renderUI ####
  output$save_container <- shiny::renderUI({
    ns <- session$ns
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6, align = 'center',
          shiny::downloadButton(
            ns('save_map_btn'),
            label = text_translate('save_map_btn', lang(), texts_thes)
          )
        ),
        shiny::column(
          6, align = 'center',
          shiny::downloadButton(
            ns('save_table_btn'),
            label = text_translate('save_table_btn', lang(), texts_thes)
          )
        )
      ), # end of buttons row
      shiny::fluidRow(
        shiny::column(
          6, offset = 6, align = 'center',
          shinyWidgets::prettyRadioButtons(
            ns('table_length_options'),
            label = text_translate('table_length_options_input', lang(), texts_thes),
            choices = c('visible', 'all') %>%
              magrittr::set_names(c(
                text_translate('visible', lang(), texts_thes),
                text_translate('all', lang(), texts_thes)
              )),
            status = 'success', fill = TRUE, shape = 'round'
          ),
          shinyWidgets::prettyRadioButtons(
            ns('table_output_options'),
            label = text_translate('table_output_options_input', lang(), texts_thes),
            choices = c('csv', 'xlsx') %>%
              magrittr::set_names(c(
                text_translate('csv', lang(), texts_thes),
                text_translate('xlsx', lang(), texts_thes)
              )),
            status = 'success', fill = TRUE, shape = 'round'
          )
        )
      )
    )
  }) # end of renderUI

  # download handlers ####
  # map
  output$save_map_btn <- shiny::downloadHandler(
    filename = function() {
      glue::glue("{stringr::str_remove_all(Sys.Date(), '-')}_nfi_map.gpkg")
    },
    content = function(filename) {
      if (isTRUE(map_reactives$aesthetics$group_by_div)) {
        sf::st_write(map_reactives$aesthetics$polygon_data, filename)
      } else {
        sf::st_write(map_reactives$aesthetics$plot_data, filename)
      }
    }
  )
  # table
  output$save_table_btn <- shiny::downloadHandler(
    filename = function() {
      glue::glue(
        "{stringr::str_remove_all(Sys.Date(), '-')}_nfi_data.",
        "{input$data_output_options}"
      )
    },
    content = function(filename) {
      browser()
      if (input$table_output_options == 'csv') {
        readr::write_csv(table_reactives$table_data, filename)
      } else {
        writexl::write_xlsx(table_reactives$table_data, filename)
      }
    }
  )
}
