#' mod_vizInput and mod_viz
#'
#' @description A shiny module to generate and populate the visualization inputs
#'
#' @param id shiny id
#'
#' @export
mod_vizInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns('mod_viz_panel'))
  )
}

#' mod_viz server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,filter_reactives,main_data_reactives reactives from
#'   other modules
#' @param nfidb pool object to access nfi db
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  data_reactives, filter_reactives, main_data_reactives,
  nfidb, var_thes, texts_thes, numerical_thes, lang
) {

  # static UI, later we populate the inputs with updaters
  ## renderUI ####
  output$mod_viz_panel <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      # color & palette settings
      shiny::fluidRow(
        shiny::column(
          8,
          shinyWidgets::pickerInput(
            ns('viz_color'),
            text_translate('viz_color_input', lang, texts_thes),
            choices = 'density',
            options = list(
              `size` = 10,
              `live-search` = TRUE,
              `action-box` = FALSE
            )
          ),

          # size
          shinyjs::hidden(
            shinyWidgets::pickerInput(
              ns('viz_size'), text_translate('viz_size_input', lang, texts_thes),
              choices = '',
              options = list(
                `size` = 10,
                `live-search` = TRUE,
                `action-box` = FALSE
              )
            )
          ),

          # statistic
          # shinyjs::hidden(
          #   shinyWidgets::pickerInput(
          #     ns('viz_statistic'), text_translate('viz_statistic_input', lang, texts_thes),
          #     choices = statistic_choices
          #   )
          # ),
          shinyWidgets::pickerInput(
            ns('viz_statistic'), text_translate('viz_statistic_input', lang, texts_thes),
            choices = statistic_choices
          ),

          # functional group value
          shinyjs::hidden(
            shinyWidgets::pickerInput(
              ns('viz_functional_group_value'), '',
              choices = '',
              options = list(
                `size` = 10,
                `live-search` = TRUE,
                `action-box` = FALSE
              )
            )
          ),

          # diameter class to visualize
          shinyjs::hidden(
            shinyWidgets::pickerInput(
              ns('viz_diamclass'), text_translate('viz_diamclass_input', lang, texts_thes),
              choices = '10'
            )
          )
        ),
        shiny::column(
          4,
          # low, normal or high palette
          shinyWidgets::radioGroupButtons(
            ns('viz_pal_config'),
            text_translate('viz_pal_config_input', lang, texts_thes),, size = 'sm',
            choices = c('low', 'normal', 'high') %>%
              magrittr::set_names(c(
                text_translate('pal_low', lang, texts_thes),
                text_translate('pal_normal', lang, texts_thes),
                text_translate('pal_high', lang, texts_thes)
              )),
            selected = 'normal', direction = 'vertical', status = 'lfc_radiogroupbuttons'
          ),
          # reverse palette
          shinyWidgets::awesomeCheckbox(
            ns('viz_reverse_pal'),
            label = text_translate('viz_reverse_pal_input', lang, texts_thes),
            value = FALSE, status = 'info'
          )
        )
      )
    )
  })
}
