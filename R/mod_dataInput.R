#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb lfcdata::lfcNFI object to access the nfi db
#' @param lang lang reactive
#'
#' @export
mod_data <- function(
  input, output, session,
  nfidb, lang
) {

  # needed thesauruses
  var_thes <- nfidb$get_data('variables_thesaurus')
  numerical_thes <- nfidb$get_data('variables_numerical')
  texts_thes <- nfidb$get_data('texts_thesaurus')

  # renderUI
  output$mod_data_container <- shiny::renderUI({

    ns <- session$ns

    ## preacalculated choices ####
    nfi_choices <- c(
      # base_data
      'nfi_2', 'nfi_3', 'nfi_4',
      # comparisions
      'nfi_2_nfi_3', 'nfi_3_nfi_4',
      # shrub
      'nfi_2_shrub', 'nfi_3_shrub', 'nfi_4_shrub',
      # regeneration
      'nfi_2_regen', 'nfi_3_regen', 'nfi_4_regen'
    ) %>%
      magrittr::set_names(c(
        text_translate('nfi_2', lang, texts_thes),
        text_translate('nfi_3', lang, texts_thes),
        text_translate('nfi_4', lang, texts_thes),
        text_translate('nfi_2_nfi_3', lang, texts_thes),
        text_translate('nfi_3_nfi_4', lang, texts_thes),
        text_translate('nfi_2_shrub', lang, texts_thes),
        text_translate('nfi_3_shrub', lang, texts_thes),
        text_translate('nfi_4_shrub', lang, texts_thes),
        text_translate('nfi_2_regen', lang, texts_thes),
        text_translate('nfi_3_regen', lang, texts_thes),
        text_translate('nfi_4_regen', lang, texts_thes)
      ))

    admin_div_choices <- c(
      'aut_community', 'province', 'vegueria', 'region', 'municipality',
      'natural_interest_area', 'special_protection_natural_area', 'natura_network_2000',
      'file'
    ) %>%
      magrittr::set_names(c(
        text_translate('aut_community', lang, texts_thes),
        text_translate('province', lang, texts_thes),
        text_translate('vegueria', lang, texts_thes),
        text_translate('region', lang, texts_thes),
        text_translate('municipality', lang, texts_thes),
        text_translate('natural_interest_area', lang, texts_thes),
        text_translate('special_protection_natural_area', lang, texts_thes),
        text_translate('natura_network_2000', lang, texts_thes),
        text_translate('file', lang, texts_thes)
      ))

    # this is gonna be a tabsetPanel, for data selection, filtering and viz
    shiny::tabsetPanel(
      id = 'sidebar_tabset', type = 'pills',

      # data tab
      shiny::tabPanel(
        title = text_translate('h4_data_selection', lang, texts_thes),
        value = 'h4_data_selection',
        # data version and admin row
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              ns('nfi'),
              label = text_translate('data_version', lang, texts_thes),
              choices = nfi_choices,
              selected = 'nfi_4'
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              ns('admin_div'), text_translate('divisions', lang, texts_thes),
              admin_div_choices, selected = 'region'
            )
          )
        ) # data version and admin row
      ) # end of data tab
    )




  }) # end of renderUI

}
