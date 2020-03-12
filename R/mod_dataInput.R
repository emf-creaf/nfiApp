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
    shiny::br(),
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
#' @param var_thes,numerical_thes,texts_thes thesauruses
#'
#' @export
mod_data <- function(
  input, output, session,
  nfidb, lang, var_thes, numerical_thes, texts_thes
) {

  # renderUI ####
  output$mod_data_container <- shiny::renderUI({

    ns <- session$ns

    ## preacalculated choices
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
      'natural_interest_area', 'special_protection_natural_area',
      'natura_network_2000', 'file'
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

    desglossament_choices <- c(
      'plot', 'species', 'simpspecies', 'genus', 'dec', 'bc'
    ) %>%
      magrittr::set_names(c(
        text_translate('fg_plot', lang, texts_thes),
        text_translate('fg_species', lang, texts_thes),
        text_translate('fg_simpspecies', lang, texts_thes),
        text_translate('fg_genus', lang, texts_thes),
        text_translate('fg_dec', lang, texts_thes),
        text_translate('fg_bc', lang, texts_thes)
      ))

    agrupament_choices <- c('none', 'polygon', 'dom') %>%
      magrittr::set_names(c(
        text_translate('none', lang, texts_thes),
        text_translate('polygon', lang, texts_thes),
        text_translate('dom', lang, texts_thes)
      ))

    dominant_group_choices <- c(
      # 'none',
      'species', 'simpspecies', 'genus', 'dec', 'bc'
    ) %>%
      magrittr::set_names(c(
        # text_translate('none', lang, texts_thes),
        text_translate('species', lang, texts_thes),
        text_translate('simpspecies', lang, texts_thes),
        text_translate('genus', lang, texts_thes),
        text_translate('dec', lang, texts_thes),
        text_translate('bc', lang, texts_thes)
      ))

    dominant_criteria_choices <- c('density', 'basal_area') %>%
      magrittr::set_names(c(
        text_translate('dominant_criteria_density', lang, texts_thes),
        text_translate('dominant_criteria_basal_area', lang, texts_thes)
      ))

    dominant_nfi_choices <- c('none', 'nfi2', 'nfi3', 'nfi4') %>%
      magrittr::set_names(c(
        text_translate('none', lang, texts_thes),
        text_translate('nfi2', lang, texts_thes),
        text_translate('nfi3', lang, texts_thes),
        text_translate('nfi4', lang, texts_thes)
      ))

    # tagList
    shiny::tagList(
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
      ), # data version and admin row
      # file selector row
      shinyjs::hidden(
        shiny::div(
          id = ns('file_upload_panel'),
          shiny::fluidRow(
            shiny::column(
              7, align = 'center',
              shiny::fileInput(
                ns('user_file_sel'),
                text_translate('user_file_sel_label', lang, texts_thes),
                accept = c('zip', 'gpkg'),
                buttonLabel = text_translate(
                  'user_file_sel_buttonLabel', lang, texts_thes
                ),
                placeholder = text_translate(
                  'user_file_sel_placeholder', lang, texts_thes
                )
              )
            ),
            shiny::column(
              5, align = 'center',
              shiny::p(text_translate('file_text', lang, texts_thes))
            )
          )
        )
      ), # end of file selector row
      # agrupament row
      shiny::h4(text_translate('h4_agrupament', lang, texts_thes)),
      shiny::fluidRow(
        shiny::column(
          width = 6, align = 'center',
          shinyWidgets::prettyCheckbox(
            ns('group_by_div'),
            text_translate('group_by_div_input', lang, texts_thes),
            status = 'success', shape = 'curve', fill = TRUE
          )

        ),
        shiny::column(
          width = 6, align = 'center',
          shinyWidgets::prettyCheckbox(
            ns('group_by_dom'),
            text_translate('group_by_dom_input', lang, texts_thes),
            status = 'success', shape = 'curve', fill = TRUE
          )

        )
      ), # end of agrupament row
      # dominant grouping row
      shinyjs::hidden(
        shiny::div(
          id = ns('dom_grouping_panel'),
          shiny::fluidRow(
            shiny::column(
              5, offset = 1,
              # inputs
              shinyWidgets::prettyRadioButtons(
                ns('dominant_group'), label = 'Dominant group to group by',
                choices = dominant_group_choices,
                selected = 'species',
                status = 'success',
                fill = TRUE,
                shape = 'round'
              )
            ),
            shiny::column(
              5, offset = 1,
              shinyWidgets::prettyRadioButtons(
                ns('dominant_criteria'),
                label = 'Dominant criteria to group by',
                choices = dominant_criteria_choices,
                selected = 'density',
                status = 'success',
                fill = TRUE,
                shape = 'round'
              ),
              shinyjs::hidden(
                shinyWidgets::prettyRadioButtons(
                  ns('dominant_nfi'),
                  label = 'Dominant NFI version',
                  choices = dominant_nfi_choices,
                  selected = 'none',
                  status = 'success',
                  fill = TRUE,
                  shape = 'round'
                )
              )
            )
          )
        )
      ), # end of dominant grouping row
      # desglossament row
      shiny::div(
        id = ns('desglossament_panel'),
        shiny::fluidRow(
          shiny::column(
            width = 6, align = 'left',
            shinyWidgets::pickerInput(
              ns('desglossament'),
              text_translate('desglossament_input', lang, texts_thes),
              choices = desglossament_choices,
              selected = 'plot', width = '100%'
            )
          ),
          shiny::column(
            width = 6, align = 'center',
            shinyWidgets::awesomeCheckbox(
              ns('diameter_classes'),
              label = text_translate(
                'diameter_classes_input', lang, texts_thes
              ),
              status = 'info'
            )
          )
        ) # end of desglossament row
      )
    ) # end of tagList
  }) # end of renderUI

  ## observers ####
  # observer to show the file upload panel if needed
  shiny::observe({

    shiny::validate(
      shiny::need(input$admin_div, 'no div')
    )
    admin_div <- input$admin_div

    if (admin_div == 'file') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
  })

  # observer to hide the dominant grouping option if the data don't fit to this
  # kind of grouping
  shiny::observe({
    shiny::validate(
      shiny::need(input$nfi, 'no data')
    )
    nfi <- input$nfi
    available <- c(
      # base_data
      'nfi_2', 'nfi_3', 'nfi_4',
      # comparisions
      'nfi_2_nfi_3', 'nfi_3_nfi_4'
    )

    if (nfi %in% available) {
      shinyjs::enable('group_by_dom')
      shinyjs::show('group_by_dom')
    } else {
      shinyjs::reset('group_by_dom')
      shinyjs::disable('group_by_dom')
      shinyjs::hide('group_by_dom')
    }
  })

  # observer to show the dominant grouping panel if needed
  shiny::observeEvent(
    eventExpr = input$group_by_dom,
    handlerExpr = {
      group_by_dom <- input$group_by_dom

      if (isTRUE(group_by_dom)) {
        shinyjs::enable('dominant_group')
        shinyjs::enable('dominant_criteria')
        shinyjs::enable('dominant_nfi')
        shinyjs::show('dom_grouping_panel')
      } else {
        shinyjs::reset('dominant_group')
        shinyjs::reset('dominant_criteria')
        shinyjs::reset('dominant_nfi')
        shinyjs::disable('dominant_group')
        shinyjs::disable('dominant_criteria')
        shinyjs::disable('dominant_nfi')
        shinyjs::hide('dom_grouping_panel')
      }
    }
  )

  # observer to show the dominant nfi version if needed
  shiny::observe({
    shiny::validate(
      shiny::need(input$nfi, 'no data')
    )
    nfi <- input$nfi

    if (nfi %in% c('nfi_2_nfi_3', 'nfi_3_nfi_4')) {
      # show only relevant options for the comparision selected
      if (nfi == 'nfi_2_nfi_3') {
        dominant_nfi_choices <- c('nfi2', 'nfi3') %>%
          magrittr::set_names(c(
            text_translate('nfi2', lang, texts_thes),
            text_translate('nfi3', lang, texts_thes)
          ))
      } else {
        dominant_nfi_choices <- c('nfi3', 'nfi4') %>%
          magrittr::set_names(c(
            text_translate('nfi3', lang, texts_thes),
            text_translate('nfi4', lang, texts_thes)
          ))
      }
      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        'dominant_nfi',
        label = 'Dominant NFI version',
        choices = dominant_nfi_choices,
        selected = dominant_nfi_choices[1],
        prettyOptions = list(
          status = 'success',
          fill = TRUE,
          shape = 'round'
        )
      )
      shinyjs::enable('dominant_nfi')
      shinyjs::show('dominant_nfi')
    } else {
      shinyjs::reset('dominant_nfi')
      shinyjs::disable('dominant_nfi')
      shinyjs::hide('dominant_nfi')
    }
  })

  # observer to limit the desglossament options when grouping by dominant
  shiny::observeEvent(
    eventExpr = input$group_by_dom,
    handlerExpr = {
      group_by_dom <- input$group_by_dom

      if (isTRUE(group_by_dom)) {
        shinyjs::reset('desglossament')
        shinyjs::reset('diameter_classes')
        # shinyjs::disable('desglossament')
        # shinyjs::disable('diameter_classes')
        shinyjs::hide('desglossament_panel')
      } else {
        # shinyjs::enable('desglossament')
        # shinyjs::enable('diameter_classes')
        shinyjs::show('desglossament_panel')
      }
    }
  )

  # observer to limit the desglossament options when querying the bush and regen
  # tables
  shiny::observe({
    shiny::validate(
      shiny::need(input$nfi, 'no data')
    )
    nfi <- input$nfi
    available <- c(
      # shrub
      'nfi_2_shrub', 'nfi_3_shrub', 'nfi_4_shrub',
      # regeneration
      'nfi_2_regen', 'nfi_3_regen', 'nfi_4_regen'
    )

    if (nfi %in% available) {

      desglossament_choices <- c('species') %>%
        magrittr::set_names(c(
          text_translate('fg_species', lang, texts_thes)
        ))
      shinyWidgets::updatePickerInput(
        session, 'desglossament',
        label = text_translate('desglossament_input', lang, texts_thes),
        choices = desglossament_choices, selected = desglossament_choices[1]
      )
    } else {

      desglossament_choices <- desglossament_choices <- c(
        'plot', 'species', 'simpspecies', 'genus', 'dec', 'bc'
      ) %>%
        magrittr::set_names(c(
          text_translate('fg_plot', lang, texts_thes),
          text_translate('fg_species', lang, texts_thes),
          text_translate('fg_simpspecies', lang, texts_thes),
          text_translate('fg_genus', lang, texts_thes),
          text_translate('fg_dec', lang, texts_thes),
          text_translate('fg_bc', lang, texts_thes)
        ))
      shinyWidgets::updatePickerInput(
        session, 'desglossament',
        label = text_translate('desglossament_input', lang, texts_thes),
        choices = desglossament_choices, selected = desglossament_choices[1]
      )
    }

  })

  ## returning inputs ####
  # reactive values to return and use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$nfi <- input$nfi
    data_reactives$admin_div <- input$admin_div
    data_reactives$desglossament <- input$desglossament
    data_reactives$diameter_classes <- input$diameter_classes
    data_reactives$user_file_sel <- input$user_file_sel
    data_reactives$group_by_div <- input$group_by_div
    data_reactives$group_by_dom <- input$group_by_dom
    data_reactives$dominant_group <- input$dominant_group
    data_reactives$dominant_criteria <- input$dominant_criteria
    data_reactives$dominant_nfi <- input$dominant_nfi
  })

  return(data_reactives)
}
