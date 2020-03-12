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

    ## precalculated choices
    statistic_choices <- c('_mean', '_se', '_min', '_max', '_n') %>%
      magrittr::set_names(c(
        text_translate('mean_stat', lang, texts_thes),
        text_translate('se_stat', lang, texts_thes),
        text_translate('min_stat', lang, texts_thes),
        text_translate('max_stat', lang, texts_thes),
        text_translate('n_stat', lang, texts_thes)
      ))

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

  # reactives ####
  # tables to look at, to avoid repeating the same over and over again
  tables_to_look_at <- shiny::reactive({
    shiny::validate(shiny::need(
      data_reactives$nfi, 'no inputs yet'
    ))

    nfi <- data_reactives$nfi
    desglossament <- data_reactives$desglossament
    diameter_classes <- data_reactives$diameter_classes
    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    # triggers
    group_by_div <- data_reactives$group_by_div
    group_by_dom <- data_reactives$group_by_dom

    return(tables_to_look_at)
  })
  # we need the vars in the data to be able to show the names in the color and size inputs
  vars_to_viz_by <- shiny::reactive({

    group_by_div <- data_reactives$group_by_div
    group_by_dom <- data_reactives$group_by_dom

    all_variables <- var_thes %>%
      dplyr::filter(
        var_table %in% tables_to_look_at(),
        stringr::str_detect(
          var_id, "^old_|^coords_|^presence_|plot_id|poly_id", negate = TRUE
        )
      ) %>%
      dplyr::pull(var_id) %>%
      translate_var(
        tables_to_look_at(), lang, var_thes, numerical_thes,
        texts_thes, need_order = TRUE
      )
    numeric_variables <- numerical_thes %>%
      dplyr::filter(
        var_id %in% all_variables, var_table %in% tables_to_look_at()
      ) %>%
      dplyr::pull(var_id) %>%
      translate_var(
        tables_to_look_at(), lang, var_thes, numerical_thes,
        texts_thes, need_order = TRUE
      )

    if (any(group_by_div, group_by_dom)) {
      return(numeric_variables)
    } else {
      return(all_variables)
    }
  })

  # observers ####
  # here we update the viz inputs based on the data available
  # color input updater
  shiny::observe({

    color_choices <- vars_to_viz_by()
    shiny::validate(
      shiny::need(color_choices, 'No variables to visualize')
    )

    # let's make density (or density_balance) the selected var
    if ('density' %in% color_choices) {
      selected_col <- 'density'
    } else {
      if ('density_balance' %in% color_choices) {
        selected_col <- 'density_balance'
      } else {
        if ('regeneration_small_trees' %in% color_choices) {
          selected_col <- 'regeneration_small_trees'
        } else {
          selected_col <- 'shrub_canopy_cover'
        }
      }
    }

    # browser()
    # update the pickerInput
    shinyWidgets::updatePickerInput(
      session, 'viz_color',
      choices = color_choices %>%
        var_inputs_aggregator(lang, texts_thes),
      label = text_translate('viz_color_input', lang, texts_thes),
      selected = selected_col
    )
  }) # end of color updater

  # size input updater
  shiny::observe({

    # browser()

    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)
    # the logic here is that if there is summary, hide this, if not show it and
    # update it
    if (any(group_by_div, group_by_dom)) {
      shinyjs::reset('viz_size')
      shinyjs::disable('viz_size')
      shinyjs::hide('viz_size')
    } else {
      size_choices <- vars_to_viz_by()
      # update the pickerInput
      shinyWidgets::updatePickerInput(
        session, 'viz_size',
        choices = c(
          '', size_choices %>% var_inputs_aggregator(lang, texts_thes)
        ),
        label = text_translate('viz_size_input', lang, texts_thes)
      )
      # show and enable
      # shinyjs::enable('viz_size')
      shinyjs::show('viz_size')
    }
  }) # end of size updater

}
