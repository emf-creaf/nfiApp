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
#' @param data_reactives,filter_reactives reactives from
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
  data_reactives, filter_reactives,
  nfidb, var_thes, texts_thes, numerical_thes, categorical_thes, lang
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

    color_choices <- vars_to_viz_by()
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
    size_choices <- c('', color_choices)
    diameter_classes_choices <- seq(10, 70, 5) %>% as.character()
    dc_filter_vals <- filter_reactives$otf_filter_inputs[['diamclass_id']]

    if (!is.null(dc_filter_vals)) {
      diameter_classes_choices <- diameter_classes_choices[
        diameter_classes_choices %in% dc_filter_vals
      ]
    }

    group_by_div <- data_reactives$group_by_div
    group_by_dom <- data_reactives$group_by_dom
    dominant_group <- data_reactives$dominant_group
    dominant_criteria <- data_reactives$dominant_criteria
    dominant_nfi <- data_reactives$dominant_nfi
    desglossament <- data_reactives$desglossament

    if (desglossament %in% c(
      'species', 'simpspecies', 'genus', 'dec', 'bc'
    )) {
      fg_var <- glue::glue("{desglossament}_id")
    } else {
      if (isTRUE(group_by_dom)) {
        if (data_reactives$nfi %in% c('nfi_2_nfi_3', 'nfi_3_nfi_4')) {
          fg_var <- glue::glue(
            "{dominant_criteria}_{dominant_group}_dominant_{dominant_nfi}"
          )
        } else {
          fg_var <- glue::glue(
            "{dominant_criteria}_{dominant_group}_dominant"
          )
        }
      } else {
        fg_var <- "density_species_dominant"
      }
    }

    fg_choices <-
      var_thes %>%
      dplyr::filter(
        var_id == fg_var,
        var_table %in% tables_to_look_at()
      ) %>%
      dplyr::select(var_id, var_table, var_type) %>%
      dplyr::distinct() %>%
      dplyr::left_join(categorical_thes, by = c('var_id', 'var_table')) %>%
      tidyr::unnest(cols = c(var_values)) %>%
      dplyr::pull(var_values) %>%
      stringr::str_sort()

    fg_filter_vals <- filter_reactives$otf_filter_inputs[[fg_var]]
    if (!is.null(fg_filter_vals)) {
      fg_choices <- fg_choices[fg_choices %in% fg_filter_vals]
    }


    # tagList ####
    shiny::tagList(
      # color & palette settings
      shiny::fluidRow(
        shiny::column(
          8,
          shinyWidgets::pickerInput(
            ns('viz_color'),
            text_translate('viz_color_input', lang, texts_thes),
            choices = color_choices %>%
              var_inputs_aggregator(lang, texts_thes),
            selected = selected_col,
            options = list(
              `size` = 10,
              `live-search` = TRUE,
              `action-box` = FALSE
            )
          ),

          # size and statistics
          if (any(group_by_div, group_by_dom)) {
            shiny::tagList(
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_size'), text_translate('viz_size_input', lang, texts_thes),
                  choices = size_choices %>%
                    var_inputs_aggregator(lang, texts_thes),
                  selected = '',
                  options = list(
                    `size` = 10,
                    `live-search` = TRUE,
                    `action-box` = FALSE
                  )
                )
              ),
              shinyWidgets::pickerInput(
                ns('viz_statistic'),
                text_translate('viz_statistic_input', lang, texts_thes),
                choices = statistic_choices
              )
            )
          } else {
            shiny::tagList(
              shinyWidgets::pickerInput(
                ns('viz_size'), text_translate('viz_size_input', lang, texts_thes),
                choices = size_choices %>%
                  var_inputs_aggregator(lang, texts_thes),
                selected = '',
                options = list(
                  `size` = 10,
                  `live-search` = TRUE,
                  `action-box` = FALSE
                )
              ),
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_statistic'),
                  text_translate('viz_statistic_input', lang, texts_thes),
                  choices = statistic_choices
                )
              )
            )
          },

          # functional group value
          {
            if (any(
              desglossament %in% c(
                'species', 'simpspecies', 'genus', 'dec', 'bc'
              ),
              isTRUE(group_by_dom)
            )) {
              shinyWidgets::pickerInput(
                ns('viz_functional_group_value'),
                text_translate('functional_group_viz_input', lang, texts_thes),
                choices = fg_choices,
                options = list(
                  `size` = 10,
                  `live-search` = TRUE,
                  `action-box` = FALSE
                )
              )
            } else {
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_functional_group_value'),
                  text_translate('functional_group_viz_input', lang, texts_thes),
                  choices = fg_choices,
                  options = list(
                    `size` = 10,
                    `live-search` = TRUE,
                    `action-box` = FALSE
                  )
                )
              )
            }
          },

          # diameter class to visualize
          {
            if (data_reactives$diameter_classes) {
              shinyWidgets::pickerInput(
                ns('viz_diamclass'), text_translate('viz_diamclass_input', lang, texts_thes),
                choices = diameter_classes_choices
              )
            } else {
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_diamclass'), text_translate('viz_diamclass_input', lang, texts_thes),
                  choices = diameter_classes_choices
                )
              )
            }
          }
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
            ns('viz_pal_reverse'),
            label = text_translate('viz_pal_reverse_input', lang, texts_thes),
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
    # group_by_div <- data_reactives$group_by_div
    # group_by_dom <- data_reactives$group_by_dom

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
  # size input updater
  # shiny::observe({
  #
  #   group_by_div <- data_reactives$group_by_div
  #   group_by_dom <- data_reactives$group_by_dom
  #   # the logic here is that if there is summary, hide this, if not show it and
  #   # update it
  #   if (any(group_by_div, group_by_dom)) {
  #     shinyjs::reset('viz_size')
  #     shinyjs::disable('viz_size')
  #     shinyjs::hide('viz_size')
  #   } else {
  #     # show and enable
  #     shinyjs::enable('viz_size')
  #     shinyjs::show('viz_size')
  #   }
  # }) # end of size updater

  # statistic input updater
  # shiny::observe({
  #   group_by_div <- data_reactives$group_by_div
  #   group_by_dom <- data_reactives$group_by_dom
  #   # the logic here is that if there is summary, show this, if not hide it
  #   if (any(group_by_div, group_by_dom)) {
  #     # show and enable
  #     shinyjs::enable('viz_statistic')
  #     shinyjs::show('viz_statistic')
  #   } else {
  #     shinyjs::reset('viz_statistic')
  #     shinyjs::disable('viz_statistic')
  #     shinyjs::hide('viz_statistic')
  #   }
  # }) # end of statistic updater

  # updaters for fg and dc are inside the renderUI to avoid conflicts and
  # circular dependencies

  # return the viz inputs
  viz_reactives <- shiny::reactiveValues()

  shiny::observe({
    viz_reactives$viz_color <- input$viz_color
    viz_reactives$viz_size <- input$viz_size
    viz_reactives$viz_statistic <- input$viz_statistic
    viz_reactives$viz_functional_group_value <- input$viz_functional_group_value
    viz_reactives$viz_diamclass <- input$viz_diamclass
    viz_reactives$viz_pal_config <- input$viz_pal_config
    viz_reactives$viz_pal_reverse <- input$viz_pal_reverse
  })

  return(viz_reactives)

}
