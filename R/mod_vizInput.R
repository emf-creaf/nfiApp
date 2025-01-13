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
#' @param data_reactives,filter_reactives,main_data_reactives, reactives needed
#' @param var_thes,texts_thes,numerical_thes,categorical_thes thesauruses
#' @param lang lang value
#' @param cache cache_mem object to store the selected col
#'
#' @export
#'
#' @rdname mod_vizUI
mod_viz <- function(
  input, output, session,
  data_reactives, filter_reactives, main_data_reactives,
  var_thes, texts_thes, numerical_thes, categorical_thes,
  lang, cache
) {

  # static UI, later we populate the inputs with updaters
  ## renderUI ####
  output$mod_viz_panel <- shiny::renderUI({

    ns <- session$ns
    ## precalculated choices and selected col ####
    # The selected choice is always the one in the cache if it exists and is
    # available, if not the default one.
    # statistic
    statistic_choices <- c('_mean', '_se', '_min', '_max', '_n') |>
      purrr::set_names(c(
        text_translate('mean_stat', lang(), texts_thes),
        text_translate('se_stat', lang(), texts_thes),
        text_translate('min_stat', lang(), texts_thes),
        text_translate('max_stat', lang(), texts_thes),
        text_translate('n_stat', lang(), texts_thes)
      ))
    selected_statistic <-
      cache_selected_choice(statistic_choices, cache, 'selectedstatistic')

    # color
    color_choices <- vars_to_viz_by()
    if ('density' %in% color_choices) {
      default_color <- 'density'
    } else {
      if ('density_balance' %in% color_choices) {
        default_color <- 'density_balance'
      } else {
        if ('regeneration_small_trees' %in% color_choices) {
          default_color <- 'regeneration_small_trees'
        } else {
          default_color <- 'shrub_canopy_cover'
        }
      }
    }
    # lets make the selected var the cached one if exists in the new data,
    # and if not, use the default for each kind of data
    selected_color <- cache_selected_choice(
      color_choices, cache, 'selectedcol',
      default = default_color
    )

    # size
    size_choices <- c('', color_choices)
    selected_size <-
      cache_selected_choice(size_choices, cache, 'selectedsize')

    # diameter classes
    diameter_classes_choices <- seq(10, 70, 5) |> as.character()
    dc_filter_vals <- filter_reactives$otf_filter_inputs[['diamclass_id']]
    if (!is.null(dc_filter_vals)) {
      diameter_classes_choices <- diameter_classes_choices[
        diameter_classes_choices %in% dc_filter_vals
      ]
    }
    # update dc_choices with the dc_var in the data if this exists.
    if (!is.null(
      main_data_reactives[['main_data']][['main_data']][['diamclass_id']]
    )) {
      data_dc_choices <-
        main_data_reactives[['main_data']][['main_data']][['diamclass_id']]
      diameter_classes_choices <-
        diameter_classes_choices[diameter_classes_choices %in% data_dc_choices]
    }
    selected_dc <-
      cache_selected_choice(diameter_classes_choices, cache, 'selecteddc')

    # functional group
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
      var_thes |>
      dplyr::filter(
        var_id == fg_var,
        var_table %in% tables_to_look_at()
      ) |>
      dplyr::select(var_id, var_table, var_type) |>
      dplyr::distinct() |>
      dplyr::left_join(categorical_thes, by = c('var_id', 'var_table')) |>
      tidyr::unnest(cols = c(var_values)) |>
      dplyr::pull(var_values) |>
      stringr::str_sort()

    fg_filter_vals <- filter_reactives$otf_filter_inputs[[fg_var]]
    if (!is.null(fg_filter_vals)) {
      fg_choices <- fg_choices[fg_choices %in% fg_filter_vals]
    }
    # update fg_choices with the fg_var in the data if this exists.
    if (!is.null(main_data_reactives[['main_data']][['main_data']])) {
      data_fg_choices <-
        main_data_reactives[['main_data']][['main_data']][[fg_var]]

      fg_choices <- fg_choices[fg_choices %in% data_fg_choices]
    }
    selected_fg <-
      cache_selected_choice(fg_choices, cache, 'selectedfg')

    # palette
    selected_pal_config <- cache$get('selectedpalconfig', 'normal')
    selected_pal_reverse <- cache$get('selectedpalreverse', FALSE)

    # tagList ####
    shiny::tagList(
      # color & palette settings
      shiny::fluidRow(
        shiny::column(
          8,
          shinyWidgets::pickerInput(
            ns('viz_color'),
            text_translate('viz_color_input', lang(), texts_thes),
            choices = color_choices |>
              var_inputs_aggregator(lang(), texts_thes),
            selected = selected_color,
            options = shinyWidgets::pickerOptions(
              actionsBox = FALSE,
              noneSelectedText = text_translate(
                'deselect-all-text', lang(), texts_thes
              ),
              selectAllText = text_translate(
                'select-all-text', lang(), texts_thes
              ),
              selectedTextFormat =  'count',
              countSelectedText =  text_translate(
                'count-selected-text-value', lang(), texts_thes
              ),
              size = 10,
              liveSearch = TRUE,
              tickIcon = 'glyphicon-tree-deciduous'
            )
          ),

          # size and statistics
          if (any(group_by_div, group_by_dom)) {
            shiny::tagList(
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_size'),
                  text_translate('viz_size_input', lang(), texts_thes),
                  choices = size_choices |>
                    var_inputs_aggregator(lang(), texts_thes),
                  selected = selected_size,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = FALSE,
                    noneSelectedText = text_translate(
                      'deselect-all-text', lang(), texts_thes
                    ),
                    selectAllText =  text_translate(
                      'select-all-text', lang(), texts_thes
                    ),
                    selectedTextFormat =  'count',
                    countSelectedText =  text_translate(
                      'count-selected-text-value', lang(), texts_thes
                    ),
                    size = 10,
                    liveSearch = TRUE,
                    tickIcon = 'glyphicon-tree-deciduous'
                  )
                )
              ),
              shinyWidgets::pickerInput(
                ns('viz_statistic'),
                text_translate('viz_statistic_input', lang(), texts_thes),
                choices = statistic_choices,
                selected = selected_statistic,
                options = shinyWidgets::pickerOptions(
                  actionsBox = FALSE,
                  size = 10,
                  liveSearch = TRUE,
                  tickIcon = 'glyphicon-tree-deciduous'
                )
              )
            )
          } else {
            shiny::tagList(
              shinyWidgets::pickerInput(
                ns('viz_size'),
                text_translate('viz_size_input', lang(), texts_thes),
                choices = size_choices |>
                  var_inputs_aggregator(lang(), texts_thes),
                selected = selected_size,
                options = shinyWidgets::pickerOptions(
                  actionsBox = FALSE,
                  noneSelectedText = text_translate(
                    'deselect-all-text', lang(), texts_thes
                  ),
                  selectAllText =  text_translate(
                    'select-all-text', lang(), texts_thes
                  ),
                  selectedTextFormat =  'count',
                  countSelectedText =  text_translate(
                    'count-selected-text-value', lang(), texts_thes
                  ),
                  size = 10,
                  liveSearch = TRUE,
                  tickIcon = 'glyphicon-tree-deciduous'
                )
              ),
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_statistic'),
                  text_translate('viz_statistic_input', lang(), texts_thes),
                  choices = statistic_choices,
                  selected = selected_statistic,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    liveSearch = TRUE,
                    tickIcon = 'glyphicon-tree-deciduous'
                  )
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
                text_translate('functional_group_viz_input', lang(), texts_thes),
                choices = fg_choices,
                selected = selected_fg,
                options = shinyWidgets::pickerOptions(
                  actionsBox = FALSE,
                  size = 10,
                  liveSearch = TRUE,
                  tickIcon = 'glyphicon-tree-deciduous'
                )
              )
            } else {
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_functional_group_value'),
                  text_translate('functional_group_viz_input', lang(), texts_thes),
                  choices = fg_choices,
                  selected = selected_fg,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    liveSearch = TRUE,
                    tickIcon = 'glyphicon-tree-deciduous'
                  )
                )
              )
            }
          },

          # diameter class to visualize
          {
            if (data_reactives$diameter_classes) {
              shinyWidgets::pickerInput(
                ns('viz_diamclass'),
                text_translate('viz_diamclass_input', lang(), texts_thes),
                choices = diameter_classes_choices,
                selected = selected_dc,
                options = shinyWidgets::pickerOptions(
                  actionsBox = FALSE,
                  size = 10,
                  liveSearch = TRUE,
                  tickIcon = 'glyphicon-tree-deciduous'
                )
              )
            } else {
              shinyjs::hidden(
                shinyWidgets::pickerInput(
                  ns('viz_diamclass'),
                  text_translate('viz_diamclass_input', lang(), texts_thes),
                  choices = diameter_classes_choices,
                  selected = selected_dc,
                  options = shinyWidgets::pickerOptions(
                    actionsBox = FALSE,
                    size = 10,
                    liveSearch = TRUE,
                    tickIcon = 'glyphicon-tree-deciduous'
                  )
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
            text_translate('viz_pal_config_input', lang(), texts_thes),
            size = 'sm',
            choices = c('high', 'normal', 'low') |>
              purrr::set_names(c(
                text_translate('pal_high', lang(), texts_thes),
                text_translate('pal_normal', lang(), texts_thes),
                text_translate('pal_low', lang(), texts_thes)
              )),
            selected = selected_pal_config, direction = 'vertical',
            status = 'lfc_radiogroupbuttons'
          ),
          # reverse palette
          shinyWidgets::awesomeCheckbox(
            ns('viz_pal_reverse'),
            label = text_translate('viz_pal_reverse_input', lang(), texts_thes),
            value = selected_pal_reverse, status = 'info'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(8, shiny::p(text_translate('using_3d', lang(), texts_thes)))
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
  # we need the vars in the data to be able to show the names in the color and
  # size inputs
  vars_to_viz_by <- shiny::reactive({

    group_by_div <- data_reactives$group_by_div
    group_by_dom <- data_reactives$group_by_dom

    all_variables <- var_thes |>
      dplyr::filter(
        var_table %in% tables_to_look_at(),
        stringr::str_detect(
          var_id, "^old_|^coords_|^presence_|plot_id|poly_id", negate = TRUE
        )
      ) |>
      dplyr::pull(var_id) |>
      translate_var(
        tables_to_look_at(), lang(), var_thes, numerical_thes,
        texts_thes, need_order = TRUE
      )
    numeric_variables <- numerical_thes |>
      dplyr::filter(
        var_id %in% all_variables, var_table %in% tables_to_look_at()
      ) |>
      dplyr::pull(var_id) |>
      translate_var(
        tables_to_look_at(), lang(), var_thes, numerical_thes,
        texts_thes, need_order = TRUE
      )

    if (any(group_by_div, group_by_dom)) {
      return(numeric_variables)
    } else {
      return(all_variables)
    }
  })

  # observers ####
  # update cache
  shiny::observe({
    shiny::validate(shiny::need(input$viz_color, 'no input yet'))
    selected_color <- input$viz_color
    cache$set('selectedcol', selected_color)
  })
  shiny::observe({
    # here we can not validate, as the defaut is '' which shiny::need coniders
    # a failure, making the observer not launching
    # shiny::validate(shiny::need(input$viz_size, 'no_input_yet')) # not working
    if (!is.null(input$viz_size)) {
      selected_size <- input$viz_size
      cache$set('selectedsize', selected_size)
    }
  })
  shiny::observe({
    shiny::validate(shiny::need(input$viz_statistic, 'no_input_yet'))
    selected_statistic <- input$viz_statistic
    cache$set('selectedstatistic', selected_statistic)
  })
  shiny::observe({
    shiny::validate(
      shiny::need(input$viz_functional_group_value, 'no_input_yet')
    )
    selected_fg <- input$viz_functional_group_value
    cache$set('selectedfg', selected_fg)
  })
  shiny::observe({
    shiny::validate(shiny::need(input$viz_diamclass, 'no_input_yet'))
    selected_dc <- input$viz_diamclass
    cache$set('selecteddc', selected_dc)
  })
  shiny::observe({
    shiny::validate(shiny::need(input$viz_pal_config, 'no_input_yet'))
    selected_pal_config <- input$viz_pal_config
    cache$set('selectedpalconfig', selected_pal_config)
  })
  shiny::observe({
    shiny::validate(shiny::need(input$viz_pal_reverse, 'no_input_yet'))
    selected_pal_reverse <- input$viz_pal_reverse
    cache$set('selectedpalreverse', selected_pal_reverse)
  })

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
