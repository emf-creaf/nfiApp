#' @title mod_filtersUI and mod_filters
#'
#' @description A shiny module to generate and process the filters
#'
#' @param id shiny id
#' @param nfidb pool object to access the nfi db
#'
#' @export
mod_filtersUI <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns('variable_selector_panel')),
    shiny::uiOutput(ns('proper_filter_panel'))
  )
}

#' mod_filters server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param nfidb object to access the nfi db
#' @param lang lang reactive value
#' @param data_reactives reactives from the dataInput module
#' @param cache memoryCache to store the filters values
#' @param var_thes,numerical_thes,texts_thes,categorical_thes thesauruses
#'
#' @importFrom dplyr between
#'
#' @export
#'
#' @rdname mod_filtersUI
mod_filters <- function(
  input, output, session,
  nfidb, lang,
  data_reactives, cache,
  var_thes, numerical_thes, texts_thes, categorical_thes
) {

  # variables available ####
  variables_available <- shiny::reactive({
    nfi <- data_reactives$nfi
    desglossament <- data_reactives$desglossament
    diameter_classes <- data_reactives$diameter_classes

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    vars_overall <- var_thes %>%
      dplyr::filter(var_table %in% tables_to_look_at) %>%
      dplyr::pull(var_id) %>%
      unique()

    clim_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^clim_")
    ]

    plot_vars <- vars_overall[
      stringr::str_detect(vars_overall, "^admin_|^feat_|^topo_")
    ]

    # sampling times, we need to remove them
    plot_vars <- plot_vars[
      stringr::str_detect(plot_vars, "_date$|_time$", negate = TRUE)
    ]

    removed_vars <- vars_overall[
      stringr::str_detect(
        vars_overall, "^old_|^coords_|^presence_|plot_id|poly_id"
      )
    ]

    res_vars <- vars_overall[
      !(vars_overall %in% c(clim_vars, plot_vars, removed_vars))
    ]

    return(list(
      res_vars = translate_var(
        res_vars, tables_to_look_at, lang, var_thes, numerical_thes
      ),
      clim_vars = translate_var(
        clim_vars, tables_to_look_at, lang, var_thes, numerical_thes
      ),
      plot_vars = translate_var(
        plot_vars, tables_to_look_at, lang, var_thes, numerical_thes
      )
    ))
  })

  # variable_selector_panel ####
  output$variable_selector_panel <- shiny::renderUI({
    # ns
    ns <- session$ns
    # choices for the inputs
    fil_res_vars_choices <- variables_available()$res_vars
    fil_clim_vars_choices <- variables_available()$clim_vars
    fil_plot_vars_choices <- variables_available()$plot_vars
    # tagList
    shiny::tagList(
      # filter categories row
      shiny::fluidRow(
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_res_vars'),
            text_translate('fil_res_vars_input', lang, texts_thes),
            choices = fil_res_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        ),
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_clim_vars'),
            text_translate('fil_clim_vars_input', lang, texts_thes),
            choices = fil_clim_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        ),
        shiny::column(
          4,
          shinyWidgets::pickerInput(
            ns('fil_plot_vars'),
            text_translate('fil_plot_vars_input', lang, texts_thes),
            choices = fil_plot_vars_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = text_translate(
                'deselect-all-text', lang, texts_thes
              ),
              `select-all-text` = text_translate(
                'select-all-text', lang, texts_thes
              ),
              `selected-text-format` = 'count > 3',
              `count-selected-text` = text_translate(
                'count-selected-text-var', lang, texts_thes
              ),
              `size` = 10,
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        )
      ) # end of filter categories row
    ) # end of tagList
  }) # end of variable_selector_panel

  # lets collect the variables selected by the user
  variables_to_filter_by <- shiny::reactive({
    c(input$fil_res_vars, input$fil_clim_vars, input$fil_plot_vars)
  })

  # filter inputs builder ####
  # a reactive to get the build the inputs. The logic is as follows:
  #   - based on the variables selected by the user, we build the inputs in a
  #     loop, but...
  #   - if the variable was previously selected and set, we set the values
  #     recorded in the cache.
  #   - we only update the cache when retrieving the input values
  #   - the cache is in the nfi_app.R file to be able to use it for the same
  #     session (if it was in the module it will be created every time the
  #     module runs, I think)
  #   - for building the input, first we need to know if the variable is
  #     character, numeric, logical or date to know the correct input for
  #     each. This is with vars_thes. Second check the cache and set the values
  #     if found. After that, build the input. This is going to be done by a
  #     function in helpers.R
  filter_inputs_builder <- shiny::eventReactive(
    eventExpr = variables_to_filter_by(),
    valueExpr = {

      # tables to look at
      nfi <- data_reactives$nfi
      desglossament <- data_reactives$desglossament
      diameter_classes <- data_reactives$diameter_classes

      tables_to_look_at <- c(
        main_table_to_look_at(nfi, desglossament, diameter_classes),
        ancillary_tables_to_look_at(nfi)
      )
      # ns
      ns <- session$ns
      # variables
      filter_inputs <- variables_to_filter_by() %>%
        purrr::map(
          filter_inputs_builder_helper, tables = tables_to_look_at,
          var_thes = var_thes, texts_thes = texts_thes,
          numerical_thes = numerical_thes, categorical_thes = categorical_thes,
          lang = lang, ns = ns, cache = cache
        )
    }
  )

  # proper_filter_panel ####
  output$proper_filter_panel <- shiny::renderUI({
    ns <- session$ns
    # create the inputs for each variable selected
    vars_to_filter_by <- variables_to_filter_by()

    # tagList
    shiny::tagList(
      shiny::hr(),
      shiny::tags$strong(text_translate('filter_the_data', lang, texts_thes)),
      shiny::br(), shiny::br(),
      filter_inputs_builder()
    )
  }) # end of proper_filter_panel

  # cache updating does not work if is done in the on_the_fly_inputs reactive,
  # as cache does not change when input change. Lets do this with an observer
  shiny::observe({
    # browser()
    # validation
    shiny::validate(
      shiny::need(variables_to_filter_by(), 'no variables to filter')
    )
    # vars
    vars_to_filter_by <- shiny::isolate(variables_to_filter_by()) %>%
      magrittr::set_names(., .)
    # cache list
    cache_list <-
      vars_to_filter_by %>%
      purrr::map(~ cache$get(stringr::str_remove_all(., '_'), missing = NULL))
    # inputs values
    input_values <-
      vars_to_filter_by %>%
      purrr::map(~ input[[.]])

    vars_to_filter_by %>%
      purrr::walk(
        ~ {
          if (
            # input must be no null and if it is not, then it must be different
            # from cache
            !is.null(input_values[[.]]) &
            !identical(cache_list[[.]], input_values[[.]])
          ) {
            cache$set(stringr::str_remove_all(., '_'), input_values[[.]])
          }
        }
      )
  })

  # reactive to activate the filter expressions generation. The logic is as
  # follows:
  #   - Lets retrieve the input value. Also, if it exists, lets retrieve the
  #     cache value.
  #   - If they are identical, return the input value, not change the cache
  #   - If they are not identical, update the cache, return the input value
  # on_the_fly_inputs <- shiny::eventReactive(
  #   eventExpr = filter_inputs_builder(),
  #   valueExpr = {
  #
  #     retrieve_function <- function(.x, cache) {
  #
  #       browser()
  #       # input value
  #       input_value <- input[[.x]]
  #       # cache value, if exists
  #       if (cache$exists(stringr::str_remove_all(.x, '_'))) {
  #         cache_value <- cache$get(stringr::str_remove_all(.x, '_'))
  #       } else {
  #         cache_value <- NULL
  #       }
  #       # check if input is the same as cache
  #       if (identical(input_value, cache_value)) {
  #         return(input_value)
  #       } else {
  #         cache$set(stringr::str_remove_all(.x, '_'), input_value)
  #         return(input_value)
  #       }
  #     }
  #
  #     variables_to_filter_by() %>%
  #       purrr::map(
  #         retrieve_function, cache = cache
  #       )
  #
  #   }
  # ) # end of onthefly inputs
  #
  # # filter expressions builder ####
  # data_filter_expressions <- shiny::observe({
  #   foo <- on_the_fly_inputs()
  # })
}
