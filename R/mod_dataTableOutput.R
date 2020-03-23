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
    shiny::uiOutput(ns('table_vars_selector_panel')),
    DT::DTOutput(ns('main_table'))
  )
}

#' mod_dataTable server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives reactives from other modules
#' @param var_thes,texts_thes,numerical_thes thesauruses
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_dataTableOutput
mod_dataTable <- function(
  input, output, session,
  main_data_reactives, data_reactives, viz_reactives,
  var_thes, texts_thes, numerical_thes, lang
) {
  ## renderUI for the input
  output$table_vars_selector_panel <- shiny::renderUI({

    shiny::validate(shiny::need(
      main_data_reactives$main_data, 'no data yet'
    ))

    ns <- session$ns

    # inputs needed to populate the input
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)
    dominant_group <- shiny::isolate(data_reactives$dominant_group)
    dominant_criteria <- shiny::isolate(data_reactives$dominant_criteria)
    dominant_nfi <- shiny::isolate(data_reactives$dominant_nfi)
    viz_color <- viz_reactives$viz_color
    viz_statistic <- viz_reactives$viz_statistic
    viz_size <- viz_reactives$viz_size


    # translate needed
    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    summary_on <- any(group_by_div, group_by_dom)

    # variables pre-selected
    admin_div_sel <- ''
    if (shiny::isolate(data_reactives$admin_div) %in% c('file', 'drawn_poly')) {
      admin_div_sel <- 'poly_id'
    } else {
      admin_div_sel <- glue::glue(
        "admin_{shiny::isolate(data_reactives$admin_div)}"
      )
    }

    if (desglossament %in% c(
      'species', 'simpspecies', 'genus', 'dec', 'bc'
    )) {
      fg_var <- glue::glue("{desglossament}_id")
    } else {
      if (isTRUE(shiny::isolate(group_by_dom))) {
        if (nfi %in% c('nfi_2_nfi_3', 'nfi_3_nfi_4')) {
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

    # validation
    shiny::validate(
      shiny::need(
        any(
          c(viz_color, glue::glue("{viz_color}{viz_statistic}")) %in%
            names(main_data_reactives$main_data$requested_data)
        ),
        text_translate('apply_warning', lang(), texts_thes)
      )
    )

    # choices
    col_vis_selector_choices <-
      names(main_data_reactives$main_data$requested_data) %>%
      magrittr::extract(
        stringr::str_detect(
          ., "^geometry$", negate = TRUE
        )
      ) %>%
      translate_var(
        tables_to_look_at, lang(), var_thes, numerical_thes,
        texts_thes, is_summary = summary_on, need_order = FALSE
      )
    # selected_choices
    col_vis_selected <- col_vis_selector_choices[
      col_vis_selector_choices %in% c(
        'plot_id',
        # admin
        admin_div_sel,
        # fg and dc id
        fg_var, "diamclass_id",
        # viz_color
        viz_color,
        glue::glue("{viz_color}{viz_statistic}"),
        viz_size,
        glue::glue("{viz_size}{viz_statistic}")
      )
    ]

    # tagList
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          # shiny::br(),
          shinyWidgets::pickerInput(
            ns('col_vis_selector'),
            label = text_translate('col_vis_selector_input', lang(), texts_thes),
            choices = col_vis_selector_choices,
            multiple = TRUE,
            selected = col_vis_selected,
            width = '90%',
            options = list(
              `actions-box` = FALSE,
              `deselect-all-text` = 'None selected...',
              `select-all-text` = 'All selected',
              `selected-text-format` = 'count',
              `count-selected-text` = "{0} variables selected (of {1})",
              `size` = 15,
              `max-options` = 50,
              `max-options-text` = 'Select limit reached (50)',
              `live-search` = TRUE,
              `tick-icon` = 'glyphicon-tree-deciduous'
            )
          )
        )
      )
    )
  })

  table_data <- shiny::reactive({

    main_data_reactives$main_data$requested_data %>%
      dplyr::as_tibble() %>% {
        temp <- .
        if ('geometry' %in% names(temp)) {
          dplyr::select(temp, -geometry)
        } else {
          temp
        }
      } %>%
      dplyr::select(dplyr::one_of(c(
        # inputs selected
        input$col_vis_selector
      ))) %>%
      dplyr::mutate_if(
        is.numeric, round, digits = 2
      )
  })

  output$main_table <- DT::renderDT({
    shiny::validate(
      shiny::need(main_data_reactives$main_data, 'no data yet'),
      shiny::need(input$col_vis_selector, 'no selected vars')
    )

    # inputs for translating
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    summary_on <- any(group_by_div, group_by_dom)

    # DT
    table_data() %>%
      DT::datatable(
        rownames = FALSE,
        colnames = names(
          translate_var(
            names(.), tables_to_look_at, lang(), var_thes, numerical_thes,
            texts_thes, is_summary = summary_on, need_order = FALSE
          )
        ),
        class = 'hover order-column stripe nowrap',
        filter = list(position = 'top', clear = FALSE, plain = FALSE),
        # extensions = 'Buttons',
        options = list(
          pageLength = 15,
          dom = 'tip',
          # buttons = I('colvis'),
          autoWidth = FALSE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-family': 'Montserrat'});",
            "$(this.api().table().body()).css({'font-family': 'Montserrat'});",
            "}"
          )
        )
      )
    })

  # reactives to return
  table_reactives <- shiny::reactiveValues()
  shiny::observe({
    table_reactives$table_data <- table_data()
  })
  return(table_reactives)

}
