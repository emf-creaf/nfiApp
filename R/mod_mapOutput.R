#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#' @param nfidb pool object to access the database
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('map_container'))
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,viz_reactives,main_data_reactives reactives
#' @param nfidb database connection object
#' @param lang lang selected
#' @param var_thes,texts_thes,numerical_thes thesauruses
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, viz_reactives, main_data_reactives,
  nfidb, lang,
  var_thes, texts_thes, numerical_thes
) {

  ## renderUI ####
  output$map_container <- shiny::renderUI({

    # ns
    ns <- session$ns
    shiny::tagList(
      leaflet::leafletOutput(ns('nfi_map'), height = 600),
      shiny::tags$div(
        id = 'cite',
        text_translate('cite_div', lang, texts_thes)
      )
    )
  }) # end of renderUI

  ## leaflet output (empty map) ####
  output$nfi_map <- leaflet::renderLeaflet({

    # we need data, and we need color var at least
    leaflet::leaflet() %>%
      leaflet::setView(1.1, 41.70, zoom = 8) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief, group = 'Relief'
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery, group = 'Imaginery'
      ) %>%
      leaflet::addMapPane('admin_divs', zIndex = 410) %>%
      leaflet::addMapPane('plots', zIndex = 420) %>%
      leaflet::addLayersControl(
        baseGroups = c('Relief', 'Imaginery'),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      # leaflet.extras plugins
      leaflet.extras::addDrawToolbar(
        targetGroup = 'custom_polygon',
        position = 'topleft',
        polylineOptions = FALSE, circleOptions = FALSE,
        rectangleOptions = FALSE, markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = leaflet.extras::drawPolygonOptions(
          shapeOptions = leaflet.extras::drawShapeOptions()
        ),
        editOptions = leaflet.extras::editToolbarOptions(
          edit = TRUE, remove = TRUE
        ),
        singleFeature = TRUE
      )
  }) # end of leaflet output (empty map)

  ## reactives ####
  # aesthetics builder
  # The idea is to abstract the needed objects to create the polygons,
  # as all we need is common to points and polygons. The logic is as follows:
  #   - get the data and the viz inputs
  #   - if summary is on, color is a composition of color and statistic
  #   - dont forget to check if the data has the viz color (useful when
  #     changing data origin)
  #   - pull the color vector, build the pal function
  #   - depending on class of vector, a legend class
  # We also get the data and polygons labels
  aesthetics_builder <- shiny::reactive({
    # browser()
    # inputs
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)
    dominant_criteria <- shiny::isolate(data_reactives$dominant_criteria)
    dominant_nfi <- shiny::isolate(data_reactives$dominant_nfi)
    dominant_group <- shiny::isolate(data_reactives$dominant_group)
    viz_color <- viz_reactives$viz_color
    viz_size <- viz_reactives$viz_size
    viz_statistic <- viz_reactives$viz_statistic
    viz_pal_config <- viz_reactives$viz_pal_config
    viz_pal_reverse <- viz_reactives$viz_pal_reverse
    viz_diamclass <- viz_reactives$viz_diamclass
    viz_functional_group_value <- viz_reactives$viz_functional_group_value
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    admin_div <- shiny::isolate(data_reactives$admin_div)

    # polygon labels, join vars for data and join data expression for data
    if (admin_div %in% c('file', 'drawn_poly')) {
      polygon_label <- as.formula('~poly_id')
      polygon_join_var <- 'poly_id'
      polygon_join_data_expr <- rlang::expr(main_data_reactives$custom_polygon)
    } else {
      polygon_label <- as.formula(glue::glue("~admin_{admin_div}"))
      polygon_join_var <- glue::glue("admin_{admin_div}")
      polygon_join_data_expr <- rlang::expr(
        !! rlang::sym(glue::glue("{admin_div}_polygons"))
      )
    }

    # validate color
    shiny::validate(
      shiny::need(viz_color, 'no color yet')

    )
    # fillColor default
    fill_color <- '#6C7A8900'

    # desglossament or dominancy filters values
    fg_var <- NULL
    dc_var <- NULL
    filter_expressions <- rlang::exprs()

    if (desglossament %in% c(
      'species', 'simpspecies', 'genus', 'dec', 'bc'
    )) {
      fg_var <- glue::glue("{desglossament}_id")
    } else {
      if (isTRUE(group_by_dom)) {
        if (nfi %in% c('nfi_2_nfi_3', 'nfi_3_nfi_4')) {
          fg_var <- glue::glue(
            "{dominant_criteria}_{dominant_group}_dominant_{dominant_nfi}"
          )
        } else {
          fg_var <- glue::glue(
            "{dominant_criteria}_{dominant_group}_dominant"
          )
        }
      }
    }
    if (isTRUE(diameter_classes)) {
      dc_var <- 'diamclass_id'
    }

    # build filter expressions if any
    if (!is.null(fg_var)) {
      # we need to be sure that there is a value before building the expression
      shiny::validate(shiny::need(viz_functional_group_value, 'no fg yet'))
      fg_filter_expression <-
        rlang::expr(!!rlang::sym(fg_var) == !!viz_functional_group_value)
    } else {
      fg_filter_expression <- rlang::expr(TRUE)
    }

    if (!is.null(dc_var)) {
      dc_filter_expression <-
        rlang::expr(!!rlang::sym(dc_var) == !!viz_diamclass)
    } else {
      dc_filter_expression <- rlang::expr(TRUE)
    }

    # plot data
    plot_data <- NULL

    # summarised polygons?
    if (isTRUE(group_by_div)) {
      # color
      viz_color <- glue::glue("{viz_color}{viz_statistic}")
      fill_color <- rlang::expr(
        aesthetics_data$pal(aesthetics_data$color_vector)
      )
      # data
      polygon_data <-  main_data_reactives$main_data$requested_data %>%
        # dplyr::as_tibble() %>%
        # dplyr::select(-geometry) %>%
        dplyr::left_join(
          rlang::eval_tidy(polygon_join_data_expr), by = polygon_join_var
        ) %>%
        sf::st_as_sf(sf_column_name = 'geometry') %>%
        dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      # validation
      shiny::validate(
        shiny::need(
          viz_color %in% names(polygon_data),
          text_translate('apply_warning', lang, texts_thes)
        )
      )
      # color vector
      color_vector <-
        polygon_data %>%
        dplyr::pull(!! rlang::sym(viz_color))
      # size vector
      size_vector <- NULL
    } else {
      # data, color is already set in this case
      polygon_data <- rlang::eval_tidy(
        rlang::sym(glue::glue("{admin_div}_polygons"))
      )
      # plot data
      if (isTRUE(group_by_dom)) {
        plot_data <-
          main_data_reactives$main_data$main_data %>%
          dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      } else {
        plot_data <-
          main_data_reactives$main_data$requested_data %>%
          dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      }
      # validation
      shiny::validate(
        shiny::need(
          viz_color %in% names(plot_data),
          text_translate('apply_warning', lang, texts_thes)
        ),
        shiny::need(nrow(plot_data) > 0, 'no plot data')
      )
      # color vector
      color_vector <-
        plot_data %>%
        dplyr::pull(!! rlang::sym(viz_color))
      # size vector
      if (is.null(viz_size) || rlang::is_empty(viz_size) || viz_size == '') {
        size_vector <- rep(750, nrow(plot_data))
      } else {
        size_vector_pre <- plot_data %>%
          dplyr::pull(!! rlang::sym(viz_size))

        if (is.numeric(size_vector_pre)) {
          size_vector <-
            ((size_vector_pre/max(size_vector_pre, na.rm = TRUE)) * 1500) + 750
        } else {
          size_vector <-
            ((as.numeric(as.factor(size_vector_pre)) /
                max(as.numeric(as.factor(size_vector_pre)), na.rm = TRUE))
             * 1500) + 750
        }
      }
      size_vector[is.na(color_vector)] <- 500
    }

    # we need to check if the color variable is in the data. When applying
    # without navigating to viz tab this can happen if the data has not the
    # old variables
    shiny::validate(shiny::need(nrow(polygon_data) > 0, 'no polygon data'))

    # palette and legend class
    if (is.numeric(color_vector)) {
      pal <- switch(
        viz_pal_config,
        "low" = leaflet::colorNumeric(
          scales::gradient_n_pal(
            viridis::plasma(9), c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
          ),
          color_vector, reverse = viz_pal_reverse, na.color = 'black'
        ),
        "high" = leaflet::colorNumeric(
          scales::gradient_n_pal(
            viridis::plasma(9), c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
          ),
          color_vector, reverse = viz_pal_reverse, na.color = 'black'
        ),
        "normal" = leaflet::colorNumeric(
          'plasma', color_vector, reverse = viz_pal_reverse, na.color = 'black'
        )
      )
      legend_class <- 'info legend na_out'
    } else {
      pal <- leaflet::colorFactor(
        'plasma', color_vector, reverse = viz_pal_reverse, na.color = 'black'
      )
      legend_class <- 'info legend'
    }

    return(list(
      # color
      color_vector = color_vector,
      size_vector = size_vector,
      pal = pal,
      legend_class = legend_class,
      viz_color = viz_color,
      fill_color = fill_color,
      polygon_label = polygon_label,
      polygon_join_var = polygon_join_var,
      polygon_data = polygon_data,
      plot_data = plot_data
    ))
  })

  ## observers
  # polygon drawing. The logic is as follows:
  #   - if summarised data, get the summarised data, join the polygon object
  #     and proxy update the map.
  #   - if not, get the polygon object and update the map
  shiny::observe({
    # validation
    shiny::validate(
      shiny::need(main_data_reactives$main_data, 'no data yet')
    )
    # inputs for translating and other stuff
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    admin_div <- shiny::isolate(data_reactives$admin_div)
    # are we drawing summary data?
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    # aesthetics (mainly for legend, but also for filling the polygons or plots)
    aesthetics_data <- aesthetics_builder()

    # update the map
    leaflet::leafletProxy('nfi_map') %>%
      leaflet::clearGroup('aut_community') %>%
      leaflet::clearGroup('province') %>%
      leaflet::clearGroup('vegueria') %>%
      leaflet::clearGroup('region') %>%
      leaflet::clearGroup('municipality') %>%
      leaflet::clearGroup('natural_interest_area') %>%
      leaflet::clearGroup('special_protection_natural_area') %>%
      leaflet::clearGroup('natura_network_2000') %>%
      leaflet::clearGroup('file') %>%
      leaflet::clearGroup('drawn_poly') %>%
      # leaflet::clearGroup('plots') %>%
      leaflet::addPolygons(
        data = aesthetics_data$polygon_data,
        group = admin_div,
        label = aesthetics_data$polygon_label,
        layerId = aesthetics_data$polygon_label,
        weight = 1, smoothFactor = 1,
        opacity = 1.0, fill = TRUE,
        color = '#6C7A89FF',
        fillColor = rlang::eval_tidy(aesthetics_data$fill_color),
        fillOpacity = 0.7,
        highlightOptions = leaflet::highlightOptions(
          color = "#CF000F", weight = 2,
          bringToFront = FALSE
        ),
        options = leaflet::pathOptions(
          pane = 'admin_divs'
        )
      ) %>% {
        # legend in summaries
        temp <- .
        if (any(group_by_div, group_by_dom)) {
          temp %>%
            leaflet::addLegend(
              position = 'bottomright', pal = aesthetics_data$pal,
              values = aesthetics_data$color_vector,
              title = names(
                translate_var(
                  aesthetics_data$viz_color,
                  tables_to_look_at, lang, var_thes, numerical_thes,
                  texts_thes, is_summary = TRUE, need_order = FALSE
                )
              ),
              layerId = 'color_legend', opacity = 1,
              na.label = '', className = aesthetics_data$legend_class
            )
        } else {
          temp
        }
      }
  }) # end of polygon observer

  # observer to plot plots
  shiny::observe({
    # validation
    shiny::validate(
      shiny::need(main_data_reactives$main_data, 'no data yet')
    )
    # inputs for translating and other stuff
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    admin_div <- shiny::isolate(data_reactives$admin_div)
    # are we drawing summary data?
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    # aesthetics (mainly for legend, but also for filling the polygons or plots)
    aesthetics_data <- aesthetics_builder()

    if (!isTRUE(group_by_div)) {
      # update the map
      leaflet::leafletProxy('nfi_map') %>%
        leaflet::clearGroup('plots') %>%
        leaflet::addCircles(
          data = aesthetics_data$plot_data,
          group = 'plots', label = ~plot_id, layerId = ~plot_id,
          stroke = FALSE, fillOpacity = 0.7,
          fillColor = aesthetics_data$pal(aesthetics_data$color_vector),
          radius = aesthetics_data$size_vector,
          options = leaflet::pathOptions(pane = 'plots')
        ) %>%
        leaflet::addLegend(
          position = 'bottomright', pal = aesthetics_data$pal,
          values = aesthetics_data$color_vector,
          title = names(
            translate_var(
              aesthetics_data$viz_color,
              tables_to_look_at, lang, var_thes, numerical_thes,
              texts_thes, is_summary = TRUE, need_order = FALSE
            )
          ),
          layerId = 'color_legend', opacity = 1,
          na.label = '', className = aesthetics_data$legend_class
        )
    } else {
      # update the map
      leaflet::leafletProxy('nfi_map') %>%
        leaflet::clearGroup('plots')
    }
  })
}
