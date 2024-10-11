#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    # leaflet::leafletOutput(ns("nfi_map"), height = 600),
    mapdeck::mapdeckOutput(ns("nfi_map"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,viz_reactives,main_data_reactives reactives
#' @param lang lang selected
#' @param var_thes,texts_thes,numerical_thes thesauruses
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, viz_reactives, main_data_reactives,
  lang, var_thes, texts_thes, numerical_thes
) {

  ## helper ####
  # leaflet_legend_helper <- function(map, group_by_div, group_by_dom, aesthetics_data, tables_to_look_at, lang_sel) {
  #   # legend in summaries
  #   if (!any(group_by_div, group_by_dom)) {
  #     return(map)
  #   }

  #   map |>
  #     leaflet::addLegend(
  #       position = 'bottomright', pal = aesthetics_data$pal_legend,
  #       values = aesthetics_data$color_vector_legend,
  #       title = names(
  #         translate_var(
  #           aesthetics_data$viz_color,
  #           tables_to_look_at, lang_sel, var_thes, numerical_thes,
  #           texts_thes, is_summary = TRUE, need_order = FALSE
  #         )
  #       ),
  #       layerId = 'color_legend', opacity = 1,
  #       na.label = '', className = aesthetics_data$legend_class,
  #       labFormat = leaflet::labelFormat(
  #         transform = function(x) {sort(x, decreasing = TRUE)}
  #       )
  #     )
  # }

  ## renderUI ####
  output$map_container <- shiny::renderUI({

    # ns
    ns <- session$ns
    # shiny::tagList(
    #   leaflet::leafletOutput(ns('nfi_map'), height = 600),
      shiny::tags$div(
        id = 'cite',
        text_translate('cite_div', lang(), texts_thes)
      )
  #   )
  }) # end of renderUI

  
  ## mapdeck output (empty map) ####
  output$nfi_map <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(
      # style = mapdeck::mapdeck_style('dark'),
      style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
      location = c(1.744, 41.726), zoom = 7, pitch = 0
    )
  }) # end of mapdeck output (empty map)
  
  # output$nfi_map <- leaflet::renderLeaflet({

  #   # we need data, and we need color var at least
  #   leaflet::leaflet() |>
  #     leaflet::setView(1.1, 41.70, zoom = 8) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldShadedRelief, group = 'Relief'
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldImagery, group = 'Imaginery'
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$OpenStreetMap, group = 'OSM'
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$Esri.WorldGrayCanvas, group = 'WorldGrayCanvas'
  #     ) |>
  #     leaflet::addProviderTiles(
  #       leaflet::providers$CartoDB.PositronNoLabels, group = 'PositronNoLabels'
  #     ) |>
  #     leaflet::addMapPane('admin_divs', zIndex = 410) |>
  #     leaflet::addMapPane('plots', zIndex = 420) |>
  #     leaflet::addLayersControl(
  #       baseGroups = c('Relief', 'Imaginery', 'OSM', 'WorldGrayCanvas', 'PositronNoLabels'),
  #       options = leaflet::layersControlOptions(collapsed = TRUE)
  #     ) |>
  #     # leaflet.extras plugins
  #     leaflet.extras::addDrawToolbar(
  #       targetGroup = 'drawn_poly',
  #       position = 'topleft',
  #       polylineOptions = FALSE, circleOptions = FALSE,
  #       rectangleOptions = FALSE, markerOptions = FALSE,
  #       circleMarkerOptions = FALSE,
  #       polygonOptions = leaflet.extras::drawPolygonOptions(
  #         shapeOptions = leaflet.extras::drawShapeOptions()
  #       ),
  #       editOptions = leaflet.extras::editToolbarOptions(
  #         edit = TRUE, remove = TRUE
  #       ),
  #       singleFeature = TRUE
  #     )
  # }) # end of mapdeck output (empty map)

  ## reactives ####
  # zoom-size transformation. Logic is as follows:
  #   - In closer zooms (10) go to the base size of 750. In far zooms increase
  #     accordingly, until zoom 7 and further, with a max size of 1500
  base_size <- shiny::reactive({
    # current_zoom <- input$nfi_map_zoom
    # if (current_zoom <= 7) {
    #   current_zoom <- 7
    # }
    # if (current_zoom >= 10) {
    #   current_zoom <- 10
    # }

    # size_transformed <- 750 + ((10 - current_zoom) * 250)

    # return(size_transformed)
    return(750)
  })

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

    shiny::validate(shiny::need(admin_div, 'no inputs yet'))

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )
    
    # polygon labels, join vars for data and join data expression for data
    if (admin_div %in% c('file', 'drawn_poly')) {
      polygon_label <- as.formula('~poly_id')
      polygon_join_var <- 'poly_id'
      polygon_join_data_expr <- rlang::expr(
        shiny::isolate(main_data_reactives$custom_polygon)
      )
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

      geometry_column <-
        attr(rlang::eval_tidy(polygon_join_data_expr), 'sf_column')
      # data
      polygon_data <-  main_data_reactives$main_data$requested_data |>
        dplyr::left_join(
          rlang::eval_tidy(polygon_join_data_expr), by = polygon_join_var
        ) |>
        sf::st_as_sf(sf_column_name = geometry_column) |>
        dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      # validation
      shiny::validate(
        shiny::need(
          viz_color %in% names(polygon_data),
          text_translate('apply_warning', lang(), texts_thes)
        )
      )
      # color vector
      color_vector <-
        polygon_data |>
        dplyr::pull(!! rlang::sym(viz_color))
      # size vector
      size_vector <- NULL
    } else {
      # data, color is already set in this case
      polygon_data <- rlang::eval_tidy(polygon_join_data_expr)
      # plot data
      if (isTRUE(group_by_dom)) {
        plot_data <-
          main_data_reactives$main_data$main_data |>
          dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      } else {
        plot_data <-
          main_data_reactives$main_data$requested_data |>
          dplyr::filter(!! fg_filter_expression, !! dc_filter_expression)
      }
      # validation
      shiny::validate(
        shiny::need(
          viz_color %in% names(plot_data),
          text_translate('apply_warning', lang(), texts_thes)
        ),
        shiny::need(nrow(plot_data) > 0, 'no plot data')
      )
      # color vector
      color_vector <-
        plot_data |>
        dplyr::pull(!! rlang::sym(viz_color))
      # size vector
      if (is.null(viz_size) || rlang::is_empty(viz_size) || viz_size == '') {
        size_vector <- rep(base_size(), nrow(plot_data))
      } else {
        shiny::validate(
          shiny::need(
            viz_size %in% names(plot_data),
            text_translate('apply_warning', lang(), texts_thes)
          )
        )
        size_vector_pre <- plot_data |>
          dplyr::pull(!! rlang::sym(viz_size))

        if (is.numeric(size_vector_pre)) {
          size_vector <- scales::rescale(size_vector_pre, c(750, 3000))
          # ((size_vector_pre/max(size_vector_pre, na.rm = TRUE)) * 1500) + base_size()
        } else {
          size_vector <- scales::rescale(as.numeric(as.factor(size_vector_pre)), c(750, 3000))
          # ((as.numeric(as.factor(size_vector_pre)) /
          #     max(as.numeric(as.factor(size_vector_pre)), na.rm = TRUE))
          #  * 1500) + base_size()
        }
      }
      size_vector[is.na(color_vector)] <- base_size()/2
    }

    # we need to check if the color variable is in the data. When applying
    # without navigating to viz tab this can happen if the data has not the
    # old variables
    shiny::validate(shiny::need(nrow(polygon_data) > 0, 'no polygon data'))

    # what happens with ES when there is only 1 value, is NA. And that creates
    # a problem with leaflet::colorNumeric, so we change is all is na to 0
    if (all(is.na(color_vector))) {
      color_vector[is.na(color_vector)] <- 0
    }

    # palette and legend class
    if (is.numeric(color_vector)) {

      # here we can have the possibility of one length numeric color vector
      # (when only one plot or polygon due to custom polygons or filters).
      # In that case, we need to add 5% above and below to build the palette
      # for the map and the legend
      if (length(color_vector) < 2) {
        color_vector_legend <- c(
          color_vector - (color_vector*0.05),
          color_vector,
          color_vector + (color_vector*0.05)
        )
      } else {
        color_vector_legend <- color_vector
      }

      pal <- switch(
        viz_pal_config,
        "low" = scales::col_numeric(
          scales::gradient_n_pal(
            hcl.colors(9, "ag_GrnYl", alpha = 0.8),
            c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
          ),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = viz_pal_reverse, alpha = TRUE
        ),
        "high" = scales::col_numeric(
          scales::gradient_n_pal(
            hcl.colors(9, "ag_GrnYl", alpha = 0.8),
            c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
          ),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = viz_pal_reverse, alpha = TRUE
        ),
        "normal" = scales::col_numeric(
          hcl.colors(256, "ag_GrnYl", alpha = 0.8),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = viz_pal_reverse, alpha = TRUE
        )
      )
      # as we need to reverse the legend values (low at bottom) we need an
      # alternative palette for the legend with the reversed values
      pal_legend <- switch(
        viz_pal_config,
        "low" = scales::col_numeric(
          scales::gradient_n_pal(
            hcl.colors(9, "ag_GrnYl", alpha = 0.8),
            c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.55, 1)
          ),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = !viz_pal_reverse, alpha = TRUE
        ),
        "high" = scales::col_numeric(
          scales::gradient_n_pal(
            hcl.colors(9, "ag_GrnYl", alpha = 0.8),
            c(0, 0.45, 0.65, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
          ),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = !viz_pal_reverse, alpha = TRUE
        ),
        "normal" = scales::col_numeric(
          hcl.colors(256, "ag_GrnYl", alpha = 0.8),
          c(min(color_vector_legend, na.rm = TRUE), max(color_vector_legend, na.rm = TRUE)),
          na.color = "#FFFFFF00", reverse = !viz_pal_reverse, alpha = TRUE
        )
      )
      legend_class <- 'info legend na_out'

      # map legend js for mapbox
      # custom legend (to be able to show in natural order, high values up)
      legend_js <- mapdeck::legend_element(
        variables = rev(round(seq(
          min(color_vector, na.rm = TRUE),
          max(color_vector, na.rm = TRUE),
          length.out = 10
        ), 3)),
        colours = pal_legend(seq(
          min(color_vector, na.rm = TRUE),
          max(color_vector, na.rm = TRUE),
          length.out = 10
        )),
        colour_type = "fill", variable_type = "gradient",
        title = translate_var(
          viz_color,
          tables_to_look_at, lang(), var_thes, numerical_thes,
          texts_thes, is_summary = TRUE, need_order = FALSE
        ) |> names()
      ) |>
        mapdeck::mapdeck_legend()
    } else {
      pal <- scales::col_factor(
        hcl.colors(256, "ag_GrnYl", alpha = 0.8), color_vector,
        na.color = "#FFFFFF00", reverse = viz_pal_reverse, alpha = TRUE
      )
      pal_legend <- scales::col_factor(
        hcl.colors(256, "ag_GrnYl", alpha = 0.8), color_vector,
        na.color = "#FFFFFF00", reverse = !viz_pal_reverse, alpha = TRUE
      )
      legend_class <- 'info legend'
      color_vector_legend <- color_vector
      # custom legend (to be able to show in natural order, high values up)
      legend_js <- mapdeck::legend_element(
        variables = rev(sort(unique(color_vector_legend))),
        colours = pal_legend(sort(unique(color_vector_legend))),
        colour_type = "fill", variable_type = "category",
        title = translate_var(
          viz_color,
          tables_to_look_at, lang(), var_thes, numerical_thes,
          texts_thes, is_summary = TRUE, need_order = FALSE
        ) |> names()
      ) |>
        mapdeck::mapdeck_legend()
    }

    return(list(
      # color
      color_vector = color_vector,
      color_vector_legend = color_vector_legend,
      size_vector = size_vector,
      pal = pal,
      pal_legend = pal_legend,
      legend_class = legend_class,
      viz_color = viz_color,
      viz_size = viz_size,
      viz_statistic = viz_statistic,
      fill_color = fill_color,
      polygon_label = polygon_label,
      polygon_join_var = polygon_join_var,
      polygon_data = polygon_data,
      plot_data = plot_data,
      fg_var = fg_var,
      nfi = nfi,
      desglossament = desglossament,
      diameter_classes = diameter_classes,
      group_by_dom = group_by_dom,
      group_by_div = group_by_div,
      legend_js = legend_js
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
    # aesthetics (mainly for legend, but also for filling the polygons or plots)
    aesthetics_data <- aesthetics_builder()
    # inputs for translating and other stuff
    lang_sel <- lang()
    admin_div <- shiny::isolate(data_reactives$admin_div)
    nfi <- aesthetics_data$nfi
    desglossament <- aesthetics_data$desglossament
    diameter_classes <- aesthetics_data$diameter_classes
    # are we drawing summary data?
    group_by_div <- aesthetics_data$group_by_div
    group_by_dom <- aesthetics_data$group_by_dom

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    if (isTRUE(group_by_div)) {
      map_data <- aesthetics_data$polygon_data |>
        dplyr::ungroup() |>
        dplyr::mutate(
          hex = rlang::eval_tidy(aesthetics_data$fill_color),
          tooltip = paste0(
            "<p>", .data[[labels(terms(aesthetics_data$polygon_label))]], ": ", round(.data[[aesthetics_data$viz_color]], 2), "</p>"
          )
        )

      mapdeck::mapdeck_update(map_id = session$ns("nfi_map")) |>
        mapdeck::clear_scatterplot(layer_id = "plots") |>
        mapdeck::clear_polygon(layer_id = "polys") |>
        mapdeck::add_polygon(
          data = map_data,
          fill_colour = "hex", fill_opacity = 0.8,
          id = labels(terms(aesthetics_data$polygon_label)), layer_id = "polys",
          update_view = FALSE, focus_layer = FALSE,
          tooltip = "tooltip",
          legend = aesthetics_data$legend_js
        )
      # update the map
      # leaflet::leafletProxy('nfi_map') |>
      #   leaflet::clearGroup('aut_community') |>
      #   leaflet::clearGroup('province') |>
      #   leaflet::clearGroup('vegueria') |>
      #   leaflet::clearGroup('region') |>
      #   leaflet::clearGroup('municipality') |>
      #   leaflet::clearGroup('natural_interest_area') |>
      #   leaflet::clearGroup('special_protection_natural_area') |>
      #   leaflet::clearGroup('natura_network_2000') |>
      #   leaflet::clearGroup('file') |>
      #   leaflet::clearGroup('drawn_poly') |>
      #   # leaflet::clearGroup('plots') |>
      #   leaflet::addPolygons(
      #     data = aesthetics_data$polygon_data,
      #     group = admin_div,
      #     label = aesthetics_data$polygon_label,
      #     layerId = aesthetics_data$polygon_label,
      #     weight = 1, smoothFactor = 1,
      #     opacity = 1.0, fill = TRUE,
      #     color = '#6C7A89FF',
      #     fillColor = rlang::eval_tidy(aesthetics_data$fill_color),
      #     fillOpacity = 0.7,
      #     highlightOptions = leaflet::highlightOptions(
      #       color = "#CF000F", weight = 2,
      #       bringToFront = FALSE
      #     ),
      #     options = leaflet::pathOptions(
      #       pane = 'admin_divs'
      #     )
      #   ) |>
      #   leaflet_legend_helper(group_by_div, group_by_dom, aesthetics_data, tables_to_look_at, lang_sel)
    } else {
      mapdeck::mapdeck_update(map_id = session$ns("nfi_map")) |>
        mapdeck::clear_scatterplot(layer_id = "polys")
    }
  }) # end of polygon observer

  # observer to plot plots
  shiny::observe({
    # validation
    shiny::validate(
      shiny::need(main_data_reactives$main_data, 'no data yet')
    )
    # aesthetics (mainly for legend, but also for filling the polygons or plots)
    aesthetics_data <- aesthetics_builder()
    # inputs for translating and other stuff
    lang_sel <- lang()
    admin_div <- shiny::isolate(data_reactives$admin_div)
    nfi <- aesthetics_data$nfi
    desglossament <- aesthetics_data$desglossament
    diameter_classes <- aesthetics_data$diameter_classes
    viz_size <- aesthetics_data$viz_size
    
    # are we drawing summary data?
    group_by_div <- aesthetics_data$group_by_div

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    if (!isTRUE(group_by_div)) {

      if (viz_size == "") {
        viz_size_tooltip <- paste0(
          "<p>", aesthetics_data$plot_data$plot_id, ":</p>",
          "<p>", translate_var(
            aesthetics_data$viz_color,
            tables_to_look_at, lang_sel, var_thes, numerical_thes,
            texts_thes, is_summary = TRUE, need_order = FALSE
          ) |> names(),
          " ", aesthetics_data$plot_data[[aesthetics_data$viz_color]], "</p>"
        )
      } else {
        viz_size_tooltip <- paste0(
          "<p>", aesthetics_data$plot_data$plot_id, ":</p>",
          "<p>", translate_var(
            aesthetics_data$viz_color,
            tables_to_look_at, lang_sel, var_thes, numerical_thes,
            texts_thes, is_summary = TRUE, need_order = FALSE
          ) |> names(),
          " ", aesthetics_data$plot_data[[aesthetics_data$viz_color]], "</p>",
          "<p>", translate_var(
            aesthetics_data$viz_size,
            tables_to_look_at, lang_sel, var_thes, numerical_thes,
            texts_thes, is_summary = TRUE, need_order = FALSE
          ) |> names(),
          " ", aesthetics_data$plot_data[[aesthetics_data$viz_size]], "</p>"
        )
      }

      map_data <- aesthetics_data$plot_data |>
        dplyr::mutate(
          hex = aesthetics_data$pal(aesthetics_data$color_vector),
          tooltip = viz_size_tooltip,
          radius = aesthetics_data$size_vector
        )

      mapdeck::mapdeck_update(map_id = session$ns("nfi_map")) |>
        mapdeck::clear_polygon(layer_id = "polys") |>
        mapdeck::clear_scatterplot(layer_id = "plots") |>
        mapdeck::add_scatterplot(
          data = map_data,
          fill_colour = "hex", fill_opacity = 0.8,
          stroke_colour = "hex", stroke_opacity = 0.8,
          id = "plot_id", layer_id = "plots",
          update_view = FALSE, focus_layer = FALSE,
          tooltip = "tooltip",
          legend = aesthetics_data$legend_js,
          radius = "radius"
        )

      # update the map
      # leaflet::leafletProxy('nfi_map') |>
      #   leaflet::clearGroup('plots') |>
      #   leaflet::addCircles(
      #     data = aesthetics_data$plot_data,
      #     group = 'plots', label = ~plot_id, layerId = ~plot_id,
      #     stroke = FALSE, fillOpacity = 0.7,
      #     fillColor = aesthetics_data$pal(aesthetics_data$color_vector),
      #     radius = aesthetics_data$size_vector,
      #     options = leaflet::pathOptions(pane = 'plots')
      #   ) |>
      #   leaflet::addLegend(
      #     position = 'bottomright', pal = aesthetics_data$pal_legend,
      #     values = aesthetics_data$color_vector_legend,
      #     title = names(
      #       translate_var(
      #         aesthetics_data$viz_color,
      #         tables_to_look_at, lang_sel, var_thes, numerical_thes,
      #         texts_thes, is_summary = TRUE, need_order = FALSE
      #       )
      #     ),
      #     layerId = 'color_legend', opacity = 1,
      #     na.label = '', className = aesthetics_data$legend_class,
      #     labFormat = leaflet::labelFormat(
      #       transform = function(x) {sort(x, decreasing = TRUE)}
      #     )
      #   )
    } else {
      # update the map
      # leaflet::leafletProxy('nfi_map') |>
      #   leaflet::clearGroup('plots')
      mapdeck::mapdeck_update(map_id = session$ns("nfi_map")) |>
        mapdeck::clear_scatterplot(layer_id = "plots")
    }
  })

  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$aesthetics <- aesthetics_builder()
    map_reactives$nfi_map_shape_click <- input$nfi_map_shape_click
    map_reactives$nfi_map_draw_all_features <- input$nfi_map_draw_all_features
  })
  return(map_reactives)
}
