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
  # ui output
  shiny::uiOutput(ns('map_panel'))
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
  output$map_panel <- shiny::renderUI({

    # ns
    ns <- session$ns

    # browser()

    # tagList
    shiny::tagList(
      shiny::fluidRow(
        leaflet::leafletOutput(ns('map'), height = 'auto'),
        shiny::tags$div(
          id = 'cite',
          text_translate('cite_div', lang, texts_thes)
        )
      )
    )
  }) # end of renderUI

  ## leaflet output (empty map) ####
  output$map <- leaflet::renderLeaflet({

    # we need data, and we need color var at least
    leaflet::leaflet() %>%
      leaflet::setView(0.74, 41.70, zoom = 8) %>%
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
  }) # end of leaflet ouput (empty map)

  ## reactives ####
  # legend builder
  # The idea is to abstract the legend building, as al we need is common to
  # points and polygons. The logic is as follows:
  #   - get the data and the viz inputs
  #   - if summary is on, color is a composition of color and statistic
  #   - pull the color vector, build the pal function
  #   - depending on class of vector, a legend class
  legend_map_builder <- shiny::reactive({
    # inputs
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)
    viz_color <- viz_reactives$viz_color
    viz_statistic <- viz_statisitc$viz_statistic
    viz_pal_config <- viz_statistic$viz_pal_config
    viz_pal_reverse <- viz_statistic$viz_pal_reverse

    # summarised polygons?
    if (any(group_by_div, group_by_dom)) {
      viz_color <- glue::glue("{viz_color}{viz_statistic}")
    }

    color_vector <-
      main_data_reactives$main_data$requested_data %>%
      dplyr::pull(!! rlang::sym(viz_color))

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
      color_vector = color_vector,
      pal = pal,
      legend_class = legend_clas
    ))
  })

  ## observers
  # polygon drawing. The logic is as follows:
  #   - if summarised data, get the summarised data, join the polygon object
  #     and proxy update the map.
  #   - if not, get the polygon object and update the map
  shiny::observer({

    # inputs for translating and other stuff
    nfi <- shiny::isolate(data_reactives$nfi)
    desglossament <- shiny::isolate(data_reactives$desglossament)
    diameter_classes <- shiny::isolate(data_reactives$diameter_classes)
    admin_div <- shiny::isolate(data_reactives$admin_div)
    viz_color <- viz_reactives$viz_color
    viz_statistic <- viz_reactives$viz_statistic
    # are we drawing summary data?
    group_by_div <- shiny::isolate(data_reactives$group_by_div)
    group_by_dom <- shiny::isolate(data_reactives$group_by_dom)

    tables_to_look_at <- c(
      main_table_to_look_at(nfi, desglossament, diameter_classes),
      ancillary_tables_to_look_at(nfi)
    )

    # polygon labels, join vars for data and join data expression for data
    if (admin_div %in% c('file', 'drawn_poly')) {
      polygon_label <- as.formula('~poly_id')
      polygon_join_var <- 'poly_id'
      polygon_join_data_expr <- rlang::quo(main_data_reactives$custom_polygon)
    } else {
      polygon_label <- as.formula(glue::glue("~admin_{admin_div}"))
      polygon_join_var <- glue::glue("admin_{admin_div}")
      polygon_join_data_expr <- rlang::quo(
        !! rlang::sym(polygon_join_var)
      )
    }

    # data
    if (any(group_by_div, group_by_dom)) {
      polygon_data <-  main_data_reactives$main_data$requested_data %>%
        dplyr::left_join(!! polygon_join_data_expr, by = polygon_join_var)
    } else {
      polygon_data <- rlang::eval_tidy(
        !! rlang::sym(glue::glue("{admin_div}_polygons"))
      )
    }

    # legend
    legend_data <- legend_map_builder()

    # update the map
    leaflet::leafletProxy('map') %>%
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
      leaflet::clearGroup('plots') %>%
      leaflet::addPolygons(
        data = polygon_data,
        group = admin_div,
        label = polygon_label,
        layerId = polygon_label,
        weight = 1, smoothFactor = 1,
        opacity = 1.0, fill = TRUE,
        color = '#6C7A89FF',
        fillColor = legend_data$pal(legend_data$color_vector),
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
              position = 'bottomright', pal = lengend_data$pal,
              values = legend_data$color_vector,
              title = names(
                translate_var(
                  viz_color, tables_to_look_at, lang, var_thes, numerical_thes,
                  texts_thes, is_summary = TRUE, need.order = FALSE
                )
              ),
              layerId = 'color_legend', opacity = 1,
              na.label = '', className = legend_data$legend_class
            )
        } else {
          temp
        }
      }
  })
}
