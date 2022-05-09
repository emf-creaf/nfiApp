library(magrittr)
library(rmapshaper)
library(lfcdata)

# lidardb <- lidar()

municipality_polygons <-
sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_municipality = NOMMUNI, geometry)
# municipality_polygons <-
#   lidar_get_data(lidardb, 'lidar_municipalities', 'AB') %>%
#   dplyr::select(admin_municipality = poly_id, geometry) %>%
#   sf::st_transform(4326)

region_polygons <-
  sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_region = NOMCOMAR, geometry)
# region_polygons <-
#   lidar_get_data(lidardb, 'lidar_counties', 'AB') %>%
#   dplyr::select(admin_region = poly_id, geometry) %>%
#   sf::st_transform(4326)

vegueria_polygons <-
  sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_vegueria = NOMVEGUE, geometry)
# vegueria_polygons <-
#   lidar_get_data(lidardb, 'lidar_vegueries', 'AB') %>%
#   dplyr::select(admin_vegueria = poly_id, geometry) %>%
#   sf::st_transform(4326)

province_polygons <-
  sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_province = NOMPROV, geometry)
# province_polygons <-
#   lidar_get_data(lidardb, 'lidar_provinces', 'AB') %>%
#   dplyr::select(admin_province = poly_id, geometry) %>%
#   sf::st_transform(4326)

aut_community_polygons <-
  sf::read_sf('data-raw/shapefiles/catalunya.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_aut_community = NOM_CA, geometry)
# aut_community_polygons <-
#   lidar_get_data(lidardb, 'lidar_municipalities', 'AB') %>%
#   dplyr::select(admin_aut_community = poly_id, geometry) %>%
#   sf::st_transform(4326)

# enpe_polygons
natural_interest_area_polygons <-
  sf::read_sf('data-raw/shapefiles/enpe_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_natural_interest_area = nom, geometry) %>%
  dplyr::mutate(dummy = admin_natural_interest_area) %>%
  dplyr::group_by(admin_natural_interest_area) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

# pein_polygons
special_protection_natural_area_polygons <-
  sf::read_sf('data-raw/shapefiles/pein_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_special_protection_natural_area = nom, geometry) %>%
  dplyr::mutate(dummy = admin_special_protection_natural_area) %>%
  dplyr::group_by(admin_special_protection_natural_area) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

# xn2000_polyogns
natura_network_2000_polygons <-
  sf::read_sf('data-raw/shapefiles/xn2000_2017.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_natura_network_2000 = nom_n2, geometry) %>%
  dplyr::mutate(dummy = admin_natura_network_2000) %>%
  dplyr::group_by(admin_natura_network_2000) %>%
  dplyr::summarise(dummy = dplyr::first(dummy)) %>%
  dplyr::select(-dummy) %>%
  sf::st_cast('MULTIPOLYGON')

# app texts thesaurus
texts_thes <- tibble::tribble(
  ~text_id, ~text_eng, ~text_spa, ~text_cat,
  "sweet_alert_returned_data_title", "No data can be retrieved with the actual filters", "Con los filtros actuales no se pueden obtener datos", "Amb els filtres actuals no es poden obtenir dades",
  "sweet_alert_returned_data_text", "Please choose another filter values", "Por favor, seleccione otros valores para los filtros", "Si us plau, seleccioni altres valors per als filtres",
  "sweet_alert_polygon_title", "No data inside the provided polygons", "No hay datos en los polígonos", "No hi ha dades en els polígons",
  "sweet_alert_polygon_text", "Check if there are filters in effect and/or the polygons are correct", "Compruebe si hay filtros activos y/o si los polígonos son correctos", "Comproveu si hi ha filtres en efecte i/o els polígons són correctes",
  "dismiss", "Dismiss", "Cancelar", "Descartar",
  "mean", "Mean", "Media", "Mitjana",
  "se", "SE", "ES", "ES",
  "n", "Number", "Número", "Nombre",
  "max", "Max", "Max", "Max",
  "min", "Min", "Min", "Min",
  "id", "ID variables", "Grupos funcionales", "Grups funcionals",
  "admin", "Administrative variables", "Variables administrativas", "Variables administratives",
  "proper_table", "NFI variables", "Variables del IFN", "Variables de l'IFN",
  "clim", "Climatic variables", "Variables climáticas", "Variables climàtiques",
  "topo", "Topographic variables", "Variables topográficas", "Variables topogràfiques",
  "feat", "Plot features variables", "Características de parcela", "Característiques de parcel·la",
  "coord", "Coordinates", "Coordenadas", "Coordenades",
  "old", "Old db variables", "Variables antigua base de datos", "Variables antiga base de dades",
  "deselect-all-text", "None selected...", "Ninguno", "Ningú",
  "select-all-text", "All selected...", "Todos", "Tots",
  "count-selected-text-value", "{0} values selected (of {1})", "{0} valores seleccionados (de {1})", "{0} valors seleccionats (de {1})",
  "count-selected-text-var", "{0} variables selected (of {1})", "{0} variables seleccionadas (de {1})", "{0} variables seleccionades (de {1})",
  "cite_div", "Data prepared by the CTFC and CREAF based on the raw NFI data served by the MAPA (Spanish Government)", "Datos elaborados por el CTFC y el CREAF a partir de los datos brutos servidos por el MAPA (Gobierno de España)", "Dades elaborades pel CTFC i el CREAF a partir de les dades brutes de NFI ateses pel MAPA (Govern espanyol)",
  "apply_warning", "Data needs to be refreshed, please click the apply button.", "Los datos deben actualizarse, haga clic en el botón Aplicar.", "Cal actualitzar les dades. Feu clic al botó Aplicar",
  "save_map_button", "Save the map", "Guardar el mapa", "Guardar el mapa",
  "save_table_button", "Save the table", "Guardar la table", "Guardar la taula",
  "table_length_options_input", "All data or visible data?", "¿Todos los datos o solo las columnas visibles?", "¿Totes les dades o només les columnes visibles?",
  "visible", "Visible columns", "Columnas visibles", "Columnes visibles",
  "all_columns", "All columns", "Todas las columnas", "Totas les columnes",
  "table_output_options_input", "Choose the output format", "Selecciona el formato", "Selecciona el format",
  "csv", "Text (csv)", "Texto (csv)", "Text (csv)",
  "xlsx", "MS Excel (xlsx)", "MS Excel (xlsx)", "MS Excel (xlsx)",
  "fil_res_vars_input", "Results filters", "Filtros de resultados", "Filtres de resultats",
  "fil_clim_vars_input", "Climatic filters", "Filtros climáticos", "Filtres climàtics",
  "fil_plot_vars_input", "Other filters", "Otros filtros", "Altres filtres",
  "filter_the_data", "Filter the data:", "Filtra los datos:", "Filtre les dades:",
  "info_count", "Number of plots", "Número de parcelas", "Nombre de parcel·les",
  "plot_id_info_plot_title", "Clicked plot compared to other plots in map", "Parcela seleccionada comparada con las otras parcelas en el mapa", "Parcel·la seleccionada comparada amb les altres parcel·les al mapa",
  # "admin_aut_community_info_plot_title", "Clicked autonomous community compared to other at in map", "Parcela seleccionada comparada con las otras parcelas en el mapa", "Parcel·la seleccionada comparada amb les altres parcel·les al mapa",
  "admin_province_info_plot_title", "Clicked province compared to other provinces in map", "Provincia seleccionada comparada con las otras provincias en el mapa", "Provincia seleccionada comparada amb les altres provincies al mapa",
  "admin_vegueria_info_plot_title", "Clicked vegueria compared to other veguerias in map", "Vegueria seleccionada comparada con las otras veguerias en el mapa", "Vegueria seleccionada comparada amb les altres vegueries al mapa",
  "admin_region_info_plot_title", "Clicked region compared to other regions in map", "Comarca seleccionada comparada con las otras comarcas en el mapa", "Comarca seleccionada comparada amb les altres comarques al mapa",
  "admin_municipality_info_plot_title", "Clicked municipality compared to other municipalities in map", "Municipio seleccionada comparada con los otros municipios en el mapa", "Municipi seleccionado comparad amb els altres municipis al mapa",
  "poly_id_info_plot_title", "Clicked polygon compared to other polygons in map", "Polígono seleccionada comparada con los otros polígonos en el mapa", "Polìgon seleccionado comparado amb les altres polìgons al mapa",
  "admin_natura_network_2000_info_plot_title", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa", "Àrea seleccionada comparada amb les altres àreas al mapa",
  "admin_special_protection_natural_area_info_plot_title", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa", "Àrea seleccionada comparada amb les altres àreas al mapa",
  "admin_natural_interest_area_info_plot_title", "Clicked natural area compared to other natural areas in map", "Área seleccionada comparada con las otras áreas en el mapa", "Àrea seleccionada comparada amb les altres àreas al mapa",
  "cat_admin_info_plot_title", "Distribution of plots in {map_reactives$nfi_map_shape_click$id} for {viz_color}", "Distribución de parcelas en {map_reactives$nfi_map_shape_click$id} para {viz_color}", "Distribució de parcel·les a {map_reactives$nfi_map_shape_click$id} per a {viz_color}",
  "cat_plot_info_plot_title", "Distribution of plots for {viz_color}", "Distribución de parcelas para {viz_color}", "Dostribució de parcel·les per a {viz_color}",
  "not_enough_info_plot_warning", "Not enough data to safely build the plot", "No hay datos suficientes para contruir la gráfica de manera segura", "No hi ha dades suficients per construir el gràfic de forma segura",
  "dri_group_by_div", "by administrative division", "por división administrativa", "per divisió administrativa",
  "dri_group_by_dom", "by dominant functional group", "por grupo funcional", "per grup funcional",
  "dri_title", "Query summary", "Resumen de la consulta", "Resum de la consulta",
  "dri_nfi", "Data version", "Versión de los datos", "Versió de les dades",
  "dri_agrupament", "Group by", "Agrupado por", "Agrupat per",
  "dri_none", "None", "Ninguno", "Cap",
  "dri_desglossament", "Breakdown by", "Desglosado por", "Desglossat per",
  "dri_filters", "Active filters", "Filtros activos", "Filtres actius",
  "dri_diameter_classes", "by diameter classes", "por clases diamétricas", "per classes diamètriques",
  "fg_bc", "Breakdown by broadleaf-conifer", "Desglosado por planifolia-conífera", "Desglossat per planifòlia-conífera",
  "fg_dec", "Breakdown by decidious-esclerophyl-conifer", "Desglosado por caducifolia-esclerófila-conífera", "Desglossat per caducifòlia-esclerofil·le-conífera",
  "fg_genus", "Breakdown by genus", "Desglosado por género", "Desglossat per gènere",
  "fg_plot", "Total by plot", "Total por parcela", "Total per parcel·la",
  "fg_simpspecies", "Breakdown by simplified species", "Desglosado por especie simplifcada", "Desglossat per espècie simplificada",
  "fg_species", "Breakdown by species", "Desglosado por especies", "Desglossat per espècies",
  "apply", "Apply", "Aplicar", "Aplicar",
  "col_vis_selector_input", "Choose the variables to show", "Selecciona las variables a mostrar en la tabla", "Selecciona les variables a mostrar en la taula",
  "glossary_var_input", "Choose the variable to describe", "Selecciona la variable a describir", "Selecciona la variable a descriure",
  "link_to_tutorials_text", "For more info, please go to the application tutorial here", "Para obtener más información, vaya al tutorial de la aplicación aquí.", "Per obtenir més informació, aneu al tutorial de l'aplicació aquí",
  "var_description_title", "Description:", "Descripción:", "Descripció:",
  "var_units_title", "Units:", "Unidades:", "Unitats:",
  "nfi_2", "NFI v2", "IFN v2", "IFN v2",
  "nfi_2_nfi_3", "NFI comp v2 - v3", "IFN comp v2 - v3", "IFN comp v2 - v3",
  "nfi_2_regen", "NFI v2 Regeneration", "IFN v2 Regeneración", "IFN v2 Regeneració",
  "nfi_2_shrub", "NFI v2 Shrubs", "IFN v2 Arbustos", "IFN v2 Arbusts",
  "nfi_3", "NFI v3", "IFN v3", "IFN v3",
  "nfi_3_nfi_4", "NFI comp v3 - v4", "IFN comp v3 - v4", "IFN comp v3 - v4",
  "nfi_3_regen", "NFI v3 Regeneration", "IFN v3 Regeneración","IFN v3 Regeneració",
  "nfi_3_shrub", "NFI v3 Shrubs", "IFN v3 Arbustos", "IFN v3 Arbusts",
  "nfi_4", "NFI v4", "IFN v4", "IFN v4",
  "nfi_4_regen", "NFI v4 Regeneration", "IFN v4 Regeneración", "IFN v4 Regeneració",
  "nfi_4_shrub", "NFI v4 Shrubs", "IFN v4 Arbustos", "IFN v4 Arbusts",
  "aut_community", "Catalonia", "Cataluña", "Catalunya",
  "province", "Provinces", "Provincias", "Provincies",
  "vegueria", "Veguerias", "Veguerias", "Vegueries",
  "region", "Regions", "Comarcas", "Comarques",
  "municipality", "Municipalities", "Municipios", "Municipis",
  "natural_interest_area", "Natural interest areas", "Áreas de interés natural", "Àrees d'interès natural",
  "special_protection_natural_area", "Special protection natural areas", "Áreas naturales de protección especial", "Àrees naturals de protecció especial",
  "natura_network_2000", "Natura 2000 network", "Red Natura 2000", "Xarxa Natura 2000",
  "file", "Polygon file", "Archivo de polígonos", "Arxiu de polìgons",
  "drawn_poly", "Drawn polygon", "Polígono dibujado", "Polígon dibuixat",
  "none", "None", "Ninguno", "Cap",
  "species", "Species", "Especie", "Espècie",
  "simpspecies", "Simplified species", "Especie simplificada", "Espècie simplificada",
  "genus", "Genus", "Género", "Gènere",
  "dec", "Cad/Escler/Conif.", "Cad/Escler/Conif.", "Cad/Escler/Conif.",
  "bc", "Broadleaf/Conifer", "Planif/Conif.", "Planif/Conif.",
  "dominant_criteria_density", "Density", "Densidad", "Densitat",
  "dominant_criteria_basal_area", "Basal area", "Área basal", "Àrea basal",
  "nfi2", "NFI 2", "IFN 2", "IFN 2",
  "nfi3", "NFI 3", "IFN 3", "IFN 3",
  "nfi4", "NFI 4", "IFN 4", "IFN 4",
  "h4_data_version", "Data selection", "Seleccion de datos", "Selecció de dades",
  "data_version", "Data version", "Versión de los datos", "Versió de les dades",
  "divisions", "Divisions", "Divisiones", "Divisions",
  "user_file_sel_label", "Select the file to upload", "Selecciona el archivo a cargar", "Selecciona l'arxiu a carregar",
  "user_file_sel_buttonLabel", "Browse...", "Inspecciona...", "Inspecciona...",
  "user_file_sel_placeholder", "No file selected", "Ningún archivo seleccionado", "Cap fitxer seleccionat",
  "file_text", 'File can be a shapefile (compressed in a zip file) or GeoPackage file (.gpkg). They must have a field called "poly_id" with the identifiers of the contained polygons.', 'El archivo puede ser un shapefile (comprimido en un archivo zip) o un archivo GeoPackage (.gpkg). Deben tener un campo llamado "poly_id" con los identificadores de los polígonos contenidos.', 'El fitxer pot ser un shapefile (comprimit en un fitxer zip) o un fitxer GeoPackage (.gpkg). Han de tenir un camp anomenat "poly_id" amb els identificadors dels polígons continguts.',
  "h4_agrupament", "Group data", "Agrupamiento de los datos", "Agrupament de les dades",
  "group_by_div_input", "Group by administrative division", "Agrupar por división administrativa", "Agrupar per divisió administrativa",
  "group_by_dom_input", "Group by dominant functional group", "Agrupar por grupo funcional", "Agrupar per grup funcional",
  'dominant_group_input', "Functional group", "Grupo funcional", "Grup funcional",
  'dominant_criteria_input', "Dominancy criteria", "Criterio de dominancia", "Criteri de dominància",
  'dominant_nfi_input', "Dominant in", "Dominante en", "Dominant en",
  "h4_desglossament", "Breakdown data", "Desglose de los datos", "Desglossat de les dades",
  "desglossament_input", "Select the level", "Selecciona el nivel", "Selecciona el nivell",
  "diameter_classes_input", "Extra breakdown by diameter classes?", "¿Desglosar por clases diamétricas?", "Desglossar per classes diamètriques?",
  "max_stat", "Max", "Max", "Max",
  "mean_stat", "Mean", "Media", "Mitjana",
  "min_stat", "Min", "Min", "Min",
  "n_stat", "Number", "Número", "Nombre",
  "se_stat", "SE", "ES", "ES",
  "viz_color_input", "Color:", "Color:", "Color:",
  "viz_diamclass_input", "Diameter class to visualize:", "Clase diamétrica a visualizar:", "Classe diamètrica a visualitzar:",
  "functional_group_viz_input", "Funtional group value to visualize", "Grupo funcional a visualizar", "Grup funcional a visualitzar:",
  "viz_pal_config_input", "Config palette", "Configurar paleta", "Configurar paleta",
  "viz_pal_reverse_input", "Reverse the palette?", "¿Invertir la paleta?", "Invertir la paleta?",
  "viz_size_input", "Size:", "Tamaño:", "Mida:",
  "viz_statistic_input", "Statistic:", "Estadístico:", "Estadístic:",
  "pal_high", "Discriminate higher values", "Discriminar valores altos", "Discriminar valors alts",
  "pal_low", "Discriminate lower values", "Discriminar valores bajos", "Discriminar valors baixos",
  "pal_normal", "Normal", "Normal", "Normal",
  "main_tab_translation", "Explore", "Explora", "Explora",
  "data_translation", "Data", "Datos", "Dades",
  "filters_translation", "Filters", "Filtros", "Filtres",
  "viz_translation", "Visualization", "Visualización", "Visualització",
  "save_translation", "Save", "Guardar", "Guardar",
  "help_translation", "Help", "Ayuda", "Ajuda",
  "map_translation", "Map", "Mapa", "Mapa",
  "table_translation", "Table", "Tabla", "Taula",
  'save_map_btn', "Save the map", "Guarda el mapa", "Guarda el map",
  'save_table_btn', "Save the table", "Guarda la tabla", "Guarda la taula",
  "max-options-text", "Select limit reached (50)", "Alcanzado el límite de selección (50)", "Assolit el límit de selecció (50)",
  "active_filters_warning", "Active filters not present in data. Not applying them", "Los filtros activos no están presentes en los datos. No se aplicarán", "Els filtres actius no estan presents en les dades. No s'aplicaran",
  "active_filters_warning_title", "Ooops!", "¡Ooops!", "Ooops!",
  "progress_message", "Obtaining data...", "Obteniendo datos...", "Obtenció de dades...",
  "progress_detail_initial", "Querying database", "Consultando base de datos", "Consultant la base de dades",
  "progress_detail_tables", "Applying filters", "Aplicando filtros", "Aplicant filtres",
  "progress_detail_calc", "Data processing", "Procesando datos", "Processar les dades",
  # poly_id_var_check
  "poly_id_missing_title", "Not 'poly_id' variable found in file", "No se ha encontrado ninguna variable llamada 'poly_id' en el archivo", "No s'ha trobat cap variable anomenada 'poly_id' al fitxer",
  "poly_id_missing_message", "First variable found in file used as poly_id", "Se ha usado la primera variable del archivo como poly_id", "S'ha fet servir la primera variable del fitxer com a poly_id",

  'NA_', "", "", ""
)


# thesauruses
nfidb <- lfcdata::nfi()

var_thes <- nfidb$get_data('variables_thesaurus')
numerical_thes <- nfidb$get_data('variables_numerical')
# texts_thes <- nfidb$get_data('texts_thesaurus')
categorical_thes <- nfidb$get_data('variables_categorical') %>%
  dplyr::select(-dummy_id) %>%
  tidyr::nest(var_values = c(var_values))



usethis::use_data(
  municipality_polygons, region_polygons, vegueria_polygons,
  province_polygons, aut_community_polygons, natural_interest_area_polygons,
  special_protection_natural_area_polygons, natura_network_2000_polygons,

  texts_thes,

  var_thes, numerical_thes, categorical_thes,

  internal = TRUE, overwrite = TRUE
)
