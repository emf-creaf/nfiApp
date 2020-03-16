library(magrittr)
library(lfcdata)

lidardb <- lidar()

# municipality_polygons <-
# sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   dplyr::select(admin_municipality = NOMMUNI, geometry)
municipality_polygons <-
  lidar_get_data(lidardb, 'lidar_municipalities', 'AB') %>%
  dplyr::select(admin_municipality = poly_id, geometry) %>%
  sf::st_transform(4326)

# region_polygons <-
#   sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   dplyr::select(admin_region = NOMCOMAR, geometry)
region_polygons <-
  lidar_get_data(lidardb, 'lidar_counties', 'AB') %>%
  dplyr::select(admin_region = poly_id, geometry) %>%
  sf::st_transform(4326)

# vegueria_polygons <-
#   sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpv1_20180101_0.shp') %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   dplyr::select(admin_vegueria = NOMVEGUE, geometry)
vegueria_polygons <-
  lidar_get_data(lidardb, 'lidar_vegueries', 'AB') %>%
  dplyr::select(admin_vegueria = poly_id, geometry) %>%
  sf::st_transform(4326)

# province_polygons <-
#   sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpp1_20180101_0.shp') %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   dplyr::select(admin_province = NOMPROV, geometry)
province_polygons <-
  lidar_get_data(lidardb, 'lidar_provinces', 'AB') %>%
  dplyr::select(admin_province = poly_id, geometry) %>%
  sf::st_transform(4326)

aut_community_polygons <-
  sf::read_sf('data-raw/shapefiles/catalunya.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_aut_community = NOM_CA, geometry)
# aut_community_polygons <-
#   lidar_get_data(lidardb, 'lidar_municipalities', 'AB') %>%
#   dplyr::select(admin_aut_community = poly_id, geometry) %>%
#   sf::st_transform(4326)

# enpe_polygons
natural_interest_area_polygons <- sf::read_sf('data-raw/shapefiles/enpe_2017.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_natural_interest_area = nom, geometry)

# pein_polygons
special_protection_natural_area_polygons <- sf::read_sf('data-raw/shapefiles/pein_2017.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_special_protection_natural_area = nom, geometry)

# xn2000_polyogns
natura_network_2000_polygons <- sf::read_sf('data-raw/shapefiles/xn2000_2017.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(admin_natura_network_2000 = nom_n2, geometry)

usethis::use_data(
  municipality_polygons, region_polygons, vegueria_polygons,
  province_polygons, aut_community_polygons, natural_interest_area_polygons,
  special_protection_natural_area_polygons, natura_network_2000_polygons,


  internal = TRUE, overwrite = TRUE
)
