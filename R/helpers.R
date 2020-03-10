# Call this function with an input (such as `textInput("text", NULL, "Search")`)
# if you want to add an input to the navbar
# (from dean attali, https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

# translate function
text_translate <- function(text, lang, texts_thes) {

  text[is.na(text)] <- 'NA_'

  text_df <- texts_thes %>%
    dplyr::select(dplyr::one_of('text_id', glue::glue("text_{lang}"))) %>%
    dplyr::filter(text_id %in% text) %>%
    # dplyr::collect() %>%
    as.data.frame()

  # if no translation is found return the original text
  if (nrow(text_df) < 1) {
    message(glue::glue("{text} not found in thesaurus"))
    return(text)
  }

  # return the translation for the selected language
  text %>%
    purrr::map_chr(
      ~ text_df[text_df$text_id == .x, glue::glue('text_{lang}')]
    )
}

# main table (results) to look at
main_table_to_look_at <- function(nfi, desglossament, diameter_classes) {
  # the logic is as follows:
  #   - except for shrubs and regen, the name of the main table is built by
  #     the desgossament level, the nfi and the diameter_classes if checked
  #   - a simple switch must work
  if (isTRUE(diameter_classes)) {
    dc <- 'DIAMCLASS_'
  } else {
    dc <- ''
  }

  main_table <- switch(
    nfi,
    'nfi_2' = glue::glue('{desglossament}_nfi_2_{dc}results'),
    'nfi_3' = glue::glue('{desglossament}_nfi_3_{dc}results'),
    'nfi_4' = glue::glue('{desglossament}_nfi_4_{dc}results'),
    'nfi_2_nfi_3' = glue::glue('{desglossament}_comp_nfi2_nfi3_{dc}results'),
    'nfi_3_nfi_4' = glue::glue('{desglossament}_comp_nfi3_nfi4_{dc}results'),
    'nfi_2_shrub' = glue::glue('shrub_nfi_2_info'),
    'nfi_3_shrub' = glue::glue('shrub_nfi_3_info'),
    'nfi_4_shrub' = glue::glue('shrub_nfi_4_info'),
    'nfi_2_regen' = glue::glue('regeneration_nfi_2'),
    'nfi_3_regen' = glue::glue('regeneration_nfi_3'),
    'nfi_4_regen' = glue::glue('regeneration_nfi_4')
  )

  return(main_table)
}

# ancillary tables to look at
ancillary_tables_to_look_at <- function(nfi) {
  # the logic is as follows:
  #   - plots is always an ancillary plot
  #   - except for the comparision data, the nfi_X indicates the dyanmic table
  #     to retrieve
  if (nfi %in% c('nfi_2_nfi_3', 'nfi_3_nfi_4')) {
    ancillary_tables <- 'plots'
  } else {
    nfi_strip <- stringr::str_extract(nfi, 'nfi_[2-4]')
    ancillary_tables <- c(
      'plots', glue::glue({"plots_{nfi_strip}_dynamic_info"})
    )
  }
  return(ancillary_tables)
}

# translate variables function. Similar as text_translate, but for variables
translate_var <- function(
  vars, tables, lang,
  var_thes, numerical_thes,
  is_summary = FALSE, need_order = TRUE
) {
  if (isTRUE(is_summary)) {

  } else {
    var_lookup_table <-
      var_thes %>%
      dplyr::select(
        dplyr::one_of(
          'var_id', glue::glue('translation_{lang}'),
          'var_order_app', 'var_table'
        )
      ) %>%
      dplyr::filter(var_id %in% vars, var_table %in% tables) %>%
      dplyr::left_join(
        numerical_thes %>%
          dplyr::select(var_id, var_table, var_units),
        by = c('var_id', 'var_table')
      ) %>%
      dplyr::mutate(
        var_units = dplyr::if_else(
          is.na(var_units), NA_character_,
          as.character(glue::glue("[{var_units}]"))
        )
      ) %>%
      tidyr::unite(
        col = var_name,
        !!rlang::sym(glue::glue("translation_{lang}")), var_units,
        sep = ' '
      ) %>%
      dplyr::mutate(var_name = stringr::str_remove(var_name, ' NA')) %>%
      dplyr::select(-dplyr::one_of(
        'var_table'
      )) %>%
      dplyr::distinct() %>%
      as.data.frame()
  }

  if (isTRUE(need_order)) {
    order_of_vars <-
      var_lookup_table %>%
      dplyr::arrange(var_order_app) %>%
      dplyr::pull(var_id) %>%
      match(vars, .) %>%
      order()

    ordered_res <- vars[order_of_vars] %>%
      magrittr::set_names(var_lookup_table[order_of_vars, 'var_name'])

    return(ordered_res)
  } else {
    var_lookup_table %>%
      dplyr::pull(var_id) %>%
      magrittr::set_names(
        var_lookup_table %>% dplyr::pull(var_name)
      )
  }
}
