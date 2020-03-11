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
  res <- texts_thes[
    texts_thes$text_id == text, as.character(glue::glue('text_{lang}')),
    drop = TRUE
  ]
  if (length(res) < 1) {
    message(glue::glue("{text} not found in thesaurus"))
    return(text)
  } else {
    return(res)
  }
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
      dplyr::distinct()
  }

  if (isTRUE(need_order)) {
    order_of_vars <-
      var_lookup_table %>%
      dplyr::arrange(var_order_app) %>%
      dplyr::pull(var_id) %>%
      match(vars, .) %>%
      order()

    ordered_res <- vars[order_of_vars] %>%
      magrittr::set_names(
        var_lookup_table[order_of_vars, 'var_name', drop = TRUE]
      )

    return(ordered_res)
  } else {
    var_lookup_table %>%
      dplyr::pull(var_id) %>%
      magrittr::set_names(
        var_lookup_table %>% dplyr::pull(var_name)
      )
  }
}

# Input builder, for a single variable, designed to be used in a map or lapply
#   - for building the input, first we need to know if the variable is
#     character, numeric, logical or date to know the correct input for
#     each. This is with vars_thes and the table names. Second check the cache
#     and set the values if found. After that, build the input. This is going to
#     be done by a function in helpers.R
filter_inputs_builder_helper <- function(
  variable, tables,
  texts_thes, var_thes, numerical_thes, categorical_thes,
  lang, ns, cache
) {

  # checking the type of the variable
  variable_description <-
    var_thes %>%
    dplyr::filter(var_id == variable, var_table %in% tables) %>%
    dplyr::select(var_id, var_table, var_type) %>%
    dplyr::distinct() %>%
    dplyr::left_join(numerical_thes, by = c('var_id', 'var_table')) %>%
    dplyr::left_join(categorical_thes, by = c('var_id', 'var_table'))

  # check for special case, plot_id which is present in all the tables,
  # in that case, we choose the results table, that is the one more
  # restrictive (less options)
  if (nrow(variable_description) > 1) {
    variable_description <- dplyr::slice(variable_description, 1)
  }
  # variable_type
  variable_type <- variable_description %>% dplyr::pull(var_type)

  # character variable
  if (variable_type == 'character') {
    # get the available choices for the categorical variable
    input_choices <- variable_description %>%
      tidyr::unnest(cols = c(var_values)) %>%
      dplyr::arrange(var_values) %>%
      dplyr::pull(var_values)

    # check the cache to look if there was set before
    if (cache$exists(stringr::str_remove_all(variable, '_'))) {
      previous_value <- cache$get(stringr::str_remove_all(variable, '_'))
    } else {
      previous_value <- NULL
    }

    # build the input
    res <- shinyWidgets::pickerInput(
      ns(variable),
      label = translate_var(variable, tables, lang, var_thes, numerical_thes),
      choices = input_choices,
      selected = previous_value,
      multiple = TRUE,
      options = list(
        `actions-box` = FALSE,
        `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
        `select-all-text` = text_translate('select-all-text', lang, texts_thes),
        `selected-text-format` = 'count',
        `count-selected-text` = text_translate('count-selected-text-value', lang, texts_thes),
        `size` = 10,
        `live-search` = TRUE,
        `tick-icon` = 'glyphicon-tree-deciduous'
      )
    )
    return(res)
  } # end of character
  # numeric
  if (variable_type %in% c('numeric', 'integer')) {
    # get the available min max values for the numeric
    input_choices <- c(
      min = variable_description %>% dplyr::pull(var_min),
      max = variable_description %>% dplyr::pull(var_max)
    )

    # check the cache to look if there was set before
    if (cache$exists(stringr::str_remove_all(variable, '_'))) {
      previous_value <- cache$get(stringr::str_remove_all(variable, '_'))
    } else {
      previous_value <- c(input_choices[['min']], input_choices[['max']])
    }

    # build the input
    res <- shiny::sliderInput(
      ns(variable),
      label = translate_var(variable, tables, lang, var_thes, numerical_thes),
      min = input_choices[['min']],
      max = input_choices[['max']],
      value = c(previous_value[[1]], previous_value[[2]]),
      width = '100%'
    )
    return(res)
  } # end of numeric
  # logical
  if (variable_type == 'logical') {
    # choices
    input_choices <- c('true', 'false')

    # check the cache to look if there was set before
    if (cache$exists(stringr::str_remove_all(variable, '_'))) {
      previous_value <- cache$get(stringr::str_remove_all(variable, '_'))
    } else {
      previous_value <- input_choices[1]
    }

    # build the input
    res <- shinyWidgets::pickerInput(
      ns(variable),
      label = translate_var(variable, tables, lang, var_thes, numerical_thes),
      choices = input_choices,
      selected = previous_value,
      multiple = TRUE,
      options = list(
        `actions-box` = FALSE,
        `deselect-all-text` = text_translate('deselect-all-text', lang, texts_thes),
        `select-all-text` = text_translate('select-all-text', lang, texts_thes),
        `selected-text-format` = 'count',
        `count-selected-text` = text_translate('count-selected-text-value', lang, texts_thes),
        `size` = 10,
        `live-search` = TRUE,
        `tick-icon` = 'glyphicon-tree-deciduous'
      )
    )
    return(res)
  } # end of logical
  if (variable_type == 'POSIXct') {
    # TODO (they are removed from appear)
    return()
  }

}
