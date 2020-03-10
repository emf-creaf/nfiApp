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
