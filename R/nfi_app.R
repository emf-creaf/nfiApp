#' function to launch the nfi app
#'
#' @importFrom magrittr %>%
#'
#' @export
nfi_app <- function() {

  ### DB access ################################################################
  nfidb <- lfcdata::nfi()

  ### thesauruses ##############################################################
  var_thes <- nfidb$get_data('variables_thesaurus')
  numerical_thes <- nfidb$get_data('variables_numerical')
  texts_thes <- nfidb$get_data('texts_thesaurus')
  categorical_thes <- nfidb$get_data('variables_categorical') %>%
    dplyr::select(-dummy_id) %>%
    tidyr::nest(var_values = c(var_values))

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'NFIappkg')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue(
      "<img class='flag-image' src='images/cat.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/spa.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    ),
    glue::glue(
      "<img class='flag-image' src='images/eng.png'",
      " width=20px><div class='flag-lang'>%s</div></img>"
    )
  )

  ## JS code needed ############################################################
  js_script <- shiny::HTML(
'$(document).on("shiny:idle", function(event) {
  Shiny.setInputValue("first_time", "true", {priority: "event"});
});'
  )


  ## UI ########################################################################
  ui <- shiny::tagList(

    # use shinyjs
    shinyjs::useShinyjs(),

    # navbar with inputs (custom function, see helpers.R)
    navbarPageWithInputs(
      # opts
      title = 'NFI app',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for
      # the lang selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # main tab
      shiny::tabPanel(
        title = 'Main',
        # css
        shiny::tags$head(
          # js script,
          shiny::tags$script(js_script),
          # custom css
          shiny::includeCSS(
            system.file('resources', 'nfi.css', package = 'NFIappkg')
          ),
          # corporative image css
          shiny::includeCSS(
            system.file('resources', 'corp_image.css', package = 'NFIappkg')
          )
        ),
        # Sidebar layout
        shiny::sidebarLayout(
          ## options
          position = 'left', fluid = TRUE,
          ## sidebar panel
          sidebarPanel = shiny::sidebarPanel(
            width = 5,
            # this is gonna be a tabsetPanel, for data selection, filtering and
            # viz. The apply button will be on the top, that way is always
            # visible in any size
            # apply button module
            mod_applyButtonInput("mod_applyButtonInput"),
            shiny::br(),
            # tabset panel
            shiny::tabsetPanel(
              id = 'sidebar_tabset', type = 'pills',
              # TODO transform titles in ui's for translations
              # data tab
              shiny::tabPanel(
                title = 'data',
                value = 'data_inputs_panel',
                mod_dataInput('mod_dataInput')
              ), # end of data tab
              # filter tab
              shiny::tabPanel(
                title = 'filters',
                value = 'filters_panel',
                mod_filtersUI('mod_filtersUI')
              ), # end of filter tab
              # viz tab
              shiny::tabPanel(
                title = 'viz',
                value = 'viz_panel',
                mod_vizInput('mod_vizInput')
              ) # end of viz tab
            ) # end of sidebar tabsetPanel
          ),
          ## main panel
          mainPanel = shiny::mainPanel(
            width = 7,
            shiny::tabsetPanel(
              id = 'main_panel_tabset', type = 'pills',
              shiny::tabPanel(
                title = 'map',
                value = 'map_panel',
                mod_mapOutput('mod_mapOutput')
              ),
              shiny::tabPanel(
                title = 'table',
                value = 'table_panel',
                mod_dataTableOutput('mod_dataTableOutput')
              )
            )
          )
        ) # end sidebar layout
      )
    ) # end NavBarWithInputs

  ) # end of UI

  ## server ####
  server <- function(input, output, session) {

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # cache ####
    filters_cache <- shiny::memoryCache(evict = 'fifo')

    # modules ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', nfidb, lang,
      var_thes, numerical_thes, texts_thes
    )
    # filters
    filter_reactives <- shiny::callModule(
      mod_filters, 'mod_filtersUI', nfidb, lang,
      data_reactives, filters_cache,
      var_thes, numerical_thes, texts_thes, categorical_thes
    )
    # apply button
    apply_reactives <- shiny::callModule(
      mod_applyButton, 'mod_applyButtonInput', lang,
      texts_thes, var_thes, numerical_thes,
      data_reactives, filter_reactives
    )
    # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, filter_reactives, apply_reactives, map_reactives,
      nfidb, lang, texts_thes, session
    )
    # viz
    viz_reactives <- shiny::callModule(
      mod_viz, 'mod_vizInput',
      data_reactives, filter_reactives,
      nfidb, var_thes, texts_thes, numerical_thes, categorical_thes,
      lang
    )
    # table
    shiny::callModule(
      mod_dataTable, 'mod_dataTableOutput',
      main_data_reactives, data_reactives, viz_reactives,
      nfidb, var_thes, texts_thes, numerical_thes, lang
    )
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, viz_reactives, main_data_reactives,
      nfidb, lang,
      var_thes, texts_thes, numerical_thes
    )
    # info
    shiny::callModule(
      mod_info, 'mod_infoUI',
      map_reactives, main_data_reactives, viz_reactives,
      nfidb, var_thes, texts_thes, numerical_thes, lang
    )

    ## observers ####
    # modal observer
    shiny::observeEvent(
      eventExpr = map_reactives$nfi_map_shape_click,
      handlerExpr = {
        shiny::showModal(
          shiny::modalDialog(
            mod_infoUI('mod_infoUI'),
            footer = shiny::modalButton(
              text_translate('dismiss', lang(), texts_thes)
            ),
            size = 'm', easyClose = TRUE
          )
        )
      }
    )

    # first time observer
    shiny::observeEvent(
      once = TRUE, priority = 1,
      eventExpr = {
        input$first_time
        shiny::isolate(data_reactives$nfi)
      },
      handlerExpr = {
        shinyjs::click("mod_applyButtonInput-apply", asis = TRUE)
      }
    )


  } # end of server

  # Run the application
  nfi_app_res <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(nfi_app_res)

}
