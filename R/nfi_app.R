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

  ## UI ####
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
            # viz
            shiny::tabsetPanel(
              id = 'sidebar_tabset', type = 'tabs',
              # TODO transform titles in ui's for translations
              # data tab
              shiny::tabPanel(
                title = 'data',
                value = 'h4_data_selection',
                mod_dataInput('mod_dataInput')
              ), # end of data tab
              # filter tab
              shiny::tabPanel(
                title = 'filters',
                value = 'data_tab_2',
                mod_filtersUI('mod_filtersUI')
              ) # end of filter tab
            ) # end of sidebar tabsetPanel
          ),
          ## main panel
          mainPanel = shiny::mainPanel(
            width = 7,
            shiny::uiOutput("main_tabbed")
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

    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', nfidb, lang(),
      var_thes, numerical_thes, texts_thes
    )

    filters_reactives <- shiny::callModule(
      mod_filters, 'mod_filtersUI', nfidb, lang(),
      data_reactives,
      var_thes, numerical_thes, texts_thes
    )

  } # end of server

  # Run the application
  nfi_app_res <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(nfi_app_res)

}
