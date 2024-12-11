library(shiny)
library(shinydashboard)
library(tidyverse)
# library(plotly)
# library(ggtext)
library(shinyjs)
library(htmlwidgets)
library(shinyWidgets)
# library(ggExtra)
# library(ggridges)
# library(gghalves)
# library(scales)
# library(Rtsne)
# library(ggiraph)
# library(ggrepel)
library(shinydashboardPlus)
library(DT)
library(shinyBS)
library(shinyjs)
library(cowplot)
library(magick)
library(writexl)

# '#1f77b4', #blue
# '#ff7f0e', #orange
# '#2ca02c', #green 
# '#d62728', #red 
# '#9467bd', #purple
# '#8c564b', #brown
# '#e377c2', #pink 
# '#7f7f7f', #grey

################
#load in data
################

sociolex_norms_wide <- read_rds("data/summary/sociolex_data_summary_wide.rds") %>%
  select(item, word_english, everything()) %>%
  mutate_if(is.numeric, round, 3)

sociolex_norms_prop <- read_rds("data/sociolex_norms_prop.rds")

sociolex_norms_age_prop <- read_rds("data/sociolex_norms_age_prop.rds")

sociolex_norms_definitions <- read_csv("data/word_list_definitions.csv")

plot_labels <- read_csv("data/plot_labels.csv") %>%
  mutate(rating = factor(rating))



extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions1 <- length(extensions)
  nChoices1 <- length(cbg$children[[2]]$children[[1]])
  
  if (nExtensions1 > 0 && nChoices1 > 0) {
    lapply(1:min(nExtensions1, nChoices1), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

extendedRadioGroup <- function(..., extensions = list()) {
  rb1 <- radioButtons(...)
  nExtensions <- length(extensions)
  nChoices <- length(rb1$children[[2]]$children[[1]])
  
  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      rb1$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  rb1
}

bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}


###############
#UI
###############

ui <- dashboardPage(
  
  skin = "black", #set colour scheme
  
  #remove header
  dashboardHeader(disable = TRUE,
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 0px}"),
                          tags$style(".main-header .logo {max-height: 0px;}"),
                          tags$style(".sidebar-toggle {max-height: 0px; padding-top: 0px !important;}"),
                          tags$style(".navbar {max-height:0px !important}"))),
  
  ###############
  #sidebar menu
  ###############
  
  dashboardSidebar(
    
    sidebarMenu(id = "sidebarid",
                # style = "position:fixed",
                shinyjs::useShinyjs(),
                #add minimise switch
                prettySwitch(
                  inputId = "showSidebar",
                  label = NULL
                ),
                #add logo
                span(img(src="https://jamesbrandscience.github.io/assets/sociolex.png", width = "100%")),
                #add menu items
                # menuItem("information", tabName = "information", icon = icon("info-sign", lib = "glyphicon")),
                menuItem("Norms", tabName = "wordlist", icon = icon("book")),
                menuItem("Variables", tabName = "variables", icon = icon("question"))
                # menuItem("correlation", tabName = "correlation", icon = icon("chart-line")),
                # menuItem("age", tabName = "age", icon = icon("cloud-rain", class = "fas fa-cloud-rain fa-rotate-90")),
                # menuItem("t-SNE", tabName = "t-SNE", icon = icon("hubspot"))
    ),
    width = "15%"),
  
  
  ##################
  #main section
  ##################
  
  dashboardBody(
    
    ##################
    #css
    ##################
    
    #add css to customise style
    tags$head(
      tags$style(HTML(
      "
      .selectize-input {
      height: 90%;
      width: 90%;
      }
      
      .sidebar {
      margin-top: -50px;
      }
      
      .box {
      margin: 0px;
      padding:0px;
      }
      
      .box-header{
      display: none
      }
      
      "))
      ),
    
    #define what goes in the tabs from the sidebar
    tabItems(
      
      ###############
      #information tab
      ###############
      
      tabItem(
        tabName = "variables",
        box(DTOutput("variable_definitions"), width = 12),
      ),
      
      ###############
      #word list tab
      ###############
      
      tabItem(
        tabName = "wordlist",
        box(downloadButton("downloadData1", "Download data .csv"),
            downloadButton("downloadData2", "Download data .xlsx"),
            DTOutput("wordlist1", height = 14),
            width = 9, style = "overflow-x: scroll;"),
        
        fluidRow(
          fluidRow(
          column(
            width = 3,
            box(align = "left",
                fluidRow(

                  ###########
                  #search for a word
                  ###########
                  column(
                    dropdown(
                      circle = TRUE,
                      margin = "2px",
                      style = "material-circle",
                      size = "s",
                      icon = icon("search"),
                      status = "primary",
                      width = "auto",
                      
                      uiOutput("word_selections"),

                      textAreaInput(
                        inputId = "text_input_filter",
                        label = "Word list input",
                        value = "Insert
text
this
way
Or, this, way
CheckForTypos",
                        height = 200,
                        width = 200
                      )
                    ),
                    width = 1),

                  #############
                  #settings
                  #############
                  column(
                    offset = 1,
                    dropdown(
                      circle = TRUE,
                      margin = "2px",
                      style = "material-circle",
                      size = "s",
                      icon = icon("gear"),
                      status =  "primary",
                      width = "auto",

                      ############
                      #hover/click button
                      ############
                      tags$h5("Dimnesions"),
                      # switchInput(
                      #   inputId = "hover1a",
                      #   onLabel = "Hover",
                      #   offLabel = "Click",
                      #   value = TRUE,
                      #   width = "auto",
                      #   onStatus = "success"
                      # )
                      # prettyRadioButtons(
                      #   inputId = "hover1a",
                      #   label = NULL,
                      #   choiceNames = c("Word list and ratings", "Definition of variables"),
                      #   choiceValues = c("words", "definitions"),
                      #   inline = FALSE,
                      #   status = "success"
                      # ),
                      prettyCheckboxGroup(
                        inputId = "dimension_selection",
                        label = NULL,
                        choiceNames = c("Gender", "Location", "Political", "Valence", "Age"),
                        choiceValues = c("gender", "location", "political", "valence", "age"),
                        selected = c("gender", "location", "political", "valence", "age"),
                        inline = FALSE,
                        status = "success"
                      ),
                      tags$h5("Summary values"),
                      prettyCheckboxGroup(
                        inputId = "summary_values_selection",
                        label = NULL,
                        choiceNames = c("Mean", "SD", "Latent mean", "Entropy"),
                        choiceValues = c("mean_", "sd_", "latent_mean_", "entropy_"),
                        selected = c("mean_", "sd_", "latent_mean_", "entropy_"),
                        inline = FALSE,
                        status = "success"
                      ),
                      prettyCheckboxGroup(
                        inputId = "summary_values_selection_age",
                        label = NULL,
                        choiceNames = c("Age PC1", "Age PC2", "Age PC3"),
                        choiceValues = c("_PC1", "_PC2", "_PC3"),
                        selected = c("_PC1", "_PC2", "_PC3"),
                        inline = FALSE,
                        status = "success"
                      ),
                      tags$h5("Sample values"),
                      prettyCheckboxGroup(
                        inputId = "sample_selection",
                        label = NULL,
                        choiceNames = c("N Sample", "N known", "Prop known"),
                        choiceValues = c("n_sample_", "n_known_", "prop_known_"),
                        selected = c("n_sample_", "n_known_", "prop_known_"),
                        inline = FALSE,
                        status = "success"
                      ),
                      tags$h5("Linguistic values"),
                      prettyCheckboxGroup(
                        inputId = "linguistic_selection",
                        label = NULL,
                        choiceNames = c("Word Czech", "Word English", "Grammatical gender", "Animacy", "POS", "Stimuli type"),
                        choiceValues = c("word_czech", "word_english", "gramm_gend", "animacy", "pos", "stim_type"),
                        selected = c("item", "word_czech", "word_english", "gramm_gend", "animacy", "pos", "stim_type"),
                        inline = FALSE,
                        status = "success"
                      ),
                    ),
                    width = 1),

                  ##############
                  #info
                  ##############

                  column(
                    offset = 1,
                    actionBttn(
                      inputId = "infoa",
                      color = "primary",
                      style = "material-circle",
                      size = "s",
                      label = "",
                      icon = icon("info")
                    ),
                    width = 1)
                ),
                
                width = NULL),
            
            div(style = "height:30px"),
            
            fluidRow(
              column(
                plotOutput("row_plot", height = "700px") # Plot output
                ,
                width = 12
                )
            ),
            
            fluidRow(column(uiOutput("downloadPlot"),
                            width = 12))

            ###########
            #ratings plots
            ###########
            # box(uiOutput("word_ratingsa"), width = 0, height = 0),
            # box(uiOutput("word_ratings_agea"), width = NULL)
          
          )
          )
        )
        )
      ),
    
    tags$br(),
    ################
    #bottom matter
    ################
    
    #add in various information about the data and app
    HTML(paste(
      #creative commons
      a(img(src="cc-by-nc.png", width=120, height=42), href="https://creativecommons.org/licenses/by/4.0/", target="_blank"),
      
      #Lancaster University
      a(img(src="UK_logo.png", width=110, height=42), href="https://cuni.cz/UK-1.html", target="_blank"),
      
      #Embodied Cognition Lab
      a(img(src="ercel.png", width=180, height=42), href="https://ercel.ff.cuni.cz/", target="_blank"),
      
      #reuse declaration
      h5("You can distribute, remix, tweak, and build upon this work as long as you credit us for the original creation. Please use the following citation:")))
    )
)

##################
#server
##################

server <- function(input, output, session) {
  
  Modelcl2 <-reactive({
    data.frame()
  })

  observe({
    if(rlang::is_empty(Modelcl2())){
      sendSweetAlert(
        session = session,
        title = NULL,
        text = tags$span(img(src="https://jamesbrandscience.github.io/assets/sociolex.png", width = "40%"),
                         tags$hr(),
                         tags$h3("Vítejte/Welcome"),
                         tags$br(),
                         "This app presents the data for the SocioLex-CZ norms",
                         tags$br(),
                         tags$br(),
                         "You can see a visualisation of the data by clicking on a row",
                         tags$br(),
                         tags$br(),
                         "You can search and filter the items using search 🔍",
                         tags$br(),
                         tags$br(),
                         "You can select displayed variables using settings ⚙️",
                         tags$br(),
                         tags$br(),
                         "The definitions for the variables is on the side with ❓",
                         tags$br(),
                         tags$br(),
                         "You can download the data/visualisation using the download buttons "
                         
        ),
        closeOnClickOutside = TRUE,
        width = "80%",
        type = NULL
      )
    }
  })

  ###############
  #sidebar minimise
  ###############
  
  observeEvent(input$showSidebar, {
    
    if (input$showSidebar == FALSE) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    }
    else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  
  
  #############
  # variable definitions
  #############
  
  output$variable_definitions <- DT::renderDataTable(DT::datatable(
    sociolex_norms_definitions,
    rownames = FALSE,
    escape=FALSE,
    options = list(
      paging = FALSE,
      searching = TRUE,
      ordering = TRUE,
      dom = 'rtip',
      # columnDefs = list(
      #   list(className = 'dt-left', targets = which(sapply(dataset, is.numeric)) - 1)
      # ),
      scroller = TRUE,
      scrollX = TRUE,
      scrollY = "650px",
      info = TRUE,
      autoWidth = FALSE)))
  
  ###############
  #word list
  ###############
  
  observeEvent(input$infoa, {
    sendSweetAlert(
      session = session,
      title = NULL,
      text = tags$span(img(src="https://jamesbrandscience.github.io/assets/sociolex.png", width = "40%"),
                       tags$hr(),
                   tags$h3("Vítejte/Welcome"),
                   tags$br(),
        "This app presents the data for the SocioLex-CZ norms",
        tags$br(),
        tags$br(),
        "You can see a visualisation of the data by clicking on a row",
        tags$br(),
        tags$br(),
        "You can search and filter the items using search 🔍",
        tags$br(),
        tags$br(),
        "You can select displayed variables using settings ⚙️",
        tags$br(),
        tags$br(),
        "The definitions for the variables is on the side with ❓",
        tags$br(),
        tags$br(),
        "You can download the data/visualisation using the download buttons "
        
      ),
      closeOnClickOutside = TRUE,
      width = "80%",
      type = NULL
    )
  })
  
  # observeEvent(input$hover1a,{
  #   req(input$hover1a)
  #   if(input$hover1a == "words"){
  #     hide("wordlist2")
  #     show("wordlist1")
  #   }
  #   if(input$hover1a == "deinitions"){
  #     hide("wordlist1")
  #     show("wordlist2")
  #   }
  # })
  
  output$word_selections <-   renderUI({
    extendedRadioGroup("word_selections", label = "Item selection", choiceNames  = c("Complete dataset", "Text input"),
                       choiceValues = c("all_words", "text_input1"), selected = "all_words"
    )
  })
  
  datasetInput <- reactive({
    #handle line breaks and commas to split each word and store it in a vector called word_input
    word_input <- unlist(strsplit(input$text_input_filter, '\n|,|, '))
    
    dimension_selection1 <- paste0(rep(input$summary_values_selection, each = length(input$dimension_selection)), input$dimension_selection)
    
    dimension_selection2 <- paste0(rep(input$dimension_selection, each = length(input$summary_values_selection_age)), input$summary_values_selection_age)
    
    dimension_selection3 <- paste0(rep(input$sample_selection, each = length(input$dimension_selection)), input$dimension_selection)
    
    sociolex_variables <- names(sociolex_norms_wide)
    
    #make the input to uppercase to match the original data format
    # word_input <- toupper(word_input)
    
    #if the user does not select all_words (i.e. they want to use the text input box), then filter the full dataset kepping those words
    #else use the full dataset
    tryCatch({
    if (input$word_selections == "all_words") {
      sociolex_filtered <- sociolex_norms_wide %>%
        select(item,
               any_of(c(dimension_selection1, dimension_selection2, dimension_selection3, input$linguistic_selection))) %>%
        select(any_of(sociolex_variables))
               # ,
               # any_of(dimension_selection2),
               # any_of(dimension_selection3),
               # any_of(linguistic_selection)
      
      sociolex_filtered
    }
    else {
      sociolex_filtered <- sociolex_norms_wide %>%
        filter(item %in% word_input) %>%
        select(item,
                 any_of(c(dimension_selection1, dimension_selection2, dimension_selection3, input$linguistic_selection))) %>%
        select(any_of(sociolex_variables))
      
      sociolex_filtered
    }
    },
    error = function(e) {
      sociolex_filtered <- sociolex_norms_wide %>%
        select(item,
               any_of(c(dimension_selection1, dimension_selection2, dimension_selection3, input$linguistic_selection))) %>%
        select(any_of(sociolex_variables))
      
      sociolex_filtered
    }
    )
  })
  
  output$wordlist1 <- DT::renderDataTable(server = TRUE,
                                          DT::datatable({
    
    #store the data in a data frame  
    dataset_input1 <<- datasetInput()
    
    dataset_input1
    
  },
  filter = list(position = 'top'),
  rownames = FALSE,
  selection = "single",
  extensions = c("Buttons","FixedColumns"),
  options = list(
    searchHighlight = TRUE,
    selection = "single",
    paging = TRUE,
    ordering = TRUE,
    fixedColumns = list(leftColumns = 1),
    # lengthMenu = list(c(100, 500, 1000), c('100', '500', '1000')),
    pageLength = 20,
    dom = 'rtip',
    # buttons = list(
    #   list(extend = "csv", text = "csv", filename = paste0("sociolex_data_shiny_", format(Sys.time(), "%d-%m-%Y_%H_%M_%S")),
    #        exportOptions = list(
    #          modifier = list(page = "all")
    #        )
    #   ),
    #   list(extend = "excel", text = "xlsx", filename = paste0("sociolex_data_shiny_", format(Sys.time(), "%d-%m-%Y_%H_%M_%S")),
    #        exportOptions = list(
    #          modifier = list(page = "all")
    #        )
    #   )
    # ),
    # buttons = c('copy', 'csv', 'excel'),
    # columnDefs = list(
    #   list(className = 'dt-left', targets = which(sapply(dataset, is.numeric)) - 1)
    # ),
    scroller = TRUE,
    scrollX = TRUE,
    scrollY = "600px",
    info = TRUE,
    autoWidth = FALSE)))
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("sociolex_data_shiny_", format(Sys.time(), "%d-%m-%Y_%H_%M_%S"), ".csv")
    },
    content = function(file) {
      # Subset the filtered data
      filtered_data <- dataset_input1[input$wordlist1_rows_all, ]  # Get filtered rows
      write_delim(filtered_data, file, delim = ",")
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste0("sociolex_data_shiny_", format(Sys.time(), "%d-%m-%Y_%H_%M_%S"), ".xlsx")
    },
    content = function(file) {
      # Subset the filtered data
      filtered_data <- dataset_input1[input$wordlist1_rows_all, ]  # Get filtered rows
      write_xlsx(filtered_data, file)
    }
  )
  
  
  # Render the plot
  output$row_plot <- renderPlot({
    selected_row <- input$wordlist1_rows_selected # Get selected row index
    req(selected_row) # Ensure a row is selected
    
    # Extract the selected row's data
    selected_data <- dataset_input1[selected_row, ]
    
    selected_data_prop <- sociolex_norms_prop %>%
      filter(item == selected_data$item)
    
    selected_data_age_prop <- sociolex_norms_age_prop %>%
      filter(item == selected_data$item) %>%
      mutate(age_category = factor(age_category, ordered = TRUE))
    
    # Generate a ggplot
    sociolex_prop_plot <- ggplot(selected_data_prop, aes(x = rating, y = prop, fill = dimension)) +
      geom_bar(stat = "identity") +
      geom_text(data = plot_labels, aes(label = label)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      facet_grid(dimension~item) +
      theme_bw() +
      theme(legend.position = "none")
    
    sociolex_prop_age_plot <- ggplot(selected_data_age_prop, aes(x = age_category, y = prop, fill = age_category)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_fill_viridis_d() +
      facet_grid(dimension~item) +
      theme_bw() +
      theme(legend.position = "none")
    
    # word_plot <- plot_grid(sociolex_prop_plot, sociolex_prop_age_plot, ncol = 1, rel_heights = c(2.5, 1))
    # 
    # image_plot <- plot_grid(sociolex_prop_plot, ggdraw(ggplot() + theme_nothing() + draw_image(paste0("MultiPic/", selected_data$item, ".png"))), ncol = 1, rel_heights = c(2.5, 1))
    
    if (!str_starts(selected_data$item, "PICTURE_")) {
      plot_selected_row <<- plot_grid(sociolex_prop_plot, sociolex_prop_age_plot, ncol = 1, rel_heights = c(2.5, 1))
      
      plot_selected_row
    }
    else {
      plot_selected_row <<- plot_grid(sociolex_prop_plot, ggdraw(ggplot() + theme_nothing() + draw_image(paste0("MultiPic/", selected_data$item, ".png"))), ncol = 1, rel_heights = c(2.5, 1))
      
      plot_selected_row
    }
    
  })
  
  output$downloadPlot <- renderUI({
    
    selected_row <- input$wordlist1_rows_selected # Get selected row index
    req(selected_row) # Ensure a row is selected
    
    downloadButton(
      outputId = "downloadPlot1",
      label = "Download plot"  # Change the text here
    )
    
  })
  
  output$downloadPlot1 <- downloadHandler(
      filename = function() {
        
        selected_row <- input$wordlist1_rows_selected
        
        selected_data <- dataset_input1[selected_row, ]
        
        selected_data_prop <- sociolex_norms_prop %>%
          filter(item == selected_data$item) %>%
          pull(item)
        
        paste0(selected_data_prop, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plot_selected_row, width = 5, height = 10)
      }
    )
  
  
  
  rowCallback <- c(
    "function(row, data){",
    "  for(var i=0; i<data.length; i++){",
    "    if(data[i] === 'none'){",
    "      $('td:eq('+i+')', row).html('none')",
    "        .css({'color': 'rgb(151,151,151,0.5)', 'font-style': 'italic'});",
    "    }",
    "  }",
    "}"  
  )

}

####################
#run the app
####################

shinyApp(ui = ui, server = server)
