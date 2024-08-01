#package installation and read-ins
#install.packages("bslib")
library(shiny)
library(shinythemes)
library(bslib)
library(fontawesome)

ui <- fluidPage(
  
  #html style to pull in my CSS file - actually just downloaded bootswatch for the theme, no CSS
  theme = bs_theme(bootswatch = "minty"), 
  
  #name the app
  titlePanel(tags$strong("Paige's Page"), "Paige's Page"),
  
  #create the navigation bar - why is this color only covering text? Don't need navbar with tabsetPanels - may re-add if I can animate an icon to walk across it
  #navbarPage(title = span("", style = "background-color: #56cc9d"),
  
  #tab panels
  tabsetPanel(
    
    #tab1 - check out data
    tabPanel(title ="My Data",
             
             #sidebar
             sidebarLayout(
               sidebarPanel( 
                 
                 # Input: File upload for user dataset 
                 fileInput(inputId = "file",
                           label = "Choose a dataset file (CSV):",
                           accept = ".csv"),
                 
                 #option to label and remove missing
                 textInput("code_missing", label = "What are missing values coded as?"),
                 checkboxInput("missing", label = "Remove Missing Data"),
                 
                 # Input: Numeric entry for the number of obs to view globally 
                 numericInput(inputId = "obs_global",
                              label = "Number of dataset rows shown:",
                              value = 5),
                 
                 # Input: Numeric entry for the number of obs to view for the selected variable 
                 numericInput(inputId = "obs_variable",
                              label = "Number of observations of the selected variable:",
                              value = 0),
                 
                 # Input: Selector for choosing a variable for summary table and graph 
                 selectInput(inputId = "variable",
                             label = "Choose a variable:",
                             choices = NULL),
                 
                 #Output: Count of rows and columns
                 span(textOutput("dim"), style = "color: #f3969a")
               ), #close sidebar
               
               mainPanel(
                 
                 navset_pill(  
                   
                   # Output: Verbatim text for data summary 
                   nav_panel("Summary Stats", verbatimTextOutput("summary")),
                   
                   #Output: Simple plots
                   nav_panel("Plots", plotOutput("plots")),
                   
                   #output: Table with requested number of obs after missing removed - should this just be a reactive event tied to the data, and instead updated within view global?
                   nav_panel("Data Set", tableOutput("no_miss"),
                             # Output: HTML table with requested number of observations for the selected variable - with miss unless checkbox clicked 
                             tableOutput("view_variable"),
                             # Output: HTML table with the first 5 rows of the entire dataset 
                             tableOutput("view_global")
                   ),
                 )
               ) #close mainpanel
             ) #close nav_panels
    ), #close tabpanel
    
    #tab2 - quick guide of what to run
    tabPanel("Analysis Descriptions", icon("cat", style = "color: #e83e8c", class = c("fa-spin", "fa-3x")),
             mainPanel(
               navset_pill(
                 nav_panel("Chi-Square", verbatimTextOutput("cs")),
                 nav_panel("Linear Regression", verbatimTextOutput("linr")),
                 nav_panel("Logistic Regression", verbatimTextOutput("logr")),
               )#close navset pill
             ) #close mainpanel   
             
    ),#close tab panel
    
    #tab3 - run the tests
    tabPanel("Run Analyses", icon("cat", style = "color: #6cc3d5", class = c("fa-spin", "fa-3x")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("runanalysis", "Run Analysis", 
                             c("Chi-Square" = "cs_run", "Linear Regression" = "linr_run", 
                               "Logistic Regression" = "logr_run")
                 )
               ),
               
               mainPanel(
                 textOutput("analysis")
               )#close main panel
             )#close sidebar layout
    ),#close tab panel
    
    #tab4 - graphs and visualizations
    tabPanel("Visualizations", icon("cat", style = "color: #6610f2", class = c("fa-spin", "fa-3x")),
             sidebarLayout(
               sidebarPanel(
                 selectInput("graphs", "Graphs", 
                             c("Graph1" = "g1", "Graph2" = "g2", 
                               "Graph3" = "g3")
                 )
               ),
               
               mainPanel(
                 plotOutput("graphs")
               )#close main panel
             )#close sidebar layout
    )#close tab panel
  ) 
)#close tabsetpanel


server <- function(input, output, session) {
  #File tab
  # Reactive expression to read the uploaded dataset 
  uploaded_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update summary variable choices based on the uploaded data - this will be recalibrated if there are changes made to uploaded_data (i.e. if the person removes missing)
  observeEvent(uploaded_data(), {
    var_choices <- names(uploaded_data())
    updateSelectInput(session, "variable", choices = var_choices)
  })
  
  
  #filter data based on state of checkboxInput - if there is input do na.omit if there is not input then just show uploaded data
  filtered_data <- reactive({
    if (input$missing) {
      na.omit(uploaded_data())
    } else {
      uploaded_data()
    }
  })
  
  # Show the first 5 rows of the entire dataset - fix to observeEvent - if I do head(no_miss), it wont show unless the box is clicked but if I do it this way it won't update when uploaded_data changes? 
  #still showing missing values even when remove missing is checked - why isn't this updating with the change in uploaded data?

    output$view_global <- renderTable({
      head(filtered_data(), n = input$obs_global)
    })
  
    # Show the first "n" observations for the selected variable - this is not updating if missing is removed
    output$view_variable <- renderTable({
      head(data.frame(Variable = filtered_data()[, input$variable]), n = input$obs_variable)
    })
    
  
  
  #missing data still showing in global and variable lists
  #change variable title to which variable being shown
  # Generate a summary of the dataset for the selected variable - fix this to be an observeEvent of uploaded data, showing information without checkbox being clicked
  output$summary <- renderPrint({
    var_name <- input$variable
    var <- uploaded_data()[, var_name]
    
    cat(paste("Summary statistics for variable:", var_name), "\n\n")
    
    if (is.factor(var) | is.character(var) | length(unique(var)) <= 2) {
      # Categorical or dichotomous variable
      table_summary <- table(var)
      prop_summary <- prop.table(table_summary)
      percent_summary <- prop.table(table_summary) * 100
      cat_summary <- cbind(table_summary, prop_summary, percent_summary)
      colnames(cat_summary) <- c("Count", "Proportion", "Percent")
      cat_summary
    } else {
      # Continuous variable
      summary(var)
      
    }
  })
  
  # Create a plot based on the type of variable
  output$plots <- renderPlot({
    var_name <- input$variable
    var <- uploaded_data()[, var_name]
    
    if (is.factor(var) | is.character(var) | length(unique(var)) <= 2) {
      # Categorical or dichotomous variable - Create a bar plot
      barplot(table(var), main = paste("Frequency of", var_name), col = "#85C070")
    } else {
      # Continuous variable - Create a histogram
      hist(var, main = paste("Histogram of", var_name), col = "#2E7797", xlab = var_name)
    }
  })
  
  #give dimensions of the data - Change to OBSERVEEVENT to make sure this shows even without checkbox being clicked
  output$dim <- renderText({
    row <- nrow(uploaded_data())
    col <- ncol(uploaded_data())
    paste("There are", 
          (row), 
          "rows and",
          (col),
          "columns")
  })
  
  
  #Data Description tab
  
  #Data analysis tab
  
  #graph tab
  
  
} #close server

# Create Shiny app
shinyApp(ui, server)


