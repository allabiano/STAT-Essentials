# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(rlang)
library(tidyr)
library(stringr)
library(ggpattern) # New library for patterned fills

# Define UI for application that draws a barplot with facets
ui <- fluidPage(
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$style(HTML("
      body {
        font-family: 'Inter', serif;
        background-color: #f0f2f5;
        color: #333;
        padding: 20px;
      }
      .container-fluid {
        max-width: 1200px;
        margin: auto;
        background-color: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        padding: 30px;
      }
      h1 {
        color: #2c3e50;
        text-align: center;
        margin-bottom: 30px;
        font-weight: 700;
      }
      .well {
        background-color: #ecf0f1;
        border: 1px solid #bdc3c7;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .form-group label {
        font-weight: 600;
        margin-bottom: 8px;
        display: block;
      }
      .form-control {
        border-radius: 6px;
        border: 1px solid #ced4da;
        padding: 10px 15px;
        box-shadow: inset 0 1px 2px rgba(0,0,0,.075);
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        transition: background-color 0.3s ease, border-color 0.3s ease;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      .shiny-plot-output {
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        overflow: hidden;
        background-color: #fdfdfd;
      }
      .col-sm-4, .col-sm-8 {
        padding: 15px;
      }
      @media (max-width: 768px) {
        .col-sm-4, .col-sm-8 {
          width: 100%;
          float: none;
        }
        .container-fluid {
          padding: 15px;
        }
      }
    "))
  ),
  
  titlePanel("Faceted Barplot Generator for Multiple Response Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      uiOutput("x_var_selector"), # Selector for X-axis variable
      uiOutput("fill_var_selector"), # Selector for fill variable
      uiOutput("response_vars_selector"),    # Selector for multiple response vars
      tags$hr(),
      helpText("Upload a CSV file. Select a variable for the X-axis, an optional second variable for the bar fill, and multiple response variables for the facets. Bar height will represent the mean value.")
    ),
    
    mainPanel(
      plotOutput("barplot_plot", height = "600px"),
      br(),
      downloadButton("downloadPlot", "Download Plot")
    )
  )
)

# Define server logic required to draw a barplot
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  data_upload <- reactiveVal(NULL)
  
  # Observe file input and read data
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df <- read_csv(input$file1$datapath, show_col_types = FALSE)
      data_upload(df)
    },
    error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      data_upload(NULL)
    })
  })
  
  # Render UI for X-axis variable selection
  output$x_var_selector <- renderUI({
    df <- data_upload()
    if (is.null(df)) {
      return(NULL)
    }
    categorical_cols <- names(df)[!sapply(df, is.numeric)]
    selectInput("x_var_col", "Select Categorical Variable for X-axis:",
                choices = c("None", categorical_cols),
                selected = "None")
  })
  
  # Render UI for fill variable selection
  output$fill_var_selector <- renderUI({
    df <- data_upload()
    if (is.null(df) || input$x_var_col == "None") {
      return(NULL)
    }
    # Exclude the X-axis variable from the choices for fill
    categorical_cols <- setdiff(names(df)[!sapply(df, is.numeric)], input$x_var_col)
    selectInput("fill_var_col", "Select Categorical Variable for Bar Fill (Optional):",
                choices = c("None", categorical_cols),
                selected = "None")
  })
  
  # Render UI for multiple response variables selection
  output$response_vars_selector <- renderUI({
    df <- data_upload()
    if (is.null(df)) {
      return(NULL)
    }
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("response_vars", "Select Response Variables (for Barplots/Facets):",
                choices = numeric_cols,
                multiple = TRUE,
                selected = numeric_cols[1])
  })
  
  # Reactive expression for the plot data, summarized and prepared
  plot_data <- reactive({
    df <- data_upload()
    req(df, input$response_vars)
    
    # Validate selections
    x_var_selected <- input$x_var_col != "None"
    fill_var_selected <- input$fill_var_col != "None"
    
    if (length(input$response_vars) == 0) {
      showNotification("Please select at least one Response Variable.", type = "warning")
      return(NULL)
    }
    
    # Reshape data from wide to long format
    df_long <- df %>%
      pivot_longer(
        cols = all_of(input$response_vars),
        names_to = "Variable",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value))
    
    if (nrow(df_long) == 0) {
      showNotification("No valid data points remaining after filtering for missing values in Response Variables.", type = "warning")
      return(NULL)
    }
    
    # Define grouping variables
    grouping_vars <- c("Variable")
    if (x_var_selected) {
      grouping_vars <- c(grouping_vars, input$x_var_col)
    }
    if (fill_var_selected) {
      grouping_vars <- c(grouping_vars, input$fill_var_col)
    }
    
    # Summarize data based on selected variables
    df_summary <- df_long %>%
      group_by(!!!syms(grouping_vars)) %>%
      summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
    
    # Convert 'Variable' to factor for faceting
    df_summary$Variable <- as.factor(df_summary$Variable)
    
    # Convert x and fill variables to factors for proper plotting
    if (x_var_selected) {
      df_summary[[input$x_var_col]] <- as.factor(df_summary[[input$x_var_col]])
    }
    if (fill_var_selected) {
      df_summary[[input$fill_var_col]] <- as.factor(df_summary[[input$fill_var_col]])
    }
    
    df_summary
  })
  
  # Create a reactive expression for the plot itself
  my_plot <- reactive({
    df_plot <- plot_data()
    
    validate(
      need(!is.null(df_plot), "Please upload a CSV file and select at least one Response Variable.")
    )
    
    x_var_selected <- input$x_var_col != "None"
    fill_var_selected <- input$fill_var_col != "None"
    
    # Create the base ggplot object
    if (x_var_selected) {
      p <- ggplot(df_plot, aes(x = !!sym(input$x_var_col), y = Mean_Value))
    } else {
      # Dummy x-axis if no x variable is selected
      p <- ggplot(df_plot, aes(x = factor(1), y = Mean_Value))
    }
    
    # Use geom_col_pattern for patterned fills
    if (fill_var_selected) {
      p <- p + geom_col_pattern(
        aes(fill = !!sym(input$fill_var_col), pattern = !!sym(input$fill_var_col)),
        stat = "identity",
        position = "dodge",
        color = "black", # A solid black border for each bar
        pattern_fill = "gray80", # A consistent light gray for the pattern
        pattern_colour = "gray20" # A consistent dark gray for the pattern lines
      ) +
        labs(fill = input$fill_var_col)
    } else {
      p <- p + geom_col_pattern(fill = "gray50", stat = "identity")
    }
    
    # Apply faceting
    p <- p + facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
      labs(
        title = "Bar Plot of Mean Values",
        y = "Mean Value"
      ) +
      theme_minimal() +
      # Use grayscale palettes for the legend keys
      scale_fill_grey() +
      scale_pattern_fill_grey() +
      scale_pattern_colour_grey() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),
        axis.text.y = element_text(size = 10),
        axis.text.x = if (x_var_selected) element_text(size = 10, angle = 45, hjust = 1) else element_blank(),
        strip.text = element_text(size = 12, face = "bold", color = "black"),
        strip.background = element_rect(fill = "gray90", color = NA),
        panel.spacing = unit(1.5, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
      )
    
    p
  })
  
  # Render the plot for display
  output$barplot_plot <- renderPlot({
    my_plot()
  })
  
  # Handle the download button logic
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("barplot_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      # Use ggsave to save the plot as a high-quality JPG
      ggsave(file, plot = my_plot(), device = "jpg", width = 8.5, height = 11, dpi = 300)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)