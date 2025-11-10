# Soil Quality Index (SQI) Shiny Application

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(leaflet)
library(readxl)
library(writexl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(sf)
library(terra)
library(gstat)
library(automap)
library(sp)
library(caret)

# Source the normalization function from original script
norm_indicator <- function(x, tipo = "more_is_better", min_obs = NULL, max_obs = NULL, min_optimo = NULL, max_optimo = NULL) {
  if (is.null(min_obs)) min_obs <- min(x, na.rm = TRUE)
  if (is.null(max_obs)) max_obs <- max(x, na.rm = TRUE)
  if (tipo == "more_is_better") {
    return((x - min_obs) / (max_obs - min_obs))
  } else if (tipo == "minus_is_better") {
    return((max_obs - x) / (max_obs - min_obs))
  } else if (tipo == "range_optimo") {
    if (is.null(min_optimo) || is.null(max_optimo)) {
      stop("Must be defined the optimal range for 'range_optimo' normalization.")
    }
    normalized <- ifelse(x >= min_optimo & x <= max_optimo, 1, 0)
    return(normalized)
  } else {
    stop("Type of normalization not recognized. Must be 'more_is_better', 'minus_is_better' or 'range_optimo'.")
  }
}

# Define recommended optimal ranges for common soil variables
default_ranges <- list(
  CEC = c(12.0, 20.0), 
  BD = c(1.2, 1.4),
  EC = c(2.0, 15.0),
  OM = c(1.0, 3.0),
  K = c(300, 800),
  Clay = c(10, 25),
  Silt = c(30, 50),
  PH = c(6.5, 7.5),
  P = c(15, 45),
  Sand = c(40, 60)
)

# Default normalization types
default_norm_types <- c(
  CEC = "more_is_better", 
  BD = "minus_is_better",
  EC = "range_optimo",
  OM = "more_is_better",
  K = "range_optimo",
  Clay = "range_optimo",
  Silt = "range_optimo",
  PH = "range_optimo",
  P = "more_is_better",
  Sand = "range_optimo"
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Soil Quality Index (SQI) Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Guide", tabName = "guide", icon = icon("info-circle")),
      menuItem("1. Input Data", tabName = "input", icon = icon("upload")),
      menuItem("2. Exploratory Data", tabName = "exploratory", icon = icon("chart-line")),
      menuItem("3. PCA Analysis", tabName = "pca", icon = icon("project-diagram")),
      menuItem("4. SQI Calculation", tabName = "sqi", icon = icon("calculator")),
      menuItem("5. Spatial Assessment", tabName = "spatial", icon = icon("map")),
      menuItem("6. Results", tabName = "results", icon = icon("chart-area"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f5f5f5;
        }
        .box {
          margin-bottom: 20px;
        }
        .small-box {
          margin-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      # Guide Tab
      tabItem(tabName = "guide",
        fluidRow(
          box(
            title = "SQI Application Guide", status = "primary", solidHeader = TRUE, width = 12,
            h3("Welcome to the Soil Quality Index Calculator"),
            p("This application implements a comprehensive workflow for calculating and mapping soil quality indices using Principal Component Analysis (PCA) and geostatistical methods."),
            
            h4("Workflow Steps:"),
            tags$ol(
              tags$li(strong("Input Data:"), " Upload your soil data (CSV/XLS) with X, Y coordinates and soil properties"),
              tags$li(strong("Exploratory Analysis:"), " Examine correlations and remove highly correlated variables (>0.98)"),
              tags$li(strong("PCA Analysis:"), " Perform Principal Component Analysis to determine variable weights"),
              tags$li(strong("SQI Calculation:"), " Define normalization parameters and calculate the Soil Quality Index"),
              tags$li(strong("Spatial Assessment:"), " Apply geostatistical interpolation (Kriging or IDW)"),
              tags$li(strong("Results:"), " Visualize spatial distribution and download maps")
            ),
            
            h4("Data Requirements:"),
            tags$ul(
              tags$li("CSV or Excel file with soil property data"),
              tags$li("Mandatory columns: X, Y (coordinates)"),
              tags$li("Soil properties: Any numerical variables (pH, organic matter, texture, nutrients, physical properties, etc.)"),
              tags$li("The application will automatically detect all numerical variables for analysis"),
              tags$li("Study area boundary (optional): GeoJSON or GeoPackage format")
            ),
            
            h4("Normalization Types:"),
            tags$ul(
              tags$li(strong("More is Better:"), " Higher values indicate better soil quality (e.g., organic matter, nutrients)"),
              tags$li(strong("Less is Better:"), " Lower values indicate better soil quality (e.g., bulk density)"),
              tags$li(strong("Optimal Range:"), " Values within a specific range are considered optimal (e.g., pH, texture)")
            ),
            
            h4("Recommended Optimal Ranges:"),
            tags$ul(
              tags$li("pH: 6.5 - 7.5 (optimal for most crops)"),
              tags$li("Clay: 10% - 25% (balanced texture)"),
              tags$li("Silt: 30% - 50% (good water retention)"),
              tags$li("Sand: 40% - 60% (adequate drainage)"),
              tags$li("EC: 2.0 - 15.0 dS/m (moderate salinity range)")
            )
          )
        )
      ),
      
      # Input Data Tab
      tabItem(tabName = "input",
        fluidRow(
          box(
            title = "Data Upload", status = "primary", solidHeader = TRUE, width = 6,
            fileInput("datafile", "Choose CSV/Excel File",
                     accept = c(".csv", ".xlsx", ".xls")),
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
            radioButtons("sep", "Separator",
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                        selected = ","),
            radioButtons("quote", "Quote",
                        choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                        selected = '"')
          ),
          box(
            title = "Coordinate System", status = "info", solidHeader = TRUE, width = 6,
            selectInput("crs_input", "Select Coordinate Reference System:",
                       choices = list(
                         "UTM Zone 18S (EPSG:32718)" = "EPSG:32718",
                         "WGS84 Geographic (EPSG:4326)" = "EPSG:4326",
                         "UTM Zone 19S (EPSG:32719)" = "EPSG:32719",
                         "UTM Zone 20S (EPSG:32720)" = "EPSG:32720",
                         "Custom" = "custom"
                       ), 
                       selected = "EPSG:32718"),
            conditionalPanel(
              condition = "input.crs_input == 'custom'",
              textInput("custom_crs", "Enter EPSG Code:", placeholder = "e.g., EPSG:32718")
            ),
            br(),
            fileInput("boundary_file", "Study Area Boundary (Optional)",
                     accept = c(".geojson", ".gpkg", ".shp"))
          )
        ),
        fluidRow(
          box(
            title = "Data Preview", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("data_preview")
          )
        ),
        fluidRow(
          box(
            title = "Data Summary", status = "info", solidHeader = TRUE, width = 12,
            verbatimTextOutput("data_summary")
          )
        )
      ),
      
      # Exploratory Data Tab
      tabItem(tabName = "exploratory",
        fluidRow(
          box(
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 4,
            h4("Select Variables for Analysis:"),
            uiOutput("variable_selection"),
            br(),
            numericInput("cor_threshold", "Correlation Threshold for Removal:", 
                        value = 0.98, min = 0.5, max = 1, step = 0.01),
            actionButton("remove_correlated", "Remove Highly Correlated Variables", 
                        class = "btn-warning")
          ),
          box(
            title = "Correlation Matrix", status = "success", solidHeader = TRUE, width = 8,
            plotOutput("correlation_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Variable Statistics", status = "info", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("variable_stats")
          )
        ),
        fluidRow(
          box(
            title = "Removed Variables", status = "warning", solidHeader = TRUE, width = 12,
            verbatimTextOutput("removed_variables")
          )
        )
      ),
      
      # PCA Analysis Tab
      tabItem(tabName = "pca",
        fluidRow(
          box(
            title = "PCA Controls", status = "primary", solidHeader = TRUE, width = 4,
            actionButton("run_pca", "Run PCA Analysis", class = "btn-success"),
            br(), br(),
            h4("PCA Parameters:"),
            checkboxInput("scale_data", "Scale Variables", value = TRUE),
            numericInput("eigenvalue_threshold", "Eigenvalue Threshold:", 
                        value = 1, min = 0.1, max = 5, step = 0.1)
          ),
          box(
            title = "Scree Plot", status = "success", solidHeader = TRUE, width = 8,
            plotOutput("scree_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Variable Contributions", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("pca_contributions")
          ),
          box(
            title = "PCA Weights", status = "success", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("pca_weights")
          )
        ),
        fluidRow(
          box(
            title = "PCA Biplot", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("pca_biplot", height = "500px")
          )
        )
      ),
      
      # SQI Calculation Tab
      tabItem(tabName = "sqi",
        fluidRow(
          box(
            title = "Normalization Settings", status = "primary", solidHeader = TRUE, width = 12,
            h4("Configure normalization for each variable:"),
            p("Select the normalization type and define optimal ranges where applicable."),
            uiOutput("normalization_controls")
          )
        ),
        fluidRow(
          box(
            title = "SQI Calculation", status = "success", solidHeader = TRUE, width = 4,
            actionButton("calculate_sqi", "Calculate SQI", class = "btn-success"),
            br(), br(),
            downloadButton("download_sqi_csv", "Download SQI Table (CSV)", class = "btn-info"),
            br(), br(),
            h4("SQI Statistics:"),
            verbatimTextOutput("sqi_stats")
          ),
          box(
            title = "SQI Distribution", status = "info", solidHeader = TRUE, width = 8,
            plotOutput("sqi_histogram", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "SQI by Sample Points", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("sqi_points_plot", height = "400px")
          )
        )
      ),
      
      # Spatial Assessment Tab
      tabItem(tabName = "spatial",
        fluidRow(
          box(
            title = "Interpolation Settings", status = "primary", solidHeader = TRUE, width = 4,
            selectInput("interpolation_method", "Interpolation Method:",
                       choices = list("Ordinary Kriging" = "kriging", 
                                    "Inverse Distance Weighting" = "idw")),
            conditionalPanel(
              condition = "input.interpolation_method == 'kriging'",
              h5("Kriging Parameters:"),
              checkboxInput("auto_variogram", "Automatic Variogram Fitting", value = TRUE)
            ),
            conditionalPanel(
              condition = "input.interpolation_method == 'idw'",
              h5("IDW Parameters:"),
              numericInput("idw_power", "Power Parameter:", value = 2, min = 1, max = 5, step = 0.5),
              numericInput("idw_nmax", "Max. number of points:", value = 10, min = 5, max = 20)
            ),
            numericInput("grid_size", "Grid Cell Size (m):", value = 100, min = 10, max = 1000, step = 10),
            br(),
            actionButton("run_interpolation", "Run Interpolation", class = "btn-success")
          ),
          box(
            title = "Variogram", status = "info", solidHeader = TRUE, width = 8,
            conditionalPanel(
              condition = "input.interpolation_method == 'kriging'",
              plotOutput("variogram_plot", height = "400px")
            ),
            conditionalPanel(
              condition = "input.interpolation_method == 'idw'",
              h4("IDW method selected - no variogram needed")
            )
          )
        ),
        fluidRow(
          box(
            title = "Interpolation Summary", status = "success", solidHeader = TRUE, width = 12,
            verbatimTextOutput("interpolation_summary")
          )
        )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Spatial Distribution Map", status = "primary", solidHeader = TRUE, width = 12,
            leafletOutput("sqi_map", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Download Results", status = "success", solidHeader = TRUE, width = 6,
            h4("Download Options:"),
            downloadButton("download_sqi_data", "Download SQI Data (CSV)", class = "btn-primary"),
            br(), br(),
            downloadButton("download_raster", "Download SQI Raster (GeoTIFF)", class = "btn-info")
          ),
          box(
            title = "SQI Classification", status = "info", solidHeader = TRUE, width = 6,
            h4("SQI Quality Classes:"),
            DT::dataTableOutput("sqi_classification")
          )
        ),
        fluidRow(
          box(
            title = "Final Results Summary", status = "primary", solidHeader = TRUE, width = 12,
            verbatimTextOutput("final_summary")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store data throughout the workflow
  values <- reactiveValues(
    raw_data = NULL,
    selected_vars = NULL,
    final_vars = NULL,
    pca_result = NULL,
    pca_weights = NULL,
    normalized_data = NULL,
    sqi_values = NULL,
    spatial_data = NULL,
    interpolated_result = NULL,
    interpolated_raster = NULL,
    variogram_model = NULL,
    boundary = NULL
  )
  
  # Data Input Section
  observeEvent(input$datafile, {
    req(input$datafile)
    
    ext <- tools::file_ext(input$datafile$datapath)
    
    if(ext == "csv") {
      values$raw_data <- read.csv(input$datafile$datapath, 
                                 header = input$header,
                                 sep = input$sep,
                                 quote = input$quote,
                                 stringsAsFactors = input$stringsAsFactors)
    } else if(ext %in% c("xlsx", "xls")) {
      values$raw_data <- read_excel(input$datafile$datapath)
    }
    
    # Check for required coordinates
    if(!all(c("X", "Y") %in% colnames(values$raw_data))) {
      showNotification("Warning: X and Y coordinate columns are required!", type = "warning")
    }
  })
  
  # Load boundary file
  observeEvent(input$boundary_file, {
    req(input$boundary_file)
    
    ext <- tools::file_ext(input$boundary_file$datapath)
    
    tryCatch({
      if(ext == "geojson") {
        values$boundary <- st_read(input$boundary_file$datapath)
      } else if(ext == "gpkg") {
        values$boundary <- st_read(input$boundary_file$datapath)
      } else if(ext == "shp") {
        # For shapefiles, we need all associated files
        showNotification("Please upload a complete shapefile with all components (.shp, .shx, .dbf)", type = "warning")
      }
      showNotification("Boundary file loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading boundary file:", e$message), type = "error")
    })
  })
  
  output$data_preview <- DT::renderDataTable({
    req(values$raw_data)
    DT::datatable(values$raw_data, options = list(scrollX = TRUE))
  })
  
  output$data_summary <- renderPrint({
    req(values$raw_data)
    summary(values$raw_data)
  })
  
  # Variable Selection UI
  output$variable_selection <- renderUI({
    req(values$raw_data)
    
    # Get numeric variables excluding coordinates
    numeric_vars <- names(values$raw_data)[sapply(values$raw_data, is.numeric)]
    numeric_vars <- numeric_vars[!numeric_vars %in% c("X", "Y")]
    
    if(length(numeric_vars) == 0) {
      return(p("No numeric variables found in the dataset (excluding X, Y coordinates)"))
    }
    
    checkboxGroupInput("selected_variables", "Select Variables:",
                      choices = setNames(numeric_vars, numeric_vars),
                      selected = numeric_vars)
  })
  
  # Correlation Analysis
  observeEvent(input$selected_variables, {
    req(values$raw_data, input$selected_variables)
    values$selected_vars <- input$selected_variables
    
    # Initialize final_vars with selected_vars if not set
    if(is.null(values$final_vars)) {
      values$final_vars <- input$selected_variables
    }
  })
  
  output$correlation_plot <- renderPlot({
    req(values$raw_data, values$selected_vars)
    
    tryCatch({
      cor_data <- values$raw_data[, values$selected_vars, drop = FALSE]
      cor_data <- cor_data[complete.cases(cor_data), ]
      
      if(nrow(cor_data) < 3) {
        plot.new()
        text(0.5, 0.5, "Insufficient data for correlation plot", cex = 1.5)
        return()
      }
      
      cor_matrix <- cor(cor_data, use = "complete.obs")
      
      if(any(is.na(cor_matrix)) || any(!is.finite(cor_matrix))) {
        plot.new()
        text(0.5, 0.5, "Unable to calculate correlations", cex = 1.5)
        return()
      }
      
      corrplot(cor_matrix, method = "circle", type = "upper", 
               order = "hclust", tl.cex = 0.8, tl.col = "black",
               addCoef.col = "black", number.cex = 0.7)
               
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error creating correlation plot", cex = 1.5)
    })
  })
  
  # Remove highly correlated variables
  observeEvent(input$remove_correlated, {
    req(values$raw_data, values$selected_vars)
    
    tryCatch({
      # Show progress notification
      showNotification("Analyzing correlations...", type = "default", duration = 2)
      
      cor_data <- values$raw_data[, values$selected_vars, drop = FALSE]
      
      # Remove rows with missing values
      cor_data <- cor_data[complete.cases(cor_data), ]
      
      # Check if we have enough data
      if(nrow(cor_data) < 3) {
        showNotification("Insufficient data for correlation analysis", type = "error")
        return()
      }
      
      # Calculate correlation matrix
      cor_matrix <- cor(cor_data, use = "complete.obs")
      
      # Check for invalid correlations
      if(any(is.na(cor_matrix)) || any(!is.finite(cor_matrix))) {
        showNotification("Unable to calculate correlations - check your data", type = "error")
        return()
      }
      
      # Find highly correlated pairs using a custom approach if caret::findCorrelation fails
      highly_correlated <- tryCatch({
        findCorrelation(cor_matrix, cutoff = input$cor_threshold)
      }, error = function(e) {
        # Alternative approach: find pairs manually
        cor_matrix_upper <- cor_matrix
        cor_matrix_upper[lower.tri(cor_matrix_upper, diag = TRUE)] <- 0
        
        high_cor_pairs <- which(abs(cor_matrix_upper) > input$cor_threshold, arr.ind = TRUE)
        
        if(nrow(high_cor_pairs) > 0) {
          # Remove the variable with higher average correlation
          to_remove <- c()
          for(i in 1:nrow(high_cor_pairs)) {
            var1 <- high_cor_pairs[i, 1]
            var2 <- high_cor_pairs[i, 2]
            
            avg_cor1 <- mean(abs(cor_matrix[var1, -var1]))
            avg_cor2 <- mean(abs(cor_matrix[var2, -var2]))
            
            if(avg_cor1 > avg_cor2) {
              to_remove <- c(to_remove, var1)
            } else {
              to_remove <- c(to_remove, var2)
            }
          }
          return(unique(to_remove))
        } else {
          return(integer(0))
        }
      })
      
      if(length(highly_correlated) > 0) {
        removed_vars <- values$selected_vars[highly_correlated]
        values$final_vars <- values$selected_vars[-highly_correlated]
        
        output$removed_variables <- renderText({
          paste("Removed variables:", paste(removed_vars, collapse = ", "))
        })
        
        showNotification(paste("Removed", length(removed_vars), "highly correlated variables"), type = "message")
      } else {
        values$final_vars <- values$selected_vars
        output$removed_variables <- renderText("No variables removed - all correlations below threshold")
        showNotification("No highly correlated variables found", type = "message")
      }
      
    }, error = function(e) {
      showNotification(paste("Error in correlation analysis:", e$message), type = "error")
      # Fallback: use all selected variables
      values$final_vars <- values$selected_vars
      output$removed_variables <- renderText("Error in correlation analysis - using all selected variables")
    })
  })
  
  output$variable_stats <- DT::renderDataTable({
    req(values$raw_data, values$final_vars)
    
    stats_data <- values$raw_data[, values$final_vars, drop = FALSE]
    stats_summary <- data.frame(
      Variable = names(stats_data),
      Mean = round(sapply(stats_data, mean, na.rm = TRUE), 3),
      SD = round(sapply(stats_data, sd, na.rm = TRUE), 3),
      Min = round(sapply(stats_data, min, na.rm = TRUE), 3),
      Max = round(sapply(stats_data, max, na.rm = TRUE), 3),
      Missing = sapply(stats_data, function(x) sum(is.na(x)))
    )
    
    DT::datatable(stats_summary, options = list(pageLength = 15))
  })
  
  # PCA Analysis
  observeEvent(input$run_pca, {
    req(values$raw_data, values$final_vars)
    
    pca_data <- values$raw_data[, values$final_vars, drop = FALSE]
    pca_data <- pca_data[complete.cases(pca_data), ]
    
    if(input$scale_data) {
      pca_data_scaled <- scale(pca_data)
    } else {
      pca_data_scaled <- pca_data
    }
    
    values$pca_result <- PCA(pca_data_scaled, graph = FALSE)
    
    # Calculate weights based on significant PCs
    eigenvalues <- get_eigenvalue(values$pca_result)
    significant_pcs <- which(eigenvalues[, "eigenvalue"] > input$eigenvalue_threshold)
    
    if(length(significant_pcs) > 0) {
      explained_var <- eigenvalues[significant_pcs, "variance.percent"]
      explained_var_norm <- explained_var / sum(explained_var)
      
      var_contrib <- get_pca_var(values$pca_result)$contrib
      weight_pca <- numeric(ncol(pca_data))
      
      for(i in 1:length(significant_pcs)) {
        pc <- significant_pcs[i]
        weight_pca <- weight_pca + (var_contrib[, pc] * explained_var_norm[i] / 100)
      }
      
      weight_pca <- weight_pca / sum(weight_pca)
      names(weight_pca) <- colnames(pca_data)
      values$pca_weights <- weight_pca
    } else {
      # Equal weights if no significant PCs
      weight_pca <- rep(1/ncol(pca_data), ncol(pca_data))
      names(weight_pca) <- colnames(pca_data)
      values$pca_weights <- weight_pca
    }
    
    showNotification("PCA analysis completed successfully!", type = "message")
  })
  
  output$scree_plot <- renderPlot({
    req(values$pca_result)
    fviz_eig(values$pca_result, addlabels = TRUE, ylim = c(0, 80))
  })
  
  output$pca_biplot <- renderPlot({
    req(values$pca_result)
    fviz_pca_var(values$pca_result, col.var = "contrib", 
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE)
  })
  
  output$pca_contributions <- DT::renderDataTable({
    req(values$pca_result)
    var_contrib <- get_pca_var(values$pca_result)$contrib
    DT::datatable(round(var_contrib, 2), options = list(pageLength = 15))
  })
  
  output$pca_weights <- DT::renderDataTable({
    req(values$pca_weights)
    weights_df <- data.frame(
      Variable = names(values$pca_weights),
      Weight = round(values$pca_weights, 4)
    )
    DT::datatable(weights_df, options = list(pageLength = 15))
  })
  
  # Normalization Controls UI
  output$normalization_controls <- renderUI({
    req(values$final_vars)
    
    controls_list <- list()
    
    for(i in seq_along(values$final_vars)) {
      var_name <- values$final_vars[i]
      
      # Default normalization type
      default_type <- ifelse(var_name %in% names(default_norm_types), 
                           default_norm_types[[var_name]], "more_is_better")
      
      # Default range
      default_range <- ifelse(var_name %in% names(default_ranges),
                            list(default_ranges[[var_name]]), list(c(0, 1)))[[1]]
      
      controls_list[[i]] <- box(
        title = var_name, status = "info", solidHeader = TRUE, width = 4,
        selectInput(paste0("norm_type_", var_name), "Normalization Type:",
                   choices = list("More is Better" = "more_is_better",
                                "Less is Better" = "minus_is_better", 
                                "Optimal Range" = "range_optimo"),
                   selected = default_type),
        conditionalPanel(
          condition = paste0("input.norm_type_", var_name, " == 'range_optimo'"),
          numericInput(paste0("min_range_", var_name), "Min Optimal:", 
                      value = default_range[1], step = 0.1),
          numericInput(paste0("max_range_", var_name), "Max Optimal:", 
                      value = default_range[2], step = 0.1)
        )
      )
    }
    
    do.call(fluidRow, controls_list)
  })
  
  # Calculate SQI
  observeEvent(input$calculate_sqi, {
    req(values$raw_data, values$final_vars, values$pca_weights)
    
    sqi_data <- values$raw_data[, values$final_vars, drop = FALSE]
    sqi_data <- sqi_data[complete.cases(sqi_data), ]
    
    # Normalize each variable
    normalized_list <- list()
    
    for(var_name in values$final_vars) {
      norm_type <- input[[paste0("norm_type_", var_name)]]
      
      if(norm_type == "range_optimo") {
        min_opt <- input[[paste0("min_range_", var_name)]]
        max_opt <- input[[paste0("max_range_", var_name)]]
        normalized_list[[var_name]] <- norm_indicator(sqi_data[[var_name]], 
                                                     tipo = norm_type,
                                                     min_optimo = min_opt, 
                                                     max_optimo = max_opt)
      } else {
        normalized_list[[var_name]] <- norm_indicator(sqi_data[[var_name]], tipo = norm_type)
      }
    }
    
    values$normalized_data <- as.data.frame(normalized_list)
    
    # Calculate weighted SQI
    values$sqi_values <- rowSums(values$normalized_data * values$pca_weights[values$final_vars])
    
    showNotification("SQI calculation completed successfully!", type = "message")
  })
  
  output$sqi_stats <- renderPrint({
    req(values$sqi_values)
    summary(values$sqi_values)
  })
  
  output$sqi_histogram <- renderPlot({
    req(values$sqi_values)
    hist(values$sqi_values, breaks = 20, main = "SQI Distribution", 
         xlab = "SQI Values", col = "lightblue", border = "black")
  })
  
  output$sqi_points_plot <- renderPlot({
    req(values$sqi_values)
    plot_data <- data.frame(Sample = 1:length(values$sqi_values), 
                           SQI = values$sqi_values)
    
    ggplot(plot_data, aes(x = Sample, y = SQI)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_hline(yintercept = mean(values$sqi_values), linetype = "dashed", color = "red") +
      labs(title = "Soil Quality Index by Sample Point", 
           x = "Sample Point", y = "SQI") +
      theme_minimal()
  })
  
  # Spatial Interpolation
  observeEvent(input$run_interpolation, {
    req(values$raw_data, values$sqi_values)
    
    # Check coordinates
    if(!all(c("X", "Y") %in% colnames(values$raw_data))) {
      showNotification("Error: X and Y coordinates are required for spatial interpolation!", type = "error")
      return()
    }
    
    # Prepare spatial data
    complete_rows <- complete.cases(values$raw_data[, values$final_vars])
    spatial_df <- data.frame(
      X = values$raw_data$X[complete_rows],
      Y = values$raw_data$Y[complete_rows],
      SQI = values$sqi_values
    )
    
    # Get CRS
    crs_code <- ifelse(input$crs_input == "custom", input$custom_crs, input$crs_input)
    
    # Create spatial object
    spatial_sf <- st_as_sf(spatial_df, coords = c("X", "Y"), crs = crs_code)
    values$spatial_data <- spatial_sf
    
    # Create prediction grid
    bbox <- st_bbox(spatial_sf)
    grid_x <- seq(bbox[1], bbox[3], by = input$grid_size)
    grid_y <- seq(bbox[2], bbox[4], by = input$grid_size)
    pred_grid <- expand.grid(X = grid_x, Y = grid_y)
    pred_grid_sf <- st_as_sf(pred_grid, coords = c("X", "Y"), crs = crs_code)
    
    # Clip to boundary if available
    if(!is.null(values$boundary)) {
      pred_grid_sf <- st_intersection(pred_grid_sf, values$boundary)
    }
    
    # Perform interpolation
    if(input$interpolation_method == "kriging") {
      # Kriging
      if(input$auto_variogram) {
        vgm_fit <- autofitVariogram(SQI ~ 1, spatial_sf)
        values$variogram_model <- vgm_fit
      }
      
      interpolated <- krige(SQI ~ 1, spatial_sf, pred_grid_sf, 
                           model = values$variogram_model$var_model)
      
    } else {
      # IDW
      interpolated <- gstat::idw(SQI ~ 1, spatial_sf, pred_grid_sf, 
                               idp = input$idw_power, nmax = input$idw_nmax)
    }
    
    values$interpolated_result <- interpolated
    
    # Convert interpolated points to raster
    tryCatch({
      # Extract coordinates and values
      coords <- st_coordinates(interpolated)
      sqi_values <- interpolated$var1.pred
      
      # Create a regular grid
      x_coords <- coords[, 1]
      y_coords <- coords[, 2]
      
      # Create raster from interpolated points
      if(length(x_coords) > 0 && length(y_coords) > 0) {
        # Create a SpatRaster
        raster_extent <- c(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
        
        # Calculate raster dimensions based on grid size
        ncols <- ceiling((raster_extent[2] - raster_extent[1]) / input$grid_size)
        nrows <- ceiling((raster_extent[4] - raster_extent[3]) / input$grid_size)
        
        # Create empty raster
        sqi_raster <- rast(extent = raster_extent, ncols = ncols, nrows = nrows, crs = crs_code)
        
        # Rasterize the interpolated points
        interpolated_vect <- vect(interpolated)
        sqi_raster_filled <- rasterize(interpolated_vect, sqi_raster, field = "var1.pred", fun = mean)
        
        # Apply mask if boundary is available
        if(!is.null(values$boundary)) {
          tryCatch({
            # Transform boundary to same CRS as raster if needed
            boundary_transformed <- st_transform(values$boundary, crs = crs_code)
            boundary_vect <- vect(boundary_transformed)
            
            # Mask the raster to boundary
            values$interpolated_raster <- mask(sqi_raster_filled, boundary_vect)
            
            showNotification("Spatial interpolation, rasterization, and masking completed successfully!", type = "message")
          }, error = function(mask_error) {
            # If masking fails, use unmasked raster
            values$interpolated_raster <- sqi_raster_filled
            showNotification(paste("Masking failed, using full raster:", mask_error$message), type = "warning")
          })
        } else {
          # No boundary available, use full raster
          values$interpolated_raster <- sqi_raster_filled
          showNotification("Spatial interpolation and rasterization completed successfully!", type = "message")
        }
      } else {
        showNotification("Warning: No valid coordinates for rasterization", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Warning: Rasterization failed:", e$message), type = "warning")
      showNotification("Spatial interpolation completed (points only)", type = "message")
    })
  })
  
  output$variogram_plot <- renderPlot({
    req(values$variogram_model)
    plot(values$variogram_model)
  })
  
  output$interpolation_summary <- renderPrint({
    req(values$interpolated_result)
    cat("Interpolation completed\n")
    cat("Method:", input$interpolation_method, "\n")
    cat("Grid size:", input$grid_size, "m\n")
    cat("Number of prediction points:", nrow(values$interpolated_result), "\n")
    cat("Predicted SQI range:", round(range(values$interpolated_result$var1.pred, na.rm = TRUE), 3), "\n")
    
    if(!is.null(values$interpolated_raster)) {
      cat("Raster created successfully\n")
      cat("Raster dimensions:", dim(values$interpolated_raster)[1], "x", dim(values$interpolated_raster)[2], "cells\n")
    } else {
      cat("Raster creation failed - displaying points only\n")
    }
  })
  
  # Results Map
  output$sqi_map <- renderLeaflet({
    req(values$interpolated_result)
    
    # Define custom SQI classification breaks and colors
    sqi_breaks <- c(0, 0.1, 0.3, 0.5, 1.0)
    sqi_labels <- c("Very Low (<0.1)", "Low (0.1-0.3)", "Medium (0.3-0.5)", "Optimum (>0.5)")
    sqi_colors <- c("#d73027", "#fc8d59", "#fee08b", "#91cf60")  # Red, Orange, Yellow, Green
    
    # Create base map
    map <- leaflet() %>% addTiles()
    
    # Try to add raster layer if available
    if(!is.null(values$interpolated_raster)) {
      tryCatch({
        # Create color palette for continuous values
        sqi_values_raster <- values(values$interpolated_raster, na.rm = TRUE)
        pal_continuous <- colorNumeric(palette = sqi_colors, domain = range(sqi_values_raster, na.rm = TRUE))
        
        # Add raster layer
        map <- map %>%
          addRasterImage(values$interpolated_raster, 
                        colors = pal_continuous, 
                        opacity = 0.7,
                        group = "SQI Raster") %>%
          addLegend(pal = pal_continuous, 
                   values = sqi_values_raster,
                   title = "SQI Values",
                   opacity = 1,
                   group = "SQI Raster")
      }, error = function(e) {
        # Fallback to point display
        showNotification(paste("Raster display failed, showing points:", e$message), type = "warning")
      })
    }
    
    # Always add sample points as an overlay
    tryCatch({
      # Convert to regular data frame with coordinates
      result_coords <- st_coordinates(values$interpolated_result)
      result_df <- data.frame(
        lon = result_coords[, 1],
        lat = result_coords[, 2],
        sqi = values$interpolated_result$var1.pred
      )
      
      # Classify SQI values for points
      result_df$sqi_class <- cut(result_df$sqi, breaks = sqi_breaks, 
                                labels = sqi_labels, include.lowest = TRUE, right = FALSE)
      
      # Create color palette for classification
      pal_factor <- colorFactor(palette = sqi_colors, domain = sqi_labels)
      
      # Add point layer
      map <- map %>%
        addCircleMarkers(
          data = result_df,
          lng = ~lon, lat = ~lat,
          color = ~pal_factor(sqi_class),
          popup = ~paste("SQI:", round(sqi, 3), "<br>Class:", sqi_class),
          radius = 4,
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          group = "Sample Points"
        )
        
      # Add layer control if raster exists
      if(!is.null(values$interpolated_raster)) {
        map <- map %>%
          addLayersControl(
            overlayGroups = c("SQI Raster", "Sample Points"),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
    }, error = function(e) {
      showNotification(paste("Point display failed:", e$message), type = "error")
    })
    
    return(map)
  })
  
  # SQI Classification
  output$sqi_classification <- DT::renderDataTable({
    req(values$sqi_values)
    
    # Define custom SQI classification breaks and labels
    sqi_breaks <- c(0, 0.1, 0.3, 0.5, 1.0)
    sqi_labels <- c("Very Low", "Low", "Medium", "Optimum")
    sqi_ranges <- c("< 0.1", "0.1 - 0.3", "0.3 - 0.5", "> 0.5")
    
    # Classify SQI values using custom breaks
    sqi_classified <- cut(values$sqi_values, breaks = sqi_breaks, 
                         labels = sqi_labels, include.lowest = TRUE, right = FALSE)
    
    # Count samples in each class
    class_counts <- table(sqi_classified)
    
    # Calculate percentages
    class_percentages <- round((class_counts / length(values$sqi_values)) * 100, 1)
    
    classification_df <- data.frame(
      Class = sqi_labels,
      Range = sqi_ranges,
      Count = as.numeric(class_counts),
      Percentage = paste0(class_percentages, "%")
    )
    
    DT::datatable(classification_df, options = list(pageLength = 5, dom = 't'),
                  rownames = FALSE) %>%
      DT::formatStyle("Class",
        backgroundColor = DT::styleEqual(sqi_labels, c("#ffebee", "#fff3e0", "#fffde7", "#e8f5e8")))
  })
  
  output$final_summary <- renderPrint({
    req(values$sqi_values, values$interpolated_result)
    
    cat("=== SOIL QUALITY INDEX ANALYSIS SUMMARY ===\n\n")
    cat("Dataset Information:\n")
    cat("- Total samples:", length(values$sqi_values), "\n")
    cat("- Variables analyzed:", length(values$final_vars), "\n")
    cat("- Interpolation method:", input$interpolation_method, "\n\n")
    
    cat("SQI Statistics:\n")
    cat("- Mean SQI:", round(mean(values$sqi_values), 3), "\n")
    cat("- Standard deviation:", round(sd(values$sqi_values), 3), "\n")
    cat("- Range:", round(min(values$sqi_values), 3), "-", round(max(values$sqi_values), 3), "\n\n")
    
    cat("Variables and Weights:\n")
    for(i in seq_along(values$final_vars)) {
      var_name <- values$final_vars[i]
      weight <- round(values$pca_weights[var_name], 4)
      cat("-", var_name, ":", weight, "\n")
    }
  })
  
  # Download handlers
  output$download_sqi_data <- downloadHandler(
    filename = function() {
      paste("sqi_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$raw_data, values$sqi_values)
      
      complete_rows <- complete.cases(values$raw_data[, values$final_vars])
      result_data <- values$raw_data[complete_rows, ]
      result_data$SQI <- values$sqi_values
      
      write.csv(result_data, file, row.names = FALSE)
    }
  )

  # Download handler for SQI Calculator section
  output$download_sqi_csv <- downloadHandler(
    filename = function() {
      paste("sqi_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$raw_data, values$sqi_values)

      complete_rows <- complete.cases(values$raw_data[, values$final_vars])
      result_data <- values$raw_data[complete_rows, ]
      result_data$SQI <- values$sqi_values

      write.csv(result_data, file, row.names = FALSE)
    }
  )

  output$download_raster <- downloadHandler(
    filename = function() {
      paste("sqi_raster_", Sys.Date(), ".tif", sep = "")
    },
    content = function(file) {
      req(values$interpolated_raster)
      
      tryCatch({
        # Save the raster as GeoTIFF
        writeRaster(values$interpolated_raster, file, overwrite = TRUE)
        showNotification("GeoTIFF raster exported successfully!", type = "message")
      }, error = function(e) {
        # Fallback: create raster from interpolated points if main raster failed
        if(!is.null(values$interpolated_result)) {
          tryCatch({
            # Extract coordinates and values
            coords <- st_coordinates(values$interpolated_result)
            sqi_values <- values$interpolated_result$var1.pred
            
            # Get CRS
            crs_code <- ifelse(input$crs_input == "custom", input$custom_crs, input$crs_input)
            
            # Create a regular grid for rasterization
            x_coords <- coords[, 1]
            y_coords <- coords[, 2]
            
            if(length(x_coords) > 0 && length(y_coords) > 0) {
              # Create raster extent
              raster_extent <- c(min(x_coords), max(x_coords), min(y_coords), max(y_coords))
              
              # Calculate raster dimensions
              ncols <- ceiling((raster_extent[2] - raster_extent[1]) / input$grid_size)
              nrows <- ceiling((raster_extent[4] - raster_extent[3]) / input$grid_size)
              
              # Create empty raster
              sqi_raster <- rast(extent = raster_extent, ncols = ncols, nrows = nrows, crs = crs_code)
              
              # Rasterize the interpolated points
              interpolated_vect <- vect(values$interpolated_result)
              rasterized <- rasterize(interpolated_vect, sqi_raster, field = "var1.pred", fun = mean)
              
              # Apply mask if boundary is available
              if(!is.null(values$boundary)) {
                tryCatch({
                  # Transform boundary to same CRS as raster if needed
                  boundary_transformed <- st_transform(values$boundary, crs = crs_code)
                  boundary_vect <- vect(boundary_transformed)
                  
                  # Mask the raster to boundary
                  final_raster <- mask(rasterized, boundary_vect)
                }, error = function(mask_error) {
                  # If masking fails, use unmasked raster
                  final_raster <- rasterized
                })
              } else {
                # No boundary available, use full raster
                final_raster <- rasterized
              }
              
              # Save the raster
              writeRaster(final_raster, file, overwrite = TRUE)
              showNotification("GeoTIFF raster created and exported successfully!", type = "message")
            } else {
              stop("No valid coordinates for raster creation")
            }
          }, error = function(e2) {
            showNotification(paste("Error creating GeoTIFF:", e2$message), type = "error")
          })
        } else {
          showNotification("No interpolated data available for raster export", type = "error")
        }
      })
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)