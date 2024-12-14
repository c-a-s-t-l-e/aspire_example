library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(leaflet)
library(withr)
library(rmarkdown)
library(tinytex)

# Example datasets
available_datasets <- list(
  "iris" = iris,
  "mtcars" = mtcars
)

# Define global reactive value
global_filtered_data <- reactiveVal(NULL)

ui <- navbarPage(
  theme = bs_theme(version = 5),
  title = "Data Explorer",
  
  # Data Selection Tab
  tabPanel(
    "Data Selection",
    page_sidebar(
      sidebar = sidebar(
        fileInput("file", "Upload CSV File",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        selectInput("dataset_choice", "Or Choose Example Dataset:",
                    choices = c("upload", names(available_datasets))),
        hr(),
        h4("Filters"),
        uiOutput("dynamic_filters")
      ),
      card(
        card_header("Data Preview"),
        DTOutput("data_preview"),
        card_footer(
          downloadButton("download_data", "Download Filtered Data")
        ),
      ),
      card(
        card_header("Select Columns by Type"),
        uiOutput("column_selector_by_type")
      )
    )
  ),
  
  # Graph Generator Tab
  tabPanel(
    "Graphs",
    page_sidebar(
      # Replace the existing Graphs tabPanel sidebar content with:
      sidebar = sidebar(
        selectInput("plot_type", "Plot Type:",
                    choices = c("Scatter Plot", "Box Plot", "Histogram", "Bar Plot")),
        uiOutput("column_selector"),
        uiOutput("plot_options"),
        uiOutput("aesthetic_controls"),  # New UI output for aesthetic controls
        textInput("plot_title", "Plot Title:", value = "My Plot"),
        textInput("x_axis_label", "X-axis Label:", value = "X"),
        textInput("y_axis_label", "Y-axis Label:", value = "Y"),
        downloadButton("download_plot", "Download Plot", class = "btn-primary")
      ),
      card(
        card_header("Visualization"),
        plotOutput("plot")
      ),
      card(
        card_header("Statistical Analysis"),
        verbatimTextOutput("stats_tests")
      )
    )
  ),
  # Report Generator Tab
  tabPanel(
    "Report Generator",
    page_sidebar(
      sidebar = sidebar(
        textInput("report_title", "Report Title", value = "Data Analysis Report"),
        textAreaInput("report_description", "Report Description", 
                      height = "100px",
                      value = "This report summarizes the analysis of our dataset."),
        actionButton("save_current_plot", "Add Current Plot to Report", class = "btn-success"),
        hr(),
        checkboxInput("include_data_summary", "Include Data Summary", TRUE),
        checkboxInput("include_stats", "Include Statistical Analysis", TRUE),
        actionButton("clear_report", "Clear Report", class = "btn-danger"),
        hr(),
        downloadButton("download_report", "Download Report", class = "btn-primary")
      ),
      card(
        card_header("Report Preview"),
        card_body(
          h2(textOutput("preview_title")),
          pre(style = "white-space: pre-wrap; font-family: inherit;", 
              textOutput("preview_description")),
          uiOutput("data_summary_preview"),
          hr(),
          uiOutput("saved_plots_preview"),
          verbatimTextOutput("report_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the current dataset
  current_data <- reactiveVal(NULL)
  # Reactive value to store the filtered dataset
  filtered_data <- reactiveVal(NULL)
  # Add near the top of the server function
  input_data_summary <- reactiveVal(TRUE)  # Default to TRUE
  
  # Update data when file is uploaded or dataset is selected
  observeEvent(list(input$dataset_choice, input$file), {
    if (input$dataset_choice == "upload") {
      if (!is.null(input$file)) {
        data <- read.csv(input$file$datapath)
        current_data(data)
        filtered_data(data)
        global_filtered_data(data)
      }
    } else {
      data <- available_datasets[[input$dataset_choice]]
      current_data(data)
      filtered_data(data)
      global_filtered_data(data)
    }
  })
  
  # Function to get columns grouped by type
  get_columns_by_type <- reactive({
    req(current_data())
    data <- current_data()
    
    list(
      numeric = names(data)[sapply(data, is.numeric)],
      factor = names(data)[sapply(data, is.factor)],
      character = names(data)[sapply(data, is.character)],
      other = names(data)[!sapply(data, function(x) 
        is.numeric(x) || is.factor(x) || is.character(x))]
    )
  })
  
  # Render column selector grouped by type
  # Add this output in the Data Selection tab's UI
  output$column_selector_by_type <- renderUI({
    req(current_data())
    cols_by_type <- get_columns_by_type()
    
    tagList(
      checkboxGroupInput("selected_numeric", "Numeric Columns:",
                         choices = cols_by_type$numeric,
                         selected = cols_by_type$numeric),
      checkboxGroupInput("selected_factor", "Factor Columns:",
                         choices = cols_by_type$factor,
                         selected = cols_by_type$factor),
      checkboxGroupInput("selected_character", "Character Columns:",
                         choices = cols_by_type$character,
                         selected = cols_by_type$character),
      checkboxGroupInput("selected_other", "Other Columns:",
                         choices = cols_by_type$other,
                         selected = cols_by_type$other)
    )
  })
  
  # Combine all selected columns
  # Update the selected_cols reactive
  selected_cols <- reactive({
    req(current_data())
    unique(c(
      input$selected_numeric,
      input$selected_factor,
      input$selected_character,
      input$selected_other
    ))
  })
  
  # Update the observe block that applies filters
  observe({
    req(current_data())
    data <- current_data()
    cols <- selected_cols()
    
    if (length(cols) > 0) {
      filtered <- data
      
      for (col in cols) {
        filter_input <- input[[paste0("filter_", col)]]
        
        if (!is.null(filter_input)) {
          if (is.numeric(data[[col]])) {
            filtered <- filtered[
              filtered[[col]] >= filter_input[1] & 
                filtered[[col]] <= filter_input[2], , drop = FALSE
            ]
          } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
            if (length(filter_input) > 0) {
              filtered <- filtered[filtered[[col]] %in% filter_input, , drop = FALSE]
            }
          }
        }
      }
      
      filtered_data(filtered)
      global_filtered_data(filtered)
    } else {
      filtered_data(data)
      global_filtered_data(data)
    }
  })
  
  # Generate dynamic filters
  output$dynamic_filters <- renderUI({
    req(current_data())
    data <- current_data()
    cols <- selected_cols()
    
    filters <- lapply(cols, function(col) {
      if (is.numeric(data[[col]])) {
        rng <- range(data[[col]], na.rm = TRUE)
        sliderInput(
          inputId = paste0("filter_", col),
          label = paste("Filter", col),
          min = rng[1],
          max = rng[2],
          value = rng
        )
      } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
        choices <- unique(data[[col]])
        selectInput(
          inputId = paste0("filter_", col),
          label = paste("Filter", col),
          choices = choices,
          multiple = TRUE,
          selected = choices
        )
      }
    })
    
    tagList(filters)
  })
  
  # Apply filters and update filtered data
  observe({
    req(current_data())
    data <- current_data()
    cols <- selected_cols()
    
    if (length(cols) > 0) {
      filtered <- data[, cols, drop = FALSE]
      
      for (col in cols) {
        filter_input <- input[[paste0("filter_", col)]]
        
        if (!is.null(filter_input)) {
          if (is.numeric(filtered[[col]])) {
            filtered <- filtered[
              filtered[[col]] >= filter_input[1] & 
                filtered[[col]] <= filter_input[2],
            ]
          } else if (is.factor(filtered[[col]]) || is.character(filtered[[col]])) {
            if (length(filter_input) > 0) {
              filtered <- filtered[filtered[[col]] %in% filter_input, ]
            }
          }
        }
      }
      
      filtered_data(filtered)
      global_filtered_data(filtered)
    } else {
      filtered_data(data)
      global_filtered_data(data)
    }
  })
  
  # Render data preview
  output$data_preview <- renderDT({
    req(filtered_data())
    datatable(filtered_data(),
              options = list(pageLength = 10,
                             scrollX = TRUE))
  })
  
  # Download handler for filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(filtered_data())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Graph tab functionality using filtered data
  output$column_selector <- renderUI({
    req(filtered_data())
    data <- filtered_data()
    num_cols <- names(data)[sapply(data, is.numeric)]
    
    tagList(
      selectInput("x_var", "X Variable:", choices = names(data)),
      conditionalPanel(
        condition = "input.plot_type == 'Scatter Plot'",
        selectInput("y_var", "Y Variable:", choices = num_cols)
      )
    )
  })
  
  output$plot_options <- renderUI({
    req(input$plot_type, filtered_data())
    
    if (input$plot_type == "Histogram") {
      tagList(
        sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30),
        checkboxInput("density", "Show density curve", FALSE)
      )
    } else if (input$plot_type == "Bar Plot") {
      checkboxInput("sort_bars", "Sort bars by frequency", TRUE)
    }
  })
  
  output$aesthetic_controls <- renderUI({
    req(filtered_data())
    data <- filtered_data()
    num_cols <- names(data)[sapply(data, is.numeric)]
    cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    tagList(
      selectInput("color_by", "Color by:", 
                  choices = c("(None)" = "", cat_cols)),
      selectInput("size_by", "Size by:", 
                  choices = c("(None)" = "", num_cols)),
      selectInput("facet_by", "Facet by:", 
                  choices = c("(None)" = "", cat_cols))
    )
  })
  
  current_plot <- reactive({
    req(input$x_var, filtered_data())
    data <- filtered_data()
    
    p <- switch(input$plot_type,
                "Scatter Plot" = {
                  req(input$y_var)
                  p <- ggplot(data)
                  
                  # Build aesthetics based on selected options
                  aes_list <- list(x = as.name(input$x_var), 
                                   y = as.name(input$y_var))
                  
                  if (input$color_by != "") {
                    aes_list$color <- as.name(input$color_by)
                  }
                  if (input$size_by != "") {
                    aes_list$size <- as.name(input$size_by)
                  }
                  
                  p + do.call(aes, aes_list) +
                    geom_point() +
                    theme_minimal()
                },
                "Box Plot" = {
                  p <- ggplot(data)
                  
                  # Build aesthetics for box plot
                  aes_list <- list(y = as.name(input$x_var))
                  if (input$color_by != "") {
                    aes_list$x <- as.name(input$color_by)
                    aes_list$fill <- as.name(input$color_by)
                  }
                  
                  p + do.call(aes, aes_list) +
                    geom_boxplot() +
                    theme_minimal()
                },
                "Histogram" = {
                  p <- ggplot(data)
                  
                  # Build aesthetics for histogram
                  aes_list <- list(x = as.name(input$x_var))
                  if (input$color_by != "") {
                    aes_list$fill <- as.name(input$color_by)
                  }
                  
                  p + do.call(aes, aes_list) +
                    geom_histogram(bins = input$bins,
                                   position = if(input$color_by != "") "dodge" else "stack") +
                    theme_minimal()
                },
                "Bar Plot" = {
                  if (is.numeric(data[[input$x_var]])) {
                    p <- ggplot(data)
                    
                    # Build aesthetics for numeric bar plot
                    aes_list <- list(x = as.name(input$x_var))
                    if (input$color_by != "") {
                      aes_list$fill <- as.name(input$color_by)
                    }
                    
                    p + do.call(aes, aes_list) +
                      geom_bar(position = if(input$color_by != "") "dodge" else "stack") +
                      theme_minimal()
                  } else {
                    # For categorical variables, count first
                    if (input$color_by != "") {
                      data_summary <- data %>%
                        count(across(all_of(c(input$x_var, input$color_by))))
                    } else {
                      data_summary <- data %>%
                        count(across(all_of(input$x_var)))
                    }
                    
                    # Order levels by frequency if not colored
                    if (input$color_by == "") {
                      data_summary[[input$x_var]] <- factor(
                        data_summary[[input$x_var]],
                        levels = data_summary[[input$x_var]][order(data_summary$n, decreasing = TRUE)]
                      )
                    }
                    
                    p <- ggplot(data_summary)
                    
                    # Build aesthetics for categorical bar plot
                    aes_list <- list(x = as.name(input$x_var), y = as.name("n"))
                    if (input$color_by != "") {
                      aes_list$fill <- as.name(input$color_by)
                    }
                    
                    p + do.call(aes, aes_list) +
                      geom_col(position = if(input$color_by != "") "dodge" else "stack") +
                      theme_minimal()
                  }
                }
    )
    
    # Add faceting if selected
    if (input$facet_by != "") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_by)))
    }
    
    # Add labels and theme
    p + labs(title = input$plot_title,
             x = input$x_axis_label,
             y = input$y_axis_label) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Display plot
  output$plot <- renderPlot({
    print(current_plot())
  })
  
  # Download handler for the plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(current_plot())
      ggsave(file, plot = current_plot(), width = 10, height = 7, dpi = 300)
    }
  )
  
  # Statistical calculations
  stats_calculations <- reactive({
    req(input$x_var, filtered_data())
    data <- filtered_data()
    
    # Function to generate statistics for a dataset
    generate_stats <- function(data, group_name = NULL) {
      # Initialize results
      results <- list()
      
      # Initialize output text
      output_text <- c(
        if(!is.null(group_name)) paste("\n=== Analysis for", group_name, "===") else "\n=== Overall Analysis ===",
        "\nQuick Summary:",
        paste(capture.output(summary(data[[input$x_var]])), collapse = "\n")
      )
      
      # Simple and Technical Interpretations
      output_text <- c(output_text,
                       "\nSimple Interpretation:",
                       if(is.numeric(data[[input$x_var]])) {
                         paste("Most values of", input$x_var, "are around",
                               round(mean(data[[input$x_var]], na.rm = TRUE), 2),
                               ". The lowest value is",
                               round(min(data[[input$x_var]], na.rm = TRUE), 2),
                               "and the highest is",
                               round(max(data[[input$x_var]], na.rm = TRUE), 2), "."
                         )
                       } else {
                         paste("This data contains", length(unique(data[[input$x_var]])),
                               "different categories.")
                       },
                       "\nTechnical Details:",
                       if(is.numeric(data[[input$x_var]])) {
                         paste("Mean:", round(mean(data[[input$x_var]], na.rm = TRUE), 2),
                               "| SD:", round(sd(data[[input$x_var]], na.rm = TRUE), 2),
                               "| Range:", round(min(data[[input$x_var]], na.rm = TRUE), 2),
                               "to", round(max(data[[input$x_var]], na.rm = TRUE), 2))
                       } else {
                         paste("Categorical variable with",
                               length(unique(data[[input$x_var]])), "unique values")
                       }
      )
      
      # Shapiro-Wilk test for numeric data
      if (is.numeric(data[[input$x_var]]) && length(data[[input$x_var]]) >= 3) {
        results$shapiro <- tryCatch(
          shapiro.test(data[[input$x_var]]),
          error = function(e) NULL
        )
        
        if (!is.null(results$shapiro)) {
          simple_shapiro <- if(results$shapiro$p.value < 0.05) {
            "The data is not normally distributed (bell-shaped)."
          } else {
            "The data follows a normal (bell-shaped) distribution."
          }
          
          technical_shapiro <- if(results$shapiro$p.value < 0.05) {
            "Significantly deviates from normality (p < 0.05). Consider non-parametric methods."
          } else {
            "Normal distribution (p >= 0.05). Parametric tests appropriate."
          }
          
          output_text <- c(output_text,
                           "\nDistribution Check:",
                           paste("Simple:", simple_shapiro),
                           paste("Technical:", technical_shapiro),
                           paste("Test Statistics: W =", round(results$shapiro$statistic, 4),
                                 ", p-value =", format.pval(results$shapiro$p.value)))
        }
      }
      
      # Scatter plot specific tests
      if (input$plot_type == "Scatter Plot" && !is.null(input$y_var)) {
        results$correlation <- tryCatch(
          cor.test(data[[input$x_var]], data[[input$y_var]]),
          error = function(e) NULL
        )
        
        if (!is.null(results$correlation)) {
          cor_strength <- abs(results$correlation$estimate)
          cor_interpretation <- case_when(
            cor_strength >= 0.7 ~ "strong",
            cor_strength >= 0.3 ~ "moderate",
            TRUE ~ "weak"
          )
          
          direction <- if(results$correlation$estimate > 0)
            "as one increases, the other tends to increase"
          else
            "as one increases, the other tends to decrease"
          
          simple_cor <- paste("There is a", cor_interpretation, "relationship:", direction, ".",
                              if(results$correlation$p.value < 0.05)
                                "This pattern is reliable."
                              else
                                "This pattern might be by chance.")
          
          technical_cor <- paste(
            "Correlation coefficient (r) =", round(results$correlation$estimate, 4),
            sprintf("(p %s 0.05)", ifelse(results$correlation$p.value < 0.05, "<", "≥"))
          )
          
          output_text <- c(output_text,
                           "\nRelationship Analysis:",
                           paste("Simple:", simple_cor),
                           paste("Technical:", technical_cor))
          
          # Regression analysis
          results$regression <- tryCatch(
            summary(lm(data[[input$y_var]] ~ data[[input$x_var]])),
            error = function(e) NULL
          )
          
          if (!is.null(results$regression)) {
            simple_reg <- paste("This relationship can explain",
                                round(results$regression$r.squared * 100, 0),
                                "% of the changes in", input$y_var, ".")
            
            technical_reg <- paste(
              "R² =", round(results$regression$r.squared, 4),
              "| Adj.R² =", round(results$regression$adj.r.squared, 4),
              "| Slope =", round(results$regression$coefficients[2,1], 4),
              sprintf("(p %s 0.05)", ifelse(results$regression$coefficients[2,4] < 0.05, "<", "≥"))
            )
            
            output_text <- c(output_text,
                             "\nPredictive Analysis:",
                             paste("Simple:", simple_reg),
                             paste("Technical:", technical_reg))
          }
        }
      }
      
      # Distribution shape analysis for histograms
      if (input$plot_type == "Histogram" && is.numeric(data[[input$x_var]])) {
        skewness <- (sum((data[[input$x_var]] - mean(data[[input$x_var]]))^3) /
                       (length(data[[input$x_var]]) * sd(data[[input$x_var]])^3))
        
        simple_skew <- case_when(
          abs(skewness) < 0.5 ~ "values are spread evenly around the middle",
          skewness >= 0.5 ~ "there are more low values with some high outliers",
          skewness <= -0.5 ~ "there are more high values with some low outliers"
        )
        
        technical_skew <- case_when(
          abs(skewness) < 0.5 ~ "approximately symmetric",
          skewness >= 0.5 ~ "right-skewed (positively skewed)",
          skewness <= -0.5 ~ "left-skewed (negatively skewed)"
        )
        
        output_text <- c(output_text,
                         "\nShape Analysis:",
                         paste("Simple: In this dataset,", simple_skew, "."),
                         paste("Technical: Distribution is", technical_skew,
                               "(skewness =", round(skewness, 4), ")"))
      }
      
      paste(output_text, collapse = "\n")
    }
    
    # Generate overall statistics
    all_stats <- generate_stats(data)
    
    # Generate statistics for each facet group if faceting is enabled
    if (!is.null(input$facet_by) && input$facet_by != "") {
      facet_groups <- split(data, data[[input$facet_by]])
      facet_stats <- lapply(names(facet_groups), function(group) {
        generate_stats(facet_groups[[group]], paste(input$facet_by, "=", group))
      })
      
      # Combine overall and faceted statistics
      paste(c(all_stats, unlist(facet_stats)), collapse = "\n\n")
    } else {
      all_stats
    }
  })
  
  # Render statistical analysis
  output$stats_tests <- renderText({
    stats_calculations()
  })
  
  # Store saved plots and their descriptions
  saved_plots <- reactiveVal(list())
  
  # Clear report
  observeEvent(input$clear_report, {
    saved_plots(list())
    showNotification("Report cleared", type = "message")
  })
  
  # When save button is clicked, save current plot and stats
  observeEvent(input$save_current_plot, {
    req(current_plot())
    
    # Create a new plot entry
    new_plot <- list(
      plot = current_plot(),
      title = input$plot_title,
      type = input$plot_type,
      variables = list(
        x = input$x_var,
        y = if(!is.null(input$y_var)) input$y_var else NULL
      ),
      stats = stats_calculations()
    )
    
    # Add to saved plots
    current_saved <- saved_plots()
    saved_plots(c(current_saved, list(new_plot)))
    
    showNotification("Plot added to report", type = "message")
  })
  
  # Generate report preview
  output$saved_plots_preview <- renderUI({
    plots <- saved_plots()
    
    if (length(plots) == 0) {
      return(p("No plots saved yet. Create plots in the Graphs tab and click 'Add Current Plot to Report' to include them here."))
    }
    
    plot_previews <- lapply(seq_along(plots), function(i) {
      card(
        card_header(
          paste0("Plot ", i, ": ", plots[[i]]$title)
        ),
        plotOutput(paste0("saved_plot_", i)),
        if(input$include_stats) {
          verbatimTextOutput(paste0("saved_stats_", i))
        }
      )
    })
    
    # Render each saved plot
    for(i in seq_along(plots)) {
      local({
        local_i <- i
        output[[paste0("saved_plot_", local_i)]] <- renderPlot({
          plots[[local_i]]$plot
        })
        if(input$include_stats) {
          output[[paste0("saved_stats_", local_i)]] <- renderText({
            plots[[local_i]]$stats
          })
        }
      })
    }
    
    do.call(tagList, plot_previews)
  })
  
  # Preview report title
  output$preview_title <- renderText({
    input$report_title
  })
  
  # Preview report description
  output$preview_description <- renderText({
    input$report_description
  })
  
  # Add this output in the server function to render the data summary preview
  output$data_summary_preview <- renderUI({
    req(filtered_data())
    if (!input$include_data_summary) return(NULL)
    
    data <- filtered_data()
    
    # Function to create summary for each column
    create_col_summary <- function(col_name, col_data) {
      if (is.numeric(col_data)) {
        tagList(
          h4(col_name),
          p(strong("Numeric Variable Summary:")),
          tags$ul(
            tags$li(paste("Type:", class(col_data)[1])),
            tags$li(paste("Minimum:", round(min(col_data, na.rm = TRUE), 2))),
            tags$li(paste("Maximum:", round(max(col_data, na.rm = TRUE), 2))),
            tags$li(paste("Mean:", round(mean(col_data, na.rm = TRUE), 2))),
            tags$li(paste("Median:", round(median(col_data, na.rm = TRUE), 2))),
            tags$li(paste("Standard Deviation:", round(sd(col_data, na.rm = TRUE), 2))),
            tags$li(paste("Missing Values:", sum(is.na(col_data))))
          )
        )
      } else {
        freq_table <- table(col_data)
        top_categories <- head(sort(freq_table, decreasing = TRUE), 5)
        
        tagList(
          h4(col_name),
          p(strong("Categorical Variable Summary:")),
          tags$ul(
            tags$li(paste("Type:", class(col_data)[1])),
            tags$li(paste("Number of unique values:", length(unique(col_data)))),
            tags$li(paste("Missing Values:", sum(is.na(col_data)))),
            tags$li(
              "Top 5 most frequent categories:",
              tags$ul(
                lapply(1:length(top_categories), function(i) {
                  tags$li(paste0(
                    names(top_categories)[i], ": ",
                    top_categories[i], " (",
                    round(100 * top_categories[i]/length(col_data), 1), "%)"
                  ))
                })
              )
            )
          )
        )
      }
    }
    
    # Create summary for dataset overview
    overview <- tagList(
      h2("Data Summary"),
      h3("Dataset Overview"),
      tags$ul(
        tags$li(paste("Number of Observations:", nrow(data))),
        tags$li(paste("Number of Variables:", ncol(data))),
        tags$li("Types of Variables:",
                tags$ul(
                  tags$li(paste("Numeric:", sum(sapply(data, is.numeric)))),
                  tags$li(paste("Character:", sum(sapply(data, is.character)))),
                  tags$li(paste("Factor:", sum(sapply(data, is.factor))))
                )
        )
      ),
      h3("Variable Summaries")
    )
    
    # Generate summary for each column
    col_summaries <- lapply(names(data), function(col) {
      create_col_summary(col, data[[col]])
    })
    
    # Combine all elements
    tagList(
      hr(),
      overview,
      col_summaries
    )
  })
  
  # Download report handler
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("analysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      temp_dir <- tempfile()
      dir.create(temp_dir)
      on.exit(unlink(temp_dir, recursive = TRUE))
      
      # Create images directory
      img_dir <- file.path(temp_dir, "images")
      dir.create(img_dir)
      
      plots <- saved_plots()
      
      # Save plots as images
      plot_files <- character(length(plots))
      for(i in seq_along(plots)) {
        plot_files[i] <- file.path("images", paste0("plot_", i, ".png"))
        ggsave(
          file.path(temp_dir, plot_files[i]),
          plots[[i]]$plot,
          width = 10,
          height = 7,
          dpi = 300
        )
      }
      
      # Create CSS for better formatting
      css_content <- "
    .stats-section {
      margin: 20px 0;
      padding: 15px;
      background-color: #f8f9fa;
      border-radius: 5px;
    }
    .stats-header {
      color: #2c3e50;
      margin-bottom: 10px;
    }
    .stats-content {
      font-family: monospace;
      white-space: pre-wrap;
      margin-left: 20px;
    }
    .plot-section {
      margin: 30px 0;
      padding: 20px;
      border: 1px solid #dee2e6;
      border-radius: 8px;
    }
    .plot-title {
      color: #2c3e50;
      border-bottom: 2px solid #e9ecef;
      padding-bottom: 10px;
      margin-bottom: 20px;
    }
    "
      
      writeLines(css_content, file.path(temp_dir, "custom.css"))
      
      # Create Rmd content
      rmd_content <- c(
        "---",
        paste0('title: "', input$report_title, '"'),
        'date: "`r format(Sys.time(), \'%B %d, %Y\')`"',
        'output:',
        '  html_document:',
        '    toc: true',
        '    toc_float: true',
        '    theme: cosmo',
        '    highlight: tango',
        '    css: custom.css',
        '    code_folding: hide',
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        "",
        "## Description",
        "",
        input$report_description,
        ""
      )
      
      # Add data summary if requested
      # Add data summary if requested
      if (input$include_data_summary) {
        data <- filtered_data()
        summary_content <- c(
          "## Data Summary",
          "",
          "### Dataset Overview",
          sprintf("* Number of Observations: %d", nrow(data)),
          sprintf("* Number of Variables: %d", ncol(data)),
          "",
          "### Variable Types",
          sprintf("* Numeric Variables: %d", sum(sapply(data, is.numeric))),
          sprintf("* Character Variables: %d", sum(sapply(data, is.character))),
          sprintf("* Factor Variables: %d", sum(sapply(data, is.factor))),
          "",
          "### First Few Rows",
          "<div style='overflow-x: auto;'>",
          "```{r}",
          "knitr::kable(head(filtered_data(), 5), 
                  format = 'html',
                  align = rep('c', ncol(filtered_data())),
                  table.attr = 'class=\"table table-striped table-bordered\" style=\"width: auto !important; margin-left: auto; margin-right: auto; font-size: 14px;\"')",
          "```",
          "</div>",
          "",
          "### Summary Statistics",
          "```{r}",
          "summary(filtered_data())",
          "```"
        )
        rmd_content <- c(rmd_content, summary_content)
      }
      
      # Add plots and statistics
      if (length(plots) > 0) {
        for (i in seq_along(plots)) {
          plot_content <- c(
            paste("## Plot", i, ":", plots[[i]]$title),
            "",
            '<div class="plot-section">',
            paste0('<div class="plot-title">', plots[[i]]$title, '</div>'),
            paste0("![](", plot_files[i], ")"),
            "</div>"
          )
          
          if (input$include_stats) {
            stats_text <- plots[[i]]$stats
            stats_content <- c(
              "### Statistical Analysis",
              '<div class="stats-section">',
              paste0('<div class="stats-content">', stats_text, '</div>'),
              "</div>"
            )
            plot_content <- c(plot_content, stats_content)
          }
          
          rmd_content <- c(rmd_content, plot_content, "")
        }
      }
      
      # Write Rmd file
      writeLines(rmd_content, file.path(temp_dir, "report.Rmd"))
      
      # Render the report
      withr::with_dir(temp_dir, {
        rmarkdown::render("report.Rmd",
                          output_format = rmarkdown::html_document(
                            toc = TRUE,
                            toc_float = TRUE,
                            theme = "cosmo",
                            highlight = "tango",
                            css = "custom.css",
                            code_folding = "hide"
                          ),
                          output_file = file,
                          quiet = TRUE,
                          encoding = "UTF-8")
      })
    },
    contentType = "text/html"
  )
}

shinyApp(ui, server)