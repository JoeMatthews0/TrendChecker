library(shiny) # For building the app
library(dplyr) # Data processing
library(tidyr) # Data processing
library(ggplot2) # Producing plots
library(broom) # Regression results

# Import data
raw_data <- read.csv("Raw STATS-19.csv", stringsAsFactors = FALSE)

# Range of possible selection choices available in the data
all_years  <- sort(unique(raw_data$Year)) 
all_las    <- sort(unique(raw_data$LA))
all_sevs   <- sort(as.character(unique(raw_data$Severity)))
sev_labels <- c("1" = "Fatal (1)", "2" = "Serious (2)", "3" = "Slight (3)")

group_choices <- c(
  "Local Authority" = "LA",
  "Road Class"      = "Class",
  "Road Type"       = "Type",
  "Speed Limit"     = "SpeedLimit",
  "Junction Detail" = "JunctionDetail",
  "Urban/Rural"     = "Urban"
)

# ── Helpers ───────────────────────────────────────────────────────────────────

# Model for fitting Poisson GLM
fit_trend_glm <- function(sub_df) {
  if (length(unique(sub_df$Year)) < 2) return(NULL) # Guard in case too short a dataset is tried
  tryCatch(glm(Count ~ Year, data = sub_df, family = poisson()), error = function(e) NULL) # Fit model
}

# Summary sentence describing model outputs in human readable terms
trend_sentence <- function(label, fit, metric = "collisions") {
  # If we couldn't fit the model, tell the user
  if (is.null(fit)) return(sprintf("%s: insufficient data to assess trend.", label))
  # Extract year trend coefficient
  yr  <- tidy(fit)[tidy(fit)$term == "Year", ]
  # Convert trend coefficient to percentage change
  pct <- round(abs(100 * (exp(yr$estimate) - 1)), 1)
  # Give p-value interpretation
  if (yr$p.value < 0.001) {
    sig <- "a statistically significant"; pvtxt <- "(p < 0.001)"
  } else if (yr$p.value < 0.05) {
    sig <- "a statistically significant"; pvtxt <- sprintf("(p = %.3f)", yr$p.value)
  } else {
    sig <- "no statistically significant"; pvtxt <- sprintf("(p = %.3f)", yr$p.value)
  }
  if (yr$p.value < 0.05) {
    # Text for significant trend
    # Determine whether trend is increasing or decreasing based on regression coefficient
    dir <- if (yr$estimate > 0) "increase" else "decrease"
    sprintf("%s showed %s %s in %s of approximately %s%% per year %s.",
            label, sig, dir, metric, pct, pvtxt)
  } else {
    # Text for no significant trend
    sprintf("%s showed %s trend in %s over this period %s.", label, sig, metric, pvtxt)
  }
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("STATS-19 Collision Trend Analysis"),

  wellPanel(
    h4("Filters"),
    # Variable to group trends by, e.g. to compare across different LAs, road types etc
    fluidRow(
      column(3,
        selectInput("group_var", "Group by",
          choices = group_choices, selected = "LA")
      ),
      column(5,
        conditionalPanel(
          # If we're comparing LAs, which LAs do you want to compare?
          condition = "input.group_var == 'LA'",
          selectizeInput("las_main", "Local Authorities",
            choices  = all_las,
            selected = all_las[1:3],
            multiple = TRUE,
            options  = list(placeholder = "Select one or more LAs…"))
        ),
        conditionalPanel(
          # If we're not grouping by LAs, do you want to just look at data for some LAs? If so which ones?
          condition = "input.group_var != 'LA'",
          selectizeInput("las_filter", "Filter by Local Authority (optional)",
            choices  = all_las,
            selected = NULL,
            multiple = TRUE,
            options  = list(placeholder = "All LAs — select if you just want to look at a subset"))
        )
      ),
      column(4,
             # Which years of data to look at
        sliderInput("years", "Year range",
          min = min(all_years), max = max(all_years),
          value = c(min(all_years), max(all_years)),
          step = 1, sep = "")
      )
    ),
    fluidRow(
      # Do we want to analyse collision counts or casualty counts?
      column(3,
             radioButtons("metric", "Analyse",
                          choices  = c("Collisions" = "collisions", "Casualties" = "casualties"),
                          selected = "collisions",
                          inline   = TRUE)
      ),
      column(9,
        # Which severities do we want to consider?
          checkboxGroupInput("severities", "Severity levels",
            choiceNames  = unname(sev_labels[all_sevs]),
            choiceValues = all_sevs,
            selected     = all_sevs,
            inline       = TRUE)
      )
    )
  ),

  h4(textOutput("plot_heading")), # Are we showing casualties or collisions over time?
  checkboxInput("show_trend_line", "Show smoothed trend line", value = FALSE), # Do we want to include the Poisson GLM mean?
  plotOutput("trend_plot", height = "400px"), # Plot counts over time?
  h4("Trend analysis"),
  uiOutput("glm_summary"), # Summary of GLM model
  hr(), # Split before next section

  h4(textOutput("comp_heading")), # Comparison section, heading depends on what variables we're grouping by
  fluidRow(
    column(4, selectInput("comp_g1", "Group 1", choices = NULL)), # First group to compare
    column(4, selectInput("comp_g2", "Group 2", choices = NULL)), # Second group to compare
    column(4, br(), actionButton("run_comparison", "Run comparison", class = "btn-primary")) # Run the comparison
  ),
  uiOutput("comparison_summary") # Give output from comparison
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  grp_col <- reactive(input$group_var) # Identify grouping column

  # Data after LA filter and severity filter, with a generic group column added
  base_filtered <- reactive({
    req(input$severities)
    grp <- grp_col()
    df  <- raw_data

    if (grp == "LA") { # Extract relevant LAs
      req(input$las_main)
      df <- filter(df, LA %in% input$las_main) 
    } else if (length(input$las_filter) > 0) {
      df <- filter(df, LA %in% input$las_filter)
    }

    df |> # Extract relevant severities and group based on grouping variable
      filter(Severity %in% input$severities) |>
      mutate(Group = as.character(.data[[grp]]))
  })

  # Update the comparison menu options based on selected grouping variable
  observe({
    vals <- sort(unique(base_filtered()$Group))
    updateSelectInput(session, "comp_g1", choices = vals, selected = vals[1])
    updateSelectInput(session, "comp_g2", choices = vals,
                      selected = vals[min(2, length(vals))])
  })

  # Aggregate data, summing casualties or row numbers (i.e. collisions) over year range and grouping
  agg_data <- reactive({
    yr_range <- seq(input$years[1], input$years[2])
    base_filtered() |>
      filter(Year %in% yr_range) |>
      group_by(Group, Year) |>
      summarise(
        Count = if (input$metric == "casualties") sum(Casualties, na.rm = TRUE) else n(),
        .groups = "drop"
      ) |>
      complete(Group, Year = yr_range, fill = list(Count = 0L)) # Any missing year/group combinations means 0 collisions
  })

  # GLM predictions for trend ribbon
  glm_pred_data <- reactive({
    df <- agg_data()
    if (nrow(df) == 0) return(NULL)
    pred_list <- lapply(unique(df$Group), function(g) { # Across each value of grouping variable
      sub_df <- filter(df, Group == g) # Extract relevant data
      if (nrow(sub_df) < 2 || length(unique(sub_df$Year)) < 2) return(NULL)
      fit <- tryCatch(glm(Count ~ Year, data = sub_df, family = poisson()), # Fit Poisson model
                      error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      year_seq   <- seq(min(sub_df$Year), max(sub_df$Year), length.out = 100) # Range of values for plotting mean
      pred_link  <- predict(fit, newdata = data.frame(Year = year_seq), se.fit = TRUE) # Fit model to range of values
      data.frame(
        Group = g, Year = year_seq,
        fit   = exp(pred_link$fit),
        lower = exp(pred_link$fit - 1.96 * pred_link$se.fit),
        upper = exp(pred_link$fit + 1.96 * pred_link$se.fit) # Give predicted mean + 95% error band
      )
    })
    do.call(rbind, Filter(Negate(is.null), pred_list)) # Combine results at the end
  })
  
  # Store chosen grouping variable and count metric
  grp_label    <- reactive(names(group_choices)[group_choices == grp_col()])
  metric_label <- reactive(if (input$metric == "casualties") "Casualties" else "Collisions")

  # Produce plot heading based on count
  output$plot_heading <- renderText({
    paste(metric_label(), "over time")
  })

  # Trend plot
  output$trend_plot <- renderPlot({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    
    # Line and point plot of counts over time, different colour per group
    p <- ggplot(df, aes(x = Year, y = Count, colour = Group)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.5) +
      scale_x_continuous(breaks = all_years) +
      labs(
        x      = "Year",
        y      = paste("Total", tolower(metric_label())),
        colour = grp_label(),
        fill   = grp_label(),
        title  = paste("Annual", tolower(metric_label()), "by", tolower(grp_label()))
      ) +
      theme_bw(base_size = 13) +
      theme(legend.position = "right")
    
    # Add GLM line if requested
    if (isTRUE(input$show_trend_line)) {
      pred_df <- glm_pred_data()
      if (!is.null(pred_df) && nrow(pred_df) > 0) {
        p <- p +
          geom_ribbon(
            data = pred_df,
            aes(x = Year, ymin = lower, ymax = upper, group = Group, fill = Group),
            alpha = 0.15, colour = NA, inherit.aes = FALSE) +
          geom_line(
            data = pred_df,
            aes(x = Year, y = fit, group = Group, colour = Group),
            linewidth = 0.7, linetype = "dashed", inherit.aes = FALSE
          )
      }
    }
    p
  })

  # Model summary in bullet point form
  output$glm_summary <- renderUI({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    m <- tolower(metric_label())
    bullets <- lapply(sort(unique(df$Group)), function(g) {
      fit <- fit_trend_glm(filter(df, Group == g)) # Fit the model to each group
      tags$li(trend_sentence(g, fit, m)) # Write the plain English sentence describing results
    })
    wellPanel(do.call(tags$ul, bullets)) # Put into bullet point form
  })

  # Comparison section heading
  output$comp_heading <- renderText({
    paste("Compare trends between two", tolower(grp_label()), "groups")
  })

  # Comparison GLM
  comp_result <- eventReactive(input$run_comparison, { # When someone presses the button we run the code
    req(input$comp_g1, input$comp_g2)
    # Check we don't have the same group twice
    validate(need(input$comp_g1 != input$comp_g2, "Please select two different groups."))

    g1       <- input$comp_g1
    g2       <- input$comp_g2
    yr_range <- seq(input$years[1], input$years[2])
    
    # Extract relevant data based on requested groupings and years
    df_comp <- base_filtered() |>
      filter(Group %in% c(g1, g2), Year %in% yr_range) |>
      group_by(Group, Year) |>
      summarise(
        Count = if (input$metric == "casualties") sum(Casualties, na.rm = TRUE) else n(),
        .groups = "drop") |>
      complete(Group, Year = yr_range, fill = list(Count = 0L)) |>
      mutate(Group = factor(Group))

    validate(need(nrow(df_comp) >= 4, "Not enough data to fit a comparison model."))
    
    m       <- tolower(isolate(metric_label()))
    # Fit model to group one to learn its overall trend
    fit1    <- fit_trend_glm(filter(df_comp, Group == g1))
    # Fit model to group two to learn its overall trend
    fit2    <- fit_trend_glm(filter(df_comp, Group == g2))
    # Fit model to both groups with interaction term to see if trends are significantly different
    fit_int <- tryCatch(
      glm(Count ~ Year * Group, data = df_comp, family = poisson()),
      error = function(e) NULL
    )
    # Possible that interaction models might fall over so need a check for that
    validate(need(!is.null(fit_int), "Interaction model failed to converge."))
    # Extract interaction p-value
    int_row <- tidy(fit_int)[grepl(":", tidy(fit_int)$term), ]
    # Return results for processing later
    list(
      sentence1  = trend_sentence(g1, fit1, m),
      sentence2  = trend_sentence(g2, fit2, m),
      int_pvalue = int_row$p.value[1]
    )
  })
  
  # Print a sentence with the outcome of the interaction model based on the interaction p-value
  output$comparison_summary <- renderUI({
    res  <- comp_result()
    pval <- res$int_pvalue
    diff_msg <- if (pval < 0.001) {
      "The difference in trends between the two groups is statistically significant (p < 0.001)."
    } else if (pval < 0.05) {
      sprintf("The difference in trends between the two groups is statistically significant (p = %.3f).", pval)
    } else {
      sprintf("There is no statistically significant difference in trends between the two groups (p = %.3f).", pval)
    }
    wellPanel(
      tags$ul(tags$li(res$sentence1), tags$li(res$sentence2)),
      p(strong(diff_msg))
    )
  })
}

shinyApp(ui, server)
