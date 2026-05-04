library(shiny)
library(dplyr)
library(ggplot2)
library(broom)

# Load data
raw_data <- read.csv("UK Road Safety Data.csv", stringsAsFactors = FALSE)
raw_data$Severity <- as.character(raw_data$Severity)

all_areas <- sort(unique(raw_data$Area))
all_years <- sort(unique(raw_data$Year))
all_sevs <- sort(unique(raw_data$Severity))

sev_labels <- c("1" = "Fatal (1)", "2" = "Serious (2)", "3" = "Slight (3)")

# ── Helper: plain-English trend sentence from a Poisson GLM ──────────────────
fit_trend_glm <- function(sub_df) {
  if (nrow(sub_df) < 2 || length(unique(sub_df$Year)) < 2) return(NULL)
  tryCatch(glm(Count ~ Year, data = sub_df, family = poisson()), error = function(e) NULL)
}

trend_sentence <- function(area, fit) {
  if (is.null(fit)) return(sprintf("%s: insufficient data to assess trend.", area))
  yr  <- tidy(fit)[tidy(fit)$term == "Year", ]
  pct <- round(abs(100 * (exp(yr$estimate) - 1)), 1)
  if (yr$p.value < 0.001) {
    sig <- "a statistically significant"
    pvtxt <- "(p < 0.001)"
  } else if (yr$p.value < 0.05) {
    sig <- "a statistically significant"
    pvtxt <- sprintf("(p = %.3f)", yr$p.value)
  } else {
    sig <- "no statistically significant"
    pvtxt <- sprintf("(p = %.3f)", yr$p.value)
  }
  if (yr$p.value < 0.05) {
    dir <- if (yr$estimate > 0) "increase" else "decrease"
    sprintf("%s showed %s %s in collisions of approximately %s%% per year %s.",
            area, sig, dir, pct, pvtxt)
  } else {
    sprintf("%s showed %s trend in collisions over this period %s.",
            area, sig, pvtxt)
  }
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("STATS-19 Collision Trend Analysis"),

  # ── Filters ----------------------------------------------------------------
  wellPanel(
    h4("Filters"),
    fluidRow(
      column(
        5,
        selectizeInput(
          "areas", "Areas",
          choices = all_areas,
          selected = all_areas[1:3],
          multiple = TRUE,
          options = list(placeholder = "Select one or more areas…")
        )
      ),
      column(
        4,
        sliderInput(
          "years", "Year range",
          min = min(all_years),
          max = max(all_years),
          value = c(min(all_years), max(all_years)),
          step = 1,
          sep = ""
        )
      ),
      column(
        3,
        checkboxGroupInput(
          "severities", "Severity levels",
          choiceNames = unname(sev_labels[all_sevs]),
          choiceValues = all_sevs,
          selected = all_sevs
        )
      )
    )
  ),

  # ── Trend chart + per-area GLM results ------------------------------------
  h4("Collision counts over time"),
  checkboxInput("show_trend_line", "Show smoothed trend line", value = FALSE),
  plotOutput("trend_plot", height = "400px"),
  h4("Trend analysis"),
  uiOutput("glm_summary"),
  hr(),

  # ── Area comparison -------------------------------------------------------
  h4("Compare trends between two areas"),
  fluidRow(
    column(
      4,
      selectInput("comp_area1", "Area 1", choices = all_areas, selected = all_areas[1])
    ),
    column(
      4,
      selectInput("comp_area2", "Area 2", choices = all_areas, selected = all_areas[2])
    ),
    column(
      4,
      br(),
      actionButton("run_comparison", "Run comparison", class = "btn-primary")
    )
  ),
  uiOutput("comparison_summary")
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # ── Reactive: filtered & aggregated data ----------------------------------
  agg_data <- reactive({
    req(input$areas, input$severities)
    raw_data |>
      filter(
        Area %in% input$areas,
        Year >= input$years[1],
        Year <= input$years[2],
        Severity %in% input$severities
      ) |>
      group_by(Area, Year) |>
      summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  })

  # ── Reactive: GLM fitted values + CI for each area -----------------------
  glm_pred_data <- reactive({
    df <- agg_data()
    if (nrow(df) == 0) {
      return(NULL)
    }

    pred_list <- lapply(unique(df$Area), function(a) {
      sub_df <- filter(df, Area == a)
      if (nrow(sub_df) < 2 || length(unique(sub_df$Year)) < 2) {
        return(NULL)
      }

      fit <- tryCatch(
        glm(Count ~ Year, data = sub_df, family = poisson()),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(NULL)
      }

      # Predict over a fine grid so the ribbon looks smooth
      year_seq <- seq(min(sub_df$Year), max(sub_df$Year), length.out = 100)
      pred_link <- predict(fit, newdata = data.frame(Year = year_seq), se.fit = TRUE)

      data.frame(
        Area   = a,
        Year   = year_seq,
        fit    = exp(pred_link$fit),
        lower  = exp(pred_link$fit - 1.96 * pred_link$se.fit),
        upper  = exp(pred_link$fit + 1.96 * pred_link$se.fit)
      )
    })

    do.call(rbind, Filter(Negate(is.null), pred_list))
  })

  # ── Trend plot -------------------------------------------------------------
  output$trend_plot <- renderPlot({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    p <- ggplot(df, aes(x = Year, y = Count, colour = Area, group = Area)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.5) +
      scale_x_continuous(breaks = all_years) +
      labs(
        x      = "Year",
        y      = "Total collision count",
        colour = "Area",
        fill   = "Area",
        title  = "Annual collision counts by area"
      ) +
      theme_bw(base_size = 13) +
      theme(legend.position = "right")

    if (isTRUE(input$show_trend_line)) {
      pred_df <- glm_pred_data()
      if (!is.null(pred_df) && nrow(pred_df) > 0) {
        p <- p +
          geom_ribbon(
            data = pred_df,
            aes(x = Year, ymin = lower, ymax = upper, group = Area, fill = Area),
            alpha = 0.15,
            colour = NA,
            inherit.aes = FALSE
          ) +
          geom_line(
            data = pred_df,
            aes(x = Year, y = fit, group = Area, colour = Area),
            linewidth = 0.7,
            linetype = "dashed",
            inherit.aes = FALSE
          )
      }
    }

    p
  })

  # ── Per-area trend summary ------------------------------------------------
  output$glm_summary <- renderUI({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    bullets <- lapply(sort(unique(df$Area)), function(a) {
      fit <- fit_trend_glm(filter(df, Area == a))
      tags$li(trend_sentence(a, fit))
    })

    wellPanel(do.call(tags$ul, bullets))
  })

  # ── Area comparison --------------------------------------------------------
  comp_result <- eventReactive(input$run_comparison, {
    req(input$comp_area1, input$comp_area2, input$severities)
    validate(need(input$comp_area1 != input$comp_area2, "Please select two different areas."))

    a1 <- input$comp_area1
    a2 <- input$comp_area2

    df_comp <- raw_data |>
      filter(
        Area     %in% c(a1, a2),
        Year     >= input$years[1],
        Year     <= input$years[2],
        Severity %in% input$severities
      ) |>
      group_by(Area, Year) |>
      summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
      mutate(Area = factor(Area))

    validate(need(nrow(df_comp) >= 4, "Not enough data to fit a comparison model."))

    # Separate GLMs for individual trend sentences
    fit1 <- fit_trend_glm(filter(df_comp, Area == a1))
    fit2 <- fit_trend_glm(filter(df_comp, Area == a2))

    # Interaction model to test whether trends differ
    fit_int <- tryCatch(
      glm(Count ~ Year * Area, data = df_comp, family = poisson()),
      error = function(e) NULL
    )

    validate(need(!is.null(fit_int), "Interaction model failed to converge."))

    int_row <- tidy(fit_int)[grepl(":", tidy(fit_int)$term), ]

    list(
      sentence1  = trend_sentence(a1, fit1),
      sentence2  = trend_sentence(a2, fit2),
      int_pvalue = int_row$p.value[1],
      a1 = a1, a2 = a2
    )
  })

  output$comparison_summary <- renderUI({
    res <- comp_result()

    pval <- res$int_pvalue
    diff_msg <- if (pval < 0.001) {
      sprintf(
        "The difference in trends between the two areas is statistically significant (p < 0.001)."
      )
    } else if (pval < 0.05) {
      sprintf(
        "The difference in trends between the two areas is statistically significant (p = %.3f).", pval
      )
    } else {
      sprintf(
        "There is no statistically significant difference in trends between the two areas (p = %.3f).", pval
      )
    }

    wellPanel(
      tags$ul(
        tags$li(res$sentence1),
        tags$li(res$sentence2)
      ),
      p(strong(diff_msg))
    )
  })
}

shinyApp(ui, server)
