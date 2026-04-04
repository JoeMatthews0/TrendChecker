library(shiny)
library(dplyr)
library(ggplot2)
library(broom)

# Load data
raw_data <- read.csv("UK Road Safety Data.csv", stringsAsFactors = FALSE)
raw_data$Severity <- as.character(raw_data$Severity)

all_areas  <- sort(unique(raw_data$Area))
all_years  <- sort(unique(raw_data$Year))
all_sevs   <- sort(unique(raw_data$Severity))

sev_labels <- c("1" = "Fatal (1)", "2" = "Serious (2)", "3" = "Slight (3)")

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("UK Road Safety – Collision Trend Analyser"),

  # ── Filters ----------------------------------------------------------------
  wellPanel(
    h4("Filters"),
    fluidRow(
      column(5,
        selectizeInput(
          "areas", "Areas",
          choices  = all_areas,
          selected = all_areas[1:3],
          multiple = TRUE,
          options  = list(placeholder = "Select one or more areas…")
        )
      ),
      column(4,
        sliderInput(
          "years", "Year range",
          min   = min(all_years),
          max   = max(all_years),
          value = c(min(all_years), max(all_years)),
          step  = 1,
          sep   = ""
        )
      ),
      column(3,
        checkboxGroupInput(
          "severities", "Severity levels",
          choiceNames  = unname(sev_labels[all_sevs]),
          choiceValues = all_sevs,
          selected     = all_sevs
        )
      )
    )
  ),

  # ── Trend chart + per-area GLM results ------------------------------------
  h4("Collision counts over time"),
  checkboxInput("show_trend_line", "Show Poisson GLM trend line with 95% CI", value = FALSE),
  plotOutput("trend_plot", height = "400px"),

  h4("Trend analysis (Poisson GLM per area)"),
  p("Each area is fitted with a Poisson GLM: Count ~ Year.
     The table shows the Year coefficient, its p-value, and a plain-English interpretation."),
  tableOutput("glm_table"),

  hr(),

  # ── Area comparison -------------------------------------------------------
  h4("Compare trends between two areas"),
  p("A Poisson GLM is fitted to both areas together: Count ~ Year * Area.
     The Area:Year interaction tests whether the two trends differ significantly."),
  fluidRow(
    column(4,
      selectInput("comp_area1", "Area 1", choices = all_areas, selected = all_areas[1])
    ),
    column(4,
      selectInput("comp_area2", "Area 2", choices = all_areas, selected = all_areas[2])
    ),
    column(4,
      br(),
      actionButton("run_comparison", "Run comparison", class = "btn-primary")
    )
  ),
  tableOutput("comparison_table"),
  uiOutput("comparison_interp")
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive: filtered & aggregated data ----------------------------------
  agg_data <- reactive({
    req(input$areas, input$severities)
    raw_data |>
      filter(
        Area     %in% input$areas,
        Year     >= input$years[1],
        Year     <= input$years[2],
        Severity %in% input$severities
      ) |>
      group_by(Area, Year) |>
      summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  })

  # ── Reactive: GLM fitted values + CI for each area -----------------------
  glm_pred_data <- reactive({
    df <- agg_data()
    if (nrow(df) == 0) return(NULL)

    pred_list <- lapply(unique(df$Area), function(a) {
      sub_df <- filter(df, Area == a)
      if (nrow(sub_df) < 2 || length(unique(sub_df$Year)) < 2) return(NULL)

      fit <- tryCatch(
        glm(Count ~ Year, data = sub_df, family = poisson()),
        error = function(e) NULL
      )
      if (is.null(fit)) return(NULL)

      # Predict over a fine grid so the ribbon looks smooth
      year_seq  <- seq(min(sub_df$Year), max(sub_df$Year), length.out = 100)
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
            data    = pred_df,
            aes(x = Year, ymin = lower, ymax = upper, group = Area, fill = Area),
            alpha   = 0.15,
            colour  = NA,
            inherit.aes = FALSE
          ) +
          geom_line(
            data    = pred_df,
            aes(x = Year, y = fit, group = Area, colour = Area),
            linewidth = 0.7,
            linetype  = "dashed",
            inherit.aes = FALSE
          )
      }
    }

    p
  })

  # ── Per-area Poisson GLM table --------------------------------------------
  output$glm_table <- renderTable({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    results <- lapply(unique(df$Area), function(a) {
      sub_df <- filter(df, Area == a)

      # Need at least 2 data points and non-zero variance in Year
      if (nrow(sub_df) < 2 || length(unique(sub_df$Year)) < 2) {
        return(data.frame(
          Area          = a,
          Coefficient   = NA_real_,
          `Std. Error`  = NA_real_,
          `p-value`     = NA_real_,
          Interpretation = "Insufficient data",
          check.names = FALSE
        ))
      }

      fit <- tryCatch(
        glm(Count ~ Year, data = sub_df, family = poisson()),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        return(data.frame(
          Area           = a,
          Coefficient    = NA_real_,
          `Std. Error`   = NA_real_,
          `p-value`      = NA_real_,
          Interpretation = "Model failed to converge",
          check.names = FALSE
        ))
      }

      coef_tbl <- tidy(fit)
      year_row  <- coef_tbl[coef_tbl$term == "Year", ]
      beta      <- year_row$estimate
      se        <- year_row$std.error
      pval      <- year_row$p.value

      interp <- if (is.na(pval)) {
        "Could not determine"
      } else if (pval < 0.05) {
        dir <- if (beta > 0) "significant increasing trend" else "significant decreasing trend"
        sprintf("%s (each year × %.1f%%)", dir, 100 * (exp(beta) - 1))
      } else {
        "No significant trend"
      }

      data.frame(
        Area           = a,
        Coefficient    = round(beta, 4),
        `Std. Error`   = round(se, 4),
        `p-value`      = signif(pval, 3),
        Interpretation = interp,
        check.names = FALSE
      )
    })

    bind_rows(results)
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

  # ── Area comparison --------------------------------------------------------
  comp_result <- eventReactive(input$run_comparison, {
    req(input$comp_area1, input$comp_area2, input$severities)
    validate(
      need(
        input$comp_area1 != input$comp_area2,
        "Please select two different areas."
      )
    )

    df_comp <- raw_data |>
      filter(
        Area     %in% c(input$comp_area1, input$comp_area2),
        Year     >= input$years[1],
        Year     <= input$years[2],
        Severity %in% input$severities
      ) |>
      group_by(Area, Year) |>
      summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
      mutate(Area = factor(Area))

    validate(need(nrow(df_comp) >= 4, "Not enough data to fit a comparison model."))

    fit <- tryCatch(
      glm(Count ~ Year * Area, data = df_comp, family = poisson()),
      error = function(e) NULL
    )

    validate(need(!is.null(fit), "Model failed to converge."))

    tidy(fit)
  })

  output$comparison_table <- renderTable({
    tbl <- comp_result()
    tbl |>
      mutate(
        estimate   = round(estimate, 5),
        std.error  = round(std.error, 5),
        statistic  = round(statistic, 3),
        p.value    = signif(p.value, 3)
      ) |>
      rename(
        Term         = term,
        Coefficient  = estimate,
        `Std. Error` = std.error,
        `z value`    = statistic,
        `p-value`    = p.value
      )
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%")

  output$comparison_interp <- renderUI({
    tbl <- comp_result()

    # The interaction term is the row whose name contains ":"
    int_row <- tbl[grepl(":", tbl$term), ]

    if (nrow(int_row) == 0) {
      return(p("Interaction term not found in model output."))
    }

    pval  <- int_row$p.value[1]
    beta  <- int_row$estimate[1]
    a1    <- input$comp_area1
    a2    <- input$comp_area2

    msg <- if (pval < 0.001) {
      sprintf(
        "The Year × Area interaction is highly significant (p = %.2e). The two trends differ significantly: %s shows a %.1f%% %s annual change relative to %s.",
        pval,
        a2, abs(100 * (exp(beta) - 1)),
        if (beta > 0) "faster increase" else "faster decrease",
        a1
      )
    } else if (pval < 0.05) {
      sprintf(
        "The Year × Area interaction is significant (p = %.3f). There is evidence that the trends in %s and %s differ.",
        pval, a1, a2
      )
    } else {
      sprintf(
        "The Year × Area interaction is not significant (p = %.3f). There is no strong evidence that the trends in %s and %s differ.",
        pval, a1, a2
      )
    }

    wellPanel(
      h5("Interpretation"),
      p(msg)
    )
  })
}

shinyApp(ui, server)
