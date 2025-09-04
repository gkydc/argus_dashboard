# app.R
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(openxlsx)

df <- read.csv("summary_stats_daynight.csv")

fixed_params <- c("temperature", "humidity","vapor pressure deficit","indoor PAR light","growing degree units corn/soy","indoor daily light integral","outdoor daily light integral")  # these will be pre-selected when available

plot_ranges_by_parameter <- function(data, target = NA_real_, show_target = TRUE) {
  bar_col <- "#25A7D9"; dot_col <- "#F2C14E"
  target_col <- "#FF4D4F"  # red
  bg_dark <- "#222426"; fg_light <- "#E8EEF2"
  data <- data %>% arrange(desc(parameter))
  p <- plot_ly() %>%
    add_segments(
      data = data, x = ~min, xend = ~max, y = ~parameter, yend = ~parameter,
      line = list(width = 10, color = bar_col),
      hovertemplate = paste("<b>%{y}</b><br>Min: %{x}<br>Max: %{xend}<extra></extra>")
    ) %>%
    add_markers(data = data, x = ~min, y = ~parameter,
                marker = list(size = 12, color = "white",
                              line = list(color = bar_col, width = 3)),
                hoverinfo = "skip") %>%
    add_markers(data = data, x = ~max, y = ~parameter,
                marker = list(size = 12, color = "white",
                              line = list(color = bar_col, width = 3)),
                hoverinfo = "skip") %>%
    add_markers(data = data, x = ~median_value, y = ~parameter,
                marker = list(size = 12, color = dot_col),
                hovertemplate = paste("<b>%{y}</b><br>Median: %{x}<extra></extra>")) %>%
    add_text(data = data, x = ~min, y = ~parameter, text = ~sprintf("%.1f", min),
             textposition = "middle right", textfont = list(color = fg_light), hoverinfo = "skip") %>%
    add_text(data = data, x = ~median_value, y = ~parameter,
             text = ~sprintf("%.1f", median_value),
             textposition = "top center", textfont = list(color = fg_light), hoverinfo = "skip") %>%
    add_text(data = data, x = ~max, y = ~parameter, text = ~sprintf("%.1f", max),
             textposition = "middle left", textfont = list(color = fg_light), hoverinfo = "skip") %>%
    layout(
      xaxis = list(title = "", zeroline = FALSE, gridcolor = "#444A4F",
                   tickfont = list(color = fg_light)),
      yaxis = list(title = "", tickfont = list(color = fg_light)),
      paper_bgcolor = bg_dark, plot_bgcolor = bg_dark,
      margin = list(l = 110, r = 40, t = 20, b = 40)
    )
  
  if (show_target && is.finite(target)) {
    p <- p %>% layout(
      shapes = list(list(
        type = "line",
        x0 = target, x1 = target, xref = "x",
        y0 = 0,      y1 = 1,      yref = "paper",
        line = list(color = target_col, width = 2, dash = "dash")
      ))
    ) %>%
      add_annotations(
        x = target, y = 1, xref = "x", yref = "paper",
        text = paste0("Target: ", sprintf("%.2f", target)),
        showarrow = FALSE, yanchor = "bottom",
        font = list(color = target_col, size = 11)
      )
  }
  p
}

ui <- dashboardPage(
  dashboardHeader(title = "Argus Dashboard"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE,
          column(3, uiOutput("date_ui")),
          column(3, uiOutput("dn_ui")),
          column(3, selectInput("facility", "Facility", choices = NULL, multiple = TRUE)),
          column(3, uiOutput("param_ui"))
      )
    ),
    fluidRow(
      box(width = 12, title = "Target Value", status = "primary", solidHeader = TRUE,
          column(3, numericInput("target", "Target value", value = NA, step = 0.1)),
          column(2, checkboxInput("show_target", "Show target line", value = TRUE))
      )
    ),
    fluidRow(
      box(width = 12, title = "Range Plot", status = "primary", solidHeader = TRUE,
          plotlyOutput("plot", height = "440px"))
    ),
    fluidRow(
      box(width = 12, title = "Filtered Data", status = "primary", solidHeader = TRUE,
          tags$div(style = "margin-bottom:8px",
                   downloadButton("download_csv", "Download CSV"),
                   downloadButton("download_xlsx", "Download XLSX")),
          DTOutput("table"))
    )
  )
)

server <- function(input, output, session) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  mapped <- reactive({
    df %>%
      transmute(
        date= as.Date(.data[["l_bound_date"]]),
        facility     = .data[["facility"]],
        parameter    = .data[["parameter_type"]],
        day_night  = .data[["day_or_night"]],
        min          = as.numeric(.data[["min_value"]]),
        max          = as.numeric(.data[["max_value"]]),
        p25          = suppressWarnings(as.numeric(.data[["percentile_25"]])),
        p75          = suppressWarnings(as.numeric(.data[["percentile_75"]])),
        median_value = suppressWarnings(as.numeric(.data[["median_value"]]))
      )
  })
  keep_or_all <- function(current, choices) {
    kept <- intersect(current %||% character(0), choices)
    if (length(kept)) kept else choices
  }
  keep_or_first <- function(current, choices) {
    if (length(choices) == 0) return(character(0))
    if (!is.null(current) && current %in% choices) current else choices[1]
  }
  output$date_ui <- renderUI({
    dts <- sort(unique(mapped()$date))
    selectInput("the_date", "Date", choices = dts, selected = max(dts))
  })
  output$dn_ui <- renderUI({
    req(input$the_date)
    d <- mapped() %>% filter(date == as.Date(input$the_date))
    if (!is.null(input$facility) && length(input$facility) > 0)
      d <- d %>% filter(facility %in% input$facility)
    dn_choices <- sort(unique(d$day_night))
    radioButtons("dn", "Day/Night",
                 choices = c("All", dn_choices),
                 selected = keep_or_first(input$dn, c("All", dn_choices)),
                 inline = TRUE)
  })
  observeEvent(list(input$the_date, input$dn), {
    req(input$the_date)
    d <- mapped() %>% filter(date == as.Date(input$the_date))
    if (!is.null(input$dn) && input$dn != "All") d <- d %>% filter(day_night == input$dn)
    fac_choices <- sort(unique(d$facility))
    updateSelectInput(session, "facility",
                      choices = fac_choices,
                      selected = keep_or_all(input$facility, fac_choices))
  }, ignoreInit = FALSE)
  output$param_ui <- renderUI({
    req(input$the_date)
    d <- mapped() %>% filter(date == as.Date(input$the_date))
    if (!is.null(input$dn) && input$dn != "All") d <- d %>% filter(day_night == input$dn)
    if (!is.null(input$facility) && length(input$facility) > 0) {
      d <- d %>% filter(facility %in% input$facility)
    }
    available <- sort(unique(d$parameter))
    current <- intersect(input$parameter %||% character(0), available)
    default_sel <- if (length(current) > 0) current else intersect(fixed_params, available)
    selectInput("parameter", "Parameters",
                choices = available,
                selected = if (length(default_sel) > 0) default_sel else available,
                multiple = TRUE)
  })
  filtered <- reactive({
    req(input$the_date)
    d <- mapped() %>% filter(date == as.Date(input$the_date))
    if (!is.null(input$dn) && input$dn != "All") d <- d %>% filter(day_night == input$dn)
    if (!is.null(input$facility) && length(input$facility) > 0) d <- d %>% filter(facility %in% input$facility)
    d <- d %>% filter(parameter %in% (input$parameter %||% character(0)))
    validate(need(nrow(d) > 0, "No matching rows"))
    d
  })
  aggregated <- reactive({
    filtered() %>%
      group_by(parameter) %>%
      summarise(
        min          = min(min, na.rm = TRUE),
        max          = max(max, na.rm = TRUE),
        median_value = median(median_value, na.rm = TRUE),
        .groups = "drop"
      )
  })
  output$plot <- renderPlotly({
    plot_ranges_by_parameter(
      aggregated(),
      target = suppressWarnings(as.numeric(input$target)),
      show_target = isTRUE(input$show_target)
    )
  })
  output$table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("filtered_data_", Sys.Date(), ".xlsx"),
    content  = function(file) openxlsx::write.xlsx(filtered(), file)
  )
}
shinyApp(ui, server)