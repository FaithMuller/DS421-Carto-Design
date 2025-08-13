library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(viridis)
library(mapgl)

# --- DATA LOAD ---
# adjust path if needed
language_data <- st_read("data/county_data.gpkg", quiet = TRUE) |>
  st_transform(4326) |>
  mutate(
    percent_speakers = suppressWarnings(as.numeric(percent_speakers)),
    speakers         = suppressWarnings(as.numeric(speakers))
  ) |>
  filter(!is.na(percent_speakers))   # simplest: drop NAs for a clean slider

# fixed bins / legend
breaks <- c(0, 0.1, 0.5, 1, 2, 5, 10, Inf)
labels <- c("0–0.1%", "0.1–0.5%", "0.5–1%", "1–2%", "2–5%", "5–10%", "10%+")
pal    <- viridis(length(breaks) - 1)
bin_percent <- function(x) cut(x, breaks = breaks, include.lowest = TRUE, labels = labels)

# language choices
lang_choices <- language_data |>
  filter(!is.na(language)) |>
  count(language, sort = TRUE) |>
  pull(language)

# --- UI ---
ui <- page_sidebar(
  title = "Languages by U.S. County (Percent of Speakers)",
  sidebar = sidebar(
    selectInput("lang", "Language", choices = lang_choices,
                selected = lang_choices[1], multiple = FALSE),
    uiOutput("pct_ui"),
    tags$hr(),
    tags$strong("Legend"),
    uiOutput("legend")
  ),
  card(full_screen = TRUE, maplibreOutput("map"))
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # per-language sf with color column
  lang_sf <- reactive({
    req(input$lang)
    language_data |>
      filter(language == input$lang) |>
      mutate(
        pct_bin   = bin_percent(percent_speakers),
        fillcol   = pal[as.integer(pct_bin)]
      )
  })
  
  # slider adapts to selected language
  output$pct_ui <- renderUI({
    d <- lang_sf()
    rng <- range(d$percent_speakers, na.rm = TRUE)
    sliderInput("pct", "Percent speakers",
                min = floor(rng[1]),
                max = ceiling(max(rng[2], 0.1)),
                value = rng, step = 0.1
    )
  })
  
  # legend
  output$legend <- renderUI({
    tags$div(
      lapply(seq_along(labels), function(i) {
        tags$div(style = "display:flex;align-items:center;margin:2px 0;",
                 tags$span(style = paste0(
                   "width:18px;height:12px;display:inline-block;margin-right:8px;background:", pal[i], ";"
                 )),
                 tags$span(labels[i]))
      })
    )
  })
  
  # render map: NOTE we use maplibre()/renderMaplibre and pass the sf directly in "source"
  output$map <- renderMaplibre({
    d <- lang_sf()  # makes the map re-render when language changes
    maplibre(style = carto_style("positron")) |>
      fit_bounds(d, animate = FALSE) |>
      add_fill_layer(
        id = "counties",
        source = d,
        fill_color = list("get", "fillcol"),
        fill_opacity = 0.75
      ) |>
      add_line_layer(
        id = "borders",
        source = d,
        line_color = "#808080",
        line_width = 0.2,
        line_opacity = 0.5
      )
  })
  
  # filter by percent range using a proxy (fast, client-side)
  observe({
    req(input$pct)
    lo <- input$pct[1]; hi <- input$pct[2]
    maplibre_proxy("map") |>
      set_filter("counties", list(
        "all",
        list(">=", get_column("percent_speakers"), lo),
        list("<=", get_column("percent_speakers"), hi)
      ))
  })
}

shinyApp(ui, server)