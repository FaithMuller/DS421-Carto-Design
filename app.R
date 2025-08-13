# app.R
# Shiny app to display MPA polygons with MapLibre via {mapgl},
# using Shiny map inputs + proxies (set_filter) and popups via layer column.

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(mapgl)
library(here)
library(viridisLite)
library(glue)

# ---- Load data once ----
mpa <- st_read(here("data/NOAA_Marine_Protected_Area_(MPA)_Inventory.geojson"), quiet = TRUE) |>
  st_transform(4326) |>
  mutate(
    areakm   = as.numeric(areakm),
    prot_lvl = as.character(prot_lvl),
    state    = as.character(state),
    popup    = glue("<b>{site_name}</b><br>State: {state}<br>Protection: {prot_lvl}<br>IUCN: {iucncat}<br>Area: {round(areakm, 2)} km²")
  )

# Categorical palette for protection levels
prot_lvls <- sort(unique(mpa$prot_lvl))
prot_cols <- setNames(viridis(length(prot_lvls)), prot_lvls)

# Build a MapLibre match expression to color by prot_lvl (fully unnamed list)
kv <- as.vector(rbind(names(prot_cols), unname(prot_cols)))
color_expr <- c(list("match", get_column("prot_lvl")), as.list(kv), list("#cccccc"))

# ---- UI ----
ui <- page_sidebar(
  title = "MPA Inventory — mapgl (MapLibre)",
  sidebar = sidebar(
    selectInput("state_filter", "State/Territory",
                choices = c("All", sort(unique(mpa$state))), selected = "HI"),
    selectInput("prot_filter", "Protection level",
                choices = c("All", prot_lvls), selected = "All"),
    textInput("name_search", "Search by site name", placeholder = "e.g., Hanauma"),
    helpText("Tip: Filters apply instantly via set_filter(). Click a polygon to see a popup.")
  ),
  card(full_screen = TRUE,
       maplibreOutput("map", height = "80vh")),
  footer = tagList(
    HTML("<small>Basemap: carto \"positron\". Data: NOAA MPA Inventory (GeoJSON).</small>")
  )
)

# ---- Server ----
server <- function(input, output, session) {
  output$map <- renderMaplibre({
    maplibre(style = carto_style("positron")) |>
      fit_bounds(mpa, animate = FALSE) |>
      add_fill_layer(
        id = "mpa_polys",
        source = mpa,
        fill_color = color_expr,
        fill_opacity = 0.55,
        popup = "popup",
        tooltip = "site_name",
        hover_options = list(
          fill_color = "yellow",
          fill_opacity = 1
        )
      ) |>
      add_line_layer(
        id = "mpa_outline",
        source = mpa,
        line_color = "#1F2D3D",
        line_width = 1
      ) |>
      add_navigation_control() |>
      add_fullscreen_control() |>
      add_legend(
        legend_title = "Protection level",
        values = names(prot_cols),
        colors = unname(prot_cols),
        type = "categorical"
      )
  })
  
  # Apply filter whenever inputs change
  observe({
    conds <- list()
    if (input$state_filter != "All")
      conds <- append(conds, list(list("==", get_column("state"), input$state_filter)))
    if (input$prot_filter  != "All")
      conds <- append(conds, list(list("==", get_column("prot_lvl"), input$prot_filter)))
    if (nzchar(input$name_search)) {
      q <- tolower(input$name_search)
      conds <- append(conds, list(list("!=", -1, list("index-of", q, list("downcase", get_column("site_name"))))))
    }
    filter_expr <- if (length(conds) > 0) c(list("all"), conds) else list("all")
    
    maplibre_proxy("map") |>
      set_filter("mpa_polys", filter_expr) |>
      set_filter("mpa_outline", filter_expr)
  })
}

shinyApp(ui, server)
