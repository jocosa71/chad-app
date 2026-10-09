# =============================================================================
# Chad MRV electrification platform - Shiny prototype for the EOI demo
# -----------------------------------------------------------------------------
# A single-file Shiny app (bslib) letting ADERM explore the OnSSET scenario
# results: pick a scenario, filter technologies, see KPIs, an interactive map
# and a ranked table of priority mini-grid sites. UI text is in French.
#
# Input : onsset_results/*_Results.csv (same files used by 03/04 scripts)
#         TCD_Chad/Data/TCD/Boundaries/TCD_adm_0.gpkg
#
# Run with: shiny::runApp() from the project root, or click "Run App" in
# RStudio/Positron with this file open.
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(here)
library(leaflet)
library(leafgl)
library(DT)


# ---- Data preparation (runs once, at app start) ------------------------------

results_dir <- here("onsset_results")
admin_path  <- list.files(here("TCD_Chad", "Data", "TCD", "Boundaries"),
                          pattern = "TCD_adm_0\\.gpkg$", full.names = TRUE,
                          recursive = TRUE)[1]

# Technology labels (French, for display) and colours (consistent with the
# methodology note figures)
tech_levels_en <- c("Grid densification", "Grid extension", "Stand-alone PV (SHS)",
                    "PV hybrid mini-grid", "Wind mini-grid", "Hydro mini-grid",
                    "Not electrified")
tech_levels_fr <- c("Densification du reseau", "Extension du reseau",
                    "Systeme solaire autonome", "Mini-reseau solaire hybride",
                    "Mini-reseau eolien", "Mini-reseau hydraulique",
                    "Non electrifie")
tech_colours <- c("#4e53de", "#a6aaff", "#ffc700", "#e628a0",
                  "#1b8f4d", "#28e66d", "#9aa0a6") %>%
  set_names(tech_levels_fr)

scenario_labels <- c(low = "Demande faible", medium = "Demande moyenne",
                     high = "Demande elevee",
                     high_uncapped = "Demande elevee, reseau sans limite")

scenario_from_file <- function(path) {
  stem <- basename(path) %>% str_remove("_Results\\.csv$")
  demand <- str_match(stem, "_(low|medium|high)_demand")[, 2]
  key <- if (str_detect(stem, "_uncapped_grid")) "high_uncapped" else demand
  key
}

cols_needed <- c("id", "X_deg", "Y_deg", "ElecStart", "FinalElecCode2030",
                 "Pop2030", "NewConnections2030", "NewCapacity2030",
                 "InvestmentCost2030", "InvestmentPerConnection2030",
                 "MinimumOverallLCOE2030", "Admin_1")

result_files <- list.files(results_dir, pattern = "_Results\\.csv$", full.names = TRUE)

all_results <- map_dfr(result_files, function(f) {
  key <- scenario_from_file(f)
  read_csv(f, col_select = any_of(cols_needed), show_col_types = FALSE) %>%
    mutate(scenario_key = key)
}) %>%
  mutate(
    tech_fr = case_when(
      FinalElecCode2030 == 1 & ElecStart == 1 ~ tech_levels_fr[1],
      FinalElecCode2030 %in% c(1, 2)          ~ tech_levels_fr[2],
      FinalElecCode2030 == 3                  ~ tech_levels_fr[3],
      FinalElecCode2030 == 5                  ~ tech_levels_fr[4],
      FinalElecCode2030 == 6                  ~ tech_levels_fr[5],
      FinalElecCode2030 == 7                  ~ tech_levels_fr[6],
      TRUE                                    ~ tech_levels_fr[7]
    ),
    tech_fr = factor(tech_fr, levels = tech_levels_fr)
  )

admin <- st_read(admin_path, quiet = TRUE)

available_scenarios <- scenario_labels[names(scenario_labels) %in% unique(all_results$scenario_key)]


# ---- UI -----------------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#e628a0",
  secondary = "#4e53de",
  success = "#1b8f4d",
  warning = "#ffc700",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
) %>%
  bs_add_rules("
    .bslib-sidebar-layout > .sidebar > .sidebar-content,
    .sidebar-content {
      font-size: 0.78rem !important;
      padding: 0.75rem !important;
    }
    .sidebar-content h2, .sidebar-content .h2 {
      font-size: 0.9rem !important;
      margin-bottom: 0.4rem !important;
    }
    .sidebar-content hr {
      margin: 0.5rem 0 !important;
    }
    .sidebar-content .form-group {
      margin-bottom: 0.4rem !important;
    }
    .sidebar-content .radio, .sidebar-content .checkbox {
      margin-top: 1px !important;
      margin-bottom: 1px !important;
    }
    .sidebar-content p {
      margin-bottom: 0.3rem !important;
    }
    .sidebar-content .irs {
      margin-top: 0.2rem !important;
    }
  ")

ui <- page_sidebar(
  title = "Plateforme d'electrification du Tchad - Demo Mini-Reseaux Verts",
  theme = app_theme,
  
  sidebar = sidebar(
    width = 300,
    title = "Parametres",
    
    radioButtons(
      "scenario", "Scenario",
      choices = available_scenarios,
      selected = available_scenarios[1]
    ),
    
    checkboxGroupInput(
      "techs", "Technologies affichees",
      choices = tech_levels_fr,
      selected = tech_levels_fr
    ),
    
    hr(),
    
    sliderInput(
      "min_pop", "Population minimale du site (pour le tableau)",
      min = 0, max = 5000, value = 500, step = 100
    ),
    
    hr(),
    p(class = "text-muted small",
      "Donnees: OnSSET Starter Data Kit (Tchad), scenarios de demande et de reseau, annee cible 2030.")
  ),
  
  # ---- KPI row --------------------------------------------------------------
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Population totale (2030)",
      value = textOutput("kpi_pop", inline = TRUE),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "primary"
    ),
    value_box(
      title = "Nouveaux raccordements",
      value = textOutput("kpi_connections", inline = TRUE),
      showcase = bsicons::bs_icon("plug-fill"),
      theme = "secondary"
    ),
    value_box(
      title = "Investissement total",
      value = textOutput("kpi_investment", inline = TRUE),
      showcase = bsicons::bs_icon("cash-coin"),
      theme = "success"
    ),
    value_box(
      title = "Part des mini-reseaux",
      value = textOutput("kpi_mg_share", inline = TRUE),
      showcase = bsicons::bs_icon("pie-chart-fill"),
      theme = "warning"
    )
  ),
  
  # ---- Main tabs --------------------------------------------------------------
  navset_card_tab(
    nav_panel(
      "Carte",
      leafletOutput("map", height = 650)
    ),
    nav_panel(
      "Comparaison des scenarios",
      plotOutput("comparison_plot", height = 550)
    ),
    nav_panel(
      "Sites prioritaires (mini-reseaux)",
      p(class = "text-muted",
        "Sites classes par cout d'investissement par raccordement (du moins cher au plus cher)."),
      DTOutput("priority_table"),
      downloadButton("download_table", "Telecharger le tableau (CSV)", class = "btn-primary mt-2")
    )
  )
)


# ---- Server ---------------------------------------------------------------

server <- function(input, output, session) {
  
  # Settlements for the selected scenario
  scenario_data <- reactive({
    key <- names(available_scenarios)[available_scenarios == input$scenario]
    all_results %>% filter(scenario_key == key)
  })
  
  # Same data, filtered by the technologies checked in the sidebar (map + KPIs)
  filtered_data <- reactive({
    req(input$techs)
    scenario_data() %>% filter(tech_fr %in% input$techs)
  })
  
  # ---- KPIs (computed on the full scenario, not the technology filter) ------
  
  output$kpi_pop <- renderText({
    format(round(sum(scenario_data()$Pop2030)), big.mark = " ")
  })
  
  output$kpi_connections <- renderText({
    format(round(sum(scenario_data()$NewConnections2030)), big.mark = " ")
  })
  
  output$kpi_investment <- renderText({
    inv_musd <- sum(scenario_data()$InvestmentCost2030) / 1e6
    paste0(format(round(inv_musd), big.mark = " "), " M USD")
  })
  
  output$kpi_mg_share <- renderText({
    df <- scenario_data()
    mg_pop <- df %>% filter(tech_fr == "Mini-reseau solaire hybride") %>% pull(Pop2030) %>% sum()
    paste0(round(100 * mg_pop / sum(df$Pop2030)), " %")
  })
  
  # ---- Map --------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
      addPolylines(data = st_cast(admin, "MULTILINESTRING"),
                   color = "#333333", weight = 1.2, opacity = 0.8) %>%
      addLegend(position = "bottomright", colors = unname(tech_colours),
                labels = names(tech_colours), title = "Technologie", opacity = 0.9) %>%
      fitBounds(13, 7, 24, 24)
  })
  
  # Update points when the scenario or the technology filter changes,
  # without redrawing the base map (tiles, boundary, legend) each time
  observe({
    df <- filtered_data() %>%
      st_as_sf(coords = c("X_deg", "Y_deg"), crs = 4326, remove = FALSE)
    
    leafletProxy("map") %>%
      clearGlLayers() %>%
      addGlPoints(
        data = df,
        fillColor = unname(tech_colours[as.character(df$tech_fr)]),
        radius = 5, fillOpacity = 0.9
      )
  })
  
  # ---- Comparison plot (all scenarios, all technologies, for context) --------
  
  output$comparison_plot <- renderPlot({
    all_results %>%
      group_by(scenario_key, tech_fr) %>%
      summarise(pop_share = sum(Pop2030), .groups = "drop") %>%
      group_by(scenario_key) %>%
      mutate(pop_share = pop_share / sum(pop_share)) %>%
      ungroup() %>%
      mutate(
        scenario_label = factor(scenario_labels[scenario_key],
                                levels = unname(available_scenarios)),
        highlight = scenario_labels[scenario_key] == input$scenario
      ) %>%
      ggplot(aes(scenario_label, pop_share, fill = tech_fr, alpha = highlight)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = tech_colours, name = NULL) +
      scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
      labs(x = NULL, y = "Part de la population",
           title = "Population desservie par technologie, par scenario") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 15, hjust = 1))
  })
  
  # ---- Priority table (mini-grid sites, selected scenario) -------------------
  
  priority_data <- reactive({
    scenario_data() %>%
      filter(tech_fr == "Mini-reseau solaire hybride", Pop2030 >= input$min_pop) %>%
      transmute(
        `Population 2030` = round(Pop2030),
        `Nouveaux raccordements` = round(NewConnections2030),
        `Capacite (kW)` = round(NewCapacity2030, 1),
        `Investissement (USD)` = round(InvestmentCost2030),
        `Cout par raccordement (USD)` = round(InvestmentPerConnection2030),
        `LCOE (USD/kWh)` = round(MinimumOverallLCOE2030, 3)
      ) %>%
      arrange(`Cout par raccordement (USD)`)
  })
  
  output$priority_table <- renderDT({
    datatable(priority_data(), rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste0("sites_prioritaires_mini_reseaux_", input$scenario, ".csv"),
    content = function(file) write_csv(priority_data(), file)
  )
}

shinyApp(ui, server)