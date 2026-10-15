# ============================================================
# BETT Dissertation Dashboard Prototype
# Version 8.3 polished dashboard
# Structure:
# - Overview
# - Bill difficulty
# - Bill worry
# - Heating behaviour
# - Travel behaviour
# - Model results: model plots + model KPI tiles only (v8.3 refreshed)
# - Regression Results: interpretation/key findings
# - About / Methods
# ============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(data.table)

# ============================================================
# Load dashboard dataset
# ============================================================

data_path <- "../outputs/bett_dashboard_small.csv"

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Dashboard data file not found at: ", data_path, "\n",
      "The public GitHub repository does not redistribute the underlying BETT-derived dataset. ",
      "Place an authorised copy of bett_dashboard_small.csv in the outputs/ folder to run the app locally."
    )
  )
}

bett_dashboard <- fread(data_path)

required_columns <- c(
  "year",
  "season",
  "wave_label_ordered",
  "income_group",
  "tenure",
  "region",
  "urbanrural",
  "bill_difficulty_binary",
  "bills_worry_binary",
  "heating_7days_binary",
  "drive2k_binary"
)

missing_columns <- setdiff(required_columns, names(bett_dashboard))
if (length(missing_columns) > 0) {
  stop(
    paste(
      "The dashboard dataset is missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Convert blank character values to NA without changing numeric columns.
char_cols <- names(bett_dashboard)[sapply(bett_dashboard, is.character)]
for (col in char_cols) {
  bett_dashboard[[col]][bett_dashboard[[col]] == ""] <- NA
}

# Standardise dashboard fields as character variables for filtering and plotting.
character_fields <- c(
  "year", "season", "wave_label_ordered", "income_group",
  "tenure", "region", "urbanrural", "bill_difficulty_binary",
  "bills_worry_binary", "heating_7days_binary", "drive2k_binary"
)

for (col in character_fields) {
  bett_dashboard[[col]] <- as.character(bett_dashboard[[col]])
}

message("Dashboard data loaded: ", format(nrow(bett_dashboard), big.mark = ","), " rows")

# Keep full dashboard sample.
# "No fixed address" is excluded only from tenure comparison graphs.

# ============================================================
# Helper functions
# ============================================================

clean_choices <- function(x) {
  sort(unique(as.character(x[!is.na(x) & x != ""])))
}

format_count <- function(x) {
  format(x, big.mark = ",")
}

format_percent <- function(x) {
  ifelse(is.na(x), "NA", paste0(round(x, 1), "%"))
}

comparison_labels <- c(
  year = "year",
  season = "season",
  income_group = "income group",
  tenure = "tenure",
  region = "region",
  urbanrural = "urban/rural location"
)

comparison_axis_labels <- c(
  year = "Year",
  season = "Season",
  income_group = "Income group",
  tenure = "Tenure",
  region = "Region",
  urbanrural = "Urban/rural location"
)

metric_strip <- function(items) {
  div(
    class = "metric-strip",
    lapply(items, function(item) {
      div(
        class = "metric-card",
        div(class = "metric-label", item$label),
        div(class = "metric-value", item$value),
        if (!is.null(item$subtext)) {
          div(class = "metric-subtext", item$subtext)
        }
      )
    })
  )
}

# ============================================================
# Plot theme helper
# Larger text and cleaner dashboard graph styling
# ============================================================

bett_plot_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        color = "#1F2A44",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 13,
        color = "#5B6775",
        margin = margin(b = 8)
      ),
      axis.title.x = element_text(
        face = "bold",
        size = 14,
        color = "#2F3A45",
        margin = margin(t = 8)
      ),
      axis.title.y = element_text(
        face = "bold",
        size = 14,
        color = "#2F3A45",
        margin = margin(r = 8)
      ),
      axis.text.x = element_text(
        size = 13,
        color = "#2F3A45"
      ),
      axis.text.y = element_text(
        size = 13,
        color = "#2F3A45"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(6, 18, 6, 8)
    )
}

make_percentage_plot <- function(
    data,
    compare_var,
    outcome_var,
    positive_level,
    plot_title,
    plot_subtitle,
    y_label,
    fill_colour = "#007C89"
) {
  
  data <- data[!is.na(get(outcome_var))]
  data <- data[!is.na(get(compare_var))]
  
  if (compare_var == "tenure") {
    data <- data[get(compare_var) != "No fixed address"]
  }
  
  validate(
    need(nrow(data) > 0, "No valid data available for the selected filters.")
  )
  
  plot_table <- data[
    ,
    .N,
    by = c(compare_var, outcome_var)
  ]
  
  plot_table[
    ,
    Percentage := N / sum(N) * 100,
    by = compare_var
  ]
  
  plot_table <- plot_table[
    get(outcome_var) == positive_level
  ]
  
  setnames(plot_table, compare_var, "Comparison_group")
  
  # ----------------------------------------------------------
  # Short display labels for long categories.
  # Full category labels remain in filters and source data.
  # ----------------------------------------------------------
  
  plot_table[, Display_group := as.character(Comparison_group)]
  
  if (compare_var == "region") {
    plot_table[grepl("^Dublin", Display_group), Display_group := "Dublin"]
    plot_table[grepl("^Connacht", Display_group), Display_group := "Connacht"]
    plot_table[grepl("^Leinster", Display_group), Display_group := "Leinster excl. Dublin"]
    plot_table[grepl("^Munster", Display_group), Display_group := "Munster"]
    plot_table[grepl("^Ulster", Display_group), Display_group := "Ulster"]
  }
  
  if (compare_var == "tenure") {
    plot_table[Display_group == "Own home outright", Display_group := "Own outright"]
    plot_table[Display_group == "Own home with loan/mortgage", Display_group := "Mortgage holder"]
    plot_table[Display_group == "Renting (private landlord)", Display_group := "Private renter"]
    plot_table[
      Display_group == "Renting (local authority or housing association)",
      Display_group := "LA/HA renter"
    ]
    plot_table[
      Display_group == "Living rent-free (e.g. with parents or friends)",
      Display_group := "Rent-free"
    ]
  }
  
  number_of_groups <- length(unique(plot_table$Display_group))
  bar_width <- ifelse(number_of_groups <= 3, 0.42, 0.58)
  y_axis_max <- max(plot_table$Percentage, na.rm = TRUE) * 1.20
  
  # ----------------------------------------------------------
  # Year and season are easier as vertical bars.
  # Other comparison variables are easier as horizontal bars.
  # ----------------------------------------------------------
  
  if (compare_var %in% c("year", "season")) {
    
    if (compare_var == "season") {
      plot_table$Display_group <- factor(
        plot_table$Display_group,
        levels = c("Winter", "Spring", "Summer", "Autumn")
      )
    }
    
    ggplot(
      plot_table,
      aes(x = Display_group, y = Percentage)
    ) +
      geom_col(fill = fill_colour, width = bar_width) +
      scale_y_continuous(
        limits = c(0, y_axis_max),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = comparison_axis_labels[[compare_var]],
        y = y_label
      ) +
      bett_plot_theme()
    
  } else {
    
    ggplot(
      plot_table,
      aes(
        x = reorder(Display_group, Percentage),
        y = Percentage
      )
    ) +
      geom_col(fill = fill_colour, width = bar_width) +
      coord_flip() +
      scale_y_continuous(
        limits = c(0, y_axis_max),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = "",
        y = y_label
      ) +
      bett_plot_theme()
  }
}

# ============================================================
# bslib theme
# ============================================================

bett_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0B5D8C",
  secondary = "#5B6775",
  success = "#007C89",
  info = "#56B4E9",
  warning = "#C47F00",
  danger = "#B00020",
  base_font = "Arial",
  heading_font = "Arial"
)

# ============================================================
# UI
# ============================================================

ui <- page_sidebar(
  
  title = div(
    class = "dashboard-title-row",
    div(
      class = "dashboard-title-left",
      div(class = "dashboard-title-main", "BETT Energy Poverty Dashboard"),
      div(class = "dashboard-title-subtitle", "Dissertation prototype | SEAI Behavioural Energy and Travel Tracker")
    ),
    div(
      class = "dashboard-title-right",
      div(class = "dashboard-title-chip", "Dibyajyoti Dash"),
      div(class = "dashboard-title-chip", "Supervisor: Emma Howard"),
      div(class = "dashboard-title-chip", "MSc Statistics & Sustainability")
    )
  ),
  theme = bett_theme,
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F7F9FB;
        color: #2F3A45;
        font-family: Arial, Helvetica, sans-serif;
      }

      .bslib-page-title {
        background: linear-gradient(135deg, #073B63 0%, #0B5D8C 58%, #0E7FA8 100%) !important;
        color: #FFFFFF !important;
        padding: 22px 28px !important;
        margin-bottom: 0 !important;
        border-bottom: 5px solid #56B4E9;
        box-shadow: 0 4px 16px rgba(11, 45, 77, 0.24);
      }

      .dashboard-title-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 24px;
        width: 100%;
      }

      .dashboard-title-left {
        display: flex;
        flex-direction: column;
        gap: 5px;
      }

      .dashboard-title-main {
        font-size: 27px;
        font-weight: 800;
        letter-spacing: -0.02em;
        color: #FFFFFF;
      }

      .dashboard-title-subtitle {
        font-size: 14px;
        font-weight: 500;
        color: #E6F2F4;
      }

      .dashboard-title-right {
        display: flex;
        flex-wrap: wrap;
        justify-content: flex-end;
        gap: 8px;
        max-width: 520px;
      }

      .dashboard-title-chip {
        background-color: rgba(255, 255, 255, 0.14);
        border: 1px solid rgba(255, 255, 255, 0.24);
        border-radius: 999px;
        padding: 6px 11px;
        font-size: 12px;
        font-weight: 700;
        color: #FFFFFF;
        white-space: nowrap;
      }

      .sidebar {
        background-color: #FFFFFF;
        border-right: 1px solid #D9E2EC;
      }

      .card {
        border-radius: 14px;
        border: 1px solid #D9E2EC;
        box-shadow: 0 2px 10px rgba(31, 42, 68, 0.06);
        background-color: #FFFFFF;
      }

      .card-header {
        font-weight: 700;
        color: #1F2A44;
        background-color: #FFFFFF;
        border-bottom: 1px solid #E5ECF2;
      }

      .nav-tabs {
        border-bottom: 1px solid #D9E2EC;
      }

      .nav-tabs .nav-link {
        color: #1F2A44;
        font-weight: 700;
      }

      .nav-tabs .nav-link.active {
        color: #007C89;
        border-color: #D9E2EC #D9E2EC #FFFFFF;
        background-color: #FFFFFF;
      }

      .metric-strip {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 10px;
        margin-bottom: 14px;
      }

      .metric-card {
        background-color: #FFFFFF;
        border: 1px solid #D9E2EC;
        border-left: 4px solid #007C89;
        border-radius: 10px;
        padding: 10px 12px;
        min-height: 72px;
        box-shadow: 0 2px 8px rgba(31, 42, 68, 0.04);
      }

      .metric-label {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: #5B6775;
        font-weight: 700;
        margin-bottom: 4px;
      }

      .metric-value {
        font-size: 20px;
        color: #1F2A44;
        font-weight: 700;
        line-height: 1.15;
      }

      .metric-subtext {
        font-size: 11px;
        color: #6B7785;
        margin-top: 3px;
      }

      .form-label {
        font-weight: 700;
        color: #1F2A44;
      }

      .form-select,
      .selectize-input {
        border-radius: 8px;
        border-color: #C8D3DF;
      }

      .small-note {
        background-color: #EEF4F7;
        border-left: 5px solid #007C89;
        border-radius: 10px;
        padding: 10px 12px;
        color: #2F3A45;
        margin-top: 10px;
        font-size: 13px;
      }

      .warning-note {
        background-color: #FFF7E6;
        border-left: 5px solid #C47F00;
        border-radius: 10px;
        padding: 10px 12px;
        color: #2F3A45;
        margin-top: 10px;
        font-size: 13px;
      }

      .control-row {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 0;
        padding-bottom: 0;
      }

      .control-row .shiny-input-container {
        width: 560px !important;
        margin-bottom: 0 !important;
        display: flex;
        align-items: center;
        gap: 12px;
      }

      .control-row label {
        margin-bottom: 0 !important;
        white-space: nowrap;
        font-weight: 700;
        color: #1F2A44;
      }

      .control-row .selectize-control,
      .control-row select {
        min-width: 230px;
      }

      .plot-holder {
        margin-top: 0;
        padding-top: 0;
        clear: both;
      }

      .compact-chart-card .card-body {
        padding-top: 10px;
        padding-bottom: 6px;
      }

      .compact-chart-card .form-group,
      .compact-chart-card .mb-3 {
        margin-bottom: 0 !important;
      }

      .compact-chart-card .shiny-plot-output {
        overflow: visible !important;
      }

      /* When the bslib card is expanded, allow more vertical space for the plot. */
      .bslib-full-screen .compact-chart-card .shiny-plot-output,
      .bslib-card-full-screen .compact-chart-card .shiny-plot-output,
      .bslib-full-screen-enter .compact-chart-card .shiny-plot-output {
        height: calc(100vh - 150px) !important;
      }

      .model-plot-container {
        width: 88%;
        max-width: 1050px;
        margin: 12px auto 0 auto;
      }

      .model-plot-container .shiny-image-output {
        width: 100% !important;
        height: 520px !important;
      }

      .model-plot-container img {
        width: 100% !important;
        height: 100% !important;
        object-fit: contain !important;
        display: block;
      }

      .bslib-full-screen .model-plot-container,
      .bslib-card-full-screen .model-plot-container,
      .bslib-full-screen-enter .model-plot-container {
        width: 82%;
        max-width: 980px;
      }

      .bslib-full-screen .model-plot-container .shiny-image-output,
      .bslib-card-full-screen .model-plot-container .shiny-image-output,
      .bslib-full-screen-enter .model-plot-container .shiny-image-output {
        height: 500px !important;
      }

      @media (max-width: 900px) {
        .control-row .shiny-input-container {
          width: 100% !important;
          flex-direction: column;
          align-items: flex-start;
        }
      }

      .app-subtitle {
        color: #5B6775;
        font-size: 14px;
        margin-top: 8px;
        margin-bottom: 14px;
      }

      .compact-selection {
        font-size: 14px;
        line-height: 1.65;
      }

      .compact-selection strong {
        color: #1F2A44;
      }

      .findings-list li {
        margin-bottom: 7px;
      }

      pre {
        background-color: #F4F7FA;
        border: 1px solid #D9E2EC;
        border-radius: 10px;
        color: #2F3A45;
      }

      @media (max-width: 1000px) {
        .metric-strip {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }



      @media (max-width: 900px) {
        .dashboard-title-row {
          flex-direction: column;
          align-items: flex-start;
        }
        .dashboard-title-right {
          justify-content: flex-start;
        }
      }

      @media (max-width: 650px) {
        .metric-strip {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  sidebar = sidebar(
    width = 280,
    
    h4("Global filters"),
    p(
      "These filters subset the whole dashboard. The dropdown inside each tab only changes how the selected data is grouped in that chart."
    ),
    
    selectInput(
      inputId = "year_filter",
      label = "Year",
      choices = c("All", clean_choices(bett_dashboard$year)),
      selected = "All"
    ),
    
    selectInput(
      inputId = "season_filter",
      label = "Season",
      choices = c("All", "Winter", "Spring", "Summer", "Autumn"),
      selected = "All"
    ),
    
    selectInput(
      inputId = "region_filter",
      label = "Region",
      choices = c("All", clean_choices(bett_dashboard$region)),
      selected = "All"
    ),
    
    selectInput(
      inputId = "urbanrural_filter",
      label = "Urban/rural",
      choices = c("All", clean_choices(bett_dashboard$urbanrural)),
      selected = "All"
    ),
    
    div(
      class = "small-note",
      strong("Note: "),
      "Percentages are calculated among valid responses only. Regression model plots do not change with these filters."
    )
  ),
  
  div(
    class = "app-subtitle",
    "SEAI Behavioural Energy and Travel Tracker | January 2023 to November 2025"
  ),
  
  navset_card_tab(
    
    # --------------------------------------------------------
    # Overview tab
    # --------------------------------------------------------
    
    nav_panel(
      "Overview",
      
      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
            span("Dashboard overview"),
            span(
              style = "font-size:12px; font-weight:400; color:#6B7785;",
              "Scroll down for more details ↓"
            )
          )
        ),
        p("This prototype summarises selected BETT indicators from January 2023 to November 2025."),
        p("Bill difficulty is treated as the main energy-poverty outcome, while bill worry is treated as a supplementary financial-stress measure.")
      ),
      
      uiOutput("overview_metric_strip"),
      
      card(
        full_screen = TRUE,
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
            span("Current selection"),
            span(
              style = "font-size:12px; font-weight:400; color:#6B7785;",
              "Scroll down for more details ↓"
            )
          )
        ),
        uiOutput("selection_summary_compact")
      )
    ),
    
    # --------------------------------------------------------
    # Bill difficulty tab
    # --------------------------------------------------------
    
    nav_panel(
      "Bill difficulty",
      
      uiOutput("difficulty_metric_strip"),
      
      card(
        class = "compact-chart-card",
        full_screen = TRUE,
        card_header("Bill difficulty explorer"),
        
        div(
          class = "control-row",
          selectInput(
            inputId = "difficulty_compare",
            label = "Group bill difficulty chart by",
            choices = c(
              "Year" = "year",
              "Income group" = "income_group",
              "Tenure" = "tenure",
              "Region" = "region",
              "Urban/rural" = "urbanrural"
            ),
            selected = "year"
          )
        ),
        
        div(
          class = "plot-holder",
          plotOutput("bill_difficulty_dynamic_plot", height = "340px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Bill worry tab
    # --------------------------------------------------------
    
    nav_panel(
      "Bill worry",
      
      uiOutput("worry_metric_strip"),
      
      card(
        class = "compact-chart-card",
        full_screen = TRUE,
        card_header("Bill worry explorer"),
        
        div(
          class = "control-row",
          selectInput(
            inputId = "worry_compare",
            label = "Group bill worry chart by",
            choices = c(
              "Year" = "year",
              "Income group" = "income_group",
              "Tenure" = "tenure",
              "Region" = "region",
              "Urban/rural" = "urbanrural"
            ),
            selected = "year"
          )
        ),
        
        div(
          class = "plot-holder",
          plotOutput("bill_worry_dynamic_plot", height = "340px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Heating behaviour tab
    # --------------------------------------------------------
    
    nav_panel(
      "Heating behaviour",
      
      uiOutput("heating_metric_strip"),
      
      card(
        class = "compact-chart-card",
        full_screen = TRUE,
        card_header("Heating behaviour explorer"),
        
        div(
          class = "control-row",
          selectInput(
            inputId = "heating_compare",
            label = "Group heating behaviour chart by",
            choices = c(
              "Year" = "year",
              "Season" = "season",
              "Income group" = "income_group",
              "Tenure" = "tenure",
              "Region" = "region",
              "Urban/rural" = "urbanrural"
            ),
            selected = "season"
          )
        ),
        
        div(
          class = "plot-holder",
          plotOutput("heating_dynamic_plot", height = "340px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Travel behaviour tab
    # --------------------------------------------------------
    
    nav_panel(
      "Travel behaviour",
      
      uiOutput("travel_metric_strip"),
      
      card(
        class = "compact-chart-card",
        full_screen = TRUE,
        card_header("Travel behaviour explorer"),
        
        div(
          class = "control-row",
          selectInput(
            inputId = "travel_compare",
            label = "Group travel behaviour chart by",
            choices = c(
              "Year" = "year",
              "Income group" = "income_group",
              "Tenure" = "tenure",
              "Region" = "region",
              "Urban/rural" = "urbanrural"
            ),
            selected = "year"
          )
        ),
        
        div(
          class = "plot-holder",
          plotOutput("travel_dynamic_plot", height = "340px")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Model results tab: plots only
    # --------------------------------------------------------
    
    nav_panel(
      "Model results",
      
      navset_card_tab(
        
        nav_panel(
          "Bill difficulty",
          
          metric_strip(
            list(
              list(
                label = "Outcome",
                value = "Bill difficulty",
                subtext = "Main energy-poverty outcome"
              ),
              list(
                label = "Model type",
                value = "Logistic",
                subtext = "Binary outcome model"
              ),
              list(
                label = "Complete cases",
                value = "18,994",
                subtext = "Used in regression"
              ),
              list(
                label = "Status",
                value = "Main model",
                subtext = "Primary analysis"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Odds-ratio plot: bill difficulty model"),
                span(
                  style = "font-size:12px; font-weight:400; color:#6B7785;",
                  "Selected adjusted estimates"
                )
              )
            ),
            
            div(
              class = "small-note",
              strong("Model note: "),
              "Selected adjusted odds ratios are shown for readability. Values above 1 indicate higher odds and values below 1 indicate lower odds relative to the reference category. Model estimates are fixed and do not change with dashboard filters."
            ),
            
            div(
              class = "model-plot-container",
              imageOutput("difficulty_model_plot")
            )
          )
        ),
        
        nav_panel(
          "Bill worry",
          
          metric_strip(
            list(
              list(
                label = "Outcome",
                value = "Bill worry",
                subtext = "Financial-stress indicator"
              ),
              list(
                label = "Model type",
                value = "Logistic",
                subtext = "Binary outcome model"
              ),
              list(
                label = "Complete cases",
                value = "10,791",
                subtext = "Used in regression"
              ),
              list(
                label = "Status",
                value = "Supplementary",
                subtext = "Interpret cautiously"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Odds-ratio plot: bill worry model"),
                span(
                  style = "font-size:12px; font-weight:400; color:#6B7785;",
                  "Selected adjusted estimates"
                )
              )
            ),
            
            div(
              class = "warning-note",
              strong("Model note: "),
              "Selected adjusted odds ratios are shown for readability. Bill worry is a supplementary outcome because of its higher missingness. Values above 1 indicate higher odds and values below 1 indicate lower odds relative to the reference category. Model estimates are fixed and do not change with dashboard filters."
            ),
            
            div(
              class = "model-plot-container",
              imageOutput("worry_model_plot")
            )
          )
        ),
        
        nav_panel(
          "Heating behaviour",
          
          metric_strip(
            list(
              list(
                label = "Outcome",
                value = "Heating 7 days",
                subtext = "Heating-use behaviour"
              ),
              list(
                label = "Model type",
                value = "Logistic",
                subtext = "Binary outcome model"
              ),
              list(
                label = "Complete cases",
                value = "20,547",
                subtext = "Used in regression"
              ),
              list(
                label = "Status",
                value = "Behaviour model",
                subtext = "Secondary analysis"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Odds-ratio plot: heating behaviour model"),
                span(
                  style = "font-size:12px; font-weight:400; color:#6B7785;",
                  "Selected adjusted estimates"
                )
              )
            ),
            
            div(
              class = "small-note",
              strong("Model note: "),
              "Selected adjusted odds ratios are shown for readability. The full model includes season, although seasonal predictors may be omitted from the displayed plot where necessary to improve readability. Values above 1 indicate higher odds and values below 1 indicate lower odds of heating seven days per week relative to the reference category. Model estimates are fixed and do not change with dashboard filters."
            ),
            
            div(
              class = "model-plot-container",
              imageOutput("heating_model_plot")
            )
          )
        ),
        
        nav_panel(
          "Travel behaviour",
          
          metric_strip(
            list(
              list(
                label = "Outcome",
                value = "Driving 2 km",
                subtext = "Perceived energy-intensity outcome"
              ),
              list(
                label = "Model type",
                value = "Logistic",
                subtext = "Binary outcome model"
              ),
              list(
                label = "Complete cases",
                value = "11,792",
                subtext = "Used in regression"
              ),
              list(
                label = "Status",
                value = "Exploratory",
                subtext = "Supplementary analysis"
              )
            )
          ),
          
          card(
            full_screen = TRUE,
            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
                span("Odds-ratio plot: travel behaviour model"),
                span(
                  style = "font-size:12px; font-weight:400; color:#6B7785;",
                  "Selected adjusted estimates"
                )
              )
            ),
            
            div(
              class = "small-note",
              strong("Model note: "),
              "Selected adjusted odds ratios are shown for readability. Values above 1 indicate higher odds and values below 1 indicate lower odds of higher perceived energy intensity of driving 2 km relative to the reference category. Model estimates are fixed and do not change with dashboard filters."
            ),
            
            div(
              class = "model-plot-container",
              imageOutput("travel_model_plot")
            )
          )
        )
      )
    ),
    # --------------------------------------------------------
    # Regression Results tab: interpretation and key findings
    # --------------------------------------------------------
    
    nav_panel(
      "Regression Results",
      
      navset_card_tab(
        
        nav_panel(
          "Bill difficulty",
          
          card(
            card_header("Bill difficulty regression results"),
            p("This is the main regression model for the dissertation's energy-poverty outcome.")
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              card_header("Key findings"),
              div(
                class = "findings-list",
                tags$ul(
                  tags$li("Income is the strongest predictor of reported bill difficulty."),
                  tags$li("The lowest income group shows substantially higher odds compared with the €6,000+ reference group."),
                  tags$li("Tenure is also important, with local authority / housing association renters showing elevated odds."),
                  tags$li("Rural respondents show higher odds than urban respondents."),
                  tags$li("2024 and 2025 show lower odds of bill difficulty compared with 2023.")
                )
              )
            ),
            
            card(
              card_header("How to report this"),
              p("The model supports the descriptive dashboard patterns by showing that income and tenure remain strongly associated with bill difficulty after controlling for time, geography, age, and dwelling characteristics."),
              p("This result can be framed as evidence that energy-poverty risk is socially and structurally patterned, rather than evenly distributed across respondents.")
            )
          )
        ),
        
        nav_panel(
          "Bill worry",
          
          card(
            card_header("Bill worry regression results"),
            p("This is a supplementary regression model for financial stress related to energy bills."),
            div(
              class = "warning-note",
              strong("Caution: "),
              "Bill worry has higher missingness than bill difficulty, so this model should be interpreted as supplementary."
            )
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              card_header("Key findings"),
              div(
                class = "findings-list",
                tags$ul(
                  tags$li("Income and tenure remain important predictors of bill worry."),
                  tags$li("The income gradient is visible but less steep than in the bill difficulty model."),
                  tags$li("Rural respondents show higher odds of bill worry than urban respondents."),
                  tags$li("The regional pattern differs from the bill difficulty model."),
                  tags$li("The model should be discussed cautiously because of the higher missingness in bill worry.")
                )
              )
            ),
            
            card(
              card_header("How to report this"),
              p("The bill worry model suggests that financial stress overlaps with bill difficulty but is not identical to it."),
              p("This supports treating bill worry as a supplementary financial-stress measure rather than combining it directly with the main energy-poverty outcome.")
            )
          )
        ),
        
        nav_panel(
          "Heating behaviour",
          
          card(
            card_header("Heating behaviour regression results"),
            p("This behavioural model examines whether respondents heated their home on all seven days of the reference week.")
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              card_header("Key findings"),
              div(
                class = "findings-list",
                tags$ul(
                  tags$li("Heating every day is strongly seasonal in the full model, with much lower odds in spring, summer, and autumn compared with winter."),
                  tags$li("Lower-income groups have lower odds of heating seven days per week than the €6,000+ reference group."),
                  tags$li("Rural respondents have higher odds of heating every day than urban respondents."),
                  tags$li("Younger respondents, especially those aged 18–24, have lower odds than the 65+ reference group."),
                  tags$li("Apartment residents have lower odds than respondents in detached houses.")
                )
              )
            ),
            
            card(
              card_header("How to report this"),
              p("The heating model should be reported as a behaviour model that contextualises energy use. It should not be presented as a direct energy-poverty classification."),
              p("The result complements the bill difficulty model: lower-income respondents are more likely to report bill difficulty but less likely to report heating every day, suggesting that heating use may be constrained for some groups.")
            )
          )
        ),
        
        nav_panel(
          "Travel behaviour",
          
          card(
            card_header("Travel behaviour regression results"),
            p("This behavioural model examines higher short-distance driving tendency for journeys of up to 2 km.")
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              card_header("Key findings"),
              div(
                class = "findings-list",
                tags$ul(
                  tags$li("The model is reported as a travel behaviour model, not as a direct energy-poverty outcome."),
                  tags$li("The outcome distinguishes respondents with higher short-distance driving tendency from those with lower tendency."),
                  tags$li("The model controls for year, season, income group, tenure, urban/rural location, region, age group, and house type."),
                  tags$li("The results are useful for discussing how travel behaviour differs across household and geographic contexts.")
                )
              )
            ),
            
            card(
              card_header("How to report this"),
              p("The travel model should be presented as a supplementary behavioural analysis. It extends the dissertation beyond household energy stress by showing how short-distance car-use tendencies vary after controlling for demographic, household, and geographic factors."),
              p("The odds-ratio plot is best placed in the appendix, while the main report can briefly summarise the model alongside heating behaviour.")
            )
          )
        )
      )
    ),
    
    # --------------------------------------------------------
    # About / Methods tab
    # --------------------------------------------------------
    
    nav_panel(
      "About / Methods",
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Dataset scope"),
          tags$ul(
            tags$li("Dataset: SEAI Behavioural Energy and Travel Tracker."),
            tags$li("Analysis period: January 2023 to November 2025."),
            tags$li("December 2022 is excluded because it is the only 2022 observation."),
            tags$li("The dashboard uses a lightweight version of the cleaned dataset.")
          )
        ),
        
        card(
          card_header("Outcome variables"),
          tags$ul(
            tags$li("Bill difficulty is the main energy-poverty outcome."),
            tags$li("Bill worry is a supplementary financial-stress indicator."),
            tags$li("Heating 7 days is a heating-use behaviour indicator."),
            tags$li("Short-distance driving tendency is a travel behaviour indicator."),
            tags$li("Percentages are calculated among valid responses only.")
          )
        ),
        
        card(
          card_header("Dashboard variables"),
          tags$ul(
            tags$li("Time: year, season, and survey wave labels."),
            tags$li("Household and demographic context: income group, tenure, region, and urban/rural location."),
            tags$li("Regression models additionally use age group and house type.")
          )
        ),
        
        card(
          card_header("Limitations"),
          tags$ul(
            tags$li("Bill worry has higher missingness than bill difficulty."),
            tags$li("The dashboard is a prototype focused on energy poverty, financial stress, heating behaviour, and travel behaviour."),
            tags$li("Regression model outputs are displayed separately because odds ratios use a different scale from descriptive percentages.")
          )
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output) {
  
  # ----------------------------------------------------------
  # Global filtered data
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- copy(bett_dashboard)
    
    if (input$year_filter != "All") {
      data <- data[year == input$year_filter]
    }
    
    if (input$season_filter != "All") {
      data <- data[season == input$season_filter]
    }
    
    if (input$region_filter != "All") {
      data <- data[region == input$region_filter]
    }
    
    if (input$urbanrural_filter != "All") {
      data <- data[urbanrural == input$urbanrural_filter]
    }
    
    data
  })
  # ----------------------------------------------------------
  # KPI calculations
  # ----------------------------------------------------------
  
  difficulty_stats <- reactive({
    
    data <- filtered_data()
    total_n <- nrow(data)
    
    valid_data <- data[!is.na(bill_difficulty_binary)]
    valid_n <- nrow(valid_data)
    missing_n <- total_n - valid_n
    
    difficulty_pct <- if (valid_n > 0) {
      mean(valid_data$bill_difficulty_binary == "Difficulty") * 100
    } else {
      NA_real_
    }
    
    list(
      total_n = total_n,
      valid_n = valid_n,
      missing_n = missing_n,
      difficulty_pct = difficulty_pct
    )
  })
  
  worry_stats <- reactive({
    
    data <- filtered_data()
    total_n <- nrow(data)
    
    valid_data <- data[!is.na(bills_worry_binary)]
    valid_n <- nrow(valid_data)
    missing_n <- total_n - valid_n
    
    worry_pct <- if (valid_n > 0) {
      mean(valid_data$bills_worry_binary == "Worry") * 100
    } else {
      NA_real_
    }
    
    list(
      total_n = total_n,
      valid_n = valid_n,
      missing_n = missing_n,
      worry_pct = worry_pct
    )
  })
  
  heating_stats <- reactive({
    
    data <- filtered_data()
    total_n <- nrow(data)
    
    valid_data <- data[!is.na(heating_7days_binary)]
    valid_n <- nrow(valid_data)
    missing_n <- total_n - valid_n
    
    heating_pct <- if (valid_n > 0) {
      mean(valid_data$heating_7days_binary == "Heated 7 days") * 100
    } else {
      NA_real_
    }
    
    list(
      total_n = total_n,
      valid_n = valid_n,
      missing_n = missing_n,
      heating_pct = heating_pct
    )
  })
  
  travel_stats <- reactive({
    
    data <- filtered_data()
    total_n <- nrow(data)
    
    valid_data <- data[!is.na(drive2k_binary)]
    valid_n <- nrow(valid_data)
    missing_n <- total_n - valid_n
    
    travel_pct <- if (valid_n > 0) {
      mean(valid_data$drive2k_binary == "Higher short-driving tendency") * 100
    } else {
      NA_real_
    }
    
    list(
      total_n = total_n,
      valid_n = valid_n,
      missing_n = missing_n,
      travel_pct = travel_pct
    )
  })
  
  # ----------------------------------------------------------
  # Overview compact metric strip
  # ----------------------------------------------------------
  
  output$overview_metric_strip <- renderUI({
    
    d <- difficulty_stats()
    w <- worry_stats()
    data <- filtered_data()
    
    metric_strip(
      list(
        list(
          label = "Total observations",
          value = format_count(nrow(data)),
          subtext = "Based on selected global filters"
        ),
        list(
          label = "Bill difficulty",
          value = format_percent(d$difficulty_pct),
          subtext = paste0(format_count(d$valid_n), " valid responses")
        ),
        list(
          label = "Bill worry",
          value = format_percent(w$worry_pct),
          subtext = paste0(format_count(w$valid_n), " valid responses")
        ),
        list(
          label = "Heated 7 days",
          value = format_percent(heating_stats()$heating_pct),
          subtext = paste0(format_count(heating_stats()$valid_n), " valid responses")
        ),
        list(
          label = "Driving 2 km",
          value = format_percent(travel_stats()$travel_pct),
          subtext = paste0(format_count(travel_stats()$valid_n), " valid responses")
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # Outcome compact metric strips
  # ----------------------------------------------------------
  
  output$difficulty_metric_strip <- renderUI({
    
    d <- difficulty_stats()
    
    metric_strip(
      list(
        list(
          label = "Total observations",
          value = format_count(d$total_n),
          subtext = "Based on selected global filters"
        ),
        list(
          label = "Valid responses",
          value = format_count(d$valid_n),
          subtext = "Used in graph"
        ),
        list(
          label = "Reporting difficulty",
          value = format_percent(d$difficulty_pct),
          subtext = "Among valid responses"
        ),
        list(
          label = "Missing / excluded",
          value = format_count(d$missing_n),
          subtext = "Excluded from percentage"
        )
      )
    )
  })
  
  output$worry_metric_strip <- renderUI({
    
    w <- worry_stats()
    
    metric_strip(
      list(
        list(
          label = "Total observations",
          value = format_count(w$total_n),
          subtext = "Based on selected global filters"
        ),
        list(
          label = "Valid responses",
          value = format_count(w$valid_n),
          subtext = "Used in graph"
        ),
        list(
          label = "Reporting worry",
          value = format_percent(w$worry_pct),
          subtext = "Among valid responses"
        ),
        list(
          label = "Missing / excluded",
          value = format_count(w$missing_n),
          subtext = "Excluded from percentage"
        )
      )
    )
  })
  
  output$heating_metric_strip <- renderUI({
    
    h <- heating_stats()
    
    metric_strip(
      list(
        list(
          label = "Total observations",
          value = format_count(h$total_n),
          subtext = "Based on selected global filters"
        ),
        list(
          label = "Valid responses",
          value = format_count(h$valid_n),
          subtext = "Used in graph"
        ),
        list(
          label = "Heated 7 days",
          value = format_percent(h$heating_pct),
          subtext = "Among valid responses"
        ),
        list(
          label = "Missing / excluded",
          value = format_count(h$missing_n),
          subtext = "Excluded from percentage"
        )
      )
    )
  })
  
  output$travel_metric_strip <- renderUI({
    
    t <- travel_stats()
    
    metric_strip(
      list(
        list(
          label = "Total observations",
          value = format_count(t$total_n),
          subtext = "Based on selected global filters"
        ),
        list(
          label = "Valid responses",
          value = format_count(t$valid_n),
          subtext = "Used in graph"
        ),
        list(
          label = "Higher energy intensity",
          value = format_percent(t$travel_pct),
          subtext = "Among valid responses"
        ),
        list(
          label = "Missing / excluded",
          value = format_count(t$missing_n),
          subtext = "Excluded from percentage"
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # Compact selection summary
  # ----------------------------------------------------------
  
  output$selection_summary_compact <- renderUI({
    
    data <- filtered_data()
    d <- difficulty_stats()
    w <- worry_stats()
    
    years_text <- if (input$year_filter == "All") {
      "All years: 2023, 2024, 2025"
    } else {
      input$year_filter
    }
    
    seasons_text <- if (input$season_filter == "All") {
      "All seasons"
    } else {
      input$season_filter
    }
    
    regions_text <- if (input$region_filter == "All") {
      "All regions"
    } else {
      input$region_filter
    }
    
    urbanrural_text <- if (input$urbanrural_filter == "All") {
      "Urban and rural"
    } else {
      input$urbanrural_filter
    }
    
    div(
      class = "compact-selection",
      tags$p(
        tags$strong("Observations: "),
        format_count(nrow(data))
      ),
      tags$p(
        tags$strong("Time selection: "),
        years_text, " | ", seasons_text
      ),
      tags$p(
        tags$strong("Geographic selection: "),
        regions_text, " | ", urbanrural_text
      ),
      tags$p(
        tags$strong("Bill difficulty: "),
        format_percent(d$difficulty_pct),
        " of ",
        format_count(d$valid_n),
        " valid responses"
      ),
      tags$p(
        tags$strong("Bill worry: "),
        format_percent(w$worry_pct),
        " of ",
        format_count(w$valid_n),
        " valid responses"
      ),
      tags$p(
        tags$strong("Heated 7 days: "),
        format_percent(heating_stats()$heating_pct),
        " of ",
        format_count(heating_stats()$valid_n),
        " valid responses"
      ),
      tags$p(
        tags$strong("Higher energy-intensity perception: "),
        format_percent(travel_stats()$travel_pct),
        " of ",
        format_count(travel_stats()$valid_n),
        " valid responses"
      )
    )
  })
  
  # ----------------------------------------------------------
  # Dynamic bill difficulty plot
  # ----------------------------------------------------------
  
  output$bill_difficulty_dynamic_plot <- renderPlot({
    
    data <- filtered_data()
    compare_var <- input$difficulty_compare
    
    plot_title <- paste(
      "Reported difficulty paying energy bills by",
      comparison_labels[[compare_var]]
    )
    
    make_percentage_plot(
      data = data,
      compare_var = compare_var,
      outcome_var = "bill_difficulty_binary",
      positive_level = "Difficulty",
      plot_title = plot_title,
      plot_subtitle = "Percentage of valid responses reporting difficulty",
      y_label = "Respondents reporting difficulty (%)",
      fill_colour = "#007C89"
    )
  })
  
  # ----------------------------------------------------------
  # Dynamic bill worry plot
  # ----------------------------------------------------------
  
  output$bill_worry_dynamic_plot <- renderPlot({
    
    data <- filtered_data()
    compare_var <- input$worry_compare
    
    plot_title <- paste(
      "Worry about energy bills by",
      comparison_labels[[compare_var]]
    )
    
    make_percentage_plot(
      data = data,
      compare_var = compare_var,
      outcome_var = "bills_worry_binary",
      positive_level = "Worry",
      plot_title = plot_title,
      plot_subtitle = "Percentage of valid responses reporting worry",
      y_label = "Respondents reporting worry (%)",
      fill_colour = "#56B4E9"
    )
  })
  
  # ----------------------------------------------------------
  # Dynamic heating behaviour plot
  # ----------------------------------------------------------
  
  output$heating_dynamic_plot <- renderPlot({
    
    data <- filtered_data()
    compare_var <- input$heating_compare
    
    plot_title <- paste(
      "Heating every day by",
      comparison_labels[[compare_var]]
    )
    
    make_percentage_plot(
      data = data,
      compare_var = compare_var,
      outcome_var = "heating_7days_binary",
      positive_level = "Heated 7 days",
      plot_title = plot_title,
      plot_subtitle = "Percentage of valid responses heating seven days per week",
      y_label = "Respondents heating seven days (%)",
      fill_colour = "#E69F00"
    )
  })
  
  # ----------------------------------------------------------
  # Dynamic travel behaviour plot
  # ----------------------------------------------------------
  
  output$travel_dynamic_plot <- renderPlot({
    
    data <- filtered_data()
    compare_var <- input$travel_compare
    
    plot_title <- paste(
      "Perceived energy intensity of driving 2 km by",
      comparison_labels[[compare_var]]
    )
    
    make_percentage_plot(
      data = data,
      compare_var = compare_var,
      outcome_var = "drive2k_binary",
      positive_level = "Higher short-driving tendency",
      plot_title = plot_title,
      plot_subtitle = "Percentage of valid responses with higher perceived energy intensity",
      y_label = "Higher perceived energy intensity (%)",
      fill_colour = "#009E73"
    )
  })
  
  # ----------------------------------------------------------
  # Model result images
  # ----------------------------------------------------------
  
  output$difficulty_model_plot <- renderImage({
    
    validate(
      need(
        file.exists("../outputs/graphs/odds_ratio_plot_bill_difficulty.png"),
        "Bill difficulty odds-ratio plot not found."
      )
    )
    
    list(
      src = normalizePath("../outputs/graphs/odds_ratio_plot_bill_difficulty.png"),
      contentType = "image/png",
      width = "100%"
    )
    
  }, deleteFile = FALSE)
  
  output$worry_model_plot <- renderImage({
    
    validate(
      need(
        file.exists("../outputs/graphs/odds_ratio_plot_bill_worry.png"),
        "Bill worry odds-ratio plot not found."
      )
    )
    
    list(
      src = normalizePath("../outputs/graphs/odds_ratio_plot_bill_worry.png"),
      contentType = "image/png",
      width = "100%"
    )
    
  }, deleteFile = FALSE)
  
  output$heating_model_plot <- renderImage({
    
    heating_plot_path <- if (file.exists("../outputs/graphs/odds_ratio_plot_heating_7days_no_season.png")) {
      "../outputs/graphs/odds_ratio_plot_heating_7days_no_season.png"
    } else {
      "../outputs/graphs/odds_ratio_plot_heating_7days.png"
    }
    
    validate(
      need(
        file.exists(heating_plot_path),
        "Heating odds-ratio plot not found."
      )
    )
    
    list(
      src = normalizePath(heating_plot_path),
      contentType = "image/png",
      width = "100%"
    )
    
  }, deleteFile = FALSE)
  
  output$travel_model_plot <- renderImage({
    
    travel_plot_path <- if (file.exists("../outputs/graphs/odds_ratio_plot_drive2k_clean.png")) {
      "../outputs/graphs/odds_ratio_plot_drive2k_clean.png"
    } else {
      "../outputs/graphs/odds_ratio_plot_drive2k.png"
    }
    
    validate(
      need(
        file.exists(travel_plot_path),
        "Travel odds-ratio plot not found."
      )
    )
    
    list(
      src = normalizePath(travel_plot_path),
      contentType = "image/png",
      width = "100%"
    )
    
  }, deleteFile = FALSE)
}

# ============================================================
# Run app
# ============================================================

shinyApp(ui = ui, server = server)