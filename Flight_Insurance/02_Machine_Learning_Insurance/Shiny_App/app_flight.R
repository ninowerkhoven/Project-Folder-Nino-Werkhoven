# ══════════════════════════════════════════════════════════════
# FLIGHT DELAY & CANCELLATION RISK PREDICTOR
# Beautiful Modern UI - Integrated with Actual Trained Models
# ══════════════════════════════════════════════════════════════

library(shiny)
library(shinydashboard)
library(xgboost)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(fresh)
library(arrow)

# ══════════════════════════════════════════════════════════════
# CUSTOM THEME
# ══════════════════════════════════════════════════════════════

my_theme <- create_theme(
  adminlte_color(
    light_blue = "#1976d2",
    blue = "#1976d2",
    navy = "#1565c0",
    red = "#dc3545",
    yellow = "#ffc107",
    green = "#28a745"
  ),
  adminlte_sidebar(
    dark_bg = "#1e3a5f",
    dark_hover_bg = "#2c4f7c",
    dark_color = "#ffffff"
  ),
  adminlte_global(
    content_bg = "#f5f7fa",
    box_bg = "#ffffff",
    info_box_bg = "#ffffff"
  )
)

# Insurance Advisor Style CSS
insurance_css <- tags$style(HTML("
  /* Sky background with airplane wing photo */
  .content-wrapper {
    background: linear-gradient(180deg, #4A90E2 0%, #7FB3E8 30%, #B5D5F0 60%, #E8F2F8 100%) !important;
    background-attachment: fixed !important;
  }
  
  /* Header banner with airplane image */
  .flight-header {
    background-image: url('plane_image.jpg');
    background-size: cover;
    background-position: center;
    background-repeat: no-repeat;
    border-radius: 20px;
    padding: 60px 40px;
    margin-bottom: 25px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15);
    border: 1px solid rgba(255, 255, 255, 0.3);
    position: relative;
    z-index: 2;
    overflow: hidden;
  }
  
  /* Dark overlay for better text readability */
  .flight-header::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: linear-gradient(90deg, rgba(0, 0, 0, 0.4) 0%, rgba(0, 0, 0, 0.1) 100%);
    z-index: 1;
  }
  
  .flight-header-title {
    color: white;
    font-size: 32px;
    font-weight: 700;
    margin: 0;
    text-shadow: 2px 2px 8px rgba(0, 0, 0, 0.5);
    position: relative;
    z-index: 2;
  }
  
  /* Clean white cards */
  .box {
    background: white !important;
    border-radius: 20px !important;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.08) !important;
    border: none !important;
    position: relative !important;
    z-index: 2 !important;
  }
  
  .box-header {
    background: white !important;
    border-bottom: 2px solid #f0f4f8 !important;
    border-radius: 20px 20px 0 0 !important;
    padding: 20px 25px !important;
  }
  
  .box-title {
    font-weight: 700 !important;
    font-size: 18px !important;
    color: #1e3a5f !important;
  }
  
  .box-body {
    padding: 25px !important;
    position: relative;
    z-index: 2;
  }
  
  /* Insurance advisor buttons - consistent blue */
  .btn-primary {
    background: #1976d2 !important;
    border: none !important;
    border-radius: 12px !important;
    padding: 12px 28px !important;
    font-weight: 600 !important;
    color: white !important;
    box-shadow: 0 4px 12px rgba(25, 118, 210, 0.3) !important;
    transition: all 0.2s ease !important;
  }
  
  .btn-primary:hover {
    background: #1565c0 !important;
    transform: translateY(-2px) !important;
    box-shadow: 0 6px 16px rgba(25, 118, 210, 0.4) !important;
  }
  
  /* Risk status cards - insurance advisor colors */
  .risk-low {
    background: #28a745 !important;
    color: white !important;
  }
  
  .risk-moderate {
    background: #ffc107 !important;
    color: #1e3a5f !important;
  }
  
  .risk-high {
    background: #dc3545 !important;
    color: white !important;
  }
  
  /* Value boxes - clean white with colored icons */
  .small-box {
    background: white !important;
    border-radius: 16px !important;
    box-shadow: 0 6px 20px rgba(0, 0, 0, 0.08) !important;
    position: relative !important;
    z-index: 2 !important;
  }
  
  .small-box h3 {
    font-weight: 700 !important;
    font-size: 36px !important;
    color: #1976d2 !important;
  }
  
  .small-box p {
    color: #6c757d !important;
    font-weight: 600 !important;
  }
  
  /* Progress bars - insurance advisor style */
  .progress-bar-container {
    background: #e9ecef;
    border-radius: 12px;
    height: 32px;
    margin: 12px 0;
    overflow: hidden;
    box-shadow: inset 0 2px 4px rgba(0,0,0,0.06);
  }
  
  .progress-bar-fill {
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
    color: white;
    font-weight: 700;
    font-size: 14px;
    transition: width 1s ease;
    border-radius: 12px;
  }
  
  /* Section titles */
  .section-title {
    font-size: 18px;
    font-weight: 700;
    color: #1e3a5f;
    margin: 25px 0 15px 0;
  }
  
  .section-title i {
    color: #1976d2;
    margin-right: 8px;
  }
  
  /* Info boxes - blue accent */
  .info-box {
    background: white !important;
    border-left: 4px solid #1976d2 !important;
  }
  
  /* Input fields - clean modern style */
  .form-control, .selectize-input {
    border-radius: 10px !important;
    border: 2px solid #e1e8ed !important;
    padding: 10px 14px !important;
  }
  
  .form-control:focus, .selectize-input.focus {
    border-color: #1976d2 !important;
    box-shadow: 0 0 0 3px rgba(25, 118, 210, 0.1) !important;
  }
  
  /* Sidebar - darker blue */
  .main-sidebar {
    background: #1e3a5f !important;
  }
  
  .sidebar-menu > li.active > a {
    background: #1976d2 !important;
  }
  
  /* Tab styling */
  .nav-tabs-custom {
    background: white;
    border-radius: 20px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.08);
    margin-bottom: 20px;
  }
  
  .nav-tabs-custom > .nav-tabs {
    border-bottom: 2px solid #e1e8ed;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a {
    color: #6c757d;
    font-weight: 600;
    border: none;
    border-radius: 0;
    padding: 15px 20px;
  }
  
  .nav-tabs-custom > .nav-tabs > li.active > a {
    color: #1976d2;
    background: white;
    border: none;
    border-bottom: 3px solid #1976d2;
  }
  
  .nav-tabs-custom > .nav-tabs > li > a:hover {
    background: #f8f9fa;
    border: none;
  }
  
  .nav-tabs-custom > .tab-content {
    padding: 25px;
  }
"))

# ══════════════════════════════════════════════════════════════
# LOAD HISTORICAL STATISTICS
# ══════════════════════════════════════════════════════════════

historical_stats_loaded <- FALSE

if(file.exists("historical_flight_stats.parquet")) {
  tryCatch({
    historical_stats <- arrow::read_parquet("historical_flight_stats.parquet")
    historical_stats_loaded <- TRUE
    cat("✓ Historical statistics loaded successfully!\n")
    cat("  - Patterns:", nrow(historical_stats), "\n")
    cat("  - Airports:", n_distinct(historical_stats$Origin), "\n")
  }, error = function(e) {
    cat("ℹ Could not load historical statistics - app will use default view\n")
    historical_stats_loaded <- FALSE
  })
} else if(file.exists("historical_stats.rds")) {
  tryCatch({
    historical_stats <- readRDS("historical_stats.rds")
    historical_stats_loaded <- TRUE
    cat("✓ Historical statistics loaded successfully!\n")
  }, error = function(e) {
    historical_stats_loaded <- FALSE
  })
} else {
  cat("ℹ Historical statistics not available - app will use default view\n")
}

# ══════════════════════════════════════════════════════════════
# LOAD DELAY TYPE & AIRLINE STATISTICS
# ══════════════════════════════════════════════════════════════

delay_types_loaded <- FALSE
if(file.exists("historical_delay_types.parquet")) {
  tryCatch({
    delay_types <- arrow::read_parquet("historical_delay_types.parquet")
    delay_types_loaded <- TRUE
    cat("✓ Delay type statistics loaded!\n")
  }, error = function(e) {
    cat("ℹ Could not load delay type statistics\n")
  })
} else if(file.exists("delay_type_stats.rds")) {
  tryCatch({
    delay_types <- readRDS("delay_type_stats.rds")
    delay_types_loaded <- TRUE
    cat("✓ Delay type statistics loaded!\n")
  }, error = function(e) {
    cat("ℹ Could not load delay type statistics\n")
  })
} else {
  cat("ℹ Delay type statistics not available\n")
}

airlines_loaded <- FALSE
if(file.exists("historical_airline_routes.parquet")) {
  tryCatch({
    airline_routes <- arrow::read_parquet("historical_airline_routes.parquet")
    airlines_loaded <- TRUE
    cat("✓ Airline statistics loaded!\n")
  }, error = function(e) {
    cat("ℹ Could not load airline statistics\n")
  })
} else if(file.exists("airline_stats.rds")) {
  tryCatch({
    airline_routes <- readRDS("airline_stats.rds")
    airlines_loaded <- TRUE
    cat("✓ Airline statistics loaded!\n")
  }, error = function(e) {
    cat("ℹ Could not load airline statistics\n")
  })
} else {
  cat("ℹ Airline statistics not available\n")
}


# ══════════════════════════════════════════════════════════════
# HISTORICAL LOOKUP FUNCTION
# ══════════════════════════════════════════════════════════════

get_historical_pattern <- function(origin, month, dep_time) {
  
  if (!historical_stats_loaded) {
    # Fallback if no historical data
    return(list(
      found = FALSE,
      message = "Historical data not available. Run generate_historical_stats.R"
    ))
  }



  
  # Determine time of day category
  time_category <- case_when(
    dep_time < 800 ~ "Early Morning (6-8 AM)",
    dep_time < 1200 ~ "Morning (8 AM-12 PM)",
    dep_time < 1500 ~ "Midday (12-3 PM)",
    dep_time < 1800 ~ "Afternoon (3-6 PM)",
    TRUE ~ "Evening (6+ PM)"
  )
  
  # Look up historical pattern
  pattern <- historical_stats %>%
    filter(Origin == origin, Month == month, TimeOfDay == time_category)
  
  if (nrow(pattern) == 0) {
    # Try without time of day filter (less specific)
    pattern <- historical_stats %>%
      filter(Origin == origin, Month == month) %>%
      summarise(
        total_flights = sum(total_flights),
        pct_delayed_15 = mean(pct_delayed_15, na.rm = TRUE),
        pct_delayed_30 = mean(pct_delayed_30, na.rm = TRUE),
        pct_delayed_45 = mean(pct_delayed_45, na.rm = TRUE),
        pct_delayed_60 = mean(pct_delayed_60, na.rm = TRUE),
        pct_cancelled = mean(pct_cancelled, na.rm = TRUE),
        avg_delay_all = mean(avg_delay_all, na.rm = TRUE),
        avg_delay_when_delayed = mean(avg_delay_when_delayed, na.rm = TRUE),
        pct_ontime = mean(pct_ontime, na.rm = TRUE),
        pct_minor_delay = mean(pct_minor_delay, na.rm = TRUE),
        pct_moderate_delay = mean(pct_moderate_delay, na.rm = TRUE),
        pct_major_delay = mean(pct_major_delay, na.rm = TRUE),
        avg_visibility = mean(avg_visibility, na.rm = TRUE),
        avg_temperature = mean(avg_temperature, na.rm = TRUE)
      )
    
    time_specific = FALSE
  } else {
    time_specific = TRUE
  }
  
  if (nrow(pattern) == 0) {
    return(list(
      found = FALSE,
      message = paste("No historical data for", origin, "in", month.name[month])
    ))
  }
  
  # Return the pattern
  list(
    found = TRUE,
    time_specific = time_specific,
    origin = origin,
    month = month.name[month],
    time_category = time_category,
    sample_size = pattern$total_flights[1],
    
    # Delay rates (convert to 0-1 scale for consistency)
    delay_15 = pattern$pct_delayed_15[1] / 100,
    delay_30 = pattern$pct_delayed_30[1] / 100,
    delay_45 = pattern$pct_delayed_45[1] / 100,
    delay_60 = pattern$pct_delayed_60[1] / 100,
    cancel = pattern$pct_cancelled[1] / 100,
    
    # Distribution
    pct_ontime = pattern$pct_ontime[1],
    pct_minor = pattern$pct_minor_delay[1],
    pct_moderate = pattern$pct_moderate_delay[1],
    pct_major = pattern$pct_major_delay[1],
    
    # Averages
    avg_delay_all = pattern$avg_delay_all[1],
    avg_delay_when_delayed = pattern$avg_delay_when_delayed[1],
    
    # Weather context
    avg_visibility = pattern$avg_visibility[1],
    avg_temperature = pattern$avg_temperature[1],
    
    method = if(time_specific) {
      paste0("Historical Pattern: ", origin, " - ", month.name[month], " - ", time_category)
    }
 else {
      paste0("Historical Pattern: ", origin, " - ", month.name[month], " (all times)")
    }
  )
}


# ══════════════════════════════════════════════════════════════
# DELAY TYPE & AIRLINE LOOKUP FUNCTIONS
# ══════════════════════════════════════════════════════════════

get_delay_type_breakdown <- function(origin, month) {
  if (!delay_types_loaded) return(list(found = FALSE))
  pattern <- delay_types %>% filter(Origin == origin, Month == month)
  if (nrow(pattern) == 0) return(list(found = FALSE))
  list(
    found = TRUE, origin = origin, month = month.name[month],
    sample_size = pattern$delay_type_sample_size[1],
    carrier_pct = pattern$carrier_pct[1],
    weather_pct = pattern$weather_pct[1],
    nas_pct = pattern$nas_pct[1],
    security_pct = pattern$security_pct[1],
    late_aircraft_pct = pattern$late_aircraft_pct[1]
  )
}

get_airline_comparison <- function(origin, dest, month) {
  if (!airlines_loaded) return(list(found = FALSE))
  airlines <- airline_routes %>%
    filter(Origin == origin, Dest == dest) %>%  # Removed Month filter - now annual
    arrange(avg_delay)
  if (nrow(airlines) == 0) return(list(found = FALSE))
  cheapest_idx <- which.min(airlines$avg_fare)
  list(
    found = TRUE, origin = origin, dest = dest,
    airlines = airlines %>%
      mutate(
        is_best = row_number() == 1,
        is_worst = row_number() == n(),
        is_cheapest = row_number() == cheapest_idx
      ) %>%
      select(airline = Reporting_Airline, flights, avg_delay, avg_fare,
             ontime_rate, cancel_rate, is_best, is_worst, is_cheapest, rank = rank_by_delay)
  )
}
# ══════════════════════════════════════════════════════════════
# UI DEFINITION
# ══════════════════════════════════════════════════════════════

ui <- fluidPage(
  use_theme(my_theme),
  insurance_css,
  
  # Header
  div(
    style = "background: white;
             padding: 25px 30px; 
             margin-bottom: 30px; 
             border-radius: 12px;
             box-shadow: 0 2px 12px rgba(0,0,0,0.08);",
    h1(style = "color: #1976d2;
                margin: 0; 
                font-weight: 700; 
                font-size: 32px;",
      icon("plane"), " Flight Insurance Advisor"
    )
  ),
  
  div(style = "max-width: 1600px; margin: 0 auto; padding: 0 20px;",
    
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        
        * { font-family: 'Inter', sans-serif !important; }
        
        body {
          background: url('sky_wallpaper.jpg') no-repeat center center fixed;
          background-size: cover;
        }
        
        body::before {
          content: '';
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(255, 255, 255, 0.3);
          z-index: -1;
        }
        
        /* Hide icon loading placeholders */
        .fa:before {
          display: inline-block !important;
        }
        
        /* Make tabs opaque */
        .nav-tabs {
          background: white !important;
          padding: 10px 10px 10px 10px !important;
          border-radius: 12px 12px 0 0 !important;
          margin-top: 10px !important;
        }
        
        .nav-tabs > li > a {
          background: rgba(255, 255, 255, 0.9) !important;
          border: 1px solid #ddd !important;
          color: #1976d2 !important;
        }
        
        .nav-tabs > li.active > a {
          background: white !important;
          border-bottom-color: white !important;
          color: #1976d2 !important;
        }
        
        .content-wrapper, .main-sidebar, .main-header {
          transition: all 0.3s ease;
        }
        
        /* Remove extra spacing/gaps */
        .box {
          margin-bottom: 20px !important;
        }
        
        .box-body {
          padding: 15px !important;
        }
        
        /* Fix checkbox/label spacing */
        .checkbox, .form-group {
          margin-bottom: 15px !important;
        }
        
        .box {
          background: white !important;
          border-radius: 12px !important;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;
          border: none !important;
          margin-bottom: 20px;
        }
        
        .box-header {
          border-radius: 12px 12px 0 0 !important;
          min-height: 60px !important;
          padding: 15px !important;
        }
        
        .box-header .box-title {
          line-height: 1.4 !important;
          padding: 5px 0 !important;
        }
        
        .risk-card {
          padding: 30px;
          border-radius: 16px;
          text-align: center;
          margin: 15px 0;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
          transition: transform 0.2s, box-shadow 0.2s;
        }
        
        .risk-card:hover {
          transform: translateY(-4px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.15);
        }
        
        .risk-high {
          background: linear-gradient(135deg, #dc3545 0%, #b71c1c 100%);
          color: white;
        }
        
        .risk-moderate {
          background: linear-gradient(135deg, #ffc107 0%, #d68910 100%);
          color: white;
        }
        
        .risk-low {
          background: linear-gradient(135deg, #28a745 0%, #229954 100%);
          color: white;
        }
        
        .risk-title {
          font-size: 28px;
          font-weight: 700;
          margin-bottom: 10px;
          text-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .risk-subtitle {
          font-size: 16px;
          opacity: 0.95;
          font-weight: 500;
        }
        
        .metric-card {
          background: white;
          padding: 25px;
          border-radius: 12px;
          text-align: center;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          transition: all 0.2s;
          border-left: 4px solid #1976d2;
        }
        
        .metric-card:hover {
          box-shadow: 0 4px 16px rgba(0,0,0,0.12);
          transform: translateY(-2px);
        }
        
        .metric-value {
          font-size: 48px;
          font-weight: 700;
          background: linear-gradient(135deg, #1976d2 0%, #1565c0 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          margin: 10px 0;
        }
        
        .metric-label {
          font-size: 13px;
          color: #7f8c8d;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        .info-badge {
          display: inline-block;
          padding: 6px 14px;
          border-radius: 20px;
          font-size: 12px;
          font-weight: 600;
          margin: 5px;
        }
        
        .badge-weather { background: #e8f4f8; color: #1565c0; }
        .badge-time { background: #fef5e7; color: #d68910; }
        .badge-operational { background: #eafaf1; color: #229954; }
        
        .btn-predict {
          background: linear-gradient(135deg, #1976d2 0%, #1565c0 100%) !important;
          border: none !important;
          font-size: 16px !important;
          font-weight: 600 !important;
          padding: 14px 28px !important;
          border-radius: 8px !important;
          box-shadow: 0 4px 12px rgba(52, 152, 219, 0.3) !important;
          transition: all 0.2s !important;
        }
        
        .btn-predict:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 6px 20px rgba(52, 152, 219, 0.4) !important;
        }
        
        .input-group {
          margin-bottom: 20px;
        }
        
        .input-label {
          font-size: 13px;
          font-weight: 600;
          color: #34495e;
          margin-bottom: 6px;
          display: block;
        }
        
        .form-control, .selectize-input {
          border-radius: 8px !important;
          border: 2px solid #ecf0f1 !important;
          padding: 10px 14px !important;
          transition: border-color 0.2s !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #1976d2 !important;
          box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1) !important;
        }
        
        .progress-bar-container {
          background: #ecf0f1;
          border-radius: 10px;
          height: 24px;
          overflow: hidden;
          margin: 15px 0;
        }
        
        .progress-bar-fill {
          height: 100%;
          border-radius: 10px;
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-weight: 600;
          font-size: 13px;
          transition: width 0.6s ease;
        }
        
        .section-title {
          font-size: 20px;
          font-weight: 700;
          color: #2c3e50;
          margin: 25px 0 15px 0;
          display: flex;
          align-items: center;
        }
        
        .section-title i {
          margin-right: 10px;
          color: #1976d2;
        }
        
        .feature-card {
          background: #f8f9fa;
          padding: 15px 20px;
          border-radius: 10px;
          margin: 10px 0;
          border-left: 4px solid #1976d2;
        }
        
        .feature-card-title {
          font-weight: 600;
          color: #2c3e50;
          margin-bottom: 8px;
        }
        
        .feature-card-text {
          color: #7f8c8d;
          font-size: 14px;
          line-height: 1.6;
        }
        
        .stat-row {
          display: flex;
          justify-content: space-between;
          padding: 12px 0;
          border-bottom: 1px solid #ecf0f1;
        }
        
        .stat-row:last-child {
          border-bottom: none;
        }
        
        .stat-label {
          font-weight: 600;
          color: #34495e;
        }
        
        .stat-value {
          color: #1976d2;
          font-weight: 600;
        }
      "))
    ),
    
    # Main content area
    fluidRow(
      # INPUT PANEL - LEFT SIDE
      column(4,
        box(
          title = tags$div(
            icon("plane-departure"),
            " Flight Information",
            style = "font-size: 18px; font-weight: 600;"
          ),
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          
          tags$label(class = "input-label", "Flight Number"),
          textInput("flight_number", NULL, "AA 1234", placeholder = "e.g., AA 1234"),
          
          tags$label(class = "input-label", icon("plane-departure"), " Origin"),
          selectInput("origin", NULL, choices = NULL, selected = "JFK"),
          
          tags$label(class = "input-label", icon("plane-arrival"), " Destination"),
          selectInput("dest", NULL, choices = NULL, selected = "LAX"),
          
          fluidRow(
            column(6,
              tags$label(class = "input-label", icon("calendar"), " Date"),
              dateInput("date", NULL, value = Sys.Date() + 7, min = Sys.Date())
            ),
            column(6,
              tags$label(class = "input-label", icon("clock"), " Time"),
              selectInput("time", NULL,
                choices = c(
                  "05:00" = 500, "06:00" = 600, "07:00" = 700, "08:00" = 800,
                  "09:00" = 900, "10:00" = 1000, "11:00" = 1100,
                  "12:00" = 1200, "13:00" = 1300, "14:00" = 1400,
                  "15:00" = 1500, "16:00" = 1600, "17:00" = 1700,
                  "18:00" = 1800, "19:00" = 1900, "20:00" = 2000,
                  "21:00" = 2100, "22:00" = 2200, "23:00" = 2300
                ),
                selected = 1800
              )
            )
          ),
          
          tags$label(class = "input-label", icon("building"), " Airline"),
          selectInput("airline", NULL,
            choices = c(
              "Southwest Airlines" = "WN",
              "Delta Air Lines" = "DL",
              "American Airlines" = "AA",
              "SkyWest Airlines" = "OO",
              "United Airlines" = "UA",
              "ExpressJet" = "EV",
              "Envoy Air" = "MQ",
              "US Airways" = "US",
              "JetBlue Airways" = "B6",
              "Alaska Airlines" = "AS",
              "AirTran Airways" = "FL",
              "Mesa Airlines" = "YV",
              "Endeavor Air" = "9E",
              "PSA Airlines" = "XE",
              "Frontier Airlines" = "F9",
              "Hawaiian Airlines" = "HA",
              "Spirit Airlines" = "NK",
              "Continental Airlines" = "CO",
              "PSA Airlines (OH)" = "OH",
              "Virgin America" = "VX",
              "Republic Airways" = "YX",
              "Allegiant Air" = "G4"
            ),
            selected = "AA"
          ),
          
          tags$label(class = "input-label", icon("route"), " Distance (miles)"),
          sliderInput("distance", NULL,
            min = 200, max = 5000, value = 2475, step = 50, post = " mi"
          ),
          
          hr(style = "margin: 20px 0;"),
          
          actionButton("predict_btn", 
            tagList(icon("calculator"), " Predict Flight Risk"),
            class = "btn-predict btn-block"
          )
        )
      ),
      
      # RESULTS AREA - RIGHT SIDE
      column(8,
            
            tabsetPanel(
              id = "results_tabs",
              type = "tabs",
              
              # ========== OVERVIEW TAB ==========
              tabPanel(
                title = tagList(icon("home"), " Overview"),
                value = "overview",
                
                br(),
                
                box(
                  title = tags$div(
                    icon("chart-bar"),
                    " Delay Risk Distribution",
                    style = "font-size: 18px; font-weight: 600;"
                  ),
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  
                  plotlyOutput("delay_distribution", height = "280px")
                ),
                
                # Risk banner below chart
                uiOutput("risk_summary_card")
              ),
              
              # ========== DELAY DETAILS TAB ==========
              tabPanel(
                title = tagList(icon("clock"), " Delay Details"),
                value = "delay_details",
                
                br(),
                
                box(
                  title = tags$div(
                    icon("history"),
                    " Historical Context & Analysis",
                    style = "font-size: 18px; font-weight: 600;"
                  ),
                  status = "warning",
                  solidHeader = TRUE,
                  width = NULL,
                  
                  uiOutput("historical_context_ui"),
                  
                  hr(),
                  
                  uiOutput("delay_type_ui"),
                  
                  hr(),
                  
                  uiOutput("weather_info_ui"),
                  
                  hr(),
                  
                  uiOutput("interpretation_ui")
                )
              ),
              
              # ========== AIRLINE COMPARISON TAB ==========
              tabPanel(
                title = tagList(icon("plane"), " Airline Comparison"),
                value = "airline_comparison",
                
                br(),
                
                box(
                  title = tags$div(
                    icon("building"),
                    " Compare Airlines on This Route",
                    style = "font-size: 18px; font-weight: 600;"
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  
                  uiOutput("airline_comparison_ui")
                )
              ),
              
              # ========== CANCELLATION INSURANCE TAB ==========
              tabPanel(
                title = tagList(icon("shield-alt"), " Cancellation Insurance"),
                value = "cancel_insurance",
                
                br(),
                
                box(
                  title = tags$div(
                    icon("ban"),
                    " Should You Buy Cancellation Insurance?",
                    style = "font-size: 18px; font-weight: 600;"
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = NULL,
                  
                  fluidRow(
                    column(6,
                      div(style = "margin-bottom: 20px;",
                        numericInput("cancel_ticket_price",
                                    "Ticket Price ($):",
                                    value = 400,
                                    min = 50,
                                    max = 5000,
                                    step = 50,
                                    width = "100%")
                      )
                    )
                  ),
                  
                  uiOutput("cancel_insurance_ui")
                )
              ),
              
              # ========== CONNECTION INSURANCE TAB ==========
              tabPanel(
                title = tagList(icon("plane-arrival"), " Connection Insurance"),
                value = "connection_insurance",
                
                br(),
                
                box(
                  title = tags$div(
                    icon("clock"),
                    " Should You Buy Connection Insurance?",
                    style = "font-size: 18px; font-weight: 600;"
                  ),
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  
                  # Inputs in a row
                  fluidRow(
                    column(6,
                      div(style = "margin-bottom: 20px;",
                        sliderInput("connection_time",
                                   "Your Connection Time:",
                                   min = 60,
                                   max = 300,
                                   value = 120,
                                   step = 15,
                                   post = " minutes",
                                   width = "100%")
                      )
                    ),
                    column(6,
                      div(style = "margin-bottom: 20px;",
                        numericInput("ticket_price",
                                    "Ticket Price ($):",
                                    value = 400,
                                    min = 50,
                                    max = 5000,
                                    step = 50,
                                    width = "100%")
                      )
                    )
                  ),
                  
                  uiOutput("connection_insurance_ui")
                
              )
            )
          )
        )
      )
    )
  )


# ══════════════════════════════════════════════════════════════
# SERVER LOGIC
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ══════════════════════════════════════════════════════════════
  # HELPER FUNCTION: Convert SHAP values to grouped percentages
  # ══════════════════════════════════════════════════════════════
  
  get_grouped_shap_percentages <- function(shap_contrib, feature_names) {
    # SHAP contrib is a matrix with feature contributions + BIAS column
    # Last column is BIAS, we exclude it
    n_features <- length(feature_names)
    contributions <- abs(shap_contrib[1, 1:n_features])
    
    # Create named vector
    names(contributions) <- feature_names
    
    # Define feature groups
    weather_features <- c("origin_TEMP", "origin_DEWP", "origin_VISIB", "origin_WDSP", 
                         "origin_MXSPD", "origin_MAX", "origin_MIN", "origin_PRCP", "origin_SNDP",
                         "dest_TEMP", "dest_DEWP", "dest_VISIB", "dest_WDSP", 
                         "dest_MXSPD", "dest_MAX", "dest_MIN", "dest_PRCP", "dest_SNDP")
    
    route_features <- c("Origin", "Dest", "Distance", "OriginWac", "DestWac")
    
    time_features <- c("CRSDepTime", "Month", "Quarter", "DayOfWeek", "DayofMonth")
    
    airline_features <- c("Reporting_Airline")
    
    other_features <- c("CRSElapsedTime", "avg_fare")
    
    # Sum contributions by group
    groups <- list(
      "Time & Schedule" = sum(contributions[names(contributions) %in% time_features]),
      "Route & Airports" = sum(contributions[names(contributions) %in% route_features]),
      "Airline" = sum(contributions[names(contributions) %in% airline_features]),
      "Weather Conditions" = sum(contributions[names(contributions) %in% weather_features]),
      "Flight Details" = sum(contributions[names(contributions) %in% other_features])
    )
    
    # Convert to percentages
    total <- sum(unlist(groups))
    percentages <- sapply(groups, function(x) (x / total) * 100)
    
    # Sort by percentage
    sorted_idx <- order(percentages, decreasing = TRUE)
    
    return(list(
      groups = names(percentages)[sorted_idx],
      percentages = as.numeric(percentages)[sorted_idx],
      raw_values = unlist(groups)[sorted_idx]
    ))
  }
  
  # ══════════════════════════════════════════════════════════════
  # LOAD MODELS & THRESHOLDS
  # ══════════════════════════════════════════════════════════════
  
  cancel_model_loaded <- FALSE
  cancel_model <- NULL
  cancel_thresholds <- NULL
  monthly_weather_origin <- NULL
  monthly_weather_dest <- NULL
  historical_stats <- NULL
  
  delay_model_loaded <- FALSE
  delay_model <- NULL
  delay_thresholds <- NULL
  
  tryCatch({
    cancel_model <- readRDS("model_cancel_booking.rds")
    cancel_thresholds <- readRDS("cancel_insurance_thresholds.rds")
    monthly_weather_origin <- readRDS("monthly_weather_origin.rds")
    monthly_weather_dest <- readRDS("monthly_weather_dest.rds")
    cancel_model_loaded <- TRUE
    cat("✓ Loaded cancellation model and thresholds\n")
    cat("✓ Loaded monthly weather lookup tables\n")
    
    # Populate airport dropdowns with ALL airports from thresholds
    all_airports <- cancel_thresholds$origin_levels
    airport_choices <- setNames(all_airports, all_airports)
    
    updateSelectInput(session, "origin", choices = airport_choices, selected = "JFK")
    updateSelectInput(session, "dest", choices = airport_choices, selected = "LAX")
    
    cat("✓ Populated dropdowns with", length(all_airports), "airports\n")
  }, error = function(e) {
    cat("⚠️ Could not load cancellation model:", e$message, "\n")
    cat("   Using default airport list\n")
    
    # Fallback to basic airport list if thresholds not available
    default_airports <- c("ATL", "ORD", "DFW", "DEN", "LAX", "PHX", "IAH", "SFO", 
                         "LAS", "MCO", "SEA", "EWR", "MSP", "DTW", "BOS", "PHL", 
                         "LGA", "FLL", "BWI", "DCA", "MDW", "SLC", "SAN", "IAD", 
                         "TPA", "PDX", "HNL", "JFK", "MIA", "CLT")
    default_choices <- setNames(default_airports, default_airports)
    updateSelectInput(session, "origin", choices = default_choices, selected = "JFK")
    updateSelectInput(session, "dest", choices = default_choices, selected = "LAX")
  })
  
  # Load delay model for connection insurance
  tryCatch({
    delay_model <- readRDS("model_delay_15min_final.rds")
    delay_thresholds <- readRDS("delay_connection_insurance_thresholds.rds")
    delay_model_loaded <- TRUE
    cat("✓ Loaded delay model and connection insurance thresholds\n")
    cat("✓ Connection insurance: 13 risk tiers with pre-calculated miss rates\n")
  }, error = function(e) {
    cat("⚠️ Could not load delay model:", e$message, "\n")
    cat("   Connection insurance tab will be unavailable\n")
  })
  
  # ══════════════════════════════════════════════════════════════
  
  predictions <- reactiveValues(
    cancel = NULL,
    delay_15 = NULL,
    delay_30 = NULL,
    delay_45 = NULL,
    delay_60 = NULL,
    method = NULL,
    # New: Model-based predictions
    cancel_model_score = NULL,
    cancel_risk_tier = NULL,
    delay_model_score = NULL
  )
  
  # Sample flight buttons
  observeEvent(input$sample_high, {
    updateSelectInput(session, "origin", selected = "JFK")
    updateSelectInput(session, "dest", selected = "LAX")
    updateSelectInput(session, "time", selected = 1800)
    updateSliderInput(session, "distance", value = 2475)
    updateDateInput(session, "date", value = as.Date("2026-01-15"))
  })
  
  observeEvent(input$sample_low, {
    updateSelectInput(session, "origin", selected = "ATL")
    updateSelectInput(session, "dest", selected = "ORD")
    updateSelectInput(session, "time", selected = 900)
    updateSliderInput(session, "distance", value = 700)
    updateDateInput(session, "date", value = as.Date("2026-06-15"))
  })
  
  # Main prediction
  observeEvent(input$predict_btn, {
    
    month <- as.numeric(format(input$date, "%m"))
    
    # Get historical pattern
    pattern <- get_historical_pattern(
      origin = input$origin,
      month = month,
      dep_time = as.numeric(input$time)
    )
    
    if (!pattern$found) {
      showNotification(
        pattern$message,
        type = "warning",
        duration = 5
      )
      return()
    }
    
    # Store in reactive values
    predictions$cancel <- pattern$cancel
    predictions$delay_15 <- pattern$delay_15
    predictions$delay_30 <- pattern$delay_30
    predictions$delay_45 <- pattern$delay_45
    predictions$delay_60 <- pattern$delay_60
    predictions$method <- pattern$method
    predictions$sample_size <- pattern$sample_size
    predictions$time_specific <- pattern$time_specific
    predictions$weather_used <- list(
      visibility = pattern$avg_visibility,
      temperature = pattern$avg_temperature,
      season = pattern$month
    )
    predictions$distribution <- list(
      ontime = pattern$pct_ontime,
      minor = pattern$pct_minor,
      moderate = pattern$pct_moderate,
      major = pattern$pct_major
    )
    
    # Get delay type breakdown
    delay_type_info <- get_delay_type_breakdown(input$origin, month)
    predictions$delay_type_info <- delay_type_info
    
    # Get airline comparison
    airline_info <- get_airline_comparison(input$origin, input$dest, month)
    predictions$airline_info <- airline_info
    predictions$avg_delay_when_delayed <- pattern$avg_delay_when_delayed
    
    # ══════════════════════════════════════════════════════════════
    # MODEL-BASED CANCELLATION PREDICTION (for insurance tab)
    # ══════════════════════════════════════════════════════════════
    
    if (cancel_model_loaded) {
      tryCatch({
        # Get encoded airport values
        origin_encoded <- match(input$origin, cancel_thresholds$origin_levels)
        dest_encoded <- match(input$dest, cancel_thresholds$dest_levels)
        
        # Look up monthly weather averages for this origin/dest/month
        origin_weather <- monthly_weather_origin %>%
          filter(Origin == origin_encoded, Month == month)
        
        dest_weather <- monthly_weather_dest %>%
          filter(Dest == dest_encoded, Month == month)
        
        # Extract weather values (with fallback to reasonable defaults)
        if (nrow(origin_weather) > 0) {
          origin_temp <- origin_weather$origin_TEMP
          origin_dewp <- origin_weather$origin_DEWP
          origin_visib <- origin_weather$origin_VISIB
          origin_wdsp <- origin_weather$origin_WDSP
          origin_mxspd <- origin_weather$origin_MXSPD
          origin_max <- origin_weather$origin_MAX
          origin_min <- origin_weather$origin_MIN
          origin_prcp <- origin_weather$origin_PRCP
          origin_sndp <- origin_weather$origin_SNDP
        } else {
          # Fallback to season-based estimates if airport/month not in training data
          origin_temp <- 60
          origin_dewp <- 50
          origin_visib <- 10
          origin_wdsp <- 5
          origin_mxspd <- 15
          origin_max <- 70
          origin_min <- 50
          origin_prcp <- 0
          origin_sndp <- 0
        }
        
        if (nrow(dest_weather) > 0) {
          dest_temp <- dest_weather$dest_TEMP
          dest_dewp <- dest_weather$dest_DEWP
          dest_visib <- dest_weather$dest_VISIB
          dest_wdsp <- dest_weather$dest_WDSP
          dest_mxspd <- dest_weather$dest_MXSPD
          dest_max <- dest_weather$dest_MAX
          dest_min <- dest_weather$dest_MIN
          dest_prcp <- dest_weather$dest_PRCP
          dest_sndp <- dest_weather$dest_SNDP
        } else {
          dest_temp <- 60
          dest_dewp <- 50
          dest_visib <- 10
          dest_wdsp <- 5
          dest_mxspd <- 15
          dest_max <- 70
          dest_min <- 50
          dest_prcp <- 0
          dest_sndp <- 0
        }
        
        # Engineer features to match cancellation model training data
        cancel_flight_features <- data.frame(
          Year = 2019,  # Use test year
          Quarter = ceiling(month / 3),
          Month = month,
          DayofMonth = as.numeric(format(input$date, "%d")),
          DayOfWeek = as.numeric(format(input$date, "%u")),
          CRSDepTime = as.numeric(input$time),
          CRSElapsedTime = input$distance / 7.5,  # Rough estimate
          Reporting_Airline = match(input$airline, cancel_thresholds$airline_levels),
          Origin = origin_encoded,
          Dest = dest_encoded,
          Distance = input$distance,
          OriginWac = 1,  # Placeholder
          DestWac = 1,    # Placeholder
          avg_fare = 400,  # Default ticket price
          # REAL monthly weather averages from training data
          origin_TEMP = origin_temp,
          origin_DEWP = origin_dewp,
          origin_VISIB = origin_visib,
          origin_WDSP = origin_wdsp,
          origin_MXSPD = origin_mxspd,
          origin_MAX = origin_max,
          origin_MIN = origin_min,
          origin_PRCP = origin_prcp,
          origin_SNDP = origin_sndp,
          dest_TEMP = dest_temp,
          dest_DEWP = dest_dewp,
          dest_VISIB = dest_visib,
          dest_WDSP = dest_wdsp,
          dest_MXSPD = dest_mxspd,
          dest_MAX = dest_max,
          dest_MIN = dest_min,
          dest_PRCP = dest_prcp,
          dest_SNDP = dest_sndp
        )
        
        # Ensure correct column order
        cancel_flight_features <- cancel_flight_features[, cancel_thresholds$feature_names]
        
        # DEBUG: Show encoding values
        cat("\n=== DEBUG ENCODING ===\n")
        cat("Input Airline:", input$airline, "\n")
        cat("Encoded as:", match(input$airline, cancel_thresholds$airline_levels), "\n")
        cat("Origin:", input$origin, "→", match(input$origin, cancel_thresholds$origin_levels), "\n")
        cat("Dest:", input$dest, "→", match(input$dest, cancel_thresholds$dest_levels), "\n")
        cat("Month:", month, "\n")
        cat("DayofMonth:", as.numeric(format(input$date, "%d")), "\n")
        cat("CRSDepTime:", as.numeric(input$time), "\n")
        cat("Distance:", input$distance, "\n")
        cat("=====================\n\n")
        
        # Create DMatrix
        dtest_flight <- xgb.DMatrix(as.matrix(cancel_flight_features))
        
        # Get prediction (just the score, UI will calculate tier)
        cancel_score <- predict(cancel_model, dtest_flight)
        
        # Get SHAP values (feature contributions)
        cancel_shap <- predict(cancel_model, dtest_flight, predcontrib = TRUE, approxcontrib = FALSE)
        
        predictions$cancel_model_score <- cancel_score
        predictions$cancel_shap <- cancel_shap
        # Don't store tier - UI will calculate it fresh
        
        cat(sprintf("✓ Cancellation prediction: score=%.4f\n", cancel_score))
        
      }, error = function(e) {
        cat("Error in cancellation prediction:", e$message, "\n")
        predictions$cancel_model_score <- NULL
      })
    }
    
    # ══════════════════════════════════════════════════════════════
    # MODEL-BASED DELAY PREDICTION (for connection insurance tab)
    # ══════════════════════════════════════════════════════════════
    
    if (delay_model_loaded) {
      tryCatch({
        # Same feature preparation as cancellation model
        origin_encoded <- match(input$origin, cancel_thresholds$origin_levels)
        dest_encoded <- match(input$dest, cancel_thresholds$dest_levels)
        
        # Look up monthly weather
        origin_weather <- monthly_weather_origin %>%
          filter(Origin == origin_encoded, Month == month)
        
        dest_weather <- monthly_weather_dest %>%
          filter(Dest == dest_encoded, Month == month)
        
        # Extract weather (same fallbacks as cancellation)
        if (nrow(origin_weather) > 0) {
          origin_temp <- origin_weather$origin_TEMP
          origin_dewp <- origin_weather$origin_DEWP
          origin_visib <- origin_weather$origin_VISIB
          origin_wdsp <- origin_weather$origin_WDSP
          origin_mxspd <- origin_weather$origin_MXSPD
          origin_max <- origin_weather$origin_MAX
          origin_min <- origin_weather$origin_MIN
          origin_prcp <- origin_weather$origin_PRCP
          origin_sndp <- origin_weather$origin_SNDP
        } else {
          origin_temp <- 60
          origin_dewp <- 50
          origin_visib <- 10
          origin_wdsp <- 5
          origin_mxspd <- 15
          origin_max <- 70
          origin_min <- 50
          origin_prcp <- 0
          origin_sndp <- 0
        }
        
        if (nrow(dest_weather) > 0) {
          dest_temp <- dest_weather$dest_TEMP
          dest_dewp <- dest_weather$dest_DEWP
          dest_visib <- dest_weather$dest_VISIB
          dest_wdsp <- dest_weather$dest_WDSP
          dest_mxspd <- dest_weather$dest_MXSPD
          dest_max <- dest_weather$dest_MAX
          dest_min <- dest_weather$dest_MIN
          dest_prcp <- dest_weather$dest_PRCP
          dest_sndp <- dest_weather$dest_SNDP
        } else {
          dest_temp <- 60
          dest_dewp <- 50
          dest_visib <- 10
          dest_wdsp <- 5
          dest_mxspd <- 15
          dest_max <- 70
          dest_min <- 50
          dest_prcp <- 0
          dest_sndp <- 0
        }
        
        # Get airline encoding (use cancellation thresholds airline levels)
        airline_match <- match(input$airline, cancel_thresholds$airline_levels)
        airline_encoded <- if (is.na(airline_match)) 1 else airline_match
        
        # Build feature vector - MUST match training order exactly!
        # Training order (from booking_features after removing Year):
        # Quarter, Month, DayofMonth, DayOfWeek, CRSDepTime, CRSElapsedTime,
        # Reporting_Airline, Origin, Dest, Distance, OriginWac, DestWac, avg_fare,
        # origin_TEMP, origin_DEWP, origin_VISIB, origin_WDSP, origin_MXSPD, origin_MAX, origin_MIN, origin_PRCP, origin_SNDP,
        # dest_TEMP, dest_DEWP, dest_VISIB, dest_WDSP, dest_MXSPD, dest_MAX, dest_MIN, dest_PRCP, dest_SNDP
        flight_features <- data.frame(
          Quarter = ceiling(month / 3),
          Month = month,
          DayofMonth = as.numeric(format(input$date, "%d")),
          DayOfWeek = as.numeric(format(input$date, "%u")),
          CRSDepTime = as.numeric(input$time),
          CRSElapsedTime = 180,  # Placeholder - average flight time
          Reporting_Airline = airline_encoded,
          Origin = origin_encoded,
          Dest = dest_encoded,
          Distance = input$distance,
          OriginWac = 1,  # Placeholder
          DestWac = 1,    # Placeholder
          avg_fare = input$ticket_price,  # Use actual ticket price from user input
          origin_TEMP = origin_temp,
          origin_DEWP = origin_dewp,
          origin_VISIB = origin_visib,
          origin_WDSP = origin_wdsp,
          origin_MXSPD = origin_mxspd,
          origin_MAX = origin_max,
          origin_MIN = origin_min,
          origin_PRCP = origin_prcp,
          origin_SNDP = origin_sndp,
          dest_TEMP = dest_temp,
          dest_DEWP = dest_dewp,
          dest_VISIB = dest_visib,
          dest_WDSP = dest_wdsp,
          dest_MXSPD = dest_mxspd,
          dest_MAX = dest_max,
          dest_MIN = dest_min,
          dest_PRCP = dest_prcp,
          dest_SNDP = dest_sndp
        )
        
        # DEBUG: Print feature values
        cat("=== DEBUG DELAY MODEL FEATURES ===\n")
        cat(sprintf("Airline: %s → %d\n", input$airline, airline_encoded))
        cat(sprintf("Origin: %s → %d\n", input$origin, origin_encoded))
        cat(sprintf("Dest: %s → %d\n", input$dest, dest_encoded))
        cat(sprintf("Month: %d\n", month))
        cat(sprintf("DayofMonth: %d\n", as.numeric(format(input$date, "%d"))))
        cat(sprintf("CRSDepTime: %s → %d\n", input$time, as.numeric(input$time)))
        cat(sprintf("Distance: %d\n", input$distance))
        
        # DEBUG: Show the actual matrix values
        cat("\nActual matrix being passed to model:\n")
        feature_matrix <- as.matrix(flight_features)
        print(feature_matrix[1, 1:7])  # Show first 7 features
        cat("==================================\n")
        
        # Predict
        dtest_delay <- xgb.DMatrix(feature_matrix)
        delay_score <- predict(delay_model, dtest_delay)
        
        # Get SHAP values (feature contributions)
        delay_shap <- predict(delay_model, dtest_delay, predcontrib = TRUE, approxcontrib = FALSE)
        
        predictions$delay_model_score <- delay_score
        predictions$delay_shap <- delay_shap
        
        cat(sprintf("✓ Delay prediction: score=%.4f\n\n", delay_score))
        
      }, error = function(e) {
        cat("Error in delay prediction:", e$message, "\n")
        predictions$delay_model_score <- NULL
      })
    }
  })
  
  # Risk summary card
  output$risk_summary_card <- renderUI({
    req(predictions$delay_15, input$origin, input$date)
    
    # Get month name
    month_name <- format(input$date, "%B")
    origin_airport <- input$origin
    
    div(
      style = "background: linear-gradient(135deg, #1976d2 0%, #1565c0 100%); 
               color: white; 
               padding: 25px 30px; 
               border-radius: 12px; 
               margin-bottom: 20px;
               box-shadow: 0 4px 12px rgba(25, 118, 210, 0.3);",
      h4(style = "margin: 0 0 10px 0; font-weight: 700; font-size: 18px;",
         "Historical Delay Patterns"),
      p(style = "margin: 0; font-size: 15px; line-height: 1.6; opacity: 0.95;",
        sprintf("The chart below shows historical delay distribution for flights departing from %s in %s. This data reflects typical delay patterns based on thousands of similar flights during this month and time period.", 
                origin_airport, month_name))
    )
  })
  
  # Metrics
  output$cancel_risk_text <- renderText({
    req(predictions$cancel)
    paste0(round(predictions$cancel * 100), "%")
  })
  
  output$delay_15_text <- renderText({
    req(predictions$delay_15)
    paste0(round(predictions$delay_15 * 100), "%")
  })
  
  output$delay_30_text <- renderText({
    req(predictions$delay_30)
    paste0(round(predictions$delay_30 * 100), "%")
  })
  
  output$delay_45_text <- renderText({
    req(predictions$delay_45)
    paste0(round(predictions$delay_45 * 100), "%")
  })
  
  # Historical context box
  output$historical_context_ui <- renderUI({
    req(predictions$delay_15)
    
    if (!is.null(predictions$sample_size) && !is.null(predictions$distribution)) {
      
      # Get day of week
      day_name <- format(input$date, "%A")
      month_name <- format(input$date, "%B")
      
      # Get time category
      time_cat <- if (as.numeric(input$time) < 800) {
        "early morning"
      } else if (as.numeric(input$time) < 1200) {
        "morning"
      } else if (as.numeric(input$time) < 1500) {
        "midday"
      } else if (as.numeric(input$time) < 1800) {
        "afternoon"
      } else {
        "evening"
      }
      
      div(
        style = "background: linear-gradient(135deg, #fff9e6 0%, #fff3cc 100%); 
                 padding: 25px; border-radius: 12px; margin-bottom: 20px;
                 border-left: 5px solid #ffc107; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
        
        div(class = "section-title",
          style = "margin: 0 0 15px 0; font-size: 17px; color: #d68910;",
          icon("chart-bar"),
          " Historical Context"
        ),
        
        p(style = "font-size: 15px; color: #2c3e50; margin-bottom: 15px; font-weight: 600;",
          sprintf("At %s in %s (%s flights):", 
                  input$origin, month_name, time_cat)
        ),
        
        if (!is.null(predictions$avg_delay_when_delayed)) {
          p(style = "font-size: 14px; color: #34495e; margin: 10px 0 15px 20px;",
            icon("clock", style = "color: #ffc107; margin-right: 8px;"),
            sprintf("Average historical delay: %.0f minutes", 
                    predictions$avg_delay_when_delayed),
            tags$br(),
            tags$span(
              style = "font-size: 13px; color: #7f8c8d; font-style: italic;",
              sprintf("(based on %s similar flights)", 
                      format(predictions$sample_size, big.mark = ","))
            )
          )
        },
        
        div(style = "margin: 15px 0 5px 20px;",
          div(style = "margin: 8px 0; font-size: 14px; color: #2c3e50;",
            tags$span(style = "display: inline-block; width: 12px; height: 12px; 
                              background: #28a745; border-radius: 3px; margin-right: 8px;"),
            sprintf("%.0f%% were on-time (< 15 min)", predictions$distribution$ontime)
          ),
          div(style = "margin: 8px 0; font-size: 14px; color: #2c3e50;",
            tags$span(style = "display: inline-block; width: 12px; height: 12px; 
                              background: #ffc107; border-radius: 3px; margin-right: 8px;"),
            sprintf("%.0f%% had minor delays (15-30 min)", predictions$distribution$minor)
          ),
          div(style = "margin: 8px 0; font-size: 14px; color: #2c3e50;",
            tags$span(style = "display: inline-block; width: 12px; height: 12px; 
                              background: #dc3545; border-radius: 3px; margin-right: 8px;"),
            sprintf("%.0f%% had major delays (30+ min)", 
                    predictions$distribution$moderate + predictions$distribution$major)
          )
        ),
        
        if (!is.null(predictions$time_specific) && !predictions$time_specific) {
          tags$small(
            style = "display: block; margin-top: 15px; color: #7f8c8d; font-style: italic;",
            icon("info-circle"),
            sprintf(" Pattern averaged across all %s times of day (limited data for %s specifically)", 
                    month_name, time_cat)
          )
        }
      )
    }
  })
  
  # Weather info display
  output$weather_info_ui <- renderUI({
    req(predictions$delay_15)
    
    if (!is.null(predictions$weather_used)) {
      weather <- predictions$weather_used
      
      div(
        style = "background: linear-gradient(135deg, #e8f4f8 0%, #d4e9f2 100%); 
                 padding: 20px; border-radius: 10px; margin-bottom: 20px;
                 border-left: 4px solid #1976d2;",
        
        div(class = "section-title",
          style = "margin: 0 0 15px 0; font-size: 16px;",
          icon("cloud"),
          sprintf(" Typical Weather (%s)", input$origin)
        ),
        
        fluidRow(
          column(4,
            div(style = "text-align: center;",
              div(style = "font-size: 14px; color: #7f8c8d; margin-bottom: 5px;",
                icon("eye"), " Visibility"
              ),
              div(style = "font-size: 24px; font-weight: 700; color: #2c3e50;",
                paste0(round(weather$visibility, 1), " mi")
              )
            )
          ),
          column(4,
            div(style = "text-align: center;",
              div(style = "font-size: 14px; color: #7f8c8d; margin-bottom: 5px;",
                icon("temperature-high"), " Temperature"
              ),
              div(style = "font-size: 24px; font-weight: 700; color: #2c3e50;",
                paste0(round(weather$temperature, 0), "°F")
              )
            )
          ),
          column(4,
            div(style = "text-align: center;",
              div(style = "font-size: 14px; color: #7f8c8d; margin-bottom: 5px;",
                icon("calendar-alt"), " Month"
              ),
              div(style = "font-size: 24px; font-weight: 700; color: #2c3e50;",
                weather$season
              )
            )
          )
        ),
        
        tags$small(
          style = "display: block; margin-top: 15px; color: #7f8c8d; font-style: italic;",
          icon("info-circle"),
          sprintf(" Historical average conditions at %s in %s", 
                  input$origin, weather$season)
        )
      )
    }
  })
  
  # Delay distribution plot
  
  # Delay type UI
  output$delay_type_ui <- renderUI({
    req(predictions$delay_15)
    if (is.null(predictions$delay_type_info) || !predictions$delay_type_info$found) return(NULL)
    dt <- predictions$delay_type_info
    delay_types_list <- list(
      list(name = "Late Aircraft", pct = dt$late_aircraft_pct, icon = "plane-arrival", color = "#dc3545"),
      list(name = "Carrier", pct = dt$carrier_pct, icon = "building", color = "#1976d2"),
      list(name = "NAS (Air Traffic)", pct = dt$nas_pct, icon = "tower-broadcast", color = "#9b59b6"),
      list(name = "Weather", pct = dt$weather_pct, icon = "cloud-rain", color = "#ffc107"),
      list(name = "Security", pct = dt$security_pct, icon = "shield-halved", color = "#95a5a6")
    )
    delay_types_list <- delay_types_list[order(sapply(delay_types_list, function(x) x$pct), decreasing = TRUE)]
    significant_types <- Filter(function(x) x$pct > 5, delay_types_list)
    if (length(significant_types) == 0) significant_types <- delay_types_list[1:min(3, length(delay_types_list))]
    div(style = "background: linear-gradient(135deg, #f0f4ff 0%, #e6edff 100%); padding: 25px; border-radius: 12px; margin-bottom: 20px; border-left: 5px solid #5a67d8;",
      div(style = "margin: 0 0 15px 0; font-size: 17px; color: #5a67d8; font-weight: 600;", icon("chart-pie"), " Historical Delay Causes"),
      p(style = "font-size: 14px; color: #34495e; margin-bottom: 15px;", sprintf("At %s in %s, when delays occurred historically:", dt$origin, dt$month)),
      div(style = "margin: 15px 0;", lapply(significant_types, function(type) {
        div(style = "margin: 12px 0;",
          div(style = "display: flex; align-items: center; margin-bottom: 5px;",
            icon(type$icon, style = sprintf("color: %s; margin-right: 10px;", type$color)),
            tags$span(style = "font-weight: 600;", type$name),
            tags$span(style = "margin-left: auto; font-weight: 700;", sprintf("%.0f%%", type$pct))
          ),
          div(style = "width: 100%; height: 8px; background: #ecf0f1; border-radius: 4px;",
            div(style = sprintf("width: %.1f%%; height: 100%%; background: %s;", type$pct, type$color))
          )
        )
      })),
      tags$small(style = "color: #7f8c8d; font-style: italic;", icon("info-circle"), sprintf(" Based on %s delayed flights", format(dt$sample_size, big.mark = ",")))
    )
  })
  
  # Airline comparison UI
  output$airline_comparison_ui <- renderUI({
    req(predictions$delay_15)
    if (is.null(predictions$airline_info) || !predictions$airline_info$found) return(NULL)
    ai <- predictions$airline_info
    airlines <- ai$airlines
    airline_names <- c(
      "AA" = "American", "DL" = "Delta", "UA" = "United", "WN" = "Southwest", 
      "B6" = "JetBlue", "AS" = "Alaska", "NK" = "Spirit", "F9" = "Frontier", 
      "G4" = "Allegiant", "OO" = "SkyWest", "EV" = "ExpressJet", "MQ" = "Envoy",
      "US" = "US Airways", "FL" = "AirTran", "YV" = "Mesa", "9E" = "Endeavor",
      "XE" = "PSA", "HA" = "Hawaiian", "CO" = "Continental", "OH" = "PSA (OH)",
      "VX" = "Virgin America", "YX" = "Republic"
    )
    airlines <- airlines %>% mutate(display_name = ifelse(airline %in% names(airline_names), airline_names[airline], airline))
    div(style = "background: linear-gradient(135deg, #f5f9ff 0%, #e8f2ff 100%); padding: 25px; border-radius: 12px; margin-bottom: 20px; border-left: 5px solid #1976d2;",
      div(style = "margin: 0 0 15px 0; font-size: 17px; color: #1565c0; font-weight: 600;", icon("plane-departure"), " Airlines on this Route"),
      p(style = "font-size: 14px; color: #34495e; margin-bottom: 15px;", sprintf("%s → %s", ai$origin, ai$dest)),
      p(style = "font-size: 12px; color: #7f8c8d; font-style: italic; margin-bottom: 15px;", 
        icon("info-circle"), " Historical averages (2010-2018) across all seasons"),
      div(style = "margin: 15px 0;", lapply(1:nrow(airlines), function(i) {
        a <- airlines[i, ]
        bg_color <- if (a$is_best) "#d4edda" else if (a$is_worst) "#f8d7da" else "#ffffff"
        border_color <- if (a$is_best) "#28a745" else if (a$is_worst) "#dc3545" else "#dee2e6"
        div(style = sprintf("background: %s; border: 2px solid %s; border-radius: 8px; padding: 15px; margin: 10px 0;", bg_color, border_color),
          div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
            div(style = "display: flex; align-items: center;",
              tags$span(style = sprintf("background: %s; color: white; padding: 4px 10px; border-radius: 4px; font-weight: 700; margin-right: 10px;", border_color), a$airline),
              tags$span(style = "font-size: 16px; font-weight: 600;", a$display_name)
            ),
            div(style = "display: flex; gap: 8px;",
              if (a$is_best) tags$span(style = "background: #28a745; color: white; padding: 4px 12px; border-radius: 12px; font-size: 12px; font-weight: 600;", icon("star"), " Best"),
              if (a$is_cheapest) tags$span(style = "background: #ffc107; color: white; padding: 4px 12px; border-radius: 12px; font-size: 12px; font-weight: 600;", icon("dollar-sign"), " Cheapest")
            )
          ),
          div(style = "display: flex; justify-content: space-around; margin-top: 12px;",
            div(style = "text-align: center;", div(style = "font-size: 11px; color: #7f8c8d; text-transform: uppercase;", "Avg Delay"), div(style = "font-size: 18px; font-weight: 700; color: #2c3e50;", sprintf("%.0f min", a$avg_delay))),
            div(style = "text-align: center;", div(style = "font-size: 11px; color: #7f8c8d; text-transform: uppercase;", "Avg Fare"), div(style = "font-size: 18px; font-weight: 700; color: #28a745;", sprintf("$%.0f", a$avg_fare))),
            div(style = "text-align: center;", div(style = "font-size: 11px; color: #7f8c8d; text-transform: uppercase;", "On-Time"), div(style = "font-size: 18px; font-weight: 700; color: #1976d2;", sprintf("%.0f%%", a$ontime_rate))),
            div(style = "text-align: center;", div(style = "font-size: 11px; color: #7f8c8d; text-transform: uppercase;", "Flights"), div(style = "font-size: 14px; font-weight: 600; color: #7f8c8d;", format(a$flights, big.mark = ",")))
          )
        )
      })),
      tags$small(style = "color: #7f8c8d; font-style: italic;", icon("info-circle"), " Historical averages - actual performance may vary")
    )
  })

  output$delay_distribution <- renderPlotly({
    req(predictions$delay_15)
    
    # Calculate range probabilities
    on_time <- (1 - predictions$delay_15) * 100
    minor <- (predictions$delay_15 - predictions$delay_30) * 100
    moderate <- (predictions$delay_30 - predictions$delay_45) * 100
    major <- (predictions$delay_45 - predictions$delay_60) * 100
    severe <- predictions$delay_60 * 100
    
    categories <- c("On-time\n(0-14 min)", "Minor\n(15-29 min)", 
                   "Moderate\n(30-44 min)", "Major\n(45-59 min)",
                   "Severe\n(60+ min)")
    probabilities <- c(on_time, minor, moderate, major, severe)
    colors <- c("#28a745", "#ffc107", "#e67e22", "#dc3545", "#8b0000")
    
    plot_ly(
      x = factor(categories, levels = categories),  # Force category order
      y = probabilities,
      type = "bar",
      marker = list(
        color = colors,
        line = list(color = 'white', width = 2)
      ),
      text = paste0(round(probabilities, 1), "%"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Probability: %{y:.1f}%<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        yaxis = list(
          title = "Probability (%)",
          range = c(0, max(probabilities) * 1.2),
          showgrid = TRUE,
          gridcolor = "#ecf0f1"
        ),
        xaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = categories
        ),
        showlegend = FALSE,
        margin = list(t = 30, b = 80),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  # Interpretation
  output$interpretation_ui <- renderUI({
    req(predictions$delay_15, predictions$delay_30)
    
    on_time <- (1 - predictions$delay_15) * 100
    minor <- (predictions$delay_15 - predictions$delay_30) * 100
    moderate <- predictions$delay_30 * 100
    cancel <- predictions$cancel * 100
    
    most_likely_idx <- which.max(c(on_time, minor, moderate))
    outcome <- c("on-time or minimal delay", 
                "minor delay (15-30 minutes)",
                "substantial delay (30+ minutes)")[most_likely_idx]
    
    div(
      div(class = "section-title",
        icon("chart-pie"), "Historical Pattern"
      ),
      
      p(style = "font-size: 16px; color: #2c3e50; margin-bottom: 20px;",
        "Flights matching this profile historically experienced: ",
        tags$strong(outcome),
        sprintf(" (%.1f%% of flights)", max(c(on_time, minor, moderate)))
      ),
      
      if (!is.null(predictions$sample_size)) {
        p(style = "font-size: 14px; color: #7f8c8d; font-style: italic;",
          icon("database"),
          sprintf(" Based on %s historical flights", 
                  format(predictions$sample_size, big.mark = ","))
        )
      },
      
      div(class = "section-title",
        icon("percentage"), "Probability Breakdown"
      ),
      
      div(
        div(class = "progress-bar-container",
          div(class = "progress-bar-fill",
            style = paste0("width: ", on_time, "%; background: #28a745;"),
            if(on_time > 10) paste0(round(on_time, 1), "% On-time") else ""
          )
        ),
        div(class = "progress-bar-container",
          div(class = "progress-bar-fill",
            style = paste0("width: ", minor, "%; background: #ffc107;"),
            if(minor > 10) paste0(round(minor, 1), "% Minor") else ""
          )
        ),
        div(class = "progress-bar-container",
          div(class = "progress-bar-fill",
            style = paste0("width: ", moderate, "%; background: #dc3545;"),
            if(moderate > 10) paste0(round(moderate, 1), "% Substantial") else ""
          )
        ),
        div(class = "progress-bar-container",
          div(class = "progress-bar-fill",
            style = paste0("width: ", cancel, "%; background: #8b0000;"),
            if(cancel > 3) paste0(round(cancel, 1), "% Cancelled") else ""
          )
        )
      ),
      
      hr(),
      
      div(class = "section-title",
        icon("lightbulb"), "What This Means"
      ),
      
      if (!is.null(predictions$avg_delay_when_delayed)) {
        p(style = "font-size: 15px; color: #2c3e50; margin-bottom: 15px;",
          "When delays occurred, the average delay was ",
          tags$strong(sprintf("%.0f minutes", predictions$avg_delay_when_delayed)),
          "."
        )
      },
      
      p(if(predictions$delay_15 > 0.6) {
        "⚠️ High historical delay rate for this route/time combination. Consider rebooking to an earlier flight or different time of day if you have tight connections."
      } else if(predictions$delay_15 > 0.4) {
        "⚠️ Moderate historical delay rate. Allow extra buffer time for connections. Check flight status the day before departure."
      } else {
        "✓ Low historical delay rate for this route/time combination. Normal operations typically experienced, but always monitor flight status as conditions can change."
      }),
      
      if(!is.null(predictions$method)) {
        tags$small(
          style = "display: block; margin-top: 15px; color: #95a5a6; font-style: italic;",
          icon("info-circle"),
          " ", predictions$method
        )
      }
    )
  })
  
  # Connection risk
  
  # ══════════════════════════════════════════════════════════════
  # CANCELLATION INSURANCE UI
  # ══════════════════════════════════════════════════════════════
  
  output$cancel_insurance_ui <- renderUI({
    
    if (!cancel_model_loaded) {
      return(
        div(style = "background: #fff3cd; padding: 20px; border-radius: 10px; border-left: 5px solid #ffc107;",
          h4(icon("exclamation-triangle"), " Model Not Available"),
          p("Cancellation insurance analysis requires the trained model file. Please ensure 'model_cancel_final.rds' and 'cancel_insurance_thresholds.rds' are available.")
        )
      )
    }
    
    # Check if prediction has been run
    if (is.null(predictions$cancel_model_score)) {
      return(
        div(style = "background: #e3f2fd; padding: 30px; border-radius: 10px; text-align: center;",
          h4(style = "color: #1976d2;", icon("info-circle"), " Click 'Predict Flight Risk' to analyze"),
          p("Enter your flight details and click the Predict button to see cancellation insurance recommendations.")
        )
      )
    }
    
    # Get score and calculate tier/rate fresh each time
    score <- predictions$cancel_model_score
    
    # Calculate tier based on current score (12 tiers for maximum granularity)
    tier <- if (score > cancel_thresholds$top_1_pct) {
      "Very High (Top 1%)"
    } else if (score > cancel_thresholds$top_2_pct) {
      "High (Top 2%)"
    } else if (score > cancel_thresholds$top_3_pct) {
      "Moderate (Top 3%)"
    } else if (score > cancel_thresholds$top_5_pct) {
      "Above Average (Top 5%)"
    } else if (score > cancel_thresholds$top_10_pct) {
      "Average (Top 10%)"
    } else if (!is.null(cancel_thresholds$top_20_pct) && score > cancel_thresholds$top_20_pct) {
      "Below Average (Top 20%)"
    } else if (!is.null(cancel_thresholds$top_30_pct) && score > cancel_thresholds$top_30_pct) {
      "Low (Top 30%)"
    } else if (!is.null(cancel_thresholds$top_40_pct) && score > cancel_thresholds$top_40_pct) {
      "Low (Top 40%)"
    } else if (!is.null(cancel_thresholds$top_50_pct) && score > cancel_thresholds$top_50_pct) {
      "Low (Top 50%)"
    } else if (!is.null(cancel_thresholds$top_60_pct) && score > cancel_thresholds$top_60_pct) {
      "Very Low (Top 60%)"
    } else if (!is.null(cancel_thresholds$top_70_pct) && score > cancel_thresholds$top_70_pct) {
      "Very Low (Top 70%)"
    } else if (!is.null(cancel_thresholds$top_80_pct) && score > cancel_thresholds$top_80_pct) {
      "Very Low (Top 80%)"
    } else {
      "Minimal Risk"
    }
    
    # Map model score to empirical cancellation rate (with granular tiers)
    # Use tryCatch to handle missing threshold fields gracefully
    cancel_rate <- tryCatch({
      if (score > cancel_thresholds$top_1_pct) {
        cancel_thresholds$cancel_rate_top_1
      } else if (score > cancel_thresholds$top_2_pct) {
        cancel_thresholds$cancel_rate_top_2
      } else if (score > cancel_thresholds$top_3_pct) {
        cancel_thresholds$cancel_rate_top_3
      } else if (score > cancel_thresholds$top_5_pct) {
        cancel_thresholds$cancel_rate_top_5
      } else if (score > cancel_thresholds$top_10_pct) {
        cancel_thresholds$cancel_rate_top_10
      } else if (!is.null(cancel_thresholds$top_20_pct) && score > cancel_thresholds$top_20_pct) {
        cancel_thresholds$cancel_rate_top_20
      } else if (!is.null(cancel_thresholds$top_30_pct) && score > cancel_thresholds$top_30_pct) {
        cancel_thresholds$cancel_rate_top_30
      } else if (!is.null(cancel_thresholds$top_40_pct) && score > cancel_thresholds$top_40_pct) {
        cancel_thresholds$cancel_rate_top_40
      } else if (!is.null(cancel_thresholds$top_50_pct) && score > cancel_thresholds$top_50_pct) {
        cancel_thresholds$cancel_rate_top_50
      } else if (!is.null(cancel_thresholds$top_60_pct) && score > cancel_thresholds$top_60_pct) {
        cancel_thresholds$cancel_rate_top_60
      } else if (!is.null(cancel_thresholds$top_70_pct) && score > cancel_thresholds$top_70_pct) {
        cancel_thresholds$cancel_rate_top_70
      } else if (!is.null(cancel_thresholds$top_80_pct) && score > cancel_thresholds$top_80_pct) {
        cancel_thresholds$cancel_rate_top_80
      } else {
        # For flights below top 80%, use base cancellation rate
        cancel_thresholds$base_cancel_rate * 100
      }
    }, error = function(e) {
      # Fallback if threshold fields don't exist
      cat("Error accessing thresholds:", e$message, "\n")
      cancel_thresholds$base_cancel_rate * 100
    })
    
    # Use ticket price from input
    ticket_price        <- input$cancel_ticket_price
    insurance_cost      <- ticket_price * (cancel_thresholds$insurance_cost_pct / 100)
    expected_loss       <- ticket_price * (cancel_rate / 100)
    net_value           <- expected_loss - insurance_cost
    recommend           <- cancel_rate >= cancel_thresholds$break_even_rate

    # Color coding
    tier_color <- if (grepl("Very High|Top 1%", tier)) {
      "#dc3545"
    } else if (grepl("High|Top 2%", tier)) {
      "#fd7e14"
    } else if (grepl("Moderate|Top 3%", tier)) {
      "#ffc107"
    } else {
      "#28a745"
    }

    div(
      # Combined risk tier + EV banner
      div(style = sprintf("background: %s; color: white; padding: 20px; border-radius: 12px; margin-bottom: 20px; text-align: center;",
                         if(recommend) "#28a745" else "#dc3545"),
        h3(style = "margin: 0 0 8px 0; font-weight: 700;",
           if(recommend) "Expected Value: Positive" else "Expected Value: Negative"),
        h4(style = "margin: 0 0 6px 0; font-weight: 400;",
           sprintf("Risk Tier: %s | Cancellation Risk: %.1f%%", tier, cancel_rate)),
        h4(style = "margin: 0; font-weight: 400; opacity: 0.9;",
           sprintf("Break-Even Premium: $%.2f (%.1f%% of ticket)", expected_loss, cancel_rate))
      ),

      # Analysis + Economics
      div(style = "background: white; padding: 20px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
        h4(style = "margin-top: 0; color: #333;", icon("info-circle"), " Analysis"),
        p(style = "font-size: 15px; line-height: 1.6; color: #555;",
          if(recommend) {
            sprintf("Your estimated cancellation risk (%.1f%%) exceeds the break-even threshold (%.1f%%). Based on this model, insurance has positive expected value (+$%.2f on a $%.0f ticket). These results should be treated as a decision benchmark rather than a guarantee.",
                   cancel_rate, cancel_thresholds$break_even_rate, net_value, ticket_price)
          } else {
            sprintf("Your estimated cancellation risk (%.1f%%) is below the break-even threshold (%.1f%%). Based on this model, insurance has negative expected value at the standard premium. Insurance would be actuarially fair at a premium of $%.2f or less (%.1f%% of your $%.0f ticket).",
                   cancel_rate, cancel_thresholds$break_even_rate, expected_loss, cancel_rate, ticket_price)
          }
        ),

        hr(),

        h4(style = "color: #333;", icon("calculator"), " Economics"),
        p(style = "font-size: 14px; line-height: 1.6; color: #555;",
          sprintf("• Ticket Price: $%.0f", ticket_price), br(),
          sprintf("• Standard Premium: $%.2f (%.1f%% of ticket)", insurance_cost, cancel_thresholds$insurance_cost_pct), br(),
          sprintf("• Risk Tier: %s", tier), br(),
          sprintf("• Cancellation Rate: %.1f%%", cancel_rate), br(),
          sprintf("• Break-Even Threshold: %.1f%%", cancel_thresholds$break_even_rate), br(),
          sprintf("• Break-Even Premium: $%.2f (%.1f%% of ticket)", expected_loss, cancel_rate), br(),
          sprintf("• Net Expected Value: %s$%.2f", if(net_value > 0) "+" else "", net_value)
        )
      ),
      
      # Feature Importance - What Drives This Prediction
      div(style = "background: #f8f9fa; padding: 20px; border-radius: 12px; margin-bottom: 20px;",
        h4(style = "margin-top: 0; color: #333;", icon("lightbulb"), " What Drives This Prediction?"),
        p(style = "font-size: 14px; line-height: 1.6; color: #555; margin-bottom: 15px;",
          "Our model analyzes multiple factors to predict cancellation risk. Here's what matters most for YOUR specific flight:"
        ),
        
        # Dynamic SHAP-based feature importance (GROUPED)
        if(!is.null(predictions$cancel_shap)) {
          # Feature names for cancellation model
          cancel_feature_names <- c("Quarter", "Month", "DayofMonth", "DayOfWeek", "CRSDepTime", "CRSElapsedTime",
                                   "Reporting_Airline", "Origin", "Dest", "Distance", "OriginWac", "DestWac", "avg_fare",
                                   "origin_TEMP", "origin_DEWP", "origin_VISIB", "origin_WDSP", "origin_MXSPD",
                                   "origin_MAX", "origin_MIN", "origin_PRCP", "origin_SNDP",
                                   "dest_TEMP", "dest_DEWP", "dest_VISIB", "dest_WDSP", "dest_MXSPD",
                                   "dest_MAX", "dest_MIN", "dest_PRCP", "dest_SNDP")
          
          shap_result <- get_grouped_shap_percentages(predictions$cancel_shap, cancel_feature_names)
          
          # Map group names to user-friendly info
          group_info <- list(
            "Time & Schedule" = list(
              emoji = "⏰", 
              desc = sprintf("Your %s departure in %s", 
                           format(as.POSIXct(sprintf("2000-01-01 %s:00", input$time), format="%Y-%m-%d %H:%M"), "%I:%M %p"),
                           format(input$date, "%B"))
            ),
            "Route & Airports" = list(
              emoji = "✈️",
              desc = sprintf("%s → %s route characteristics", input$origin, input$dest)
            ),
            "Airline" = list(
              emoji = "🛫",
              desc = sprintf("%s's cancellation history", input$airline)
            ),
            "Weather Conditions" = list(
              emoji = "🌤️",
              desc = sprintf("Typical %s weather at %s and %s", format(input$date, "%B"), input$origin, input$dest)
            ),
            "Flight Details" = list(
              emoji = "📋",
              desc = "Flight duration and pricing factors"
            )
          )
          
          # Display all 5 groups
          div(
            lapply(1:5, function(i) {
              group_name <- shap_result$groups[i]
              group_pct <- round(shap_result$percentages[i])
              
              info <- group_info[[group_name]]
              
              div(style = "margin-bottom: 12px;",
                div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                  span(style = "font-weight: 600; color: #333;", sprintf("%s %s", info$emoji, group_name)),
                  span(style = "color: #1976d2; font-weight: 600;", sprintf("%d%%", group_pct))
                ),
                div(style = "background: #e3f2fd; height: 8px; border-radius: 4px;",
                  div(style = sprintf("background: #1976d2; width: %d%%; height: 100%%; border-radius: 4px;", group_pct))
                ),
                tags$small(style = "color: #666;", info$desc)
              )
            })
          )
        } else {
          p(style = "color: #666; font-style: italic;", "Feature importance will be calculated after prediction.")
        }
      ),
      
      # Model info
      div(style = "margin-top: 20px; padding: 15px; background: #e3f2fd; border-radius: 8px; font-size: 13px;",
        p(style = "margin: 0 0 5px 0;",
          icon("info-circle"),
          sprintf(" Model Score: %.4f | AUC: %.4f | Based on XGBoost prediction with all flight features", 
                 score, cancel_thresholds$model_auc)
        ),
        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #666;",
          sprintf("Tier: %s | Cancel Rate: %.1f%% | Thresholds: Top 1%%=%.3f, Top 3%%=%.3f, Top 10%%=%.3f",
                 tier, cancel_rate,
                 cancel_thresholds$top_1_pct, 
                 cancel_thresholds$top_3_pct, 
                 cancel_thresholds$top_10_pct)
        )
      )
    )
  })
  
  # ══════════════════════════════════════════════════════════════
  # CONNECTION INSURANCE UI
  # ══════════════════════════════════════════════════════════════
  
  output$connection_insurance_ui <- renderUI({
    if (!delay_model_loaded) {
      return(div(
        style = "padding: 20px; text-align: center;",
        icon("times-circle", style = "font-size: 48px; color: #dc3545;"),
        h4("Connection Insurance Unavailable"),
        p("The delay prediction model is not loaded. Please ensure model_delay_15min_final.rds and delay_connection_insurance_thresholds.rds are in the working directory.")
      ))
    }
    
    if (is.null(predictions$delay_model_score)) {
      return(div(
        style = "padding: 20px; text-align: center;",
        icon("info-circle", style = "font-size: 48px; color: #17a2b8;"),
        h4("Awaiting Flight Prediction"),
        p("Click 'Predict Flight Risk' to get your connection insurance recommendation.")
      ))
    }
    
    # Get the delay score
    score <- predictions$delay_model_score
    
    # Determine risk tier (13 tiers)
    tier <- if (score > delay_thresholds$top_1_pct) {
      tier_idx <- 1
      "Top 1%"
    } else if (score > delay_thresholds$top_2_pct) {
      tier_idx <- 2
      "Top 2%"
    } else if (score > delay_thresholds$top_5_pct) {
      tier_idx <- 3
      "Top 5%"
    } else if (score > delay_thresholds$top_10_pct) {
      tier_idx <- 4
      "Top 10%"
    } else if (score > delay_thresholds$top_20_pct) {
      tier_idx <- 5
      "Top 20%"
    } else if (score > delay_thresholds$top_30_pct) {
      tier_idx <- 6
      "Top 30%"
    } else if (score > delay_thresholds$top_40_pct) {
      tier_idx <- 7
      "Top 40%"
    } else if (score > delay_thresholds$top_50_pct) {
      tier_idx <- 8
      "Top 50%"
    } else if (score > delay_thresholds$top_60_pct) {
      tier_idx <- 9
      "Top 60%"
    } else if (score > delay_thresholds$top_70_pct) {
      tier_idx <- 10
      "Top 70%"
    } else if (score > delay_thresholds$top_80_pct) {
      tier_idx <- 11
      "Top 80%"
    } else if (score > delay_thresholds$top_90_pct) {
      tier_idx <- 12
      "Top 90%"
    } else {
      tier_idx <- 13
      "Below Average"
    }
    
    # Get connection time and ticket price from input
    conn_time <- input$connection_time  # in minutes
    ticket_price <- input$ticket_price  # in dollars
    
    # Find the closest connection time bucket
    connection_times <- delay_thresholds$connection_times
    bucket_idx <- which.min(abs(connection_times - conn_time))
    
    # Get miss rate for this tier and connection time
    tier_data <- delay_thresholds$miss_rates[[paste0("tier_", tier_idx)]]
    miss_rate <- tier_data$miss_rates_pct[bucket_idx]
    
    # Break-even calculation with actual ticket price
    insurance_cost_pct <- delay_thresholds$insurance_cost_pct
    break_even <- delay_thresholds$break_even_rate
    
    # Calculate actual dollar amounts
    insurance_cost_dollars <- (insurance_cost_pct / 100) * ticket_price
    expected_value_dollars <- ((miss_rate - insurance_cost_pct) / 100) * ticket_price
    max_fair_price_dollars <- (miss_rate / 100) * ticket_price
    
    # Recommendation logic
    recommend_buy <- miss_rate >= break_even
    
    # Determine color scheme and explanation
    if (recommend_buy) {
      banner_color  <- "#28a745"
      recommendation <- "Expected Value: Positive"
      explanation <- sprintf("Your estimated miss probability (%.1f%%) exceeds the break-even threshold (%.1f%%). Based on this model, insurance has positive expected value (+$%.2f on a $%.0f ticket). These results should be treated as a decision benchmark rather than a guarantee.",
                           miss_rate, break_even, expected_value_dollars, ticket_price)
    } else {
      banner_color  <- "#dc3545"
      recommendation <- "Expected Value: Negative"
      explanation <- sprintf("Your estimated miss probability (%.1f%%) is below the break-even threshold (%.1f%%). Based on this model, insurance has negative expected value at the standard premium. Insurance would be actuarially fair at a premium of $%.2f or less (%.1f%% of your $%.0f ticket).",
                           miss_rate, break_even, max_fair_price_dollars, miss_rate, ticket_price)
    }
    
    div(
      # Combined banner with risk tier
      div(
        style = sprintf("background: %s; color: white; padding: 20px; border-radius: 12px; margin-bottom: 20px; text-align: center;", banner_color),
        h3(style = "margin: 0 0 8px 0; font-weight: 700;", recommendation),
        h4(style = "margin: 0 0 6px 0; font-weight: 400;",
           sprintf("Risk Tier: %s | Miss Rate: %.1f%%", tier, miss_rate)),
        h4(style = "margin: 0; font-weight: 400; opacity: 0.9;",
           sprintf("Connection Time: %.1f hours | Break-Even Premium: $%.2f", conn_time / 60, max_fair_price_dollars))
      ),
      
      # Explanation
      div(
        style = "background: white; padding: 20px; border-radius: 12px; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
        h4(style = "margin-top: 0; color: #333;", icon("info-circle"), " Analysis"),
        p(style = "font-size: 15px; line-height: 1.6; color: #555;", explanation),
        
        hr(),
        
        h4(style = "color: #333;", icon("calculator"), " Economics"),
        p(style = "font-size: 14px; line-height: 1.6; color: #555;",
          sprintf("• Ticket Price: $%.0f", ticket_price),
          br(),
          sprintf("• Standard Premium: $%.2f (%.1f%% of ticket)", insurance_cost_dollars, insurance_cost_pct),
          br(),
          sprintf("• Risk Tier: %s", tier),
          br(),
          sprintf("• Miss Rate: %.1f%%", miss_rate),
          br(),
          sprintf("• Break-Even Threshold: %.1f%%", break_even),
          br(),
          sprintf("• Break-Even Premium: $%.2f (%.1f%% of ticket)", max_fair_price_dollars, miss_rate),
          br(),
          sprintf("• Net Expected Value: %s$%.2f", if(expected_value_dollars > 0) "+" else "", expected_value_dollars)
        )
      ),
      
      # Feature Importance - What Drives This Prediction
      div(
        style = "background: #f8f9fa; padding: 20px; border-radius: 12px; margin-bottom: 20px;",
        h4(style = "margin-top: 0; color: #333;", icon("lightbulb"), " What Drives This Prediction?"),
        p(style = "font-size: 14px; line-height: 1.6; color: #555; margin-bottom: 15px;",
          "Our model considers multiple factors when predicting your connection miss risk. Here's what matters most for YOUR specific flight:"
        ),
        
        # Dynamic SHAP-based feature importance (GROUPED)
        if(!is.null(predictions$delay_shap)) {
          # Feature names for delay model
          delay_feature_names <- c("Quarter", "Month", "DayofMonth", "DayOfWeek", "CRSDepTime", "CRSElapsedTime",
                                  "Reporting_Airline", "Origin", "Dest", "Distance", "OriginWac", "DestWac", "avg_fare",
                                  "origin_TEMP", "origin_DEWP", "origin_VISIB", "origin_WDSP", "origin_MXSPD",
                                  "origin_MAX", "origin_MIN", "origin_PRCP", "origin_SNDP",
                                  "dest_TEMP", "dest_DEWP", "dest_VISIB", "dest_WDSP", "dest_MXSPD",
                                  "dest_MAX", "dest_MIN", "dest_PRCP", "dest_SNDP")
          
          shap_result <- get_grouped_shap_percentages(predictions$delay_shap, delay_feature_names)
          
          # Map group names to user-friendly info
          group_info <- list(
            "Time & Schedule" = list(
              emoji = "⏰", 
              desc = sprintf("Your %s departure in %s", 
                           format(as.POSIXct(sprintf("2000-01-01 %s:00", input$time), format="%Y-%m-%d %H:%M"), "%I:%M %p"),
                           format(input$date, "%B"))
            ),
            "Route & Airports" = list(
              emoji = "✈️",
              desc = sprintf("%s → %s (%s miles)", input$origin, input$dest, format(round(input$distance), big.mark=","))
            ),
            "Airline" = list(
              emoji = "🛫",
              desc = sprintf("%s's on-time performance", input$airline)
            ),
            "Weather Conditions" = list(
              emoji = "🌤️",
              desc = sprintf("Typical %s weather at both airports", format(input$date, "%B"))
            ),
            "Flight Details" = list(
              emoji = "📋",
              desc = "Flight duration and pricing factors"
            )
          )
          
          # Display all 5 groups
          div(
            lapply(1:5, function(i) {
              group_name <- shap_result$groups[i]
              group_pct <- round(shap_result$percentages[i])
              
              info <- group_info[[group_name]]
              
              div(style = "margin-bottom: 12px;",
                div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                  span(style = "font-weight: 600; color: #333;", sprintf("%s %s", info$emoji, group_name)),
                  span(style = "color: #1976d2; font-weight: 600;", sprintf("%d%%", group_pct))
                ),
                div(style = "background: #e3f2fd; height: 8px; border-radius: 4px;",
                  div(style = sprintf("background: #1976d2; width: %d%%; height: 100%%; border-radius: 4px;", group_pct))
                ),
                tags$small(style = "color: #666;", info$desc)
              )
            })
          )
        } else {
          p(style = "color: #666; font-style: italic;", "Feature importance will be calculated after prediction.")
        }
      ),
      
      # Risk tier info
      div(
        style = "background: #f8f9fa; padding: 15px; border-radius: 8px; font-size: 12px; color: #666;",
        p(style = "margin: 0;",
          sprintf("🎯 Delay Risk Score: %.4f | AUC: %.4f | Based on XGBoost model with booking-time features", 
                 score, delay_thresholds$model_auc)
        ),
        p(style = "margin: 5px 0 0 0; font-size: 11px; color: #666;",
          sprintf("Tier: %s | Connection: %s | Sample: %d flights in this tier", 
                 tier, delay_thresholds$connection_labels[bucket_idx], tier_data$sample_size)
        )
      )
    )
  })
}

# ══════════════════════════════════════════════════════════════
# RUN APP
# ══════════════════════════════════════════════════════════════

shinyApp(ui = ui, server = server)
