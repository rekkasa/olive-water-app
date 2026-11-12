# Single-file R Shiny application. Save as app.R.
# Install once: install.packages(c("shiny", "httr", "jsonlite"))
# Run: shiny::runApp("app.R")
# Model: V100 = ET0 * Kc * 2 * canopy_area, for sparse young olive trees.
# This is an estimate of replacement demand, not a soil-water balance.

required <- c("shiny", "httr", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing packages: ", paste(missing, collapse = ", "))
library(shiny)

today_at <- function(tz) as.Date(format(Sys.time(), tz = tz, format = "%Y-%m-%d"))

stop_weather_gaps <- function(issues) {
  stop(structure(list(message = paste0("Calculation blocked: ", nrow(issues),
    " missing or invalid weather entries. See the dates and fields below. No values were replaced with zero."),
    call = NULL, issues = issues), class = c("weather_data_error", "error", "condition")))
}

validate_daily <- function(d, expected) {
  issues <- data.frame(Date = character(), Field = character(), Problem = character())
  add <- function(dates, field, problem) {
    if (length(dates)) issues <<- rbind(issues, data.frame(
      Date = as.character(dates), Field = field, Problem = problem))
  }
  raw_dates <- if (is.null(d$time)) character() else as.character(d$time)
  dates <- suppressWarnings(as.Date(raw_dates, format = "%Y-%m-%d"))
  invalid_date_rows <- which(is.na(dates))
  # In R, paste0("Response row ", integer(0)) yields "Response row ",
  # not character(0). Guard before formatting to avoid a phantom issue.
  if (length(invalid_date_rows) > 0L)
    add(paste0("Response row ", invalid_date_rows), "Date", "Missing or invalid date")
  duplicate <- !is.na(dates) & (duplicated(dates) | duplicated(dates, fromLast = TRUE))
  add(unique(raw_dates[duplicate]), "Date", "Duplicate date")
  add(raw_dates[!is.na(dates) & !dates %in% expected], "Date", "Outside requested period")
  idx <- match(as.character(expected), as.character(dates))
  add(expected[is.na(idx)], "Date / both weather fields", "Entire daily record missing")
  read_field <- function(key, label) {
    raw <- d[[key]]
    if (is.null(raw)) {
      add(expected[!is.na(idx)], label, "Field absent from response")
      return(rep(NA_real_, length(expected)))
    }
    if (length(raw) != length(dates)) {
      add(expected[!is.na(idx)], label, "Value count differs from date count; alignment cannot be trusted")
      return(rep(NA_real_, length(expected)))
    }
    # Preserve JSON null positions even if jsonlite returns a list.
    values <- vapply(seq_along(raw), function(i) {
      x <- raw[[i]]
      if (length(x) != 1 || is.null(x)) return(NA_real_)
      suppressWarnings(as.numeric(x))
    }, numeric(1))
    out <- values[idx]
    add(expected[!is.na(idx) & !is.finite(out)], label, "Missing/null or non-finite value")
    add(expected[!is.na(idx) & is.finite(out) & out < 0], label, "Invalid negative value")
    out
  }
  et <- read_field("et0_fao_evapotranspiration", "ET0 (mm)")
  rain <- read_field("precipitation_sum", "Precipitation (mm)")
  if (nrow(issues)) stop_weather_gaps(issues)
  data.frame(date = expected, et0_mm = et, precipitation_mm = rain)
}

fetch_segment <- function(start, end, lat, lon, tz, archive, log = message) {
  endpoint <- if (archive) "https://archive-api.open-meteo.com/v1/archive" else
    "https://api.open-meteo.com/v1/forecast"
  query <- list(latitude = lat, longitude = lon, start_date = as.character(start),
                end_date = as.character(end), timezone = tz,
                daily = "et0_fao_evapotranspiration,precipitation_sum",
                precipitation_unit = "mm")
  url <- httr::modify_url(endpoint, query = query)
  log(paste("REQUEST", url))
  response <- NULL
  for (attempt in seq_len(2L)) {
    log(sprintf("Attempt %d/2: connecting (8 s connection limit, 20 s total request limit).", attempt))
    began <- Sys.time()
    response <- tryCatch(
      httr::GET(url, httr::timeout(20), httr::config(connecttimeout = 8),
                httr::user_agent("YoungOliveShiny/1.1")),
      error = function(e) e)
    elapsed <- as.numeric(difftime(Sys.time(), began, units = "secs"))
    if (inherits(response, "error")) {
      log(sprintf("NETWORK ERROR after %.1f s: %s", elapsed, conditionMessage(response)))
      if (attempt == 2L) stop("Could not reach Open-Meteo: ", conditionMessage(response),
        ". See Diagnostics / R console. Try opening the logged URL in your browser; check network, proxy and certificate settings.")
    } else {
      status <- httr::status_code(response)
      log(sprintf("HTTP %d received after %.1f s; %d response bytes.", status, elapsed, length(response$content)))
      if (!(status %in% c(408L, 429L) || status >= 500L) || attempt == 2L) break
      if (status == 429L) {
        log("Rate limited (429). No immediate retry; wait before calculating again.")
        stop("Open-Meteo rate limit (HTTP 429). Wait and retry later.")
      }
    }
    log("Transient failure. Retrying in 1 second.")
    Sys.sleep(1)
  }
  log("Reading and parsing JSON response.")
  body <- httr::content(response, as = "text", encoding = "UTF-8")
  obj <- tryCatch(jsonlite::fromJSON(body), error = function(e) NULL)
  if (httr::status_code(response) >= 400 || isTRUE(obj$error)) {
    reason <- if (!is.null(obj$reason)) obj$reason else paste("HTTP", httr::status_code(response))
    log(paste("API ERROR:", reason))
    log(paste("Response excerpt:", substr(gsub("[\r\n]+", " ", body), 1, 500)))
    stop("Open-Meteo: ", reason)
  }
  if (is.null(obj)) {
    log(paste("Non-JSON response excerpt:", substr(gsub("[\r\n]+", " ", body), 1, 500)))
    stop("Open-Meteo returned unreadable data; no calculation performed.")
  }
  expected <- seq(start, end, by = "day")
  log(sprintf("Validating %d expected days (%s to %s).", length(expected), start, end))
  d <- validate_daily(obj$daily, expected)
  if (!identical(obj$daily_units$et0_fao_evapotranspiration, "mm") ||
      !identical(obj$daily_units$precipitation_sum, "mm")) stop("Unexpected API units.")
  log(sprintf("Validated %d days. ET0 %.2f mm; precipitation %.2f mm.", nrow(d), sum(d$et0_mm), sum(d$precipitation_mm)))
  data.frame(d,
             source = if (archive) "Historical reanalysis" else "Recent/forecast model",
             grid_latitude = obj$latitude, grid_longitude = obj$longitude,
             grid_elevation_m = obj$elevation,
             request_url = response$url, stringsAsFactors = FALSE)
}

fetch_weather <- function(start, end, lat, lon, tz, log = message) {
  # The main Forecast API supports the past 92 days. Avoid requiring the
  # separate archive host for recent periods (including last week).
  # Dates >= today - 92 use the main API; older dates use the archive.
  cutoff <- today_at(tz) - 93
  log(paste("Historical/recent boundary:", cutoff, "Timezone:", tz))
  parts <- list()
  issues <- list()
  get_part <- function(a, b, archive) {
    log(paste(if (archive) "Historical segment:" else "Recent/forecast segment:", a, "through", b))
    tryCatch(fetch_segment(a, b, lat, lon, tz, archive, log), weather_data_error = function(e) {
      log(conditionMessage(e))
      for (i in seq_len(nrow(e$issues))) log(paste(e$issues[i, ], collapse = " | "))
      issues[[length(issues) + 1L]] <<- e$issues
      NULL
    })
  }
  if (start <= cutoff) parts[[length(parts) + 1L]] <-
    get_part(start, min(end, cutoff), TRUE)
  if (end > cutoff) parts[[length(parts) + 1L]] <-
    get_part(max(start, cutoff + 1), end, FALSE)
  if (length(issues)) stop_weather_gaps(do.call(rbind, issues))
  do.call(rbind, parts)
}

calculate <- function(d, p) {
  area <- pi * (p$d1 / 100) * (p$d2 / 100) / 4
  d$full_use_L <- d$et0_mm * p$kc * 2 * area
  d$target_net_L <- d$full_use_L * p$target / 100
  d$no_rain_gross_L <- d$target_net_L / (p$eff / 100)
  # Optional sensitivity scenario: usable rainfall must be independently assumed.
  # Current/future precipitation is never credited as if already received.
  d$assumed_rain_credit_L <- if (p$rain_mode == "scenario")
    ifelse(d$date < p$today, d$precipitation_mm * p$root_area * p$retention / 100, 0) else 0
  net <- sum(d$target_net_L)
  credit <- min(net, sum(d$assumed_rain_credit_L))
  list(data = d, inputs = p, canopy = area, full = sum(d$full_use_L), net = net,
       gross = sum(d$no_rain_gross_L), credit = credit,
       scenario = max(0, net - credit) / (p$eff / 100), fetched = Sys.time())
}

ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f6f7f2;color:#24352c} .container-fluid{max-width:1300px} .well{background:white;border:1px solid #dde4db;border-radius:12px} h1,h3{color:#34583f} .btn-primary{background:#34583f;border-color:#34583f} .metric{background:white;padding:18px;border-radius:12px;border:1px solid #dde4db;margin:12px 0}.metric strong{font-size:28px;display:block}.note{padding:14px;background:#fff4d9;border-radius:8px;margin:12px 0} table{background:white}"))),
  titlePanel("Young olive irrigation calculator"),
  p("Estimate water replacement from canopy size and Open-Meteo weather. Defaults match your 103-tree orchard."),
  sidebarLayout(
    sidebarPanel(width = 4,
      h4("Canopy and location"),
      fluidRow(column(6, numericInput("d1", "D1 width (cm)", 40, min = 1)),
               column(6, numericInput("d2", "D2 width (cm)", 40, min = 1))),
      fluidRow(column(6, numericInput("lat", "Latitude", 40.39696, min = -90, max = 90, step = 0.00001)),
               column(6, numericInput("lon", "Longitude", 23.13693, min = -180, max = 180, step = 0.00001))),
      selectInput("tz", "Timezone", choices = c("Europe/Athens", "UTC", OlsonNames()), selected = "Europe/Athens"),
      dateRangeInput("period", "Period (inclusive)", start = Sys.Date() - 7, end = Sys.Date() - 1),
      sliderInput("target", "ETc replacement target (%)", min = 0, max = 100, value = 80),
      numericInput("kc", "Crop coefficient Kc", 0.65, min = 0.01, max = 1.5, step = 0.01),
      numericInput("eff", "Application efficiency (%)", 90, min = 1, max = 100),
      numericInput("trees", "Number of trees", 103, min = 1, step = 1),
      tags$details(tags$summary("Tank and cost"),
        numericInput("tank", "Tank volume (m³)", 10, min = 0.1),
        numericInput("cost", "Cost per full tank (€)", 290, min = 0)),
      tags$hr(),
      selectInput("rain_mode", "Rainfall treatment", c("Show rain; do not assume a credit" = "none",
                     "Explore an assumed rainfall credit" = "scenario")),
      conditionalPanel("input.rain_mode == 'scenario'",
        numericInput("root_area", "Effective rooted footprint (m²/tree)", 0.5, min = 0.01),
        sliderInput("retention", "Assumed usable fraction of precipitation (%)", 0, 100, 50),
        helpText("These are scenario assumptions, not measured values. Rooted area is not canopy area. Precipitation includes snow; only credit liquid water actually available to roots.")),
      actionButton("go", "Fetch weather & calculate", class = "btn-primary"),
      p("Coordinates and dates are sent to Open-Meteo when you click Calculate.", style = "margin-top:12px;font-size:12px")
    ),
    mainPanel(width = 8,
      uiOutput("status"),
      tableOutput("data_issues"),
      uiOutput("results"),
      tabsetPanel(
        tabPanel("Calculation", tableOutput("steps"), uiOutput("rain_result")),
        tabPanel("Daily weather", plotOutput("weather_plot", height = "300px"), tableOutput("daily")),
        tabPanel("7-day blocks", p("Consecutive blocks starting on your selected start date; final blocks may be shorter."), tableOutput("weekly")),
        tabPanel("Method & sources",
          p("Canopy area Ac = pi × (D1/100) × (D2/100) / 4, in m²."),
          p("The sparse-canopy approximation Kr = 2fc gives V100 = ET0 × Kc × 2Ac in litres/tree. Allocated field area cancels. It is intended for young, widely spaced trees, not closed canopies or general mature-orchard scheduling."),
          p("Target net replacement = V100 × target/100. Delivered volume = net replacement / (efficiency/100). Kc = 0.65 is an editable working assumption, not a calibration for your field. Choosing 80% does not establish that this deficit is safe for establishment."),
          p("Rain scenario: credit = precipitation × rooted area × usable fraction. The period credit is capped at the target demand. This sensitivity calculation ignores soil storage, drainage dynamics, starting moisture and rain timing. Late rain cannot retrospectively prevent early stress. No current-day or future rainfall is credited."),
          p("Daily ET0 and precipitation are model estimates, not field observations. Historical reanalysis and recent/forecast data can differ from an earlier CSV download. Dates use the selected timezone. The previous 92 days, today and future dates use the main Forecast API; earlier dates use the Historical Weather API. This avoids contacting the archive host unnecessarily for recent periods."),
          p("Weekly replacement volume is not a watering interval. Check root-ball and surrounding soil moisture. Weekend-only application is suitable only if the rooted soil stores enough usable water until the next visit."),
          tags$a("UC Davis: young-orchard irrigation, Chapter 6", href = "https://ucanr.edu/sites/default/files/2025-08/Young%20Orchard%20Irrigation%20Nutrient%20Handbbok%20English%20Version%20corrected%20.pdf", target = "_blank"), tags$br(),
          tags$a("Open-Meteo Forecast API / usage terms", href = "https://open-meteo.com/en/docs", target = "_blank"), tags$br(),
          tags$a("Open-Meteo Historical Weather API", href = "https://open-meteo.com/en/docs/historical-weather-api", target = "_blank"),
          p("Weather attribution: Open-Meteo.com. Check its terms for commercial use. Dependencies: shiny, httr, jsonlite.")
        ),
        tabPanel("Diagnostics",
          p("Live messages are printed in the R/RStudio console. This panel updates when the request completes or fails. Each endpoint has at most two 20-second attempts; some periods use two endpoints."),
          verbatimTextOutput("diagnostics"),
          downloadButton("download_log", "Download diagnostic log"))
      ),
      downloadButton("download", "Download daily results CSV")
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  error <- reactiveVal(NULL)
  data_issues <- reactiveVal(NULL)
  diagnostics <- reactiveVal(character())
  started <- NULL
  log_event <- function(text) {
    elapsed <- if (is.null(started)) 0 else as.numeric(difftime(Sys.time(), started, units="secs"))
    line <- sprintf("[%s +%.1fs] %s", format(Sys.time(), "%H:%M:%S"), elapsed, text)
    diagnostics(c(isolate(diagnostics()), line))
    message(line)
    flush.console()
  }
  output$diagnostics <- renderText({
    lines <- diagnostics()
    if (!length(lines)) "No request yet." else paste(lines, collapse="\n")
  })
  output$download_log <- downloadHandler(
    filename=function() "olive-diagnostics.txt",
    content=function(file) writeLines(isolate(diagnostics()), file))
  parameters <- reactive(list(d1=input$d1,d2=input$d2,lat=input$lat,lon=input$lon,
    tz=input$tz,period=input$period,target=input$target,kc=input$kc,eff=input$eff,
    trees=input$trees,tank=input$tank,cost=input$cost,rain_mode=input$rain_mode,
    root_area=input$root_area,retention=input$retention))

  observeEvent(input$go, {
    result(NULL); error(NULL); data_issues(NULL)
    diagnostics(character()); started <<- Sys.time()
    log_event(paste("Starting calculation. R", getRversion(), "httr", packageVersion("httr"),
      "jsonlite", packageVersion("jsonlite")))
    tryCatch({
      p <- parameters()
      labels <- c(d1="D1 canopy width",d2="D2 canopy width",lat="Latitude",lon="Longitude",
        target="ETc target",kc="Crop coefficient",eff="Application efficiency",trees="Number of trees",
        tank="Tank volume",cost="Tank cost",root_area="Rooted footprint",retention="Rain retention")
      absent <- names(labels)[vapply(names(labels), function(n)
        length(p[[n]]) != 1 || !is.finite(p[[n]]), logical(1))]
      if (length(absent)) stop("Missing or invalid input: ", paste(labels[absent], collapse=", "))
      if (p$d1 <= 0 || p$d2 <= 0 || abs(p$lat)>90 || abs(p$lon)>180 ||
          p$target<0 || p$target>100 || p$kc<=0 || p$kc>1.5 || p$eff<=0 || p$eff>100 ||
          p$trees<1 || p$trees!=floor(p$trees) || p$tank<=0 || p$cost<0 ||
          p$root_area<=0 || p$retention<0 || p$retention>100) stop("Check the input ranges.")
      if (!p$tz %in% c("UTC", OlsonNames())) stop("Select a valid timezone.")
      p$today <- today_at(p$tz)
      dates <- as.Date(p$period)
      if (length(dates)!=2 || anyNA(dates) || dates[1]>dates[2]) stop("Choose a valid date range.")
      if (dates[1]<as.Date("1940-01-01") || dates[2]>p$today+15 || dates[2]-dates[1]>365)
        stop("Choose at most 366 days, from 1940 through 15 days ahead.")
      withProgress(message = "Retrieving daily weather", value = 0.2, {
        progress_log <- function(text) {
          log_event(text)
          setProgress(detail = substr(text, 1, 180))
        }
        d <- fetch_weather(dates[1], dates[2], p$lat, p$lon, p$tz, progress_log)
        incProgress(0.7)
        progress_log("Weather complete. Calculating replacement volumes.")
        result(calculate(d, p))
        log_event("SUCCESS: calculation complete.")
      })
    }, error = function(e) {
      log_event(paste("FAILED:", conditionMessage(e)))
      if (inherits(e, "weather_data_error")) data_issues(e$issues)
      error(conditionMessage(e))
    })
  })

  output$data_issues <- renderTable({ req(data_issues()); data_issues() }, striped=TRUE, bordered=TRUE)

  output$status <- renderUI({
    if (!is.null(error())) return(div(class="note", error()))
    r <- result()
    if (is.null(r)) return(p("Choose inputs, then click Fetch weather & calculate."))
    saved <- r$inputs; saved$today <- NULL
    if (!identical(saved, parameters())) return(div(class="note", "Inputs changed. Results below still use the last submitted inputs. Click Calculate to refresh."))
    p(paste("Calculated", format(r$fetched, tz=r$inputs$tz), "—", r$inputs$tz))
  })
  output$results <- renderUI({
    r <- result(); req(r)
    scenario_active <- r$inputs$rain_mode == "scenario"
    cost_volume <- if (scenario_active) r$scenario else r$gross
    total_cost <- cost_volume * r$inputs$trees / 1000 * r$inputs$cost / r$inputs$tank
    cost_basis <- if (scenario_active) "After assumed rainfall credit" else "Before rainfall credit"
    metric <- function(label, value, note=NULL) div(class="metric", span(label), strong(value), tags$small(note))
    tagList(fluidRow(
      column(6,metric("Period ET₀",sprintf("%.2f mm",sum(r$data$et0_mm)))),
      column(6,metric("Period precipitation",sprintf("%.2f mm",sum(r$data$precipitation_mm))))),
      fluidRow(
        column(6,metric("Before rain credit",sprintf("%.2f L/tree",r$gross))),
        column(6,metric("Total water cost",sprintf("€%.2f",total_cost),
          paste(cost_basis, "— all trees, selected period. Value of water used, not the cost of purchasing whole tank loads.")))),
      div(class="note", "This is replacement demand, not a confirmed watering prescription. No initial soil deficit is included. Rainfall credit is optional and assumption-dependent."),
      p(sprintf("%s through %s • %d days • %d trees • %.1f litres total before rain credit (%.2f%% of tank; €%.2f water used).",
        min(r$data$date),max(r$data$date),nrow(r$data),r$inputs$trees,r$gross*r$inputs$trees,
        100*r$gross*r$inputs$trees/(1000*r$inputs$tank),r$gross*r$inputs$trees/1000*r$inputs$cost/r$inputs$tank)))
  })
  output$steps <- renderTable({
    r<-result(); req(r); p<-r$inputs
    data.frame(Step=c("Canopy area (m²)","Full estimated use (L/tree)","Target net replacement (L/tree)","Delivered before rainfall credit (L/tree)"),
      Calculation=c(sprintf("pi × %.2f × %.2f / 4",p$d1/100,p$d2/100),
        sprintf("%.2f × %.2f × 2 × %.6f",sum(r$data$et0_mm),p$kc,r$canopy),
        sprintf("%.4f × %.1f%%",r$full,p$target),sprintf("%.4f / %.3f",r$net,p$eff/100)),
      Result=round(c(r$canopy,r$full,r$net,r$gross),4),check.names=FALSE)
  }, striped=TRUE, bordered=FALSE, spacing="m")
  output$rain_result <- renderUI({
    r<-result(); req(r)
    if(r$inputs$rain_mode=="none") return(p("Precipitation is shown but not subtracted. Enable the rainfall scenario only if you can supply defensible root-area and retention assumptions."))
    div(class="note",sprintf("Assumed rain credit: %.2f L/tree. Period-total scenario: %.2f L/tree delivered, %.1f L for the orchard. This is not a dated watering schedule. Forecast/current-day rain excluded.",r$credit,r$scenario,r$scenario*r$inputs$trees))
  })
  output$daily <- renderTable({
    r<-result();req(r);d<-r$data
    data.frame(Date=as.character(d$date),ET0_mm=d$et0_mm,Precipitation_mm=d$precipitation_mm,
      Delivered_before_rain_L=round(d$no_rain_gross_L,3),Source=d$source)
  },digits=3,striped=TRUE)
  output$weekly <- renderTable({
    r<-result();req(r);d<-r$data
    groups<-split(seq_len(nrow(d)),(seq_len(nrow(d))-1)%/%7)
    do.call(rbind,lapply(groups,function(i) data.frame(Start=as.character(min(d$date[i])),
      End=as.character(max(d$date[i])),Days=length(i),ET0_mm=sum(d$et0_mm[i]),
      Precipitation_mm=sum(d$precipitation_mm[i]),Litres_per_tree_before_rain=sum(d$no_rain_gross_L[i]))))
  },digits=2,striped=TRUE)
  output$weather_plot <- renderPlot({
    r<-result();req(r);d<-r$data
    # Numeric dates avoid locale-dependent month names and mojibake in base graphics.
    x <- as.numeric(d$date)
    width <- session$clientData$output_weather_plot_width
    max_ticks <- if (is.null(width) || !is.finite(width)) 5L else max(2L, min(7L, floor(width/95)))
    ticks <- unique(as.integer(round(seq(1, nrow(d), length.out=min(nrow(d),max_ticks)))))
    old_par <- par(mar=c(4.5,4,2.8,1),family="sans",las=1)
    on.exit(par(old_par),add=TRUE)
    upper <- max(c(d$et0_mm,d$precipitation_mm,0.1)) * 1.18
    matplot(x,cbind(d$et0_mm,d$precipitation_mm),type="o",pch=c(16,1),lty=1,lwd=1.6,
      col=c("#3a6848","#3476a8"),xlab="Date (YYYY-MM-DD)",ylab="mm/day",
      xaxt="n",ylim=c(0,upper),cex.axis=0.85)
    axis(1,at=x[ticks],labels=format(d$date[ticks],"%Y-%m-%d"),cex.axis=0.8)
    legend("top",inset=c(0,-0.17),xpd=NA,horiz=TRUE,
      legend=expression(ET[0],Precipitation),col=c("#3a6848","#3476a8"),
      lty=1,lwd=1.6,pch=c(16,1),bty="n",cex=0.9)
  })
  output$download <- downloadHandler(
    filename=function(){r<-result();req(r);paste0("olive-water-",min(r$data$date),"-",max(r$data$date),".csv")},
    content=function(file){
      r<-result();req(r);d<-r$data;p<-r$inputs
      for(n in c("d1","d2","lat","lon","tz","target","kc","eff","trees","rain_mode","root_area","retention")) d[[paste0("input_",n)]]<-p[[n]]
      d$fetched_at<-format(r$fetched,tz=p$tz)
      d$period_scenario_gross_L_per_tree<-if(p$rain_mode=="scenario") r$scenario else NA_real_
      write.csv(d,file,row.names=FALSE,na="")
    })
}

# Offline arithmetic checks (executed when the app is loaded).
stopifnot(abs(25.16 * 0.65 * 2 * pi * 0.4 * 0.4 / 4 * 0.8 / 0.9 - 3.653518667) < 1e-6)
# Data-quality regression checks: real zero rainfall is valid; missing data is not.
local({
  days <- as.Date(c("2026-09-15", "2026-09-16"))
  good <- list(time=as.character(days), et0_fao_evapotranspiration=c(2,3), precipitation_sum=c(0,1))
  stopifnot(nrow(validate_daily(good, days)) == 2)
  bad <- good; bad$time[2] <- NA_character_
  issues <- tryCatch(validate_daily(bad, days), weather_data_error=function(e) e$issues)
  stopifnot(sum(issues$Problem == "Missing or invalid date") == 1,
            any(issues$Date == "Response row 2"))
  bad <- good; bad$et0_fao_evapotranspiration <- list(2, NULL)
  issues <- tryCatch(validate_daily(bad, days), weather_data_error=function(e) e$issues)
  stopifnot(any(issues$Date == "2026-09-16" & issues$Field == "ET0 (mm)"))
  bad <- good; bad$precipitation_sum <- NULL
  issues <- tryCatch(validate_daily(bad, days), weather_data_error=function(e) e$issues)
  stopifnot(nrow(issues) == 2, all(issues$Field == "Precipitation (mm)"))
  bad <- lapply(good, function(x) x[1])
  issues <- tryCatch(validate_daily(bad, days), weather_data_error=function(e) e$issues)
  stopifnot(any(issues$Date == "2026-09-16" & issues$Problem == "Entire daily record missing"))
})
shinyApp(ui, server)
