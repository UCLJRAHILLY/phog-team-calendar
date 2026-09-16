# ============================================================
# PHOG presentation-slot scheduler
# ============================================================
# Local testing: uses SQLite automatically.
# Shared deployment: set PHOG_DB_* environment variables to use PostgreSQL.
# ============================================================

library(shiny)
library(toastui)
library(DBI)
library(RSQLite)
library(RPostgres)
library(htmlwidgets)

# ------------------------------------------------------------
# 1. Meeting dates
# ------------------------------------------------------------

SCHEDULE_END <- as.Date("2027-12-31")

team_dates <- seq.Date(
  from = as.Date("2026-09-30"),
  to   = SCHEDULE_END,
  by   = "4 weeks"
)

bitesize_dates <- sort(unique(c(
  seq.Date(
    from = as.Date("2026-10-14"),
    to   = SCHEDULE_END,
    by   = "4 weeks"
  ),
  as.Date("2026-10-21")   # additional PHOG Bitesize meeting
)))

SLOTS <- rbind(
  data.frame(
    slot_id = paste0("team_", format(team_dates, "%Y-%m-%d")),
    meeting_type = "PHOG Team",
    date = team_dates,
    duration_minutes = 30L,
    calendar_id = "team",
    stringsAsFactors = FALSE
  ),
  data.frame(
    slot_id = paste0("bitesize_", format(bitesize_dates, "%Y-%m-%d")),
    meeting_type = "PHOG Bitesize",
    date = bitesize_dates,
    duration_minutes = 60L,
    calendar_id = "bitesize",
    stringsAsFactors = FALSE
  )
)

SLOTS <- SLOTS[order(SLOTS$date), ]
row.names(SLOTS) <- NULL

# ------------------------------------------------------------
# 2. Database helpers
# ------------------------------------------------------------

use_postgres <- function() {
  nzchar(Sys.getenv("PHOG_DB_HOST"))
}

connect_db <- function() {
  if (use_postgres()) {
    DBI::dbConnect(
      RPostgres::Postgres(),
      host     = Sys.getenv("PHOG_DB_HOST"),
      port     = as.integer(Sys.getenv("PHOG_DB_PORT", "5432")),
      dbname   = Sys.getenv("PHOG_DB_NAME"),
      user     = Sys.getenv("PHOG_DB_USER"),
      password = Sys.getenv("PHOG_DB_PASSWORD"),
      sslmode  = Sys.getenv("PHOG_DB_SSLMODE", "require")
    )
  } else {
    DBI::dbConnect(
      RSQLite::SQLite(),
      "phog_bookings.sqlite"
    )
  }
}

with_db <- function(fun) {
  con <- connect_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fun(con)
}

init_db <- function() {
  with_db(function(con) {
    DBI::dbExecute(
      con,
      paste(
        "CREATE TABLE IF NOT EXISTS bookings (",
        "slot_id TEXT PRIMARY KEY,",
        "presenter TEXT NOT NULL,",
        "talk_title TEXT,",
        "booked_at TEXT NOT NULL",
        ")"
      )
    )
  })
}

read_bookings <- function() {
  with_db(function(con) {
    DBI::dbGetQuery(
      con,
      "SELECT slot_id, presenter, talk_title, booked_at FROM bookings ORDER BY slot_id"
    )
  })
}

booking_version <- function() {
  with_db(function(con) {
    x <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) AS n,",
        "COALESCE(MAX(booked_at), '') AS latest",
        "FROM bookings"
      )
    )
    paste(x$n[[1]], x$latest[[1]], sep = "|")
  })
}

book_slot <- function(slot_id, presenter, talk_title) {
  with_db(function(con) {
    sql <- DBI::sqlInterpolate(
      con,
      paste(
        "INSERT INTO bookings (slot_id, presenter, talk_title, booked_at)",
        "VALUES (?slot_id, ?presenter, ?talk_title, ?booked_at)",
        "ON CONFLICT (slot_id) DO NOTHING"
      ),
      slot_id    = slot_id,
      presenter  = presenter,
      talk_title = talk_title,
      booked_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    )
    
    DBI::dbExecute(con, sql)
  })
}

init_db()

# ------------------------------------------------------------
# 3. Convert slots + bookings into calendar events
# ------------------------------------------------------------

make_calendar_events <- function(bookings) {
  x <- merge(SLOTS, bookings, by = "slot_id", all.x = TRUE, sort = FALSE)
  x <- x[match(SLOTS$slot_id, x$slot_id), ]
  
  booked <- !is.na(x$presenter) & nzchar(x$presenter)
  
  x$title <- ifelse(
    booked,
    paste0(x$meeting_type, " | BOOKED: ", x$presenter),
    paste0(x$meeting_type, " | AVAILABLE (", x$duration_minutes, " min)")
  )
  
  x$body <- ifelse(
    booked,
    paste0(
      "Presenter: ", x$presenter,
      ifelse(
        is.na(x$talk_title) | !nzchar(x$talk_title),
        "",
        paste0("\nPresentation: ", x$talk_title)
      )
    ),
    paste0("Available presentation slot: ", x$duration_minutes, " minutes")
  )
  
  data.frame(
    id         = x$slot_id,
    calendarId = x$calendar_id,
    title      = x$title,
    body       = x$body,
    start = format(x$date, "%Y-%m-%d"),
    end   = format(x$date, "%Y-%m-%d"),
    category = "allday",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 4. User interface
# ------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { max-width: 1250px; margin: 0 auto; padding: 20px; }\n      .legend-row { display: flex; gap: 24px; margin: 12px 0 18px 0; flex-wrap: wrap; }\n      .legend-item { display: flex; align-items: center; gap: 8px; }\n      .legend-box { width: 18px; height: 18px; border-radius: 4px; }\n      .help-text { color: #555; margin-bottom: 10px; }\n\n      /* Keep calendar event labels inside their coloured event boxes. */\n      .tui-full-calendar-weekday-schedule {\n        height: auto !important;\n        min-height: 34px !important;\n        overflow: hidden !important;\n      }\n\n      .tui-full-calendar-weekday-schedule-title {\n        white-space: normal !important;\n        overflow-wrap: anywhere !important;\n        word-break: break-word !important;\n        line-height: 1.15 !important;\n        padding-top: 3px !important;\n        padding-bottom: 3px !important;\n        display: -webkit-box !important;\n        -webkit-box-orient: vertical;\n        -webkit-line-clamp: 2;\n        overflow: hidden !important;\n        text-overflow: clip !important;\n      }\n    "))
  ),
  
  titlePanel("PHOG Meeting Scheduler"),
  
  tags$p(
    class = "help-text",
    "Click an available meeting to book the presentation slot. ",
    "PHOG Team slots are 30 minutes; PHOG Bitesize slots are 60 minutes."
  ),
  
  tags$div(
    class = "legend-row",
    tags$div(
      class = "legend-item",
      tags$span(class = "legend-box", style = "background:rgba(125, 185, 235, 0.68); border:1px solid #7DB9EB;"),
      tags$span("PHOG Team")
    ),
    tags$div(
      class = "legend-item",
      tags$span(class = "legend-box", style = "background:rgba(157, 216, 166, 0.68); border:1px solid #9DD8A6;"),
      tags$span("PHOG Bitesize")
    )
  ),
  
  calendarOutput("phog_calendar", height = "720px")
)

# ------------------------------------------------------------
# 5. Server logic
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Poll the shared database so bookings made by colleagues appear automatically.
  bookings <- reactivePoll(
    intervalMillis = 3000,
    session = session,
    checkFunc = booking_version,
    valueFunc = read_bookings
  )
  
  # Remember the month the user is currently viewing. This prevents a database
  # refresh from throwing them back to September 2026.
  visible_date <- reactiveVal(as.Date("2026-09-30"))
  
  observeEvent(input$phog_calendar_dates, {
    d <- input$phog_calendar_dates
    req(d$start, d$end)
    
    start_date <- as.Date(substr(d$start, 1, 10))
    end_date   <- as.Date(substr(d$end, 1, 10))
    
    visible_date(
      start_date + floor(as.numeric(end_date - start_date) / 2)
    )
  }, ignoreInit = TRUE)
  
  output$phog_calendar <- renderCalendar({
    events <- make_calendar_events(bookings())
    
    calendar(
      view = "month",
      defaultDate = isolate(visible_date()),
      navigation = TRUE,
      navOpts = navigation_options(
        fmt_date = "DD/MM/YYYY",
        sep_date = " - "
      ),
      useDetailPopup = FALSE,
      useCreationPopup = FALSE,
      isReadOnly = TRUE
    ) |>
      cal_props(
        list(
          id = "team",
          name = "PHOG Team",
          color = "ivory4",
          backgroundColor = "rgba(125, 185, 235, 0.68)",
          borderColor = "#7DB9EB"
        ),
        list(
          id = "bitesize",
          name = "PHOG Bitesize",
          color = "ivory4",
          backgroundColor = "rgba(157, 216, 166, 0.68)",
          borderColor = "#9DD8A6"
        )
      ) |>
      cal_schedules(events) |>
      cal_events(
        clickSchedule = htmlwidgets::JS(
          "function(event) {",
          "  Shiny.setInputValue(",
          "    'slot_click',",
          "    {id: event.event.id, nonce: Math.random()},",
          "    {priority: 'event'}",
          "  );",
          "}"
        )
      )
  })
  
  selected_slot <- reactiveVal(NULL)
  
  observeEvent(input$slot_click, {
    slot_id <- as.character(input$slot_click$id)
    slot <- SLOTS[SLOTS$slot_id == slot_id, , drop = FALSE]
    req(nrow(slot) == 1)
    
    current <- read_bookings()
    current_booking <- current[current$slot_id == slot_id, , drop = FALSE]
    
    if (nrow(current_booking) == 1) {
      showModal(
        modalDialog(
          title = paste0(slot$meeting_type, " — ", format(slot$date, "%d/%m/%Y")),
          tags$p(tags$b("This slot is already booked.")),
          tags$p("Presenter: ", current_booking$presenter),
          if (!is.na(current_booking$talk_title) && nzchar(current_booking$talk_title))
            tags$p("Presentation: ", current_booking$talk_title),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }
    
    selected_slot(slot_id)
    
    showModal(
      modalDialog(
        title = paste0(
          "Book ", slot$meeting_type,
          " — ", format(slot$date, "%d/%m/%Y"),
          " (", slot$duration_minutes, " min)"
        ),
        textInput(
          "presenter_name",
          "Your name",
          placeholder = "e.g. Jane Smith"
        ),
        textInput(
          "presentation_title",
          "Presentation title / topic",
          placeholder = "Optional"
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "confirm_booking",
            "Book this slot",
            class = "btn-primary"
          )
        )
      )
    )
  })
  
  observeEvent(input$confirm_booking, {
    req(selected_slot())
    
    presenter <- trimws(input$presenter_name)
    talk_title <- trimws(input$presentation_title)
    
    if (!nzchar(presenter)) {
      showNotification("Please enter your name.", type = "warning")
      return()
    }
    
    # The PRIMARY KEY on slot_id plus ON CONFLICT DO NOTHING is the critical
    # protection against two people booking the same slot simultaneously.
    inserted <- book_slot(
      slot_id = selected_slot(),
      presenter = presenter,
      talk_title = talk_title
    )
    
    removeModal()
    
    if (identical(as.integer(inserted), 1L)) {
      showNotification("Presentation slot booked.", type = "message")
    } else {
      showNotification(
        "Someone else booked that slot just before you. Please choose another date.",
        type = "warning",
        duration = 7
      )
    }
    
    selected_slot(NULL)
  })
}

shinyApp(ui, server)
