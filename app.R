# ============================================================
# PHOG presentation-slot scheduler
# ============================================================
# Local testing: uses SQLite automatically if PHOG_DB_HOST is not set.
# Shared deployment: set PHOG_DB_* environment variables to use PostgreSQL/Neon.
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
    slot_id = paste0(
      "team_",
      format(team_dates, "%Y-%m-%d")
    ),
    meeting_type = "PHOG Team",
    date = team_dates,
    duration_minutes = 30L,
    calendar_id = "team",
    stringsAsFactors = FALSE
  ),

  data.frame(
    slot_id = paste0(
      "bitesize_",
      format(bitesize_dates, "%Y-%m-%d")
    ),
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
      port     = as.integer(
        Sys.getenv(
          "PHOG_DB_PORT",
          "5432"
        )
      ),
      dbname   = Sys.getenv("PHOG_DB_NAME"),
      user     = Sys.getenv("PHOG_DB_USER"),
      password = Sys.getenv("PHOG_DB_PASSWORD"),
      sslmode  = Sys.getenv(
        "PHOG_DB_SSLMODE",
        "require"
      )
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

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  fun(con)
}


# ------------------------------------------------------------
# Create / update database structure
# ------------------------------------------------------------

init_db <- function() {

  with_db(function(con) {

    # PostgreSQL / Neon
    if (use_postgres()) {

      DBI::dbExecute(
        con,
        paste(
          "CREATE TABLE IF NOT EXISTS bookings (",
          "slot_id TEXT PRIMARY KEY,",
          "presenter TEXT NOT NULL,",
          "talk_title TEXT,",
          "booked_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP",
          ")"
        )
      )

    } else {

      # Local SQLite fallback
      DBI::dbExecute(
        con,
        paste(
          "CREATE TABLE IF NOT EXISTS bookings (",
          "slot_id TEXT PRIMARY KEY,",
          "presenter TEXT NOT NULL,",
          "talk_title TEXT,",
          "booked_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP",
          ")"
        )
      )
    }


    # Protect against an older table which did not contain talk_title
    fields <- DBI::dbListFields(
      con,
      "bookings"
    )

    if (!"talk_title" %in% fields) {

      DBI::dbExecute(
        con,
        "ALTER TABLE bookings ADD COLUMN talk_title TEXT"
      )
    }
  })
}


# ------------------------------------------------------------
# Read bookings
# ------------------------------------------------------------

read_bookings <- function() {

  with_db(function(con) {

    DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "slot_id,",
        "presenter,",
        "talk_title,",
        "booked_at",
        "FROM bookings",
        "ORDER BY slot_id"
      )
    )
  })
}


# ------------------------------------------------------------
# Detect changes for reactivePoll
# ------------------------------------------------------------

booking_version <- function() {

  with_db(function(con) {

    x <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT COUNT(*) AS n,",
        "MAX(booked_at) AS latest",
        "FROM bookings"
      )
    )

    latest <- x$latest[[1]]

    if (
      length(latest) == 0 ||
      is.na(latest)
    ) {

      latest <- ""

    } else {

      latest <- as.character(latest)
    }

    paste(
      x$n[[1]],
      latest,
      sep = "|"
    )
  })
}


# ------------------------------------------------------------
# Create booking
# ------------------------------------------------------------

book_slot <- function(
  slot_id,
  presenter,
  talk_title
) {

  with_db(function(con) {

    sql <- DBI::sqlInterpolate(
      con,

      paste(
        "INSERT INTO bookings",
        "(slot_id, presenter, talk_title, booked_at)",
        "VALUES",
        "(?slot_id, ?presenter, ?talk_title, CURRENT_TIMESTAMP)",
        "ON CONFLICT (slot_id) DO NOTHING"
      ),

      slot_id = slot_id,
      presenter = presenter,
      talk_title = talk_title
    )

    DBI::dbExecute(
      con,
      sql
    )
  })
}


# ------------------------------------------------------------
# Edit booking
# ------------------------------------------------------------

update_slot <- function(
  slot_id,
  presenter,
  talk_title
) {

  with_db(function(con) {

    sql <- DBI::sqlInterpolate(
      con,

      paste(
        "UPDATE bookings",
        "SET presenter = ?presenter,",
        "talk_title = ?talk_title,",
        "booked_at = CURRENT_TIMESTAMP",
        "WHERE slot_id = ?slot_id"
      ),

      slot_id = slot_id,
      presenter = presenter,
      talk_title = talk_title
    )

    DBI::dbExecute(
      con,
      sql
    )
  })
}


# ------------------------------------------------------------
# Cancel booking
# ------------------------------------------------------------

cancel_slot <- function(slot_id) {

  with_db(function(con) {

    sql <- DBI::sqlInterpolate(
      con,
      "DELETE FROM bookings WHERE slot_id = ?slot_id",
      slot_id = slot_id
    )

    DBI::dbExecute(
      con,
      sql
    )
  })
}


# Initialise database
init_db()

message(
  "PHOG database backend: ",
  if (use_postgres()) {
    "PostgreSQL / Neon"
  } else {
    "local SQLite"
  }
)


# ------------------------------------------------------------
# 3. Convert slots + bookings into calendar events
# ------------------------------------------------------------

make_calendar_events <- function(bookings) {

  x <- merge(
    SLOTS,
    bookings,
    by = "slot_id",
    all.x = TRUE,
    sort = FALSE
  )

  # Restore chronological slot order
  x <- x[
    match(
      SLOTS$slot_id,
      x$slot_id
    ),
  ]

  booked <-
    !is.na(x$presenter) &
    nzchar(x$presenter)


  x$title <- ifelse(

    booked,

    paste0(
      x$meeting_type,
      " | BOOKED: ",
      x$presenter
    ),

    paste0(
      x$meeting_type,
      " | AVAILABLE (",
      x$duration_minutes,
      " min)"
    )
  )


  x$body <- ifelse(

    booked,

    paste0(
      "Presenter: ",
      x$presenter,

      ifelse(
        is.na(x$talk_title) |
          !nzchar(x$talk_title),

        "",

        paste0(
          "\nPresentation: ",
          x$talk_title
        )
      )
    ),

    paste0(
      "Available presentation slot: ",
      x$duration_minutes,
      " minutes"
    )
  )


  data.frame(
    id = x$slot_id,
    calendarId = x$calendar_id,
    title = x$title,
    body = x$body,

    # Same start/end date prevents event spreading into next day
    start = format(
      x$date,
      "%Y-%m-%d"
    ),

    end = format(
      x$date,
      "%Y-%m-%d"
    ),

    category = "allday",

    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 4. User interface
# ------------------------------------------------------------

ui <- fluidPage(

  tags$head(

    tags$style(

      HTML("

        body {
          max-width: 1250px;
          margin: 0 auto;
          padding: 20px;
        }

        .legend-row {
          display: flex;
          gap: 24px;
          margin: 12px 0 18px 0;
          flex-wrap: wrap;
        }

        .legend-item {
          display: flex;
          align-items: center;
          gap: 8px;
        }

        .legend-box {
          width: 18px;
          height: 18px;
          border-radius: 4px;
        }

        .help-text {
          color: #555;
          margin-bottom: 10px;
        }


        /* ---------------------------------------------
           Calendar event text wrapping
           --------------------------------------------- */

        .tui-full-calendar-weekday-schedule {

          height: auto !important;

          min-height: 34px !important;

          overflow: visible !important;
        }


        .tui-full-calendar-weekday-schedule-title {

          white-space: normal !important;

          overflow-wrap: anywhere !important;

          word-break: break-word !important;

          line-height: 1.2 !important;

          padding-top: 3px !important;

          padding-bottom: 3px !important;

          display: block !important;

          overflow: visible !important;

          text-overflow: clip !important;
        }

      ")
    )
  ),


  titlePanel(
    "PHOG Meeting Scheduler"
  ),


  tags$p(
    class = "help-text",

    "Click an available meeting to book the presentation slot. ",

    "Click a booked meeting to edit or cancel the booking. ",

    "PHOG Team slots are 30 minutes; PHOG Bitesize slots are 60 minutes."
  ),


  # ----------------------------------------------------------
  # Legend
  # ----------------------------------------------------------

  tags$div(

    class = "legend-row",


    tags$div(

      class = "legend-item",

      tags$span(
        class = "legend-box",

        style = paste0(
          "background: rgba(125, 185, 235, 0.68); ",
          "border: 1px solid #7DB9EB;"
        )
      ),

      tags$span(
        "PHOG Team"
      )
    ),


    tags$div(

      class = "legend-item",

      tags$span(
        class = "legend-box",

        style = paste0(
          "background: rgba(157, 216, 166, 0.68); ",
          "border: 1px solid #9DD8A6;"
        )
      ),

      tags$span(
        "PHOG Bitesize"
      )
    )
  ),


  calendarOutput(
    "phog_calendar",
    height = "840px"
  )
)


# ------------------------------------------------------------
# 5. Server logic
# ------------------------------------------------------------

server <- function(
  input,
  output,
  session
) {


  # ----------------------------------------------------------
  # Poll database
  # ----------------------------------------------------------

  bookings <- reactivePoll(

    intervalMillis = 3000,

    session = session,

    checkFunc = booking_version,

    valueFunc = read_bookings
  )


  # ----------------------------------------------------------
  # Remember currently visible month
  # ----------------------------------------------------------

  visible_date <- reactiveVal(
    as.Date("2026-09-30")
  )


  observeEvent(

    input$phog_calendar_dates,

    {

      d <- input$phog_calendar_dates

      req(
        d$start,
        d$end
      )


      start_date <- as.Date(
        substr(
          d$start,
          1,
          10
        )
      )


      end_date <- as.Date(
        substr(
          d$end,
          1,
          10
        )
      )


      visible_date(

        start_date +

          floor(
            as.numeric(
              end_date -
                start_date
            ) / 2
          )
      )
    },

    ignoreInit = TRUE
  )


  # ----------------------------------------------------------
  # Calendar
  # ----------------------------------------------------------

  output$phog_calendar <- renderCalendar({

    events <- make_calendar_events(
      bookings()
    )


    calendar(

      view = "month",

      defaultDate = isolate(
        visible_date()
      ),

      navigation = TRUE,


      # UK date format
      navOpts = navigation_options(

        fmt_date = "DD/MM/YYYY",

        sep_date = " - "
      ),


      useDetailPopup = FALSE,

      useCreationPopup = FALSE,

      isReadOnly = TRUE

    ) |>


      # ------------------------------------------------------
      # Meeting colours
      # ------------------------------------------------------

      cal_props(

        list(

          id = "team",

          name = "PHOG Team",

          color = "#18324A",

          backgroundColor =
            "rgba(125, 185, 235, 0.68)",

          borderColor =
            "#7DB9EB"
        ),


        list(

          id = "bitesize",

          name = "PHOG Bitesize",

          color = "#24452B",

          backgroundColor =
            "rgba(157, 216, 166, 0.68)",

          borderColor =
            "#9DD8A6"
        )
      ) |>


      cal_schedules(
        events
      ) |>


      # Pass clicked event ID back to Shiny
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


  # Remember which slot user is currently working with
  selected_slot <- reactiveVal(
    NULL
  )


  # ----------------------------------------------------------
  # User clicks a calendar slot
  # ----------------------------------------------------------

  observeEvent(

    input$slot_click,

    {

      slot_id <-
        as.character(
          input$slot_click$id
        )


      slot <- SLOTS[
        SLOTS$slot_id == slot_id,
        ,
        drop = FALSE
      ]


      req(
        nrow(slot) == 1
      )


      current <-
        read_bookings()


      current_booking <-
        current[
          current$slot_id == slot_id,
          ,
          drop = FALSE
        ]


      # ======================================================
      # BOOKED SLOT
      # ======================================================

      if (
        nrow(current_booking) == 1
      ) {


        selected_slot(
          slot_id
        )


        current_title <-
          current_booking$talk_title[[1]]


        if (
          is.na(current_title)
        ) {

          current_title <- ""
        }


        showModal(

          modalDialog(


            title = paste0(

              "Edit ",

              slot$meeting_type,

              " — ",

              format(
                slot$date,
                "%d/%m/%Y"
              )
            ),


            tags$p(

              tags$b(
                "This presentation slot is currently booked."
              )
            ),


            textInput(

              "edit_presenter_name",

              "Presenter",

              value =
                current_booking$presenter[[1]]
            ),


            textInput(

              "edit_presentation_title",

              "Presentation title / topic",

              value =
                current_title
            ),


            easyClose = TRUE,


            footer = tagList(


              modalButton(
                "Close"
              ),


              actionButton(

                "cancel_booking",

                "Cancel booking",

                class =
                  "btn-danger"
              ),


              actionButton(

                "save_booking_changes",

                "Save changes",

                class =
                  "btn-primary"
              )
            )
          )
        )


        return()
      }


      # ======================================================
      # AVAILABLE SLOT
      # ======================================================

      selected_slot(
        slot_id
      )


      showModal(

        modalDialog(


          title = paste0(

            "Book ",

            slot$meeting_type,

            " — ",

            format(
              slot$date,
              "%d/%m/%Y"
            ),

            " (",

            slot$duration_minutes,

            " min)"
          ),


          textInput(

            "presenter_name",

            "Your name",

            placeholder =
              "e.g. Jane Smith"
          ),


          textInput(

            "presentation_title",

            "Presentation title / topic",

            placeholder =
              "Optional"
          ),


          easyClose = TRUE,


          footer = tagList(


            modalButton(
              "Cancel"
            ),


            actionButton(

              "confirm_booking",

              "Book this slot",

              class =
                "btn-primary"
            )
          )
        )
      )
    }
  )


  # ----------------------------------------------------------
  # CREATE booking
  # ----------------------------------------------------------

  observeEvent(

    input$confirm_booking,

    {

      req(
        selected_slot()
      )


      presenter <-
        trimws(
          input$presenter_name
        )


      talk_title <-
        trimws(
          input$presentation_title
        )


      if (
        !nzchar(presenter)
      ) {

        showNotification(

          "Please enter your name.",

          type =
            "warning"
        )

        return()
      }


      inserted <-
        book_slot(

          slot_id =
            selected_slot(),

          presenter =
            presenter,

          talk_title =
            talk_title
        )


      removeModal()


      if (
        identical(
          as.integer(inserted),
          1L
        )
      ) {

        showNotification(

          "Presentation slot booked.",

          type =
            "message"
        )

      } else {

        showNotification(

          paste(
            "Someone else booked that slot just before you.",
            "Please choose another date."
          ),

          type =
            "warning",

          duration =
            7
        )
      }


      selected_slot(
        NULL
      )
    }
  )


  # ----------------------------------------------------------
  # EDIT existing booking
  # ----------------------------------------------------------

  observeEvent(

    input$save_booking_changes,

    {

      req(
        selected_slot()
      )


      presenter <-
        trimws(
          input$edit_presenter_name
        )


      talk_title <-
        trimws(
          input$edit_presentation_title
        )


      if (
        !nzchar(presenter)
      ) {

        showNotification(

          "Please enter a presenter name.",

          type =
            "warning"
        )

        return()
      }


      updated <-
        update_slot(

          slot_id =
            selected_slot(),

          presenter =
            presenter,

          talk_title =
            talk_title
        )


      removeModal()


      if (
        identical(
          as.integer(updated),
          1L
        )
      ) {

        showNotification(

          "Booking updated.",

          type =
            "message"
        )

      } else {

        showNotification(

          paste(
            "The booking could not be updated.",
            "It may already have been removed."
          ),

          type =
            "warning",

          duration =
            7
        )
      }


      selected_slot(
        NULL
      )
    }
  )


  # ----------------------------------------------------------
  # Ask before cancelling
  # ----------------------------------------------------------

  observeEvent(

    input$cancel_booking,

    {

      req(
        selected_slot()
      )


      slot <- SLOTS[
        SLOTS$slot_id ==
          selected_slot(),
        ,
        drop = FALSE
      ]


      removeModal()


      showModal(

        modalDialog(


          title =
            "Cancel this booking?",


          tags$p(

            paste0(

              slot$meeting_type,

              " — ",

              format(
                slot$date,
                "%d/%m/%Y"
              )
            )
          ),


          tags$p(
            paste(
              "This will make the presentation slot",
              "available to the team again."
            )
          ),


          easyClose =
            TRUE,


          footer = tagList(


            modalButton(
              "Keep booking"
            ),


            actionButton(

              "confirm_cancel_booking",

              "Yes, cancel booking",

              class =
                "btn-danger"
            )
          )
        )
      )
    }
  )


  # ----------------------------------------------------------
  # CANCEL booking
  # ----------------------------------------------------------

  observeEvent(

    input$confirm_cancel_booking,

    {

      req(
        selected_slot()
      )


      deleted <-
        cancel_slot(
          selected_slot()
        )


      removeModal()


      if (
        identical(
          as.integer(deleted),
          1L
        )
      ) {

        showNotification(

          paste(
            "Booking cancelled.",
            "The slot is now available."
          ),

          type =
            "message"
        )

      } else {

        showNotification(

          paste(
            "The booking could not be cancelled.",
            "It may already have been removed."
          ),

          type =
            "warning",

          duration =
            7
        )
      }


      selected_slot(
        NULL
      )
    }
  )
}


# ------------------------------------------------------------
# Start application
# ------------------------------------------------------------

shinyApp(
  ui,
  server
)
