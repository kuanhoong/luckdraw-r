#!/usr/bin/env Rscript
# Lucky Draw Wheel - Shiny App
# Single-file app (app.R)

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(readr)
library(dplyr)
library(jsonlite)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      body { background: linear-gradient(90deg,#fff7e6,#e6f7ff); }
      .app-title { font-size: 32px; font-weight:700; margin-bottom:10px; }
      .wheel-wrap { display:flex; justify-content:center; align-items:center; }
      #wheelOuter { width:600px; height:600px; border-radius:50%; position:relative; transform-origin: center center; }
      #wheelCanvas { width:100%; height:100%; display:block; }
      #pointer { position:absolute; left:50%; top:-20px; transform:translateX(-50%); z-index:5; visibility: hidden; }
      .has-names #pointer { visibility: visible; }
      .spin-btn { font-size:22px; padding:14px 22px; }
      .status-text { font-size:18px; margin-top:8px; }
      .sidebar-title { font-size:24px; font-weight:600; }
      .winner-name { font-size:20px; font-weight:700; }
      @media (max-width:900px) { #wheelOuter { width:360px; height:360px; } }
      "
    ))
  ),
  useShinyjs(),

  fluidRow(
    column(9,
      div(class="app-title", "🎉 Lucky Draw Wheel"),
      fluidRow(
        column(4,
          fileInput("file", "Upload CSV of names", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
          helpText("CSV should contain a single column of names (header recommended, e.g., 'name')."),
          actionButton("resetBtn", "Reset Draw", icon = icon("redo"), class = "btn-warning"),
          br(), br(),
          actionButton("spinBtn", "Spin the Wheel", icon = icon("gamepad"), class="btn-primary spin-btn"),
          div(class='status-text', textOutput("statusText"))
        ),
        column(8,
          div(class="wheel-wrap",
            tags$div(id="wheelOuter",
              tags$canvas(id="wheelCanvas", width="600", height="600"),
              tags$div(id="pointer", HTML('<svg width="40" height="40"><polygon points="20,0 30,40 10,40" style="fill:#ff4d4d;"/></svg>'))
            )
          )
        )
      )
    ),

    column(3,
      div(class='sidebar-title', "🏆 Winners"),
      br(),
      uiOutput("winnersUI"),
      br(),
      downloadButton("downloadWinners", "Download winners (CSV)", class='btn-success'),
      br(), br(),
      div(textOutput("counterText"))
    )
  ),

  # include client-side JS to build and animate wheel
  tags$script(HTML('
    // Colors palette
    const palette = ["#FFB300","#FF6F61","#6B5B95","#88B04B","#92A8D1","#F7CAC9","#955251","#BFD8B8","#F2E394","#D65076"];

    function drawWheel(names) {
      const canvas = document.getElementById("wheelCanvas");
      const ctx = canvas.getContext("2d");
      const w = canvas.width;
      const h = canvas.height;
      const cx = w/2, cy = h/2;
      const r = Math.min(w,h)/2 - 10;
      ctx.clearRect(0,0,w,h);
      const n = names.length;
      if(n === 0) return;
      const angle = 2*Math.PI/n;
      for(let i=0;i<n;i++){
        // Start at top (90 degrees), and make the segment centered there
        const start = -Math.PI/2 - angle/2 + i*angle;
        const end = start + angle;
        ctx.beginPath();
        ctx.moveTo(cx,cy);
        ctx.arc(cx,cy,r,start,end);
        ctx.closePath();
        ctx.fillStyle = palette[i % palette.length];
        ctx.fill();
        // draw borders
        ctx.strokeStyle = "#ffffff";
        ctx.lineWidth = 2;
        ctx.stroke();
        // labels
        ctx.save();
        ctx.translate(cx,cy);
        const mid = start + angle/2;
        ctx.rotate(mid);
        ctx.textAlign = "right";
        ctx.fillStyle = "#222";
        ctx.font = Math.max(12, Math.floor(r/10)) + "px sans-serif";
        ctx.fillText(names[i], r - 10, 6);
        ctx.restore();
      }
    }

    // rebuild wheel from server data
    Shiny.addCustomMessageHandler("buildWheel", function(msg){
      let names = msg.names || [];
      // ensure names is an array (when msg.names is a single string, wrap it)
      if(!Array.isArray(names)){
        if(typeof names === "string" && names.length > 0) names = [names];
        else names = [];
      }
      // reset rotation and update pointer visibility
      const outer = document.getElementById("wheelOuter");
      if(outer) {
        outer.style.transition = "none";
        outer.style.transform = "rotate(0deg)";
        // Show/hide pointer based on whether we have names
        outer.className = names.length > 0 ? "has-names" : "";
      }
      drawWheel(names);
    });

    // handle spin command
    Shiny.addCustomMessageHandler("spin", function(msg){
      const deg = msg.deg || 0;
      const winner = msg.winner || null;
      const canvas = document.getElementById("wheelCanvas");
      if(!canvas) return;
      // animate rotation: add a smooth transition
      // Use a cubic-bezier for fun effect
      canvas.style.transition = "transform 4s cubic-bezier(0.25, 0.1, 0.25, 1)";
      canvas.style.transform = "rotate(" + deg + "deg)";
      // when animation ends, notify server
      function doneOnce(){
        // tiny timeout to ensure final position
        setTimeout(function(){
          Shiny.setInputValue("spin_complete", {winner: winner, nonce: Math.random()}, {priority: "event"});
        }, 150);
        canvas.removeEventListener("transitionend", doneOnce);
      }
      canvas.addEventListener("transitionend", doneOnce);
    });
  '))
)

server <- function(input, output, session) {
  # reactive values to track lists
  rv <- reactiveValues(
    original = character(0),
    remaining = character(0),
    winners = character(0),
    status = "Waiting to upload..."
  )

  # helper: send wheel build to client
  sendWheel <- function() {
    session$sendCustomMessage('buildWheel', list(names = as.character(rv$remaining)))
  }

  # Read uploaded CSV
  observeEvent(input$file, {
    req(input$file)
    rv$winners <- character(0)
    tryCatch({
      df <- read_csv(input$file$datapath, show_col_types = FALSE)
      # try to find a single column of names
      if(ncol(df) == 0) stop('CSV has no columns')
      # if there's a column named name or Name, prefer it
      name_col <- NULL
      if('name' %in% tolower(names(df))) {
        # match case-insensitive
        name_col <- names(df)[tolower(names(df)) == 'name'][1]
      } else if(ncol(df) == 1) {
        name_col <- names(df)[1]
      } else {
        # otherwise, take first column
        name_col <- names(df)[1]
      }
      names_vec <- df[[name_col]] %>% as.character() %>% na.omit() %>% trimws()
      names_vec <- names_vec[nzchar(names_vec)]
      if(length(names_vec) == 0) {
        rv$status <- "Uploaded file contains no names."
        showNotification(rv$status, type = 'error')
        return()
      }
      # set lists
      rv$original <- names_vec
      rv$remaining <- names_vec
      rv$winners <- character(0)
      rv$status <- paste0('Loaded ', length(names_vec), ' names. Ready to spin!')
      sendWheel()
    }, error = function(e){
      rv$status <- paste0('Error reading CSV: ', e$message)
      showNotification(rv$status, type = 'error')
    })
  })

  # status text
  output$statusText <- renderText({ rv$status })

  # winners UI
  output$winnersUI <- renderUI({
    if(length(rv$winners) == 0) return(tags$div("No winners yet. Be the first!"))
    tags$ol(lapply(seq_along(rv$winners), function(i){
      tags$li(span(class='winner-name', rv$winners[i]))
    }))
  })

  output$counterText <- renderText({
    sprintf('Winners: %d | Remaining: %d', length(rv$winners), length(rv$remaining))
  })

  # Spin logic: server picks winner and instructs client to animate
  observeEvent(input$spinBtn, {
    if(length(rv$remaining) == 0) {
      rv$status <- 'All winners have been chosen!'
      return()
    }
    # disable spin button while spinning
    shinyjs::runjs("document.getElementById('spinBtn').disabled = true;")
    rv$status <- 'Spinning...'

    # choose random winner index
    n <- length(rv$remaining)
    chosen_index <- sample(seq_len(n), 1)
    winner_name <- rv$remaining[chosen_index]

    # compute degrees to rotate so that chosen segment lands at pointer (top)
    # each segment size in degrees
    seg_deg <- 360 / n
    # compute center angle of chosen segment measured clockwise from top (0)
    # chosen segment center angle (degrees) measured clockwise
    center_angle <- (chosen_index - 0.5) * seg_deg
    # we want to rotate so that center_angle ends at 0 (top).
    # If we rotate clockwise by X degrees, the wheel visually rotates clockwise. We will rotate clockwise negative in CSS (positive deg rotates clockwise visually in browser), so target = 360 - center_angle
    target_deg <- 360 - center_angle
    spins <- sample(3:6,1)
    final_deg <- spins*360 + target_deg

    # send command to client to animate
    session$sendCustomMessage('spin', list(deg = final_deg, winner = winner_name))
    # store the chosen candidate temporarily while waiting for client callback
    rv$pending_winner <- winner_name
  })

  # observe when the client notifies that spin animation is complete
  observeEvent(input$spin_complete, {
    payload <- input$spin_complete
    if(is.null(payload$winner)) return()
    winner_name <- as.character(payload$winner)
    # move winner to winners list and remove from remaining
    if(winner_name %in% rv$remaining) {
      rv$winners <- c(rv$winners, winner_name)
      rv$remaining <- rv$remaining[rv$remaining != winner_name]
      rv$status <- 'Winner selected!'
    } else {
      # already removed (shouldn't happen) but handle gracefully
      rv$status <- 'Winner was already removed.'
    }

    # show modal with winner
    showModal(modalDialog(
      title = tagList(icon('trophy'), 'Winner!'),
      tags$h2(style='color:#d9534f; text-align:center;', winner_name),
      easyClose = TRUE,
      footer = tagList(modalButton('Close'))
    ))

    # rebuild wheel
    sendWheel()

    # re-enable spin button or disable if none remaining
    if(length(rv$remaining) == 0) {
      rv$status <- 'All winners have been chosen!'
      # disable button via JS
      shinyjs::runjs("document.getElementById('spinBtn').disabled = true;")
    } else {
      shinyjs::runjs("document.getElementById('spinBtn').disabled = false;")
    }
  })

  # Reset draw to original uploaded list
  observeEvent(input$resetBtn, {
    if(length(rv$original) == 0) {
      rv$status <- 'No upload to reset to.'
      return()
    }
    rv$remaining <- rv$original
    rv$winners <- character(0)
    rv$status <- 'Reset to original list.'
    sendWheel()
    shinyjs::runjs("document.getElementById('spinBtn').disabled = false;")
  })

  # download winners
  output$downloadWinners <- downloadHandler(
    filename = function(){ paste0('winners_', Sys.Date(), '.csv') },
    content = function(file){
      df <- data.frame(order = seq_along(rv$winners), name = rv$winners, stringsAsFactors = FALSE)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # initial build with no names
  observe({ sendWheel() })
}

# Run app
shinyApp(ui = ui, server = server)
