source("game.2018.R")
#-------------------------------------------------------------------------------------
# SHINY APP: Civilization Game Dashboard
#-------------------------------------------------------------------------------------
library(shiny)

# UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'Enter' && $('#cmd_pol').is(':focus')) {
          $('#submit_cmd').click();
        }
      });
    "))
  ),
  titlePanel("Sustainability Game"),
  sidebarLayout(
    sidebarPanel(
      textInput("cmd_pol", "Enter Command (P/F/G ...):", value = ""),
      actionButton("submit_cmd", "Submit Command"),
      verbatimTextOutput("turn_info"),
      width = 3
    ),
    mainPanel(
      plotOutput("game_plot", height = "800px"),
      width = 9
    )
  )
)

# SERVER
server <- function(input, output, session) {
  Ts <- 500

  vals <- reactiveValues(
    state = state,
    dat = {
      df <- data.frame(
        A.forest=0, A.farm=0, A.city=0, A.ind=0, 
        demand.food_0=0, produce.farm=0, food.surplus=0, price.food=0, sold.food=0, 
        N.city=0, N.farmers=0, revenue.city=0, revenue.farmer=0, inc.city=0, inc.farmer=0, 
        satiety.city=0, satiety.farmer=0, hdi.city=0, hdi.farmer=0, polit.popularity=0,
        supply.food_max=0, cost.city=0, invest.farmer=0
      )
      l <- update_state(state)
      df[1,] <- l$dat1
      df[2,] <- l$dat1
      df
    },
    turn = 2
  )

  observeEvent(input$submit_cmd, {
    req(input$cmd_pol)
    vals$state <- process_command(input$cmd_pol, vals$state)
    t <- vals$turn + 1
    if (t > Ts) return()
    l <- update_state(vals$state)
    d <- vals$dat
    if (nrow(d) < t) {
      d[t,] <- l$dat1
    } else {
      d[t,] <- l$dat1
    }
    vals$dat <- d
    vals$state <- l$state
    vals$turn <- t
    updateTextInput(session, "cmd_pol", value = "")
  })

  output$turn_info <- renderPrint({
    cat("Turn:", vals$turn, "\n")
    cat("Current State:\n")
    print(vals$state)
  })

  output$game_plot <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_state(d[1:max(2, t),])
  })
}

# Run the app
shinyApp(ui, server)
