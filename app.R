source("game.2018.R")
#-------------------------------------------------------------------------------------
# SHINY APP: Civilization Game Dashboard
#-------------------------------------------------------------------------------------
library(shiny)

# UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('keyup', function(e) {
        if (e.key === 'Enter' && $('#cmd_pol').is(':focus')) {
          e.preventDefault();
          $('#submit_cmd').click();
        }
      });
    "))
  ),
  titlePanel("Sustainability Game"),
  sidebarLayout(
    sidebarPanel(
      tags$pre(
"The game is played by entering commands at each turn:
Politicians' commands:
  P x  <--- x = do nothing
  P a <X> <Y> xx <--- take xx% of total area from X and add it to Y
    X/Y : F = forest, f = farm, c = city, i = industry
Farmers' commands:
  F f xx <---- set fulfilment to xx % (fraction of food requirement farmer keeps)
  F p xx <---- set fertilizer usage to xx (kg/yr/hct)
  F m xx <---- xx % of farmers migrate to city (negative means xx % migrate from city)"
      ),
      textInput("cmd_pol", "Enter Command:", value = ""),
      actionButton("submit_cmd", "Submit Command"),
      verbatimTextOutput("submitted_command"), # New UI element to display the submitted command
      verbatimTextOutput("turn_info"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(12, plotOutput("plot_world_area", height = "250px")),
      ),
      fluidRow(
        column(4, plotOutput("plot_population", height = "250px")),
        column(4, plotOutput("plot_food_market", height = "250px")),
        column(4, plotOutput("plot_food_price", height = "250px"))
      ),
      fluidRow(
        column(4, plotOutput("plot_income", height = "250px")),
        column(4, plotOutput("plot_nutrition", height = "250px")),
        column(4, plotOutput("plot_happiness", height = "250px"))
      ),
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
    forest_effect = K.forest_effect,
    turn = 2
  )

  observeEvent(input$submit_cmd, {
    req(input$cmd_pol)
    vals$forest_effect <- K.forest_effect
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
    
    # Update the submitted command output
    output$submitted_command <- renderText({
      paste("Submitted Command:", input$cmd_pol)
    })
  })

  output$turn_info <- renderPrint({
    cat("Turn:", vals$turn, "\n")
    cat("Forest Effect Enabled:", vals$forest_effect, "\n")
    cat("Current State:\n")
    print(vals$state)
  })

  output$plot_world_area <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_world_area(d[t,])
  })

  output$plot_population <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_population(d[t,], d[t-1,])
  })

  output$plot_food_market <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_food_market(d[t,], d[t-1,])
  })

  output$plot_food_price <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_food_price(d[t,], d[t-1,])
  })

  output$plot_income <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_income(d[t,], d[t-1,])
  })

  output$plot_nutrition <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_nutrition(d[t,], d[t-1,])
  })

  output$plot_happiness <- renderPlot({
    d <- vals$dat
    t <- vals$turn
    if (nrow(d) < 2) return()
    plot_happiness(d[t,], d[t-1,])
  })
}

# Run the app
shinyApp(ui, server)
