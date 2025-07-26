
source("game.2018.R")

#-------------------------------------------------------------------------------------
# INITIAL WORLD STATE
#-------------------------------------------------------------------------------------

# Initial area fractions for each land type: forest, farm, city, industry
if (abs(sum(state$world.areas) - 1)>1e-6) cat("Areas dont add up: ", sum(state$world.areas)) # Warn if areas don't sum to 1

# Dataframe to store time series of all tracked variables for plotting and analysis
dat = data.frame(
  A.forest=0, A.farm=0, A.city=0, A.ind=0, 
  demand.food_0=0, produce.farm=0, food.surplus=0, price.food=0, sold.food=0, 
  N.city=0, N.farmers=0, revenue.city=0, revenue.farmer=0, inc.city=0, inc.farmer=0, 
  satiety.city=0, satiety.farmer=0, hdi.city=0, hdi.farmer=0, polit.popularity=0,
  supply.food_max=0, cost.city=0, invest.farmer=0
)

Ts = 500 # Number of time steps (turns) in the simulation

#-------------------------------------------------------------------------------------
# MAIN GAME LOOP
#-------------------------------------------------------------------------------------
for (t in 2:Ts){

  #------------------- PLAYER INPUT SECTION -------------------
  if (t > 2){
    # Prompt for player command (Politician or Farmer)
    cmd_pol  = readline(prompt=">> ")
    state = process_command(cmd_pol, state)
  }  
  
  l = update_state(state)
  state = l$state
  dat[t,] = l$dat1

  plot_state(dat)  # Plot the current state of the game

}

#-------------------------------------------------------------------------------------
# TODO: Implement disasters with a 10-round lag, proportional to forest area
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
