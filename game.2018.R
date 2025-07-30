# require("rootSolve")

#-------------------------------------------------------------------------------------
# GAME COMMANDS AND INTERFACE
#-------------------------------------------------------------------------------------
# The game is played by entering commands at each turn.
# Commands:
#   P/F <--- Politician / Farmer (who is acting)
#   P a/x  <--- x = do nothing, a = allocate area
#   P a XX YY xx <--- take xx% of total area from XX and add it to YY
#       XX/YY : F = forest, f = farm, c = city, i = industry
#   F f/p/m xx
#     f xx <---- set fulfilment to xx % (fraction of food requirement farmer keeps)
#     p xx <---- set fertilizer usage to xx (kg/yr/hct)
#     m xx <---- xx % of farmers migrate to city (negative means xx % migrate from city)
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# UTILITY FUNCTIONS
#-------------------------------------------------------------------------------------

# Sigmoid function for smooth transitions, used in various calculations (e.g., drought probability, HDI)
sigmoid = function(x,a){
  1/(1+exp(-a*x))
}

#-------------------------------------------------------------------------------------
# INITIAL PARAMETERS AND CONSTANTS
#-------------------------------------------------------------------------------------

# State variables
state = list(
  N.farmers = 1500,  # Initial number of farmers
  N.city = 1500,     # Initial number of city dwellers
  farmer.fulfillment = 1,  # Fraction of food requirement farmer keeps for self
  fert.usage = 10,  # Fertilizer usage (kg/yr/hct)
  world.areas = c(F=0.25, f=.45, c=0.2, i=0.1)
)

# Constants
A.tot = 900   # Total area (hectares)
K.fert_price = 200  # Fertilizer price (Rs/kg)
K.food_cons_pc = 200/1000*365    # Per capita food consumption (Kg/yr) for city
# K.food_cons_pc_farmer = 200/1000*365    # Per capita food consumption (Kg/yr) for farmer
K.farm_yield_0 = 500 # Baseline farm yield (kg/hct)
K.demand_elast = 100 # Demand elasticity (higher = less sensitive to price)
K.supply_elast = 20  # Supply elasticity (higher = less sensitive to price)
K.rev_industry = 100000 # Revenue per industrial area (Rs)
K.fert_effectiveness = 1/15 # Effectiveness of fertilizer (yield increase per kg)
K.forest_effect = T

#-------------------------------------------------------------------------------------
# PLOTTING FUNCTION FOR SUPPLY-DEMAND CURVES
#-------------------------------------------------------------------------------------
plot_supply_demand = function(supply.food_max, demand.food_0, price.food){
  # Plots the supply and demand curves for food, showing equilibrium
  x = seq(1,200,0.1)
  plot(y=supply.food_max*(1-exp(-x/K.supply_elast))/1000,
       x=x, 
       type="l", 
       ylim=c(0, max(supply.food_max, demand.food_0))/1000,
       xlab="Price (Rs/Kg)",
       ylab="Quantity (tons)",
       main="Food S&D")
  points(y=demand.food_0*exp(-x/K.demand_elast)/1000,
         x=x, 
         type="l", 
         col="blue")
  abline(v=price.food, col="grey")
}

#-------------------------------------------------------------------------------------
# COMMAND PROCESSING FUNCTION
#-------------------------------------------------------------------------------------
process_command = function(cmd_pol, state){
  if (cmd_pol != ""){
    cmd_vec = strsplit(cmd_pol, split = " ")[[1]]
    
    # incorrect command format, do nothing
    if (length(cmd_vec) < 2) return(state)
    
    agent = cmd_vec[1]    # "P" or "F"
    command = cmd_vec[2]  # action

    # Politician commands
    if (agent == "P"){        
      if (command == "a"){    
        if (length(cmd_vec) < 5) return(state)
        
        # Area reallocation: e.g., "P a F i 20" moves 20% of total area from Forest to Industry
        from = cmd_vec[3]     # Source area type
        to = cmd_vec[4]       # Destination area type
        
        # Do nothing if invalid from and to specified
        if (from == to) return (state)
        if (!(from %in% c('F', 'f', 'c', 'i'))) return(state) 
        if (!(to %in% c('F', 'f', 'c', 'i'))) return(state) 
                
        amt = min(as.numeric(cmd_vec[5])/100*sum(state$world.areas), state$world.areas[from]-0.04) # Amount to move (cannot exceed available)
        state$world.areas[from] <- state$world.areas[from] - amt
        state$world.areas[to]   <- state$world.areas[to]   + amt
      }
      else if (command == "x"){} # Do nothing
    }
    # Farmer commands
    else if (agent == "F"){
      if (length(cmd_vec) < 3) return(state)
      if (command == "f"){    
        # Set farmer's own food fulfilment (fraction of requirement kept)
        state$farmer.fulfillment <- max(min(as.numeric(cmd_vec[3]), 1), 0.01)
      }
      if (command == "p"){    
        # Set fertilizer usage (kg/yr/hct)
        state$fert.usage <- as.numeric(cmd_vec[3])
      }
      if (command == "m"){    
        # Farmer migration: xx% of farmers migrate to city (negative = migrate from city to farm)
        percent_migrants = abs(as.numeric(cmd_vec[3]))
        dir_migration = sign(as.numeric(cmd_vec[3]))
        percent_migrants = max(min(percent_migrants, 99), 0)

        if (dir_migration > 0){
          # migration from farm
          migrants = round(state$N.farmers*percent_migrants/100)
          if (migrants >= state$N.farmers) migrants = migrants - 15 # ensure at least 5 individuals remain
          state$N.farmers <- state$N.farmers - migrants
          state$N.city    <- state$N.city    + migrants
        } else {
          # Migration from city
          migrants = round(state$N.city*percent_migrants/100)
          if (migrants >= state$N.city) migrants = migrants - 15 # ensure at least 5 individuals remain
          state$N.farmers <- state$N.farmers + migrants
          state$N.city    <- state$N.city    - migrants
        }
      }
    }
    # Game manager commands
    else if (agent == "G"){
      if (command == "sd"){
        # Plot supply-demand curves
        # plot_supply_demand()
      }
      else if (command == "x"){
        # End game
        stop("Game exited by user command")
      }
      else if (command == "f"){
        # Remove forest effect
        K.forest_effect <<- F
      }
      else if (command == "F"){
        # Restore forest effect
        K.forest_effect <<- T
      }
    }
    else {
      cat("Unknown command: ", command, "\n")
    }
  }

  state
}


update_state = function(state){
  # Calculate actual areas for each land type
  A.forest = state$world.areas[1]*A.tot
  A.farm   = state$world.areas[2]*A.tot
  A.city   = state$world.areas[3]*A.tot
  A.ind    = state$world.areas[4]*A.tot

  # Calculate total food demand from city population
  demand.food_0   = (state$N.city)*K.food_cons_pc   # Kg

  #------------------- DROUGHT DYNAMICS -------------------
  # Probability of drought is a function of forest area (less forest = higher drought risk)
  # Uses a sigmoid for smooth transition, with a sharp increase below 15% forest
  p.drought = 0*0.4*sigmoid(A.forest/A.tot - 0.15, -5)  
  b.drought = rbinom(n = 1, size=1, prob=p.drought) # Simulate drought event (1 = drought, 0 = no drought)
  # TODO: introduce lagged effect of forest area on drought

  #------------------- FARM SECTOR DYNAMICS -------------------
  # Farm yield increases with fertilizer usage, saturating at high usage
  yield.farm = K.farm_yield_0*2*(1-exp(-state$fert.usage*K.fert_effectiveness)) * (1-0.2*b.drought) # Reduce yield by 20% during drought
  # Farmer's own food consumption (can be less than requirement if fulfilment < 1)
  cons.food_pc_farmer = K.food_cons_pc* state$farmer.fulfillment 
  # Total farm produce (kg)
  produce.farm = yield.farm*A.farm 
  # Food surplus = total produce minus what farmers keep for themselves
  food.surplus = produce.farm - state$N.farmers*cons.food_pc_farmer  
  # Maximum food available for sale (cannot be negative)
  supply.food_max = max(0, food.surplus)
  
  #------------------- FOOD MARKET DYNAMICS -------------------
  # Market equilibrium price is found where supply equals demand
  price.food = uniroot(
    f = function(x) {
      supply.food_max * (1 - exp(-x / K.supply_elast)) - demand.food_0 * (exp(-x / K.demand_elast))
    },
    lower = 0, upper = 1000
  )$root
  # Amount of food actually sold at equilibrium price
  sold.food = supply.food_max*(1-exp(-price.food/K.supply_elast))  
  # Farmer revenue per capita (from food sales)
  revenue.farmer = sold.food/state$N.farmers*price.food   
  # Farmer investment per capita (fertilizer cost)
  invest.farmer  = (A.farm/state$N.farmers)*state$fert.usage*K.fert_price  
  # Farmer income = revenue - investment
  inc.farmer = revenue.farmer - invest.farmer  
  # Satiety: fraction of food requirement met (capped at 1)
  satiety.farmer = min(cons.food_pc_farmer/K.food_cons_pc, 1)
  # Farmer HDI: function of satiety and income (using sigmoid for smoothness)
  hdi.farmer =  sigmoid(satiety.farmer-0.2, 5) * sigmoid(inc.farmer-3000, .001)  
  
  #------------------- CITY SECTOR DYNAMICS -------------------
  # Per capita food consumption in city (from market)
  cons.food_pc = supply.food_max*(1-exp(-price.food/K.supply_elast))/state$N.city 
  # City revenue per capita (from industry)
  revenue.city = A.ind/state$N.city * K.rev_industry
  # City food cost per capita
  cost.city = cons.food_pc*price.food
  # City income = revenue - food cost
  inc.city = revenue.city - cost.city  
  # Health proxy: ratio of forest to industry area (more forest = healthier)
  health.city = A.forest/A.ind
  if (!K.forest_effect) health.city = 2 # Forest doesnt affect city health (baseline case), if K.forest_effect is FALSE
  # Crowding: area per city dweller (higher = less crowded)
  crowding.city = A.city/state$N.city 
  # Satiety: fraction of food requirement met
  satiety.city = cons.food_pc/K.food_cons_pc
  # City HDI: function of satiety, income, crowding, and health
  hdi.city =  sigmoid(satiety.city-0.2, 5) * sigmoid(inc.city-3000, .001) * sigmoid(crowding.city-0.05,20) * 2*sigmoid(health.city-2,1)  
  
  #------------------- POLITICAL SECTOR DYNAMICS -------------------
  # Tax collection: 20% of city income, 5% of farmer income
  tax.collection =  inc.city*state$N.city*0.20 + inc.farmer*state$N.farmers*0.05
  # Political popularity: function of both city and farmer HDI
  polit.popularity = sigmoid(hdi.city*hdi.farmer - 0.2, 10)
  
  #------------------- (OPTIONAL) COUNTRY MAP -------------------
  # Matrix representation of land types (for visualization)
  country = matrix(nrow=100, ncol=100, data=0)
  
  #------------------- RECORD DATA FOR THIS TURN -------------------
  dat1 = c(
    A.forest, A.farm, A.city, A.ind, demand.food_0, produce.farm, food.surplus, 
    price.food, sold.food, state$N.city, state$N.farmers, revenue.city, revenue.farmer, 
    inc.city, inc.farmer, satiety.city, satiety.farmer, hdi.city, hdi.farmer, polit.popularity, 
    supply.food_max, cost.city, invest.farmer
  )  

  list(state = state, dat1 = dat1)
}


#-------------------------------------------------------------------------------------
# PLOTTING SECTION: VISUALIZE GAME STATE
#-------------------------------------------------------------------------------------
plotArrow = function(x,y,value){
  if (is.na(value)) return()
  if (value < 0){
    pch1 = -9660 # Unicode down arrow
    col1 = scales::alpha("red", 0.8)
  }
  else {
    pch1 = -9650 # Unicode up arrow
    col1 = scales::alpha("green4", 0.8)
  }
  mag = min(abs(value), 300)
  # cat(value, "\n")
  cex = 1+mag/70 # Arrow size proportional to magnitude of change
  if (value != 0 && !is.infinite(value)) points(x=x,y=y, pch=pch1, col=col1, cex=cex)
}

plot_world_area = function(tt) {
  world = matrix(
    data = rep(c(1,2,3,4), c(round(tt$A.forest), round(tt$A.farm), round(tt$A.city), round(tt$A.ind))),
    nrow=30,
    byrow = F
  )
  b = (which(diff(c(-1,world[1,],10)) > 0))-1
  image(
    t(world), xaxt="n", yaxt="n", main="World Area",
    col=c("green4","lightgreen", "grey", "red")
  )
  axis(
    side = 1, at = (b[-length(b)]+diff(b)/2)/30,
    labels = c("Forest", "Farm", "City", "Ind")
  )
}

plot_population = function(tt, tm) {
  barplot(c(tt$N.city, tt$N.farmers), main="Population", names.arg = c("city", "farm"), ylim=c(0,3000))
  citypopchange = (tt$N.city - tm$N.city) / tm$N.city * 100
  farmerpopchange = (tt$N.farmers - tm$N.farmers) / tm$N.farmers * 100
  plotArrow(x=0.7,y=2800, citypopchange)
  plotArrow(x=1.9,y=2800, farmerpopchange)
}

plot_food_market = function(tt, tm) {
  barplot(
    rbind(c(tt$sold.food, tt$sold.food), c(tt$demand.food_0-tt$sold.food, tt$supply.food_max-tt$sold.food))/1000,
    main="Food market", names.arg = c("demand", "supply"), ylab = "Quantity (tons)", ylim=c(0,160)
  )
  sold.food.change = (tt$sold.food - tm$sold.food) / tm$sold.food * 100
  plotArrow(x=0.7,y=148, sold.food.change)
  plotArrow(x=1.9,y=148, sold.food.change)
}

plot_food_price = function(tt, tm) {
  barplot(tt$price.food, main="Food Price", ylim=c(0,150), names.arg = "")
  price_change = (tt$price.food - tm$price.food) / tm$price.food * 100
  plotArrow(x=0.7,y=95,price_change)
}

plot_income = function(tt, tm) {
  barplot(
    rbind(c(tt$inc.city, tt$inc.farmer), c(tt$revenue.city-tt$inc.city, tt$revenue.farmer-tt$inc.farmer)),
    main="Income", names.arg = c("city", "farm"), ylim=c(0,10000)
  )
  inc_citychange = (tt$inc.city - tm$inc.city) / tm$inc.city * 100
  inc_farmchange = (tt$inc.farmer - tm$inc.farmer) / tm$inc.farmer * 100
  plotArrow(x=0.7,y=9000,inc_citychange)
  plotArrow(x=1.9,y=9000,inc_farmchange)
}

plot_nutrition = function(tt, tm) {
  barplot(
    rbind(c(tt$satiety.city, tt$satiety.farmer)),
    main="Nutrition", names.arg = c("city", "farm"), ylim = c(0,1), border=F
  )
  satiety_citychange = (tt$satiety.city - tm$satiety.city) / tm$satiety.city * 100
  satiety_farmerchange = (tt$satiety.farmer - tm$satiety.farmer) / tm$satiety.farmer * 100
  plotArrow(x=0.7,y=0.95,satiety_citychange)
  plotArrow(x=1.9,y=0.95,satiety_farmerchange)
}

plot_happiness = function(tt, tm) {
  barplot(
    rbind(c(tt$hdi.city, 
            tt$hdi.farmer
            # tt$polit.popularity
            )),
    main="Happiness", names.arg = c("city", "farm"), ylim=c(0,1)
  )
  hdi_citychange = (tt$hdi.city - tm$hdi.city) / tm$hdi.city * 100
  hdi_farmerchange = (tt$hdi.farmer - tm$hdi.farmer) / tm$hdi.farmer * 100
  # polit_change = (tt$polit.popularity - tm$polit.popularity) / tm$polit.popularity * 100
  plotArrow(x=0.7,y=0.95,hdi_citychange)
  plotArrow(x=1.9,y=0.95,hdi_farmerchange)
  # plotArrow(x=3.1,y=0.95,polit_change)
}

# For backward compatibility, plot_state can dispatch to the above
plot_state = function(dat, which = "world_area") {
  layout(rbind(c(1,1,2,3),c(4,5,6,7)))
  par(cex.lab=1.2, cex=1.2)
  tt = dat[nrow(dat),]
  tm = dat[nrow(dat)-1,]
  plot_world_area(tt)
  plot_population(tt, tm)
  plot_food_market(tt, tm)
  plot_food_price(tt, tm)
  plot_income(tt, tm)
  plot_nutrition(tt, tm)
  plot_happiness(tt, tm)
}

