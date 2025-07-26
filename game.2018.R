require("rootSolve")

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

A.tot = 900   # Total area (hectares)
N.farmers = 1500  # Initial number of farmers
N.city = 1500     # Initial number of city dwellers
farmer.fulfillment = 1  # Fraction of food requirement farmer keeps for self
fert.usage = 10  # Fertilizer usage (kg/yr/hct)

K.fert_price = 200  # Fertilizer price (Rs/kg)
K.food_cons_pc = 200/1000*365    # Per capita food consumption (Kg/yr) for city
# K.food_cons_pc_farmer = 200/1000*365    # Per capita food consumption (Kg/yr) for farmer
K.farm_yield_0 = 500 # Baseline farm yield (kg/hct)
K.demand_elast = 100 # Demand elasticity (higher = less sensitive to price)
K.supply_elast = 20  # Supply elasticity (higher = less sensitive to price)
K.rev_industry = 100000 # Revenue per industrial area (Rs)
K.fert_effectiveness = 1/15 # Effectiveness of fertilizer (yield increase per kg)

#-------------------------------------------------------------------------------------
# PLOTTING FUNCTION FOR SUPPLY-DEMAND CURVES
#-------------------------------------------------------------------------------------
plot_supply_demand = function(){
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
# INITIAL WORLD STATE
#-------------------------------------------------------------------------------------

# Initial area fractions for each land type: forest, farm, city, industry
world.areas = c(0.25, .45, 0.2, 0.1)
labels = c("F", "f", "c", "i")
names(world.areas) = labels
if (abs(sum(world.areas) - 1)>1e-6) cat("Areas dont add up: ", sum(world.areas)) # Warn if areas don't sum to 1

# Dataframe to store time series of all tracked variables for plotting and analysis
dat = data.frame(
  A.forest=0, A.farm=0, A.city=0, A.ind=0, 
  demand.food_0=0, produce.farm=0, food.surplus=0, price.food=0, sold.food=0, 
  N.city=0, N.farmers=0, revenue.city=0, revenue.farmer=0, inc.city=0, inc.farmer=0, 
  satiety.city=0, satiety.farmer=0, hdi.city=0, hdi.farmer=0, polit.popularity=0
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

    if (cmd_pol != ""){
      cmd_vec = strsplit(cmd_pol, split = " ")[[1]]
      agent = cmd_vec[1]    # "P" or "F"
      command = cmd_vec[2]  # action

      # Politician commands
      if (agent == "P"){        
        if (command == "a"){    
          # Area reallocation: e.g., "P a F i 20" moves 20% of total area from Forest to Industry
          from = cmd_vec[3]     # Source area type
          to = cmd_vec[4]       # Destination area type
          amt = min(as.numeric(cmd_vec[5])/100*sum(world.areas), world.areas[from]) # Amount to move (cannot exceed available)
          world.areas[from] = world.areas[from] - amt
          world.areas[to]   = world.areas[to]   + amt
        }
        else if (command == "x"){} # Do nothing
      }
      # Farmer commands
      else if (agent == "F"){
        if (command == "f"){    
          # Set farmer's own food fulfilment (fraction of requirement kept)
          farmer.fulfillment = as.numeric(cmd_vec[3])
        }
        if (command == "p"){    
          # Set fertilizer usage (kg/yr/hct)
          fert.usage = as.numeric(cmd_vec[3])
        }
        if (command == "m"){    
          # Farmer migration: xx% of farmers migrate to city (negative = migrate from city to farm)
          migrants = round(N.farmers*as.numeric(cmd_vec[3])/100)
          N.farmers = N.farmers - migrants
          N.city    = N.city    + migrants
        }
      }
      # Game manager commands
      else if (agent == "G"){
        if (command == "supply_demand"){
          # Plot supply-demand curves
          plot_supply_demand()
        }
        else if (command == "exit"){
          # End game
          break
        }
        next
      }
      else {
        cat("Unknown command: ", command, "\n")
      }
    }  
  }  
  
  #------------------- UPDATE WORLD STATE -------------------

  # Calculate actual areas for each land type
  A.forest = world.areas[1]*A.tot
  A.farm   = world.areas[2]*A.tot
  A.city   = world.areas[3]*A.tot
  A.ind    = world.areas[4]*A.tot

  # Calculate total food demand from city population
  demand.food_0   = (N.city)*K.food_cons_pc   # Kg

  #------------------- DROUGHT DYNAMICS -------------------
  # Probability of drought is a function of forest area (less forest = higher drought risk)
  # Uses a sigmoid for smooth transition, with a sharp increase below 15% forest
  p.drought = 0.4*sigmoid(A.forest/A.tot - 0.15, -20)  
  b.drought = rbinom(n = 1, size=1, prob=p.drought) # Simulate drought event (1 = drought, 0 = no drought)
  # TODO: introduce lagged effect of forest area on drought

  #------------------- FARM SECTOR DYNAMICS -------------------
  # Farm yield increases with fertilizer usage, saturating at high usage
  yield.farm = K.farm_yield_0*2*(1-exp(-fert.usage*K.fert_effectiveness))  
  # Farmer's own food consumption (can be less than requirement if fulfilment < 1)
  cons.food_pc_farmer = K.food_cons_pc* farmer.fulfillment 
  # Total farm produce (kg)
  produce.farm = yield.farm*A.farm 
  # Food surplus = total produce minus what farmers keep for themselves
  food.surplus = produce.farm - N.farmers*cons.food_pc_farmer  
  # Maximum food available for sale (cannot be negative)
  supply.food_max = max(0, food.surplus)
  
  #------------------- FOOD MARKET DYNAMICS -------------------
  # Market equilibrium price is found where supply equals demand
  price.food = multiroot(
    f=function(x){
      supply.food_max*(1-exp(-x/K.supply_elast)) - demand.food_0*(exp(-x/K.demand_elast))
    }, 
    start = 0
  )$root   
  # Amount of food actually sold at equilibrium price
  sold.food = supply.food_max*(1-exp(-price.food/K.supply_elast))  
  # Farmer revenue per capita (from food sales)
  revenue.farmer = sold.food/N.farmers*price.food   
  # Farmer investment per capita (fertilizer cost)
  invest.farmer  = (A.farm/N.farmers)*fert.usage*K.fert_price  
  # Farmer income = revenue - investment
  inc.farmer = revenue.farmer - invest.farmer  
  # Satiety: fraction of food requirement met (capped at 1)
  satiety.farmer = min(cons.food_pc_farmer/K.food_cons_pc, 1)
  # Farmer HDI: function of satiety and income (using sigmoid for smoothness)
  hdi.farmer =  sigmoid(satiety.farmer-0.2, 5) * sigmoid(inc.farmer-3000, .001)  
  
  #------------------- CITY SECTOR DYNAMICS -------------------
  # Per capita food consumption in city (from market)
  cons.food_pc = supply.food_max*(1-exp(-price.food/K.supply_elast))/N.city 
  # City revenue per capita (from industry)
  revenue.city = A.ind/N.city * K.rev_industry
  # City food cost per capita
  cost.city = cons.food_pc*price.food
  # City income = revenue - food cost
  inc.city = revenue.city - cost.city  
  # Health proxy: ratio of forest to industry area (more forest = healthier)
  health.city = A.forest/A.ind
  # Crowding: area per city dweller (higher = less crowded)
  crowding.city = A.city/N.city 
  # Satiety: fraction of food requirement met
  satiety.city = cons.food_pc/K.food_cons_pc
  # City HDI: function of satiety, income, and crowding
  hdi.city =  sigmoid(satiety.city-0.2, 5) * sigmoid(inc.city-3000, .001) * sigmoid(crowding.city-0.05,20)   
  
  #------------------- POLITICAL SECTOR DYNAMICS -------------------
  # Tax collection: 20% of city income, 5% of farmer income
  tax.collection =  inc.city*N.city*0.20 + inc.farmer*N.farmers*0.05
  # Political popularity: function of both city and farmer HDI
  polit.popularity = sigmoid(hdi.city*hdi.farmer - 0.2, 10)
  
  #------------------- (OPTIONAL) COUNTRY MAP -------------------
  # Matrix representation of land types (for visualization)
  country = matrix(nrow=100, ncol=100, data=0)
  
  #------------------- RECORD DATA FOR THIS TURN -------------------
  dat1 = c(
    A.forest, A.farm, A.city, A.ind, demand.food_0, produce.farm, food.surplus, 
    price.food, sold.food, N.city, N.farmers, revenue.city, revenue.farmer, 
    inc.city, inc.farmer, satiety.city, satiety.farmer, hdi.city, hdi.farmer, 
    polit.popularity
  )
  dat[t,] = dat1

  #-------------------------------------------------------------------------------------
  # PLOTTING SECTION: VISUALIZE GAME STATE
  #-------------------------------------------------------------------------------------
  
  # Layout for multiple plots in one window
  layout(rbind(c(1,1,2,3),c(4,5,6,7)))
  par(cex.lab=1.2, cex=1.2)
  
  #--- Plot 1: World Area Map ---
  # Create a matrix representing the world, with each cell assigned a land type
  world = matrix(
    data = rep(c(1,2,3,4), c(round(A.forest), round(A.farm), round(A.city), round(A.ind))), 
    nrow=30, 
    byrow = F
  )
  # Find boundaries for axis labels
  b = (which(diff(c(-1,world[1,],10)) > 0))-1
  image(
    t(world), xaxt="n", yaxt="n", main="World Area", 
    col=c("green4","lightgreen", "grey", "red")
  )
  axis(
    side = 1, at = (b[-length(b)]+diff(b)/2)/30, 
    labels = c("Forest", "Farm", "City", "Ind")
  )
  
  #--- Helper function: Plot up/down arrows to indicate change ---
  plotArrow = function(x,y,value){
    if (value < 0){
      pch1 = -9660 # Unicode down arrow
      col1 = "red"
    }
    else {
      pch1 = -9650 # Unicode up arrow
      col1 = "green4"
    }
    cex = 1+abs(value)/70 # Arrow size proportional to magnitude of change
    if (value != 0) points(x=x,y=y, pch=pch1, col=col1, cex=cex)
  }

  #--- Plot 2: Population Barplot ---
  barplot(c(N.city, N.farmers), main="Population", names.arg = c("city", "farm"), ylim=c(0,3000))
  citypopchange=(dat$N.city[nrow(dat)] - dat$N.city[nrow(dat)-1])/dat$N.city[nrow(dat)-1]*100
  farmerpopchange=(dat$N.farmers[nrow(dat)] - dat$N.farmers[nrow(dat)-1])/dat$N.farmers[nrow(dat)-1]*100
  plotArrow(x=0.7,y=2800, citypopchange)
  plotArrow(x=1.9,y=2800, farmerpopchange)
  
  #--- Plot 3: Food Production (Supply vs Demand) ---
  # The barplot below shows food demand (left bar) and supply (right bar).
  # Each bar is stacked: 
  #   - The dark grey (bottom) part is the amount of food actually sold (sold.food).
  #   - The light grey (top) part is the unmet demand (left bar) or unsold supply (right bar).
  #   - Total height of the bars represents total demand and total production, respectively
  barplot(
    rbind(c(sold.food, sold.food), c(demand.food_0-sold.food, supply.food_max-sold.food))/1000, 
    main="Food Production", names.arg = c("demand", "supply"), ylab = "Quantity (tons)", ylim=c(0,150)
  )
  sold.food.change=(dat$sold.food[nrow(dat)] - dat$sold.food[nrow(dat)-1])/dat$sold.food[nrow(dat)-1]*100
  plotArrow(x=0.7,y=148, sold.food.change)
  plotArrow(x=1.9,y=148, sold.food.change)

  #--- Plot 4: Food Price ---
  barplot(price.food, main="Food\nPrice", ylim=c(0,100), names.arg = "")
  price_change=(dat$price.food[nrow(dat)] - dat$price.food[nrow(dat)-1])/dat$price.food[nrow(dat)-1]*100
  plotArrow(x=0.7,y=95,price_change)
  
  #--- Plot 5: Income (City vs Farmer) ---
  # This barplot visualizes the income and cost/investment for "city" and "farm":
  # - The dark grey bars represent income for city (inc.city) and farmer (inc.farmer).
  # - The light grey bars (stacked above) represent costs: cost.city for city and invest.farmer for farmer.
  # - Thus total height of the bars represents revenue
  barplot(
    rbind(c(inc.city, inc.farmer), c(cost.city, invest.farmer)), 
    main="Income", names.arg = c("city", "farm"), ylim=c(0,8000)
  )
  inc_citychange=(dat$inc.city[nrow(dat)] - dat$inc.city[nrow(dat)-1])/dat$inc.city[nrow(dat)-1]*100
  inc_farmchange=(dat$inc.farmer[nrow(dat)] - dat$inc.farmer[nrow(dat)-1])/dat$inc.farmer[nrow(dat)-1]*100
  plotArrow(x=0.7,y=7000,inc_citychange)
  plotArrow(x=1.9,y=7000,inc_farmchange)

  #--- Plot 6: Nutrition (Satiety) ---
  barplot(
    rbind(c(satiety.city, satiety.farmer)), 
    main="Nutrition", names.arg = c("city", "farm"), ylim = c(0,1), border=F
  )
  satiety_citychange=(dat$satiety.city[nrow(dat)] - dat$satiety.city[nrow(dat)-1])/dat$satiety.city[nrow(dat)-1]*100
  satiety_farmerchange=(dat$satiety.farmer[nrow(dat)] - dat$satiety.farmer[nrow(dat)-1])/dat$satiety.farmer[nrow(dat)-1]*100
  plotArrow(x=0.7,y=0.95,satiety_citychange)
  plotArrow(x=1.9,y=0.95,satiety_farmerchange)
  
  #--- Plot 7: Happiness (HDI and Political Popularity) ---
  barplot(
    rbind(c(hdi.city, hdi.farmer, polit.popularity)), 
    main="Happiness", names.arg = c("city", "farm", "polit"), ylim=c(0,1)
  )
  hdi_citychange=(dat$hdi.city[nrow(dat)] - dat$hdi.city[nrow(dat)-1])/dat$hdi.city[nrow(dat)-1]*100
  hdi_farmerchange=(dat$hdi.farmer[nrow(dat)] - dat$hdi.farmer[nrow(dat)-1])/dat$hdi.farmer[nrow(dat)-1]*100
  polit_change=(dat$polit.popularity[nrow(dat)] - dat$polit.popularity[nrow(dat)-1])/dat$polit.popularity[nrow(dat)-1]*100
  
  plotArrow(x=0.7,y=0.95,hdi_citychange)
  plotArrow(x=1.9,y=0.95,hdi_farmerchange)
  plotArrow(x=3.1,y=0.95,polit_change)
}

#-------------------------------------------------------------------------------------
# TODO: Implement disasters with a 10-round lag, proportional to forest area
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
