library(ggplot2) 
library(gridExtra)


# =======================================================================
initialize.problem = function(){
  problem = list()
  problem$state.initial = data.frame(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
  problem$actions.possible = data.frame(action=c("farmer","wolf","goat","cabbage"), stringsAsFactors = F)
  problem$name = paste0("River crossing puzzle")
  return(problem)
}

# =======================================================================
is.applicable = function (state,action,problem){
  if (action == "farmer"){
    result = (state$wolf != state$goat) && (state$goat != state$cabbage)
  }
  if (action == "wolf"){
    result = (state$goat != state$cabbage) && (state$farmer == state$wolf)
  }
  if (action == "goat"){
    result = (state$farmer == state$goat)
  }
  if (action == "cabbage"){
    result = (state$goat != state$cabbage) && (state$farmer == state$cabbage)
  }
  return(result)
}

# =======================================================================
effect = function (state,action){
  result = state
  result$farmer = !result$farmer
  if (action == "wolf"){
    result$wolf = !result$wolf
  }
  if (action == "goat"){
    result$goat = !result$goat
  }
  if (action == "cabbage"){
    result$cabbage = !result$cabbage
  }
  return(result)
}


# =======================================================================
is.final.state = function (state,finalstate){
  return(all(state==FALSE))
}

# =======================================================================
to.string = function (state){
    print(state)
}

# =======================================================================
get.cost = function (action,state){
  return(1)
}

# =======================================================================
# (Used for Informed Algorithms)
get.evaluation = function(state,problem){
  return(1)
}