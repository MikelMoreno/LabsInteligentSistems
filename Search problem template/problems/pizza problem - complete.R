# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire template folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to 
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================

# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(target = 17, 
                              pizzas = c(2, 5, 6, 8)){
  problem = list()
  problem$state.initial = runif(length(pizzas))>0.5
  problem$state.final = NULL
  problem$actions.possible = data.frame(flip = 1:length(pizzas),
                                        cost = 1)
  problem$pizzas = pizzas
  problem$target = target
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  return(TRUE)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  result = state
  result[action$flip] = !state[action$flip]
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalstate=NULL){
  return(FALSE)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  print(paste0("pizza #",which(state==T)))
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  return(1)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  if (problem$target < sum(problem$pizzas*state)){
    cost = problem$target + sum(problem$pizzas*state)
  } else{
    cost = problem$target - sum(problem$pizzas*state)
  }
	return(cost)
}

# returns a random state given a problem
get.random.state = function(problem){
  return(runif(length(problem$pizzas))>0.5)
}

