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

# tests para ver que pasa, si he puesto como 2000 comentarios lo siento de antemano. Mikel <3
# si lo ejecutas pinta cosas pero no encuentra una respuesta, pero no se si es por el problema o por el metodo.
# data.frame(include = 1:4, cost = 1)
# pizzas = c(2,5,6,8)
# state = c(FALSE, FALSE, TRUE, FALSE)
# current = sum(pizzas*state)
# target = 17

initialize.problem = function(target = 17, 
                              pizzas = c(2, 5, 6, 8)){
  problem = list()
  problem$state.initial = pizzas == 0 
  problem$state.final = NULL 
  problem$actions.possible = data.frame(include = 1:length(pizzas), cost = 1) 
  problem$pizzas = pizzas 
  problem$target = target 
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  # result = FALSE
  current = sum(problem$pizzas*state)
  result = (current + problem$pizzas[action$include]) <= problem$target & state[action$include] == F 
  return(result)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  result = state 
  result[action$include] = TRUE 
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
  cost = problem$target - sum(problem$pizzas*state)
	return(cost)
}


#before returning a random state, checks if it doesn't surpass the "target".
get.random.state = function(problem){
  st = runif(length(problem$pizzas))>0.5
  
  while(sum(problem$pizzas*st) > problem$target){
    st = runif(length(problem$pizzas))>0.5
  }
  return(st)
}
