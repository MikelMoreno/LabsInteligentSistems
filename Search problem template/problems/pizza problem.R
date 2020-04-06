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
  problem$state.initial = pizzas == 0 # crea un vector booleano que pone todo false, no hemos cogido ninguna pizza
  problem$state.final = NULL # no hay un estado final claro, puede haber varios finales (ejemplo: 8 = una pizza de 8 o 2 pizzas de 2 y 6).
  problem$actions.possible = data.frame(include = 1:length(pizzas), cost = 1) # cada eleccion de coger una pizza tiene un coste, el inicial es todos a 1
  problem$pizzas = pizzas # lista de tipos de pizza posibles (no puedes coger 2 pizzas del mismo tipo)
  problem$target = target # numero de personas, un slice por persona --> nuestro maximo de slides
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  # result = FALSE
  current = sum(problem$pizzas*state)# devuelve cuantos slices llevamos, podemos suponer que state es un vector de 1 o 0 (o booleano TRUE/FALSE). Por eso multiplica state
  result = (current + problem$pizzas[action$include]) <= problem$target & state[action$include] == F # si sumando lo que llevamos (current) y la accion (action) no nos pasamos del limite de slices (target); y si no hemos usado antes dicha accion, entonces devuelve true
  return(result)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  result = state # el state es un vector de TRUE/FALSE de longitud =len(pizzas)
  result[action$include] = TRUE # quiere decir que hemos usado esta pizza, para no repetir.
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

