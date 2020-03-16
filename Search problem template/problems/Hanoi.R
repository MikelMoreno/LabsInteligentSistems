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
initialize.problem = function(rods, disks){
  problem = list()
  problem$state.initial = seq(1,1, length.out =disks)
  problem$state.final   = seq(rods, rods, length.out =disks)
  #problem$actions.possible = (data.frame(orig = 1:rods, dest = 1:rods))
  problem$actions.possible = permutations(rods, 2, v=c( 1:rods) , repeats.allowed=FALSE)
  problem$name = paste0("Hanoi tower of ", rods, " rods and ", disks, " disks." )
  # problem$<aditional info> = <Insert code here>
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action, problem){
  discosXVar1 = which(state==action[1])
  if(length(discosXVar1)!= 0){
    pDisco1 =discosXVar1[length(discosXVar1)]  
  }else{
    pDisco1=0
  }
  
  discosXVar2 = which(state==action[2])
  if(length(discosXVar2)!= 0){
    pDisco2 =discosXVar2[length(discosXVar2)]  
  }else{
    pDisco2=0
  }
  print(pDisco2)
  
  if (pDisco1 > pDisco2){
    result = TRUE
  
  }else{
    result = FALSE
  }
  
  
  
  # <insert code here in order to calculate result value>
  return(result)
}

# problema = initialize.problem(3,3)
# is.applicable(state = problema$state.initial, action = c(2,1), problem = problema)
# effect(state = problema$state.initial, action = c(1,2))
# =======================================================================
# Must return the state resulting on applying the action over the state


effect = function (state,action){ 
  # Funciona, tal vez hay que hacer otro test mas.
  
  # Decimos que el formato de action es un vector de (VarillaOrigen, VarillaDestino)
  # Sacamos tamaño del vector = numero de discos
  
   # action <- c(1,2)
   # state <- c(1,2)
  i = length(state)
  
  # recorremos el vector de alante hacia atras
  while (i != 0) {
    if(state[i] == action[1]){ # coje el disco de más arriba de la varilla origen
      state[i] = action[2] # mueve el disco a la varilla destino
      break() # no necesitamos mirar mas
    }else{# si el disco [i] no esta en la varilla origen, mira el siguiente  <--
      i = i-1
    }
  }

  result = state
  # <insert code here in order to modify the resulting state> 
  # print(result)
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited

# ahora hace una comparacion buena usando estos metodos. FUNCIONA
is.final.state = function (state, finalstate){
  # state <- c(1,1)
  # finalstate<- c(1,3)
  if(all(length(state)==length(finalstate)) && all(state==finalstate)){
    result = TRUE
  }else{
    result = FALSE
  }
  result
  # <insert code here in order to check if a state is final> 
  return(result)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  # <insert code here to print the state> 
  print(state)
  # for (i in state){
  #   print(state[i])
  # }

  
  # <try to print the state in the most visual way>
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  # Return the cost of applying an action over a state
  return(1)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
	return(1)
}

