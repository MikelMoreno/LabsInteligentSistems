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

initialize.problem = function(rods, disks){ # we receive (#rods, #disks)
  problem = list()
  problem$state.initial = seq(1,1, length.out =disks) #initial state = [1, 1, 1, ... 1[#disks]]
  problem$state.final   = seq(rods, rods, length.out =disks) # final state = [#rods, #rods, ... #rods[#disks]]
  problem$actions.possible = permutations(rods, 2, v=c( 1:rods) , repeats.allowed=FALSE) #actions are the possible permutations between rods 
  
  problem$name = paste0("Hanoi tower of ", rods, " rods and ", disks, " disks." )
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state

# Method :
#First we look if the OrigRod has any disk to move
#Then we look if the DestRod has any disk smaller than the one on the OrigRod
#if neither of this conditions are fulfilled, we return TRUE

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
  
  # Our action is a vector of (OrigRod, DestRod)
  # vectorLength = numDisks
  
   # action <- c(1,2)
   # state <- c(1,2)
  
  i = length(state)
  
  # we iterate over the vector from top to bottom
  while (i != 0) {
    if(state[i] == action[1]){ # picks up the most small disk from OrigRod
      state[i] = action[2] # moves it to DestRod
      break() 
    }else{# if disk[i]] is not in OrigRod, look for next one  <--
      i = i-1
    }
  }
  result = state 
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
  return(result)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  print(state)
  
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

