# =======================================================================
# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(file){
  problem = list()
  problem$state.initial = read.csv(file, header = F)
  problem$actions.possible = data.frame(value=1:9)
  problem$name = paste0("Sudoku - [",file,"]")
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  
  value = action
  
  where.put = which(state==0, arr.ind = T)[1,]
  where.is  = which(state==value, arr.ind = T)
  
  app.row = any(where.is[,1] == where.put[1])
  app.col = any(where.is[,2] == where.put[2])
  
  square = floor((where.put-0.01)/3)
  square = (square*3)+1
  square = state[square[1]:(square[1]+2),square[2]:(square[2]+2)]
  app.squ = any(square == value)
  
  return(!app.row & !app.col & !app.squ)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  result = state
  where.put = which(state==0, arr.ind = T)[1,]
  result[where.put[1], where.put[2]] = action
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited

is.final.state = function (state,finalstate=NULL){
  return(length(which(state==0))==0)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
    print(state)

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
	return(1)
}