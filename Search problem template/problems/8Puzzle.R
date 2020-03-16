library(ggplot2) 
library(gridExtra)


# =======================================================================
initialize.problem = function(rows=3,columns=3,perm = sample(0:(rows*columns-1))){
  problem = list()
  problem$state.initial = matrix(perm,nrow=rows,byrow = TRUE)
  problem$state.final   = matrix(0:(rows*columns-1),nrow=rows,byrow = TRUE)
  problem$actions.possible = data.frame(direction=c("Up","Left","Down","Right"), stringsAsFactors = F)
  problem$rows = rows
  problem$columns = columns
  problem$name = paste0("8-Puzzle (",rows,"x",columns,") - [",paste0(perm,collapse="-"),"]")
  return(problem)
}

# =======================================================================
is.applicable = function (state,action,problem){
  where = which(state==0, arr.ind=TRUE)
  row = where[1]
  col = where[2]  
  if (action == "Up"){
    result = row!=1
  }
  if (action == "Down"){
    result = row!=problem$rows
  }
  if (action == "Left"){
    result = col!=1
  }
  if (action == "Right"){
    result = col!=problem$columns
  }
  return(result)
}

# =======================================================================
effect = function (state,action){
  where = which(state==0, arr.ind=TRUE)
  row = where[1]
  col = where[2]
  result = state  
  if (action == "Up"){
    result[row-1,col]=state[row,col]
    result[row,col]=state[row-1,col]
  }
  if (action == "Down"){
    result[row+1,col]=state[row,col]
    result[row,col]=state[row+1,col]
  }
  if (action == "Left"){
    result[row,col-1]=state[row,col]
    result[row,col]=state[row,col-1]
  }
  if (action == "Right"){
    result[row,col+1]=state[row,col]
    result[row,col]=state[row,col+1]
  }
  return(result)
}


# =======================================================================
is.final.state = function (state,finalstate){
  return(sum(state == finalstate)==(nrow(state)*ncol(state)))
}

# =======================================================================
to.string = function (state){
  for (i in 1:nrow(state)){
    print(state[i,])
  }
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