Local.Beam.Search = function(problem,
                             count.limit=100, 
                             count.print = 100, 
                             k,
                             trace = FALSE){
  
  name.method = "Local Beam Search"
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  frontier = list()  
  successors = list()
  for (i in 1:k){    
    node = list(parent=c(),
                state= runif(length(problem$pizzas))>0.5,
                actions=c(),
                depth=0,
                cost=0,
                evaluation=get.evaluation(state.initial,problem))
    
   #frontier[[i]]= node # k random nodes in the frontier
    frontier = c(list(node), frontier) #we have our list of all expanded nodes
    
  }
  
  frontier = frontier[order(sapply(frontier, function (x) x$evaluation))] #we order the frontier
  
  bestNode = frontier[[1]]
  
  count = 1
  end.reason = 0
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  
  # while (count<=count.limit){ # o length(frontera)==0 //lo quitamos porq no añade ahora valor
  while (count<= count.limit){                                   
    
    for( i in 1:k){ # Take out nodes from frontier 
      firstnode = frontier[[1]]
      frontier[[1]] = NULL
      
      print(i)
      
      newnodes = expand.node(firstnode, actions.possible) #expand those nodes
      for(a in 1:length(newnodes)){ # insert node by node in a list of successors
        node = newnodes[a]
        successors = c(list(node), successors) #we have our list of all expanded nodes
      }
    }
    successors = successors[order(sapply(successors,function (x) x$evaluation))] #we order the list of successors 
    localBest = successors[[1]] #we have our localBest here
    
    best = ifelse(best$evaluation > localBest$evaluation, localBest, best)                                           
    
    for(e in k:length(frontier)){
      frontier[[k+1]] = NULL # We keep the best successors nodes in the frontier
    }                                     
    report = rbind(report,
                   data.frame(iteration = count,
                              nodes.frontier = length(frontier),
                              depth.of.expanded = firstnode$depth,
                              nodes.added.frontier = 1))
    
    count = count+1
  }
  
  # A PARTIR DE AQUÍ YA MANDA LOS RESULTADOS  
  result = list()
  result$report = report
  result$name = name.method
  
  # Show the obtained (or not) final solution
  if (end.reason == "Sollution"){
    print("Best solution found!!", quote = F)
    to.string(firstnode$state)
    print("Actions: ", quote = F)
    print(firstnode$actions, quote = F)
    result$state.final = firstnode
  } else{
    if (end.reason == "Frontier"){
      print("Best solution found!!", quote = F)
      to.string(firstnode$state)
      print("Actions: ", quote = F)
      print(firstnode$actions, quote = F)
      result$state.final = firstnode
    } else{
      print("Best solution found!!", quote = F)
      to.string(firstnode$state)
      print("Actions: ", quote = F)
      print(firstnode$actions, quote = F)
      result$state.final = firstnode
    }
    result$state.final = NA
  }
  
  plot.results(report,name.method,problem)
  
  return(result)
}