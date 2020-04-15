#We are going to expand the nodes keeping the best 'k' nodes as long as we don't get a goal


Local.Beam.Search = function(problem,
                             count.limit=100, 
                             count.print = 100, 
                             k,
                             trace = FALSE){
  
  name.method = "Local Beam Search"
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  frontier = c()  
  successors = c()
  for (i in 1:k){    
    state = get.random.state(problem) # This is the right way of creating a random state for every iteration
    node = list(parent=c(),
                state= state,
                actions=c(),
                depth=0,
                cost=0,
                evaluation=get.evaluation(state,problem))
    

    frontier = c(list(node), frontier) #We have our list of all nodes
    
  }
  
  frontier = frontier[order(sapply(frontier, function (x) x$evaluation))] #We order the frontier
  
  bestNode = frontier[[1]] # Now the best node is bestNode
  
  count = 1
  end.reason = 0
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  
  while (count<= count.limit){                                   
    
    for( i in 1:k){ # Take out nodes from frontier 
      firstnode = frontier[[1]]
      frontier[[1]] = NULL
      # print(i) #test for showing the program enters and exits the loop
      newnodes = expand.node(firstnode, actions.possible) #Expand those nodes
      successors = c(newnodes, successors) #We have our list of all expanded nodes
    }
    successors = successors[order(sapply(successors,function (x) x$evaluation))] #We order the list of successors 
    frontier = head(successors, n = k) # We update the frontier with the best k first nodes
    
    report = rbind(report, # Generating the report and adding the information
               data.frame(iteration = count,
                          nodes.frontier = length(frontier),
                          depth.of.expanded = firstnode$depth,
                          nodes.added.frontier = 1))

    count = count+1
  }
  
 ## Now we deal with the report handling 
  firstnode = frontier[[1]]
  end.reason = "Sollution"
  
  if(is.null(firstnode)){
    end.reason = "Frontier"
  }
  
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