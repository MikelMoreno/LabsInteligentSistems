

Random.Hill.Climber = function(problem,
                               numberofrepetitions,
                               count.limit=100, 
                               count.print = 100, 
                               trace = FALSE){
  
  name.method = "Random.Hill Climber"
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
              cost=0,
              evaluation=get.evaluation(state.initial,problem))
  
  frontier=list(node)
  end.reason=0
  BEST = 100000000 # We put a high number to compare with 

  
  print("EVAL of BEST: ")
  print(BEST) 
  hillNode = node
  bestSolution = list() # future solution of solutions
  
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  for( i in 1:numberofrepetitions ){
    print("vuelta:")
    print(i)
    problem$state.initial = get.random.state(problem) # we assign a random state to the initial one 
    solution = Hill.Climber(problem,count.limit = count.limit, count.print = count.print, trace = TRUE) # getting the hill solution
    hillNode = get.evaluation(solution$state.final$state, problem) # getting the evaluation of that solution
  
    print("HILL NODE eval:")
    print(hillNode)
    print("HILL NODE \n")
  
    if(!is.null(hillNode)){ 
      if (hillNode < BEST ){ # if eval of solution is better than the best one, keep the new one  
        bestSolution = solution
        BEST = hillNode
        end.reason = "Sollution"
      } 
      print("EVAL of the best node until k: ")
      print(BEST)
    } 
    report = rbind(report, # Generating the report and adding the information
                   data.frame(iteration = i,
                              nodes.frontier = length(solution$state.final),
                              depth.of.expanded = solution$state.final$depth,
                              nodes.added.frontier = 1))
  }
  
  result = list()
  result$name = name.method
  
  # Show the obtained (or not) final solution
  if (end.reason == "Sollution"){
    print("Best solution found! :)", quote = F)
    to.string(bestSolution$state.final$state)
    print("Actions: ", quote = F)
    print(bestSolution$state.final$actions, quote = F)
    result$state.final = bestSolution$state.final
  } else{
    print("Best solution found! :D", quote = F)
    to.string(bestSolution$state.final$state)
    print("Actions: ", quote = F)
    print(bestSolution$state.final$actions, quote = F)
    result$state.final = bestSolution$state.final 
  }
  
  plot.results(report, name.method,problem)
  
  return(result)
}




