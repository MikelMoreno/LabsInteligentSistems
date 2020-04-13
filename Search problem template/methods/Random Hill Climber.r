# we will execute the Hill Climber.R function 'n' times (n = numberofrepetitions).
# each iteration of the loop will retun a solution with the one with the lowest cost (Local maximum).
# Random Hill Climber. R will compare those Local Bests and return the one with the lowest cost (Global maximum)
# Random Hill Climber.R does not examine all its neighbor before moving.
# This search algorithm selects one neighbor node at random and decides whether to choose it as a current state or examine another state.

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
  end.reason=0 # the end reason is still not specified
  BEST = 100000000 # We put a high number to compare with
  # print("EVAL of BEST: ")
  # print(BEST) 
  hillNode = node 
  bestSolution = list() # future solution of solutions
  
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  for( i in 1:numberofrepetitions ){ 
    # we execute the hill climber 'numberofrepetition' times
    print(paste0("Hill climber executed time:",i), quote = F)
    problem$state.initial = get.random.state(problem) # we assign a random state to the initial one 
    solution = Hill.Climber(problem,count.limit = count.limit, count.print = count.print, trace = TRUE) # we get the hill solution
    hillNode = get.evaluation(solution$state.final$state, problem) # we get the evaluation of that solution
    print(paste0("Local solution evaluation: ",hillNode),quote = F)
  
    # now we start comparing it to our current best solution
    # the solution of the first loop is going to be of course the best one for the moment
    if(!is.null(hillNode)){
      if (hillNode < BEST || i == 1){ # if evaluation of solution (hillNode)is better (has a lower cost) than the best one until now  
        bestSolution = solution #  keep the new solution as the best one
        BEST = hillNode # and its evaluation as the lowest cost until now
        end.reason = "Sollution"
      } 
      print(paste0("Evaluation of the best node until now: ",BEST),quote = F)
      print(" ",quote = F)
      print("================================================================",quote = F)
    } 
    #print(solution$state.final)
    # Generating the report and adding the information
    report = rbind(report, 
                   data.frame(iteration = i,
                              nodes.frontier = length(solution$state.final),
                              depth.of.expanded = solution$state.final$depth,
                              nodes.added.frontier = 1))
  }
  
  #create the result that will be returned
  result = list()
  result$name = name.method
  
  # Show the obtained (or not) final solution
  if (end.reason == "Sollution"){
    print("Best global solution found! :)", quote = F)
    to.string(bestSolution$state.final$state)
    print("Actions: ", quote = F)
    print(bestSolution$state.final$actions, quote = F)
    result$state.final = bestSolution$state.final
  } else{
    print("Best global solution found! :D", quote = F)
    to.string(bestSolution$state.final$state)
    print("Actions: ", quote = F)
    print(bestSolution$state.final$actions, quote = F)
    result$state.final = bestSolution$state.final 
  }
  
  plot.results(report, name.method,problem)
  
  return(result)
}




