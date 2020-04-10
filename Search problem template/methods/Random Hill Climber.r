source("../methods/Hill Climber.R") # para que importamos el hill climber.R??
                                    # la funcion de evaluacion pertenece a pizza problem.R

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
  BEST = node
  hillNode = BEST
  BEST$evaluation = 100000000 # numero elevado 
  
  # NO CONSEGUIMOS QUE COMPARE LA EVALUACIÓN DE HILLNODE (error de atomic vector)
  # NECESARIO CONSEGUIR EVALUACIÓN DE SOLUCION DE HILL.CLIMBER PARA COMPARAR CON BEST
  # UNA VEZ HECHO ESO, QUEDARSE CON ESA SOLUCION
  
  print("EVAL de BEST: ")
  print(BEST$evaluation) # nos esta devolviendo 1e+08 --> 10 elevado a 8
  print("EVAL de get.eval(hill...: ")
  print(get.evaluation(hillNode$state, problem)) # no habiamos metido el $state !!!!
  solGeneral = list()
  
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  for( i in 1:numberofrepetitions ){
    problem$state.initial = runif(length(problem$pizzas))>0.5
    solution = Hill.Climber(problem, trace = T, count.limit, count.print)
    hillNode = solution$state.final
    if (is.null(hillNode)) end.reason = " "
    if(!is.null(hillNode)){
      if (get.evaluation(hillNode$state,problem)< BEST$evaluation ){  #hillNode$evaluation es el que peta (hillNode$evaluation )
        BEST$evaluation = get.evaluation(hillNode$state, problem) # tenemos que meter el $state, sino la funcion no tira.
        solGeneral = solution
      } 
      print("EVAL del mejor nodo hasta el momento: ")
      print(BEST$evaluation)
    } 
  }
  
  result = list()
  result$name = name.method
  
  # Show the obtained (or not) final solution
  # no esta entrando aqui, entra al Hill Cliember normal
  if (end.reason == "Sollution"){
    print("Best solution found! :)", quote = F)
    to.string(BEST$state)
    print("Actions: ", quote = F)
    print(BEST$actions, quote = F) # sale null
    result$state.final = BEST$state
  } else{
    print("Best solution found! :D", quote = F)
    to.string(BEST$state)
    print("Actions: ", quote = F)
    print(BEST$actions, quote = F)
    result$state.final = BEST$state # best no tiene STATE.FINAL  
  }
  
  plot.results(report, name.method,problem)
  
  return(result)
}




