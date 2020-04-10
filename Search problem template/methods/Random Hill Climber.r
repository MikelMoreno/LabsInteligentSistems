source("../methods/Hill Climber.R")

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
    print("vuelta:")
    print(i)
    problem$state.initial = get.random.state(problem)
    solution = Hill.Climber(problem,count.limit = count.limit, count.print = count.print, trace = TRUE)
    print("ha pasado hill climber")
    print(solution)
    hillNode = solution$state.final # no es nada
    print(hillNode)
    if (is.null(hillNode)) end.reason = " "
    if(!is.null(hillNode)){
      if (get.evaluation(hillNode$state,problem)< BEST$evaluation ){  #hillNode$evaluation es el que peta (hillNode$evaluation )
        #BEST$evaluation = get.evaluation(hillNode$state, problem) # tenemos que meter el $state, sino la funcion no tira.
        #solGeneral = solution
        print("ha entrado en el IF")
        BEST = hillNode
        end.reason = "Sollution"
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




