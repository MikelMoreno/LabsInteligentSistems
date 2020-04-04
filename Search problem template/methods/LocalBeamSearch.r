Hill.Climber = function(problem,
                        count.limit=100, 
                        count.print = 100, 
                        k,
                        trace = FALSE){
  
  name.method = "Hill Climber"
  state.initial    = problem$state.initial
  state.final      = problem$state.final
  actions.possible = problem$actions.possible
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
              cost=0,
              evaluation=get.evaluation(state.initial,problem))
  frontier = list(node)
  
  count = 1
  end.reason = 0
  report = data.frame(iteration=numeric(),
                      nodes.frontier=numeric(),
                      depth.of.expanded=numeric(),
                      nodes.added.frontier=numeric())
  
  
  while (count<=count.limit){ # o length(frontera)==0 //lo quitamos porq no añade ahora valor
    
    firstnode = frontier[[1]]
    frontier[[1]] = NULL
    
    newnodes = expand.node(firstnode, actions.possible)
    newnodes = newnodes[order(sapply(newnodes,function (x) x$evaluation))] #ordenar los nuevos nodos por función de evaluación

    if (length(newnodes)){
      
      #para el local beam search
      k #numero de mejores nodos a explorar 
      
      while(i<=k){
        newbetternodes = newnodes[[i]] #lista de nuevos mejores nodos      
        for(newnode in newbetternodes){
          if (firstnode$evaluation > newnode$evaluation){
            frontier = c(list(newnode),frontier)
          } else{ # HA ENCONTRADO EL NODO PICO/VALLE
            soluciones = list()
            end.reason = "LocalSollution" # or "worse situation"            
            break
	        }        
        }
        #newnode = newbetternodes[[1]]
        i++
      }
    }
    
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
