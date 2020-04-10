# =======================================================================
# Names: Ander Eguiluz and Mikel Moreno
# Group Number: ?
# Assignment: Assignment 3 - Pizza Problem
# Date:
# Time spent in the assignment:
# Proportion of effort done by members of the group: 
# Doubts and difficulties that arose during the realization:
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
# Configuring the Environment
rm(list=ls())
cat("\014")
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
dir("../methods")
dir("../problems")

# LIBRARIES (add any needed library here)
library(rstudioapi)
library(ggplot2)
library(gridExtra)

# ADDITIONAL FUNCTIONS (add any used method/problem here)
source("../problems/pizza problem.R")
source("../methods/Breadth First Search.R")
source("../methods/Greedy Best First Search.R")
source("../methods/Hill Climber.R")
source("../methods/LocalBeamSearch.R")
source("../methods/Random Hill Climber.R") # no lo habiamos añadido

# And here, there are additional (needed) functions
source("../methods/Expand Node.R")
source("../methods/Analyze Results.R")
source("../methods/Plot Results.R")
# =======================================================================
# Check the proper operation of implemented function here!



# =======================================================================
# Solving of the problem (you have to adapt it)
problem   = initialize.problem(target = 100, pizzas = c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,96))
#res1 = Breadth.First.Search(problem, count.limit = 2000, graph.search = T, trace = F)
res4 = Local.Beam.Search(problem,count.limit=100, count.print = 100, k= 3, trace = FALSE)
#res2 = Greedy.Best.First.Search(problem, trace = T)
res3 = Random.Hill.Climber(problem, trace = T, count.limit = 1000, count.print = 1) # no habiamos puesto RANDOM!
analyze.results(list(res4, res3),problem)

# Similar for complete state
source("../problems/pizza problem - complete.R")
problem   = initialize.problem(target = 100, pizzas = c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,96))
res4 = Local.Beam.Search(problem,count.limit=100, count.print = 100, k= 3, trace = FALSE)
res1 = Breadth.First.Search(problem, count.limit = 100, graph.search = T, trace = F)
#res2 = Greedy.Best.First.Search(problem, trace = T)
res3 = Hill.Climber(problem, trace = T, count.limit = 10)
analyze.results(list(res4, res3),problem)
#analyze.results(list(res3), problem)
