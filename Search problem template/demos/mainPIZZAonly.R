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
source("../methods/Random Hill Climber.R")

# And here, there are additional (needed) functions
source("../methods/Expand Node.R")
source("../methods/Analyze Results.R")
source("../methods/Plot Results.R")
# =======================================================================
# Check the proper operation of implemented function here!



# =======================================================================
# Solving of the problem (you have to adapt it)
problem   = initialize.problem(target = 100, pizzas = c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,96))
res1 = Random.Hill.Climber(problem, 5, trace = T, count.limit = 1000, count.print = 1)
res2 = Local.Beam.Search(problem, trace = T, count.limit = 1000, count.print = 1, k=3)

