# =======================================================================
# Names: Ander Eguiluz and Mikel Moreno
# Group Number: Group A
# Assignment:Assigment 2 - Hanoi Puzzle
# Date:16/03/2020
# Time spent in the assignment: 6 hours
# Proportion of effort done by members of the group: 50/50 
# Doubts and difficulties that arose during the realization: we are not very familiar with R sintax.
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

# LIBRARIES (add any needed library here)
library(rstudioapi)
library(ggplot2)
library(gridExtra)
library(gtools)

# ADDITIONAL FUNCTIONS (add any used method/problem here)

source("../methods/Breadth First Search.R")
source("../methods/Depth First Search.R")
source("../methods/Iterative Deepening Search.R")
source("../methods/Depth Limited Search.R")
source("../methods/Greedy Best First Search.R")
source("../methods/Uniform Cost Search.R")


source("../problems/Hanoi.R")

# And here, there are additional (needed) functions
source("../methods/Expand Node.R")
source("../methods/Analyze Results.R")
source("../methods/Plot Results.R")
# =======================================================================
# Check the proper operation of implemented function here!



# =======================================================================
# Solving of the problem (you have to adapt it)
problem   = initialize.problem(3,3)

res1 = Breadth.First.Search(problem, count.limit = 2000) # yes
res2 = Breadth.First.Search(problem,graph.search = T, count.limit = 2000) # yes

res3 = Depth.First.Search(problem, count.limit = 2000) # no
res4 = Depth.First.Search(problem,graph.search = T, count.limit = 2000) # yes

res5 = Iterative.Deepening.Search(problem, count.limit = 2000) # yes

all = list(res1,res2,res3,res4,res5)
analyze.results(list(res1,res2,res3,res4,res5),problem)

