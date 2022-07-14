# Loading data
maze <- as.matrix(read.csv("Data/maze_2.csv", header = F))

# loading the function that will simulate episodes
source(file = "Scripts/Simulations/SimulateEpisodeMaze.R")

if(exists("SimulateEpisodeMaze")){
  cat("SimulateEpisodeMaze function has been import to the environment.")
}

# loading the miscellaneous functions
source(file = "Scripts/RL_algorithm/miscfunc.R")

if(exists("QtoMaze")){
  cat("\n QtoMaze function has been imported to the environment.")
}
