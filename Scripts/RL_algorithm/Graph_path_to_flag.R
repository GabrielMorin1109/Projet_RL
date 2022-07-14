# setting working directory
{
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(paste0(script_path, "/../../"))
  cat("Directory is currently at : \n", getwd(), "\n")
  source(file = paste0(getwd(), "/Scripts/Main.R"))
  source(paste0(getwd(), "/Scripts/RL_algorithm/GPI.R"))
}

library(ggplot2)
library(ggthemes)
library(magrittr)
# ______________________________________________________________________________

# Optimal action from a given state
{
  optimal.action <- data.frame(
    states = as.integer(
      rownames(optimal_path)
      ),
    seq_action = optimal_path
  )
}
colnames(SASRp) <- c("s","a","s'","r","t")
# sequence of state given the optimal action
{
  s <- 1
  i <- 2
  seq_state <- c(1)
  while(s!=100){
    
    a <- optimal.action[optimal.action$states == s,"seq_action"]
    
    seq_state[i] <- SASRp[SASRp[,"s"] == s,][a,"s'"]
    # next s
    s <- seq_state[i]
    i <- i+1
  }
}

# object creation for the plot
{
  maze.table <- as.data.frame.table(state)
  levels(maze.table$Var1) <-  seq_along(levels(maze.table$Var1))
  levels(maze.table$Var2) <-  seq_along(levels(maze.table$Var2))
  
  colnames(maze.table) <- c("x","y","cell")
  maze.table$wall <- is.na(maze.table$cell)*1
  maze.table$cell <- seq_along(maze.table$cell)
  trajectory <- data.frame(t = seq_along(seq_state), s = seq_state)
  
  trajectory.index <- merge(
    x = maze.table, 
    y = trajectory,
    by.y = "s",
    by.x = "cell",
    all=T
  )
}

{
  # arrangment of class
  trajectory.index$wall <- trajectory.index$wall %>% as.factor()
  trajectory.index$t <- trajectory.index$t %>% as.factor()
  
  
  maze.index <- trajectory.index
  maze.index[is.na(maze.index$t),]$cell <- NA
  maze.index$cell <- maze.index$cell %>% as.factor()
  ## plot of the GPI optimal solution
  g.maze <-
    ggplot(data = maze.index) +
    geom_tile(aes(x = y, y = rev(x), fill = as.factor(t))) +
    geom_text(aes(x = y, y = rev(x), label = as.factor(t)))+
    geom_tile(aes(x = y, y = rev(x), colour = as.factor(-1)), colour = 'blue', fill = '#00000000', size = 0.5) +
    geom_tile(aes(x = as.double(y)*(wall==1), y = as.double(rev(x))*(wall==1))) + 
    geom_tile(aes(x = 0, y = 0, colour = as.factor(-1)), fill = 'white', colour = "white") +
    xlab("")+
    ylab("")+
    ggthemes::theme_tufte() +
    theme(legend.position = "none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
    );g.maze
  if(!file.exists("Results/optimal_path.png")){
    ggplot2::ggsave(g.maze,filename = "Results/optimal_path.png")
  }
  
  # plot of the whole board 
  g.board <- ggplot(data = trajectory.index) +
    geom_text(aes(x = y, y = rev(x), label = as.factor(cell)))+
    geom_tile(aes(x = y, y = rev(x), colour = as.factor(-1)), colour = 'blue', fill = '#00000000', size = 0.5) +
    geom_tile(aes(x = as.double(y)*(wall==1), y = as.double(rev(x))*(wall==1))) + 
    geom_tile(aes(x = 0, y = 0, colour = as.factor(-1)), fill = 'white', colour = "white") +
    xlab("")+
    ylab("")+
    ggthemes::theme_tufte() +
    theme(legend.position = "none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
    );g.board
  
  if(!file.exists("Results/board.png")){
    ggplot2::ggsave(g.board,filename = "Results/board.png")
  }
}


