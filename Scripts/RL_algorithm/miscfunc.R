#--------------------------------------------------
# Miscellaneaous functions
#--------------------------------------------------
# Display arrows
arrows <- function(x){
  sapply(1:length(x), function(i)
    if(x[i]==1) '\U02190'
    else if(x[i]==2) '\U02191'
    else if(x[i]==3) '\U02192'
    else if(x[i]==4) '\U02193'
  )
}

# QtoMaze
QtoMaze <- function(Qmat, maze, arrows = FALSE, solution_path = FALSE){
  # Find path to solution
  if(solution_path){
    path = 1
    i = 0
    while(path[i+1]!=100){
      i = i +1
      path[i+1] <- ifelse(Qmat[as.character(path[i])]==1,path[i]-10,
                          ifelse(Qmat[as.character(path[i])]==2,path[i]-1,
                                 ifelse(Qmat[as.character(path[i])]==3,path[i]+10,
                                        path[i]+1))
      )
      if(path[i+1] %in% path[-(i+1)] | path[i+1]<1 | is.na(path[i+1])) {
        print("The action value function has not converged to the optimum (solution_path = FALSE).")
        solution_path = FALSE
        break
      }
    }
  }
  
  # Display arrows
  if (arrows) Qmat <- arrows(Qmat)
  
  # Map states on maze
  walls <- which(maze == 1) 
  for(i in walls){
    Qmat <- append(Qmat, 'X', after = i-1)
  }
  Qmat[length(maze)] <- "END"
  
  if(solution_path) Qmat[-c(walls, path)] <- ""
  
  matrix(Qmat, nrow = nrow(maze), ncol = ncol(maze))
}
