# Read csv file containing maze

if(!exists("maze")){
  maze <- as.matrix(read.csv("Data/maze_2.csv", header = F))
}



#Modifications of the maze
maze[1]=0
maze[24]=0

state <- maze

for(i in 1:(prod(dim(state)))){
  
  state[i]<-ifelse(state[i]==1,NA,i)
  
}

cat("Mapping of states : \n")
print(state) #NA's are the walls (which are not states)

#Possible actions in each state
#1 : LEFT
#2 : UP
#3 : RIGHT
#4 : DOWN
action <- c(1,2,3,4)

#If the agent hits a wall, he stays at the same state he was before. 

#Function that simulates an episode of the maze :

#Vector of probabilities (probabilities of going left, up, right, and down respectively)
prob <- rep(1/4,4)

#Let's suppose the agent starts at position (1,1) of the maze for each episode.

#Vector of possible rewards
#Possible rewards are -0.1 if the player moves, 1 if he attains the exit, and
#-0.25 if he explores states that have already been explored. 
rewards <- c(-0.1,1,-0.25)


SimulateEpisodeMaze <- function(state,prob,rewards){
  
  
  stateseq <- c(1)
  actionseq <- c()
  rewardseq <- c()
  
  #States at the border of the maze (subsets of border states set)
  sub_s1<-c(na.omit(state[1,]))
  sub_s2<-c(na.omit(state[nrow(state),]))
  sub_s3<-c(na.omit(state[,1]))
  sub_s4<-c(na.omit(state[,ncol(state)]))
  
  #Border states set
  BORDERS <- unique(c(sub_s1,sub_s2,sub_s3,sub_s4))
  
  i <- 1 #iterative term
  next_state<-1 #Must have a predefined value of next_state 
  while(next_state!=100){
    
    actionseq[i]<-sample(x = action,size = 1,prob = prob)
    
    current_action <- actionseq[i]
    current_state <- stateseq[i]
    
    #Value of the next state (ifelse structure depending on the action taken)
    next_state <- ifelse(current_action==1,current_state-10,ifelse(current_action==2,current_state-1,ifelse(current_action==3,current_state+10,current_state+1)))
    
    
    #Value of the next state if the current states is in the border states of the maze
    next_state <- ifelse((current_state%in%sub_s3)&&(current_action==1),current_state,next_state)
    
    next_state <- ifelse((current_state%in%sub_s1)&&(current_action==2),current_state,next_state)
    
    next_state <- ifelse((current_state%in%sub_s4)&&(current_action==3),current_state,next_state)
    
    next_state <- ifelse((current_state%in%sub_s2)&&(current_action==4),current_state,next_state)
    
    
    #This calculates the next state in case the agent hits a wall in the maze
    
    next_state <- ifelse(is.na(state[next_state]),current_state,next_state) #If hits a wall stays at the same position
    
    
    #Rewards
    Rwrd<-ifelse(next_state%in%stateseq,rewards[3],rewards[1]) #If the agent explore states already known, r[3], else r[1]
    Rwrd<-ifelse(next_state==100,rewards[2],Rwrd) #If the agent attains the exit, the reward is positive r[2]
    rewardseq[i]<-Rwrd #store reward
    
    #Store next_state
    if(next_state!=100) {stateseq[i+1] <- next_state}
    
    
    i <- 1+i
  }
  
  
  EpisodeOutcomes <- rbind(stateseq,actionseq,rewardseq)
  return(EpisodeOutcomes)
}
