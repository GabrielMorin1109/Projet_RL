# setting working directory
{
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(paste0(script_path, "/../../"))
  cat("Directory is currently at : \n", getwd(), "\n")
  source(file = paste0(getwd(), "/Scripts/Main.R"))
}
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#---------------------------------------------------
# SASRp matrix
#---------------------------------------------------

#Creation of the matrix SASRp
setofstates <- c(na.omit(c(state)))
SASRp <- matrix(0,nrow=4*length(setofstates),ncol=5)
colnames(SASRp) = c('S', 'A', "`S'`", 'R', 'P')

SASRp[,1]<-rep(setofstates,each=4) #Column of s.
SASRp[,2]<-rep(1:4,length(setofstates)) #Column of A(s) = {1,2,3,4}, for all s.
SASRp[,5]<-1
SASRp[SASRp[,1]==100,5]<-0 #terminal state

rewards <- c(-1,  0, -1)

#Column of s' 
#States at the border of the maze (subsets of border states set)
sub_s1<-c(na.omit(state[1,]))
sub_s2<-c(na.omit(state[nrow(state),]))
sub_s3<-c(na.omit(state[,1]))
sub_s4<-c(na.omit(state[,ncol(state)]))

for(i in 1:nrow(SASRp)){
  
  current_action <- SASRp[i,2]
  current_state <- SASRp[i,1]
  
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
  Rwrd<-ifelse(next_state == current_state,rewards[3],rewards[1]) #If the agent explore states already known, r[3], else r[1]
  Rwrd<-ifelse(next_state==100,rewards[2],Rwrd) #If the agent attains the exit, the reward is positive r[2]
  SASRp[i,4]<-Rwrd #store reward
  
  #Store next_state
  SASRp[i,3]<-next_state
  
}

View(SASRp)

#---------------------------------------------------
# Calulate Policy Value Function
#---------------------------------------------------
Policy <- read.table("Data/OptPolTies.txt")
DF = 0.5

#Calculation of b
b<-rep(0, length(setofstates))
i = 1
for(s in setofstates){
  for(a in 1:4){
    index <- which(SASRp[,1]==s & SASRp[,2]==a)
    b[i] <- b[i] + sum((SASRp[,4]*SASRp[,5])[index] * Policy[i, a])
  }
  i = i + 1
}

names(b) <- setofstates
b

#Calculation of M
M <- matrix(0,nrow=length(setofstates),ncol=length(setofstates))
colnames(M) <- rownames(M) <- setofstates

i = 1
for(s in setofstates){
  j = 1
  for(`s'` in setofstates){
    for(a in 1:4){
    index <- which(SASRp[,1]==s & SASRp[,2]==a & SASRp[,3]==`s'`)
    
    M[i,j] <- ifelse(length(index)>0, 
                     M[i,j] + DF * SASRp[index,5] * Policy[i, a],
                     M[i,j]
    )
    }
    j = j + 1
  }
  i = i + 1
}

PolicyVF <- solve(diag(nrow(M))-M)%*%b
rownames(PolicyVF) <- setofstates
PolicyVF
QtoMaze(round(PolicyVF, 1), maze)

#---------------------------------------------------
# Calculate of Policy Action Value Function
#---------------------------------------------------

PolicyAVF <- matrix(0, nrow = nrow(PolicyVF), ncol = 4)
i = 1
for(s in setofstates){
  for(a in 1:4){
    j = 1
    for(`s'` in setofstates){
      index <- which((SASRp[,1]==s)*(SASRp[,2]==a)*(SASRp[,3]==`s'`)==TRUE)
      PolicyAVF[i, a] <- ifelse(length(index)>0,
                                PolicyAVF[i, a] + SASRp[index,5] * (SASRp[index,4] + DF * PolicyVF[j]),
                                PolicyAVF[i, a]
      )
      j = j + 1
    }
  }
  i = i + 1
}
colnames(PolicyAVF) <- c("left", "up", "right","down")
rownames(PolicyAVF) <- setofstates
PolicyAVF
QtoMaze(apply(PolicyAVF, 1, which.max), maze, arrows = TRUE, solution_path = FALSE)
QtoMaze(apply(PolicyAVF, 1, which.max), maze, arrows = TRUE, solution_path = TRUE)
