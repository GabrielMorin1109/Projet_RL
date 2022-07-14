# setting working directory
{
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(paste0(script_path, "/../../"))
  cat("Directory is currently at : \n", getwd(), "\n")
  source(file = paste0(getwd(), "/Scripts/Main.R"))
}
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source("Scripts/RL_algorithm/miscfunc.R")
source("Scripts/RL_algorithm/PolicyAVF.R")

Policy <- read.table(paste0(getwd(),"/Data/OptPolicy.txt"))
OptPolTies <- read.table(paste0(getwd(),"/Data/OptPolTies.txt"))
#-----------------------------------------------------------------
#Dynamic programming approach - GPI
#-----------------------------------------------------------------
#-----------------------------------------------------------------
#Creation of the matrix SASRp
setofstates <- c(na.omit(c(state)))
setofstates[-length(setofstates)]
SASRp <- matrix(0,nrow=4*length(setofstates[-length(setofstates)]),ncol=5)


SASRp[,1]<-rep(setofstates[-length(setofstates)],each=4) #Column of s.
SASRp[,2]<-rep(1:4,length(setofstates[-length(setofstates)])) #Column of A(s) = {1,2,3,4}, for all s.
SASRp[,5]<-1

rewards <- c(-1,0)

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
  Rwrd<-ifelse(next_state!=100,rewards[1],rewards[2]) #If the agent explore states already known, r[3], else r[1]
  SASRp[i,4]<-Rwrd #store reward
  
  #Store next_state
  SASRp[i,3]<-next_state
  
}
#-----------------------------------------------------------------
#OUTPUT : 
SASRp
#-----------------------------------------------------------------

#-----------------------------------------------------------------
#Policy Evaluation 
#-----------------------------------------------------------------
###Inputs### 
#StartVF : A vector containing the initial estimate of the state-value function
#Policy : A vector containing a deterministic policy, i.e. the action in both of the states
#SASRp : Transition probability table (matrix)
#DF : The discount factor (real number >0)
#niter : Number of iterations of policy evaluation applied


PolicyEvaluation <- function(StartVF,Policy,SASRp,DF,niter){
  
  v_pi <- StartVF
  temp<-c()
  setofstates <- unique(SASRp[,1])
  for(k in 1:niter){
    
    
    j <- 1 #corresponds to the index of considered state in vector setofstates
    for(i in Policy){
      s <- setofstates[j]
      
      indexes<- which(as.logical((SASRp[,1]==s)*(SASRp[,2]==i))) #Rows which corresponds to s,a
      index_vpi <- which(setofstates%in%SASRp[,3][indexes])
      next_state <- SASRp[indexes,3]
      temp[j]<-ifelse(next_state==100,sum((SASRp[,5][indexes])*(SASRp[,4][indexes])),sum((SASRp[,5][indexes])*(SASRp[,4][indexes]+DF*v_pi[index_vpi]))) #Application of Bellman equation
      j <- j+1 #update next index
      temp
    }
    v_pi <- temp
  }
  return(v_pi)
  
}


#-----------------------------------------------------------------
#Policy Improvement 
#-----------------------------------------------------------------

PolicyImprovement<-function(VFEstim,SASRp,DF){
  
  v_pi <- VFEstim
  bestaction <- c()
  
  S <- unique(SASRp[,1])
  f <- 1 #iterative variable
  
  for(j in S){
    temp <- c() #Temporary stockage of the expected reward for each j
    indexes<-which((SASRp[,1]==j)==TRUE) #Rows indexes which corresponds to state s
    indexes 
    
    A_s <- unique(SASRp[indexes,2]) #Unique action in A(s)
    
    k<-1 #Iterative variable
    
    for(i in A_s){
      n_index<-indexes[which(SASRp[indexes,2]==i)]
      index_vpi <- which(setofstates%in%SASRp[,3][n_index])
      next_state <- SASRp[n_index,3]
      
      temp[k] <- ifelse(next_state==100,sum(SASRp[,5][n_index])*(SASRp[,4][n_index]),sum(SASRp[,5][n_index])*(SASRp[,4][n_index]+DF*v_pi[index_vpi])) #Expected Reward given the an action and state s
      k <- k+1 #Update of k 
      
    }
    temp
    bestaction[f]<- A_s[which.max(temp)] #Greedy action
    f <- f +1
    
  }   
  return(bestaction)  
}

#Inputs
#StartVF : inital value function estimate (vector of size |S|-1 (-1 beacause of s*))
#SASRp : transition matrix (must not contain s*=100 in the first column)
#DF: discount factor
#niter : number of iteration in the GPI
#niter2 : number of iteration in the policy evaluation
#PolicyAVF : matrix Optimal Q(s,a) (|S|x4)
#PolicyTies: matrix containing the policy that considers ties

GPI<-function(StartVF,SASRp,DF,niter,niter2,PolicyAVF,PolicyTies){
  numiter<-c()
  v_star <- StartVF
  setstates <- unique(SASRp[,1])
  RMSE <- c()
  Q <- array(0,dim=c(nrow(PolicyAVF),ncol(PolicyAVF),niter))
  Qestim <- matrix(0,nrow=nrow(PolicyAVF),ncol=ncol(PolicyAVF))
  for(i in 1:niter){
    
    Policy <- PolicyImprovement(StartVF,SASRp,DF) #Apply Policy Improvement Iteration 
    Policy
    new_VF <- PolicyEvaluation(StartVF,Policy,SASRp,DF,niter2) #Apply Policy evaluation (a sweep)
    StartVF <- new_VF
    numiter[i] <- ifelse(sum(Policy!=apply(PolicyAVF,1,which.max)[-nrow(PolicyAVF)])==0,1,0)
    
    #Estimate of the action-value function
    for(s in setstates){ #sweep states
      which(setstates==s)
      for(a in 1:4){ #sweep actions
        
        index_s <- which(s==setstates)
        next_state <- SASRp[as.logical((SASRp[,1]==s)*(SASRp[,2]==a)),3]
        reward <- SASRp[as.logical((SASRp[,1]==s)*(SASRp[,2]==a)),4]
        p <- SASRp[as.logical((SASRp[,1]==s)*(SASRp[,2]==a)),5]
        vpi_sp <- ifelse(next_state==100,0,StartVF[which(next_state == setstates)])
        Qestim[index_s,a] <- p * (reward+DF*vpi_sp)
      }  
      
    }
    
    RMSE[i]<-sqrt(sum((PolicyAVF-Qestim)**2))
    A_greedy = 1*t(sapply(1:nrow(Qestim), function(i) Qestim[i, ] == max(Qestim[i,])))#
    pi = A_greedy / rowSums(A_greedy)#
    numiter[i]<- sum(rowSums(pi == OptPolTies) == 4)-1#
    Q[,,i] <- Qestim    
  }
  return(list(Element1=c(StartVF),Element2=c(Policy),Element3=Q,Element4=RMSE,Element5=numiter))
}

#OUTPUTS
#Element1 is the optimal state value fonction
#Element2 is the optimal policy (matrix (|S|x4) corresponding to the probability of taking each action in each state (|S|-1, beacause it doesn't consider the probabilities in the terminal state)
#Element3 Estimation of Q at the ith iteration
#Element4 is the RMSE calculated from the exact state-action value
#Element5 is the number of states with correct optimal actions at the ith iteration (determined from the estimation of Q)

iter_1 <- GPI(rep(0,length(setofstates)),SASRp,DF = 0.5,niter = 100,niter2 = 1,PolicyAVF,OptPolTies)
iter_2 <- GPI(rep(0,length(setofstates)),SASRp,DF = 0.5,niter = 100,niter2 = 2,PolicyAVF,OptPolTies)
iter_5 <- GPI(rep(0,length(setofstates)),SASRp,DF = 0.5,niter = 100,niter2 = 5,PolicyAVF,OptPolTies)
iter_10 <- GPI(rep(0,length(setofstates)),SASRp,DF = 0.5,niter = 100,niter2 = 10,PolicyAVF,OptPolTies)


data_GPI <- data.frame(x=1:length(iter_1[[4]]),n1=iter_1[[4]],n2=iter_2[[4]],n5=iter_5[[4]],n10=iter_10[[4]],
                       m1=iter_1[[5]],m2=iter_2[[5]],m5=iter_5[[5]],m10=iter_10[[5]])



optimal_path <-matrix(apply(iter_2$Element3[,,100],1,which.max),ncol=1)
optimal_path <- optimal_path[-nrow(optimal_path),]
optimal_path <- matrix(optimal_path,ncol=1)
rownames(optimal_path) <- unique(SASRp[,1])
print(optimal_path)

#FIGURES
library(ggplot2)

# RMSE plot
RMSE_GPI <- ggplot(data = data_GPI, aes(x = data_GPI[,1])) +
  geom_line(aes(y = n1, colour = "1")) +
  geom_line(aes(y = n2, colour = "2")) +
  geom_line(aes(y = n5, colour = "5")) +
  geom_line(aes(y = n10, colour = "10")) +
  scale_colour_manual(expression(n[iter]), 
                      breaks = c("1", "2", "5","10"),
                      values = c("red", "green", "blue","purple")) +
  xlab("Number of iterations") +
  scale_y_continuous("RSSE", limits = range(data_GPI[,2:5]))+
  theme(panel.background = element_rect(fill=alpha("grey",0.05)))

if(!file.exists("Results/RMSE GPI.png")){
  ggsave(RMSE_GPI,filename = "Results/RMSE GPI.png")
}

# Convergence plot
convergence_GPI <- ggplot(data = data_GPI, aes(x = data_GPI[,1])) +
  geom_line(aes(y = m1, colour = "1")) +
  geom_line(aes(y = m2, colour = "2")) +
  geom_line(aes(y = m5, colour = "5")) +
  geom_line(aes(y = m10, colour = "10")) +
  scale_colour_manual(expression(n[iter]), 
                      breaks = c("1", "2", "5","10"),
                      values = c("red", "green", "blue","purple")) +
  xlab("Number of iterations") +
  scale_y_continuous("Number of states", limits =c(0,75))+
  theme(panel.background = element_rect(fill=alpha("grey",0.05)))

if(!file.exists("Results/convergence GPI.png")){
  ggsave(convergence_GPI,"Results/convergence GPI.png")
}