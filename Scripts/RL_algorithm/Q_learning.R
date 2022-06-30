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


#Inputs
#stepsize : learning rate
#initQestim : matrix |S| x 4
#DF: discount factor
#nepis : number of episodes
#state : mapping of the maze
#prob: vector of probabilities (1/4,1/4,1/4,1/4) to ensure exploration
#rewards : (r1,r2,r3) (r1 = reward for moving, r2 = reward terminal state, r3 = reward in a state already encountered)
#PolicyAVF : matrix matrix |S| x 4 containing the exact value of Q(s,a) ("s" is the index of the row and "a" the index of the column)
#PolicyTies : Matrix containing the optimal policy pi(a|s) that considers ties (ex 0 1/2 0 1/2 instead of 0 1 0 0)

ApplyQLearning <- function(stepsize,initQestim,DF,nepis,state,prob,rewards,PolicyAVF,PolicyTies){
  RMSE <- c()
  numiter <- c()
  setofstates <- c(na.omit(c(state)))
  Q <- array(0,dim=c(nrow(initQestim),ncol(initQestim),nepis))
  for(i in 1:nepis){ #Loop for each episode
    set.seed(i)
    EpisodeOutcomes<-SimulateEpisodeMaze(state,prob,rewards)
    
    stateseq <- EpisodeOutcomes[1,] #sequence of states
    actionseq <- EpisodeOutcomes[2,] #sequence of actions
    rewardseq <- EpisodeOutcomes[3,] #sequence of rewards
    T_ <- dim(EpisodeOutcomes)[2] #length of the episode
    
    for(j in 1:(T_)){ #Loop for each step in an episode
      
      curr_state <- stateseq[j] #current state
      curr_action<- actionseq[j] #current action
      next_state <- ifelse(j==T_,100,stateseq[j+1]) #next state
      
      #Applyinh Q-Learning formula
      index_state<-which(curr_state==setofstates) #index s
      index_next_state <- which(next_state==setofstates) #index s'
      
      initQestim[index_state,curr_action]<-initQestim[index_state,curr_action]+stepsize*(rewardseq[j]+DF*max(initQestim[index_next_state,])-initQestim[index_state,curr_action])
      
    }
    
    RMSE[i] <- sqrt(sum(((initQestim-PolicyAVF)^2)))
    Q[,,i] <- initQestim
    A_greedy = 1*t(sapply(1:nrow(initQestim), function(i) initQestim[i, ] == max(initQestim[i,])))#
    pi = A_greedy / rowSums(A_greedy)#
    numiter[i]<- sum(rowSums(pi == OptPolTies) == 4)-1#
  }
  list <- list(RMSE,initQestim,Q,numiter)
  return(list)
}


#Output (list)
#RMSE : is a vector containing the RMSE calculate after nepis
#initQestim : is the final estimation of Q(s,a)
#numiter : is a vector, contains number of states that the optimal actions(s) is(are) found.
setofstates <- c(na.omit(c(state)))
initQestim<- matrix(0,nrow=length(setofstates),ncol=4)
rownames(initQestim) <- setofstates
DF = 0.5
prob <- rep(1/4,4)
rewards <- c(-1,0,-1)
nepis<-100
RMSE <- rep(0,nepis)
PolicyAVF
#400 episodes are considered to estimate q_star
ep_0.05 <- ApplyQLearning(stepsize = 0.05,initQestim,DF,nepis=100,state,prob,rewards,PolicyAVF,OptPolTies)
ep_0.25 <- ApplyQLearning(stepsize = 0.25,initQestim,DF,nepis,state,prob,rewards,PolicyAVF,OptPolTies)
ep_0.50 <- ApplyQLearning(stepsize = 0.50,initQestim,DF,nepis,state,prob,rewards,PolicyAVF,OptPolTies)
ep_0.90 <- ApplyQLearning(stepsize = 0.90,initQestim,DF,nepis,state,prob,rewards,PolicyAVF,OptPolTies)


data_QL <- data.frame(x=1:100,a1=ep_0.05[[1]][1:100],a2=ep_0.25[[1]],a3=ep_0.50[[1]],a4=ep_0.90[[1]],e1=ep_0.05[[4]][1:100],e2=ep_0.25[[4]],e3=ep_0.50[[4]],e4=ep_0.90[[4]])
library(ggplot2)



ggplot(data = data_QL, aes(x = data_QL[,1])) +
  geom_line(aes(y = a1, colour = "0.05")) +
  geom_line(aes(y = a2, colour = "0.25")) +
  geom_line(aes(y = a3, colour = "0.50")) +
  geom_line(aes(y = a4, colour = "0.90")) +
  scale_colour_manual(expression(alpha), 
                      breaks = c("0.05", "0.25", "0.50","0.90"),
                      values = c("red", "green", "blue","purple")) +
  xlab("Episode") +
  scale_y_continuous("RSSE", limits = range(data_QL[,2:5]))+
  theme(panel.background = element_rect(fill=alpha("grey",0.05)))

ggplot(data = data_QL, aes(x = data_QL[,1])) +
  geom_line(aes(y = e1, colour = "0.05")) +
  geom_line(aes(y = e2, colour = "0.25")) +
  geom_line(aes(y = e3, colour = "0.50")) +
  geom_line(aes(y = e4, colour = "0.90")) +
  scale_colour_manual(expression(alpha), 
                      breaks = c("0.05", "0.25", "0.50","0.90"),
                      values = c("red", "green", "blue","purple")) +
  xlab("Episode") +
  scale_y_continuous("Number of states", limits = range(data_QL[,6:9]))+
  theme(panel.background = element_rect(fill=alpha("grey",0.05)))





