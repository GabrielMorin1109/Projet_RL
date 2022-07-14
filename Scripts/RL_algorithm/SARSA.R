# setting working directory
{
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(paste0(script_path, "/../../"))
  cat("Directory is currently at : \n", getwd(), "\n")
  source(file = paste0(getwd(), "/Scripts/Main.R"))
}
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source("Scripts/RL_algorithm/PolicyAVF.R")

# Function which applies the n-step off-policy SARSA algorithm
ApplySARSA <- function(nparam, stepsize, initQestim, DF, nepis, state, prob, rewards, epsilon=0, `rho=1`=FALSE){
  
  # Initialize Q(s, a)
  Q = initQestim
  Qarray = array(Q, c(dim(Q), nepis))
  rownames(Qarray) <- setofstates
  
  # Initialize greedy policy
  A_greedy = 1*t(sapply(1:nrow(Q), function(i) Q[i, ] == max(Q[i,])))
  pi = A_greedy / rowSums(A_greedy) * (1-epsilon*(rowSums(A_greedy) < 4)) + (A_greedy == 0)*epsilon/(4- rowSums(A_greedy)+10^-6)
  rownames(pi) <- setofstates
  piarray <- array(pi, c(dim(Q), nepis))
  
  # Loop forever
  for(j in 1:nepis){
    
    # Seet seed
    set.seed(j)
    
    # Simulate episodes
    EpisodeOutcomes <- SimulateEpisodeMaze(state,prob,rewards)
    
    # Store states, actions and rewards
    S = as.character(EpisodeOutcomes["stateseq",])
    A = EpisodeOutcomes["actionseq",]
    R = EpisodeOutcomes["rewardseq",]
    T_ = ncol(EpisodeOutcomes) + 1
    S[T_] = "100"
    A[T_] = sample(action, size = 1)
    R[T_] = rewards[2]
    
    # Loop
    t = -1
    while(TRUE){
      # Iterate t and tau
      t = t + 1
      tau = t + 1 - nparam
      if(tau == T_ - 1) break
      
      if(tau >= 0){
        # Ratio
        if(`rho=1`) 
          rho=1
        else 
          rho = prod(sapply((tau+1):min(tau+nparam-1,T_-1), function(i) pi[S[i+1], A[i+1]] / prob[A[i+1]]))
          
        # Target
        G = sum(sapply((tau + 1):min(tau+nparam, T_), function(i) DF^(i-tau-1) * R[i])) + ifelse(tau + nparam < T_, DF^nparam * Q[S[tau+nparam+1], A[tau+nparam+1]], 0)
       
        # Update Q
        Q[S[tau+1], A[tau+1]] = Q[S[tau+1], A[tau+1]] + stepsize * rho * (G - Q[S[tau+1], A[tau+1]])
        
        # Update greedy policy
        A_greedy <- 1*(Q[S[tau+1], ] == max(Q[S[tau+1],]))
        pi[S[tau+1], ] <- A_greedy / sum(A_greedy) * (1-epsilon*(sum(A_greedy) < 4)) + (A_greedy == 0)*epsilon/(4- sum(A_greedy)+10^-6)
      }
    }
    Qarray[,,j] <- Q
    piarray[,,j] <- pi
  }
  return(list(Q=Qarray, pi=piarray))
}

#--------------------------------------------------
# For n=1,2,5,10, apply SARSA
#--------------------------------------------------
# Initialize parameters
stepsize = c(0.005)
epsilon = c(0, 0.05, 0.10, 0.15)
setofstates <- c(na.omit(c(state)))
initQestim<- matrix(0,nrow=length(setofstates),ncol=4)
rownames(initQestim) <- setofstates
nepis = 400
prob = rep(0.25, 4)
rewards <- c(-1,  0, -1)

for(i in 1:length(epsilon)){
  
  # Apply Tree Backup
  Outlist_1 <- ApplySARSA(nparam = 1, stepsize = 0.05, initQestim, DF=0.5, nepis, state, prob, rewards, epsilon = epsilon[i])
  #Outlist_2 <- ApplySARSA(nparam = 2, stepsize, initQestim, DF=0.5, nepis, state, prob, rewards, epsilon = epsilon[i])
  Outlist_5 <- ApplySARSA(nparam = 5, stepsize = 0.005, initQestim, DF=0.5, nepis, state, prob, rewards, epsilon = epsilon[i])
  #Outlist_8 <- ApplySARSA(nparam = 8, stepsize, initQestim, DF=0.5, nepis, state, prob, rewards, epsilon = epsilon[i])
  
  # Estimate Q(s,a)
  Qestimmat_1 <- Outlist_1$Q
  #Qestimmat_2 <- Outlist_2$Q
  Qestimmat_5 <- Outlist_5$Q
  #Qestimmat_8 <- Outlist_8$Q
  
  #------------------------------------------
  # RMSE vs episode
  #------------------------------------------
  RMSE_1 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_1[,,k] - PolicyAVF)^2)))
  #RMSE_2 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_2[,,k] - PolicyAVF)^2)))
  RMSE_5 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_5[,,k] - PolicyAVF)^2)))
  #RMSE_8 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_8[,,k] - PolicyAVF)^2)))
  
  if(i==1)
    RMSE <- rbind(
      cbind(RMSE = RMSE_1, n=1,  epsilon = epsilon[i]),
      #cbind(RMSE = RMSE_2, n=2,  epsilon = epsilon[i]),
      cbind(RMSE = RMSE_5, n=5,  epsilon = epsilon[i])
      #,cbind(RMSE = RMSE_8, n=8, epsilon = epsilon[i])
    )
  else
    RMSE <- rbind(
      RMSE,
      cbind(RMSE = RMSE_1, n=1,  epsilon = epsilon[i]),
      #cbind(RMSE = RMSE_2, n=2,  epsilon = epsilon[i]),
      cbind(RMSE = RMSE_5, n=5,  epsilon = epsilon[i])
      #,cbind(RMSE = RMSE_8, n=8,  epsilon = epsilon[i])
    )
  
  #----------------------------------------------
  # Number of episode before Opt Policy is found
  #----------------------------------------------
  Nstate_1 <- sapply(1:nepis, function(k) {
      A_greedy = 1*t(sapply(1:nrow(Qestimmat_1[,,k]), function(i) Qestimmat_1[i,,k] == max(Qestimmat_1[i,,k])))
      pi = A_greedy / rowSums(A_greedy)
      sum(rowSums(pi[-75,] == Policy[-75,]) == 4)}
  )
  Nstate_5 <- sapply(1:nepis, function(k) {
      A_greedy = 1*t(sapply(1:nrow(Qestimmat_5[,,k]), function(i) Qestimmat_5[i,,k] == max(Qestimmat_5[i,,k])))
      pi = A_greedy / rowSums(A_greedy)
      sum(rowSums(pi[-75,] == Policy[-75,]) == 4)}
  )
  
  if(i==1)
    Nstate <- rbind(
      cbind(Nstate = Nstate_1, n=1,  epsilon = epsilon[i]),
      #cbind(Nstate = Nstate_2, n=2,  epsilon = epsilon[i]),
      cbind(Nstate = Nstate_5, n=5,  epsilon = epsilon[i])
      #,cbind(Nstate = Nstate_8, n=8,  epsilon = epsilon[i])
    )
  else
    Nstate <- rbind(
      Nstate,
      cbind(Nstate = Nstate_1, n=1,  epsilon = epsilon[i]),
      #cbind(Nstate = Nstate_2, n=2,  epsilon = epsilon[i]),
      cbind(Nstate = Nstate_5, n=5,  epsilon = epsilon[i])
      #,cbind(Nstate = Nstate_8, n=8,  epsilon = epsilon[i])
    )
  
}

#------------------------
# Plot RMSE vs t
#------------------------
library(ggplot2)
library(dplyr)

df_RMSE <- data.frame(RMSE, Episode = 1:nepis) %>% 
  mutate(epsilon = as.character(epsilon))

ggplot(filter(df_RMSE, Episode%in%c(0:100)),aes(x=Episode, y=RMSE, group=epsilon, color=epsilon))+
  geom_line()+
  labs(x="Episode", y="RSSE", color=expression(epsilon))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)))+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )

if(!file.exists("Results/SARSA_RMSE_1.pdf")){
  ggplot2::ggsave("Results/SARSA_RMSE_1.pdf",
         plot = last_plot(),
         device = 'pdf'
  )
}

ggplot(filter(df_RMSE, Episode%in%c(300:400)),aes(x=Episode, y=RMSE, group=epsilon, color=epsilon))+
  geom_line()+
  labs(x="Episode", y="RSSE", color=expression(epsilon))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)))+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )
if(!file.exists("Results/SARSA_RMSE_2.pdf")){
  ggplot2::ggsave("Results/SARSA_RMSE_2.pdf",
                  plot = last_plot(),
                  device = 'pdf'
  )
}

#----------------------------------------------------
# Number of iterations before Optimal Policy is found
#----------------------------------------------------
df_Nstate <- data.frame(Nstate, Episode = 1:nepis) %>% 
  mutate(epsilon = as.character(epsilon))

ggplot(filter(df_Nstate, Episode%in%c(0:nepis)),aes(x=Episode, y=Nstate, group=epsilon, color=epsilon))+
  geom_line()+
  scale_y_continuous(breaks = c(0, 15, 30, 45, 60, 75)) +
  labs(y="Number of states", x="Episode", color=expression(epsilon))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)))+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )

if(!file.exists("Results/SARSA_Nstate.pdf")){
  ggsave("Results/SARSA_Nstate.pdf",
         plot = last_plot(),
         device = 'pdf'
  )
}

