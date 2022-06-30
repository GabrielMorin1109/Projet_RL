# setting working directory
{
  script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(paste0(script_path, "/../../"))
  cat("Directory is currently at : \n", getwd(), "\n")
  source(file = paste0(getwd(), "/Scripts/Main.R"))
}
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
source("Scripts/RL_algorithm/PolicyAVF.R")

# Function which applies the TreeBackup algorithm
ApplyTreeBackup <- function(nparam, stepsize, initQestim, DF, nepis, state, prob, rewards, epsilon=0){
  
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
          # Target
          if((t+1) >= T_) G = R[T_]
          else G = R[t+1] + DF * pi[S[t+2], ] %*% Q[S[t+2], ] 
            
          # Loop for k = min(t, T-1) down through t+1
          for(k in max(tau+1, min(t, T_-1)):(tau+1)){
            G <- R[k] + DF * pi[S[k+1], -A[k+1]] %*% Q[S[k+1], -A[k+1]] + DF * pi[S[k+1], A[k+1]] * G
          }
          
          # Update Q
          Q[S[tau+1], A[tau+1]] = Q[S[tau+1], A[tau+1]] + stepsize * (G - Q[S[tau+1], A[tau+1]])
          
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
# For n=1,2,5,10, apply TreeBackup
#--------------------------------------------------
# Initialize parameters
stepsize = c(0.05, 0.25, 0.50, 0.90)
setofstates <- c(na.omit(c(state)))
initQestim<- matrix(0,nrow=length(setofstates),ncol=4)
rownames(initQestim) <- setofstates
nepis = 200
prob = rep(0.25, 4)
rewards <- c(-1,  0, -1)

for(i in 1:length(stepsize)){
  
  # Apply Tree Backup
  Outlist_1 <- ApplyTreeBackup(nparam = 1, stepsize[i], initQestim, DF=0.5, nepis, state, prob, rewards)
  Outlist_2 <- ApplyTreeBackup(nparam = 2, stepsize[i], initQestim, DF=0.5, nepis, state, prob, rewards)
  Outlist_5 <- ApplyTreeBackup(nparam = 5, stepsize[i], initQestim, DF=0.5, nepis, state, prob, rewards)
  Outlist_10 <- ApplyTreeBackup(nparam = 10, stepsize[i], initQestim, DF=0.5, nepis, state, prob, rewards)
  
  # Estimate Q(s,a)
  Qestimmat_1 <- Outlist_1$Q
  Qestimmat_2 <- Outlist_2$Q
  Qestimmat_5 <- Outlist_5$Q
  Qestimmat_10 <- Outlist_10$Q

  #------------------------------------------
  # RMSE vs episode
  #------------------------------------------
  RMSE_1 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_1[,,k] - PolicyAVF)^2)))
  RMSE_2 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_2[,,k] - PolicyAVF)^2)))
  RMSE_5 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_5[,,k] - PolicyAVF)^2)))
  RMSE_10 <- sapply(1:nepis, function(k) sqrt(sum((Qestimmat_10[,,k] - PolicyAVF)^2)))
  
  if(i==1)
    RMSE <- rbind(
                  cbind(RMSE = RMSE_1, n=1, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_2, n=2, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_5, n=5, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_10, n=10, stepsize = stepsize[i])
            )
  else
    RMSE <- rbind(
                  RMSE,
                  cbind(RMSE = RMSE_1, n=1, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_2, n=2, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_5, n=5, stepsize = stepsize[i]),
                  cbind(RMSE = RMSE_10, n=10, stepsize = stepsize[i])
                )
  
  #----------------------------------------------
  # Number of episode before Opt Policy is found
  #----------------------------------------------
  Nstate_1 <- sapply(1:nepis, function(k) sum(rowSums(Outlist_1$pi[-75,,k] == Policy[-75,]) == 4))
  Nstate_2 <- sapply(1:nepis, function(k) sum(rowSums(Outlist_2$pi[-75,,k] == Policy[-75,]) == 4))
  Nstate_5 <- sapply(1:nepis, function(k) sum(rowSums(Outlist_5$pi[-75,,k] == Policy[-75,]) == 4))
  Nstate_10 <- sapply(1:nepis, function(k) sum(rowSums(Outlist_10$pi[-75,,k] == Policy[-75,]) == 4))
  
  if(i==1)
    Nstate <- rbind(
      cbind(Nstate = Nstate_1, n=1, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_2, n=2, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_5, n=5, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_10, n=10, stepsize = stepsize[i])
    )
  else
    Nstate <- rbind(
      Nstate,
      cbind(Nstate = Nstate_1, n=1, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_2, n=2, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_5, n=5, stepsize = stepsize[i]),
      cbind(Nstate = Nstate_10, n=10, stepsize = stepsize[i])
    )
  
}

#------------------------
# Plot RMSE vs t
#------------------------
library(dplyr)
df_RMSE <- data.frame(RMSE, Episode = 1:nepis) %>% 
           mutate(alpha = as.character(stepsize))

library(ggplot2)

ggplot(filter(df_RMSE, Episode%in%c(0:50)),aes(x=Episode, y=RMSE, group=alpha, color=alpha))+
  geom_line()+
  labs(x="Episode", y="RSSE", color=expression(alpha))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)), scales = "free")+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("Results/Treebackup_RMSE_1.pdf",
       plot = last_plot(),
       device = 'pdf'
)

ggplot(filter(df_RMSE, Episode%in%c(150:200)),aes(x=Episode, y=RMSE, group=alpha, color=alpha))+
  geom_line()+
  labs(x="Episode", y="RSSE", color=expression(alpha))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)), scales = "free")+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("Results/Treebackup_RMSE_2.pdf",
       plot = last_plot(),
       device = 'pdf'
       )

#----------------------------------------------------
# Number of iterations before Optimal Policy is found
#----------------------------------------------------
df_Nstate <- data.frame(Nstate, Episode = 1:nepis) %>% 
  mutate(alpha = as.character(stepsize))

ggplot(filter(df_Nstate, Episode%in%c(0:200)),aes(x=Episode, y=Nstate, group=alpha, color=alpha))+
  geom_line()+
  scale_y_continuous(breaks = c(0, 15, 30, 45, 60, 75)) +
  labs(y="Number of states", x="Episode", color=expression(alpha))+
  facet_wrap(~n, labeller=label_bquote(n==.(n)))+
  theme(
    panel.background = element_rect(fill=alpha("grey",0.05)),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("Results/Treebackup_Nstate.pdf",
       plot = last_plot(),
       device = 'pdf'
)
