# Reinforcement Learning (MAST679W) course project.
The assignment had been done with Jacob Chenette and Julien St-Pierre.

### Introduction : 
The objective of this project was to conduct reinforcement learning in order to find the solution of a small
size maze which has multiple optimal deterministic solutions. Indeed, in our maze, multiple sequences of
actions can be candidate to be the optimal policy. The maze is defined as a Markov decision process. In
order to find one of the optimal paths to the exit of the maze, we applied different off-policy algorithms
and one dynamic programming method. Off-policy learning means learning the value function for one
policy while following another policy. Therefore, we simulated episodes using a simple fixed policy and
compare how the following methods performed for this particular problem: Generalized Policy Iteration
(GPI), Q-learning, n-step off-policy SARSA and Tree Backup.
