#data import stage, import the needed data before using any function

observed_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_obs_final.out", what="character", sep=NULL)
observed_data

simulated_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_sim_final.out", what="character", sep=" ")
simulated_data

data_parameters = scan("http://139.91.162.101/teaching/hy390_2021/pars_final.txt", what="character", sep=" ")
data_parameters
