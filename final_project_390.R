#data import stage, import the needed data before using any function

#run these to import the data
observed_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_obs_final.out", what="character", sep=NULL)
simulated_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_sim_final.out", what="character", sep=" ")
data_parameters = scan("http://139.91.162.101/teaching/hy390_2021/pars_final.txt", what="character", sep=" ")

#run these to print any data needed
observed_data
simulated_data
data_parameters


#Function gia kanoniki katanomi

#test data
observation = rnorm(10,182,10)

pars = vector("numeric", length=10000)
statistics = vector("numeric",length = 10000)

for(i in 1:10000){
  m = runif(1,100,200)
  s = rnorm(10,m,10)
  statistics[i]=mean(s)
  
}

#mesos oros paratirisis
mobs = mean(observation)

#vector me timi opote tha to kanei kiklika gia ola
statistics - mobs

#diafores
d = abs(statistics - mobs)

#thelo na valo se seira ta indexes
#FALSE giati theloume apo to mikrotero sto megalitero

myorder = order(d,decreasing = FALSE)
#averages ton prton 500 stoixeion
mean(pars[myorder[1:500]])

#gia na paroume tin grafiki parastasi tis katanomis xrisimopoioume
plot(density(pars[myorder[1:500]]))