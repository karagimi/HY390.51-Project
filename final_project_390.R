#data import stage, import the needed data before using any function

#run these to import the data
observed_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_obs_final.out", what="numeric", sep=NULL)
simulated_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_sim_final.out", what="numeric", sep=" ")
data_parameters = scan("http://139.91.162.101/teaching/hy390_2021/pars_final.txt", what="numeric", sep=" ")

#run these to print any data needed
observed_data
simulated_data
data_parameters

#spliting data
observed_data_splitted = strsplit(observed_data[1:50],"")
simulated_data_splitted = strsplit(simulated_data[1:500000],"")

#run these to print any data needed
observed_data_splitted
simulated_data_splitted


# Function Υπολογισμου k
calculate_k <- function(input_vector){
  diff_count = vector("numeric",length = 50)
  
  i=1
  while (i <= 50) {
    
    for (j in i:50) {
      
      temp <- sum(input_vector[[i]] != input_vector[[j]])
      diff_count <- append(diff_count,temp)  
    }
    i = i+1
    
  }
  #Υπολογισμος k
  
  k = sum(diff_count/50)
  k
  
}
calculate_k(observed_data_splitted)

#Κληση Υπολογισμου κ για τα simulated data

begin=1
final=50
sim_data_k_output = vector("numeric",length = 10000)
sim_data_w_output = vector("numeric",length = 10000)
sim_data_D_output = vector("numeric",length = 10000)

for (i in 1:10000) {
  
  sim_data_k_output <- append(sim_data_k_output,calculate_k(simulated_data_splitted[begin:final]))
  sim_data_w_output <- append(sim_data_w_output,calculate_w(length(simulated_data_splitted[[begin]])))
  sim_data_D_output <- append(sim_data_D_output,calculate_D(sim_data_k_output[i],sim_data_w_output[i],length(simulated_data_splitted[[begin]])))
  
  
  
  
  begin=final+1
  final= final + 50
}

plot(density(sim_data_k_output[1:10000]))
sim_data_D_output

sim_data_k_output







#function υπολογισμου w
calculate_w <- function(s){
  a1 = sum(1/(1:49))
  
  w = s/a1
  w
}


#Function υπολογισμου D
calculate_D <- function(k,w,s){
  n=50
  a1 = sum(1/(1:49))
  a2 = sum(1/((1:49)**2))
  a2
  
  b2 = (2*(n**2+n+3))/(9*n*(n-1))
  b2
  
  b1 = (n+1)/(3*(n-1))
  b1
  
  c2 = b2 - (n+2)/(a1*n) + a2/(a1**2)
  c2
  
  c1 = b1 - 1/a1
  c1
  
  e2 = c2/((a1**2)+a2)
  e2
  
  e1 = c1/a1
  e1
  
  D = (k-w) / sqrt((e1*s) + (e2*s*(s-1)))
  D
}

