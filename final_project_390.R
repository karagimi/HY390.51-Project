#data import stage, import the needed data before using any function

#run these to import the data from the web server directly
#observed_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_obs_final.out", what="numeric", sep=NULL)
#simulated_data = scan("http://139.91.162.101/teaching/hy390_2021/ms_sim_final.out", what="numeric", sep=" ")
#data_parameters = scan("http://139.91.162.101/teaching/hy390_2021/pars_final.txt", what="numeric", sep=" ")

#if the above server doesnt work use this
#run these to import the data from github server
observed_data = scan("https://raw.githubusercontent.com/mindthegeekgr/Project390_files/main/ms_obs_final.out", what="numeric", sep=NULL)
simulated_data = scan("https://raw.githubusercontent.com/mindthegeekgr/Project390_files/main/ms_sim_final.out", what="numeric", sep=" ")
data_parameters = scan("https://raw.githubusercontent.com/mindthegeekgr/Project390_files/main/pars_final.txt", what="numeric", sep=" ")


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
  diff_count = vector("numeric")
  
  for(i in 1:50) {
    for(j in i:50) {
      if(i == j) {
        next;
      }
      num_differences <- sum(strsplit(input_vector[[i]], "")[[1]] != strsplit(input_vector[[j]], "")[[1]])
      
      # Output the number of different digits
      diff_count <- append(diff_count,num_differences)
    }
  }
  #Υπολογισμος k
  k = sum(diff_count/length(diff_count))
  k
  
}


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

#Κληση Υπολογισμου κ για τα observed data

k <- calculate_k(observed_data)
w <- calculate_w(length(observed_data_splitted[[1]]))
D <- calculate_D(k,w,length(observed_data_splitted[[1]]))

#Κληση Υπολογισμου κ για τα simulated data

begin=1
final=50
sim_data_k_output = vector("numeric")
sim_data_w_output = vector("numeric")
sim_data_D_output = vector("numeric")


for (i in 1:10000) {
  
  sim_data_k_output <- append(sim_data_k_output,calculate_k(simulated_data[begin:final]))
  sim_data_w_output <- append(sim_data_w_output,calculate_w(length(simulated_data_splitted[[begin]])))
  sim_data_D_output <- append(sim_data_D_output,calculate_D(sim_data_k_output[i],sim_data_w_output[i],length(simulated_data_splitted[[begin]])))
  
  begin=final+1
  final= final + 50
}

#Δημιοργια plot απο τα output των συναρτησεων για τα simulated data

plot(density(sim_data_k_output[1:10000]))
plot(density(sim_data_w_output[1:10000]))
plot(density(sim_data_D_output[1:10000]))



# Use these to print the data needed from the function outputs
sim_data_k_output
sim_data_w_output
sim_data_D_output


#Κανονικοποίηση

#Για το w
mean_w = mean(sim_data_w_output)
var_w = sd(sim_data_w_output)
normalized_w = (sim_data_w_output - mean_w)/var_w

#Για το w0
normalized_w0 = (w - mean_w)/var_w

#Για το k
mean_k = mean(sim_data_k_output)
var_k = sd(sim_data_k_output)
normalized_k = (sim_data_k_output - mean_k)/var_k

#Για το κ0
normalized_k0 = (k - mean_k)/var_k

#Για το D
mean_D = mean(sim_data_D_output)
var_D = sd(sim_data_D_output)
normalized_D = (sim_data_D_output - mean_D)/var_D

#Για το D0
normalized_D0 = (D - mean_D)/var_D

#Υπολογισμός ευκλείδιων αποστάσεων

calc_euclidian_d <- function(){
  d = sqrt((normalized_D0 - normalized_D)**2 + (normalized_w0 - normalized_w)**2 + (normalized_k0 - normalized_k)**2)
}
#Κληση της συναρτηση και αποθηκευση τον αποτελεσματων σε array
euclidian_d = calc_euclidian_d()

temp_array <-matrix(nrow=10000,ncol = 2)
temp_array[1]<-euclidian_d


#Εύρεση των 500 μικρότερων αποστάσεων και κρατάμε τα indexes τους σε ένα νέο vector
smallest_distances_indexes = order(euclidian_d,decreasing = FALSE)[1:500]

smallest_distances_indexes

# Ευρεση τον αντιστοιχων τιμων με βαση τα indexes απο το data_parameters
data_corrensponding_values = as.double(data_parameters[smallest_distances_indexes[1:500]])

data_corrensponding_values

#Ευρεση του μεσου ορου των data_corrensponding_values
mean_cor_values = mean(data_corrensponding_values)
mean_cor_values
#Ευρεση του median των data_corrensponding_values
median_cor_values = median(data_corrensponding_values)
median_cor_values

#Κατασκευη ιστογραμματος για τα data_corrensponding_values
hist(data_corrensponding_values)
#κατασκευη 2ου ιστογραμματος για ευκολοτερη διακριση των διαστημματων με σκοπο την αναφορα
hist(data_corrensponding_values,breaks = 4,xlim = c(1,365))

#Κατασκευη διαγραμματος πυκνοτητας για τα data_corrensponding_values
plot(density(data_corrensponding_values))

