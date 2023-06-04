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

#Αρχικοποιηση vector για να μπορει να γινει χρηση του append
diff_count = vector("numeric",length = 50)

i=1
while (i != 50) {
  
  for (j in (i+1):50) {
    temp <- sum(observed_data_splitted[[i]] != observed_data_splitted[[j]])
    diff_count <- append(diff_count,temp)  
}
i = i+1

}

#Υπολογισμος k

sum(diff_count/50)


