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

k = sum(diff_count/50)
k
plot(density(diff_count))


#Υπολογισμος α

a1 = sum(1/(1:49))
a1
#Υπολογισμος w
s = 132
w = s/a1
w

#Υπολογισμος Tajima

n=50

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
