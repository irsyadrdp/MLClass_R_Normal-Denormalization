#------------- dataSet
dataX <- matrix(c(250,260,5,1450, 95,116,4,975, 70,96,2,390, 120,50,5,1300), nrow = 4, ncol = 4, byrow = TRUE,
               dimnames = list(c("1", "2","3","4"),
                               c("LT(m2)", "LB(m2)", "Jumlah Kamar","Harga(Jt)")))

#------------- Find Max and Min							   
hargaMax = max(dataX[,4])
hargaMin = min(dataX[,4])						   

#------------- Max Min Algorithm
dataXnorm = apply(dataX, MARGIN = 2, FUN = function(X) (X - min(X))/(max(X)-min(X)))

#------------- Mengambil data training (harga)
harga = dataXnorm[,4]

#------------- denormalisasi dengan menggunakan min max
hargaXdenorm <- sapply(harga, function(X) (X*(max(X)-min(X)))+min(X))

dataXdenorm <- apply(dataXnorm, MARGIN=2, FUN = function(X) (X*(max(X)-min(X)))+min(X))



#------------- z-score / standard score Algorithm
dataXnorm = apply(dataX, MARGIN = 2, FUN = function(X) (X-mean(X))/(sd(X)))

#------------- menampung kolom harga
harga = dataXnorm[,4]

#------------- mengambil mean dari kolom harga
MeanX=mean(dataX[,4])

#------------- mengambil standar deviasi dari kolom harga
StdevX=sd(dataX[,4])

#------------- denormalisasi dengan menggunakan z-score / standar scored
hargaXdenorm <- sapply(harga, function(X) (X*StdevX)+MeanX)




#------------- sigmoid algorith
dataXnorm = apply(dataX, MARGIN = 2, FUN = function(X) (1-exp(-(mean(X)/sd(X))))/(1+ exp(-(mean(X)/sd(X)))))
