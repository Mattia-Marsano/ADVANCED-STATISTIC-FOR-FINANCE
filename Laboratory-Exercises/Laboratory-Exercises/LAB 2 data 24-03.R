

#### EXERCISES 24/03/2026 #####     PER SOLUZIONI VEDI 20/03/2026 


###########     EX  1) SAMPLE MEAN FROM A NORMAL DISTRIBUTION  

# 1.1) ok
n = 100
mu = 0 
sigma = 1 

x = rnorm(n, mean = mu, sd = sigma)
x

# 1.2) ok 

l = 10
Mat = matrix(x, nrow = l)
Mat
Muhat = rowMeans(Mat)
print("The mean for each row is:")
Muhat 

print("The mean of Muhat is:")
mean(Muhat) - mu                              #this is an estimation of the bias 



##############      EX 2) DOWNLOADIND THE DATA 

getwd() #getwd() ci dice in quale cartella stiamo lavorando 

#1) ok scarica il dataset Auro dal link 
Auto = read.table("https://www.statlearning.com/s/Auto.data")
head(Auto)


#2) ok
Auto = read.table("https://www.statlearning.com/s/Auto.data", header = TRUE)   # header=T > così la prima riga del file contiene i nomi delle colonne
head(Auto)

#3) ok 
View(Auto) #mostra tutto il daset 

#4) ok 
summary(Auto)
dim(Auto)

#5) ok 
Auto = read.table("https://www.statlearning.com/s/Auto.data", header = TRUE, na.strings = "?", stringsAsFactors = TRUE) #na.strings fa visualizzare i punti interrogativi come "NA"
                                                                                                                        #stringAsFactor trasforma le colonne di testo in fattori                                                                                   
#6) ok
sum(is.na(Auto))  #quanti NA in totale ci sono nel dataset

#7) ok 
na.omit(Auto)    # rimuove tutta la riga completa dove trova un NA
head(Auto)      






###################   3) Simple Linear Regression 

set.seed(1234)    #serve a rendere riproducibili i risultati casuali in R.

#1) ok  creiamo 2 vettori, conteneti 100 valori, generati da distribuzioni normali 
x = rnorm(100, 0, 1)

eps = rnorm(100, 0 ,0.25)

#2) ok regression formula 
beta0 = -1
beta1 = 0.5
y = beta0 + beta1*x + eps

#3) ok crea grafico semplice mostrando relazione tra y e x 
plot(x, y)

#4)ok Fit a least squares linear model to predict Y using X
lreg=lm(y~x)   
names(lreg)   #mostra come si chiamano gli elementi contenuti dentro lreg
lreg$coefficients 
summary(lreg)


#5)ok creare un intervallo di confidenza 

intervallo  = confint(lreg, level = 0.95)
intervallo

predict(lreg, data.frame(x=c(0,1,10)),interval="confidence")  #stima il valore di y quando x=0, x=1 e x=10 e da un untervallo di confidenza; con confidence si usa solo B0 e B1

predict(lreg, data.frame(x=c(0,1,10)),interval="prediction") #con prediction si comprende solo l'errore 


#6) ok al grafico di prima si aggiungono le rette e la legenda 

plot(x,y)
abline(lreg,col="blue")
abline(beta0,beta1,col="red")
legend("topleft",
       legend=c("Data","FittedModel","RealModel"),
       col=c("black","blue","red"),
       pch=c(1,NA,NA),
       lty=c(NA,1,1),
       lwd=c(NA,2,2))


      
#7) ok Repeat the exercise after modifying the data generation process in such a way that there is less noise in
#the data(reduce the variance of eps)

mueps=0
sigmaeps=sqrt(0.025)
eps=rnorm(n,mueps,sigmaeps)
y=beta0+beta1*x+eps
lreg=lm(y~x)
lreg$coefficients
summary(lreg)

confint(lreg, level = 0.95)

predict(lreg, data.frame(x=c(0,1,10)), inverval = "confidence")

predict(lreg, data.frame(x=c(0,1,10)), inverval = "prediction")


plot(x, y, pch=16)
abline(lreg, col = "blue")
abline(beta0, beta1, col = "red")
legend("topleft",
       legend = c("Data", "Fitted model", "Real model"),
       col = c("Black", "Blue", "Red"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2))


#8) ok Repeat the exercise after modifying the data generation process in such a way that there is less noise in
#the data(reduce the variance of eps)


mueps=0
sigmaeps=sqrt(4) # nella versione del punto 7 sqrt era 0.025
eps=rnorm(n,mueps,sigmaeps)
y=beta0+beta1*x+eps
lreg=lm(y~x)
lreg$coefficients
summary(lreg)

confint(lreg, level = 0.95)

predict(lreg, data.frame(x=c(0,1,10)), inverval = "confidence")

predict(lreg, data.frame(x=c(0,1,10)), inverval = "prediction")


plot(x, y, pch=16)
abline(lreg, col = "blue")
abline(beta0, beta1, col = "red")
legend("topleft",
       legend = c("Data", "Fitted model", "Real model"),
       col = c("Black", "Blue", "Red"),
       pch = c(16, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2))


