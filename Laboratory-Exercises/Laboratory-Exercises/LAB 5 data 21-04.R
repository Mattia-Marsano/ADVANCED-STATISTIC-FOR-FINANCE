
######## LAB 5 - LOGISTIC REGRESSION

### EXERCISE  1 

install.packages("ISLR2")
library(ISLR2)

View(Smarket)
dim(Smarket)  # numbers of variables and osservations 

is.na(Smarket)
pairs(Smarket)



### EXERCISE 2: count the number of observations in each year
table(Smarket$Year)  # table indica quante volte compaiono i valori di una determiata variabile 


### EXERCISE 3: Evaluate the correlation among all the variables 
pairs(Smarket)    # pairs produce grafico matrice scatterplot 
cor(Smarket[,-9]) # cor serve per calcolare la correlazione tra le variabili: matrice simmetrica rispetto alla diagonale 




### EXERCISE 4: Plot the data, which are ordered chronological, to check that volume is increasing over time 
attach(Smarket)  # per considerare tutte le colonne come variabili
plot(Year,Volume)
plot(Volume, pch = 20)



### EXERCISE 5: Fit a logistic gregression on "direction" using "lagone - lagfive" and "volume"
glm.logregr <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.logregr)



### EXERCISE 6: Show the coefficients of the regression 
coef(glm.logregr)
summary(glm.logregr)$coef
summary(glm.logregr)$coef[,4] #prende solo la quarta colonna del summary




### EXERCISE 7:
contrasts(Direction)
glm.prob <- predict(glm.logregr, type = "response")
glm.prob[1:10]    # visualizziamo la probabililità dei primi 10 giorni del primo anno che il marcato vadà su dato che il modello in automatico assume 1=true




### EXERCISE 8: Predict the probability that the market will go up, give values of the predictors
glm.probs <- predict(glm.logregr, type = "response")
glm.probs[1:10]
glm.pred <- rep("Down", 1250)
glm.prob > .5
glm.pred[glm.prob > .5] = "UP"
glm.pred[1:10]  # si vsualizzano solo i primi 10 giorni del primo anno 
glm.pred # si visualizzano tutti i 1250 giorni come "UP" e "DOWN"


### EXERCISE 9:produce a confusion matrix to determine how many observations wew correctly or incorrectly claasified
table(glm.pred, Direction)
mean(glm.pred == Direction)
print("The error rate is")
1 - (glm.pred == Direction)

### EXERCISE 10: Create a vectior corrispondindg the observations from 2001 to 2004. Then use this vector 
#               to create a held out data seta of observations from 2005
train <- (Year < 2005) #controlla per ogni osservazione se l'anno è minore di 2005 (TRUE) atrimenti le mette FALSE
Smarket.2005 <- Smarket[!train,] #crea un nuovo dataset contenente l'opposto di train (grazie a !: cioè tutto ciè che era true diventa false)
# quindi !train contiene date relative al 2005

dim(Smarket.2005)

Direction.2005 <- Direction[!train]




### EXERCISE 11: fit a logistic regression using only the subset observations before the 2005 
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
#usanado subset=train prende in considerazione tutte le variabili ma solo belle riferite a prima del 2005 (train=true)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")










### EXERCISE 12
### EXERCISE 13
