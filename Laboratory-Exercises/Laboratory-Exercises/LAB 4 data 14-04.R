




######     ESERCITAZIONE 14/04   ########

##################                                                    EXERCISE 1

#1a) LOADING THE DATA 

Auto = read.table("https://www.statlearning.com/s/Auto.data", header = TRUE, na.strings = "?", stringsAsFactors = T)
# con header = TRue si eleimina la prima riga qualora non riporta il nome della variabile 
# na.strings permette di visualizzare gli ? come se fossero na ovvero missing values 
# stringAsFactor = T trasforma le variabili di testo in fattori 
View(Auto)


#1b) PERCENTAGE OF MISSING VALUE 

mean(is.na(Auto)) * 100 #fa la media del numero delle celle con valori mancanti 
colMeans(is.na(Auto)) * 100 # restituisce la percentuale di missing values di ogni variabile 

#1c) SUMMARY FOR THE VARIABLE MPG

attach(Auto) # con questo comando tutte le variabili dentro auto vengono trattate come variabili: posso usare
#direttamente il nome delle colonne direttamente senza scrivere ogni volta il nome del dataset

summary(mpg) 


# in alternativa (sensa usare attach)
summary(Auto[, c("mpg")])


#2a) REGRESS "mpg" on ""weight"

lm.auto <- lm(mpg ~ weight, data = Auto)
summary(lm.auto)


#2b) Produce two plots. Residuals vs weight and residuals vs fitted values 

par(mfrow = c(1,2))  #divide lo spazione della finestra grafica in due parti.
#par() funzione che modifica le impostazioni grafiche di R.
#mfrow indica come dividere l’area dei grafici.
#c(1, 2) crea un vettore con due numeri quindi lo spazio grafico sarà diviso in una riga e due colonne 

plot(Auto$weight, resid(lm.auto),
     xlab = "weight",
     ylab = "Residuals",
     main = "Residuals vs weight",
     pch = 20)
abline(h = 0, lty = 3, col = "red") #non è una linea di regressione 

plot(fitted(lm.auto), resid(lm.auto),
     xlab = "Fitted values",
     ylab = "resiaduals",
     main = "Residuals vs fitted values",
     pch = 20)
abline(h = 0, lty = 3, col = "red")

#INTERPRETAZIONE GRAFICA: la linea rossa rappresanta il punto in cui il residuo è uguale a zero
#ricorda che residuo = valore osservato - valore stimato dal modello
#se il residuo si trova sulla linea: il modello ha previsto bene il valore 
#se il residuo si trova sopra la linea (residuo positivo >0): il valore osservato era maggiore di quello stimato (il modello ha sottostimato) 
#se il residuo si trova sotto la linea (residuo negativo <0): il valore osservato era minore di quello stimato (il modello ha sovrastimato) 



#2c) APPLY TRANSORMATION ON WEIGHT: SQUARE ROT, LOG AND RECIPROCAL AND THEN REFIT THE LINEAR REGRESSION WITH THE GRAPHS 

lm.auto_sqrt <- lm(mpg ~ sqrt(weight), data = Auto)
par(mfrow = c(1,2))
plot(sqrt(Auto$weight), resid(lm.auto_sqrt),
     xlab = "weight",
     ylab = "Residuals",
     main = "Residuals vs weight",
     pch = 20)
abline(h = 0, lty = 3, col = "red") 

plot(fitted(lm.auto_sqrt), resid(lm.auto_sqrt),
     xlab = "Fitted values",
     ylab = "resiaduals",
     main = "Residuals vs fitted values",
     pch = 20)
abline(h = 0, lty = 3, col = "red")




lm.auto_log <- lm(mpg ~ log(weight), data = Auto)
par(mfrow = c(1,2))
plot(log(Auto$weight), resid(lm.auto_log),
     xlab = "weight",
     ylab = "Residuals",
     main = "Residuals vs weight",
     pch = 20)
abline(h = 0, lty = 3, col = "red") 

plot(fitted(lm.auto_log), resid(lm.auto_log),
     xlab = "Fitted values",
     ylab = "resiaduals",
     main = "Residuals vs fitted values",
     pch = 20)
abline(h = 0, lty = 3, col = "red")



lm.auto_inv <- lm(mpg ~ I(1 / weight), dat = Auto)
par(mfrow = c(1,2))
plot(I(1/Auto$weight), resid(lm.auto_inv),
     xlab = "weight",
     ylab = "Residuals",
     main = "Residuals vs weight",
     pch = 20)
abline(h = 0, lty = 3, col = "red") 

plot(fitted(lm.auto_inv), resid(lm.auto_inv),
     xlab = "Fitted values",
     ylab = "resiaduals",
     main = "Residuals vs fitted values",
     pch = 20)
abline(h = 0, lty = 3, col = "red")

#THIS COULD ALSO BE DONE BY PRODUCING A FUNCTION 



#3) USE THE BEST TRANSORMATION AND GET AN 80% CONFIDENCE INTERVALL 

summary(lm.auto_inv)
confint(lm.auto_inv, level = 0.80)



#########################                                             EXERCISE 2

auto2 <- Auto

#get rid of name 
auto2$name <- NULL 

#make sure "horsepower" is a numeric 
auto2$horsepower <- as.numeric(auto2$horsepower)

#make origin categorical 
auto2$origin <- factor(auto2$origin)

#check 
str(auto2)
summary(auto2)


# 2A) DO A PLOT OF ALL PAIRS OF THE VARIABLES 

pairs(auto2)

# 2B) FIT A REGRESSION OF "mpg" ON ALL THE OTHERS VARIABLES. CHECK THE DIAGNOSTICS
fit_full <- lm(mpg ~ . , data = auto2)
summary(fit_full)
confint(fit_full)
par(mfrow = c(1,2))
plot(fit_full)


par(mfrow = c(1,2))
cooks <- cooks.distance(fit_full)
which.max(fit_full)

srt(cooks, decreasing = TRUE)[1,10]

fit_logmap <- lm(log(map) ~ ., data = auto2)
summary(fit_logmpg)
confint(fit_logmpg)

par(mfrow = c(1,2))
plot(fit_logmpg)


# 2C) FIT A REGRESSION OF "mpg" ON "horsepower". PLOT THE DATA AND ADD THE REGRESION LINE 

fit_hp_linear <- lm(mpg ~ horsepower, data = auto2)
summary(fit_hp_linear)


par(mfrow = c(1,1))
plot(auto2$horsepower, auto2$mpg,
     xlab = "horsepower",
     Ylab = "mpg",
     main = "mpg versus horsepower",
     pch = 20, cex = 0.6)
abline(fit_hp_linear, lwd = 2)

#the plot show as a non-linear decreasing relationship so a straight line is too restrictive. So we try a plynomial transormation

fit_hp_quadr <- lm(mpg ~ horsepower + I(horsepower^2), data = auto2)
summary(fit_hp_quadr)

plot(auto2$horsepower, auto2$mpg,
     xlab = "horsepower",
     Ylab = "mpg",
     main = "Quadreati fit: mpg vs horsepower",
     pch = 20, cex = 0.6)
abline(fit_hp_linear, lwd = 2)

hp_grid <- seq(min(auto2$horsepower), max(auto2$horsepower), lenght.out = 200)
predquad <- predict(fit_hp_quadr, newdata = data.frame(horsepower = hap_grid))

lines (hp_grid, pred_quad, lwd =2)

warnings()

# another option is to use a transformed predictor
fit_hp_log <- lm (mpg ~ log(horsepower), data = auto2)
summary(fit_hp_log)



########################                                              EXERCISE 3
#use the Medical Cost Personal 


# 3A) FIT A LINEAR MODEL TO PREDICTIC "charges" FROM ALL THE OTHER VARIABLES
link_insurance <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"
insurance <- read.csv(link_insurance, stringsAsFactors = FALSE)
str(insurance)
summary(insurance)


#convert the categorigal variables to factors 
insurance$sex <- factor(isurance$sex)
insurance$smoker <- factor(isurace$smoker)
isurance$region <- factor(isurance$region)
str(insurance)


#full linear model

fit_full <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(fit_full)
confint(fit_full)

# 3B) EXAMINE THE DIAGNOSTICS

par(mfrow = c(2,2))
plot(fit_full)


par(mfrow = c(1,1))
cooks <- cooks.distance(fit_full)
sort(cooks, decreasing = TRUE)[1,10]
which.max(cooks)

# 3C)

# 3D)
fit_no_region <- update(fit_full, ., )


















