library(dplyr)
# function needed for visualization purposes
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

covid_italy_sum <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
library(drc)





y <- covid_italy_sum$totale_casi
x <- 0:( length(covid_italy_sum$totale_casi) -1)



# fitting code
fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(b= 2.43758048e-01, a=4.31177862e+04, c=1.95312145e+01))

xy <- data.frame(x=x,y=y)
model <- drm(y ~ x, fct = L.3(), data = xy)

summary(model)

plot(model, log="", main = "Logistic function")

x <- 1:50
y <- 1:50
xy <- data.frame(x=x,y=y)
xy_pred <-
  predict(model,xy,interval = 'prediction') %>%
  data.frame()

library(ggplot2)
ggplot(xy_pred,aes(x=x,y=Prediction))  +
  geom_point() +
  geom_line(aes(y = Lower), color = "red", linetype = "dashed")+
  geom_line(aes(y = Upper), color = "red", linetype = "dashed")

