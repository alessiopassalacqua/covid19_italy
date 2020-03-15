library(ggplot2)
library(coronavirus)
library(dplyr)
theme_set(theme_bw())
library(drc)


covid_italy_sum <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")

data("coronavirus")

y <-
coronavirus %>%
  filter(Country.Region == "Mainland China") %>%
  filter(type =="confirmed") %>%
  group_by(date,type) %>%
  summarise(cases = sum(cases)) %>%
  group_by(type) %>%
  mutate(cases_tot = cumsum(cases)) %>%
  pull(cases_tot)

x <- 0:(length(y)-1)


# https://www.statforbiology.com/nonlinearregression/usefulequations#logistic_curve

#-----------------
xy <- data.frame(x=x,y=y)
model <- drm(y ~ x, fct = L.3(), data = xy)
summary(model)

plot(model, log="", main = "Logistic function")

#-------------
covid_italy_sum
xy$italy <- NA
xy$italy[1:length(covid_italy_sum$totale_casi)] <- covid_italy_sum$totale_casi
xy$fit <- predict(model,data=xy)


library(grid)


ggplot(xy,aes(x=x,y=y)) +
  geom_point() +
  geom_line(aes(y=fit)) +
  geom_point(colour="orange",aes(y=italy)) +
  annotation_custom(grob) +
  annotate(geom = "text", x =  5, y = 70000,
           label = "Cina", hjust = 0) +
  annotate("pointrange", x = 3.5, y = 70000, ymin = 69500,
           ymax = 70500,colour = "black", size = 0.7)+
  annotate(geom = "text", x = 5, y = 65000,
           label = "Fit logistico", hjust = 0) +
  annotate(geom = "segment", x = 2.5,xend = 4,
           y = 65000,yend=65000) +
  annotate("pointrange", x = 3.5, y = 60000, ymin = 59500,
           ymax = 60500,colour = "orange", size = 0.7)+
  annotate(geom = "text", x = 5, y = 60000,
           label = "Italia", hjust = 0) +
  labs(title = "Confronto Italia - Cina",
       subtitle = "Casi Totali Coronavirus",
       caption = "Dati Italia: Protezione Civile \n
       Dati Cina: Johns Hopkins CSSE",
       x = "Giorni",
       y = "Totale Positivi")




