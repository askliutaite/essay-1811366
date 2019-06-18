if(!require(tidyverse))install.packages("tidyverse")
if(!require(eurostat))install.packages("eurostat")
if(!require(forecast))install.packages("forecast")

library(eurostat)
library(tidyverse)

#elektra

nrg_cb_e <- get_eurostat("nrg_cb_e", stringsAsFactors = FALSE)

df <- nrg_cb_e %>%
  filter(geo=="EU28"&
         nrg_bal=="FC_TRA_ROAD_E"&
         time>="2008-01-01")

df$time <- format(as.Date(df$time, format = "%Y-%m-%d"), "%Y")


newdf <- df[c(5,6)]
names(newdf) <- c("Metai", "Kiekis")
newdf$Metai <- as.numeric(newdf$Metai)

theModel <- lm(Kiekis ~ Metai,data = newdf)
Metai <- 2018:2022
Kiekis <- (Metai * theModel$coef[2]) + theModel$coef[1]
summary(theModel) 
data.frame(Metai,Kiekis)
lentele <- data.frame(Metai,Kiekis)

data <- merge(x = newdf, y = lentele, by = c("Metai","Kiekis"), all = TRUE)
data$Metai=as.character(data$Metai)

ggplot(data, aes(x=Metai, y=Kiekis, group =1))+ 
  geom_line()+
  labs(x="Metai", y="Elektros kiekis", title="Elektros kiekis transporto sektoriuje Europos Sąjungos šalyse", subtitle = "Šaltinis:Eurostat(nrg_cb_e)")+
  geom_text(aes(label=round(Kiekis,1)), vjust=-0.7)


#elektromobiliai

new.electric.vehicles.in.eu.28.1 <- read.csv("C:/Users/ausri/Desktop/rasto darbas/new-electric-vehicles-in-eu-28-1.csv")

df2 <- new.electric.vehicles.in.eu.28.1 %>%
  filter(year.text>="2012")

dff <- df2[c(1,2,3)]
names(dff) <- c("Metai", "Elektromobiliai", "Hibridai")

theModel2 <- lm(Elektromobiliai ~ Metai,data = dff)
Metai <- 2018:2022
Elektromobiliai<- (Metai * theModel2$coef[2]) + theModel2$coef[1]
summary(theModel2) 
data.frame(Metai,Elektromobiliai)
lentele2 <- data.frame(Metai,Elektromobiliai)

theModel3 <- lm(Hibridai ~ Metai,data = dff)
Metai <- 2018:2022
Hibridai <- (Metai * theModel3$coef[2]) + theModel3$coef[1]
summary(theModel3) 
data.frame(Metai,Hibridai)
lentele3 <- data.frame(Metai,Hibridai)

data2 <- merge(x = lentele2, y = lentele3, by = "Metai", all = TRUE)
dataa <- merge(x = data2, y = dff, by = c("Metai","Elektromobiliai","Hibridai"), all = TRUE)
dataa$Metai=as.character(dataa$Metai)


ggplot(dataa, aes(Metai, Elektromobiliai, group=1)) + 
  geom_line(aes(y = Elektromobiliai, col = "Elektromobiliai")) + 
  geom_line(aes(y = Hibridai, col = "Hibridai"))+
  labs(x="Metai", y="Elektromobilių skaičius", title="Elektromobilių kiekis Europos Sąjungos šalyse", subtitle="Šaltinis: European Environment Agency (EEA)")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))





