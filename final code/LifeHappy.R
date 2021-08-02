library(tidyverse)
library(gridExtra)
library(reshape2)
library(dplyr)
library(arsenal)
library(ggplot2)
library(ggthemes)

#Code imports data from my PC (WHO data pre-cleaned for 2015)
library(readxl)
WHO15 <- read_excel("C:/Users/wikto/OneDrive/Desktop/Data clean variations/WHO15.xlsx")
View(WHO15)

#importing the 2015 happiness scores using a differnt method/code
happy <- read_csv('C:/Users/wikto/OneDrive/Desktop/Data clean variations/2015.csv')
View(happy)

#performing a merge of data between the happiness data and WHO data
merge(WHO15,happy) -> mergeleha

view(mergeleha)

#visualisation to show the difference between developed and developing countries in terms of life expectancy 
mergeleha %>%
  ggplot(aes(x=`Life expectancy`)) + 
  geom_point(aes(y=`Happiness Score`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "Life expectancy", y="Happiness score", subtitle = "Are people who are likely to live longer also happier ?")

#Visualisation showing the extent to which GDP per capita dictates happiness
mergeleha %>%
  ggplot(aes(x=`Life expectancy`)) + 
  geom_point(aes(y=`Economy (GDP per Capita)`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "Life expectancy", y="Extent to which GDP contibuted to a countries happiness score", subtitle = "Are people who are likely to live longer also happier ?")


mergeleha %>%
  ggplot(aes(x=`Life expectancy`)) + 
  geom_point(aes(y=`GDP`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "Life expectancy", y="GDP", subtitle = "Are people who are likely to live longer also happier ?")


#purely GDP and happiness
mergeleha %>%
  ggplot(aes(x=`Happiness Score`)) + 
  geom_point(aes(y=`GDP`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "Happiness", y="GDP", subtitle = "Are people who are likely to live longer also happier ?")



#importing gdp PPP
GDPPPP <- read_csv('C:/Users/wikto/OneDrive/Desktop/Data clean variations/GDP PPP.csv')

view(GDPPPP)

#merge for GDP PPP
#This is a merge of GDPPPP with WHO15&happiness...
merge(WHO15,GDPPPP) -> mergelehaPPP

view(mergelehaPPP)


#import air data
Air15 <- read_csv('C:/Users/wikto/OneDrive/Desktop/Data clean variations/Air2015.csv')
view(Air15)


#merging with air pollution 2015
merge(Air15,mergelehaPPP) -> mergelehaPPPAir

view(mergelehaPPPAir)

na.omit(mergelehaPPPAir) -> lehaGDPair


#Here I have the cleaned merge with air pollution without ant null values
view(lehaGDPair)


#visualisation showing gdp ppp and life expectancy
lehaGDPair %>%
  ggplot(aes(x=`Life expectancy`)) + 
  geom_point(aes(y=`GDPPPP`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "Life expectancy", y="GDP", subtitle = "Are people who are likely to live longer also happier ?")

lehaGDPair %>%
  ggplot(aes(x=`GDPPPP`)) + 
  geom_point(aes(y=`Life expectancy`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "GDP (PPP)", y="Life Expectancy", subtitle = "Are people who are likely to live longer also happier ?")


ggplot(lehaGDPair, aes(x=`GDPPPP`, y =`Life expectancy`, color=Status ))+
  geom_point()


#LINEAR REGRESSIONS 

#Two lines in regression for developed and developing GDP and Life
ggplot(lehaGDPair, aes(x=`Life expectancy`, y =`GDPPPP`, color=Status ))+
  geom_point()+
  labs(x= "Life expectancy", y= "GDP (PPP)", title = "plotting GDP (PPP) vs Life Expectancy (with respect to country status)")+
  geom_smooth(method = 'lm', se = FALSE)


#single line for GDP and Life
ggplot(lehaGDPair, aes(x=`Life expectancy`, y =`GDPPPP`))+
  geom_point()+
  labs(x= "Life expectancy", y= "GDP (PPP)", title = "Plotting GDP (PPP) with Life Expectancy")+
  geom_smooth(method = 'lm', se = FALSE)


#merging two dataframes

merge(lehaGDPair,mergeleha) -> lehappygdpair

view(lehappygdpair)

#Two lines in regression 
ggplot(lehappygdpair, aes(x=`Happiness Score`, y =`GDPPPP`, color=Status ))+
  geom_point()+
  labs(x= "Happiness", y= "GDP (PPP)", title = "Plotting GDP (PPP) vs Happiness (with respect to country status)")+
  geom_smooth(method = 'lm', se = TRUE)


#single line f
ggplot(lehappygdpair, aes(x=`Happiness Score`, y =`GDPPPP`))+
  geom_point()+
  labs(x= "Happiness", y= "GDP (PPP)", title = "Plotting GDP (PPP) with Happiness")+
  geom_smooth(method = 'lm', se = TRUE)





#Two lines in regression AIR POLLUTION  
ggplot(lehappygdpair, aes(x=`Life expectancy`, y =`Air pollution 2015`, color=Status ))+
  geom_point()+
  labs(x= "Life Expectancy", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and Life Expectancy by Status")+
  geom_smooth(method = 'lm', se = TRUE)


#single line f
ggplot(lehappygdpair, aes(x=`Life expectancy`, y =`Air pollution 2015`))+
  geom_point()+
  labs(x= "Life Expectancy", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and Life Expectancy")+
  geom_smooth(method = 'lm', se = TRUE)



#Two lines in regression AIR POLLUTION and gdp  

ggplot(lehappygdpair, aes(x=`Life expectancy`, y =`Air pollution 2015`, color=Status ))+
  geom_point()+
  labs(x= "Life Expectancy", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and Life Expectancy by Status")+
  geom_smooth(method = 'lm', se = TRUE)


#single line f
ggplot(lehappygdpair, aes(x=`GDPPPP`, y =`Air pollution 2015`))+
  geom_point()+
  labs(x= "Life Expectancy", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and Life Expectancy")+
  geom_smooth(method = 'lm', se = TRUE)

ggplot(lehappygdpair, aes(x=`Air pollution 2015`, y =`GDPPPP`))+
  geom_point()+
  labs(x= "Pollution (PM2.5 Particles in air per cubic meter)", y= "GDP (PPP)", title = " Pollution and Life Expectancy")+
  geom_smooth(method = 'lm', se = FALSE)

ggplot(lehappygdpair, aes(x=`Air pollution 2015`, y =`GDPPPP`, color=Status ))+
  geom_point()+
  labs(x= "Pollution (PM2.5 Particles in air per cubic meter)", y= "GDP (PPP)", title = " Pollution and GDP (PPP) by Status")+
  geom_smooth(method = 'lm', se = FALSE)


# Pollution and gdp ppp with lines FINAL version
ggplot(lehappygdpair, aes(x=`GDPPPP`, y =`Air pollution 2015`, color=Status ))+
  geom_point()+
  labs(x= "GDP (PPP)", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and GDP (PPP) by Status")+
  geom_smooth(method = 'lm', se = FALSE)


ggplot(lehappygdpair, aes(x=`GDPPPP`, y =`Air pollution 2015`))+
  geom_point()+
  labs(x= "GDP (PPP)", y= "Pollution (PM2.5 Particles in air per cubic meter)", title = " Pollution and Life Expectancy")+
  geom_smooth(method = 'lm', se = FALSE)


##### Without lines but with status colour codes

lehaGDPair %>%
  ggplot(aes(x=`GDPPPP`)) + 
  geom_point(aes(y=`Air pollution 2015`, color=Status))+
  labs(title = "Happiness and Life expectancy, contrast between developed and developing countries", x= "GDP (PPP)", y="Pollution (PM2.5 Particles in air per cubic meter)", subtitle = "Are people who are likely to live longer also happier ?")








#exporting the merged and processed data
write.csv(mergeleha,"C:/Users/wikto/OneDrive/Desktop/FYP/Data export/mergeleha.csv", row.names = TRUE)

write.csv(mergelehaPPP,"C:/Users/wikto/OneDrive/Desktop/FYP/Data export/mergelehaPPP.csv", row.names = FALSE)

write.csv(lehappygdpair,"C:/Users/wikto/OneDrive/Desktop/FYP/Data export/lehappygdpair.csv", row.names = TRUE)


