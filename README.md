# Rstudio

setwd("C:\\Users\\zhang\\Desktop\\海北72样方\\2022")

library(readxl)
library(tidyverse)

# com2022 <- read_xlsx("2022.09海北72盖度 刘向.xlsx",sheet="Sheet3")
# 
# data1 <- com2022[,1:3]%>%full_join(data)%>%
#   spread(key = "Species",value = "Cover_2022")%>%as.data.frame()%>%
#   na.zero()%>%write.csv("Species.csv")
# data2 <- read.delim("clipboard")%>%gather(key = "Species",value = "Cover_2022",-Sample)%>%
#   write.csv("Species1.csv")
# 
# # data <- read.delim("clipboard")
# 
# data1 <- com2022[,1:3]%>%right_join(data)%>%write.csv("Species.csv")
# 
# data1 <- data%>%left_join(com2022[,1:3])%>%write.csv("Species.csv")

Cover2022 <- read_xlsx("2020-2022_cover.xlsx")

## Three year cover data to become community data

Cover2020 <- Cover2022 %>% select(Plot,Species,Cover_2020) %>%
  spread(Species,Cover_2020)%>%mutate(Year=2020)
Cover2021 <- Cover2022 %>% select(Plot,Species,Cover_2021) %>% 
  spread(Species,Cover_2021)%>%mutate(Year=2021)

Cover_com <- Cover2022 %>% select(Plot,Species,Cover_2022) %>% 
  spread(Species,Cover_2022)%>%mutate(Year=2022) %>% 
  full_join(Cover2020)%>%full_join(Cover2021)

## Calculate alpha diversity

library(vegan)
SR <- specnumber(Cover_com %>% select(-c("Plot","Year")),MARGIN=1)
Shannon <- diversity(Cover_com %>% select(-c("Plot","Year")),
                     index = "shannon",MARGIN=1)
Simpson <- diversity(Cover_com %>% select(-c("Plot","Year")),
                     index = "simpson",MARGIN=1)
Pielou <- Shannon/log(SR)
# Result <- data.frame(Cover_com$Year,Cover_com$Plot,SR,Shannon,
#                      Simpson,Pielou) %>% 
#   rename("Year"="Cover_com.Year","Plot"="Cover_com.Plot") %>% 
#   write.csv("Result.csv")

## Linear mixed effect model

Result <- read_xlsx("2020-2022_cover.xlsx",sheet = "Result")
summary(Result)

Result$Year <- as.factor(Result$Year)
Result$Plot <- as.factor(Result$Plot)
Result$Treatment <- as.factor(Result$Treatment)
Result$Block <- as.factor(Result$Block)
Result$Fencing <- as.factor(Result$Fencing)
Result$A <- as.factor(Result$B)
Result$B <- as.factor(Result$B)
Result$N <- as.factor(Result$N)
library(lme4)
library(lmerTest)

hist(resid(model))
shapiro.test(resid(model))

model0 <- lmer(SR~Fencing*A*B*N+Year+(1|Year/Block),Result)
model <- lmer(SR~Fencing+A+B+N+Fencing:A+Fencing:B+
                Fencing:N+A:B+A:N+B:N+Year+(1|Year/Block),
              Result)
model <- lmer(SR~Fencing*A*B*N+(1|Block),
              Result %>% filter(Year=="2022"))
summary(model)
anova(model)

qqPlot(resid(model))
hist(resid(model))
hist((Result$Shannon)^3.5)
shapiro.test(resid(model))

model <- lmer((Shannon)^3~Fencing+A+B+N+Fencing:A+Fencing:B+
                Fencing:N+A:B+A:N+B:N+(1|Block),
              Result %>% filter(Year=="2022"))

library(MASS)
bc <- boxcox(Simpson~Fencing+A+B+N+Fencing:A+Fencing:B+
               Fencing:N+A:B+A:N+B:N,data=Result)
which(bc$y==max(bc$y))
bc$x[100]

model <- lmer((Simpson)^13~Fencing+A+B+N+Fencing:A+Fencing:B+
                Fencing:N+A:B+A:N+B:N+(1|Block),
              Result %>% filter(Year=="2022"))
summary(model)
anova(model)

## Plot mean+-se point
library(sciplot)
library(ggsci)
library(viridis)
Result %>% group_by(Year,Treatment,Fencing,A,B,N) %>% 
  summarise(mean1=mean(SR),se1=se(SR),
            mean2=mean(Shannon),se2=se(Shannon),
            mean3=mean(Simpson),se3=se(Simpson),
            mean4=mean(Pielow),se4=se(Pielow)) %>% filter(Year=="2022") %>% 
  ggplot(aes(Treatment,mean1,color=Treatment))+
  geom_point(size=3)+
  geom_linerange(aes(min=mean1-se1, max=mean1+se1), size=1.2) +
  facet_wrap(~Fencing)+
  theme_classic() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(legend.position = c(0.3, 0.25), legend.direction = 'vertical') +
  scale_color_viridis(discrete=TRUE, option = 'viridis') +
  theme(axis.title.x = element_text(size=14)) + theme(axis.title.y = element_text(size=14)) + theme(axis.text.x=element_text(size=14)) + theme(axis.text.y=element_text(size=14)) +
  theme(legend.text=element_text(size=14)) + theme(legend.title=element_text(size=14)) + theme(legend.title=element_blank())
