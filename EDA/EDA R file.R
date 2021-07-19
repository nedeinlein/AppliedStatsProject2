library(tidyverse)
library(GGally)

#data import
df <- read.csv('https://raw.githubusercontent.com/nedeinlein/AppliedStatsProject2/main/data_folder/adult.data.csv')
df[df==" ?"]<-NA
sum(is.na(df))
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#filter NA
df2 <- df %>% filter(!is.na(workclass))
df2 <- df2 %>% filter(!is.na(native.country))
df2 <- df2 %>% filter(!is.na(occupation))

#check of impact of filtering NA
dim(df)
dim(df2)

#summary stats
summary(df2)
#have extreme info for capital.gain and Hours/week
df2[order(-df2$hours.per.week),]
#apparently there are a number of people that work 99 hours a week
df2[order(-df2$capital.gain),]
#as well as a number that maxed the capital gain information

#clean data write to repo
write.csv(df2,"cleandata.csv")

#creating dataset for ggpairs
df3 <- df2 %>% select(c(income,age,fnlwgt,education.num,capital.gain,capital.loss,hours.per.week))
ggpairs(df3,columns = 2:7, mapping = ggplot2::aes(colour = income))

#categorical graphing
df2 %>% ggplot(aes(x= education, fill = income)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= occupation, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= relationship, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= native.country, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= workclass, fill = income)) + geom_bar()
df2 %>% ggplot(aes(x= marital.status, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= race, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
df2 %>% ggplot(aes(x= sex, fill = income)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))
