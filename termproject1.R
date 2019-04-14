#setwd("C:/Users/wangw/OneDrive/Documents/School/Methods_of_Psychological_Experiments/termproject1")
library(dplyr)
library(afex)
library(tidyr)

all <- read.table(file = "selecAttention_all.csv", sep = ",", header = TRUE)
head(all) #查看資料

##整理所有人的正確率
all.acc<-all %>%
    group_by(Subject, Compatability) %>%
    summarize(acc.sum = sum(targetSquare.ACC)/128)
all.acc #查看資料
mean(all.acc$acc.sum) #所有人的平均正確率


##在compatible and incompatible 的正確率
acc.summary <- all.acc %>%
    group_by(Compatability) %>%
    summarize(acc.condition = mean(acc.sum))
acc.summary


##拿掉答錯的trails
all.correct <- all %>%
    filter(targetSquare.ACC == 1) %>%
    select(Subject, dis_loc, SOA, targetSquare.RT, Compatability, Sex)
head(all.correct) #查看資料

##拿掉界外值
no.outliers <- all.correct %>%
    filter(targetSquare.RT <= mean(targetSquare.RT) + sd(targetSquare.RT)*3 ||
               targetSquare.RT >= mean(targetSquare.RT) - sd(targetSquare.RT)*3) 
head(no.outliers) #查看資料

##檢查男女
id.data <- no.outliers %>% mutate(., id = 1:nrow(.))
sex.test <- id.data %>% group_by(Sex, Subject) %>% summarize(mean = mean(targetSquare.RT))
head(sex.test)
aov.sex <- aov(mean ~ Sex, data = sex.test)
summary(aov.sex)

##檢查location
location.test <- id.data %>% group_by(dis_loc, Subject) %>% 
    summarize(mean = mean(targetSquare.RT))
head(location.test)
aov.loc <- aov(mean ~ dis_loc, data = location.test)
summary(aov.loc)

##整理不同SOA & compatiblity的情況 for table1 summary
table1 <- no.outliers %>%
    group_by(SOA, Compatability) %>%
    summarize(mean = mean(targetSquare.RT), sd = sd(targetSquare.RT))
table1


#準備做anova
all.forAnova <- no.outliers%>%
    group_by(.,Subject, Compatability, SOA)%>%
    summarize(rt = mean(targetSquare.RT), sd=sd(targetSquare.RT), N=n())
head(all.forAnova)

model2 <- aov_car(rt ~ SOA*Compatability + Error(Subject/(SOA*Compatability)), 
                  data = all.forAnova)
summary(model2)

##多重比較
library(emmeans)
est.means <- emmeans(model2, specs = "SOA")
tukey.results <- contrast(est.means, method = "tukey")
tukey.results


##資料視覺化
library(ggplot2)

drawing <- all.forAnova %>% group_by(.,Compatability, SOA) %>%
    summarize(newrt = mean(rt), newsd = sqrt(sd(rt)), N=n())
drawing
drawing$newsd


Means.Within.SE <-ggplot(data = drawing, aes(x = SOA, y = newrt, group = Compatability))+
    geom_point(aes(colour = Compatability))+
    scale_y_continuous(breaks = seq(100,800,50))+
    scale_x_continuous(breaks = seq(50,950,300))+
    geom_line(aes(colour = Compatability), size=1.1)+
    geom_ribbon(aes(ymin=newrt-newsd, ymax=newrt+newsd, fill = Compatability), alpha=.25)+
    ylab("Response_time (ms)")+xlab("SOA (ms)")+
    theme(legend.position = "right")
Means.Within.SE

