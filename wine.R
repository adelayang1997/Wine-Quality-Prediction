white_wine <- read.table(file = 'winequality-white.csv',header = TRUE,sep=";")
red_wine <- read.table(file = 'winequality-red.csv',header = TRUE,sep=";")
#�򥻲έp��T
str(white_wine)
str(red_wine)
summary(white_wine)
summary(red_wine)
#���Jggplot2�PGGally�M��
install.packages(c('ggplot2','GGally'))
library(ggplot2) 
library(GGally)
#ø�s��Ƥ��G�x�}��
ggpairs(white_wine)
ggpairs(red_wine)
#���Jlattice�M��
install.packages('lattice')
library(lattice)
#�����Ϥ��e�X�u�ʦ^�k���Ͷսu
xyplot(x=alcohol~quality,         
       data=white_wine,     
       panel=function(x,y){  
         panel.xyplot(x, y)             
         panel.lmline(x, y, col="red")  
       }
)
xyplot(x=alcohol~quality,         
       data=red_wine,     
       panel=function(x,y){  
         panel.xyplot(x, y)             
         panel.lmline(x, y, col="red")  
       }
)
#�إ߰j�k�ҫ�
white_wine.lm <- lm(quality~ .,data=white_wine)
red_wine.lm <- lm(quality~ .,data=red_wine)
#���H��ǰO���覡�e�{
options(scipen=999)
#�d�ݼҫ��t�A���G
summary(white_wine.lm)
summary(red_wine.lm )
#�w�˨ø��Jggfortify�M��
install.packages("ggfortify")
library(ggfortify)
#�e�X�ҫ��E�_�Ϊ���
autoplot(white_wine.lm)
autoplot(red_wine.lm)
#�ݮt�`�A���˩w
shapiro.test(white_wine.lm$residuals)
shapiro.test(red_wine.lm$residuals)
#�w��car�M��
install.packages('car')
library(car)
#�ݮt�W�ߩ��˩w
durbinWatsonTest(white_wine.lm)
durbinWatsonTest(red_wine.lm)
#�ݮt�ܲ��ƦP����˩w
ncvTest(white_wine.lm)
ncvTest(red_wine.lm)
#����
#white_wine
length(white_wine$quality)
z1 <- sqrt(-2*log(runif(4898)))*cos(2*pi*runif(4898))
white_wine$new_quality <- white_wine$quality+z1
new_white_wine.lm <- lm(new_quality~fixed.acidity+volatile.acidity+citric.acid
                    +residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide
                    +density+pH+sulphates+alcohol
                    ,data=white_wine)
summary(new_white_wine.lm)
autoplot(new_white_wine.lm)
shapiro.test(new_white_wine.lm$residuals)
#red_wine
length(red_wine$quality)
z2 <- sqrt(-2*log(runif(1599)))*cos(2*pi*runif(1599))
red_wine$new_quality <- red_wine$quality+z2
new_red_wine.lm <- lm(new_quality~fixed.acidity+volatile.acidity+citric.acid
                        +residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide
                        +density+pH+sulphates+alcohol
                        ,data=red_wine)
summary(new_red_wine.lm)
autoplot(new_red_wine.lm)
shapiro.test(new_red_wine.lm$residuals)