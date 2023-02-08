white_wine <- read.table(file = 'winequality-white.csv',header = TRUE,sep=";")
red_wine <- read.table(file = 'winequality-red.csv',header = TRUE,sep=";")
#基本統計資訊
str(white_wine)
str(red_wine)
summary(white_wine)
summary(red_wine)
#載入ggplot2與GGally套件
install.packages(c('ggplot2','GGally'))
library(ggplot2) 
library(GGally)
#繪製資料分佈矩陣圖
ggpairs(white_wine)
ggpairs(red_wine)
#載入lattice套件
install.packages('lattice')
library(lattice)
#散布圖中畫出線性回歸的趨勢線
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
#建立迴歸模型
white_wine.lm <- lm(quality~ .,data=white_wine)
red_wine.lm <- lm(quality~ .,data=red_wine)
#不以科學記號方式呈現
options(scipen=999)
#查看模型配適結果
summary(white_wine.lm)
summary(red_wine.lm )
#安裝並載入ggfortify套件
install.packages("ggfortify")
library(ggfortify)
#畫出模型診斷用的圖
autoplot(white_wine.lm)
autoplot(red_wine.lm)
#殘差常態性檢定
shapiro.test(white_wine.lm$residuals)
shapiro.test(red_wine.lm$residuals)
#安裝car套件
install.packages('car')
library(car)
#殘差獨立性檢定
durbinWatsonTest(white_wine.lm)
durbinWatsonTest(red_wine.lm)
#殘差變異數同質性檢定
ncvTest(white_wine.lm)
ncvTest(red_wine.lm)
#模擬
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
