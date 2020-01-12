library(data.table)

library(ISLR)
library(MASS)
library(boot)
library(splines)
library(gam)
library(leaps)
library(glmnet)

WineDataInput <- read.csv(file="winemag-data-130k-v2.csv", header=TRUE, sep=",")
WineData <-na.omit(WineDataInput)
WineData <- subset(WineData, select = c(country, points, price, province, title, variety))
Continent <- read.csv(file="continents.csv", header=TRUE, sep=",")
WineData = merge(x = WineDataInput, y = Continent, by = "country")

#Dummy Variables creation ends at line 219
WineData$EastEurope[WineData$sub.region == 'Eastern Europe'] <- 1
WineData$EastEurope[WineData$sub.region != 'Eastern Europe'] <- 0

WineData$LatinAmerica[WineData$sub.region == 'Latin America and the Caribbean'] <- 1
WineData$LatinAmerica[WineData$sub.region != 'Latin America and the Caribbean'] <- 0

WineData$NorthAmerica[WineData$sub.region == 'Northern America'] <- 1
WineData$NorthAmerica[WineData$sub.region != 'Northern America'] <- 0

WineData$SouthEurope[WineData$sub.region == 'Southern Europe'] <- 1
WineData$SouthEurope[WineData$sub.region != 'Southern Europe'] <- 0

WineData$SubSahara[WineData$sub.region == 'Sub-Saharan Africa'] <- 1
WineData$SubSahara[WineData$sub.region != 'Sub-Saharan Africa'] <- 0

WineData$WestEurope[WineData$sub.region == 'Western Europe'] <- 1
WineData$WestEurope[WineData$sub.region != 'Western Europe'] <- 0

WineData$Americas[WineData$Continent == 'Americas'] <- 1
WineData$Americas[WineData$Continent != 'Americas'] <- 0

WineData$Oceania[WineData$Continent == 'Oceania'] <- 1
WineData$Oceania[WineData$Continent != 'Oceania'] <- 0

WineData$Europe[WineData$Continent == 'Europe'] <- 1
WineData$Europe[WineData$Continent != 'Europe'] <- 0

WineData$Austria[WineData$country == 'Austria'] <- 1
WineData$Austria[WineData$country != 'Austria'] <- 0

WineData$Australia[WineData$country == 'Australia'] <- 1
WineData$Australia[WineData$country != 'Australia'] <- 0

WineData$Brazil[WineData$country == 'Brazil'] <- 1
WineData$Brazil[WineData$country != 'Brazil'] <- 0

WineData$Bulgaria[WineData$country == 'Bulgaria'] <- 1
WineData$Bulgaria[WineData$country != 'Bulgaria'] <- 0

WineData$Canada[WineData$country == 'Canada'] <- 1
WineData$Canada[WineData$country != 'Canada'] <- 0

WineData$Chile[WineData$country == 'Chile'] <- 1
WineData$Chile[WineData$country != 'Chile'] <- 0

WineData$Chile[WineData$country == 'Chile'] <- 1
WineData$Chile[WineData$country != 'Chile'] <- 0

WineData$Germany[WineData$country == 'Germany'] <- 1
WineData$Germany[WineData$country != 'Germany'] <- 0

WineData$Hungary[WineData$country == 'Hungary'] <- 1
WineData$Hungary[WineData$country != 'Hungary'] <- 0

WineData$Mexico[WineData$country == 'Mexico'] <- 1
WineData$Mexico[WineData$country != 'Mexico'] <- 0

WineData$Italy[WineData$country == 'Italy'] <- 1
WineData$Italy[WineData$country != 'Italy'] <- 0

WineData$India[WineData$country == 'India'] <- 1
WineData$India[WineData$country != 'India'] <- 0

WineData$Peru[WineData$country == 'Peru'] <- 1
WineData$Peru[WineData$country != 'Peru'] <- 0

WineData$Portugal[WineData$country == 'Portugal'] <- 1
WineData$Portugal[WineData$country != 'Portugal'] <- 0

WineData$Slovenia[WineData$country == 'Slovenia'] <- 1
WineData$Slovenia[WineData$country != 'Slovenia'] <- 0

WineData$SAfrica[WineData$country == 'South Africa'] <- 1
WineData$SAfrica[WineData$country != 'South Africa'] <- 0

WineData$Spain[WineData$country == 'Spain'] <- 1
WineData$Spain[WineData$country != 'Spain'] <- 0

WineData$US[WineData$country == 'US'] <- 1
WineData$US[WineData$country != 'US'] <- 0

WineData$Aidani[WineData$variety == 'Aidani'] <- 1
WineData$Aidani[WineData$variety != 'Aidani'] <- 0

WineData$Airen[WineData$variety == 'Airen'] <- 1
WineData$Airen[WineData$variety != 'Airen'] <- 0

WineData$Brachetto[WineData$variety == 'Brachetto'] <- 1
WineData$Brachetto[WineData$variety != 'Brachetto'] <- 0

WineData$Chenin[WineData$variety == 'Chenin Blanc-Sauvignon Blanc'] <- 1
WineData$Chenin[WineData$variety != 'Chenin Blanc-Sauvignon Blanc'] <- 0

WineData$Volpe[WineData$variety == 'Coda di Volpe'] <- 1
WineData$Volpe[WineData$variety != 'Coda di Volpe'] <- 0

WineData$Blanc[WineData$variety == 'Blanc'] <- 1
WineData$Blanc[WineData$variety != 'Blanc'] <- 0

WineData$Colombard[WineData$variety == 'Colombard'] <- 1
WineData$Colombard[WineData$variety != 'Colombard'] <- 0

WineData$Gragnano[WineData$variety == 'Gragnano'] <- 1
WineData$Gragnano[WineData$variety != 'Gragnano'] <- 0


WineData$Kinali[WineData$variety == 'Kinali Yapincak'] <- 1
WineData$Kinali[WineData$variety != 'Kinali Yapincak'] <- 0

WineData$Lambrusco[WineData$variety == 'Lambrusco'] <- 1
WineData$Lambrusco[WineData$variety != 'Lambrusco'] <- 0

WineData$Muskat[WineData$variety == 'Muskat Ottonel'] <- 1
WineData$Muskat[WineData$variety != 'Muskat Ottonel'] <- 0

WineData$Picapoll[WineData$variety == 'Picapoll'] <- 1
WineData$Picapoll[WineData$variety != 'Picapoll'] <- 0

WineData$Pigato[WineData$variety == 'Pigato'] <- 1
WineData$Pigato[WineData$variety != 'Pigato'] <- 0

WineData$Pignoletto[WineData$variety == 'Pignoletto'] <- 1
WineData$Pignoletto[WineData$variety != 'Pignoletto'] <- 0

WineData$Gamay[WineData$variety == 'Pinot Noir-Gamay'] <- 1
WineData$Gamay[WineData$variety != 'Pinot Noir-Gamay'] <- 0

WineData$Semillon[WineData$variety == 'Semillon-Sauvignon Blanc'] <- 1
WineData$Semillon[WineData$variety != 'Semillon-Sauvignon Blanc'] <- 0

WineData$Tempranillo[WineData$variety == 'Tempranillo Blanco'] <- 1
WineData$Tempranillo[WineData$variety != 'Tempranillo Blanco'] <- 0

WineData$Tinta[WineData$variety == 'Tinta del Pais'] <- 1
WineData$Tinta[WineData$variety != 'Tinta del Pais'] <- 0

WineData$Bual[WineData$variety == 'Bual'] <- 1
WineData$Bual[WineData$variety != 'Bual'] <- 0

WineData$CabernetShiraz[WineData$variety == 'Cabernet-Shiraz'] <- 1
WineData$CabernetShiraz[WineData$variety != 'Cabernet-Shiraz'] <- 0

WineData$Gelber[WineData$variety == 'Gelber Traminer'] <- 1
WineData$Gelber[WineData$variety != 'Gelber Traminer'] <- 0

WineData$Terrantez[WineData$variety == 'Terrantez'] <- 1
WineData$Terrantez[WineData$variety != 'Terrantez'] <- 0

WineData <-na.omit(WineData)

WineData$Michigan[WineData$province == 'Michigan'] <- 1
WineData$Michigan[WineData$province != 'Michigan'] <- 0

WineData$Virginia[WineData$province == 'Virginia'] <- 1
WineData$Virginia[WineData$province != 'Virginia'] <- 0

WineData$Illinois[WineData$province == 'Illinois'] <- 1
WineData$Illinois[WineData$province != 'Illinois'] <- 0

WineData$NewMexico[WineData$province == 'New Mexico'] <- 1
WineData$NewMexico[WineData$province != 'New Mexico'] <- 0

WineData$Jersey[WineData$province == 'New Jersey'] <- 1
WineData$Jersey[WineData$province != 'New Jersey'] <- 0

WineData$Pennsylvania[WineData$province == 'Pennsylvania'] <- 1
WineData$Pennsylvania[WineData$province != 'Pennsylvania'] <- 0

WineData$Missouri[WineData$province == 'Missouri'] <- 1
WineData$Missouri[WineData$province != 'Missouri'] <- 0

WineData$Nevada[WineData$province == 'Nevada'] <- 1
WineData$Nevada[WineData$province != 'Nevada'] <- 0

WineData$Colorado[WineData$province == 'Colorado'] <- 1
WineData$Colorado[WineData$province != 'Colorado'] <- 0

WineData$Arizona[WineData$province == 'Arizona'] <- 1
WineData$Arizona[WineData$province != 'Arizona'] <- 0

WineData$Massachusetts[WineData$province == 'Massachusetts'] <- 1
WineData$Massachusetts[WineData$province != 'Massachusetts'] <- 0

WineData$Ohio[WineData$province == 'Ohio'] <- 1
WineData$Ohio[WineData$province != 'Ohio'] <- 0

WineData$Iowa[WineData$province == 'Iowa'] <- 1
WineData$Iowa[WineData$province != 'Iowa'] <- 0

WineData$NorthCarolina[WineData$province == 'North Carolina'] <- 1
WineData$NorthCarolina[WineData$province != 'North Carolina'] <- 0

WineData$Napa[WineData$region_1 == 'Napa Valley'] <- 1
WineData$Napa[WineData$region_1 != 'Napa Valley'] <- 0

WineData$RRValley[WineData$region_1 == 'Russian River Valley'] <- 1
WineData$RRValley[WineData$region_1 != 'Russian River Valley'] <- 0

WineData$Sonoma[WineData$region_1 != 'Sonoma Valley'] <- 0
WineData$Sonoma[WineData$region_1 %like% 'Sonoma'] <- 1

#parameter selection top 20 using forward stepwise selection
length(WineData)
colnames(WineData)

drops <- c("country","X","description","designation","province","region_1","region_2","taster_name","taster_twitter_handle", "title", "variety","winery","Continent","sub.region")
newwine <- WineData[ , !(names(WineData) %in% drops)]


# creating the training and test data
set.seed(1)
train <- sample(1:nrow(WineData), 0.8*nrow(WineData)) 
test <- -train
trainingData <- newwine[train, ]
testData <- newwine[test, ]
colnames(newwine)
fit <- regsubsets(points ~ ., data = newwine,  nvmax = 60, method = "forward")
fit.summary <- summary(fit)
plot(fit.summary$bic, xlab = "Number of variables", ylab = "BIC", type='b')
plot(fit.summary$cp, xlab = "Number of variables", ylab = "CP", type='b')
plot(fit.summary$adjr2, xlab = "Number of variables", ylab = "adjr2", type='b')

fit <- regsubsets(points ~ ., data = newwine,  nvmax = 20, method = "forward")
coeffs <- coef(fit, id = 20)
names(coeffs)


#fitting different models and comparing results

#linear
fitlm = lm(points~price+  LatinAmerica + SubSahara + Brazil + 
             Canada + Germany  + Mexico + Brachetto + Gragnano + Gamay + Pennsylvania + Missouri + Colorado +
             Massachusetts + Ohio + NorthCarolina,data=trainingData)
summary(fitlm)
yhat <- predict(fitlm, testData)
mselm <- mean((testData$points - yhat)^2)
mselm

#poly
fitply= lm(points~poly(price,3)  +  LatinAmerica + SubSahara + Brazil + 
             Canada + Germany  + Mexico + Brachetto + Gragnano + Gamay + Pennsylvania + Missouri + Colorado +
             Massachusetts + Ohio + NorthCarolina,data=trainingData)
summary(fitply)
yhat <- predict(fitply, testData)
msepoly <- mean((testData$points - yhat)^2)
msepoly


#fitting with spline
x=trainingData$price
y=trainingData$points
ytest = testData$points
xtest = testData$price
yhatsp=predict(smooth.spline(x,y),xtest)$y
msespline=mean((ytest-yhatsp)^2)
msespline

sst <- sum((ytest - mean(ytest))^2)
sse <- sum((yhatsp - ytest)^2)
rsqspline <- 1 - sse / sst
rsqspline

plot(trainingData$price,trainingData$points,col="darkgrey")
fitSpl = smooth.spline(trainingData$price,trainingData$points,cv=TRUE)
lines(fitSpl,col="red",lwd=2)


#fitting with spline and other predictors

fitgam=gam(points~s(price,18)+  LatinAmerica + SubSahara + Brazil + 
             Canada + Germany  + Mexico + Brachetto + Gragnano + Gamay + Pennsylvania + Missouri + Colorado +
             Massachusetts + Ohio + NorthCarolina,data=trainingData)

yhatsp =predict(fitgam, testData)
msegam=mean((ytest-yhatsp)^2)
msegam
sst <- sum((ytest - mean(ytest))^2)
sse <- sum((yhatsp - ytest)^2)
rsqgam <- 1 - sse / sst
rsqgam
#fit model to ridge regression

x=model.matrix(points~poly(price,3)+  LatinAmerica + SubSahara + Brazil + 
                 Canada + Germany  + Mexico + Brachetto + Gragnano + Gamay + Pennsylvania + Missouri + Colorado +
                 Massachusetts + Ohio + NorthCarolina,data=newwine)[,-1]
y=newwine$points

grid = 10^seq(10,-2,length=100)
train = sample(1:nrow(x), 0.8*nrow(x))
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train],alpha = 0, lambda = grid, thresh=1e-12)
yhat = predict(ridge.mod,s=4,newx=x[test,])
mseridge <- mean((yhat-y.test)^2)
mseridge

#fit model to Lasso 
ridge.mod = glmnet(x[train,], y[train],alpha = 1, lambda = grid, thresh=1e-12)
yhat = predict(ridge.mod,s=4,newx=x[test,])
mselasso <- mean((yhat-y.test)^2)
mselasso
