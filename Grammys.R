## IMPORTING DATA ###############################################################
library(readxl)

## For the 4 main categories
ROTY <- read_excel('ROTY.xlsx')
AOTY <- read_excel('AOTY.xlsx')
SOTY <- read_excel('SOTY.xlsx')
BNA <- read_excel('BNA.xlsx')

## For the two genre categories
BPS <- read_excel('BPS.xlsx')
BPDGP <- read_excel('BPDGP.xlsx')

## For the nationalities data set
# 1 if the artist(s) or any featuring are from an English speaking country.
nat <- read_excel('nationalities.xlsx') 
nats <- nat$nationality

#Example on how to add the nationalities of the artists in the data sets.

nationality <- rep(0,length(ROTY$artist))
eng <- rep(0,length(ROTY$artist))
ROTY <- cbind(ROTY, nationality, eng)

for(i in 1:length(nat$artist)){
  for (j in 1:length(ROTY$artist)){
    if(ROTY$artist[j] == nat$artist[i]){
      ROTY$nationality[j] = nats[i]
      ROTY$eng[j] = nat$`English speaking country`[i]
    }
  }
}

nationality <- rep(0,length(SOTY$artist))
eng <- rep(0,length(SOTY$artist))
SOTY <- cbind(SOTY, nationality, eng)
AOTY <- cbind(AOTY, nationality, eng)
BNA <- cbind(BNA, nationality, eng)

for(i in 1:length(nat$artist)){
  for (j in 1:length(SOTY$artist)){
    if(SOTY$artist[j] == nat$artist[i]){
      SOTY$nationality[j] = nats[i]
      SOTY$eng[j] = nat$`English speaking country`[i]
    }
    if(AOTY$artist[j] == nat$artist[i]){
      AOTY$nationality[j] = nats[i]
      AOTY$eng[j] = nat$`English speaking country`[i]
    }
    if(BNA$nominee[j] == nat$artist[i]){
      BNA$nationality[j] = nats[i]
      BNA$eng[j] = nat$`English speaking country`[i]
    }
  }
}

nationality <- rep(0,length(BPDGP$artist))
eng <- rep(0,length(BPDGP$artist))
BPDGP <- cbind(BPDGP, nationality, eng)

for(i in 1:length(nat$artist)){
  for (j in 1:length(BPDGP$artist)){
    if(BPDGP$artist[j] == nat$artist[i]){
      BPDGP$nationality[j] = nats[i]
      BPDGP$eng[j] = nat$`English speaking country`[i]
    }
  }
}

nationality <- rep(0,length(BPS$artist))
eng <- rep(0,length(BPS$artist))
BPS <- cbind(BPS, nationality, eng)

for(i in 1:length(nat$artist)){
  for (j in 1:length(BPS$artist)){
    if(BPS$artist[j] == nat$artist[i]){
      BPS$nationality[j] = nats[i]
      BPS$eng[j] = nat$`English speaking country`[i]
    }
  }
}

#################################################################################

## VECTORIZING AND VISUALIZING THE DATA #########################################

y <- c(ROTY$winner, SOTY$winner, AOTY$winner, BNA$winner, BPS$winner, BPDGP$winner)
x <- c(ROTY$nationality, SOTY$nationality, AOTY$nationality, BNA$nationality, BPS$nationality, BPDGP$nationality)
eng <- c(ROTY$eng, SOTY$eng, AOTY$eng, BNA$eng, BPS$eng, BPDGP$eng)

tmp <- numeric()
for(i in 1:length(y)){
  if(y[i]==1 & eng[i]==1) tmp <-c(tmp,1)
  else if(y[i]==1 & eng[i]==0) tmp <- c(tmp,2)
}

## Histogram with the proportion of winners from ESC (English-speaking countries) and NESC (non ESC)
hist(tmp, xlab = 'Winners', col='#E2252B', main = 'Frequency of winners')

tmp <- numeric()
for(i in 1:length(y)){
  if(y[i]==1) tmp <-c(tmp,x[i])
}
tmp <- as.data.frame(table(tmp))

## Barplot with the proportion of winners by nationality
barplot(tmp$Freq, ylim=c(0,70), ylab='Frequency', xlab='Countries', col='#E2252B', main='Frequency of winners by countries')
axis(side=1, at=c(3), labels=c('American'))
axis(side=1, at=c(6.7), labels=c('British'))
text(locator(1), "58", pos=3)
text(locator(1), "30", pos=3)

#################################################################################

## FIRST FIT OF THE DATA ########################################################

## Factorizing the nationalities to work with a numerical variable
factor_nationalities <- factor(x)
x <- as.numeric(factor_nationalities)

tmp <- as.data.frame(table(x))

weights <- tmp$Freq[x]

## Simple linear model
fit.1 <- lm(y ~ x-1, weights = weights)
summary(fit.1)

## Generalised linear model
fit.2 <- glm(y ~ x-1, family=binomial, weights = weights)
summary(fit.2)

## Plot of both models.
plot(x,y)
xv <- seq(min(x), max(x), 0.01)
yv <- predict(fit.1, list(x=xv), type='response') #linear regression
lines(xv,yv, col='#4C0805')
yv <- predict(fit.2, list(x=xv), type='response') #logistic regression
lines(xv,yv, col='red')

#################################################################################

##  PREPARING THE BILLBOARD DATASET #############################################

## Uploading the data

BB100 <- read.csv('billboard.csv')
BB100 <- subset(BB100, select = -c(last.week))
BB100 <- BB100[BB100$date > '2010-12-31' & BB100$date < '2021-01-01',]
rownames(BB100) <- NULL #resets row index numbers

## Grasping the year from the the dates

for (i in 1:length(BB100$date)){
  BB100$date[i] <- as.integer(substr(BB100$date[i], start = 1, stop = 4))
}

## Grouping songs by year, title and artist and leaving only the maximum number of weeks on borad

library(dplyr)
data <- as.data.frame(BB100 %>% group_by(date, song, artist) %>% slice(which.max(weeks.on.board)))

#################################################################################

## COMPUTING THE P_INDEX ########################################################

## Retrieving the data to calculate the p_index and computing it
weeks.per.year <- rep(0, length(data$weeks.on.board))
p_index <- rep(0, length(data$artist))
xtop3 <- rep(0, length(data$artist))
xtop10 <- rep(0, length(data$artist))
data <- cbind(data, weeks.per.year, xtop3, xtop10, p_index)

songs <- data$song
artist <- data$artist

for (j in 1:length(songs)){
  if (data$weeks.per.year[j] == 0){
    tmp <- data[data$song == songs[j] & data$artist == artist[j],]
    subBB100 <- BB100[BB100$song == songs[j] & BB100$artist == artist[j],]
    for (i in 1:length(tmp$date)){
      if(i==1){
        tmp$weeks.per.year[i] <- tmp$weeks.on.board[i]
      }else{
        tmp$weeks.per.year[i] <- tmp$weeks.on.board[i] - sum(tmp$weeks.on.board[(i-1)])
      }
    }
    
    row.index <- as.numeric(rownames(tmp))
    for(k in 1:length(row.index)){
      data[row.index[k], ] <- tmp[k, ]
      data$xtop3[row.index[k]] <- length(subBB100$rank[subBB100$rank<4])
      data$xtop10[row.index[k]] <- length(subBB100$rank[subBB100$rank>4 & subBB100$rank<11])
    }
  }
}

for (i in 1:length(data$p_index)){
  if(data$peak.rank[i] > 10) top = 1
  else if(3 < data$peak.rank[i] | data$peak.rank[i] <= 10) {top = 1.5}
  else if(data$peak.rank[i] <= 3) {top = 2}
  data$p_index[i] <- data$weeks.per.year[i]/data$peak.rank[i] + (100-data$rank[i])*top/100 + data$xtop3[i] + 0.5*data$xtop10[i]
}

#################################################################################

## P_INDEX BY YEAR ##############################################################

## Setting strings to lower
data$artist <- tolower(data$artist)
data$song <- tolower(data$song)

## Retrieving data from 2012 onwards to start on the same year for every data set
ROTY <- ROTY[ROTY$year >= 2012, ] 
SOTY <- SOTY[SOTY$year >= 2012, ] 

## Vectors to keep the data
grammyw <- c()
gnmax <- c()
gnmean <- c()

## To use other data sets just change 'ROTY' for the data set you want to use
w <- BPDGP[BPDGP$winner == 1, ]
w$nominee <- tolower(w$nominee)
w$artist <- tolower(w$artist)
songs <- w$nominee
artist <- w$artist
years <- w$year

winners <- c()
songs_w_pi <- c()
for (i in 1:length(songs)){
  winners <- c(winners, data$p_index[data$song == songs[i] & data$artist == artist[i] & 
                                       data$date == years[i]-1])
  songs_w_pi <- c(songs_w_pi, data$song[data$song == songs[i] & data$artist == artist[i] & 
                                          data$date == years[i]-1])
}

idx <- which(!w$nominee %in% songs_w_pi)
if (length(idx) > 0){
  w <- w[-c(idx),]
}
w <- cbind(w,winners)

n <- BPDGP[BPDGP$winner == 0, ]
n$nominee <- tolower(n$nominee)
n$artist <- tolower(n$artist)
songs <- n$nominee
artist <- n$artist
years <- n$year

nominees <- c()
songs_w_pi <- c()
for (i in 1:length(songs)){
  nominees <- c(nominees, data$p_index[data$song == songs[i] & data$artist == artist[i] & 
                                         data$date == years[i]-1])
  songs_w_pi <- c(songs_w_pi, data$song[data$song == songs[i] & data$artist == artist[i] & 
                                           data$date == years[i]-1])
}

idx <- which(!n$nominee %in% songs_w_pi)
if (length(idx) > 0){
  n <- n[-c(idx),]
}
n <- cbind(n,nominees)
meann <- aggregate(nominees ~ year, n, mean)
maxn <- as.data.frame(n %>% group_by(year) %>% slice(which.max(nominees)))

plot(w$year, w$winners, ylab = 'p_index', xlab = 'year', main = 'p_index by year', xlim = c(2012,2021),
     ylim = c(0, max(c(w$winners, n$nominees))), col = '#4C0805', type = 'l')
points(w$year, w$winners, col = '#4C0805', pch = 19)
lines(maxn$year, maxn$nominees, col = '#BC5449')
points(maxn$year, maxn$nominees, col = '#BC5449', pch = 19)
text(locator(1), "winners", pos=4, col = '#4C0805')
text(locator(1), "nominees", pos=4, col = '#BC5449')


## Collecting data from this section for the tests in the folowwing section
gnmean <- c(gnmean, meann$nominees)

for (i in seq(2012,2021)){
  if (!i %in% w$year){
    grammyw <- c(grammyw, 0)
  }
  else{
    grammyw <- c(grammyw, w$winners[w$year == i])
  }
  if (!i %in% n$year){
    gnmax <- c(gnmax, 0)
  }
  else{
    gnmax <- c(gnmax, maxn$nominees[maxn$year == i])
  }
}

#################################################################################

## PERMUTATION TEST #############################################################

## H1: E(max(nominees)) < E(winners); H0: E(max(nominees)) >= E(winners)
nr = 100000 #number of rearrangements
st <- c()
n1 <- length(gnmean)
n2 <- length(grammyw)
total <- n1 + n2
sttrue = mean(gnmean) - mean(grammyw)
obs <- c(gnmean, grammyw) #save both sample in the same vector
for (i in 1:nr){
  d <- sample(obs, total)
  new_gnmean <- d[1:n1]
  new_grammyw <- d[(n1+1):total]
  st[i] <- mean(new_gnmean) - mean(new_grammyw)
}

tmp <- length(st[st < sttrue])
pvalue <- tmp/nr 
pvalue
hist(st, col = '#E2252B')
points(sttrue,0, pch = 19)

## Use gnmax instead of gnmean for the following hipothesys 
# H1: E(mean(nominees)) < E(winners); H0: E(mean(nominees)) >= E(winners)

#################################################################################

## JACKKNIFE ####################################################################

## Preparing the variables
y <- c(rep(1, length(grammyw)), rep(0, length(gnmean)))
x <- c(grammyw, gnmax)  # x_1
x <- c(grammyw, gnmean) # x_2


## Correlation
theta=cor(x,y)

n=length(y)
thetai=numeric(n); pseudov=numeric(n)
for(j in 1:n){
  thetai[j]=cor(x[-j], y[-j])
  pseudov[j]=n*theta-(n-1)*thetai[j]
}
mean(pseudov)
sd(pseudov)/sqrt(n)

## R.squared
theta=summary(lm(y~x-1))$r.squared

n=length(y)
thetai=numeric(n); pseudov=numeric(n)
for(j in 1:n){
  thetai[j]=summary(lm(y[-j]~x[-j]-1))$r.squared
  pseudov[j]=n*theta-(n-1)*thetai[j]
}
mean(pseudov)
sd(pseudov)/sqrt(n)

#################################################################################