bank.df <- read.csv("bank-full.csv", header = TRUE)
summary(bank.df)

bank_upd.df <- bank.df
bank_upd.df$job <-as.factor(bank_upd.df$job)
bank_upd.df$marital <-as.factor(bank_upd.df$marital)
bank_upd.df$education <-as.factor(bank_upd.df$education)
bank_upd.df$default <-as.factor(bank_upd.df$default)
bank_upd.df$housing <-as.factor(bank_upd.df$housing)
bank_upd.df$loan <-as.factor(bank_upd.df$loan)
bank_upd.df$contact <-as.factor(bank_upd.df$contact)
bank_upd.df$day <-as.factor(bank_upd.df$day)
bank_upd.df$month <-as.factor(bank_upd.df$month)
bank_upd.df$poutcome <-as.factor(bank_upd.df$poutcome)
bank_upd.df$y <-as.factor(bank_upd.df$y)
summary(bank_upd.df)


bank_upd.df$ynum<-ifelse(bank_upd.df$y=="yes",1,0)

levels(bank_upd.df$job) # to look at all possible values of a factor var


#Heatmap
numerical.var <- c(1,6,12,13,14,15,18)
bank_num.df <- bank_upd.df[, numerical.var]
summary(bank_num.df)

library(gplots)
heatmap.2(cor(bank_num.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(bank_num.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


#Pivot Table

table(bank_upd.df$job, bank_upd.df$y) #Tells us the same thing as below pivots

categorical.var <- c(2:5,7:11,16,18)
bank_cat.df <- bank_upd.df[, categorical.var]
summary(bank_cat.df)

library(reshape) 
mlt <- melt(bank_cat.df, id=c("job", "marital", "education", "default", "housing", "loan", "contact", "day", "month", "poutcome"), measure=c("ynum"))
head(mlt)
cast(mlt, . ~ poutcome, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ loan, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ education, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ default, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ housing, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ job, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ contact, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ day, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)
cast(mlt, . ~ month, subset=variable=="ynum", margins=c("grand_row", "grand_col"), sum)



#data.for.plot <- aggregate(bank_upd.df$ynum, by = list(bank_upd.df$y), FUN = sum)
#names(data.for.plot) <- c("y", "YesCount")
#data.for.plot
#barplot(data.for.plot$YesCount * 100,  names.arg = data.for.plot$y, 
#        xlab = "CHAS", ylab = "% of CAT.MEDV")

library(ggplot2)
ggplot(bank_upd.df, aes(x=job, fill=y))+ geom_bar()

#kNN
bank_knn.df <- bank_upd.df[-c(12,18)]
library(dummies)
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="loan", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="job", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="marital", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="education", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="default", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="housing", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="contact", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="day", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="month", sep = "_")
bank_knn.df <- dummy.data.frame(bank_knn.df, names ="poutcome", sep = "_")
names(bank_knn.df)
summary(bank_knn.df)

set.seed(1)
train_knn.rows <- sample(rownames(bank_knn.df), dim(bank_knn.df)[1]*0.6)
train_knn.df <- bank_knn.df[train_knn.rows, ]
valid_knn.rows <- setdiff(rownames(bank_knn.df), train_knn.rows) 
valid_knn.df <- bank_knn.df[valid_knn.rows, ]

train.norm.df <- train_knn.df
valid.norm.df <- valid_knn.df
bank.norm.df <- bank_knn.df

library(caret)
norm.values <- preProcess(train_knn.df[, c(1,23,74:76)], method=c("center", "scale"))
head(norm.values)
train.norm.df[, c(1,23,74:76)] <- predict(norm.values, train_knn.df[, c(1,23,74:76)])
valid.norm.df[, c(1,23,74:76)] <- predict(norm.values, valid_knn.df[, c(1,23,74:76)])
bank.norm.df[, c(1,23,74:76)] <- predict(norm.values, bank_knn.df[, c(1,23,74:76)])

head(bank.norm.df)

library(FNN)
bankknn <- knn(train = train.norm.df[, 1:80], test = valid.norm.df[, 1:80], cl = train.norm.df[, 81], k = 3) 
confusionMatrix(bankknn, valid_knn.df[, 81])

bankaccuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
bankaccuracy.df

for(i in 1:14) {
  bankknn.pred <- knn(train = train.norm.df[, 1:80], test = valid.norm.df[, 1:80], cl = train.norm.df[, 81], k = i) 
  bankaccuracy.df[i, 2] <- confusionMatrix(bankknn.pred, valid_knn.df[, 81])$overall[1] 
}

bankaccuracy.df

bankknn11 <- knn(train = train.norm.df[, 1:80], test = valid.norm.df[, 1:80], cl = train.norm.df[, 81], k = 11) 
confusionMatrix(bankknn11, valid_knn.df[, 81])


#CT
bank_CT.df <- bank_upd.df[-c(12,18)]

library(rpart)
library(rpart.plot)

set.seed(1)
train_CT.rows <- sample(rownames(bank_CT.df), dim(bank_CT.df)[1]*0.6)
train_CT.df <- bank_CT.df[train_CT.rows, ] 
valid_CT.rows <- setdiff(rownames(bank_CT.df), train_CT.rows) 
valid_CT.df <- bank_CT.df[valid_CT.rows, ]

options(scipen=999)
Bankdefault.tree <- rpart(y ~ ., data = train_CT.df, method = "class")
prp(Bankdefault.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 

bankcv.ct <- rpart(y ~ ., data = train_CT.df, method = "class", 
                   cp = 0.00001, minsplit = 10,xval=5)
printcp(bankcv.ct)

bankpruned.ct <- prune(bankcv.ct, 
                       cp = 0.0053) #Might need to change it back as per min xerror
length(bankpruned.ct$frame$var[bankpruned.ct$frame$var == "<leaf>"])
prp(bankpruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(bankpruned.ct$frame$var == "<leaf>", 'gray', 'white'))  
summary(train_CT.df)

bankPR.pred.valid <- predict(bankpruned.ct,valid_CT.df,type = "class")
confusionMatrix(bankPR.pred.valid, as.factor(valid_CT.df$y))

library(randomForest)
bankrf <- randomForest(y ~ ., data = train_CT.df, ntree = 500, 
                       mtry = 4, nodesize = 5, importance = TRUE)  

varImpPlot(bankrf, type = 1)

bankRF.pred.valid <- predict(bankrf,valid_CT.df,type = "class")
confusionMatrix(bankRF.pred.valid, as.factor(valid_CT.df$y))


#Logit
bank_lg.df <- bank_upd.df[-c(12,18)]

set.seed(1)
train_lg.rows <- sample(rownames(bank_lg.df), dim(bank_lg.df)[1]*0.6)
train_lg.df <- bank_lg.df[train_lg.rows, ] 
valid_lg.rows <- setdiff(rownames(bank_lg.df), train_lg.rows) 
valid_lg.df <- bank_lg.df[valid_lg.rows, ]

banklogit.reg <- glm(y ~ ., data = train_lg.df, family = "binomial") 
options(scipen=999)
summary(banklogit.reg)

library(forecast)
library(caret)
banklogit.reg.pred <- predict(banklogit.reg, valid_lg.df[, -16], type = "response") 
confusionMatrix(as.factor(ifelse(banklogit.reg.pred > 0.5, "yes", "no")), valid_lg.df$y)

library(gains)
bankgain <- gains(ifelse(valid_lg.df$y=="yes",1,0), banklogit.reg.pred, groups=10)

plot(c(0,bankgain$cume.pct.of.total*sum(ifelse(valid_lg.df$y=="yes",1,0)))~c(0,bankgain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(ifelse(valid_lg.df$y=="yes",1,0)))~c(0, dim(valid_lg.df)[1]), lty=2)


heights <- bankgain$mean.resp/mean(ifelse(valid_lg.df$y=="yes",1,0))
heights
decileplot <- barplot(heights, names.arg = bankgain$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response/Overall Mean", main = "Decile-wise lift chart")
text(decileplot, heights+0.5, labels=round(heights, 1), cex = 0.8)
