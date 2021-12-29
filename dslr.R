library(mice)
library(VIM)
library(ggplot2)
library(hrbrthemes)
library(nnet)

readData <- function(path)
{
  tryCatch(
    df <- read.csv(path,header=TRUE),
    msgErr <- paste("Error: File","./data.csv ","doesn't exist",sep = " "),
    error = function(e) {print(msgErr)},
    warning = function(w) {print(msgErr)}
  )
  return (df)
}

df <- readData("./dataset_train.csv")
# Remove non-usable attributes
df <- subset(df,select=-c(Index,First.Name, Last.Name,Birthday,Best.Hand,Astronomy))
df$Hogwarts.House = as.factor(df$Hogwarts.House)
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(df, 2, p)
impute <- mice(df, m=3)
head(impute$imp$Arithmancy)
summary(df$Arithmancy)
apply(impute$imp$Arithmancy,2,mean)
newDATA <- complete(impute, 3)
dev.off()
hist <- ggplot(newDATA, aes(x=Care.of.Magical.Creatures, fill=Hogwarts.House)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=30) +
  theme_ipsum() +
  labs(title="Care.of.Magical.Creatures",fill="House",x="Marks",y="Number of students")
hist
scatter <- ggplot(newDATA, aes(x = Transfiguration, y = Care.of.Magical.Creatures, color = Hogwarts.House)) +
  geom_point()
scatter

newDATA$Hogwarts.House <- relevel(newDATA$Hogwarts.House, ref = "Gryffindor")
mymodel <- multinom(Hogwarts.House~. , data = newDATA)
mymodel
summary(mymodel)

# Z_test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1- pnorm(abs(z),0,1)) * 2
p

# Predicting the values for train dataset
pred <- predict(mymodel,newDATA)

head(pred)
head(obj$Hogwarts.House)

# Building classification table
tab <- table(pred,newDATA$Hogwarts.House)
tab

# Calculating accuracy
acc <- sum(diag(tab))/sum(tab)
acc


