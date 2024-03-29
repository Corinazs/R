library(tidyr)
library(dplyr)
library(openxlsx)
library(caret)
library(ggplot2)

df <- read.csv("Coding_Training/data/StudentsPerformance.csv")

plotRS <- ggplot(df, aes(x=reading.score, y=math.score))+ geom_point(color="black") +geom_smooth() +
  xlab("Reading score") +
  ylab("Math score") +
  ggtitle("")
plotWS <- ggplot(df, aes(x=writing.score, y=math.score))+ geom_point(color="green") + geom_smooth() +
  xlab("Writing score") +
  ylab("Math score") 

##Korrelationsmatrix
cormatrix <- cor(df[, sapply(df, is.numeric)])


modelLM <- lm(math.score ~ writing.score + reading.score, data=df)
summary(modelLM)

plotModel <- ggplot(model, aes(y=math.score, x=writing.score, shape=reading.score)) + geom_point()



##Build model train() macht hier dasselbe wie lm()
model <- train(math.score ~ writing.score + reading.score,
               data = df,
               method="lm",
               trControl = trainControl(method="none"))
print(model$finalModel)


##Generate data
random <- df %>% 
  sample_n(5)

randomWS <- random %>% 
  select(writing.score)

randomRS <- random %>% 
  select(reading.score)


predict(object = modelLM,
        newdata= c(randomRS, randomWS))
        
##result
# 1        2        3        4        5 
#55.86779 49.31150 51.65876 74.52559 74.58349 

##own function
predictfct <- function(x,y) {
  return (7.5241 + 0.2494 * x + 0.6013 * y)
}
predictfct(randomRS, randomWS)

##result
#reading.score
#1       54.4595
#2       48.2552
#3       52.7137
#4       76.6358
#5       73.1749


plotPrediction <- ggplot(data = df, mapping = aes(y=math.score, x=writing.score)) + geom_point()+
  geom_smooth(method="lm", color="red", se=F)



