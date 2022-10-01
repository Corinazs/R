library(tidyr)
library(dplyr)
library(openxlsx)
library(caret)
library(ggplot2)

df <- read.csv("Coding_Training/data/bikeshare-datei-fuer-projektaufgabe-in-r.csv")

plot <- ggplot(df, aes(temp, count)) + 
  geom_point(alpha=0.2, aes(color=temp))

df$datetime <- as.POSIXct(df$datetime, format="%Y-%m-%d %H:%M:%S")
plot2 <- ggplot(df, aes(x=datetime, y=count), shape=temp) +
  geom_point(alpha=0.1, aes(color=temp)) +
  theme_bw()

#Korrelationsmatrix
cor(df[,c("temp", "count")])


plot3 <- ggplot(df, aes(x=season, y=count, group=season)) + 
                  geom_boxplot(aes(color=season), alpha=0.2)

df2 <- separate(df,datetime, into= c("date", "time"), sep = ' ')
df2 <- df2 %>% 
  mutate("hour"=time) %>% 
  select(-time) %>% 
  relocate(hour, .after=date)

  
#Erstelle jetzt ein Scatterplot, das count vs. hour anzeigt und einen Farbverlauf aufweist, der auf temp basiert. 
#Verwende dazu nur die Daten, für die gilt: workingday == 1.              
      
df3 <- df2 %>% 
  filter(workingday==1)

plot4 <- ggplot(df3, aes( hour,count), shape=temp) + geom_point(aes(color=temp), alpha=0.5,position=position_jitter(w=1, h=0))+
  scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))


df4 <- df2 %>% 
  filter(workingday==0)

plot5 <- ggplot(df4, aes( hour,count), shape=temp) + geom_point(aes(color=temp), alpha=0.5,position=position_jitter(w=1, h=0))+
  scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))


##lineare regression
temp.model <- lm(count ~ temp, data=df)
summary(temp.model)

predict_fct <- function(x){
  return (6.0462 + 9.1705*x)
}

##Wie viele Fahrrad Mieten würden wir vorhersagen, wenn die Temperatur 25 Grad Celsius ist?
predict_fct(25)

df2$hour <- sapply(df2$hour, as.numeric)


#Jetzt erstelle ein neues Modell, das versucht count anhand der folgenden Feautres vorherzusagen:

# season
# holiday
# workingday
# weather
# temp
# humidity
# windspeed
# hour (factor)


#temp needs to be dropped
multiModel <- lm(count ~ season+holiday+workingday+weather+humidity+windspeed+hour, data=df2)
summary(multiModel)


###Result=================================================================================================
# Call:
#   lm(formula = count ~ season + holiday + workingday + weather + 
#        humidity + windspeed + as.factor(hour), data = df2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -396.79  -65.57   -8.51   52.96  512.35 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              88.95012    8.14542  10.920  < 2e-16 ***
#   season                   29.40388    1.05770  27.800  < 2e-16 ***
#   holiday                  -8.24419    7.08465  -1.164 0.244583    
# workingday                4.62485    2.53321   1.826 0.067925 .  
# weather                 -26.48211    2.05179 -12.907  < 2e-16 ***
#   humidity                 -0.91750    0.07996 -11.475  < 2e-16 ***
#   windspeed                -1.02516    0.15105  -6.787 1.21e-11 ***
#   as.factor(hour)01:00:00 -19.14033    7.90461  -2.421 0.015477 *  
#   as.factor(hour)02:00:00 -30.82730    7.93259  -3.886 0.000102 ***
#   as.factor(hour)03:00:00 -41.34201    8.00532  -5.164 2.46e-07 ***
#   as.factor(hour)04:00:00 -43.89014    7.96794  -5.508 3.70e-08 ***
#   as.factor(hour)05:00:00 -30.67608    7.92211  -3.872 0.000108 ***
#   as.factor(hour)06:00:00  27.73115    7.91054   3.506 0.000457 ***
#   as.factor(hour)07:00:00 164.29571    7.90533  20.783  < 2e-16 ***
#   as.factor(hour)08:00:00 312.01741    7.90265  39.483  < 2e-16 ***
#   as.factor(hour)09:00:00 168.67608    7.91029  21.324  < 2e-16 ***
#   as.factor(hour)10:00:00 117.97696    7.92984  14.878  < 2e-16 ***
#   as.factor(hour)11:00:00 149.30276    7.96043  18.756  < 2e-16 ***
#   as.factor(hour)12:00:00 192.68830    7.99422  24.103  < 2e-16 ***
#   as.factor(hour)13:00:00 191.49576    8.02876  23.851  < 2e-16 ***
#   as.factor(hour)14:00:00 176.13509    8.05493  21.867  < 2e-16 ***
#   as.factor(hour)15:00:00 186.78262    8.05971  23.175  < 2e-16 ***
#   as.factor(hour)16:00:00 248.62761    8.05008  30.885  < 2e-16 ***
#   as.factor(hour)17:00:00 403.28636    8.02778  50.236  < 2e-16 ***
#   as.factor(hour)18:00:00 367.14437    7.99915  45.898  < 2e-16 ***
#   as.factor(hour)19:00:00 252.50757    7.95040  31.760  < 2e-16 ***
#   as.factor(hour)20:00:00 167.13356    7.92457  21.091  < 2e-16 ***
#   as.factor(hour)21:00:00 114.06211    7.90720  14.425  < 2e-16 ***
#   as.factor(hour)22:00:00  75.99448    7.90010   9.619  < 2e-16 ***
#   as.factor(hour)23:00:00  34.09457    7.89638   4.318 1.59e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 119.1 on 10856 degrees of freedom
# Multiple R-squared:  0.5685,	Adjusted R-squared:  0.5674 
# F-statistic: 493.2 on 29 and 10856 DF,  p-value: < 2.2e-16




                
