library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(readxl)
library(scales)
library(lmtest)
library(Rdpack)
library(bdsmatrix)
library(collapse)
library(maxLik)
library(plm)

#Data merging
setwd("/RStudio/Digital Transformation/corina_zilchschuler/R_Zertifikat/data")
file_names <- dir()
df<- do.call(rbind,lapply(file_names,read.csv))

##Objectiv============================================================================================================
The aim is to establish a correlation between the introduction of the fuel discount and the price of petrol.

#Infos=========================================================================================================
##  Tank discount from 01.06 for petrol around 35 cents per litre

##Overview of data================================================================================================
str(df)
summary(df)

## df without NA data
df_withoutNA <- df %>% 
  drop_na()

df_withoutNA$location <- as.character(df_withoutNA$location)
df_withoutNA$shop <- as.character(df_withoutNA$shop)

df_withoutNA <- df_withoutNA %>% 
  mutate(location=
           case_when(
             shop == "hagen, herdecker str. 20" ~ "herdecker str. 20 - 58089 hagen",
             TRUE ~ location
           )) %>% 
  mutate(shop =
           case_when(
             location == "herdecker str. 20 - 58089 hagen" & shop == "hagen, herdecker str. 20" ~ "hem",
             TRUE ~ shop
           )
  )




#Format Column "Date" as date
df_withoutNA$date <- as.POSIXct(df_withoutNA$date, format="%Y-%m-%d %H:%M:%S")

#Amount rows df
nrow(df) #1103

#Amount nows df without NA
nrow(df_withoutNA) #863

#Amount of Shops
df_Anzahl <- df %>% 
  group_by(shop) %>% 
  count() %>% 
  arrange(n)

#Minimumprice per Shop
dfMin <- df %>% 
  dplyr::filter(!is.na(price)) %>% 
  dplyr::group_by(shop) %>%
  dplyr::summarise(Min=min(price, na.rm=TRUE))

#Maximumprice per Shop
dfMax <- df %>% 
  dplyr::filter(!is.na(price)) %>% 
  dplyr::group_by(shop) %>% 
  dplyr::summarise(Max=max(price, na.rm=TRUE))

dfMinMax <- dfMin %>% 
  dplyr::left_join(dfMax, by="shop")

##DF time splitted
df_time <- separate(df_withoutNA, col = date, into  = c('Date', 'Time'), sep = ' ')

df_time <- df_time %>% 
  arrange(Date)

df_time <- df_time %>% 
  mutate("DateTime" = paste(Date, " ", Time)) %>% 
  select(-Date,
         -Time)

df_time2 <- separate(df_withoutNA, col = date, into  = c('Date', 'Time'), sep = ' ')
df_time2$Date <- as.Date(df_time2$Date)

df_tankrabatt <- df_time2 %>% 
  mutate("Rabatt" = 
           case_when(Date >="2022-06-01" ~ "1",
                     TRUE ~ "0")
  )


df_tankrabatt$Time <- as.POSIXct(df_tankrabatt$Time, format="%H:%M:%S")

##Add month and joint with df_VPI 
df_tankrabattVPI <- df_tankrabatt %>% 
  mutate("Monat" = case_when(grepl("2022-05", Date) ~ "Mai",
                             grepl("2022-06", Date) ~ "Juni"))

df_tankrabattVPI <- df_tankrabattVPI %>% 
  left_join(df_VPI, by="Monat")

df_tankrabattVPI <- df_tankrabattVPI %>% 
  select(-`Verbraucherpreisindex.2015=100`,
         -`Veränderung.zum.Vorjahresmonat.in.%`,
         -Monat,
         -Jahr)

df_tankrabattUhrzeit <- df_tankrabatt %>% 
  mutate("Tageszeit" = 
           case_when(
             Time>= "05:00:00" & Time <= "10:00:00" ~ "Morgens",
             Time>= "10:00:00" & Time <= "12:00:00" ~ "Vormittag",
             Time>= "12:00:00" & Time <= "14:00:00" ~ "Mittag",
             Time>= "14:00:00" & Time <= "17:00:00" ~ "Nachmittag",
             Time>= "17:00:00" & Time <= "22:00:00" ~ "Abends",
             Time>= "22:00:00" | Time <= "05:00:00" ~ "Nachts",
           ))

df_tankrabattUhrzeit1 <- df_tankrabattUhrzeit %>% 
  mutate("Morgens" = 
           case_when(
             Tageszeit== "Morgens" ~ 1,
             TRUE~ 0
             
           )
  )%>% 
  mutate("Vormittags"=
           case_when(
             Tageszeit== "Vormittag" ~ 1,
             TRUE ~ 0
           ) 
  )%>% 
  mutate("Mittags"=
                     case_when(
                       Tageszeit == "Mittags" ~1,
                       TRUE ~ 0
                     )
  ) %>% 
  mutate("Nachmittags" =
           case_when(
             
             Tageszeit==  "Nachmittag" ~ 1,
             TRUE ~0
           )) %>% 
  mutate("Abends"=
           case_when(
             Tageszeit==  "Abends" ~ 1,
             TRUE ~ 0
           )) %>% 
  mutate("Nachts" = 
           case_when(
             Tageszeit =="Nachts" ~1,
             TRUE ~0
           ))



# #Count amount of shops
# df_shops <- df_withoutNA %>% 
#   group_by(shop) %>% 
#   mutate("Anzahl" = n())
# 
# ## Average price per Shop
# df_shops <- df_shops %>% 
#   group_by(shop) %>% 
#   mutate("AveragePrice" =
#            mean(price)
#   )
# 
# ##Amount and Median price per shop
# df_shops %>% 
#   select(shop, Amount, AveragePrice) %>%
#   unique() %>% 
#   arrange(AveragePrice)
# 
# ##Shops per postal code
# df_PLZ <- df_shops %>% 
#   separate(col = location, 
#            into  = c('Street','PLZ'), 
#            sep = '- ')

## Average price per shop and postal code
df_PLZ <- df_PLZ %>% 
  group_by(PLZ) %>% 
  mutate("AveragePricePLZ" =
           mean(price)
  )

df_PLZ <- df_PLZ %>% 
  mutate(PLZ =case_when(
    PLZ == "58089 hagen " ~ "58089 hagen",
    TRUE ~ PLZ
  ))

##Adjust PLZ and location name
df_PLZ <- df_PLZ %>% 
  mutate(PLZ = case_when(
    startsWith(PLZ,"58135") ~ "58135 hagen",
    startsWith(PLZ,"58093") ~ "58093 hagen",
    startsWith(PLZ,"58099") ~ "58099 hagen",
    TRUE ~ PLZ
  ))





##Amount and median price per shop and location
df_AveragePerPLZ <- df_PLZ %>% 
  select(shop, price, PLZ, Amount) %>%
  unique() 

df_AveragePerPLZ <- df_AveragePerPLZ %>% 
  group_by(PLZ) %>% 
  mutate("AveragePricePLZ" = 
           mean(price)
  )

#Amount per shop - 2 Categories
df_SizeShops2Cat <- df_shops %>% 
  group_by(shop) %>% 
  select(shop, Amount, price) %>% 
  unique() %>% 
  mutate("Category"= 
           case_when(Amount>=100 ~ "big",
                     Amount<100 ~ "small")
  )

#Amount per shop - 3 Categories
df_SizeShops3Cat <- df_shops %>% 
  group_by(shop) %>% 
  select(shop, Amount,price) %>% 
  unique() %>% 
  mutate("Category"= 
           case_when(Amount<30 ~ "small",
                     Amount>=30 & Amount<70 ~ "medium",
                     Amount>70 ~ "big"
           )
  )

##plots============================================================================================================
#pricerise over time
plotPriceRise <- ggplot(df_time, aes(y=price, x=DateTime)) +  
  geom_point(aes(color=shop))+
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Pricerise over time")

# p <- df_withoutNA %>%
#   group_by(date) %>%
#   ggplot(aes(x = date)) +
#   geom_point(aes(y = price)) +
#   geom_line(aes(y = price)) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   ggtitle("Pricerise over time") + 
#   labs(x="Date", y="Price")



#range of pricerise per shop
plotPerShop <- ggplot(df_PLZ, aes(x=shop,y = price)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Petrol price range per shop")


#Pricerise over time
plotDateTime <- ggplot(df_time,aes(y=price,x=DateTime)) + geom_line() +
  scale_x_datetime(labels = date_format("%H:%M"))

df_time$DateTime <- as.POSIXct(df_time$DateTime,tz=Sys.timezone())

plotDateTime <- ggplot(df_time,aes(y=price,x=DateTime)) + geom_line() +
  scale_x_datetime()+
  ggtitle("Petrol price trend")


##Analysis per postal code
plotPLZ <- ggplot(df_PLZ, aes(x=PLZ, y=price))+
  geom_boxplot() +
  ggtitle("Petrol price by location") +
  xlab("Location") +
  ylab("Price")

##Analysis by type and size of petrol stations
#3 Categories
plot3Cat <- ggplot(df_SizeShops3Cat, aes(x=Category, y=price)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Petrol Price") +
  ggtitle("Petrol price per size of petrol station - Grouped in 3 Categories")


#2 Categories
plot2Cat <- ggplot(df_SizeShops2Cat, aes(x=Category, y=price)) +
  geom_boxplot() +
  xlab("Category") +
  ylab("Petrol Price") +
  ggtitle("Petrol price per size of petrol station - Grouped in 2 Categories")





#Analysis=======================================================================================================

## Single Regression y= ax+b
#y=price, abhängige Variable
#x=Rabatt, unabhängige Variable



reg <- lm(formula= df_tankrabatt$price ~ df_tankrabatt$Rabatt)
summary(reg)
cormatrix <- cor(df_tankrabatt[, sapply(df_tankrabatt, is.numeric)])

##Result
#Residuals:
# Min        1Q    Median        3Q       Max 
# -0.134335 -0.034335 -0.004335  0.033302  0.215665

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            2.175698   0.002391   910.1   <2e-16 ***
#   df_tankrabatt$Rabatt1 -0.192363   0.003363   -57.2   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##The residuals are the difference between the actual values and the predicted values.
# Residual standard error: 0.0494 on 861 degrees of freedom
# Multiple R-squared:  0.7916,	Adjusted R-squared:  0.7914 
# F-statistic:  3271 on 1 and 861 DF,  p-value: < 2.2e-16

# formula: y = 2.175698 -0.192363x
#D.h. mit Tankrabatt kostet Benzin ca. 19 ct weniger.



##Multiple Regression

modelMulti <- lm(df_tankrabattVPI$price ~ df_tankrabattVPI$Rabatt + df_tankrabattVPI$`Veränderung.zum.Vormonat.in.%`)

summary(modelMulti)

##Result: Model gives no significance based on NA values -> collinearity

##Multiple Regression - Influnce of Time 

modelUhrzeit <- lm(df_tankrabattUhrzeit$price ~ df_tankrabattUhrzeit$Rabatt + df_tankrabattUhrzeit$Tageszeit)
modelUhrzeit2 <- lm(df_tankrabattUhrzeit$price ~ df_tankrabattUhrzeit$Rabatt + df_tankrabattUhrzeit1$Morgens +  df_tankrabattUhrzeit1$Nachmittags +  df_tankrabattUhrzeit1$Abends +  df_tankrabattUhrzeit1$Nachts)
summary(modelUhrzeit2)

##Result: Model gives no significance based on NA values -> collinearity


 ggplot(df_tankrabattUhrzeit, aes(y=price, x=Time))+
   geom_line()+ 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   scale_x_datetime(labels = date_format("%H:%M"))




