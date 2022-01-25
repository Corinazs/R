#Dplyr Übung
#Wir werden den mtcars Data Frame für diese Übung nutzen!

install.packages("dplyr")
library(dplyr)

head(mtcars)

#Gebe die Zeilen der Autos mit einem mpg-Wert über 20 und genau 6 Zylindern aus
mtcars %>% 
  filter(mpg>20 & cyl == 6)

#Ordne den Data Frame neu. Dabei soll zuerst nach cyl sortiert werden und dann absteigend nach wt.
mtcars %>% 
  arrange(cyl) %>% 
  arrange (wt, descending = T)

#Wähle die Spalten mpg und hp.
mtcars %>% 
  select(c(mpg,hp))

#Wähle die einzigartigen Werte der gear Spalte.
mtcars %>% 
  distinct(gear)

#Erstelle eine neue Spalte "Performance", welche durch hp geteilt durch wt berechnet wird
mtcars %>% 
  mutate(Performance = hp/wt)

#Finde den durchschnittlichen mpg Wert durch Verwendung von dplyr
mtcars %>% 
  summarise(mean(mpg))

#Nutze den Pipe Operator, um den durchschnittlichen hp Wert für Autos mit 6 Zylindern zu erhalten.
mtcars %>% 
  filter(cyl==6) %>% 
  summarise(mean(hp))
  
