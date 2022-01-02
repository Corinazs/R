
# [Aufgabe 2]: DataFrames -------------------------------------------------

# Abschnitt 1: DataFrames Filterungen -------------------------------------


# Aufgabe 1: Subsets und Filterungen
# 
# Lese zunächst die csv-Datei "life_expectancy.csv" ein und speichere sie im DataFrame df1. 

df1 <- read.csv("life_expectancy.csv")

# Gebe nun die ersten Einträge aus. Welche Datentypen liegen vor? Wie sind die Daten verteilt?

head(df1)
str(df1)
quantile(df1$Life.expectancy)
quantile(df1$Year)

#   Verwende die subset-Funktion, um die Lebenserwartungen nach 2000 für alle Länder anzuzeigen. 
df2 <- subset(df1, Year>2000)

# Speichere das Ergebnis als df2
# Verwende df2, um zusätzlich das DataFrame einzuschränken auf das Jahr 2016. 
#Selektiere dabei nur die Spalten Entity und Life.expectancy und speichere das Ergebnis im DataFrame df.2016

subset(df2, df2$Year==2006)
df.2016 <- df3[-2]
head(df.2016)

# Erstelle nun ein DataFrame df3, welches nur die Filterungsergebnisse enthält für 
# die Lebenserwartung über 83 Jahren nach dem Jahr 2012

df3 <- subset(df2, df2$Life.expectancy>83 & df2$Year>2012)
head(df3)

# Ermittle nun die Zusammenfassung (summary) für die Einträge im DataFrame im Jahr 2010.

summary(subset(df1,df1$Year==2010))

# Welche Länder haben im Jahr 2010 die geringste Lebenserwartung?

df1$Entity[min(df1$Life.expectancy)]
df1$Life.expectancy[min(df1$Life.expectancy)]


# Abschnitt 2: DataFrames Sortierungen
# Aufgabe 2: Sortierungen von DataFrames

# Erstelle nun das DataFrame df.ordered, in dem du die order()-Funktion für Life.expectancy anwendest. Sortiere damit 
# dein DataFrame absteigend und aufsteigend nach der Lebenserwartung

df.ordered.asc <- df1[order(df1$Life.expectancy),]
df.ordered.desc <- df1[order(df1$Life.expectancy, decreasing=T),]
head(df.ordered.asc)
head(df.ordered.desc)

#print(df1$Life.expectancy[min(df1$Entity=="India")])

# Bonus: Lade die dplyr-Bibliothek. Dazu kann es erforderlich sein, dass du die Library zunächst installieren musst. 
install.packages("dplyr")
library(dplyr)

# Importiere anschließend über den Library-Befehl die Bibliothek. Verwende dann eine geeignete Funktion, um alle Länder anzuzeigen, 
# die sich in der Spalte Entity befinden. Wie viele distinkte Länder liegen vor?

s <- df1 %>% 
  dplyr::distinct(Entity, .keep_all=T)

#dplyr:: distinct(df1,Entity, .keep_all=T)


# Aufgabe 3: Fortgeschrittene Filterungen

# Filtere nun die Lebenserwartung für das Land Deutschland nach dem Jahr 2005 und älter. 
# Speichere das Ergebnis im neuen DataFrame df.germany und sortiere die Lebenserwartung absteigend

df.germany <- subset(df1, Entity=="Germany" & Year >=2005)

df.germany <- df.germany[order(df.germany$Life.expectancy, decreasing = T),]
head(df.germany)

# Abschnitt 3: DataFrames: Fortgeschrittene Funktionen
# Aufgabe 4: DataFrames erstellen

# Erstelle nun ein DataFrame, welches in der ersten Spalte die Zahlen von 1 bis 20 enthält und 
# in der zweiten Spalte die Buchstaben a bis t enthält
# Verwende eine geeignete Funktion, um die Spalten im DataFrame umzubenennen in "Zahlen" und "Buchstaben"

df4 <- data.frame(Zahlen= 1:20, Buchstaben= letters[1:20])
View(df4)
