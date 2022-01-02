
# [Aufgabe 1]: DataFrames -------------------------------------------------

# Abschnitt 1: DataFrames Grundlagen --------------------------------------

# Aufgabe 1: DataFrames erstellen -----------------------------------------

# Erstelle ein DataFrame df1 mit den Zahlen 1 bis 5. Dabei soll die Spalte als Bewertung bezeichnet werden
# Überprüfe nun, ob es sich bei df1, um ein DataFrame handelt
# Versuche die Dimensionen zu ermitteln, also die Anzahl der Spalten und Zeilen

df1 <- data.frame(Bewertung= 1:5, Bedeutung = c("sehr unzufrieden", "mangelhaft", "in Ordnung", "gut", "sehr gut"))
#colnames(df1) <- "Bewertung"
print(df1)
class(df1)
dim(df1)

# Aufgabe 2: Eingebaute DataFrames ----------------------------------------

# Verwende nun die data()-Funktion, um die eingebauten Datensätze aufzurufen. 
# Lade nun den Datensatz PlantGrowth und gebe sowohl die ersten 6, als auch die letzten 6 Einträge in diesem DataFrame aus.
# Versuche nun die Datentypen im DataFrame zu analysieren
# Gebe nun die Zusammenfassung aus. Lade zusätzlich das psych-Modul und verwende die describe()-Funktion für eine Ausgabe

data(PlantGrowth)
print(head(PlantGrowth))
print(tail(PlantGrowth))
print(str(PlantGrowth))
summary(PlantGrowth)

library(psych)
dt <- PlantGrowth
print(describe(dt))

# Abschnitt 2: DataFrames: Arbeiten mit echten Daten ----------------------

# Aufgabe 3: DataFrames: Daten einlesen und verarbeiten -------------------

# Gegeben ist nun die csv-Datei fifa.csv mit Informationen über Fußball-Spieler. 
# Lese die Datei ein und speichere die Daten im DataFrame df2
# Ermittle nun die Anzahl der Zeilen und Spalten
# Versuche nun dein DataFrame df2 zu filtern auf jene Spieler, die bei "Sevilla FC" spielen
# Filtere nun das DataFrame df2 nach allen Spielern, die jünger als 20 sind

# Zeige abschließend alle Spieler an, die älter als 35 sind und selektiere 
# das DataFrame auf die Spalten team, club, name, age und weight. 
# Zeige dabei über die head()-Funktion nur die ersten Einträge an

df2 <- read.csv("fifa.csv", header=T, sep=",", encoding = "UTF-8")
head(df2)

dim(df2)
df2[df2["club"] == "Sevilla FC",]
df2[df2["age"] < 20,]
df2[df2["age"] > 35,c("team", "club", "age", "weight")]
print(head(df2[df2["age"] > 35,c("team", "club", "age", "weight")]))
subset(df2, subset=age>35, select = c("team", "club", "age", "weight"))



# Aufgabe 4: DataFrames: Filterungen und Factors --------------------------

# Arbeite nun weiter mit deinem DataFrame df2. Ermittle nun die Anzahl der Spieler, die für Chelsea FC spielen
# Ermittle nun die Datentypen der Spalten im DataFrame. Wandle nun die Spalte number um in einen Factor

print(nrow(df2[df2$club=="Chelsea FC",]))

print(str(df2))
df2$number <- as.factor(df2$number)

