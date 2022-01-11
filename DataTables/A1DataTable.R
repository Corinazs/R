
# [Aufgabe]: DataTables ---------------------------------------------------

# Abschnitt 1: DataTables: Grundlagen
# Aufgabe 1: DataTables erstellen

# Importiere die library data.table. Gegeben sind 2 Vektoren mit den höchsten 
# Bergen der Welt. Erstelle darauf basierend nun eine DataTable dt1. Füge dabei auch eine Spaltenbezeichnung hinzu.
# Berechne nun den Mittelwert und die Standardabweichung von der Höhe in Meter von allen Bergen
# Verwende nun eine geeignete Funktion, um nachträglich die Spaltenbezeichnungen umzubenennen auf "Gipfel" und "Höhenmeter"

library(data.table)

v1 <- c("Mount Everest", "K2", "Kangchendzönga", "Lhotse", "Lhotse", "Cho Oyu", "Dhaulagiri", "Manaslu", "Nanga Parbat", "Annapurna I")
v2 <- c(8848, 8611, 8586, 8516, 8485, 8188, 8167, 8163, 8125, 8091)

dt1 <- data.table(Gipfel=v1, Höhenmeter =v2)
dt1[,mean(Höhenmeter)]
dt1[, sd(Höhenmeter)]

View(dt1)

# Abschnitt 2: DataTables: Fallstudie Online-Kurse ------------------------

# Aufgabe 2: DataTables: Echte Daten verarbeiten
# Datensatz: https://www.kaggle.com/jilkothari/finance-accounting-courses-udemy-13k-course
# Gegeben ist der Datensatz "courses.xls". Dabei handelt es sich um eine Zusammenstellung von 13 Tausend Kurse, 
# die auf der Udemy-Website verfügbar sind.

# Lese die Daten ein in der DataTable dt2. Analysiere zunächst die Lageverteilung und Datentypen deiner Daten

install.packages("openxlsx")
library("openxlsx")
#dt2 <- read.csv("courses.csv")
dt2 <- read.xlsx("courses.xlsx")
dt2 <- fread("courses.xlsx")

str(dt2)
summary(dt2)
head(dt2)

install.packages("dplyr")
library("dplyr")

# Ermittle nun alle Kurse, die nach dem 1. Juni 2020 publiziert wurden. Wie viele Kurse wurden veröffentlicht?



dt2 <- as.data.table(dt2)
dt2[, created := as.Date(dt2$created, "%d.%m.%Y %H:%M:S")]

head(dt2)

# Wie viele Kurse haben mehr als 30.000 Bewertungen? Welche Themen sind besonders beliebt?

#dt2[rating>30000,title]

dt2 %>%
  filter(rating>30000)  %>%
  select(title, rating)

# Zeige alle Kurse an, in denen sich Teilnehmer kostenpflichtig eingeschrieben haben. 

dt2[is_paid==TRUE,title]

# Dabei sollen alle Kurse angezeigt werden mit mehr als 100.000 Teilnehmer mit einer Bewertung von über 4,5 Sternen


dt2 %>%
  filter(is_paid=T,
         num_subscribers>100000,
         rating>4.5) %>%
  select(title, num_subscribers, rating)

# Welcher Kurs hat mehr als 50.000 Teilnehmer und eine Bewertung unter 3.9 Sterne?
dt2 %>%
  filter(is_paid=T,
         num_subscribers>50000,
         rating<3.9) %>%
  select(title, 
         num_subscribers, 
         rating)

# Wandle nun die Spalte discount_price__currency um in einen Factor

dt2[,discount_price__currency := as.factor(discount_price__currency)]
str(dt2)



# Aufgabe 3: DataTables: Fortgeschrittene Abfragen

# Verwende nun eine geeignete Funktion, um alle Kurse anzuzeigen, die "R Programming" im Titel stehen haben. 
# Zeige dabei nur die Spalten von "id" bis "avg_rating" an.
# Wie viele Kurse gibt es mit "Python" im Titel? Gibt es mehr Kurse mit "Excel" oder "SQL" im Titel?

dt2 %>%
  select(id:avg_rating) %>%
  filter(title %like% "R Programming")

# Aufgabe 4: DataTables: Aggregationsberechnungen

# Berechne für alle Kurse, in denen sich Teilnehmer kostenpflichtig eingeschrieben haben die nachfolgenden 
# Aggregationsfunktionen für die Spalte "num_subscribers":
# Arithmetisches Mittel
# Median
# Standardabweichung
# Minimum
# Maximum
str(dt2)

dt2 %>%
  dplyr:: select(title, is_paid, num_subscribers)%>%
  dplyr:: filter(is_paid == F) %>%
  dplyr:: summarize(mean(dt2$num_subscribers),
                    median(dt2$num_subscribers),
                    sd(dt2$num_subscribers),
                    min(dt2$num_subscribers),
                    max(dt2$num_subscribers))
       
         

# Aufgabe 5: DataTables: Spalten bearbeiten

# Einige Spalten in der DataTable sind nicht mehr relevant. Entferne die Spalten von discount_price__amount bis 
# price_detail__price_string in der DataTable. Wie viele Spalten verbleiben?


dt2 <- dt2  %>% 
  select(-c(discount_price__amount:price_detail__price_string))


# Eine Auswertung zeigt, dass ein Dozent durchschnittlich 90 Minuten für die Vorbereitung, Organisation, Produktion 
# und Videoearbeitung von einer einzelnen Lektion benötigt. Berechne nun eine neue Spalte in der DataTable: 
#"Expenditure of time", 
# in dem du die Spalte num_published_lectures mit 90 Minuten gewichtest und durch 60 Minuten dividierst, um die Anzahl der 
# Stunden zu ermitteln für die Kursproduktion.

dt2 <- dt2 %>%
  mutate("Expenditure of time" = num_published_lectures*90/60)

View(dt2)

# Zeige nun alle Tableau-Kurse an. Wie lange haben die Dozenten durchschnittlich benötigt zur Kursproduktion? Berechne 
# dafür das arithmetische Mittel

dt2 %>% 
  dplyr::select("Expenditure of time") %>% 
  dplyr::filter(title %like% "Tableau")

#dt[dt2$title %ike% "Tableau",]


# Aufgabe 6: DataTables: Gruppierungen

# Erstelle nun eine Auswertung in der du die DataTable gruppierst nach der Angabe, ob sich Teilnehmer kostenlos oder 
# kostenpflichtig in einen Kurs eingeschrieben haben für die nachfolgenden Berechnungen:
# Arithmetisches Mittel: num_reviews
# Median: num_subscribers
# Median: avg_rating
# Arithmetisches Mittel: num_published_lectures

dt2 %>%
  group_by(is_paid) %>% 
  summary(mean(num_reviews),
          median(dt2$num_subcribers),
          median(arg_rating),
          mean(num_published_lectures))

dt2 %>%
  group_by(is_paid==TRUE) %>% 
  summary(mean(num_reviews),
          median(dt2$num_subcribers),
          median(arg_rating),
          mean(num_published_lectures))



# Aufgabe 7: DataTables: Sortierungen

# Sortiere nun abschließend die DataTable absteigend nach den Teilnehmerbewertungen. Zeige die obersten 10 Einträge an
# Sortiere deine DataTable nun absteigend nach den Teilnehmerzahlen für alle Python-Kurse, in denen sich Teilnehmer 
# kostenpflichtig eingeschrieben haben, die mehr als 1.000 Bewertungen haben


dt3 <- head(dt2[order(dt2$rating, decreasing=TRUE),],10)
dt3 <- dt3 %>% 
  filter(dt3$is_paid==TRUE,
         dt3$rating >1000) %>% 
  arrange(dt3$num_subscribers, decreasing=TRUE) 
View(dt3)


