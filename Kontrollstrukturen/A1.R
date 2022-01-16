
# [Aufgabe]: If-Abfragen --------------------------------------------------


# Aufgabe 1: Einfache If-Abfragen

# Gegeben sei die Variable x. Erstelle eine If-Abfrage, um zu überprüfen, ob x über bzw. gleich 5 liegt. 
# Wenn x über 5 liegt, möchtest du die Zeichenkette "Kriterium erfüllt" ausgeben. Wenn x kleiner als 5 ist, 
#möchtest du die Zeichenkette "Kriterium nicht erfüllt" ausgeben


x <- 7
if(x>=5){
  print("Kriterium erfüllt")
}else(x<5){
  print("Kriterium nicht erfüllt")
}

#oder
ifelse(x>=5, "Kritertium erfüllt", "Kriterium nicht erfüllt")


# Aufgabe 2: Readline-Funktion

# Du arbeitest bei einem E-Commerce-Unternehmen.

# Beim Anmeldeprozess haben die Kunden die Möglichkeit anzugeben, 
# ob sie sich bei einem Newsletter anmelden möchten.

# Verwende die readline-Funktion, um abzufragen, ob sich ein Kunde beim Newsletter anmeldet. 
# Wenn sich der Kunde anmeldet, dann möchtest du eine Zeichenkette ausgeben, dass er nun E-Mails vom Unternehmen erhält. 
# Wenn er sich nicht anmeldet, wird er keine E-Mails erhalten. Verwende dazu eine geeignete If-Abfrage

newsletter <- readline("Möchten Sie sich für den Newsletter anmelden? (Ja/Nein):")
if(newsletter=="Ja" | newsletter == "ja"){
  print("Sie erhalten nun E-Mails vom Unternehmen.")
}else{
  print("Sie erhalten keine E-Mails vom Unternehmen.")
}

# Aufgabe 3: Logische Operatoren

# Gegeben sei die Note 1.0 von einem Studierenden in einer Klausur. 
# Um die Prüfung zu bestehen, muss der Studierende mindestens die Note 4.0 geschrieben haben.

# Zusätzlich gilt, falls er eine 1.0 schreibt, hat er somit die Bestnote geschrieben. 
# Verwende eine If-Abfrage, um abzufragen, ob beide Kriterien erfüllt sind und er eine Bestnote geschrieben hat. 
# Falls die Kriterien nicht erfüllt sind, verwendest du bei deiner Ausgabe die Zeichenkette "nicht erfüllt"

note <- 1.0

if(note <= 4.0){
  ergebnis <- "bestanden"
  
  if(note==1.0){
   print("Glückwunsch! Mit Bestnote bestanden.")
  }
  print(paste0(ergebnis, ": ", note))

}else{
  ergebnis <-  "nicht bestanden"
  print("Kriterien nicht erfüllt.")
  }


# Aufgabe 4: If-else

# Gegeben sei die Variable z. Überprüfe über ifelse, ob die Zahl 3 vorliegt. 
# Wenn die 3 vorliegt, möchtest du die Zeichenkette "Die 3 liegt vor!" ausgeben. 
# Ansonsten möchtest du die Zeichenkette "Die 3 liegt nicht vor" ausgeben

z <- 1:3
ifelse(z==3,"Die 3 liegt vor!", "Die 3 liegt nicht vor!")


# Aufgabe 5: Tabelle: Logische Operatoren

# Gegeben sei die Tabelle tab, welche die Ausprägungen von Logicals repräsentiert. 
# Füge der Tabelle über Spalten alle möglichen Kombinationen von x und y hinzu bzgl. logischen Operatoren:

# Oder
# Und
# Nicht X
# Nicht y
# Nicht x und y
# Nicht x oder y

tab <- data.frame(
  
  x = c(T, T, F, F),
  y = c(F, T, F, T)
  
)
tab$Oder <- x | y
tab$Und <-  x&y
tab$N.X <- !x 
tab$N.Y <- !y 
tab$N.XundY <- !(x&y)
tab$N.XoderY <- !(x|y)
View(tab)
