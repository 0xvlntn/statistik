load("survey.RData")

# Berechnungen fuer a)
# Years of Experience: Mittelwert, Median, Stichprobenvarianz, Streuung, Spannweite, Interquartilsdistanz, Quantile
yoe <- c( survey$YearsCode )
mean( yoe ) # Arithmetisches Mittel
median( yoe ) # Median
var( yoe ) # Stichprobenvarianz (mittlere quadrierte Abweichung zum Mittelwert)
sd( yoe ) # Standardabweichung / Streuung
diff( range  ( yoe ) ) #Spannweite
IQR( yoe ) # Interquartilsabstand
quantile( yoe ) # Quantile
summary( yoe ) # Zusammenfassung: min, Q1, Median, Mean, Q3, Max

# Remote Work
remoteWork <- c( survey$RemoteWork )
table ( remoteWork ) # Absolute H??ufigkeit
prop.table( table( remoteWork )) * 100 # relative Haeufigkeit

# Grafiken fuer b)
boxplot( yoe, horizontal = T )
pie(table (survey$RemoteWork), main = "Remote-arbeit", col = c("blue", "green", "orange") )
barplot(table (survey$RemoteWork), col = "grey", main = "Remote-arbeit")

# Teil c
library(tidyverse)
load("../ueb1/.RData")
europeans <- survey[ survey$Currency == "EUR European Euro", ]
europeans
plot( y = europeans$YearsCode, europeans$CompTotal, ylab = "Years of Experience", xlab = "Compensation")
# Erfahrung ist keine zuverlässige Erklärung für das Gehalt
# Overplotting im unteren Bereich, hohe Streuung in den jeweils oberen Bereichen

# Spineplot (breite X und Y stellen prozentuale anteile dar) von Age und Remote work:
europeans <- survey[ survey$Currency == "EUR European Euro", ]
europeans$Age <- droplevels(europeans$Age)
barplot( y = europeans$RemoteWork, europeans$Age, ylab = "", xlab ="Age")


# Alternativ: einfacher Barplot
tab <- table(europeans$RemoteWork, europeans$Age)

# Gruppiertes Säulendiagramm
barplot(
  tab,
  beside = TRUE, # Säulen nebeneinander
  legend.text = TRUE, # Legende für RemoteWork
  args.legend = list(x = "topright", cex = 0.8),
  xlab = "Age",
  ylab = "Anzahl",
  cex.names = 0.8 # Schriftgröße x-achse
)