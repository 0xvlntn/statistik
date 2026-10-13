# ------------------------------------------------------------------------------
# Lösungsvorschlag zu Aufgabenblatt 1 
# Version 1
# Datum: 09.10.2026
# Author: Nicholas Schreck
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aufgabe 1.3
v1 <- 1 : 3
v2 = c(1, 0, -1)
# (a)
v1 + v2
# (b)
v1 * v2
# (c)
v1 > v2
# (d)
v1 ^ v2
# (e)
sum( v1 ^ 2 )
# (f)
rep("A", 5)
rep( c(1, 2), 4 )
c( rep(1, 3), rep(2, 3) )
2^{1:16}
# (g)
list( Numbers = 2^{1:16}, letter = letters[1:4])
# Kein Datensatz, da Elemente unterschiedliche Länge
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aufgabe 1.4
# (a)
install.packages("ISLR2", repos='http://cran.us.r-project.org')
library(ISLR2)
# (b)
# hier muss der eigene Pfad angegeben werden. 
load("../../Daten/survey.RData")
#load("../../Daten/survey.RData")
# (c)
dim(survey) # Erste Dimension: Beobachtungen, zweite Dimension Variablen. 
# (d)
# sihe unten. 
# (e)



indicator <- survey$MainBranch == "I am a developer by profession" & 
  survey$Age == "18-24 years old" &
  survey$RemoteWork == "Remote" &
  survey$Currency == "EUR European Euro" &
  survey$CompTotal > 100000

dim( survey[ indicator, ] )
survey$LanguageHaveWorkedWith[ indicator ] 
# Elegantere Lösung über tidyverse

library(tidyverse)
survey2 <- survey %>% filter(
  MainBranch == "I am a developer by profession", Age == "18-24 years old", 
  RemoteWork == "Remote", Currency == "EUR European Euro", CompTotal > 100000
)
survey2
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aufgabe 1.5
quick_sort <- function(x) {
  # Test auf numerischen Vektor
  if (!is.numeric(x)){
    return("Input ist nicht numerisch")
  }
  # Abbruch, falls Vektor nur noch ein Element. 
  if (length(x) <= 1) {
    return(x)
  }
  # Pivot wählen, hier gibt es verschiedene Optionen (erstes Element, letztes Element, 
  # medianes Element, zufälliges Element usw). 
  pivot <- x[ceiling(length(x) / 2)]
  # Vektor aufteilen
  kleiner <- x[x < pivot]
  gleich  <- x[x == pivot]
  groesser <- x[x > pivot]
  # Rekursiv sortieren
  c(
    quick_sort(kleiner),
    gleich,
    quick_sort(groesser)
  )
}
set.seed(1)
( x <- rnorm(10) )
quick_sort(x)
quick_sort(c(1, 2, "A"))
# ------------------------------------------------------------------------------

