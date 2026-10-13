load("survey.RData")

# Berechnungen f??r a)
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
prop.table( table( remoteWork )) * 100 # relative H??ufigkeit

# Grafiken f??r b)
boxplot( yoe, horizontal = T )
pie(table (survey$RemoteWork), main = "Remote-arbeit", col = c("blue", "green", "orange") )
barplot(table (survey$RemoteWork), col = "grey", main = "Remote-arbeit")
