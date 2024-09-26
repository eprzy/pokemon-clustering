
#Ładowanie niezbędnych bibiliotek
library(dplyr)
library(ggplot2)
library(cluster)
library(stats)
library(fmsb)
library(dbscan)

###########################################################################
#Sprawdzenie, jakiego typu są atrybuty, czy są wartości brakujące#
###########################################################################

#Plik ściągnięty z https://www.kaggle.com/abcsds/pokemon
pokemon <- read.csv("Pokemon.csv")

#Poprawka nazw niektórych kolumn
colnames(pokemon)[c(1,3,4,9,10)] <- c("Id","Type1","Type2","SpAtk","SpDef")

summary(pokemon)

#Typy danych w poszczególnych kolumnach:
sapply(pokemon, class)

#Sprawdzanie czy są braki danych
sum(is.na(pokemon))
#KOMENATARZ: nie ma braków danych (wartości NA), jednak niektóre pokemony mają tylko jeden typ (pole Type2 jest puste)


#Wybór zmiennych do klastrowania - atrybuty liczbowe 
pokemon_data <- pokemon %>%
  select(HP, Attack, Defense, SpAtk, SpDef, Speed)


################################################
#Grupowanie algorytmem partycjonującym#
################################################

#Wyznaczenie liczby grup dla algorytmu k-środków metodą „łokcia” przy
#wykorzystaniu 25% losowo wybranych danych – sprawdzenie dla kilku przykładów


#Losowanie trzech próbek 25%-owych:
pokemon_data_sample1 <- sample_frac(pokemon_data, 0.25)
pokemon_data_sample2 <- sample_frac(pokemon_data, 0.25)
pokemon_data_sample3 <- sample_frac(pokemon_data, 0.25)

pokemon_data_samples <- list(pokemon_data_sample1, pokemon_data_sample2, pokemon_data_sample3)


#Wyznaczanie optymalnej liczby grup metodą "łokcia" - dla algorytmu PAM korzystamy z sumy odległosci obserwacji
#od odpowiednich środków klastrów dostępnej w skadowej "objective" obiektu pam (po drugim kroku, czyli "swap")

wss_list <- NULL
for (sample in pokemon_data_samples){
  
  #Wyznaczanie sumy błędów średniokwadratowych dla liczności grup od 1 do 15
  wss <- 0
  for (i in 1:15){
    pam.out <- pam(sample, i)
    wss[i] <- pam.out$objective[2]
  }
  wss_list <- rbind(wss_list, wss)
}

#Wykreślanie uzyskanych wyników (dla wszystkich 3 próbek)
par(mfrow = c(3, 1))
for (i in 1:nrow(wss_list)){
  plot(1:15, wss_list[i,], type = "b",  xlab = "Liczba grup", ylab = "WSS", main = paste("PRÓBKA NUMER ", i))
}


#KOMENATARZ: według eksperymentów optymalny wydaje się podział na 4 grupy
n_groups <- 4


#Wykonanie grupowania z różnymi wartościami parametrów (np. zastosowanej miary odległości w algorytmie k-środków)

#Grupowanie z wykorzystaniem metryki euklidesowej:
pam.res1 <- pam(pokemon_data, n_groups, metric = "euclidean")
print(pam.res1)
print(pam.res1$medoids)
print(pam.res1$clusinfo)
cols <- rainbow(4)
fviz_cluster(pam.res1,
             palette = cols,
             ellipse.type = "t", # elipsa koncentracji
             repel = TRUE, # Unika nakładania się napisów (spowalnia)
             ggtheme = theme_light(), #kolor tła
             ggrepel.max.overlaps = Inf
)


#Grupowanie z wykorzystaniem metryki manthattan:
pam.res2 <- pam(pokemon_data, n_groups, metric = "manhattan")
print(pam.res2)
print(pam.res2$medoids)
print(pam.res2$clusinfo)
fviz_cluster(pam.res2,
             palette = cols,
             ellipse.type = "t", # elipsa koncentracji
             repel = TRUE, # Unika nakładania się napisów (spowalnia)
             ggtheme = theme_light(), #kolor tła
             ggrepel.max.overlaps = Inf
)

#Ocena jakości grupowania przy użyciu indeksu Silhouette
pam.res1$silinfo$avg.width
pam.res2$silinfo$avg.width
#KOMENTARZ: Wyższa, czyli lepsza miara silhouette wyszła dla metryki euklidesowej (0,23 vs. 0,19), 
#zatem to ona zostanie użyta w dalszej części badania

fviz_silhouette(pam.res1, palette="jco")
#KOMENTARZ: Najlepiej jakościowo wygląda klaster pierwszy, bo tu największa część obserwacji wypada ponad średnią miarę
#silhouetee, a co więcej nie "wpadają" one w ujemne wartości, świadczące o przynależności do niewłaściwej grupy

#Przypisanie poszczególnych rekordów do grup
pokemon_pam_clus<-cbind(pokemon, pam.res1$cluster)
head(pokemon_pam_clus)

#Znalezienie charakterystycznych elementów grup

#Wydrukowanie wyników grupowania z informacją o tym które obiekty stanowią centra utworzonych grup
print(pam.res1)
medoids <- as.data.frame(pam.res1$medoids)
pokemon[396,]
pokemon[397,]
pokemon[697,]
pokemon[620,]
#KOMENTARZ:
#Środek pierwszego klastra stanowi pokemon z numerem porządkowym 396, o nazwie Snorunt (typ Ice), dla którego wszystkie cechy przyjmują wartość 50
#Środkiem drugiego klastra jest pokemon z numerem porządkowym 397, o nazwie Glalie (typ Ice), dla którego wszystkie cechy przyjmują wartość 80
#Środkiem trzeciego klastra jest pokemon z numerem porządkowym 697, o nazwie Hydreigon (typ Dark)
#Środkiem czwartego klastra jest pokemon z numerem porządkowym 620, o nazwie Glalie (typ Bug)

#PRZYJRZYMY SIĘ LEPIEJ OBRANYM ŚRODKOM GRUP NA WYKRESIE RADAROWYM, ABY POKUSIĆ SIĘ O SZERSZĄ INTERPRETACJĘ KLASTRÓW:
#Stworzenie wykresu radarowego dla klastrów:
max_scale <- setNames(rep(130, ncol(medoids)), names(medoids))
min_scale <- setNames(rep(0, ncol(medoids)), names(medoids))

radar_data <- rbind(max_scale, min_scale)

for(i in 1:4) {
  cluster_data <- setNames(medoids[i, ], names(medoids))
  radar_data <- rbind(radar_data, cluster_data)
}

radar_data <- as.data.frame(radar_data)

dev.off()
radarchart(radar_data,
           pcol=cols, plty=1, plwd=2,
           title="Wykres radarowy dla klastrów 1-4")

legend("bottom", legend = paste("Klaster", 1:4), col = cols, lty = 1, lwd = 2, 
       cex = 0.53, # Zmniejszenie rozmiaru tekstu i symboli w legendzie
       horiz = TRUE, bty = "n", bg = "transparent")

#INTERPRETACJA KLASTRÓW NA PODSTAWIE ICH ŚRODKÓW:
# Klaster 1 - grupa SŁABYCH, o względnie niskich wszystkich cechach
# Klaster 2 - grupa ASPIRUJĄYCH, czyli lepszych od tych pierwszych, ale nie wyróżniających się szczególnie jakąś cechą
# Klaster 3 - grupa SZYBKICH I WŚCIEKŁYCH, czyli tych o ponadprzeciętnej zdolności do ataku i szybkości
# Klaster 4 - grupa ODPORNYCH, o wysokiej zdolności do obrony przed atakami (Defense)
