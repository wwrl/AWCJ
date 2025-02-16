######################### Korelacje do polityków #########################################
library(readxl)
library(corrplot)
library(ppcor)
library(RColorBrewer)
library(ggplot2)
library(vegan)
library(jaccard) 
library(proxy)
library(tidyverse)
library(factoextra)
library(cluster)
library(dendextend)
library(psych)
library("DescTools")
library(ca)


dane1 <- read_excel("")
dane1 <- as.data.frame(dane1)

cor(dane1)                    
kor=round(cor(dane1), 4)
View(kor)

pcor(dane1)  
pkor=round(pcor(dane1)$estimate, 4)
View(pkor)

Spearman = cor(dane1, method = c('spearman')) #method = c("pearson", "kendall", "spearman") # można robić na kendallu powinnismy na spearmana ale mozna 

kor <- round(Spearman, 2)
kor 
#####Heatmap
windows()
kor <- round(Spearman, 2)
kor 
corrplot(kor, method = "circle")
corrplot(pkor, method = "circle")
par(mfrow = c(1,2))
corrplot(kor, method = "circle" )
corrplot(pkor, method = "circle")

par(mfrow = c(1,1))
################################### Jaccard ##############################################

dane2 <- read_excel("")
dane2 <- as.data.frame(dane2)
str(dane2)
View(dane2)
jednostki = dane2[,1]
jednostki

dane3 <- dane2[,-1]
dane3
row.names(dane3) <- jednostki
dane3
View(dane3)


df <- data.frame(dane3)
df
vegdist(df, method = "jaccard") #odl Jaccarda
Jaccard_podob <- 1-vegdist(df,method = 'jaccard')   #2,3 msc po przeinku
Jaccard_podob #wspołoczynnik podobienstwa jaccarda


Jaccard_odl <- vegdist(x=df,method = "jaccard")
Jaccard_odl
Jaccard_odl <- round((Jaccard_odl),digits = 3)

Jaccard_podob <- 1- Jaccard_odl      
View(Jaccard_podob) <- round((1-Jaccard_odl),digits = 3)

#############Jaccard dla nowych zmiennych#############
dane4 <- read_excel("C:/Users/wikto/Desktop/UG/Sem 2/Analiza jakosciowa/Projekt/jaccard nowe zmienne.xlsx")
dane4 <- as.data.frame(dane4)
str(dane4)
View(dane4)
jednostki = dane4[,1]
jednostki

dane5 <- dane4[,-1]
dane5
row.names(dane5) <- jednostki
dane5
View(dane5)

df <- data.frame(dane5)
df
vegdist(df, method = "jaccard") #odl Jaccarda
Jaccard_podob1 <- 1-vegdist(df,method = 'jaccard')   #2,3 msc po przeinku
Jaccard_podob1 #wspołoczynnik podobienstwa jaccarda


Jaccard_odl1 <- vegdist(x=df,method = "jaccard")
Jaccard_odl1 <- as.matrix(Jaccard_odl1)[1:9,1:9]
Jaccard_odl1
Jaccard_odl1 <- round((Jaccard_odl1),digits = 3)
View(Jaccard_odl1)

Jaccard_podob1 <- 1- Jaccard_odl1      #współczynnik podobieństwa Jaccarda
Jaccard_podob1 <- round((1-Jaccard_odl1),digits = 3)
View(Jaccard_podob1)

Jaccard_odl1<-get_dist(df,method="binary", stand=FALSE)
Jaccard_podob1=1-Jaccard_odl1
View(Jaccard_podob1)
fviz_dist(Jaccard_odl1)
fviz_dist(Jaccard_podob1, order=TRUE)
fviz_dist(Jaccard_podob1, order=FALSE, gradient=list(low="red",mid='white', high='midnightblue'))
fviz_dist(Jaccard_podob1, order=TRUE, gradient=list(low="red",mid='white', high='midnightblue'))

odl <- dist(dane4, method = "Jaccard")
dend1 <- hclust(odl, method = "complete")
View(dend1)
plot(dend1)
dend1$height
plot(dend1)
plot(dend1, hang =-1)
grupy <- cutree(dend1, k=3)
grupy
rect.hculst(dend1, k=3, border = 'turquise')

########################## KOR DLA PYTAŃ ######################################################
dane6 <- read_excel("")
dane6 <- as.data.frame(dane5)
install.packages("corrplot")

cor(dane6)                    
kor=round(cor(dane6), 4)
View(kor)

pcor(dane6)  
pkor=round(pcor(dane5)$estimate, 4)
View(pkor)

kendall = cor(dane6, method = c('kendall'))
Spearman = cor(dane6, method = c('spearman')) #method = c("pearson", "kendall", "spearman") # można robić na kendallu powinnismy na spearmana ale mozna 

######################### HEATMAP ##############
windows()
kor <- round(Spearman, 2)
kor 
corrplot(kor, method = "circle")
corrplot(pkor, method = "circle")
par(mfrow = c(1,2))
corrplot(kor, method = "circle" )
corrplot(pkor, method = "circle")

par(mfrow = c(1,1))
################################################
windows()
corrplot(kor, method = "number")
corrplot(pkor, method = "number")
par(mfrow = c(1,2))
corrplot(kor, method = "number" )
corrplot(pkor, method = "number")

####################### KOR DLA PYTAŃ #############
dane7 <- read_excel("")
dane7 <- as.data.frame(dane7)

cor(dane7)                 
kor=round(cor(dane7), 4)
View(kor)

pcor(dane7) 
pkor=round(pcor(dane7)$estimate, 4)
View(pkor)

Spearman = cor(dane7, method = c('spearman')) #method = c("pearson", "kendall", "spearman") # można robić na kendallu powinnismy na spearmana ale mozna 

kor <- round(Spearman, 2)
kor 

############### WYKRES #############
corrplot(kor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos ='d') # tu np widac ze 3 cechy sa inne budujac cos innego niz wszytskie

corrplot(kor, method = 'ellipse', order = 'AOE', type = 'upper')

pairs(dane7)

corrplot(kor, method = 'square')

####################### MSA #############################
View(cortest.bartlett(dane7)) 

View(KMO(dane7)) 
danebg <- dane7 
cortest.bartlett(danebg)
KMO(danebg) 

danebg <- dane7[,-3]
cortest.bartlett(danebg)
KMO(danebg) 

danebg_15 <- dane7[,-c(3,15)]
cortest.bartlett(danebg_15)
KMO(danebg_15)

danebg_1 <- dane7[,-c(1,3,15)]
cortest.bartlett(danebg_1)
KMO(danebg_1)

danebg_17 <- dane7[,-c(1,3,15,17)]
cortest.bartlett(danebg_17)
KMO(danebg_17)

danebg_10 <- dane7[,-c(1,3,10, 15,17)]
cortest.bartlett(danebg_10)
KMO(danebg_10)

danebg_14 <- dane7[,-c(1,3,10,14, 15,17)]
cortest.bartlett(danebg_14)
KMO(danebg_14)

danebg_2 <- dane7[,-c(1,2, 3,10,14, 15,17)]
cortest.bartlett(danebg_2)
KMO(danebg_2)

danebg_16 <- dane7[,-c(1, 3,10,14, 15, 17)]
cortest.bartlett(danebg_16)
KMO(danebg_16)

cor(danebg)                    
kor=round(cor(danebg), 4)
View(kor)

Spearman = cor(danebg, method = c('spearman')) #method = c("pearson", "kendall", "spearman") # można robić na kendallu powinnismy na spearmana ale mozna 

kor <- round(Spearman, 2)
kor 

corrplot(kor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos ='d') 

corrplot(kor, method = 'ellipse', order = 'AOE', type = 'upper')

pairs(danebg)

corrplot(kor, method = 'square')

df <- danebg

################################################################################
vss(df)

Spearman = cor(df, method = 'spearman')
kor <- round(Spearman, 2)
kor

corrplot(kor, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'darkblue', rect.lwd = 3, tl.pos ='d') # mamy teraz macierz 15x10 nie mamy x2 bo wyrzucilismy ją 

############################### Metody dla czynników #################################################
vss(df) 

fa.parallel(df)

# PROPORCJE ŁADUNKÓW KTORE ODRZUCAJA KOLEJNO CZYNNIKI
# Na podstawie danych wejściowych

fa.parallel(df, fa = 'fa', fm='pa', main = 'Scree Plot') # metoda principal axis
abline(h = 1, col = 'green', lwd = 2, lty = 2) # macierz danych, pearson

fa.parallel(df, fa = 'fa', fm='ml', main = 'Scree Plot') # metoda najwiekszej wiarygodnosci
abline(h = 1, col = 'green', lwd = 2, lty = 2) #macierz danych, perason

fa.parallel(kor, fa = 'fa', fm='pa', main = 'Scree Plot')
abline(h = 1, col = 'green', lwd = 2, lty = 2) # spearman sprwdzic czy sa róznice lepiej na kor niz na df!!! sensowniej 

fa.parallel(kor, fa = 'fa', fm='minres', main = 'Scree Plot') # metoda minres
abline(h = 1, col = 'green', lwd = 2, lty = 2)

######################## PCA ########################
pc0<- principal(r = df, 2, rotate = 'none', cor = TRUE) 
pc0 

pc1<- principal(r = df, 2, rotate = 'none', cor = TRUE) 
pc1 

pc2 <- principal(r = df, 2 , rotate = "varimax", cor = TRUE) 
pc2 #
fa.diagram(pc2)

pc3 <- principal(kor, 2, rotate = "none")
pc3
fa.diagram(pc3) 

pc4 <- principal(kor, 2, rotate = "varimax")
pc4
fa.diagram(pc4)

pc5 <- principal(df, cor = TRUE, nfactors = 2, rotate = "oblimin")
pc5
fa.diagram(pc5)

pca <- prcomp(df, scale = TRUE)
fviz_pca_biplot(pca)

ml0 <- fa(kor, nfactors = 2, rotate="none", fm="ml", # 0 OZNACZA ZE NIE MAM ROTACJI
          residuals = TRUE)
ml0 # zblizone do pca bez rotacji ML! ML@ - ładunki bez rotacji 0.92 ... x8 z głowy jest git x7 ma wysoki indeks hoffmana i wysokie cos tam jeszcze niewyraznie osadza sie w jedny czynniku x11 równomiernie sie osadza w 2 czynnikach com - 2.0 znaczy ze buduje 2 czynniki w tym samym stopniu 

ml1 <- fa(kor, nfactors = 2, rotate="varimax", fm="ml", # rotacja varimax czy efekt nie zanczyna sie zmieniać efekt najczesciej ze 3 cehcy buduja 2 czynnik
          residuals = TRUE)
ml1
fa.diagram(ml1)

print(fa(kor, 2, fm="minres", rotate = "varimax")$loadings, cut=0.5)

############################# Metoda MINRES ####################################
mm0 <- fa(kor, nfactor = 2, fm = "minres", rotate = "none")
mm0
fa.diagram(mm0)

mm1 <- fa(kor, nfactor = 2, fm = "minres", rotate = "varimax")
mm1

mm2 <- fa(kor, nfactor = 2, fm = "minres", rotate = "quantimax")

########################### METODA PRINCIPAL AXIS ####################################################
pa0 <- fa(kor, nfactors = 2, fm = "pa", rotate = "none")
pa0

pa1 <- fa(kor, nfactors = 2, fm = "pa", rotate = "varimax")
pa1

fa.diagram(pa1)

pa0 <- fa(kor, nfactors = 2 , fm="pa" , rotate = "none")
pa1 <- fa(kor, nfactors = 2 , fm="pa" , rotate = "varimax")
pa2 <- fa(kor, nfactors = 2 , fm="pa" , rotate = "quartimax")
pa3 <- fa(kor, nfactors = 2 , fm="pa" , rotate = "equamax")
pa4 <- fa(kor, nfactors = 2 , fm="pa" , rotate = "varimin")

pa0
pa1
pa2
pa3
pa4
#################################################################################
dane7 <- read_excel("")
dane7 <- as.data.frame(dane7)

################################################################################
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library(factoextra)
install.packages("ggrepel")
library(ggrepel)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)

dane_pca_2 <- prcomp(dane7, scale = TRUE)

# wartości własne
eig.val <- get_eigenvalue(dane_pca_2)
View(eig.val    )                                   

# ładunki składowe
print(dane_pca_2)                               
View(dane_pca_2$x)

dane_pca_2$x                         

pc.dane7 <- princomp(dane7, cor = TRUE)     
str(pc.dane7)
View(pc.dane7)
round(cor(dane7, pc.dane7$scores), 3)     

pc.dane7$loadings              # ładunki składowe
pc.dane7$sdev                  # odchylenie standardowe składowych głównych
pc.dane7$center                # średnie zmiennych
pc.dane7$scale                 # odchylenie standardowe zmiennych
pc.dane7$scores                # współrzędne przypadków
View(pc.dane7$scores)
windows()
# wykres osypiska - liniowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca_2, type = "lines", main = "Wykres osypiska")
abline(1,0, col = 'red', lty = 2)   
windows()
# wykres osypiska - słupkowy + odcięcie na poziomie wartości własnej równej 1
screeplot(dane_pca_2, type = "barplot", col = "lightgreen", main = "")                 
abline(1,0, col = 'red', lty = 2)

fviz_eig(dane_pca_2, addlabels = TRUE, 
         ylim = c(0, 60),
         main="Scree Plot")

####################################################################################3
housetasks <- read_excel("")

housetasks
dim(housetasks)
tablica_danych = as.matrix(housetasks)
tablica_danych

suma = addmargins(tablica_danych)
suma
rozkład_brzegowy = prop.table(tablica_danych)
rozkład_brzegowy #macierz korespondencji
sumawzg = addmargins(rozkład_brzegowy)
View(sumawzg )

profil_wierszowy_wzq = prop.table(tablica_danych, margin = 1)
profil_wierszowy_wzq
profil_wierszowy_wzq_suma = addmargins(profil_wierszowy_wzq,margin = 2, FUN = sum)
profil_wierszowy_wzq_suma
profil_kolumnowy_wzg = prop.table(tablica_danych, margin = 2)
profil_kolumnowy_wzg
profil_kolumnowy_wzg_suma = addmargins(profil_kolumnowy_wzg, margin = 1, FUN = sum)
View(profil_kolumnowy_wzg_suma)

chi = chisq.test(housetasks)
chi
View(chi$expected)
View(chi)

TschuprowT(housetasks)
ContCoef(housetasks)
CramerV(housetasks)

C = ContCoef(housetasks)

w = 9
k = 15

Cmax = (sqrt((k-1)/k)+sqrt((w-1)/w))/2
Cmax
Ckor = C/Cmax
Ckor


install.packages("ca")
library("ca")
library("ggplot2")

danek = ca(housetasks)
danek
danek$sv #wartosci osobliwe - pierwiastek z wartosci wlasnych 
plot(danek)

ev = get_eigenvalue(danek)
View(ev)
inercja = sum(ev[,1]) #wariancja calego zbioru
inercja

fviz_screeplot(danek, addlabels = TRUE, ylim = c(0,50))

row = get_ca_row(danek)
row
View(row)
View(danek)
danek$rowmass # masy wierszowe, czestosci brzegowe wierszy, sredni profil kolumnowy 
row$coord     # wspolrzedne wariantow cechy 1 (wiersze)
row$cos2      # cos2 dla wszystkich wymiarów 
rowSums(row$cos2[,1:2])   # jakośc dla dwoch wymarow 
row$contrib   # bezwładnośc po wymiarach, suma = 1 
row$inertia   # inercja dla wariantów cechy 1 (wiersze), wariancja 
sum(row$inertia)
(row$inertia/sum(row$inertia))*100 # względna bezwładnośc, inercja, suma = 100%

col = get_ca_col(danek)
col
View(col)
View(danek$rowmass) # masy kolumnowe, czestosci brzegowe kolumn, sredni profil kolumnowy 
col$coord           # wspolrzedne wariantow cechy 1 (kolumny)
View(row$cos2 )     # cos2 dla wszystkich wymiarów 
colSums(col$cos2[,1:2])   # jakośc dla dwoch wymarow 
col$contrib         # bezwładnośc po wymiarach, suma = 1 
col$inertia         # inercja dla wariantów cechy 1 (kolumny), wariancja 
sum(col$inertia)
(col$inertia/sum(col$inertia))*100 # względna bezwładnośc, inercja, suma = 100%

fviz_ca_biplot(danek, repel = TRUE)

# Wczytanie danych
dane <- read_excel("") 
dane <- as.data.frame(dane)
#rownames(dane) <- c("Bosak", "Czarzasty", "Duda", "Hołownia", "Kaczyński", "Kosiniak-Kamysz", "Trzaskowski", "Tusk", "Polityk idealny")
dane <- dane[,-1]

tablica_danych <- as.matrix(dane)

# Przeprowadzenie analizy korespondencji
danek <- ca(tablica_danych, graph = FALSE)

str(danek)

# Uzyskanie współrzędnych z analizy korespondencji
coords_wiersze <- data.frame(danek$rowcoord)[1:3]
coords_kolumny <- data.frame(danek$colcoord)[1:3]

# Dodanie etykiet
coords_wiersze$Polityk <- rownames(coords_wiersze)
coords_kolumny$Cecha <- rownames(coords_kolumny)

# Upewnienie się, że kolumny mają odpowiednie nazwy
colnames(coords_wiersze) <- c("Dim1", "Dim2", "Dim3", "Polityk")
colnames(coords_kolumny) <- c("Dim1", "Dim2", "Dim3", "Cecha")

# Trójwymiarowy wykres dla wierszy (polityków)
fig_wiersze <- plot_ly(coords_wiersze, x = ~Dim1, y = ~Dim2, z = ~Dim3, 
                       type = 'scatter3d', mode = 'markers+text', 
                       text = ~Polityk, marker = list(size = 5, color = 'blue'))

# Trójwymiarowy wykres dla kolumn (cech)
fig_kolumny <- plot_ly(coords_kolumny, x = ~Dim1, y = ~Dim2, z = ~Dim3, 
                       type = 'scatter3d', mode = 'markers+text', 
                       text = ~Cecha, marker = list(size = 5, color = 'red'))

# Połączenie wykresów w jeden
fig <- subplot(fig_wiersze, fig_kolumny)

# Dodanie etykiet osi i tytułu
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Dim 1'),
                                   yaxis = list(title = 'Dim 2'),
                                   zaxis = list(title = 'Dim 3')),
                      title = "Trójwymiarowy wykres analizy korespondencji")

# Wyświetlenie wykresu
fig


#ANALIZA KORESPONDENCJI
danek <- ca(housetasks, graph = FALSE)

danek
danek$sv #wartosci osobliwe
plot(danek)

fviz_screeplot(danek, addlabels = TRUE, ylim = c(0,50))

################################################################################
row <- get_ca_row(danek)

danek$rowmass                       # masy wierszowe, czestosci brzegowe wierszy, sredni profil kolumnowy
row$coord                           # wspolrzedne wariantow cechy 1 (wiersze)
row$cos2                            # cos2 dla wszystkich wymiarow 
rowSums(row$cos2[,1:2])             # jakosc dla dwoch wymiarow
row$contrib                         # bezwladnosc po wymiarach, suma =1
row$inertia                         # inercjqa dal wariantow cechy 1 (wiersze), wariancja
sum(row$inertia)
(row$inertia/sum(row$inertia))*100  # wzgledna bezwladnosc, inercjam suma = 100%

# jakosc punktu - im blizej jednsci sa wartosci, tym lepieje reprezentowany jest
# analizowany punkt. W przykladzie problem jest z official. 

fviz_ca_biplot(danek, repel = TRUE)

#wykres mozajkowy
#wykres balonowy

