
### introducere in bucle/loops functia TAPPLY ----
?tapply
#functia tapply ne ajuta sa aplicam functii unor variabile, grupandu-le in functie de valorile altor variabile
#X = variabila sau vectorul caruia dorim sa ii aplicam functia
#INDEX = variabila sau vectorul in functie de care facem gruparea, si care trebuie sa aiba acelasi numar de 
#elemente ca X
#FUN= functia pe careo dorim aplicata

tapply(X = df_imp_ext$vec_varsta, INDEX = df_imp_ext$origine_italia, FUN = mean) # calculam varsta medie a preluarii tronului in 
tron_                                                                            #functie de provincie 
tapply(df_imp_ext$vec_varsta, df_imp_ext$origine_italia, mean) # se pot omite specificatiile argumentelor functiei, 
#dar trebuie respectata intocmai ordinea lor

tapply(df_imp_ext$Durata_domnie, list(df_imp_ext$origine_italia, df_imp_ext$Cauza_deces), mean) # calculam media duratei 
#de domnie grupata in functie de origine si cauza mortii prin utilizare functiei list


### introducere in statistica descriptiva - SUMARIZAREA VARIABILELOR NUMERICE ----

summary(df_imp_ext) # functia summary ne ofera statisticile descriptive de baza pentru tot setul de date

tapply(df_imp_ext$vec_varsta, df_imp_ext$origine_italia, summary) # functia summary ne ofera informatiile de baza asupra 
#variabilei analizate

install.packages("psych") # pentru a obtine mai multe informatii instalam pachetul psych
library(psych)

?describeBy

describeBy(df_imp_ext[, c(2,5)], group = df_imp_ext$origine_italia) # calculam valorile statistice descriptive in 
#functie de un singur grup


describeBy(df_imp_ext[, c(2,5)], group = list(df_imp_ext$origine_italia, df_imp_ext$Cauza_deces)) # calculam valorile 
# statistice descriptive in functie de doua grupuri



##########
library(lubridate)
library(pastecs)
roman_emperors <- as.data.frame(roman_emperors)
roman_emperors$Age_Throne<-as.period(interval(roman_emperors$Birth, roman_emperors$`Reign Start`)/years(1))
roman_emperors$Rule<-as.period(interval(roman_emperors$`Reign Start`, roman_emperors$`Reign End`)/years(1))
roman_emperors$Life<-as.period(interval(roman_emperors$Birth, roman_emperors$Death)/years(1))



### importul setului de date -----------

library(readxl)
setwd("C:/Users/georg/OneDrive/Curs Statistica/Proiect_curs") # setwd - comanda care stabileste dosarul de lucru

df_imp_rom <- read_excel("roman_emperors_bun_4.xlsx") # importam fisierul cu setul de date

df_imp_rom <- as.data.frame(df_imp_rom) # transformam fisierul excel itr-un format compatibil cu R


df_imp_rom [, 3:6] <- lapply(df_imp_rom[,3:6], factor)# functia lapply face parte din familia de functii apply
                                                      # in acest caz dorim sa transformam toate coloanele care 
                                                      # contin caractere in variabile categorice
                                                      # cu exceptia coloanei cu nume


summary(df_imp_rom) # ne facem o idee despre structura setului de date - avem trei variabile numerice si patru categorice
                    # pentru variabilele categorice avem deja produs tabelul de frecventa dar e incomplet

### vizualizarea datelor categorice --------
##### grafic cu bare ---------- vizualizare frecventei fiecarei categorii din interiorul fiecarei categorii / a frecventei 
                                # fiecarui nivel din interiorul factorului

?table  # crearea unui tabel de frecventa este necesara pentru crearea unui grafic cu bare

numar_provincii <- table(df_imp_rom$`Birth Province`)
numar_provincii
numar_provincii/61 # daca impartim numarul de observatii din fiecare categorie la numarul total de observatii
                  # obtinem tabelul frecventelor relative

?barplot
barplot(numar_provincii) # vedem ca sunt prea multe provincii si nu ne apar toate numele
barplot(numar_provincii/61,                                 #le exprimam sub forma de frecventa relativa
        main = "Provincia de origine",                      # main defineste numele tabelului
        xlab = "Provincii",                                 # xlab defineste numele axei x
        ylab = "%",                                         # xlab defineste numele axei x
        names.arg = c("Afr", "Dac", "Dal", "GalL", "GalN", "His", "HisB", "Ita", "Lib", "Mau", "Moe", "MoeS", 
                      "Pan", "PanI", "Fri", "Sir", "Tra", "NA")) # names.arg ne permite sa creem un vectoe cu denumiri
                                                                  # alternative

barplot(numar_provincii/61, main = "Provincia de origine", ylab = "Provincii", xlab = "%",
        col = rainbow(20),            # cerem atribuirea automata a unei culori fiecarei categorii
        horiz = T, #rasturnam tabelul pe o parte ATENTIE, trebuie schimbate etichetele axelor
        las=1)                    


##### diagrama circulara ----------

?pie
pie(numar_provincii)
pie(numar_provincii, main = "Provincii de origine")

### vizualizarea datelor numerice --------
##### histograma ---------- vizualizarea distributiei variabilelor numerice

?hist

hist(df_imp_rom$Age_Throne) # observam ca axa Y raporteaza frecventa si ca 
                            # intervalul de raportare pe axa x este din 10 in 10

hist(df_imp_rom$Age_Throne, freq = F) # cu argumentul frequency dezactivat, axa Y raporteaza frecventa relativa

hist(df_imp_rom$Age_Throne, freq = F, breaks = 14) # argumentul breaks defineste numarul de coloane prin setarea
                                                  # numarului de limite dintre aceastea. In acest caz, prin dublarea
                                                  # numarului de limite am injumatatit latimea coloanelor, fiecare 
                                                  # reprezentand un interval de 5 ani 
hist(df_imp_rom$Age_Throne, freq = F, 
     breaks = seq(from=0, to=80, by=1)) # functia seq genereaza o secventa in acest caz de la 1 la 80 din 1 in 1

hist(df_imp_rom$Age_Throne, freq = F, 
     breaks = seq(from=0, to=80, by=1),
     main = "Varsta preluarii tronului", # specificam titlul graficului
     xlab = "Varsta",                    # specificam titlul axei x
     las=1)                              # rotim legenda axei y cu 90 de grade

hist(df_imp_rom$Age_Throne, freq = F, breaks = seq(from=0, to=80, by=1), main = "Varsta preluarii tronului", 
     xlab = "Varsta", las=1,
     ylim = c(0, 0.08))       # modificam limitele axei y astfel incat sa cuprinda toate valorile exprimate
                              # specificand ca limita inferioara este 0, iar cea superioara 0.08


##### graficul densitatii ---------- vizualizarea distributiei variabilelor numerice
?density # functia density estimeaza densitatea distributiei valorilor unui vector 
          # cu valori numerice

?plot       # comanda generica pentru realizarea unui grafic cu axe x si y (ex. nu realizeaza un boxplot)

plot(density(df_imp_rom$Age_Throne, bw = 5)) # argumentul bandwidth indeplineste o functie similara argumentului sequence din 
                                              # histograma si stabileste nivelul de detaliu al curbelor

##### histograma si grafic al densitatii----------

hist(df_imp_rom$Age_Throne, freq = F, breaks = seq(from=0, to=80, by=1), main = "Varsta preluarii tronului", 
     xlab = "Varsta", las=1,
     ylim = c(0, 0.07)) 

lines(                                     # comanda lines adauga o linie ultimului grafic realizat
  density(df_imp_rom$Age_Throne, bw=4),   # comanda density indica faptul ca linia trebuie sa reprezinte densitatea variabilei
  col = "red")                            # argumentul col ne permite sa schimbam culoarea liniei


