
## importam si incarcam in memoria de lucru setul de date----
library(readxl)
example_dataset <- read_excel("example_dataset.xlsx")
roman_emperors <- as.data.frame(example_dataset)
rm(example_dataset)

roman_emperors [, 3:6] <- lapply(roman_emperors[,3:6], factor)# functia lapply face parte din familia de functii apply
                                                              # in acest caz dorim sa transformam toate coloanele care 
                                                              # contin caractere in variabile categorice
                                                              # cu exceptia coloanei cu nume
roman_emperors <- roman_emperors[, -1]


###creem o noua coloana care contine precizarea modului de ascensiune la putere ca drept ereditar, sau altul
roman_emperors$Succession_bivariate <- as.factor(                               #cerem crearea noii coloane ca factor
  ifelse(                                                                       #functie prin care putem sa creem grupuri conditionate
    roman_emperors$Succession=="Birthright",                                    # specificam setul de date si ciloana pe care le dorim grupate
    #si conditia pe care o dorim indeplinita
    "Ereditar",                                                                # specificam valoare pe care o dorim in cazul indeplinirii conditiei
    "Altul"))                                                                  # specificam valoare pe care o dorim in cazul neindeplinirii conditiei

dif_preluare_tron <- (mean(roman_emperors$Age_Throne[roman_emperors$Succession_bivariate=="Ereditar"]))- 
  mean(roman_emperors$Age_Throne[roman_emperors$Succession_bivariate=="Altul"])


## extragem un esantion aleatoriu din setul de date------

esantion_re <- roman_emperors[              #dam numele obiectului creat si specificam setul de date cu care vrem sa lucram  
  sample(                                    #in spatiul de la dreapta virgulei specificam ca vrem esantionarea pe randuri
    1:nrow(roman_emperors),                  #de la primul la ultimul rand si specificam din nou setul de date pentru functia nrow
    15), ]                                   #specificam si numarul de randuri pe care il dorim extras

#### pregatim scurtaturi pentru bootstrapping --------------------

nr.obs.ered <- length(esantion_re$Age_Throne[esantion_re$Succession_bivariate=="Ereditar"]) #stabilim numarul de randuri pentru bootstrapping, 
                                                                              #egal cu numarul de randuri din esantion, cu drept ereditar la tron 
nr.obs.alt <- length(esantion_re$Age_Throne[esantion_re$Succession_bivariate=="Altul"]) #stabilim numarul de randuri pentru bootstrapping, 
                                                                             #egal cu numarul de randuri din esantion, ne-ereditar 
nr.reesantionare <- 100000                                                    # numarul de re-esantionari

#### ne uitam la diferentele dintre cele doua seturi e date------
par(mfrow=c(2,1))    # vom produce doua boxploturi care vor fi redate intr-o singura figura cu 2 randuri si o coloana
boxplot(roman_emperors$Age_Throne~roman_emperors$Succession_bivariate)
boxplot(esantion_re$Age_Throne~esantion_re$Succession_bivariate)

summary(roman_emperors$Age_Throne)
summary(esantion_re$Age_Throne)

######## creem setul de esantioane bootstrapped -------------

#pentru succesiune ereditara
esantioane.boot.ered <- matrix(                   #creem o matrice de date
  sample(esantion_re$Age_Throne[esantion_re$Succession_bivariate=="Ereditar"], #folosind variabila varstei ascensiunii la tron pe care o esantionam aleator 
         size = nr.obs.ered*nr.reesantionare,          #care va contine 13*10000 de observatii
         replace=TRUE),                           #cu inlocuire
  nrow = nr.obs.ered,                                  #matricea va fi ordonata pe 13 de randuri
  ncol = nr.reesantionare)                        #si o suta de mii de coloane

dim(esantioane.boot.ered)       #verificam dimensiunea matricei
esantioane.boot.ered[1:2,1:2]   #verificam existenta datelor in matrice


#pentru succesiune alta decat ereditara
esantioane.boot.alt <- matrix(                   #creem o matrice de date
  sample(esantion_re$Age_Throne[esantion_re$Succession_bivariate=="Altul"], #folosind variabila varstei ascensiunii la tron pe care o esantionam aleator 
         size = nr.obs.alt*nr.reesantionare,          #care va contine 13*10000 de observatii
         replace=TRUE),                           #cu inlocuire
  nrow = nr.obs.alt,                                  #matricea va fi ordonata pe 13 de randuri
  ncol = nr.reesantionare)                        #si o suta de mii de coloane

dim(esantioane.boot.alt)       #verificam dimensiunea matricei
esantioane.boot.alt[1:2,1:2]   #verificam existenta datelor in matrice

###calculam diferentele mediilor
dif.medii.boot <- colMeans(esantioane.boot.ered)-colMeans(esantioane.boot.alt)

###calculam limitele intervalului de incredere de 95%
quantile(dif.medii.boot, prob=0.975)
quantile(dif.medii.boot, prob=0.025)

###calculam limitele intervalului de incredere de 68%
quantile(dif.medii.boot, prob=0.83)
quantile(dif.medii.boot, prob=0.17)

