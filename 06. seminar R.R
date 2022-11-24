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

set.seed(12345)                              #stabilim o valoare pentru generatorul de combinati aleatorii
esantion_re <- roman_emperors[              #dam numele obiectului creat si specificam setul de date cu care vrem sa lucram  
  sample(                                    #in spatiul de la dreapta virgulei specificam ca vrem esantionarea pe randuri
    1:nrow(roman_emperors),                  #de la primul la ultimul rand si specificam din nou setul de date pentru functia nrow
    30), ]                                   #specificam si numarul de randuri pe care il dorim extras

#### ne uitam la diferentele dintre cele doua seturi e date------
par(mfrow=c(2,1))    # vom produce doua boxploturi care vor fi redate intr-o singura figura cu 2 randuri si o coloana
boxplot(roman_emperors$Age_Throne~roman_emperors$Succession_bivariate)
boxplot(esantion_re$Age_Throne~esantion_re$Succession_bivariate)

summary(roman_emperors$Age_Throne)
summary(esantion_re$Age_Throne)


###### - t-test pentru o singura variabila numerica----------

### vom analiza variabila varstei ascensiunii la tron


?t.test # afisam pagina de descriere a functiei t.test

####t.test unilateral-------
### testam daca media esantionului este mai mica decat sau egala cu 40
### ipoteza nula, H0 media>=40
###ipoteza alternativa H1<40
### vrem sa producem si un interval de incredere de 95% pentru medie

test_unilateral <- t.test(esantion_re$Age_Throne, #specificam setul de date si variabila pe care o dorim analizata
       mu=40.6,                     #specificam ipoteza nula
       alternative = "less",      #specificam ipoteza alternativa
       conf.level = 0.95)         #specificam valoarea intervalului de incredere pe care il dorim calculat pentru medie

test_unilateral


#### test bilateral----------
### testam daca media esantionului este egala cu 44
### ipoteza nula, H0 media=44
###ipoteza alternativa H1><44
### vrem sa producem si un interval de incredere de 95% pentru medie

test_bilateral <- t.test(roman_emperors$Age_Throne,       #specificam setul de date si variabila pe care o dorim analizata
                          mu=40.6,                          #specificam ipoteza nula, media este 44
                          alternative = "two.sided",      #specificam ipoteza alternativa, media este diferita de 44
                          conf.level = 0.95)              #specificam valoarea intervalului de incredere pe care il dorim calculat pentru medie
test_bilateral

###### - t-test pentru o variabila numerica grupata in functie de o variabila categorica----------
### testam daca mediile grupelor in functie de modul de preluare a puterii sunt egale
### ipoteza nula, mediile sunt egale
### ipoteza alternativa mediile nu sunt egale
### vrem sa producem si un interval de incredere de 95% pentru medie


###creem o noua coloana care contine precizarea modului de ascensiune la putere ca drept ereditar, sau altul
roman_emperors$Succession_bivariate <- as.factor(                           #cerem crearea noii coloane ca factor
  ifelse(                                                                   #functie prin care putem sa creem grupuri conditionate
    roman_emperors$Succession=="Birthright",                                # specificam setul de date si ciloana pe care le dorim grupate
                                                                            #si conditia pe care o dorim indeplinita
    "Ereditar",                                                             # specificam valoare pe care o dorim in cazul indeplinirii conditiei
    "Altul"))                                                               # specificam valoare pe care o dorim in cazul neindeplinirii conditiei

boxplot(roman_emperors$Age_Throne~roman_emperors$Succession_bivariate)


##### NOTA BENE ---------------
# O conditíe care trebuie verificata este daca variatiile de la medie ale celor doua grupuri sunt egale
  # ipoteza nula a testului este ca variatiile sunt egale
variatie_ereditar=var(                                       #functie de calculare a variatiei fata de medie              
  roman_emperors$Age_Throne[                                   #specificam variabila pe care o dorim analizata
    roman_emperors$Succession_bivariate=="Ereditar"])        # specificam variabila si conditia in functie de care dorim sa se faca calculul

variatie_altul=var(                                       #functie de calculare a variatiei fata de medie              
  roman_emperors$Age_Throne[                             #specificam variabila pe care o dorim analizata
    roman_emperors$Succession_bivariate=="Altul"])       #specificam variabila si conditia in functie de care dorim sa se faca calculul

variatie_ereditar
variatie_altul

t_test_pe_grupuri <- t.test(roman_emperors$Age_Throne~roman_emperors$Succession_bivariate, #specificam setul de date cu variabila numerica 
                                                                                            #pe care o dorim analizata si cu variabila categorica 
                                                                                            # in functie de care o dorim grupata
                    mu=0,                                                                  # ipoteza nula este ca nu exista diferenta intre medii
                    alternative="two.sided",
                    conf.level=0.95)
t_test_pe_grupuri

###### - TEST NONPARAMETRIC pentru o variabila numerica grupata in functie de o variabila categorica / Mann-Whitney U Test / Wilcoxon Rank-Sum Test----------
### testam daca mediile grupelor in functie de modul de preluare a puterii sunt egale
### ipoteza nula, mediile sunt egale
### ipoteza alternativa mediile nu sunt egale
### vrem sa producem si un interval de incredere de 95% pentru medie
test_nonparametric=wilcox.test(roman_emperors$Age_Throne~roman_emperors$Succession_bivariate, mu=0, alternative="two.sided", conf.int=TRUE, 
                               conf.interval=0.95)
test_nonparametric


