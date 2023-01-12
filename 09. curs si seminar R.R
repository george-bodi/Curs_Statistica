#CURS---------

# importul si incarcarea datelor in memoria de lucru---------
library(readxl)
example_dataset <- read_excel("example_dataset.xlsx")
df_re <- as.data.frame(example_dataset)
rm(example_dataset)

######definim variabilele categorice ca factori---------
df_re[,c(2:6)] <- lapply(df_re[,c(2:6)], factor)

######eliminam coloana Index-----------
df_re <- df_re[, -1]

###creem o noua coloana care contine precizarea modului de ascensiune la putere ca drept ereditar, sau altul
df_re$Succession_bivariate <- as.factor(                               #cerem crearea noii coloane ca factor
  ifelse(                                                                       #functie prin care putem sa creem grupuri conditionate
    df_re$Succession=="Birthright",                                    # specificam setul de date si coloana pe care le dorim grupate
    #si conditia pe care o dorim indeplinita
    "Ereditar",                                                                # specificam valoarea pe care o dorim in cazul indeplinirii conditiei
    "Altul"))                                                                  # specificam valoarea pe care o dorim in cazul neindeplinirii conditiei

plot(df_re$Age_Throne)
abline(h=mean(df_re$Age_Throne), col="blue")
for(i in 1:61) lines(c(i,i), c(mean(df_re$Age_Throne), df_re$Age_Throne[i]), col="green")


plot(df_re$Age_Throne, pch=21, bg=as.numeric(df_re$Succession_bivariate))
abline(h=mean(df_re$Age_Throne[df_re$Succession_bivariate=="Ereditar"]), col="red")
abline(h=mean(df_re$Age_Throne[df_re$Succession_bivariate=="Altul"]))
index <- 1:length(df_re$Age_Throne)
for (i in 1:length(index)) {
  if (df_re$Succession_bivariate[i]=="Ereditar")
    lines(c(index[i], index[i]), c(mean(df_re$Age_Throne[df_re$Succession_bivariate=="Ereditar"]), df_re$Age_Throne[i]), col="red")
  else
    lines(c(index[i],index[i]), c(mean(df_re$Age_Throne[df_re$Succession_bivariate=="Altul"]), df_re$Age_Throne[i]))
}



SSY <- sum((df_re$Age_Throne-mean(df_re$Age_Throne))^2) # calculam suma deviatiilor/rezidualelor la patrat pentru variabila analizata, 
SSY                                                     # care constituie variatia totala observabila, compusa din variatia explicabila data
                                                        # de diferentele reale dintre grupuri (SSA), si variatia ne-explicata, datorata erorilor
                                                        # introduse de esantionaj (SSE)

sqr_Ereditar <- sum((df_re$Age_Throne[df_re$Succession_bivariate=="Ereditar"]-              # calculam suma rezidualelor la patrat 
                       mean(df_re$Age_Throne[df_re$Succession_bivariate=="Ereditar"]))^2)   # pentru grupul "Ereditar" al variabilei analizate


sqr_Altul <- sum((df_re$Age_Throne[df_re$Succession_bivariate=="Altul"]-                    # calculam suma rezidualelor la patrat 
                    mean(df_re$Age_Throne[df_re$Succession_bivariate=="Altul"]))^2)         # pentru grupul "Altul" al variabilei analizate


SSE <- sqr_Ereditar+sqr_Altul           #suma rezidualelor la patrat calculata pentru pentru fiecare grup constituie variatia ne-explicata
SSA=SSY-SSE                             #variatia reala dintre grupuri este data de diferenta dintre variatia totala si variatia ne-explicata     
                                     
f_stat=SSA/(SSE/59)                     #valoarea f ne raportează la o curbă de distribuție numită Distribuția F. Dacă valoarea f obținută de noi 
f_stat                                  #este mai mare decât valoarea critică f, putem respinge ipoteza nulă
qf(0.95, 1, 59)                         #calculăm valoarea critică f pentru un interval de încredere de 95%, pentru 1 grad de libertate pentru
                                        #variația totală, și 59 de grade de libertate pentru variația pe grupuri
1-pf(42.23, 1, 59)                      #calculăm probabilitățile cumulative pentru ipoteza nulă


#SEMINAR----------
#ANALIZA VARIATIEI cu functia aov---------

AOV_re <- aov(df_re$Age_Throne~df_re$Succession)           # analizam variatia varstei de accedere la tron in functie de modalitatea de succesiune
summary(AOV_re)                                            # cerem afisarea sumarului analizei
plot(AOV_re)                                               # verificam validitatea rezultatelor analizei, dar valoarea p nu ne ajuta in interpretare

#summary.lm(AOV_re)                                         #cerem afisarea sumarului analizei cu indicarea puterii de influenta a variabilei 
                                                           #independente asupra variabilei raspuns
TukeyHSD(AOV_re)                                           #functia TukeyHSD efectueaza o comparatie multipla a mediilor contrastand fiecare
                                                           #pereche de grupuri

library(ggplot2)
plot_aov_re <- ggplot(df_re, aes(x=df_re$Succession, y=df_re$Age_Throne, fill=df_re$Succession))+
  geom_violin()+                                    # graficul de tip "violin" ne reda distributia valorilor                                      
  geom_boxplot(width=0.2, color="black", alpha=0.3) # graficul de tip boxplot ne indica valorile statistice relevante si posibilele valori aberante
plot_aov_re

# Analiza multifactoriala - 2 sau mai multi factori cu 2 sau mai multe nivele---------

df_re$Provincie_bivariate <- as.factor(                               #cerem crearea noii coloane ca factor
  ifelse(                                                             #functie prin care putem sa creem grupuri conditionate
    df_re$`Birth Province`=="Italia",                                 # specificam setul de date si coloana pe care le dorim grupate
                                                                      #si conditia pe care o dorim indeplinita
    "Italia",                                                         # specificam valoare pe care o dorim in cazul indeplinirii conditiei
    "Alta"))                                                         # specificam valoare pe care o dorim in cazul neindeplinirii conditiei

aov_re_multi_2 <-aov(df_re$Age_Throne~                                        # analizam variatia varstei de accedere la tron
                       df_re$Succession_bivariate*df_re$Provincie_bivariate)  # in functie de modalitatea de succesiune si provincia de origine
                                                                              # cu presupunerea ca exista influente reciproce intre cele doua variabile
summary(aov_re_multi_2)
#summary.lm(aov_re_multi_2)

aov_re_multi_3 <-aov(df_re$Age_Throne~                                        # analizam variatia varstei de accedere la tron
                       df_re$Succession_bivariate+df_re$Provincie_bivariate)  # in functie de modalitatea de succesiune si provincia de origine
                                                                              # cu presupunerea ca nu exista influente reciproce intre cele doua variabile
summary(aov_re_multi_3)
#summary.lm(aov_re_multi_3)

TukeyHSD(aov_re_multi_3)



