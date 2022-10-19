### cream un nou vector cu provinciile de origine aleimparatilor ------

Provincie <- c("Siria", "Italia", "Italia", "Galia",  "Hispania")

Cauza_deces <- c("asasinat", "natural", "asasinat", "asasinat", "natural")

Durata_domnie <- c(4, 19, 15, 19, 20)


### atasam noile coloane la tabelul de date deja creat cu functia cbind-----

df_imp_ext <- cbind(df_imp, Provincie, Cauza_deces, Durata_domnie)


### introducem un rand nou cu functia rbind------
rand_nou <- c("Caligula", 25, "Italia", "asasinat", 4)
df_imp_ext <- rbind(df_imp_ext, rand_nou)


### extragerea anumitor observatii din setul de date-----------

pe_randuri <- df_imp_ext[1:5, ] # pastram primele cinci randuri
pe_randuri_2 <- df_imp_ext[-6, ] # eliminam randul 6

pe_coloane <- df_imp_ext[, 1:5] # pastram primele 5 coloane
pe_coloane_2 <- df_imp_ext[, -6] # eliminam coloana 6


pe_randuri_si_coloane <- df_imp_ext[-6, -3] # eliminam randul 6 si coloana 3
pe_randuri_si_coloane_2 <- df_imp_ext[c(1,2,5), c(1,3,4)] # pastram randurile 1,2,5 si coloanele 1,3,4 prin creare de vectori

### utlizarea afirmatiilor logice de tip ADEVARAT/FALS -----------

origine_italia <- df_imp_ext$Provincie == "Italia" # vrem sa stim daca toti imparatii vin din Italia
origine_italia

df_imp_ext <- cbind(df_imp_ext, origine_italia) # adaugam noul vector la tabel pentru mai tarziu

varsta <- df_imp_ext$vec_varsta > 60 # vrem sa stim daca sunt imparati care au acces la tron mai batrani de 60 de ani
varsta

italia_varsta <- df_imp_ext$Provincie == "Italia" & df_imp_ext$vec_varsta > 60 # vrem sa stim daca sunt imparati din Italia care au urcat pe tron mai batrani de 60 de ani
italia_varsta


### introducere in bucle/loops functia APPLY ----
?apply
# X - obiectul caruia dorim sa ii aplicam functia apply
#MARGIN - un vector care specifica componentele obiectului caruia dorim sa ii aplicam functia 
   #EX in cazul unui tabel MARGIN=1 - functia se aplica randurilor
   #MARGIN = 2 functia se aplica coloanelor
# FUN - functia pe care o dorim aplicata

medii <- apply(X = df_imp_ext, MARGIN = 2, FUN = mean) # eroarea ne indica faptul ca variabilele indicate 
                                                        # nu sunt formatate corespunzator
medii

df_imp_ext$vec_varsta=as.numeric(df_imp_ext$vec_varsta)# transformam cele doua variabile in valori numerice
df_imp_ext$Durata_domnie=as.numeric(df_imp_ext$Durata_domnie)


medii <- apply(X = df_imp_ext[, c(2,5)], MARGIN = 2, FUN = mean)
medii

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

