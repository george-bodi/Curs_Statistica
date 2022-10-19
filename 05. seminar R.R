install.packages(c("SmartEDA", "DataExplorer", "ggplot2", "rmarkdown", "dlookr"))    # instalam pachete speciale pentru analiza exploratorie
library(SmartEDA)               # activam pachetele
library(DataExplorer)
library(rmarkdown)
library(readxl)
library(ggplot2)
library(psych)
library(dlookr)

### importul setului de date -----------

setwd("C:/Users/georg/OneDrive/Curs Statistica/Proiect_curs") # setwd - comanda care stabileste dosarul de lucru

df_imp_rom <- read_excel("roman_emperors_bun_4.xlsx") # importam fisierul cu setul de date

df_imp_rom <- as.data.frame(df_imp_rom) # transformam fisierul excel intr-un format compatibil cu R


df_imp_rom [, 3:6] <- lapply(df_imp_rom[,3:6], factor) # functia lapply face parte din familia de functii apply
                                                       # in acest caz dorim sa transformam toate coloanele care 
                                                       # contin caractere in variabile categorice
                                                       # cu exceptia coloanei cu nume


### explorarea rapida a setului de date -----


ExpReport(df_imp_rom,                  # creem raportul cu SmartEDA - specificam setul de date pe care il dorim analizat
          op_file = "Raport_SmartEDA") # specificam numele pe care dorim sa il aiba fisierul rezultat (va fi doar in format html)

create_report(data = df_imp_rom,                         # creem raportul cu DataExplorer - specificam setul de date pe care il dorim analizat
              output_format = html_document(),           # specificam formatul documentului rezultat 
              output_file = "Raport_DataExplorer.html")  # specificam numele documentului rezultat



### explorarea setului de date----------

##### barplot variabile categorice ------------
gr_prov <- ggplot(data = df_imp_rom)+               #specificam setul de date cu care lucram
  geom_bar(mapping = aes(x=`Birth Province`,        #specificam argumentele estetice ale graficului, variabila pe care o dorim afisata
                         fill=`Birth Province`))+   # si faptul ca dorim barele sa fie colorate in functie de grup
  scale_color_viridis_c()+                          # specificam scara de culori pe care o dorim utilizata
  ggtitle("Provincii origine")                      #adaugam titlu
  gr_prov


  gr_prov <- ggplot(data = df_imp_rom)+               # specificam setul de date cu care lucram
    geom_bar(mapping = aes(x=`Birth Province`,        # specificam argumentele estetice ale graficului, variabila pe care o dorim afisata
                           y=stat(count)/sum(count),  # calculam tabelul de frecvente
                           fill=`Birth Province`))+   # si faptul ca dorim barele sa fie colorate in functie de grup
    scale_color_viridis_c()+                          # specificam scara de culori pe care o dorim utilizata
    ggtitle("Provincii origine")+                     # adaugam titlu
    theme(axis.text.x = element_text(angle = 45))+    # cerem rotirea textului axei x cu 45 de grade
         ylab("Densitate")      
gr_prov

##### histograma variabile numerice ------

gr_varsta <- ggplot(data = df_imp_rom, aes(x=`Age_Throne`,      # specificam setul de date cu care lucram si variabila
                                           fill=..x..))+        # argumentul fill indica ca vrem colorare a barelor, 
                                                                # val x indica un argument specificat ulterior
  geom_histogram(binwidth = 1)+                                 # specificam ca dorim ca barele sa reprezinte intervale de 1 an
  scale_fill_gradient("Legenda", low="blue", high = "red")+     # coloram barele pe un gradient crescator de la mic la mare            
  ggtitle("Varsta preluarii tronului")                          # adaugam titlu
  gr_varsta                                                     # cerem afisarea graficului
  
  
  gr_varsta <- ggplot(data = df_imp_rom, aes(x=`Age_Throne`, fill=..x..))+ # specificam setul de date si variabila si faptul ca va fi colorata
    geom_histogram(binwidth = 10)+                                         # specificam ca dorim ca barele sa reprezinte intervale de 10 ani
    scale_fill_gradient("Legenda", low="blue", high = "red")+              # coloram barele pe un gradient crescator de la mic la mare            
    ggtitle("Varsta preluarii tronului")
    gr_varsta  
  
 
gr_domnie <- ggplot(data = df_imp_rom, aes(x=`Rule`))+
  geom_histogram(aes(y=..density..),               # axa y ne va da densitatea relativa
                 binwidth = 2,                     #specificam ca dorim ca barele sa reprezinte intervale de 2 ani
                 color="black",                    # specificam culoarealiniei din jurul intervalelor
                 fill="white")+                    # specificam culoarea interiorului intervalelor
  geom_density(alpha=.2, fill="blue")+             # cerem afisarea curbei densitatii peste histograma, alpha = opacitate, fill = culoare
  geom_vline(aes(xintercept=mean(Rule)), color="red", linetype="dashed")+  # cerem afisarea mediei ca o linie rosie punctata
  ggtitle("Durata domniei")
gr_domnie

gr_viata <- hist(df_imp_rom$Life, freq = F, breaks = seq(from=0, to=90, by=1), main = "Durata vietii", 
       xlab = "Ani", las=1,
       ylim = c(0, 0.07)) 

lines(                              # comanda lines adauga o linie ultimului grafic realizat
  density(df_imp_rom$Life, bw=4),   # comanda density indica faptul ca linia trebuie sa reprezinte densitatea variabilei
  col = "red")  
abline(v=mean(df_imp_rom$Life),  # comanda abline adauga o linie dreapta graficului, argumentul v=mean() indica o linie verticala plasata in 
                                 # dreptul mediei pe axa x
       col="blue",               # culoarea liniei e albastra
       lty=2)                    # line type - tipul liniei ex 2 = intrerupta, 3 = punctata, 4 = intrerupta si cu puncte vexi ?par


##### sumare numerice ------
?ExpNumStat

SmartEDA::ExpNumStat(df_imp_rom,
                     by="A",              # cerem analizarea tuturor variabilelor numerice "A"=all
                     Outlier = T,         # cerem identificarea valorior extreme 
                     Qnt = c(0.25, 0.75)) # cerem identificarea primei si a celei de a treia quartile

####### SKWENESS = INDICELE DE asimetrie - valori in jur de zero indica distributie normala - valori mai mari de +- 1 indica o distributie asimetrica
####### KURTOSIS = INDICELE DE APALATIZARE - valoarea pentru o distributie normala este 3, 
        #valori<3 indica o distributie apalatizata cu posibile valori extreme departate de medie
        #valori >3 indica o distributie grupata mai strans in jurul mediei



