###Seminar 1 -----------------

##### instalare pachete--------------
install.packages("writexl")
library(writexl)


##### crearea de obiecte--------------

x=9

x

X<-12  # Alt+-
X
     
##### suprascriere ------------
X <- 16



##### stergere din memorie----------
rm(x)


x=9
x

##### valori categorice--------
xx = "C"
XX = "1"  ### in acest caz 1 este tratat ca o "litera" si nu o cifra


##### operatii matematice

1+2
2-1
2*2
4/2
suma <- X+x
10^3
##### comenzi-------

sqrt(4)  ### Tab sau Ctrl+Space pentru fortarea autocompletarii
sqrt(x)
log10(1000) #### inversul ridicarii la putere

#log10(1000   ### IMPORTANT semnul plus semnaleaza faptul ca sintaxa comenzii e incompleta, in cazul acesta paranteza ramane deschisa 
      
##### shortcuts---------
####### sageata in sus in consola readuce ultima comanda introdusa

##### vectori---------

vec_varsta <- c(15, 39, 29, 9, 41) #### c = concatena/a insirui # explica informatiile in Environment
vec.imp <- c("Elagabalus", "Marcus Aurelius", "Domitian", "Caracala", "Hadrian")
vec_x=2:6

####### daca vectorii sunt numerici si au aceeasi lungime, putem sa ii adunam/scadem/impartim ... etc pe randuri
vec_varsta+vec_x

### extragerea anumitor valori din vector

vec_selectie <- vec.imp[1] ### extragerea unei anumite valori din vector
vec_selectie2 <- vec.imp[-1] ### extragerea unor anumite valori din vector, cu exceptia uneia
vec_selectie3 <- vec.imp[-c(1,4)] ### extragerea unor anumite valori din vector, cu exceptia mai multora
vec_selectie4 <- vec_varsta[vec_varsta>10] ### extragerea unor anumite valori din vector, cu o anumita conditie

##### data frames----------
df_imp <- data.frame(vec.imp, vec_varsta) # ordinea vectorilor stabileste ordinea coloanelor

library(writexl)

write_xlsx(df_imp, path = "C:\\Users\\georg\\OneDrive\\Curs Statistica\\Proiect_curs\\df_imp.xlsx") # export ca fisier excel

df_imp <- read_excel("C:\\Users\\georg\\OneDrive\\Curs Statistica\\Proiect_curs\\df_imp.xlsx") # import din fisier excel

