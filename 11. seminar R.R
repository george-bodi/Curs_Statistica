###### library(archdata) - pentru proiect si restante
# importul si incarcarea datelor in memoria de lucru---------
library(readxl)
example_dataset <- read_excel("example_dataset.xlsx")
df_re <- as.data.frame(example_dataset)
rm(example_dataset)

######definim variabilele categorice ca factori---------
df_re[,c(2:6)] <- lapply(df_re[,c(2:6)], factor)

######eliminam coloana Index-----------
df_re <- df_re[, -1]

###### stabilim coloana cu numele imparatilor drept nume de rand ----------
rownames(df_re) <- df_re$Name

##### eliminam cazurile singulare ----------

df_re <- subset(df_re, !(Succession %in% c("Purchase","Election")))

###creem o noua coloana care contine precizarea modului de ascensiune la putere ca drept ereditar, sau altul
df_re$Succession_bivariate <- as.factor(                               #cerem crearea noii coloane ca factor
  ifelse(                                                              #functie prin care putem sa creem grupuri conditionate
    df_re$Succession=="Birthright",                                    #specificam setul de date si coloana pe care le dorim grupate
    #si conditia pe care o dorim indeplinita
    "Ereditar",                                                        # specificam valoarea pe care o dorim in cazul indeplinirii conditiei
    "Altul"))                                                          # specificam valoarea pe care o dorim in cazul neindeplinirii conditiei


##ANALIZA COMPONENTEI PRINCIPALE--

#### cream un nou set de date care contine doar variabilele numerice -----

re_num <- df_re[, 6:8]

###### calculam componentele principale cu standardizarea variabilelor -------
library(FactoMineR)
library(factoextra)

pca_re <- PCA(re_num, graph = FALSE)
print(pca_re)
summary(pca_re)

#### creem un grafic pentru a vizuliza variatia explicata de fiecare componenta principala --------
graf_pca <- fviz_eig(pca_re, addlabels = TRUE)
graf_pca

#### creem un grafic pentru a vizuliza contributia fiecarei variabile la prima componenta principala --------
contrib_cp1 <- fviz_contrib(pca_re, choice = "var",   #argumentul choices = "var" cere afisarea contributiilor variabilelor - poate fi inlocuit cu choices = "ind" pentru indivizi
             axes = 1)                                #argumentul axes = 1 cere afisarea contributiilor pentru componenta principala 1 - poate fi inlocuit cu axes = 2 pentru componenta principala 2, etc.                  
contrib_cp1

contrib_cp2 <- fviz_contrib(pca_re, choice = "var", axes = 2)                                                 
contrib_cp2


#### creem un grafic pentru a vizuliza corelatiile intre variabile si dintre variabile si primele doua componente principale --------
graf_cor_var <- fviz_pca_var(pca_re, col.var = "cos2",
             repel = TRUE)                                       # argument pentru a evita suprapunerea textului
graf_cor_var

#### creem un grafic pentru a vizuliza corelatiile intre indivizi si dintre indivizi si primele doua componente principale --------


graf_pca_ind1 <- fviz_pca_biplot(pca_re,
                col.ind = df_re$Succession_bivariate,    # cerem colorarea indivizilor in functie de modul de succesiune
                col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                legend.title = "Succesiune")             # specificam titlul legendei
graf_pca_ind1

graf_pca_ind2 <- fviz_pca_biplot(pca_re,
                                 col.ind = df_re$Cause,                   # cerem colorarea indivizilor in functie de cauza mortii
                                 col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                                 repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                                 legend.title = "Cauza Mortii")           # specificam titlul legendei
graf_pca_ind2

