#CURS---------

#####SEMINAR-----------
library(car)
library(nlme)
library(multcomp)
library(psych)


# importul si incarcarea datelor in memoria de lucru---------
library(readxl)
example_dataset <- read_excel("example_dataset.xlsx")
df_re <- as.data.frame(example_dataset)
rm(example_dataset)

######definim variabilele categorice ca factori---------
df_re[,c(2:6)] <- lapply(df_re[,c(2:5)], factor)

######eliminam coloana Index-----------
df_re <- df_re[, -1]

df_re <- subset(df_re, !(Succession %in% c("Purchase","Election")))    # eliminam cazurile singulare

###creem o noua coloana care contine precizarea modului de ascensiune la putere ca drept ereditar, sau altul
df_re$Succession_bivariate <- as.factor(                               #cerem crearea noii coloane ca factor
  ifelse(                                                              #functie prin care putem sa creem grupuri conditionate
    df_re$Succession=="Birthright",                                    #specificam setul de date si coloana pe care le dorim grupate
    #si conditia pe care o dorim indeplinita
    "Ereditar",                                                        # specificam valoarea pe care o dorim in cazul indeplinirii conditiei
    "Altul"))                                                          # specificam valoarea pe care o dorim in cazul neindeplinirii conditiei

fitted <- predict(lm(df_re$Life~df_re$Age_Throne))

plot(df_re$Age_Throne, df_re$Life)
abline(lm(df_re$Life~df_re$Age_Throne), col="green")
for (i in 1:61)
  lines (c(df_re$Age_Throne[i],df_re$Age_Throne[i]),c(df_re$Life[i],fitted[i]),col="red")


#####SEMINAR-----------
model <- lm(df_re$Life~df_re$Age_Throne)  # functia lm calculeaza un model de regresie liniara pentru variabila raspuns Life fata de variabila independenta Age_Throne
plot(model)                               # cerem afisarea graficelor de diagnostic ale modelului
summary(model)                            # cerem afisarea rezultatelor modelului


####modele robuste cu o variabila categorica--------
###Generalized Least Squares
plot(df_re$Succession, df_re$Rule)

leveneTest(df_re$Rule~df_re$Succession)  # verificam egalitatea variatiei mediilor grupului
model_gls <- gls(Rule ~ Succession, data = df_re) # cu functia gls cerem calcularea unui model de regresie robust care permite existenta de corelatii intre erori precum si variatii inegale ale acestora
plot(model_gls)                                   # distributia rezidualelor fata de valorile ajustate indica existenta unei corelatii intre acestea
qqnorm(model_gls)                                 # distributia rezidualelor fata de distributia normala a quantilelor nu indica abateri majore de la aceasta

model_gls_2 <- gls(Rule ~ Succession, data = df_re, weights=varPower()) # weights=varPower descrie heteroscedasticitatea (la date bivariate, variabila y prezinta heteroscedasticitate daca imprastierea valorilor y depinde de x)
plot(model_gls_2)
qqnorm(model_gls_2)
summary(model_gls_2)
summary(model_gls)
nagelkerke(model_gls_2) # in cazul modelelor non-parametrice puterea explicativa a modelului nu este calculata. Functia nagelkerke calculeaza o serie de indici pseudo r patrat
#cld(glht(model_gls_2, linfct=mcp(Succession="Tukey")))
glht(model_gls_2, linfct=mcp(Succession="Tukey"))


#ANCOVA- selectia modelelor--------------
plot(df_re$Life, df_re$Age_Throne)
plot(df_re$Cause, df_re$Life)
ggplot(df_re, aes(x=df_re$Cause, y=Life, fill=df_re$Cause))+
  geom_violin()+                                    # graficul de tip "violin" ne reda distributia valorilor                                      
  geom_boxplot(width=0.2, color="black", alpha=0.3) # graficul de tip boxplot ne indica valorile statistice relevante si posibilele valori aberante

summary(aov(df_re$Life~df_re$Cause))
model <- lm(df_re$Life~df_re$Age_Throne*df_re$Cause)
summary.aov(model)
model2 <- lm(df_re$Life~df_re$Cause*df_re$Age_Throne)
summary.aov(model2)
model3 <- lm(df_re$Life~df_re$Cause+df_re$Age_Throne)
summary.aov(model3)

anova(model2,model3)

summary(model3)

ggplot(df_re, aes(x=Life, y=Age_Throne, color=Cause))+
  geom_point(size=6)+ 
  geom_smooth(method = "lm")

ggplot(df_re, aes(x=Life, y=Age_Throne, color=Cause))+
  geom_point(size=6)+ 
  geom_smooth(method = "lm")+
  facet_grid(.~Cause)



###CORELATIA---------------
##variabile numerice DEPENDENTE
cor_re <- cor(df_re[,6:8])
corr.test(df_re[,6:8], use="complete")
library(corrplot)
corrplot(cor_re, type = "lower")
