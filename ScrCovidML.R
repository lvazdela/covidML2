#____________________________________________________
# title: 'Code for manuscript: mortality prediction of covid19'
# author: "Luis G. Vazquez de Lara Cisneros."
# date: "09/09/21"
#____________________________________________________

#Prueba para hacer algunos cambios en git
#segundaprueba para cambio en git
# DOWNLOAD AND/OR OPEN LIBRARIES ------------------------------------------

#Libraries needded:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org", dependencies = TRUE)


# DOWNLOAD THE NECESSARY FILES --------------------------------------------


# Download the files
urlcscovid <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/cscovid.csv'
dbcovid <- read.csv(urlcscovid)

urlengvar <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvar2.txt'
engvar <- read_lines(urlengvar)
urlcodunits <- read_lines('https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvar3utf8.txt')
codunits <- read_lines(urlcodunits)

urlengvarnum <- 'https://raw.githubusercontent.com/lvazdela/capstone2_lvl/main/other-files/engvarnum.txt'
engvarnum <- read_lines(urlengvarnum)

# DATA WRANGLING ----------------------------------------------------------

# dataframe of variables in dbcovid.
nomvar <- dbcovid %>%
  names
rawdb <- tibble(code = nomvar, Description = engvar, cod_units = codunits)

#Manage date columns
dbcovid <- dbcovid %>% mutate(across(starts_with('fecha'), dmy))

#Check errors in numeric variables
nomcadenas <- c('urea', 'creat', 'bun', 'plaq', 'ca', 'aat', 'alat')
patron <- '([^\\d\\.])|(\\.{2,})' #Anything but digits or one decimal point.

fpatron <- function(x){
  x[str_which(x, pattern = patron)]
}

errores <- apply(dbcovid[, nomcadenas],2,fpatron )
dberrores <- data.frame(mistakes = unlist(errores))

#Check errors in numeric variables
nomcadenas <- c('urea', 'creat', 'bun', 'plaq', 'ca', 'aat', 'alat')
patron <- '([^\\d\\.])|(\\.{2,})' #Anything but digits or one decimal point.

fpatron <- function(x){
  x[str_which(x, pattern = patron)]
}

errores <- apply(dbcovid[, nomcadenas],2,fpatron )

# Catch-all function to detect errors in numeric variables
farreglar <- function(x){
  x = str_trim(x)
  x = case_when(
    str_detect(x, '\\s') ~ str_replace_all(x, '\\s', ''),
    str_detect(x, '[:alpha:]') ~ str_replace_all(x, '[:alpha:]',''),
    str_detect(x, ',|\\.{2,}') ~ str_replace_all(x,',|\\.{2,}', '.'),
    TRUE ~ x
  )
}

#Fix errors in numeric variables
dbcovid <- dbcovid %>% mutate(across(all_of(nomcadenas), farreglar))
#Check if it worked
arregerrores <- apply(dbcovid[, nomcadenas],2,fpatron )
arregerrores # no mistakes

# Transform numeric variables to numeric type 
dbcovid <- dbcovid %>% mutate(across(all_of(nomcadenas), as.numeric))

#Create new variables with dates, and eliminate the date variables.
dbcovid <- dbcovid %>%
  mutate(bmi = peso/talla^2, 
         dayshosp = as.numeric(fechalta - fechahosp),
         duration = as.numeric(fechalta - fechainisint),
         daysdelay = as.numeric(fechahosp - fechainisint),
         obesity = ifelse(bmi >= 30, 1, 2)) %>%
  select(-starts_with('fecha'))

# Change codification of dichotomous categorical variables.
dicot <- c('motivoegre','has', 'tabaquismo', 'dm', 'renal', 'autoinmunidad',
           'ing_disnea', 'obesity')
dbcovid <- dbcovid %>%
  mutate(across(all_of(dicot), function(x) ifelse(x == 2, 0, 1)))

# Transform categorical variables to factors.
nomcateg <- c('motivoegre','sexo', 'ocupacion', 'nivsoc', dicot[-1])
dbcovid <- dbcovid %>%
  mutate(across(all_of(nomcateg), as.factor))
save(list = c('dbcovid', 'nomvar', 'engvar', 'engvarnum', 'codunits',
              'dicot','nomcateg') , file = 'Rdata/dbcovid.Rda')


# DATA EXPLORATION --------------------------------------------------------
library(tidyverse)
load('Rdata/dbcovid.Rda')
#Function to process categorical variables
creatortabcat <- function(nomvar, etiq){
  dbcovid %>%
    select(motivoegre, all_of(nomvar)) %>%
    mutate(across(all_of(nomvar), function(x) factor(x, labels = etiq))) %>%
    mutate(motivoegre = factor(motivoegre, levels = c(0,1), labels = c('Alive', 'Dead'))) %>%
    pivot_longer(all_of(nomvar), names_to = 'apps', values_to = 'valor') %>%
    group_by(motivoegre, apps, valor) %>%
    summarise(n = n()) %>%
    mutate(frec = round(n/sum(n)*100, 2),
           total = sum(n)) %>%
    pivot_wider(names_from = motivoegre, values_from = c(n, frec, total)) %>%
    mutate(total = n_Alive + n_Dead,
           pordef = round(n_Dead/total*100, 1),
           pormejo = round(n_Alive/total*100, 1),
           muertos = paste0(str_pad(n_Dead,4,side = 'right', pad = ' '), '(', pordef, ')'),
           vivos = paste0(str_pad(n_Alive,4,side = 'right'), '(', pormejo, ')')) %>% 
    select(apps, valor, total, muertos, vivos) %>%
    filter(!is.na(valor)) %>%
    rename(Variable = apps, Value = valor, Total = total, Dead = muertos, Alive = vivos)
}

#data frames of summaries of categorical variables:
dbsex <- creatortabcat('sexo', c('Female', 'Male'))

englaboccup <- c('Health care worker', 'Office job','Outdoor work',
                 'Work in public area', 'Work at home', 'Unemployed')
dboccup <- creatortabcat('ocupacion', englaboccup)

dbnivsoc <- creatortabcat('nivsoc', c('Low or medium-low', 'Medium-high or high'))

dbdicot <- creatortabcat(dicot[-1], c('No', 'Yes'))

#Join the dataframes:
dbvarcateg <- bind_rows(dbsex, dboccup, dbnivsoc, dbdicot)

#function to write the p value of chi-squared test in the database  
fchipavarcateg <- function(x){
  p <- round(chisq.test(table(x, dbcovid$motivoegre))$p.value, 3)
  if (!is.factor(x)){
    x <- as.factor(x)
  } 
  blancos <- length(levels(x)) -1
  if (p < 0.001){
    p <- '< 0.001'
  }
  cad <- c(p, rep('', blancos))
  return(cad)
}
varcateg <- unique(dbvarcateg$Variable)

listachis <- unlist(apply(dbcovid[,varcateg],2, fchipavarcateg))
numlistachis <- apply(dbcovid[,varcateg], 2, function(x){chisq.test(table(x, dbcovid$motivoegre))$p.value} )


#Add listachisfin to the database and translate variables to English
engvarord <- c('Autoimmunity', 'Diabetes', 'Hypertension', 'Short of breath', 'Socioeconomic level',
               'Obesity', 'Occupation', 'Renal disease', 'Gender', 'Smoking')
dbvarcateg <- dbvarcateg %>%
  ungroup %>%
  mutate(p = listachis,
         Variable = factor(Variable, labels = engvarord))

# Exploratory analysis of numeric variables
varnum <- dbcovid %>%
  select(where(is.numeric)) %>%
  names


dbtranslator <- data.frame(spa = varnum, eng = engvarnum) %>%
  arrange(spa)

listapesnum <- apply(dbcovid[, varnum[-1]], 2, function(x) t.test(x ~ dbcovid$motivoegre)$p.value)
umann <- wilcox.test(dbcovid$escolaridad ~ dbcovid$motivoegre)$p.value
listapesnum <- round(c(umann, listapesnum), 3)
listapesnum2 <- ifelse(listapesnum < 0.001, '< 0.001', listapesnum)
#list of numeric variables in the model

dftes2 <- data.frame(Variable = varnum, p = listapesnum2)

dbvarnum <- dbcovid %>%
  select(motivoegre, all_of(varnum)) %>%
  mutate(motivoegre = factor(motivoegre, labels = c('Alive', 'Dead'))) %>%
  group_by(motivoegre) %>%
  summarise(across(everything(), list(
    validos = ~ sum(!is.na(.x)),
    prom = ~ round(mean(.x, na.rm = TRUE),2),
    desvest = ~ round(sd(.x, na.rm = TRUE),2)
  )))%>%
  pivot_longer(cols = -motivoegre,
               names_sep = '_',
               names_to = c('Variable', '.value')) %>%
  left_join(dftes2, by = 'Variable')%>%
  unite(meansd, c(prom, desvest), sep = ' ? ') %>%
  select(Variable, validos, motivoegre, meansd, p  )%>%
  mutate(Variable = factor(Variable, labels = dbtranslator$eng)) %>%
  arrange(p, Variable)

# DATA PREPROCESSING ------------------------------------------------------
load('Rdata/dbcovid.Rda')
finalvarcateg <- varcateg[which(numlistachis < 0.051)]
finalvarnum <- varnum[which(listapesnum < 0.051)]
finalvarnum <- finalvarnum[1:24]
dbcovid <- dbcovid %>%
  select(all_of(c('motivoegre', finalvarcateg, finalvarnum))) %>%
  mutate(motivoegre = factor(motivoegre, labels = c('Alive', 'Dead')),
         sexo = factor(sexo, labels = c('Female', 'Male')),
         nivsoc = factor(nivsoc, labels = c('low', 'High')),
         dm = factor(dm, labels = c('No', 'Yes')),
         has = factor(has, labels = c('No', 'Yes')),
         tabaquismo = factor(tabaquismo, labels = c('No', 'Yes')),
         ing_disnea = factor(ing_disnea, labels = c('No', 'Yes')))

nearZeroVar(dbcovid)

#Impute with the median
preprocmd <- preProcess(dbcovid, method = 'medianImpute')
dbcovidmd <- predict(preprocmd, dbcovid )
sum(is.na(dbcovidmd)) # Show the missings in the categorical variables
dbcovidmd <- na.omit(dbcovidmd)
sum(is.na(dbcovidmd)) #check we have no missing data
save(dbcovidmd, file = 'Rdata/dbcovidmd.Rda')

#Imputation via bagging (if I leave the factor variables, it throws an error).
set.seed(271220)
preprocbag <- dbcovid %>%
  select(where(is.numeric)) %>%
  preProcess(., method = 'bagImpute')

dbcovidbag <- dbcovid %>%
  select(where(is.numeric)) %>%
  predict(preprocbag, .)

dbcovidbag <- dbcovid %>%
  select(where(is.factor)) %>%
  bind_cols(dbcovidbag) %>%
  na.omit
sum(is.na(dbcovidbag)) # check we have no missing data

# Check missingness in the database.
dbnas <- dbcovid %>%
  select(all_of(c('motivoegre', finalvarcateg, finalvarnum))) %>%
  summarise(across(everything(),  ~sum(is.na(.x)) )) %>%
  t(.) %>%
  as.data.frame(.) %>%
  rename(totalnas = V1) %>%
  mutate(porcentaje = round(totalnas/dim(dbcovid)[1]*100,1),
         len = dim(dbcovid)[1],
         variable = row.names(.),
         validos = len - totalnas) %>%
  select(variable, validos, totalnas, porcentaje, len)%>%
  arrange(desc(porcentaje))


# MODEL GENERATION --------------------------------------------------------
library(tidyverse)
library(caret)
load('Rdata/dbcovidmd.Rda')

# Data partition
set.seed(271220)
indtrainmd <- createDataPartition(y = dbcovidmd$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setmd <- dbcovidmd[indtrainmd,]
test_setmd <- dbcovidmd[-indtrainmd,]

set.seed(271220)
indtrainbag <- createDataPartition(y = dbcovidbag$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setbag <- dbcovidbag[indtrainbag,]
test_setbag <- dbcovidbag[-indtrainbag,]

tr.control <- trainControl(method = 'boot',
                           number = 25)

#GMM 
#Data set imputed with the median
set.seed(271220)
fit_glm <- train(motivoegre ~ .,
                 method = 'glm',
                 data = train_setmd,
                 trControl = tr.control)

model_glm <- predict(fit_glm, newdata = test_setmd, type = 'raw')
cm <- confusionMatrix(data = model_glm, reference = test_setmd$motivoegre)
acc_glmmd <- cm$overall[["Accuracy"]]
acc_glmmdup <- cm$overall[["AccuracyUpper"]]
acc_glmmdlo <-  cm$overall[["AccuracyLower"]]

#Data set imputed with bagging
# set.seed(271220)
# fit_glm <- train(motivoegre ~ .,
#                  method = 'glm',
#                  data = train_setbag,
#                  trControl = tr.control)
# 
# model_glm <- predict(fit_glm, newdata = test_setbag, type = 'raw')
# cm <- confusionMatrix(data = model_glm, reference = test_setbag$motivoegre)
# acc_glmbag <- cm$overall[["Accuracy"]]
# acc_glmbagup <- cm$overall[["AccuracyUpper"]]
# acc_glmbaglo <-  cm$overall[["AccuracyLower"]]
# 
# varimpglm <- varImp(fit_glm)[["importance"]]
# nomvars <- row.names(varimpglm)
# varimpglm <- varimpglm %>%
#   mutate(varnameglm = nomvars) %>%
#   rename(valueglm = Overall) %>%
#   select(varnameglm, valueglm) %>%
#   arrange(desc(valueglm))

# kNN

#Imputed with median
set.seed(271220)
fit_knn <- train(motivoegre ~ .,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(5,35, 1)),
                 trControl = tr.control,
                 data = train_setmd)

model_knn <- predict(fit_knn, newdata = test_setmd)
cm <- confusionMatrix(data = model_knn, reference = test_setmd$motivoegre)
acc_knnmd <- cm$overall[["Accuracy"]]
acc_knnmdup <- cm$overall[["AccuracyUpper"]]
acc_knnmdlo <-  cm$overall[["AccuracyLower"]]

#imputed with bagging
# set.seed(271220)
# fit_knn <- train(motivoegre ~ .,
#                  method = 'knn',
#                  tuneGrid = data.frame(k = seq(5,35, 1)),
#                  trControl = tr.control,
#                  data = train_setbag)
# 
# model_knn <- predict(fit_knn, newdata = test_setbag)
# 
# cm <- confusionMatrix(data = model_knn, reference = test_setbag$motivoegre)
# acc_knnbag <- cm$overall[["Accuracy"]]
# acc_knnbagup <- cm$overall[["AccuracyUpper"]]
# acc_knnbaglo <-  cm$overall[["AccuracyLower"]]
# 
# btune_knn <- fit_knn$bestTune
# 
# varimpknn <- varImp(fit_knn)$importance
# nomvars <- row.names(varimpknn)
# varimpknn <- varimpknn %>%
#   mutate(varnameknn = nomvars) %>%
#   rename(valueknn = Dead) %>%
#   select(-Alive) %>%
#   select(varnameknn, valueknn) %>%
#   arrange(desc(valueknn))

# CART

# #Imputed with bagging
# set.seed(271220)
# 
# fit_rpart <- train(motivoegre ~ ., method = 'rpart',
#                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
#                    trControl = tr.control,
#                    data = train_setbag)
# 
# 
# model_rpart <- predict(fit_rpart, newdata = test_setbag)
# cm <- confusionMatrix(data = model_rpart, reference = test_setbag$motivoegre)
# acc_rpartbag <- cm$overall[["Accuracy"]]
# acc_rpartbagup <- cm$overall[["AccuracyUpper"]]
# acc_rpartbaglo <-  cm$overall[["AccuracyLower"]]

#Imputed with the median
set.seed(271220)
fit_rpart <- train(motivoegre ~ ., method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                   trControl = tr.control,
                   data = train_setmd)


model_rpart <- predict(fit_rpart, newdata = test_setmd)
cm <- confusionMatrix(data = model_rpart, reference = test_setmd$motivoegre)
acc_rpartmd <- cm$overall[["Accuracy"]]
acc_rpartmdup <- cm$overall[["AccuracyUpper"]]
acc_rpartmdlo <-  cm$overall[["AccuracyLower"]]
btune_rpart <-  fit_rpart$bestTune

varimprpart <- varImp(fit_rpart)$importance
nomvars <- row.names(varimprpart)
varimprpart <- varimprpart %>%
  mutate(varnamerpart = nomvars) %>%
  rename(valuerpart = Overall) %>%
  select(varnamerpart, valuerpart) %>%
  arrange(desc(valuerpart))


# RANDOM FORESTS

control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))
# #Imputed with bagging
# set.seed(271220)
# fit_rf <- train(motivoegre ~ .,
#                 method = 'rf',
#                 ntree = 150,
#                 trControl = control_rf,
#                 tuneGrid = grid,
#                 data = train_setbag)
# 
# model_rf <- predict(fit_rf, newdata = test_setbag)
# cm <- confusionMatrix(data = model_rf, reference = test_setbag$motivoegre)
# acc_rfbag <- cm$overall[["Accuracy"]]
# acc_rfbagup <- cm$overall[["AccuracyUpper"]]
# acc_rfbaglo <-  cm$overall[["AccuracyLower"]]

#Imputed with the median
set.seed(271220)
fit_rf <- train(motivoegre ~ .,
                method = 'rf',
                ntree = 150,
                trControl = control_rf,
                tuneGrid = grid,
                data = train_setmd)

model_rf <- predict(fit_rf, newdata = test_setmd)
cm <- confusionMatrix(data = model_rf, reference = test_setmd$motivoegre)
acc_rfmd <- cm$overall[["Accuracy"]]
acc_rfmdup <- cm$overall[["AccuracyUpper"]]
acc_rfmdlo <-  cm$overall[["AccuracyLower"]]


varimprf <- varImp(fit_rf)$importance
nomvars <- row.names(varimprf)
varimprf <- varimprf %>%
  mutate(varnamerf = nomvars) %>%
  rename(valuerf = Overall) %>%
  select(varnamerf, valuerf) %>%
  arrange(desc(valuerf))
btune_rf <- fit_rf$bestTune %>% pull

# Data frame with the accuracies and confidence intervals
accsmd <- c(acc_glmmd, acc_knnmd, acc_rpartmd, acc_rfmd)
# accsbag <- c(acc_glmbag,acc_knnbag, acc_rpartbag, acc_rfbag)
accslomd <- c(acc_glmmdlo, acc_knnmdlo, acc_rpartmdlo, acc_rfmdlo)
# accslobag <- c(acc_glmbaglo, acc_knnbaglo, acc_rpartbaglo, acc_rfbaglo)
accsupmd <- c(acc_glmmdup, acc_knnmdup, acc_rpartmdup, acc_rfmdup)
# accsupbag <- c(acc_glmbagup, acc_knnbagup, acc_rpartbagup, acc_rfbagup )
model <- (c('Generalized linear model', 'k nearest neighbors', 'CART', 'Random forests'))

dfresfin <- tibble(model = model,
                   accsmd = accsmd,
                   accslomd = accslomd,
                   accsupmd = accsupmd) %>%
                   # accsbag = accsbag,
                   # accslobag = accslobag,
                   # accsupbag = accsupbag) %>%
  mutate(across(where(is.numeric), ~ round(.,3))) %>%
  unite(confintmd, all_of(c('accslomd', 'accsupmd')), sep = '-') #%>%
  # unite(confintbag, all_of(c('accslobag', 'accsupbag')), sep = '-')

# Data frame showing the glm coefficients.
fileconn <- file('glmres.txt')
writeLines (capture.output(summary(fit_glm)), fileconn)
close(fileconn)

dbresglm <- read.table('glmres.txt', skip = 10, nrows = 36, header = FALSE, fill = TRUE) %>%
  select(1:5) %>%
  filter(V1 != '(Intercept)') %>%
  mutate(across(c(2,3,5), ~round(., 4))) %>%
  mutate(V1 =  str_replace_all(V1, '\\_', '\\\\_')) %>%
  arrange(V5) %>%
  slice(1:20)

# plot of k tuninng
plot(fit_knn, xlab = 'Number of neighbors')

#Plot of cp parameter
plot(fit_rpart)

#Plot of the classification tree
plot(fit_rpart$finalModel, margin = 0.1) 
text(fit_rpart$finalModel, cex = 0.75)

#Plots of the random forestes parameters
plot(fit_rf, xlab = 'Number of randomly selected predictors')
plot(fit_rf$finalModel, main = NULL)

# Data frame of the variable importance
dfvarimp <- bind_cols(varimpglm[1:20,], varimpknn[1:20,], varimprpart[1:20,], varimprf[1:20,]) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


# EMPLEO DE MENOS VARIABLES USANDO RESULTADOS ANTERIORES ------------------

#FECHA: 14/09/2021
#Primero usar? la base imputada con la mediana, la guardo en una variable
#Luego, selecciono solo las variables con la mayor importancia, y que se consideren
#cl?nicament relevantes, a esta base la llamar? dbcovidmd10
dbcovidmd10 <- dbcovidmd %>%
  select(c(motivoegre, sato2sin, neutros, pao2, paco2, dhl, dimd, urea, linfos, edad))

# Data partition
set.seed(140921)
indtrainmd10 <- createDataPartition(y = dbcovidmd10$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setmd10 <- dbcovidmd10[indtrainmd10,]
test_setmd10 <- dbcovidmd10[-indtrainmd10,]

tr.control <- trainControl(method = 'boot',
                           number = 25)

#GMM 
#Data set imputed with the median
set.seed(140921)
fit_glm <- train(motivoegre ~ .,
                 method = 'glm',
                 data = train_setmd10,
                 trControl = tr.control)

model_glm <- predict(fit_glm, newdata = test_setmd10, type = 'raw')
cm <- confusionMatrix(data = model_glm, reference = test_setmd10$motivoegre)
acc_glmmd10 <- cm$overall[["Accuracy"]]
acc_glmmd10up <- cm$overall[["AccuracyUpper"]]
acc_glmmd10lo <-  cm$overall[["AccuracyLower"]]

# kNN

#Imputed with median
set.seed(140921)
fit_knn <- train(motivoegre ~ .,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(30,100, 1)),
                 trControl = tr.control,
                 data = train_setmd10)

model_knn <- predict(fit_knn, newdata = test_setmd10)
cm <- confusionMatrix(data = model_knn, reference = test_setmd10$motivoegre)
acc_knnmd10 <- cm$overall[["Accuracy"]]
acc_knnmd10up <- cm$overall[["AccuracyUpper"]]
acc_knnmd10lo <-  cm$overall[["AccuracyLower"]]

plot(fit_knn, xlab = 'Number of neighbors')
fit_knn$bestTune

# CART
#Imputed with the median
set.seed(140921)
fit_rpart <- train(motivoegre ~ ., method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                   trControl = tr.control,
                   data = train_setmd10)

plot(fit_rpart)
fit_rpart$bestTune
model_rpart <- predict(fit_rpart, newdata = test_setmd10)
cm <- confusionMatrix(data = model_rpart, reference = test_setmd10$motivoegre)
acc_rpartmd10 <- cm$overall[["Accuracy"]]
acc_rpartmd10up <- cm$overall[["AccuracyUpper"]]
acc_rpartmd10lo <-  cm$overall[["AccuracyLower"]]
btune_rpart <-  fit_rpart$bestTune

# RANDOM FORESTS

control_rf <- trainControl(method = 'boot',
                           number = 25)
#grid <- data.frame(mtry = c(1,5,10,25))

#Imputed with the median
set.seed(140921)
fit_rf <- train(motivoegre ~ .,
                method = 'rf',
                ntree = 150,
                trControl = control_rf,
                tuneGrid = grid,
                data = train_setmd10)

model_rf <- predict(fit_rf, newdata = test_setmd10)
cm <- confusionMatrix(data = model_rf, reference = test_setmd10$motivoegre)
acc_rfmd10 <- cm$overall[["Accuracy"]]
acc_rfmd10up <- cm$overall[["AccuracyUpper"]]
acc_rfmd10lo <-  cm$overall[["AccuracyLower"]]

#CONCLUSI?N: LA COSA SALI? MUY MAL, DISMINUYE LA EFICIENCIA DE TODOS LOS MODELOS.



# MODELO CON ENSAMBLE -----------------------------------------------------

#guardamos variables para no correr innecesariamente c?digo:
save(list = c('dbcovidbag', 'test_setbag','fit_glmbag', 'fit_knnbag',
              'fit_rfbag', 'fit_rpartbag' ),
     file = 'Rdata/modelofinbag.Rda')

save(list = c('dbcovidmd', 'test_setmd','fit_glmmd', 'fit_knnmd',
              'fit_rfmd', 'fit_rpartmd' ),
     file = 'Rdata/modelofinmd.Rda')

library(tidyverse)
library(caret)

load('Rdata/modelofinmd.Rda')
#head(model_glm)

p_rf <- predict(fit_rfmd, test_setmd, type = 'prob')
head(p_rf)
p_rf <- p_rf/rowSums(p_rf)
head(p_rf)
p_knn <- predict(fit_knnmd, test_setmd, type = 'prob')
p_glm <- predict(fit_glmmd, test_setmd, type = 'prob')
head(p_glm)

p_rpart <- predict(fit_rpartmd, test_setmd, type = 'prob')

dim(p_knn)
dim(p_rf)
dim(p_glm)
dim(p_rpart)

p <- (p_glm + p_knn + p_rf + p_rpart)/4
head(p)
#y_pred <- factor(apply(p, 1, which.max))
y_pred <- factor(apply(p, 1, which.max)-1, labels = c('Alive', 'Dead'))
head(y_pred)
cm_ensamble <- confusionMatrix(y_pred, reference = test_setmd$motivoegre) #, positive = '1')
cm_ensamble
names(dbcovidmd)
names(test_setmd)

#Voto por mayor?a?
yhat_rfmed <- predict(fit_rfmd, test_setmd, type = 'raw')
head(yhat_rfmed)
yhat_knn <- predict(fit_knnmd, test_setmd, type = 'raw')
yhat_glm <- predict(fit_glmmd, test_setmd, type = 'raw')
head(yhat_glm)
yhat_rpart <- predict(fit_rpartmd, test_setmd, type = 'raw')

yhats <- cbind(yhat_glm, yhat_knn, yhat_rfmed, yhat_rpart) -1
yhat_ens <- rowSums(yhats)

yhat_ensam <- map_dbl(yhat_ens, function(x){
  ifelse(x > 2, 1, 0)
})
head(yhat_ensam)
yhat_ensam <- factor(yhat_ensam, labels = c('Alive', 'Dead'))
cm2_ensamble <- confusionMatrix(yhat_rfmed, reference = test_setmd$motivoegre) #, positive = '1')
cm2_ensamble

#PCA ejemplo rafabook (en el cap 33.5.6)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
#quees <- mnist$train$images #EQUIVALENTE A 
col_means <- colMeans(mnist$test$images)
#pca <- prcomp(quees) # OJO: TARDA MUCHO
load('Rdata/pcrafa.rda')
pc <- 1:ncol(mnist$test$images)
qplot(pc, pca$sdev)
save(pca, file = 'Rdata/pcarafa.rda')

dbcovidnum <- dbcovidmd %>%
  select(where(is.numeric)) %>%
  select(-escolaridad)

pcacov <- prcomp(dbcovidnum)

pc <- 1:ncol(dbcovidnum)

qplot(pc, pcacov$sdev)
summary(pcacov)$importance[,1:5]
data.frame(PC1 = pcacov$x[,1], PC2 = pcacov$x[,3],
           label = factor(dbcovidmd$motivoegre)) %>%
  ggplot(aes(PC1, PC2, fill = label)) +
  geom_point(cex = 3, pch = 21)


# DATA WRANGLING OF dbcovidval --------------------------------------------


library(tidyverse)
library(caret)
load('Rdata/modelofinmd.Rda')
#load('Rdata/dbcovidval.Rda')
dbcovidval <- read.csv('bases-originales/dbcovidval.csv')

names(dbcovidval)
str(dbcovidval)
dim(dbcovidval)
dbcovidval <- slice(dbcovidval, 1:100)
summary(dbcovidmd$nivsoc)
table(dbcovidval$motivoegre)
table(dbcovidval$has)
table(dbcovidval$sexo)

#Arreglamos dicot?micas
dicot <- c('motivoegre','has', 'tabaquismo', 'dm', 
           'ing_disnea')
dbcovidval <- dbcovidval %>%
  mutate(across(all_of(dicot), function(x) ifelse(x == 2, 0, 1)))
#Arreglamos sexo
dbcovidval <- dbcovidval %>%
  mutate(sexo = factor(sexo,levels = c('F', 'M'), labels = c('Female', 'Male')))
#Arreglamos escolaridad y ocupaci?n
table(dbcovidval$escolaridad)
dbcovidval <- dbcovidval %>%
  mutate(across(all_of(c('escolaridad', 'ocupacion')), as.factor))
class(dbcovidval$escolaridad)
#Arreglamos nivsoc
table(dbcovidval$nivsoc)
dbcovidval <- dbcovidval %>%
  mutate(nivsoc = factor(ifelse(nivsoc %in% c(1,2), 1, 2), labels = c('low', 'High')))
str(dbcovidval)
dbcovidval <- dbcovidval %>%
  select(- c(nombre, nss))
dbcovidval$motivoegre <- factor(dbcovidval$motivoegre, labels = c('Alive', 'Dead'))
dbcovidval <- dbcovidval %>%
  mutate(across(all_of(c('has', 'dm', 'tabaquismo', 'ing_disnea')), ~factor(.x, labels = c('No', 'Yes'))))
str(dbcovidval)
# colesterol
table(dbcovidval$colesterol)
save(dbcovidval, file = 'Rdata/dbcovidval.Rda')

p_rf <- predict(fit_rfmd, dbcovidval)
#El colesterol de captur? como ordinal, no se pusieron los valores brutos.

# 26/09/21 se quitar? colesterol de dbcovingmd ----------------------------

library(tidyverse)
library((lubridate))
load('Rdata/dbcovidmd.Rda')
load('Rdata/modelofinmd.Rda')

# Veo el comportamiento de dbcovidmd, checo que salga el mismo resultado
#solo emplear? el modelo de random forests.

model_rf <- predict(fit_rfmd, newdata = test_setmd)
cm <- confusionMatrix(data = model_rf, reference = test_setmd$motivoegre)
cm

#Quito el colesterol de dbcovidmd:
names(dbcovidmd)
dbcovidnocol <- dbcovidmd %>%
  select(-colesterol)

#Particiono la nueva base, usando el misma seed.
set.seed(271220)
indtrainmd <- createDataPartition(y = dbcovidnocol$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setnocol <- dbcovidnocol[indtrainmd,]
test_setnocol <- dbcovidnocol[-indtrainmd,]
control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))

set.seed(271220)
fit_rfnocol <- train(motivoegre ~ .,
                method = 'rf',
                ntree = 150,
                trControl = control_rf,
                tuneGrid = grid,
                data = train_setnocol)

model_rfnocol <- predict(fit_rfnocol, newdata = test_setnocol)
cm2 <- confusionMatrix(data = model_rfnocol, reference = test_setnocol$motivoegre)
cm2 # SE PIERDE UNA D?CIMA DE ACCURACY.
#CONCLUSI?N: HAY QUE PREGUNTAR A ?LVARO QUE CAMBIEN EL COLESTEROL.


# MODELO rf CON VARIABLES CON p MENOR A 0.01 ---------------------------------

load('Rdata/dbcovid.Rda')
#corro parte del c?digo de la secci?n de preprocesamiento de datos
# para tener varcateg, varnumlistachis, numlistachis
# repito la parte final del c?digo aqu?:
varnum <- dbcovid %>%
  select(where(is.numeric)) %>%
  names
listapesnum <- apply(dbcovid[, varnum[-1]], 2, function(x) t.test(x ~ dbcovid$motivoegre)$p.value)
umann <- wilcox.test(dbcovid$escolaridad ~ dbcovid$motivoegre)$p.value
listapesnum <- round(c(umann, listapesnum), 3)

#AQU? CAMBIA EL MODELO
finalvarcateg <- varcateg[which(numlistachis < 0.01)]
finalvarnum <- varnum[which(listapesnum < 0.01)]
finalvarnum <- finalvarnum[1:19] #quitamos dayshosp y duration
dbcovid <- dbcovid %>%
  select(all_of(c('motivoegre', finalvarcateg, finalvarnum))) %>%
  mutate(motivoegre = factor(motivoegre, labels = c('Alive', 'Dead')),
         sexo = factor(sexo, labels = c('Female', 'Male')),
         nivsoc = factor(nivsoc, labels = c('low', 'High')),
         dm = factor(dm, labels = c('No', 'Yes')),
         tabaquismo = factor(tabaquismo, labels = c('No', 'Yes'))
  )

nearZeroVar(dbcovid)

#Impute with the median
preprocmd <- preProcess(dbcovid, method = 'medianImpute')
dbcovidmd2 <- predict(preprocmd, dbcovid )
sum(is.na(dbcovidmd2)) # Show the missings in the categorical variables
dbcovidmd2 <- na.omit(dbcovidmd2)
sum(is.na(dbcovidmd2))
save(dbcovidmd2, file = 'Rdata/dbcovidmd2.Rda')

#Impute with bagging
set.seed(271220)
preprocbag <- dbcovid %>%
  select(where(is.numeric)) %>%
  preProcess(., method = 'bagImpute')

dbcovidbag2 <- dbcovid %>%
  select(where(is.numeric)) %>%
  predict(preprocbag, .)

dbcovidbag2 <- dbcovid %>%
  select(where(is.factor)) %>%
  bind_cols(dbcovidbag2) %>%
  na.omit
sum(is.na(dbcovidbag2)) # check we have no missing data
save(dbcovidbag2, file = 'Rdata/dbcovidbag2.Rda')

#MODELO CON IMPUTACI?N POR MEDIANA
#Particiono la nueva base, usando el misma seed.
set.seed(271220)
indtrainmd <- createDataPartition(y = dbcovidmd2$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setmd2 <- dbcovidmd2[indtrainmd,]
test_setmd2 <- dbcovidmd2[-indtrainmd,]
control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))

set.seed(271220)
fit_rfmd2 <- train(motivoegre ~ .,
                     method = 'rf',
                     ntree = 150,
                     trControl = control_rf,
                     tuneGrid = grid,
                     data = train_setmd2)

model_rfmd2 <- predict(fit_rfmd2, newdata = test_setmd2)
cm3 <- confusionMatrix(data = model_rfmd2, reference = test_setmd2$motivoegre)
cm3
#CONCLUSI?N: MEJORA LA ACCURACY A 0.84 CON ESTE MODELO, EMPLEANDO RANDOM FORESTS

#MODELO POR IMPUTACI?N POR BAGGING
#Particiono la nueva base, usando el misma seed.
set.seed(271220)
indtrainmd <- createDataPartition(y = dbcovidbag2$motivoegre, times = 1, p = 0.85, list = FALSE)
train_setbag2 <- dbcovidbag2[indtrainmd,]
test_setbag2 <- dbcovidbag2[-indtrainmd,]
control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))

set.seed(271220)
fit_rfbag2 <- train(motivoegre ~ .,
                   method = 'rf',
                   ntree = 150,
                   trControl = control_rf,
                   tuneGrid = grid,
                   data = train_setbag2)

model_rfbag2 <- predict(fit_rfbag2, newdata = test_setbag2)
cm4 <- confusionMatrix(data = model_rfbag2, reference = test_setbag2$motivoegre)
cm4
# CONCLUSI?N: NO USAR BAGGING, LA ACCURACY BAJA A .789, IGUAL QUE EN EL MODELO ANTERIOR.

#RENDIMIENTO DEL MODELO 2 CON LA BASE DE VALIDACI?N:

library(tidyverse)
library(caret)

load('Rdata/dbcovidval.Rda')
nombresMod2 <- names(dbcovidmd2)
nombresMod2
dbcovidval2 <- dbcovidval %>%
  select(all_of(nombresMod2))
names(dbcovidval2)
str(dbcovidval2)
dbcovidval2$escolaridad <- as.numeric(as.character(dbcovidval2$escolaridad))
save(dbcovidval2, file = 'Rdata/dbcovidval2.Rda')

#Entreno el modelo, pero ya con toda la base de entrenamiento:
control_rf <- trainControl(method = 'boot',
                           number = 25)
grid <- data.frame(mtry = c(1,5,10,25))

set.seed(271220)
fit_rfmdfin <- train(motivoegre ~ .,
                   method = 'rf',
                   ntree = 150,
                   trControl = control_rf,
                   tuneGrid = grid,
                   data = dbcovidmd2)

model_rfmdfin <- predict(fit_rfmdfin, newdata = dbcovidval2)
cmfin <- confusionMatrix(data = model_rfmdfin, reference = dbcovidval2$motivoegre)
cmfin
save(fit_rfmdfin, file = 'Rdata/fit_rfmdfin.Rda')
save(list = c('test_setmd2', 'train_setmd2', 'test_setbag2',
              'train_setbag2'), file = 'Rdata/dfmodelo2.Rda')


# RENDIMIENTO DE LOS MODELOS glmm, rpart, knn con dbcovidmd2-----------------------------

library(tidyverse)
library(caret)

load('Rdata/dfmodelo2.Rda')
load('Rdata/dbcovidmd2.Rda')

#GMM 
#Data set imputed with the median
tr.control <- trainControl(method = 'boot',
                           number = 25)
set.seed(271220)
fit_glmmd2 <- train(motivoegre ~ .,
                 method = 'glm',
                 data = train_setmd2,
                 trControl = tr.control)

model_glmmd2 <- predict(fit_glmmd2, newdata = test_setmd2, type = 'raw')
cmglmmd2 <- confusionMatrix(data = model_glmmd2, reference = test_setmd2$motivoegre)
cmglmmd2
acc_glmmd2 <- cm$overall[["Accuracy"]]
acc_glmmd2up <- cm$overall[["AccuracyUpper"]]
acc_glmmd2lo <-  cm$overall[["AccuracyLower"]]

#Data set imputed with bagging
# set.seed(271220)
# fit_glm <- train(motivoegre ~ .,
#                  method = 'glm',
#                  data = train_setbag,
#                  trControl = tr.control)
# 
# model_glm <- predict(fit_glm, newdata = test_setbag, type = 'raw')
# cm <- confusionMatrix(data = model_glm, reference = test_setbag$motivoegre)
# acc_glmbag <- cm$overall[["Accuracy"]]
# acc_glmbagup <- cm$overall[["AccuracyUpper"]]
# acc_glmbaglo <-  cm$overall[["AccuracyLower"]]
# 
# varimpglm <- varImp(fit_glm)[["importance"]]
# nomvars <- row.names(varimpglm)
# varimpglm <- varimpglm %>%
#   mutate(varnameglm = nomvars) %>%
#   rename(valueglm = Overall) %>%
#   select(varnameglm, valueglm) %>%
#   arrange(desc(valueglm))

# kNN

#Imputed with median
set.seed(271220)
fit_knnmd2 <- train(motivoegre ~ .,
                 method = 'knn',
                 tuneGrid = data.frame(k = seq(5,35, 1)),
                 trControl = tr.control,
                 data = train_setmd2)

plot(fit_knnmd2, xlab = 'Number of neighbors')
fit_knnmd2$bestTune
model_knnmd2 <- predict(fit_knnmd2, newdata = test_setmd2)
cmknnmd2 <- confusionMatrix(data = model_knnmd2, reference = test_setmd2$motivoegre)
cmknnmd2
acc_knnmd <- cm$overall[["Accuracy"]]
acc_knnmdup <- cm$overall[["AccuracyUpper"]]
acc_knnmdlo <-  cm$overall[["AccuracyLower"]]

#imputed with bagging
# set.seed(271220)
# fit_knn <- train(motivoegre ~ .,
#                  method = 'knn',
#                  tuneGrid = data.frame(k = seq(5,35, 1)),
#                  trControl = tr.control,
#                  data = train_setbag)
# 
# model_knn <- predict(fit_knn, newdata = test_setbag)
# 
# cm <- confusionMatrix(data = model_knn, reference = test_setbag$motivoegre)
# acc_knnbag <- cm$overall[["Accuracy"]]
# acc_knnbagup <- cm$overall[["AccuracyUpper"]]
# acc_knnbaglo <-  cm$overall[["AccuracyLower"]]
# 
# btune_knn <- fit_knn$bestTune
# 
# varimpknn <- varImp(fit_knn)$importance
# nomvars <- row.names(varimpknn)
# varimpknn <- varimpknn %>%
#   mutate(varnameknn = nomvars) %>%
#   rename(valueknn = Dead) %>%
#   select(-Alive) %>%
#   select(varnameknn, valueknn) %>%
#   arrange(desc(valueknn))

# CART

# #Imputed with bagging
# set.seed(271220)
# 
# fit_rpart <- train(motivoegre ~ ., method = 'rpart',
#                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
#                    trControl = tr.control,
#                    data = train_setbag)
# 
# 
# model_rpart <- predict(fit_rpart, newdata = test_setbag)
# cm <- confusionMatrix(data = model_rpart, reference = test_setbag$motivoegre)
# acc_rpartbag <- cm$overall[["Accuracy"]]
# acc_rpartbagup <- cm$overall[["AccuracyUpper"]]
# acc_rpartbaglo <-  cm$overall[["AccuracyLower"]]

#Imputed with the median
set.seed(271220)
fit_rpartmd2 <- train(motivoegre ~ ., method = 'rpart',
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                   trControl = tr.control,
                   data = train_setmd2)

plot(fit_rpartmd2)
fit_rpartmd2$bestTune

plot(fit_rpartmd2$finalModel, margin = 0.1) 
text(fit_rpartmd2$finalModel, cex = 0.75)
model_rpartmd2 <- predict(fit_rpartmd2, newdata = test_setmd2)
cmrpartmd2 <- confusionMatrix(data = model_rpartmd2, reference = test_setmd2$motivoegre)
cmrpartmd2
acc_rpartmd <- cm$overall[["Accuracy"]]
acc_rpartmdup <- cm$overall[["AccuracyUpper"]]
acc_rpartmdlo <-  cm$overall[["AccuracyLower"]]
btune_rpart <-  fit_rpart$bestTune

varimprpart <- varImp(fit_rpart)$importance
nomvars <- row.names(varimprpart)
varimprpart <- varimprpart %>%
  mutate(varnamerpart = nomvars) %>%
  rename(valuerpart = Overall) %>%
  select(varnamerpart, valuerpart) %>%
  arrange(desc(valuerpart))

# RENDIMIENTO DE dbcovidmd2 CON dbcovidval2 -------------------------------

#GLM:
#Creamos el modelo con toda la base:
tr.control <- trainControl(method = 'boot',
                           number = 25)
set.seed(271220)
fit_glmmd2fin <- train(motivoegre ~ .,
                    method = 'glm',
                    data = dbcovidmd2,
                    trControl = tr.control)

model_glmmd2fin <- predict(fit_glmmd2fin, newdata = dbcovidval2, type = 'raw')
cmglmmd2fin <- confusionMatrix(data = model_glmmd2fin, reference = dbcovidval2$motivoegre)
cmglmmd2fin
# CONCLUSI?N: accuracy de 81%, not bad at all

#KNN
# Entrenamos con toda la base
set.seed(271220)
fit_knnmd2fin <- train(motivoegre ~ .,
                    method = 'knn',
                    tuneGrid = data.frame(k = seq(5,35, 1)),
                    trControl = tr.control,
                    data = train_setmd2)

plot(fit_knnmd2fin, xlab = 'Number of neighbors')
fit_knnmd2fin$bestTune
model_knnmd2fin <- predict(fit_knnmd2fin, newdata = dbcovidval2)
cmknnmd2fin <- confusionMatrix(data = model_knnmd2fin, reference = dbcovidval2$motivoegre)
cmknnmd2fin
# CONCLUSI?N: ACCURACY DE 0.68

#RPART
#Usamos toda la base para entrenar
set.seed(271220)
fit_rpartmd2fin <- train(motivoegre ~ ., method = 'rpart',
                      tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                      trControl = tr.control,
                      data = dbcovidmd2)

plot(fit_rpartmd2fin)
fit_rpartmd2fin$bestTune

plot(fit_rpartmd2fin$finalModel, margin = 0.1) 
text(fit_rpartmd2fin$finalModel, cex = 0.75)
model_rpartmd2fin <- predict(fit_rpartmd2fin, newdata = dbcovidval2)
cmrpartmd2fin <- confusionMatrix(data = model_rpartmd2fin, reference = dbcovidval2$motivoegre)
cmrpartmd2fin



#GLM:
#Creamos el modelo con toda la base:
tr.control <- trainControl(method = 'boot',
                           number = 25)
set.seed(271220)
fit_glmmd2fin <- train(motivoegre ~ .,
                       method = 'glm',
                       data = dbcovidmd2,
                       trControl = tr.control)

model_glmmd2fin <- predict(fit_glmmd2fin, newdata = dbcovidval2, type = 'raw')
cmglmmd2fin <- confusionMatrix(data = model_glmmd2fin, reference = dbcovidval2$motivoegre)
cmglmmd2fin
# CONCLUSI?N: accuracy de 81%, not bad at all

#KNN
# Entrenamos con toda la base
set.seed(271220)
fit_knnmd2fin <- train(motivoegre ~ .,
                       method = 'knn',
                       tuneGrid = data.frame(k = seq(5,35, 1)),
                       trControl = tr.control,
                       data = dbcovidmd2)

plot(fit_knnmd2fin, xlab = 'Number of neighbors')
fit_knnmd2fin$bestTune
model_knnmd2fin <- predict(fit_knnmd2fin, newdata = dbcovidval2)
cmknnmd2fin <- confusionMatrix(data = model_knnmd2fin, reference = dbcovidval2$motivoegre)
cmknnmd2fin
# CONCLUSI?N: ACCURACY DE 0.67

#RPART
#Usamos toda la base para entrenar
set.seed(271220)
fit_rpartmd2fin <- train(motivoegre ~ ., method = 'rpart',
                         tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                         trControl = tr.control,
                         data = dbcovidmd2)

plot(fit_rpartmd2fin)
fit_rpartmd2fin$bestTune

plot(fit_rpartmd2fin$finalModel, margin = 0.1) 
text(fit_rpartmd2fin$finalModel, cex = 0.75)
model_rpartmd2fin <- predict(fit_rpartmd2fin, newdata = dbcovidval2)
cmrpartmd2fin <- confusionMatrix(data = model_rpartmd2fin, reference = dbcovidval2$motivoegre)
cmrpartmd2fin

# RENDIMIENTO DE dbcovidbag2 con dbcovidval2 ------------------------------

load('Rdata/dbcovidbag2.Rda')
#GLM:
#Creamos el modelo con toda la base:
tr.control <- trainControl(method = 'boot',
                           number = 25)
set.seed(271220)
fit_glmbag2fin <- train(motivoegre ~ .,
                       method = 'glm',
                       data = dbcovidbag2,
                       trControl = tr.control)

model_glmbag2fin <- predict(fit_glmbag2fin, newdata = dbcovidval2, type = 'raw')
cmglmbag2fin <- confusionMatrix(data = model_glmbag2fin, reference = dbcovidval2$motivoegre)
cmglmbag2fin
# CONCLUSI?N: accuracy de 81%, not bad at all

#KNN
# Entrenamos con toda la base
set.seed(271220)
fit_knnbag2fin <- train(motivoegre ~ .,
                       method = 'knn',
                       tuneGrid = data.frame(k = seq(5,35, 1)),
                       trControl = tr.control,
                       data = dbcovidbag2)

plot(fit_knnbag2fin, xlab = 'Number of neighbors')
fit_knnbag2fin$bestTune
model_knnbag2fin <- predict(fit_knnbag2fin, newdata = dbcovidval2)
cmknnbag2fin <- confusionMatrix(data = model_knnbag2fin, reference = dbcovidval2$motivoegre)
cmknnbag2fin
# CONCLUSI?N: ACCURACY DE 0.68

#RPART
#Usamos toda la base para entrenar
set.seed(271220)
fit_rpartbag2fin <- train(motivoegre ~ ., method = 'rpart',
                         tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                         trControl = tr.control,
                         data = dbcovidbag2)

plot(fit_rpartbag2fin)
fit_rpartbag2fin$bestTune

plot(fit_rpartbag2fin$finalModel, margin = 0.1) 
text(fit_rpartbag2fin$finalModel, cex = 0.75)
model_rpartbag2fin <- predict(fit_rpartbag2fin, newdata = dbcovidval2)
cmrpartbag2fin <- confusionMatrix(data = model_rpartbag2fin, reference = dbcovidval2$motivoegre)
cmrpartbag2fin
# CONCLUSI?N: ACCURACY DE 0.78



# GR?FICA DE CORRELACI?N --------------------------------------------------

library(tidyverse)
load('Rdata/dbcovidmd2.Rda')
# Se tom? de un video de youtube, se usa el paquete corrplot
#install.packages('corrplot')
library(corrplot)

str(dbcovidmd2)
dfCorr <- dbcovidmd2 %>%
  select(where(is.numeric))

tbcor <- cor(dfCorr)
corrplot(tbcor, type = 'upper') 

install.packages('metan')
library(metan)
corrl <- corr_coef(dfCorr)
plot(corrl)
