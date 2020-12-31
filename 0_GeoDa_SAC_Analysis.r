## VERSION INFORMATION
## Author: Insang Song (isong@uoregon.edu)
## Last revision: 123120

# to edit '~' as your working directory

setwd('~')
download.file("https://github.com/biogeokim/ml_sac_prediction/raw/main/Supporting_Information_Data_1.RData",
			  "Supporting_Information_Data_1.RData")
download.file("https://github.com/biogeokim/ml_sac_prediction/raw/main/Supporting_Information_Data_1.RData",
			  "Supporting_Information_Data_2.RData")

if (!require(pacman)) { install.packages("pacman") }
library(pacman)
p_load(tidyverse, ggpmisc, spdep, sf, spatialreg, sp, stringr, caret, doSNOW, neuralnet)
p_load(gtable, grid, gridExtra)
load('Supporting_Information_Data_1.RData')
load('Supporting_Information_Data_2.RData')

## Base functions
source('1_Base_MLSAC.r')

## Data preprocessing
source('2_MLSAC_Preprocessing.R')

## List of algorithms to be compared
calg1 <- c('regr.lm', 'regr.nnet', 'regr.svm', 'regr.randomForest')

### Run MLMI ####
## 1. AirBnB
airbnb.mi <- airbnb.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(airbnb, calg1, x, airbnb.xvec, std = TRUE))
## 2. Baltimore
balt.mi <- balt.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(balt, calg1, x, balt.xvec, std = TRUE))

## 3. Boston
bost.mi <- bost.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(bost, calg1, x, bost.xvec, std = TRUE))

## 4. Industry mixes
imix.mi <- imix.lp %>% 
  lapply(function(x) {
    mi <- imix.yvec %>% 
        split(.,.) %>% 
        lapply(function(y) mlr_learn(x, calg1, y, imix.xvec, std = TRUE))
    return(mi)})

## 5. Chicago Health and Socio-Economic
chhs.mi <- chhs.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(chhs, calg1, x, chhs.xvec, std = TRUE))

## 6. Chicago Health Indicator
hein.mi <- hein.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(hein, calg1, x, hein.xvec, std = TRUE))

## 7. Cincinnati Crime
cinc.mi <- cinc.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(cinc, calg1, x, cinc.xvec, std = TRUE))

## 8. Columbus Crime
colu.mi <- colu.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(colu, calg1, x, colu.xvec, std = TRUE))

## 9. Denver Crime
denv.mi <- denv.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(denv.a, calg1, x, denv.xvec, std = TRUE))

## 10. Natregimes (60,70,80,90)
nats60.mi <- nats %>% 
  lapply(function(x){ 
    nats.yvec[grep('*.(60)$', nats.yvec)] %>% 
    split(.,.) %>% 
    lapply(function(y){
      x %>% 
           mlr_learn(., calg1, y, nats.xvec[grep('*.(60)$', nats.xvec)], std = TRUE)})})
nats70.mi <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(70)$', nats.yvec)] %>% 
      split(.,.) %>% 
    lapply(function(y)
      x %>% 
        mlr_learn(., calg1, y, nats.xvec[grep('*.(70)$', nats.xvec)], std = TRUE)))
nats80.mi <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(80)$', nats.yvec)] %>% 
      split(.,.) %>% 
    lapply(function(y)
      x %>% 
        mlr_learn(., calg1, y, nats.xvec[grep('*.(80)$', nats.xvec)], std = TRUE)))
nats90.mi <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(90)$', nats.yvec)] %>% 
      split(.,.) %>% 
    lapply(function(y)
      x %>% 
        mlr_learn(., calg1, y, nats.xvec[grep('*.(90)$', nats.xvec)], std = TRUE)))

## 11. US elections
elec.mi <- c(elec.yvec1, elec.yvec2) %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(elec.sp, calg1, x, elec.xvec, std = TRUE))
## 12. Phoenix ACS
phx.mi <- phx.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(phx, calg1, x, phx.xvec, std = TRUE))
## 13. MSA Employment
msas.mi <- msas.sts %>% 
  lapply(function(x) 
    msas.yvec %>% 
    split(.,.) %>% 
    lapply(function(y) x %>% 
             mlr_learn(., calg1, y, msas.xvec, std = TRUE)))
## 14. New York Education
nyet.mi <- nyet %>% 
  lapply(function(x) 
    nyet.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) 
        x %>% 
          mlr_learn(., calg1, y, nyet.xvec, std = TRUE)))
## 15. New York Unemployment
nyue.mi <- nyue %>% 
  lapply(function(x) 
    nyue.yvec %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg1, y, nyue.xvec, std = TRUE)))

### No resampling (=no tuning) ####
## 1. AirBnB
airbnb.mitr <- airbnb.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(airbnb, calg1, x, airbnb.xvec, std = TRUE))
## 2. Baltimore
balt.mitr <- balt.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(balt, calg1, x, balt.xvec, std = TRUE))

## 3. Boston
bost.mitr <- bost.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(bost, calg1, x, bost.xvec, std = TRUE))

## 4. Industry mixes
imix.mitr <- imix.lp %>% 
  lapply(function(x) {
    mi <- imix.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) mlr_learn_tr(x, calg1, y, imix.xvec, std = TRUE))
    return(mi)})

## 5. Chicago Health and Socio-Economic
chhs.mitr <- chhs.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(chhs, calg1, x, chhs.xvec, std = TRUE))

## 6. Chicago Health Indicator
hein.mitr <- hein.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(hein, calg1, x, hein.xvec, std = TRUE))

## 7. Cincinnati Crime
cinc.mitr <- cinc.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(cinc, calg1, x, cinc.xvec, std = TRUE))

## 8. Columbus Crime
colu.mitr <- colu.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(colu, calg1, x, colu.xvec, std = TRUE))

## 9. Denver Crime
denv.mitr <- denv.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(denv.a, calg1, x, denv.xvec, std = TRUE))

## 10. Natregimes (60,70,80,90)
nats60.mitr <- nats %>% 
  lapply(function(x){ 
    nats.yvec[grep('*.(60)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y){
        x %>% 
          mlr_learn_tr(., calg1, y, nats.xvec[grep('*.(60)$', nats.xvec)], std = TRUE)})})
nats70.mitr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(70)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn_tr(., calg1, y, nats.xvec[grep('*.(70)$', nats.xvec)], std = TRUE)))
nats80.mitr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(80)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn_tr(., calg1, y, nats.xvec[grep('*.(80)$', nats.xvec)], std = TRUE)))
nats90.mitr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(90)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn_tr(., calg1, y, nats.xvec[grep('*.(90)$', nats.xvec)], std = TRUE)))

## 11. US elections
elec.mitr <- c(elec.yvec1, elec.yvec2) %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(elec.sp, calg1, x, elec.xvec, std = TRUE))
## 12. Phoenix ACS
phx.mitr <- phx.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn_tr(phx, calg1, x, phx.xvec, std = TRUE))
## 13. MSA Employment
msas.mitr <- msas.sts %>% 
  lapply(function(x) 
    msas.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) x %>% 
               mlr_learn_tr(., calg1, y, msas.xvec, std = TRUE)))
## 14. New York Education
nyet.mitr <- nyet %>% 
  lapply(function(x) 
    nyet.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) 
        x %>% 
          mlr_learn_tr(., calg1, y, nyet.xvec, std = TRUE)))
## 15. New York Unemployment
nyue.mitr <- nyue %>% 
  lapply(function(x) 
    nyue.yvec %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn_tr(., calg1, y, nyue.xvec, std = TRUE)))






## Sensitivity analysis 1: SpCV ####
## 1. AirBnB
airbnb.mi.spcv <- airbnb.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(airbnb, calg1, x, airbnb.xvec, cvmethod = 'SpCV', ncv = 10, std = TRUE))
## 2. Baltimore
balt.mi.spcv <- balt.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(balt, calg1, x, balt.xvec, cvmethod = 'SpCV', ncv= 10, std = TRUE))

## 3. Boston
bost.mi.spcv <- bost.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(bost, calg1, x, bost.xvec, cvmethod = 'SpCV', std = TRUE))

## 4. Industry mixes
imix.mi.spcv <- imix.lp %>% 
  lapply(function(x) {
    mi <- imix.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) mlr_learn(x, calg1, y, imix.xvec, cvmethod = 'SpCV', std = TRUE))
    return(mi)})

## 5. Chicago Health and Socio-Economic
chhs.mi.spcv <- chhs.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(chhs, calg1, x, chhs.xvec, cvmethod = 'SpCV', std = TRUE))

## 6. Chicago Health Indicator
hein.mi.spcv <- hein.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(hein, calg1, x, hein.xvec, cvmethod = 'SpCV', std = TRUE))

## 7. Cincinnati Crime
cinc.mi.spcv <- cinc.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(cinc, calg1, x, cinc.xvec, cvmethod = 'SpCV', std = TRUE))

## 8. Columbus Crime
colu.mi.spcv <- colu.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(colu, calg1, x, colu.xvec, cvmethod = 'SpCV', std = TRUE))

## 9. Denver Crime
denv.mi.spcv <- denv.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(denv.a, calg1, x, denv.xvec, cvmethod = 'SpCV', std = TRUE))

## 10. Natregimes (60,70,80,90)
nats60.mi.spcv <- nats %>% 
  lapply(function(x){ 
    nats.yvec[grep('*.(60)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y){
        x %>% 
          mlr_learn(., calg1, y, nats.xvec[grep('*.(60)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)})})
nats70.mi.spcv <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(70)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg1, y, nats.xvec[grep('*.(70)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))
nats80.mi.spcv <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(80)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg1, y, nats.xvec[grep('*.(80)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))
nats90.mi.spcv <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(90)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg1, y, nats.xvec[grep('*.(90)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))

## 11. US elections
elec.mi.spcv <- c(elec.yvec1, elec.yvec2) %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(elec.sp, calg1, x, elec.xvec, cvmethod = 'SpCV', std = TRUE))
## 12. Phoenix ACS
phx.mi.spcv <- phx.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(phx, calg1, x, phx.xvec, cvmethod = 'SpCV', std = TRUE))
## 13. MSA Employment
msas.mi.spcv <- msas.sts %>% 
  lapply(function(x) 
    msas.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) x %>% 
               mlr_learn(., calg1, y, msas.xvec, cvmethod = 'SpCV', std = TRUE)))
## 14. New York Education
nyet.mi.spcv <- nyet %>% 
  lapply(function(x) 
    nyet.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) 
        x %>% 
          mlr_learn(., calg1, y, nyet.xvec, cvmethod = 'SpCV', std = TRUE)))
## 15. New York Unemployment
nyue.mi.spcv <- nyue %>% 
  lapply(function(x) 
    nyue.yvec %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg1, y, nyue.xvec, cvmethod = 'SpCV', std = TRUE)))


# Sensitivity Analysis 2: Regularized algorithms ####
## 1. AirBnB
airbnb.mi.spr <- airbnb.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(airbnb, calg2, x, airbnb.xvec, cvmethod = 'SpCV', ncv = 10, std = TRUE))
## 2. Baltimore
balt.mi.spr <- balt.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(balt, calg2, x, balt.xvec, cvmethod = 'SpCV', ncv= 10, std = TRUE))

## 3. Boston
bost.mi.spr <- bost.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(bost, calg2, x, bost.xvec, cvmethod = 'SpCV', std = TRUE))

## 4. Industry mixes
imix.mi.spr <- imix.lp %>% 
  lapply(function(x) {
    mi <- imix.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) mlr_learn(x, calg2, y, imix.xvec, cvmethod = 'SpCV', std = TRUE))
    return(mi)})

## 5. Chicago Health and Socio-Economic
chhs.mi.spr <- chhs.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(chhs, calg2, x, chhs.xvec, cvmethod = 'SpCV', std = TRUE))

## 6. Chicago Health Indicator
hein.mi.spr <- hein.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(hein, calg2, x, hein.xvec, cvmethod = 'SpCV', std = TRUE))

## 7. Cincinnati Crime
cinc.mi.spr <- cinc.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(cinc, calg2, x, cinc.xvec, cvmethod = 'SpCV', std = TRUE))

## 8. Columbus Crime
colu.mi.spr <- colu.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(colu, calg2, x, colu.xvec, cvmethod = 'SpCV', std = TRUE))

## 9. Denver Crime
denv.mi.spr <- denv.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(denv.a, calg2, x, denv.xvec, cvmethod = 'SpCV', std = TRUE))

## 10. Natregimes (60,70,80,90)
nats60.mi.spr <- nats %>% 
  lapply(function(x){ 
    nats.yvec[grep('*.(60)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y){
        x %>% 
          mlr_learn(., calg2, y, nats.xvec[grep('*.(60)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)})})
nats70.mi.spr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(70)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg2, y, nats.xvec[grep('*.(70)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))
nats80.mi.spr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(80)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg2, y, nats.xvec[grep('*.(80)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))
nats90.mi.spr <- nats %>% 
  lapply(function(x) 
    nats.yvec[grep('*.(90)$', nats.yvec)] %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg2, y, nats.xvec[grep('*.(90)$', nats.xvec)], cvmethod = 'SpCV', std = TRUE)))

## 11. US elections
elec.mi.spr <- c(elec.yvec1, elec.yvec2) %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(elec.sp, calg2, x, elec.xvec, cvmethod = 'SpCV', std = TRUE))
## 12. Phoenix ACS
phx.mi.spr <- phx.yvec %>% 
  split(.,.) %>% 
  lapply(function(x) mlr_learn(phx, calg2, x, phx.xvec, cvmethod = 'SpCV', std = TRUE))
## 13. MSA Employment
msas.mi.spr <- msas.sts %>% 
  lapply(function(x) 
    msas.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) x %>% 
               mlr_learn(., calg2, y, msas.xvec, cvmethod = 'SpCV', std = TRUE)))
## 14. New York Education
nyet.mi.spr <- nyet %>% 
  lapply(function(x) 
    nyet.yvec %>% 
      split(.,.) %>% 
      lapply(function(y) 
        x %>% 
          mlr_learn(., calg2, y, nyet.xvec, cvmethod = 'SpCV', std = TRUE)))
## 15. New York Unemployment
nyue.mi.spr <- nyue %>% 
  lapply(function(x) 
    nyue.yvec %>% 
      split(.,.) %>% 
      lapply(function(y)
        x %>% 
          mlr_learn(., calg2, y, nyue.xvec, cvmethod = 'SpCV', std = TRUE)))



# 1
airbnb.mi.res <- airbnb.mitr %>% extr_modelres(., airbnb.yvec, calg1, neighbor = airbnb %>% poly2nb) %>% 
  mutate(Dataset = 'AirBnB')
# 2
balt.mi.res <- balt.mitr %>% extr_modelres(., balt.yvec, neighbor = balt %>% knearneigh(k = 30) %>% knn2nb) %>% 
  mutate(Dataset = 'Baltimore housing')
# 3
bost.mi.res <- bost.mitr %>% extr_modelres(., bost.yvec, neighbor = bost %>% knearneigh(k = 50) %>% knn2nb) %>% 
  mutate(Dataset = 'Boston housing')
# 4
imix.names <- c('Charleston', 'Hickory', 'Lansing', 'Orlando', 'Sacramento', 'Seattle') %>% 
  str_c(., 'MSA')
imix.mi.resl <- imix.mitr %>% 
  mapply(function(x, y) extr_modelres(x, imix.yvec, neighbor = y %>% poly2nb),
         ., imix.ll, SIMPLIFY = FALSE)
imix.mi.res <- imix.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(MSA = y), ., imix.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Industry Mix')
#5
chhs.mi.res <- chhs.mitr %>% 
  extr_modelres(., chhs.yvec, neighbor = chhs %>% poly2nb) %>% 
  mutate(Dataset = 'Chicago Health and Socio-economic')
#6
hein.mi.res <- hein.mitr %>% extr_modelres(., hein.yvec, neighbor = hein %>% poly2nb) %>% 
  mutate(Dataset = 'Chicago Health Indicators')
#7
cinc.mi.res <- cinc.mitr %>% extr_modelres(., cinc.yvec, neighbor = cinc %>% poly2nb) %>% 
  mutate(Dataset = 'Cincinnati crime')
#8
colu.mi.res <- colu.mitr %>% extr_modelres(., colu.yvec, neighbor = colu %>% poly2nb) %>% 
  mutate(Dataset = 'Columbus crime')
#9
denv.mi.res <- denv.mitr %>% extr_modelres(., denv.yvec, neighbor = denv %>% poly2nb) %>% 
  mutate(Dataset = 'Denver crime')
#10
nats.names <- names(nats60.mitr)

nats60.mi.resl <- nats60.mitr %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[1], neighbor = y %>% poly2nb), ., nats, SIMPLIFY = FALSE)
nats70.mi.resl <- nats70.mitr %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[2], neighbor = y %>% poly2nb), ., nats, SIMPLIFY = FALSE)
nats80.mi.resl <- nats80.mitr %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[3], neighbor = y %>% poly2nb), ., nats, SIMPLIFY = FALSE)
nats90.mi.resl <- nats90.mitr %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[4], neighbor = y %>% poly2nb), ., nats, SIMPLIFY = FALSE)

nats60.mi.res <- nats60.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1960')
nats70.mi.res <- nats70.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1970')
nats80.mi.res <- nats80.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1980')
nats90.mi.res <- nats90.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1990')

#11
elec.mi.res <- elec.mitr %>% 
  extr_modelres(., c(elec.yvec1, elec.yvec2), neighbor = elec.sp %>% poly2nb) %>% 
  mutate(Dataset = 'US elections')
#12
phx.mi.res <- phx.mitr %>% extr_modelres(., phx.yvec, neighbor = phx %>% poly2nb) %>% 
  mutate(Dataset = 'Phoenix ACS')
#13
msas.names <- msas %>% names %>% str_split_fixed(., '/', n = 6) %>% .[,4]
msas.mi01.res <- msas.mitr[[1]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[1]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[1]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi02.res <- msas.mitr[[2]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[2]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[2]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi03.res <- msas.mitr[[3]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[3]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[3]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi04.res <- msas.mitr[[4]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[4]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[4]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi05.res <- msas.mitr[[5]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[5]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[5]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi06.res <- msas.mitr[[6]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[6]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[6]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi07.res <- msas.mitr[[7]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[7]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[7]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi08.res <- msas.mitr[[8]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[8]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[8]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mi09.res <- msas.mitr[[9]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[9]] %>% poly2nb) %>% 
  mutate(MSA = msas.names[9]) %>% 
  mutate(Dataset = 'MSA Employment')
#14
nyet.names <- nyet %>% names
nyet.mi1.res <- nyet.mitr[[1]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[1]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York education')
nyet.mi2.res <- nyet.mitr[[2]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[2]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York education')
nyet.mi3.res <- nyet.mitr[[3]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[3]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York education')
nyet.mi4.res <- nyet.mitr[[4]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[4]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York education')
nyet.mi5.res <- nyet.mitr[[5]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[5]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York education')
#15
nyue.mi1.res <- nyue.mitr[[1]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[1]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mi2.res <- nyue.mitr[[2]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[2]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mi3.res <- nyue.mitr[[3]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[3]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mi4.res <- nyue.mitr[[4]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[4]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mi5.res <- nyue.mitr[[5]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[5]] %>% poly2nb) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York unemployment')


mi_res_sf_mi <- mi_res_sf %>% 
  filter(Model == 'SpFilter') %>% 
  dplyr::select(YVAR, MoranI, Dataset, MSA, State, Neigh) 
mi_res_sf_vals <- mi_res_sf %>% 
  dplyr::select(YVAR, Model, Dataset, MSA, State, Neigh, RMSE, MAE, MAPE, NRMSE, res.MI) %>% 
  filter(Model == 'SpFilter') %>% 
  pivot_longer(cols = RMSE:res.MI, names_to = 'measure', values_to = 'value')

# Combine results (Rsquared, etc.)
rsq.res <- bind_rows(
  airbnb.mi.res %>% mutate(NX = length(airbnb.xvec), N = nrow(airbnb)), 
  balt.mi.res %>% mutate(NX = length(balt.xvec), N = nrow(balt)), 
  bost.mi.res %>% mutate(NX = length(bost.xvec), N = nrow(bost)),  
  imix.mi.res %>% mutate(NX = length(imix.xvec), N = nrow(airbnb)), 
  chhs.mi.res %>% mutate(NX = length(chhs.xvec), N = nrow(chhs)),  
  hein.mi.res %>% mutate(NX = length(hein.xvec), N = nrow(hein)),  
  cinc.mi.res %>% mutate(NX = length(cinc.xvec), N = nrow(cinc)),  
  colu.mi.res %>% mutate(NX = length(colu.xvec), N = nrow(colu)), 
  denv.mi.res %>% mutate(NX = length(denv.xvec), N = nrow(denv)),  
  nats60.mi.res %>% mutate(NX = length(nats.xvec)/4),
  nats70.mi.res %>% mutate(NX = length(nats.xvec)/4),
  nats80.mi.res %>% mutate(NX = length(nats.xvec)/4), 
  nats90.mi.res %>% mutate(NX = length(nats.xvec)/4), 
  elec.mi.res %>% mutate(NX = length(elec.xvec), N = nrow(elec)), 
  phx.mi.res %>% mutate(NX = length(phx.xvec), N = nrow(phx)),
  msas.mi01.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[1]])), 
  msas.mi02.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[2]])), 
  msas.mi03.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[3]])), 
  msas.mi04.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[4]])), 
  msas.mi05.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[5]])), 
  msas.mi06.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[6]])), 
  msas.mi07.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[7]])), 
  msas.mi08.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[8]])), 
  msas.mi09.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[9]])),
  nyet.mi1.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[1]])), 
  nyet.mi2.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[2]])), 
  nyet.mi3.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[3]])), 
  nyet.mi4.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[4]])), 
  nyet.mi5.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[5]])),
  nyue.mi1.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[1]])), 
  nyue.mi2.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[2]])), 
  nyue.mi3.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[3]])), 
  nyue.mi4.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[4]])), 
  nyue.mi5.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[5]]))
) %>% 
  bind_rows(mi_res_sf_vals) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  left_join(mi_res_sf_mi, by = c('Dataset', 'YVAR', 'MSA', 'State', 'Neigh')) %>% 
  mutate(Dataset = str_replace(Dataset, 'Natregimes.*', 'Natregimes'))

viewtable <- data.frame(
  from = unique(rsq.result$Dataset),
  to = c('Etc', 'Housing', 'Housing', 'Unemployment', 'Health', 'Health', 'Crime', 'Crime', 'Crime', 'Crime', 'Etc', 'Etc', 'Unemployment', 'Etc', 'Unemployment')
)



nats.nrows <- nats %>% 
  lapply(function(x) data.frame(State = unique(x$STATE_NAME), N1 = nrow(x))) %>% 
  do.call(bind_rows,.)


rsq.result <- rsq.res %>% 
  filter(is.na(MSA) | !(MSA == 'HickoryMSA' & Dataset == 'Industry Mix')) %>% 
  left_join(nats.nrows) %>% 
  mutate(N = ifelse(grepl('^(Natregimes).*', Dataset), N1, N)) %>% 
  dplyr::select(-N1) %>% 
  group_by(Dataset, MSA, State, Neigh) %>% 
  mutate(N = unique(N[which(!is.na(N))]),
         NX = unique(NX[which(!is.na(NX))])) %>% 
  ungroup %>% 
  mutate(Ngroup = cut(N, c(0, 100, 1000, max(N))),
         NXgroup = cut(NX, c(0, 5, 10, 13)),
         Model = plyr::mapvalues(Model, c('lm', 'nnet', 'randomForest', 'SpFilter', 'svm'),
                                 c('OLS', 'ANN', 'RF', 'SF', 'SVM')),
         Model = factor(Model, levels = c('ANN', 'RF', 'SVM', 'SF', 'OLS')),
         Field = plyr::mapvalues(Dataset, viewtable$from, viewtable$to))


## CV Results ####
# 1
airbnb.micv.res <- airbnb.mi %>% extr_modelres(., airbnb.yvec, calg1, neighbor = airbnb %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'AirBnB')
# 2
balt.micv.res <- balt.mi %>% extr_modelres(., balt.yvec, neighbor = balt %>% knearneigh(k = 30) %>% knn2nb, mode = 2) %>% 
  mutate(Dataset = 'Baltimore housing')
# 3
bost.micv.res <- bost.mi %>% extr_modelres(., bost.yvec, neighbor = bost %>% knearneigh(k = 50) %>% knn2nb, mode = 2) %>% 
  mutate(Dataset = 'Boston housing')
# 4
imix.names <- c('Charleston', 'Hickory', 'Lansing', 'Orlando', 'Sacramento', 'Seattle') %>% 
  str_c(., 'MSA')
imix.micv.resl <- imix.mi %>% 
  mapply(function(x, y) extr_modelres(x, imix.yvec, neighbor = y %>% poly2nb, mode = 2),
         ., imix.ll, SIMPLIFY = FALSE)
imix.micv.res <- imix.micv.resl %>% 
  mapply(function(x, y) x %>% mutate(MSA = y), ., imix.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Industry Mix')
#5
chhs.micv.res <- chhs.mi %>% 
  extr_modelres(., chhs.yvec, neighbor = chhs %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Chicago Health and Socio-economic')
#6
hein.micv.res <- hein.mi %>% extr_modelres(., hein.yvec, neighbor = hein %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Chicago Health Indicators')
#7
cinc.micv.res <- cinc.mi %>% extr_modelres(., cinc.yvec, neighbor = cinc %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Cincinnati crime')
#8
colu.micv.res <- colu.mi %>% extr_modelres(., colu.yvec, neighbor = colu %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Columbus crime')
#9
denv.micv.res <- denv.mi %>% extr_modelres(., denv.yvec, neighbor = denv %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Denver crime')
#10
nats.names <- names(nats60.mi)

nats60.micv.resl <- nats60.mi %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[1], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats70.micv.resl <- nats70.mi %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[2], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats80.micv.resl <- nats80.mi %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[3], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats90.micv.resl <- nats90.mi %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[4], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)

nats60.micv.res <- nats60.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1960')
nats70.micv.res <- nats70.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1970')
nats80.micv.res <- nats80.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1980')
nats90.micv.res <- nats90.mi.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1990')

#11
elec.micv.res <- elec.mi %>% 
  extr_modelres(., c(elec.yvec1, elec.yvec2), neighbor = elec.sp %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'US elections')
#12
phx.micv.res <- phx.mi %>% extr_modelres(., phx.yvec, neighbor = phx %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Phoenix ACS')
#13
msas.names <- msas %>% names %>% str_split_fixed(., '/', n = 6) %>% .[,4]
msas.micv01.res <- msas.mi[[1]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[1]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv02.res <- msas.mi[[2]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[2]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv03.res <- msas.mi[[3]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[3]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv04.res <- msas.mi[[4]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[4]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv05.res <- msas.mi[[5]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[5]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv06.res <- msas.mi[[6]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[6]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[6]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv07.res <- msas.mi[[7]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[7]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[7]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv08.res <- msas.mi[[8]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[8]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[8]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.micv09.res <- msas.mi[[9]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[9]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[9]) %>% 
  mutate(Dataset = 'MSA Employment')
#14
nyet.names <- nyet %>% names
nyet.micv1.res <- nyet.mi[[1]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York education')
nyet.micv2.res <- nyet.mi[[2]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York education')
nyet.micv3.res <- nyet.mi[[3]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York education')
nyet.micv4.res <- nyet.mi[[4]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York education')
nyet.micv5.res <- nyet.mi[[5]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York education')
#15
nyue.micv1.res <- nyue.mi[[1]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.micv2.res <- nyue.mi[[2]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.micv3.res <- nyue.mi[[3]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.micv4.res <- nyue.mi[[4]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.micv5.res <- nyue.mi[[5]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York unemployment')


mi_res_sf_mi <- mi_res_sf %>% 
  filter(Model == 'SpFilter') %>% 
  dplyr::select(YVAR, MoranI, Dataset, MSA, State, Neigh) 
mi_res_sf_vals <- mi_res_sf %>% 
  dplyr::select(YVAR, Model, Dataset, MSA, State, Neigh, RMSE, MAE, MAPE, NRMSE, res.MI) %>% 
  filter(Model == 'SpFilter') %>% 
  pivot_longer(cols = RMSE:res.MI, names_to = 'measure', values_to = 'value')

# Combine results (Rsquared, etc.)
rsq.cvres <- bind_rows(
  airbnb.micv.res %>% mutate(NX = length(airbnb.xvec), N = nrow(airbnb)), 
  balt.micv.res %>% mutate(NX = length(balt.xvec), N = nrow(balt)), 
  bost.micv.res %>% mutate(NX = length(bost.xvec), N = nrow(bost)),  
  imix.micv.res %>% mutate(NX = length(imix.xvec), N = nrow(airbnb)), 
  chhs.micv.res %>% mutate(NX = length(chhs.xvec), N = nrow(chhs)),  
  hein.micv.res %>% mutate(NX = length(hein.xvec), N = nrow(hein)),  
  cinc.micv.res %>% mutate(NX = length(cinc.xvec), N = nrow(cinc)),  
  colu.micv.res %>% mutate(NX = length(colu.xvec), N = nrow(colu)), 
  denv.micv.res %>% mutate(NX = length(denv.xvec), N = nrow(denv)),  
  nats60.micv.res %>% mutate(NX = length(nats.xvec)/4),
  nats70.micv.res %>% mutate(NX = length(nats.xvec)/4),
  nats80.micv.res %>% mutate(NX = length(nats.xvec)/4), 
  nats90.micv.res %>% mutate(NX = length(nats.xvec)/4), 
  elec.micv.res %>% mutate(NX = length(elec.xvec), N = nrow(elec)), 
  phx.micv.res %>% mutate(NX = length(phx.xvec), N = nrow(phx)),
  msas.micv01.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[1]])), 
  msas.micv02.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[2]])), 
  msas.micv03.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[3]])), 
  msas.micv04.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[4]])), 
  msas.micv05.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[5]])), 
  msas.micv06.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[6]])), 
  msas.micv07.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[7]])), 
  msas.micv08.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[8]])), 
  msas.micv09.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[9]])),
  nyet.micv1.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[1]])), 
  nyet.micv2.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[2]])), 
  nyet.micv3.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[3]])), 
  nyet.micv4.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[4]])), 
  nyet.micv5.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[5]])),
  nyue.micv1.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[1]])), 
  nyue.micv2.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[2]])), 
  nyue.micv3.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[3]])), 
  nyue.micv4.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[4]])), 
  nyue.micv5.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[5]]))
) %>% 
  bind_rows(mi_res_sf_vals) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  left_join(mi_res_sf_mi, by = c('Dataset', 'YVAR', 'MSA', 'State', 'Neigh')) %>% 
  mutate(Dataset = str_replace(Dataset, 'Natregimes.*', 'Natregimes'))

nats.nrows <- nats %>% 
  lapply(function(x) data.frame(State = unique(x$STATE_NAME), N1 = nrow(x))) %>% 
  do.call(bind_rows,.)




rsq.cvresult <- rsq.cvres %>% 
  filter(is.na(MSA) | !(MSA == 'HickoryMSA' & Dataset == 'Industry Mix')) %>% 
  left_join(nats.nrows) %>% 
  mutate(N = ifelse(grepl('^(Natregimes).*', Dataset), N1, N)) %>% 
  dplyr::select(-N1) %>% 
  group_by(Dataset, MSA, State, Neigh) %>% 
  mutate(N = unique(N[which(!is.na(N))]),
         NX = unique(NX[which(!is.na(NX))])) %>% 
  ungroup %>% 
  mutate(Ngroup = cut(N, c(0, 100, 1000, max(N))),
         NXgroup = cut(NX, c(0, 5, 10, 13)),
         Model = plyr::mapvalues(Model, c('lm', 'nnet', 'randomForest', 'SpFilter', 'svm'),
                                 c('OLS', 'ANN', 'RF', 'SF', 'SVM')),
         Model = factor(Model, levels = c('ANN', 'RF', 'SVM', 'SF', 'OLS')),
         Field = plyr::mapvalues(Dataset, viewtable$from, viewtable$to))



## SpCV Results ####
# 1
airbnb.mispcv.res <- airbnb.mi.spcv %>% extr_modelres(., airbnb.yvec, calg1, neighbor = airbnb %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'AirBnB')
# 2
balt.mispcv.res <- balt.mi.spcv %>% extr_modelres(., balt.yvec, neighbor = balt %>% knearneigh(k = 30) %>% knn2nb, mode = 2) %>% 
  mutate(Dataset = 'Baltimore housing')
# 3
bost.mispcv.res <- bost.mi.spcv %>% extr_modelres(., bost.yvec, neighbor = bost %>% knearneigh(k = 50) %>% knn2nb, mode = 2) %>% 
  mutate(Dataset = 'Boston housing')
# 4
imix.names <- c('Charleston', 'Hickory', 'Lansing', 'Orlando', 'Sacramento', 'Seattle') %>% 
  str_c(., 'MSA')
imix.mispcv.resl <- imix.mi.spcv %>% 
  mapply(function(x, y) extr_modelres(x, imix.yvec, neighbor = y %>% poly2nb, mode = 2),
         ., imix.ll, SIMPLIFY = FALSE)
imix.mispcv.res <- imix.mispcv.resl %>% 
  mapply(function(x, y) x %>% mutate(MSA = y), ., imix.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Industry Mix')
#5
chhs.mispcv.res <- chhs.mi.spcv %>% 
  extr_modelres(., chhs.yvec, neighbor = chhs %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Chicago Health and Socio-economic')
#6
hein.mispcv.res <- hein.mi.spcv %>% extr_modelres(., hein.yvec, neighbor = hein %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Chicago Health Indicators')
#7
cinc.mispcv.res <- cinc.mi.spcv %>% extr_modelres(., cinc.yvec, neighbor = cinc %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Cincinnati crime')
#8
colu.mispcv.res <- colu.mi.spcv %>% extr_modelres(., colu.yvec, neighbor = colu %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Columbus crime')
#9
denv.mispcv.res <- denv.mi.spcv %>% extr_modelres(., denv.yvec, neighbor = denv %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Denver crime')
#10
nats.names <- names(nats60.mi)

nats60.mispcv.resl <- nats60.mi.spcv %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[1], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats70.mispcv.resl <- nats70.mi.spcv %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[2], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats80.mispcv.resl <- nats80.mi.spcv %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[3], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)
nats90.mispcv.resl <- nats90.mi.spcv %>% 
  mapply(function(x, y) extr_modelres(x, nats.yvec[4], neighbor = y %>% poly2nb, mode = 2), ., nats, SIMPLIFY = FALSE)

nats60.mispcv.res <- nats60.mispcv.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1960')
nats70.mispcv.res <- nats70.mispcv.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1970')
nats80.mispcv.res <- nats80.mispcv.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1980')
nats90.mispcv.res <- nats90.mispcv.resl %>% 
  mapply(function(x, y) x %>% mutate(State = y), ., nats.names %>% split(.,.), SIMPLIFY = FALSE) %>% 
  do.call(bind_rows, .) %>% 
  mutate(Dataset = 'Natregimes 1990')

#11
elec.mispcv.res <- elec.mi.spcv %>% 
  extr_modelres(., c(elec.yvec1, elec.yvec2), neighbor = elec.sp %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'US elections')
#12
phx.mispcv.res <- phx.mi.spcv %>% extr_modelres(., phx.yvec, neighbor = phx %>% poly2nb, mode = 2) %>% 
  mutate(Dataset = 'Phoenix ACS')
#13
msas.names <- msas %>% names %>% str_split_fixed(., '/', n = 6) %>% .[,4]
msas.mispcv01.res <- msas.mi.spcv[[1]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[1]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv02.res <- msas.mi.spcv[[2]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[2]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv03.res <- msas.mi.spcv[[3]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[3]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv04.res <- msas.mi.spcv[[4]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[4]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv05.res <- msas.mi.spcv[[5]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[5]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv06.res <- msas.mi.spcv[[6]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[6]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[6]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv07.res <- msas.mi.spcv[[7]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[7]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[7]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv08.res <- msas.mi.spcv[[8]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[8]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[8]) %>% 
  mutate(Dataset = 'MSA Employment')
msas.mispcv09.res <- msas.mi.spcv[[9]] %>% 
  extr_modelres(., msas.yvec, neighbor = msas[[9]] %>% poly2nb, mode = 2) %>% 
  mutate(MSA = msas.names[9]) %>% 
  mutate(Dataset = 'MSA Employment')
#14
nyet.names <- nyet %>% names
nyet.mispcv1.res <- nyet.mi.spcv[[1]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York education')
nyet.mispcv2.res <- nyet.mi.spcv[[2]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York education')
nyet.mispcv3.res <- nyet.mi.spcv[[3]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York education')
nyet.mispcv4.res <- nyet.mi.spcv[[4]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York education')
nyet.mispcv5.res <- nyet.mi.spcv[[5]] %>% 
  extr_modelres(., nyet.yvec, neighbor = nyet[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York education')
#15
nyue.mispcv1.res <- nyue.mi.spcv[[1]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[1]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[1]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mispcv2.res <- nyue.mi.spcv[[2]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[2]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[2]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mispcv3.res <- nyue.mi.spcv[[3]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[3]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[3]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mispcv4.res <- nyue.mi.spcv[[4]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[4]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[4]) %>% 
  mutate(Dataset = 'New York unemployment')
nyue.mispcv5.res <- nyue.mi.spcv[[5]] %>% 
  extr_modelres(., nyue.yvec, neighbor = nyue[[5]] %>% poly2nb, mode = 2) %>% 
  mutate(Neigh = nyet.names[5]) %>% 
  mutate(Dataset = 'New York unemployment')


mi_res_sf_mi <- mi_res_sf %>% 
  filter(Model == 'SpFilter') %>% 
  dplyr::select(YVAR, MoranI, Dataset, MSA, State, Neigh) 
mi_res_sf_vals <- mi_res_sf %>% 
  dplyr::select(YVAR, Model, Dataset, MSA, State, Neigh, RMSE, MAE, MAPE, NRMSE, res.MI) %>% 
  filter(Model == 'SpFilter') %>% 
  pivot_longer(cols = RMSE:res.MI, names_to = 'measure', values_to = 'value')

# Combine results (Rsquared, etc.)
rsq.spcvres <- bind_rows(
  airbnb.mispcv.res %>% mutate(NX = length(airbnb.xvec), N = nrow(airbnb)), 
  balt.mispcv.res %>% mutate(NX = length(balt.xvec), N = nrow(balt)), 
  bost.mispcv.res %>% mutate(NX = length(bost.xvec), N = nrow(bost)),  
  imix.mispcv.res %>% mutate(NX = length(imix.xvec), N = nrow(airbnb)), 
  chhs.mispcv.res %>% mutate(NX = length(chhs.xvec), N = nrow(chhs)),  
  hein.mispcv.res %>% mutate(NX = length(hein.xvec), N = nrow(hein)),  
  cinc.mispcv.res %>% mutate(NX = length(cinc.xvec), N = nrow(cinc)),  
  colu.mispcv.res %>% mutate(NX = length(colu.xvec), N = nrow(colu)), 
  denv.mispcv.res %>% mutate(NX = length(denv.xvec), N = nrow(denv)),  
  nats60.mispcv.res %>% mutate(NX = length(nats.xvec)/4),
  nats70.mispcv.res %>% mutate(NX = length(nats.xvec)/4),
  nats80.mispcv.res %>% mutate(NX = length(nats.xvec)/4), 
  nats90.mispcv.res %>% mutate(NX = length(nats.xvec)/4), 
  elec.mispcv.res %>% mutate(NX = length(elec.xvec), N = nrow(elec)), 
  phx.mispcv.res %>% mutate(NX = length(phx.xvec), N = nrow(phx)),
  msas.mispcv01.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[1]])), 
  msas.mispcv02.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[2]])), 
  msas.mispcv03.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[3]])), 
  msas.mispcv04.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[4]])), 
  msas.mispcv05.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[5]])), 
  msas.mispcv06.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[6]])), 
  msas.mispcv07.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[7]])), 
  msas.mispcv08.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[8]])), 
  msas.mispcv09.res %>% mutate(NX = length(msas.xvec), N = nrow(msas.sts[[9]])),
  nyet.mispcv1.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[1]])), 
  nyet.mispcv2.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[2]])), 
  nyet.mispcv3.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[3]])), 
  nyet.mispcv4.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[4]])), 
  nyet.mispcv5.res %>% mutate(NX = length(nyet.xvec), N = nrow(nyet[[5]])),
  nyue.mispcv1.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[1]])), 
  nyue.mispcv2.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[2]])), 
  nyue.mispcv3.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[3]])), 
  nyue.mispcv4.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[4]])), 
  nyue.mispcv5.res %>% mutate(NX = length(nyue.xvec), N = nrow(nyue[[5]]))
) %>% 
  bind_rows(mi_res_sf_vals) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  left_join(mi_res_sf_mi, by = c('Dataset', 'YVAR', 'MSA', 'State', 'Neigh')) %>% 
  mutate(Dataset = str_replace(Dataset, 'Natregimes.*', 'Natregimes'))

nats.nrows <- nats %>% 
  lapply(function(x) data.frame(State = unique(x$STATE_NAME), N1 = nrow(x))) %>% 
  do.call(bind_rows,.)



rsq.spcvresult <- rsq.spcvres %>% 
  filter(is.na(MSA) | !(MSA == 'HickoryMSA' & Dataset == 'Industry Mix')) %>% 
  left_join(nats.nrows) %>% 
  mutate(N = ifelse(grepl('^(Natregimes).*', Dataset), N1, N)) %>% 
  dplyr::select(-N1) %>% 
  group_by(Dataset, MSA, State, Neigh) %>% 
  mutate(N = unique(N[which(!is.na(N))])) %>% 
  ungroup %>% 
  mutate(Ngroup = cut(N, c(0, 100, 1000, max(N))),
         NXgroup = cut(NX, c(0, 5, 10, 13)),
         Model = plyr::mapvalues(Model, c('lm', 'nnet', 'randomForest', 'SpFilter', 'svm'),
                                 c('OLS', 'ANN', 'RF', 'SF', 'SVM')),
         Model = factor(Model, levels = c('ANN', 'RF', 'SVM', 'SF', 'OLS')),
         Field = plyr::mapvalues(Dataset, viewtable$from, viewtable$to))



