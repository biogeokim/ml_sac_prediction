## Data Preprocessing

source('Base_MLSAC.r')


## Initialize yvec and xvec

## 1. AirBnB ####
airbnb <- airbnb %>% st_transform(crs = 3857) %>% 
  mutate(crimes = num_crimes / population * 1000,
         theft = num_theft / population * 1000) %>% 
  filter(!is.na(accept_r) & !is.na(rev_rating)) %>% 
  mutate_if(is.numeric, funs(scale))

airbnb.yvec <- c('response_r', 'accept_r', 'rev_rating', 'price_pp')
airbnb.xvec <- c('poverty', 'crowded', 'dependency', 'without_hs', 'unemployed', 'income_pc', 'harship_in', 'crimes', 'theft')


## 2. Baltimore Housing ####
balt <- balt %>% st_transform(crs = 3857) %>% 
  mutate(PRICE_LIV = PRICE / SQFT,
         PRICE_LOT = PRICE / LOTSZ) %>% 
  mutate_if(is.numeric, list(~scale(.)))

balt.yvec <- c('PRICE_LIV', 'PRICE_LOT')
balt.xvec <- colnames(balt)[3:13]


## 3. Boston Housing ####
bost <- bost %>% st_transform(crs = 3857)# %>% 
bost.sp <- bost %>% mutate_at(.vars = vars(9:23), .funs = list(~scale(.)))

bost.yvec <- c('CMEDV')
bost.xvec <- bost.sp %>% colnames %>% .[11:23]


## 4. Industry mixes ####
## Charleston Industry Mix
## Hickory Industry Mix
## Orlando Industry Mix
## Sacramento Industry Mix
## Seattle Industry Mix
## Lansing Industry Mix
zipstat.s <- zipstat %>% 
  mutate(ZIP = sprintf('%05s', ZIPA),
         rateuniv = (E33006 + E33007) / (E4H001 + E4H002) * 100,
         hin = E4U001,
         youth = (E1L013 + E1L014 + E1L015 + E1L016 + E1L017) / (E4H001 + E4H002) * 100) %>% 
  dplyr::select(ZIP, rateuniv, hin, youth)
imix.ll <- imix.l %>% 
  lapply(function(x) x %>% 
           st_transform(crs = 3857) %>% 
           left_join(zipstat.s, by = 'ZIP') %>% 
           mutate_at(.vars = vars(rateuniv:youth), 
                     .funs = list(~ifelse(is.na(.), median(., na.rm = TRUE), .))))
#colnames(imix.ll[[5]])[c(1:50)] <- colnames(imix.ll[[4]])#[c(1:13, 15:51)]

imix.ll[[1]] <- imix.ll[[1]] %>% rename(EST98 = EST98_0, EST01 = EST01_0)
imix.ll[[2]] <- imix.ll[[2]] %>% rename(EST98 = EST98_0, EST01 = EST01_0)
imix.ll[[3]] <- imix.ll[[3]] %>% rename(EST98 = EST98_0, EST01 = EST01_0)
imix.ll[[4]] <- imix.ll[[4]] %>% rename(EST98 = EST98_0, EST01 = EST01_0)
imix.ll[[5]] <- imix.ll[[5]] %>% rename(EST98 = EST98_0, EST01 = EST01_0)
imix.ll[[6]] <- imix.ll[[6]] #%>% rename(EST98 = EST98_0, EST01 = EST01_0)

imix.lp <- imix.ll %>% lapply(function(x) x %>% mutate_if(is.numeric, list(~scale(.))))# %>% as('Spatial'))

imix.lp[[1]] %>% colnames %>% .[c(15:50)[-grep('.US.*', .[15:50])]] -> imix.yvec
#imix.lp[[1]]@data %>% colnames %>% .[c(15:50)[grep('.(98).*', .[15:50])]] -> imix.ylist98
#imix.lp[[1]]@data %>% colnames %>% .[c(15:50)[grep('.(01).*', .[15:50])]] -> imix.ylist01
#imix.lp[[1]]@data %>% colnames %>% .[15:50] -> imix.ylist2
# X variable list
imix.lp[[1]] %>% colnames %>% .[c(56:58, 60:62)] -> imix.xvec



## 5. Chicago Health and Socio-Economic ####
#chhs <- st_read(str_c(ddir, 'GeoDa/comarea/ComArea_ACS14_f.shp')) %>% st_transform(3857)

chhs.std <- chhs %>% mutate_at(.vars = vars(62:65, 70:86), .funs = list(~scale(.)))
chhs.yvec <- colnames(chhs)[70:86]
chhs.xvec <- colnames(chhs)[62:65]


## 6. Chicago Health Indicators ####
hein <- hein %>% st_transform(crs = 3857)
hein.sp <- hein %>% mutate_at(.vars = vars(5:31), .funs = funs(scale))

hein.yvec <- colnames(hein)[5:25]
hein.xvec <- colnames(hein)[26:31]


## 7. Cincinnati Crime ####
cinc <- cinc %>% st_transform(crs = 3857) %>% 
  mutate(BURG = BURGLARY / POPULATION * 1000,
         ASSA = ASSAULT / POPULATION * 1000,
         THEF = THEFT / POPULATION * 1000,
         HH_NON = HH_NONFAMI / HOUSEHOLDS * 100,
         HU_VACANT = HU_VACANT / HSNG_UNITS * 100,
         GROUP_QUAR = GROUP_QUAR / POPULATION * 100,
         GQ_NONINST = GQ_NONINST / POPULATION * 100,
         NONWHITE = (BLACK + AMINDIAN + ASIAN + HAWAIIAN + OTHER_RACE)/ POPULATION * 100,
         JUVENILE = (AGE_15_19 + AGE_20_24) / POPULATION * 100) %>% 
  filter(!is.na(HH_NON)) %>% 
  mutate_if(is.numeric, funs(scale))
cinc.yvec <- c('BURG', 'ASSA', 'THEF')
cinc.xvec <- c('HH_NON', 'HU_VACANT', 'GROUP_QUAR', 'GQ_NONINST', 'NONWHITE', 'JUVENILE')

## 8. Columbus Crime ####
colu <- colu %>% st_transform(3857)
colu.sp <- colu %>% 
  mutate_at(.vars = vars(HOVAL:DISCBD), .funs = funs(scale))

colu.yvec <- c('CRIME')
colu.xvec <- c('INC', 'HOVAL', 'OPEN', 'PLUMB', 'DISCBD', 'NSA', 'NSB', 'EW', 'CP')


## 9. Denver Crime ####
#denv <- st_read('D:/GeoDa/denver/denver.shp')

denv <- denv %>% 
  mutate(NEIGHBOR = stringr::str_replace(tolower(NBRHD_NAME), ' ', '-'))
denv.crime.s <- st_set_geometry(denv.crime, NULL) %>% 
  mutate(year = substr(REPORTED_D, 1, 4)) %>%
  filter(year != '2019') %>% 
  mutate(OFFENSE_YEAR = paste(OFFENSE_TY, year, sep = '')) %>% 
  group_by(NEIGHBORHO, OFFENSE_YEAR) %>% 
  summarize(N = n()) %>% 
  ungroup %>% 
  spread(key = OFFENSE_YEAR, value = N)

denv.a <- denv %>% 
  left_join(denv.crime.s, by = c('NEIGHBOR' = 'NEIGHBORHO')) %>% 
  mutate_at(.vars = vars(130:(ncol(.)-1)), .funs = funs(ifelse(is.na(.), 0, .))) %>% 
  .[,sapply(., FUN = function(x) length(unique(x)) >= 0.2 * length(x))] %>% 
  mutate_at(.vars = vars(123:321), .funs = funs(./POPULATION * 100000)) %>% 
  mutate(pnonwhite = 100 - PCT_WHITE,
         pvacant = 100 * VACANTUNIT / HOUSINGUNI,
         prent = 100 * HU_RENTED / HOUSINGUNI,
         psinglef = 100 * (MALE_HHL_1 + FEMALE_H_1) / FAMILY_HHL,
         pjuvenile = 100 * (AGE_18_AND + AGE_20 + AGE_21 + AGE_22_TO_) / POPULATION
  ) %>% 
  mutate_at(.vars = vars(pnonwhite, pvacant, prent, psinglef, pjuvenile, 123:321),
            .funs = list(~scale(.)))

denv.yvec <- stringr::str_replace_all(colnames(denv.a)[123:321], '-', '.')
denv.xvec <- c('pnonwhite', 'pvacant', 'prent', 'psinglef', 'pjuvenile')

## 10. Natregimes (National Crime) ####
#nat <- st_read(str_c(ddir, 'GeoDa/natregimes/natregimes.gpkg'))

nat <- nat %>% st_transform(3857)
nats <- nat %>% split(., .$STATE_NAME)# STATE: STATE_FIPS

# Excluding states with less than 30 counties
for (i in 1:length(nats)){
  if (nrow(nats[[i]]) < 30) { nats[[i]] <- NA}
}

nats[which((nats %>% lapply(length) %>% do.call(c, .)) <= 1)] <- NULL
nats.soc <- nats %>% lapply(function(x){
  x %>% 
    mutate_if(is.numeric, list(~scale(.)))
  return(x)})

nats.yvec <- c('HR60', 'HR70', 'HR80', 'HR90')
nats.xvec <- expand.grid(c('RD', 'PS', 'UE', 'DV', 'MA'), seq(60, 90, 10)) %>% 
  mutate(fnm = str_c(Var1, Var2, sep = '')) %>%
  .$fnm


## 11. US Elections ####
#elec <- st_read(str_c(ddir, 'GeoDa/election/County_election_2012_16.shp'))
elec <- elec %>% st_transform(crs = 3857) %>% 
  mutate_if(is.numeric, funs(replace_na(., 0)))
elec.sp <- elec %>% 
  mutate(ELDER14 = AGE775214,
         WHITE14 = RHI825214,
         FOREIGN14 = POP645213,
         FEMALE14 = SEX255214,
         LATINO14 = RHI725214,
         VETERAN14 = VET605213 / PST045214 * 100,
         RETAIL07 = RTN131207,
         ESTAB13 = NES010213,
         INCOME13 = INC910213,
         POVERTY13 = PVY020213,
         FIRMNW07 = (SBO001207 - (SBO315207 + SBO115207 + SBO215207 + SBO515207 + SBO415207)) / SBO001207 * 100) %>% 
  filter(PST045214 > 0)
elec.sp1 <- elec.sp %>% mutate_at(.vars = vars(85:95), .funs = funs(scale))
elec.yvec1 <- c('diff_2016')
elec.yvec2 <- c('diff_2012')
elec.xvec <- c('ELDER14', 'WHITE14', 'FEMALE14', 'LATINO14', # VETERAN14, FOREIGN14
               'RETAIL07', 'ESTAB13', 'INCOME13', 'POVERTY13')#, 'FIRMNW07')

## 12. Phoenix ACS ####
phx <- phx %>% st_transform(crs = 3857)

phx.yvec <- c('inc')
phx.xvec <- c('renter_rt', 'vac_hsu_rt', 'white_rt', 'black_rt', 'hisp_rt', 'fem_nh_rt')


## 13. MSA Employment ####
## MSA Employment (Charleston, South Carolina)
## MSA Employment (Hickory, North Carolina)
## MSA Employment (Lansing, Michigan)
## MSA Employment (Milwaukee, Wisconsin)
## MSA Employment (Orlando, Florida)
## MSA Employment (Sacramento, California)
## MSA Employment (Savannah, Georgia)
## MSA Employment (Seattle, Washington)
##MSA Employment (Tampa, Florida)

#msas <- list.files(path = str_c(ddir, 'GeoDa/MSA'),
#                   pattern = '*(2|4).gpkg$',
#                   full.names = TRUE,
#                   recursive = TRUE)
#msas <- msas %>% split(.,.) %>% lapply(st_read)
msas.s <- msas
msas.st <- msas.s %>% 
  lapply(function(x) {x <- x %>% 
    mutate(emp_away_p = EMP_AWAY / EMPL16 * 100,
           emp_home_p = EMP_HOME / EMPL16 * 100,
           emp_29_p = EMP_29 / EMPL16 * 100,
           emp_30_p = EMP_30 / EMPL16 * 100,
           emp_civ_p = (EMP16_2) / EMPL16 * 100,
           occ_man_p = OCC_MAN / EMPL16 * 100,
           occ_off1_p = OCC_OFF1 / EMPL16 * 100,
           occ_info_p = OCC_INFO / EMPL16 * 100,
           pov_tot1k = POV_TOT / 1000,
           hh_inc1k = HH_INC / 1000,
           hsg_val1k = HSG_VAL / 1000) %>% 
    mutate_if(is.numeric, .funs = list(~replace_na2(.))) %>% 
    mutate_if(is.numeric, .funs = list(~replace_inf(.)))
  return(x)})
msas.sts <- msas.st %>% 
  lapply(function(x){x <- x %>% 
    mutate_at(.vars = vars(31:41), .funs = vars(~scale(.)))
  return(x)})

msas.yvec <- c('emp_away_p', 'emp_home_p', 'emp_29_p', 'emp_30_p', 'emp_civ_p', 'occ_man_p', 'occ_off1_p', 'occ_info_p')
msas.xvec <- c('pov_tot1k', 'hh_inc1k', 'hsg_val1k')


## 14. New York Education ####
nye <- nye %>% st_transform(3857)
nye <- nye %>% 
  mutate(
    hs_d = hs / over25 * 1000,
    somecol_d = somecol / over25 * 1000,
    col_d = college / over25 * 1000,
    master_d = master / over25 * 1000,
    prof_d = prof / over25 * 1000,
    phd_d = phd / over25 * 1000,
    school_d = SCHOOL_CT / population * 1000
  ) %>% 
  mutate_if(is.numeric, funs(replace_na2)) %>% 
  mutate_if(is.numeric, funs(replace_inf))
nyet <- nye %>% split(., .$BoroName) %>% 
  lapply(function(x) {x %>% mutate_if(is.numeric, .funs = list(~scale(.)))
         return(x)})
nyet.xvec <- c('school_d', 'GENDER_PAR', 'PER_PRV_SC', 'YOUTH_DROP', 'PER_MNRTY', 'mean_inc', 'HS_DROP', 'PER_ASIAN')
nyet.yvec <- c('hs_d', 'somecol_d', 'col_d', 'master_d', 'prof_d', 'phd_d', 'COL_DEGREE')



## 15. New York Unemployment ####
#nyt <- st_read(str_c(ddir, 'GeoDa/nyctract_acs/NYC_Tract_ACS2008_12.shp'))
st_crs(nyt) <- 4326
nyt <- nyt %>% st_transform(3857)

nyt <- nyt %>%
  mutate(#pac.une = pacificune / pacific * 100,
    other.une = otherunemp / otherethni * 100,
    mixed.une = mixedunemp / mixed * 100,
    male.une = maleunempl/ male * 100,
    his.une = hispanicun / hispanic * 100,
    fem.une = femaleunem / female * 100,
    euro.une = europeanun/ european * 100,
    ameind.une = americanun / american * 100,
    asian.une = asianunemp / asian * 100,
    afr.une = africanune / african * 100,
    withssi_p = withssi / households * 100,
    withpubass_p = withpubass / households * 100,
    p_professionb = profession / poptot * 100,
    medianage = medianage %>% as.character %>% as.numeric,
    gini = gini %>% as.character %>% as.numeric,
    medianinco = medianinco %>% as.character %>% as.numeric) %>% 
  mutate_if(is.numeric, funs(replace_na2)) %>% 
  mutate_if(is.numeric, funs(replace_nan)) %>% 
  mutate_if(is.numeric, funs(replace_inf))

nyts <- nyt %>% split(., .$boroname) %>% 
  lapply(function(x) {
    x %>% 
      mutate_if(is.numeric, .funs = list(~replace_nan(.))) %>% 
      mutate_if(is.numeric, .funs = list(~replace_na2(.))) %>% 
      mutate_if(is.numeric, .funs = list(~scale(.)))
    return(x)})

nyts.yvec <- c('other.une', 'male.une', 'his.une', 'fem.une', 'euro.une', 'ameind.une', 'afr.une')
nyts.xvec <- c('medianage', 'medianinco', 'onlylessth', 'poor', 'withssi_p', 'withpubass_p', 'p_professionb', 'struggling')

