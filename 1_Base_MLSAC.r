if (!require(pacman)) { install.packages("pacman") }
library(pacman)
p_load(tidyverse, spdep, vegan, sf, sp, stringr, caret, kernlab, RRF, foreach, doParallel, CAST, mlr, parallelMap)


## Utility functions
replace_nan <- function(x) ifelse(is.nan(x), 0, x)
replace_na2 <- function(x) ifelse(is.na(x), 0, x)
replace_inf <- function(x) ifelse(is.infinite(x), 0, x)


gen_form <- function(form.origin, vectors){
    formo <- as.character(form.origin)
    vcn <- colnames(vectors)
    form <- as.formula(paste(formo[2], formo[1], paste(formo[3]), '+', paste(vcn, collapse = '+')))
    return(form)
}

gen_dat <- function(dat.origin, me){
    dat <- bind_cols(dat.origin, me$vectors %>% data.frame)
    return(dat)
}
gen_dat.sf <- function(dat.origin, sf){
  dat <- bind_cols(dat.origin, fitted(sf) %>% data.frame)
  return(dat)
}



### varpart
ME.varpart <- function(dat, xvars = imix.xvec){
  # dat: x by y data.frame. which contains ME vectors and X variables
  library(vegan)
  lc <- length(colnames(dat))
  datcols <- colnames(dat)
  form.xs <- as.formula(paste('~', paste(xvars[-1], collapse = '+')))
  form.me <- as.formula(paste('~', paste(datcols[grep('^vec.', datcols)], collapse = '+')))
  #full_df <- ;
  MEV <- varpart(dat[,lc], form.xs, form.me, data = dat)
  
  return(MEV)
}

treat_analysis <- function(treat,
                           yvec,
                           xvec,
                           mode = 'area'){
  treat.st <- treat %>% as('Spatial')
  if (mode == 'area'){
    treat.nb <- treat %>% as('Spatial') %>% poly2nb
  }
  if (mode == 'point'){
    treat.nb <- treat %>% as('Spatial') %>% knearneigh(k = floor(nrow(treat) * 0.1)) %>% knn2nb
  }
  treat.lw <- treat.nb %>% nb2listw(zero.policy = TRUE)
  
  
  treat.yvec <- sort(yvec)
  treat.xvec <- xvec
  treat.mi <- treat.yvec %>% split(.,.) %>% 
    lapply(function(x) moran.test(unlist(treat.st@data[,x]), listw = treat.lw, zero.policy = TRUE)$estimate[1]
    )
  treat.mi <- treat.mi %>% do.call(c, .)
  
  treat.forms <- treat.yvec %>% split(.,.) %>% lapply(function(x) as.formula(paste(x, '~', paste(treat.xvec, collapse = '+'))))
  treat.lms <- treat.forms %>% lapply(function(x) lm(formula = x, data = treat.st@data))
  
  treat.sf <- treat.forms %>% 
    lapply(function(x) SpatialFiltering(formula = x, nb = treat.nb, ExactEV = FALSE, data = treat.st@data, zero.policy = TRUE))
  # augmented data
  treat.sfa <- lapply(treat.sf, function(x) return(gen_dat.sf(treat.st@data, x)))
  treat.form1 <- mapply(function(x, y) gen_form(x, y$dataset), treat.forms, treat.sf, SIMPLIFY = FALSE)
  treat.sfl <- mapply(function(x, y) lm(x, data = y), treat.form1, treat.sfa, SIMPLIFY = FALSE)
  
  # rsq1
  treat.sfl.rsq <- treat.sfl %>% lapply(function(x) summary(x)$adj.r.squared)
  treat.lms.rsq <- treat.lms %>% lapply(function(x) summary(x)$adj.r.squared)
  # ressac
  treat.sf.rs <- treat.sfl %>% 
    lapply(residuals) %>% 
    lapply(function(x) moran.test(x = x, listw = treat.lw, zero.policy = TRUE)$estimate[1])
  treat.lm.rs <- treat.lms %>% 
    lapply(residuals) %>% 
    lapply(function(x) moran.test(x = x, listw = treat.lw, zero.policy = TRUE)$estimate[1])
  
  
  treat.result <- data.frame(
    MoranI = treat.mi,
    LMresI = treat.lm.rs %>% do.call(c, .),
    SFresI = treat.sf.rs %>% do.call(c, .),
    LMrsq = treat.lms.rsq %>% do.call(c, .),
    SFrsq = treat.sfl.rsq %>% do.call(c, .)
  )
  return(treat.result)
}



# modification: 200623 (finished)
# 200919: sorting problem in split(.,.) part: fixed - may affect the whole result
sf_analysis <- function(treat,
                        yvec,
                        xvec,
                        mode = 'area',
                        std = TRUE,
                        return.residuals = FALSE,
                        extract.coef = FALSE){
  treat.st <- treat %>% as('Spatial')
  if (std){
    treat.st@data <- treat.st@data %>% 
      mutate_if(is.numeric, list(~as.vector(scale(.)))) %>% 
      as.data.frame
  }
  if (mode == 'area'){
    treat.nb <- treat %>% as('Spatial') %>% poly2nb
  }
  if (mode == 'point'){
    treat.nb <- treat %>% as('Spatial') %>% 
      knearneigh(k = max(30, floor(nrow(treat) * 0.1))) %>% knn2nb
  }
  treat.lw <- treat.nb %>% nb2listw(zero.policy = TRUE)
  
  # added 'sort' (101120)
  treat.yvec <- sort(yvec)
  treat.xvec <- xvec
  treat.mi <- treat.yvec %>% split(.,.) %>% 
    lapply(function(x) moran.test(unlist(treat.st@data[,x]), listw = treat.lw, zero.policy = TRUE)$estimate[1]
    )
  treat.mi <- treat.mi %>% do.call(c, .)
  
  treat.forms <- treat.yvec %>% split(.,.) %>% lapply(function(x) as.formula(paste(x, '~', paste(treat.xvec, collapse = '+'))))
  treat.lms <- treat.forms %>% lapply(function(x) lm(formula = x, data = treat.st@data, y = TRUE))
  
  treat.sf <- treat.forms %>% 
    lapply(function(x) SpatialFiltering(formula = x, nb = treat.nb, ExactEV = FALSE, data = treat.st@data, zero.policy = TRUE))
  # augmented data
  treat.sfa <- lapply(treat.sf, function(x) return(gen_dat.sf(treat.st@data, x)))
  treat.form1 <- mapply(function(x, y) gen_form(x, y$dataset), treat.forms, treat.sf, SIMPLIFY = FALSE)
  treat.sfl <- mapply(function(x, y) lm(x, data = y, y = TRUE), treat.form1, treat.sfa, SIMPLIFY = FALSE)
  
  # rsq1
  treat.sfl.rsq <- treat.sfl %>% lapply(function(x) summary(x)$adj.r.squared)
  treat.lms.rsq <- treat.lms %>% lapply(function(x) summary(x)$adj.r.squared)
  treat.sfl.prsq <- treat.sfl %>% lapply(function(x) summary(x)$r.squared)
  treat.lms.prsq <- treat.lms %>% lapply(function(x) summary(x)$r.squared)
  # ressac
  treat.sf.rs <- treat.sfl %>% 
    lapply(residuals)
  treat.sf.rsi <- treat.sf.rs %>% 
    lapply(function(x) moran.test(x = x, listw = treat.lw, zero.policy = TRUE)$estimate[1])
  treat.lm.rs <- treat.lms %>% 
    lapply(residuals)
  treat.lm.rsi <- treat.lm.rs %>% 
    lapply(function(x) moran.test(x = x, listw = treat.lw, zero.policy = TRUE)$estimate[1])
  
  # accuracy measures (200623)
  treat.sf.am <- treat.sfl %>% 
    lapply(function(x) 
      data.frame(RMSE = mlr::measureRMSE(x$y, x$y + residuals(x)),
        NRMSE = mlr::measureRMSE(x$y, x$y + residuals(x))/(max(x$y)-min(x$y)),
        MAE = mlr::measureMAE(x$y, x$y + residuals(x)),
        MAPE = mlr::measureMAPE(x$y, x$y + residuals(x)))) 
  treat.lm.am <- treat.lms %>% 
    lapply(function(x) 
      data.frame(RMSE = mlr::measureRMSE(x$y, x$y + residuals(x)),
                 MAE = mlr::measureMAE(x$y, x$y + residuals(x)),
                 MAPE = mlr::measureMAPE(x$y, x$y + residuals(x))))

  
  # varimp (relative rank of regression coefficients)  
  treat.sfl.vi <- treat.sfl %>% lapply(function(x) x[1]$coefficients[2:(length(treat.xvec)+1)] %>% t %>% as.data.frame)
  treat.lms.vi <- treat.lms %>% lapply(function(x) x[1]$coefficients[2:(length(treat.xvec)+1)] %>% t %>% as.data.frame)
  varimp.result <- mapply(FUN = function(x, y){
    vis <- bind_rows(x, y) %>% 
      mutate(Model = c('SpFilter', 'OLS')) %>% 
      gather(key = independent, value = coef, 1:length(treat.xvec))
    return(vis)},
    treat.sfl.vi, treat.lms.vi, SIMPLIFY = FALSE) %>% 
    mapply(function(x, y){
      x1 <- x %>% 
        mutate(dependent = y)
      return(x1)
    }, ., treat.yvec %>% split(.,.), SIMPLIFY = FALSE) %>% 
    do.call(bind_rows, .) %>% 
    group_by(Model, dependent) %>% 
    mutate(Rank = n() + 1 - rank(abs(coef))) %>% 
    ungroup
  
  treat.result1 <- data.frame(
    YVAR = sort(yvec),
    MoranI = treat.mi,
    Model = 'OLS',
    res.MI = treat.lm.rsi %>% do.call(c, .),
    #SFresI = treat.sf.rs %>% do.call(c, .),
    Rsquared = treat.lms.rsq %>% do.call(c, .),
    #SFrsq = treat.sfl.rsq %>% do.call(c, .),
    Rsquared.plain = treat.lms.prsq %>% do.call(c,.)
    #SFrsq.plain = treat.sfl.prsq %>% do.call(c,.)
  ) %>% 
    bind_cols(do.call(rbind, treat.lm.am))
  treat.result2 <- data.frame(
    YVAR = sort(yvec),
    MoranI = treat.mi,
    Model = 'SpFilter',
    #LMresI = treat.lm.rs %>% do.call(c, .),
    res.MI = treat.sf.rsi %>% do.call(c, .),
    #LMrsq = treat.lms.rsq %>% do.call(c, .),
    Rsquared = treat.sfl.rsq %>% do.call(c, .),
    #LMrsq.plain = treat.lms.prsq %>% do.call(c,.),
    Rsquared.plain = treat.sfl.prsq %>% do.call(c,.)
  ) %>% 
    bind_cols(do.call(rbind, treat.sf.am))
  treat.result <-
    bind_rows(treat.result1, treat.result2)
  if (extract.coef){
    return(varimp.result)
  } else {
    if (return.residuals){
      return(list(treat.result, lmres = treat.lm.rs, sfres = treat.sf.rs))
    } else {
      return(treat.result)
    }
  }
}





X.sac <- function(treat, xvars, mode){
  #treat.st <- treat %>% as('Spatial')
  if (mode == 'area'){
    treat.nb <- treat %>% as('Spatial') %>% poly2nb
  }
  if (mode == 'point'){
    treat.nb <- treat %>% as('Spatial') %>% knearneigh(k = floor(nrow(treat) * 0.1)) %>% knn2nb
  }
  treat.lw <- treat.nb %>% nb2listw(zero.policy = TRUE)
  
  xl <- xvars %>% split(.,.)
  xls.mt <- xl %>% 
    lapply(function(x) moran.test(x = treat[,x] %>% st_set_geometry(NULL) %>% unlist, 
                                  listw = treat.lw, randomisation = F,
                                  zero.policy = TRUE)$estimate[1])
  xls.mt <- xls.mt %>% do.call(cbind, .) %>% data.frame
  colnames(xls.mt) <- xvars
  return(xls.mt)
    
}

### residual SAC
res.sac <- function(dat, xvars = imix.xvec, nbs = imix.nb[[1]]){
  # dat: x by y data.frame. which contains ME vectors and X variables
  lc <- length(colnames(dat))
  datcols <- colnames(dat)
  yvec <- datcols[lc]
  form.full <- as.formula(paste(yvec, '~', paste(datcols[-c(1, lc)], collapse = '+')))
  form.o <- as.formula(paste(yvec, '~', paste(datcols[-c(grep('^(vec).', datcols), lc)], collapse = '+')))
  mod.full <- lm(formula = form.full, data = dat)
  mod.o <- lm(formula = form.o, data = dat)
  aic.full <- AIC(mod.full)
  aic.o <- AIC(mod.o)
  
  res.full <- residuals(mod.full)
  res.o <- residuals(mod.o)
  mor.full <- moran.test(res.full, listw = nbs)$estimate[1]
  mor.o <- moran.test(res.o, listw = nbs)$estimate[1]
  
  mor.diff <- list(mor.full, mor.o)
  names(mor.diff) <- c('MoranI.full', 'MoranI.O')
  lap <- mor.diff
  return(lap)
}

### r.squared
rsqs <- function(dat, xvars = imix.xvec, nbs = imix.nb[[1]]){
  # dat: x by y data.frame. which contains ME vectors and X variables
  lc <- length(colnames(dat))
  datcols <- colnames(dat)
  yvec <- datcols[lc]
  form.full <- as.formula(paste(yvec, '~', paste(datcols[-c(1, lc)], collapse = '+')))
  form.o <- as.formula(paste(yvec, '~', paste(datcols[-c(grep('^(vec).', datcols), lc)], collapse = '+')))

  mod.full <- lm(formula = form.full, data = dat)
  mod.o <- lm(formula = form.o, data = dat)
  rsq.full <- summary(mod.full)$adj.r.squared
  rsq.o <- summary(mod.o)$adj.r.squared
  
  mor.diff <- list(rsq.full, rsq.o)
  names(mor.diff) <- c('rsq_full', 'rsq_O')
  lap <- mor.diff
  return(lap)
}



### AIC computation
aic.diff <- function(dat, xvars = imix.xvec){
  # dat: x by y data.frame. which contains ME vectors and X variables
  lc <- length(colnames(dat))
  datcols <- colnames(dat)
  yvec <- datcols[lc]
  form.full <- as.formula(paste(yvec, '~', paste(datcols[-c(1, lc)], collapse = '+')))
  form.o <- as.formula(paste(yvec, '~', paste(datcols[-c(grep('^(vec).', datcols), lc)], collapse = '+')))
  
  mod.full <- lm(formula = form.full, data = dat)
  mod.o <- lm(formula = form.o, data = dat)
  
  aic.full <- AIC(mod.full)
  aic.o <- AIC(mod.o)
  
  aic.diff <- list(aic.full, aic.o)#aic.o - aic.full
  lap <- aic.diff
  return(lap)
}



### MLR_LEARN ####
# 200708: added deep learning (by h2o)
mlr_learn <- function(input_data, algs = list(), yvec, xvec, 
                      cvmethod = 'CV', ncore = parallel::detectCores()-2, 
                      std = FALSE, ncv = 10, seed.num = 202006){
  library(mlrCPO)
  if (length(yvec) != 1){
    cat('The task cannot run with yvec longer than 1.')
    break
  }
  set.seed(seed.num)
  input_std <- input_data %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(yvec, xvec) #%>% 
  if (std){
    input_std <- input_std %>% 
      mutate_if(is.numeric, list(~scale(.) %>% as.vector))
  } 
    #
  mlr_learn <- algs %>% split(.,.) %>% 
    lapply(function(x){
      library(parallelMap)
      parallelStartSocket(ncore)
      # Make learner and define the regression task
      lx <- makeLearner(cl = x, predict.type = 'response')
      lre <- makeRegrTask(data = input_std, 
                          target = yvec,
                          coordinates = st_centroid(input_data, of_largest_polygon = TRUE) %>% st_coordinates %>% data.frame) 
      lrs <- makeResampleDesc(method = cvmethod, 
                              iters = ncv, 
                              predict = 'both')
      # parset
      if (grepl('*.(lm)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('tol', lower = -10, upper = -5, trafo = function(x) 10 ^ x) 
        )
        ctrl <- makeTuneControlGrid()
        
      }
      if (grepl('*.(deeplearning)$', x)){
        pset <- makeParamSet(
          makeDiscreteParam('activation', values = c('RectifierWithDropout')),
          makeDiscreteParam('epochs', values = c(50)),
          makeDiscreteParam('hidden', values = list(set1 = c(50, 50), set2 = c(100, 100))),
          makeDiscreteParam(id = 'hidden_dropout_ratios', values = list(vec1 = c(0.25, 0.25), vec2 = c(0.5, 0.5), vec3 = c(0.75, 0.75)))
        )
        ctrl <- makeTuneControlGrid()
      }
      if (grepl('*.(svm)$', x)){
        pset <- makeParamSet(
          makeIntegerParam("nu", lower = -4, upper = 4, trafo = function(x) 10 ** x),
          makeDiscreteParam("kernel", values = c("polynomial", "radial basis", "linear")),
          makeIntegerParam("gamma", lower = -6, upper = 6, trafo = function(x) 2^x,
                           requires = quote(kernel == "radial basis")),
          makeIntegerParam("degree", lower = 2L, upper = 4L,
                           requires = quote(kernel == "polynomial"))
        )
        ctrl <- makeTuneControlGrid()
        
      } else if (grepl('*.(randomForest)$', x)){
        pset <- makeParamSet(
          makeIntegerParam("mtry", lower = floor(length(xvec)/2), upper = floor(length(xvec))),
          makeIntegerParam("maxnodes", lower = floor(nrow(input_std)/5), upper = min(floor(nrow(input_std)*0.75), 1000))
        )
        ctrl <- makeTuneControlGrid()
        
      } else if (grepl('*.(nnet)$', x)) {
        pset <- makeParamSet(
          makeIntegerParam("size", lower = 2, 
                           upper = ifelse(floor(nrow(input_std)/30) <= 3, 10, 
                                          min(floor(nrow(input_std)/30), 16)))
        )
        ctrl <- makeTuneControlGrid()
      } else if (grepl('*.(SVR)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('svr_eps', lower = -4, upper = 4, trafo = function(x) 10^x),
          makeIntegerParam('type', lower = 11, upper = 12)
        )
        ctrl <- makeTuneControlGrid()
      } else if (grepl('*.(brnn)$', x)) {
        pset <- makeParamSet(
          makeIntegerParam('neurons', lower = 2, upper = max(7, floor(nrow(input_std)^(1/3))))
          )
        ctrl <- makeTuneControlGrid()
      } else if (grepl('*.(RRF)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('mtry', lower = floor(length(xvec)/2), upper = length(xvec)),
          makeIntegerParam("maxnodes", lower = floor(nrow(input_std)/5), upper = min(floor(nrow(input_std)*0.75), 1000))
          )
        ctrl <- makeTuneControlGrid()
      }
      res <- tuneParams(learner = lx, task = lre, 
                        resampling= lrs, 
                        control = ctrl,
                        par.set = pset,
                        measures = list(rmse, mae, mape, expvar, spearmanrho))
      if (grepl('*.(deeplearning)$', x)){
        vimp <- NA
      } else {
        vimp <- generateFeatureImportanceData(lre, learner = lx, nmc=100L)
      }
      lres = lre %>>% cpoRegrResiduals(learner = x, 
                                       crr.train.residuals = 'resample')
      lres = getTaskData(lres)[,yvec] %>% unlist %>% as.vector
      parallelStop()
      res <- list(tuneResult = res, residuals = lres, varimp = vimp)
      return(res)
    }
    )
  return(mlr_learn)
}

mlr_learn_tr <- function(input_data, algs = list(), yvec, xvec, 
                      ncore = parallel::detectCores()-2, std = FALSE, ncv = 10,
                      seed.num = 202006){
  library(mlrCPO)
  if (length(yvec) != 1){
    cat('The task cannot run with yvec longer than 1.')
    break
  }
  set.seed(seed.num)
  input_std <- input_data %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(yvec, xvec) #%>% 
  if (std){
    input_std <- input_std %>% 
      mutate_if(is.numeric, list(~scale(.) %>% as.vector))
  } 
  #
  mlr_learn <- algs %>% split(.,.) %>% 
    lapply(function(x){
      library(parallelMap)
      cl <- parallelStartSocket(ncore)
      registerDoSNOW(cl)
      lx <- makeLearner(cl = x, predict.type = 'response')
      lre <- makeRegrTask(data = input_std, 
                          target = yvec,
                          coordinates = st_centroid(input_data, of_largest_polygon = TRUE) %>% st_coordinates %>% data.frame) 

      if (grepl('*.(lm)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('tol', lower = -10, upper = -5, trafo = function(x) 10 ^ x) 
        )
        ctrl <- makeTuneControlGrid()
        
      }
      if (grepl('*.(svm)$', x)){
        pset <- makeParamSet(
          makeIntegerParam("nu", lower = -4, upper = 4, trafo = function(x) 10 ** x),
          makeDiscreteParam("kernel", values = c("polynomial", "radial basis", "linear")),
          makeIntegerParam("gamma", lower = -6, upper = 6, trafo = function(x) 2^x,
                           requires = quote(kernel == "radial basis")),
          makeIntegerParam("degree", lower = 2L, upper = 4L,
                           requires = quote(kernel == "polynomial"))
        )
        ctrl <- makeTuneControlGrid()#makeTuneControlRandom(maxit = 100L)
        
      } else if (grepl('*.(randomForest)$', x)){
        pset <- makeParamSet(
          makeIntegerParam("mtry", lower = floor(length(xvec)/2), upper = floor(length(xvec))),
          makeIntegerParam("maxnodes", lower = floor(nrow(input_std)/5), upper = min(floor(nrow(input_std)*0.75), 1000))
        )
        ctrl <- makeTuneControlGrid()#makeTuneControlRandom(maxit = 100L)
        
      } else if (grepl('*.(nnet)$', x)) {
        pset <- makeParamSet(
          makeIntegerParam("size", lower = 2, 
                           upper = ifelse(floor(nrow(input_std)/30) <= 3, 10, 
                                          min(floor(nrow(input_std)/30), 16)))
        )
        ctrl <- makeTuneControlGrid()#makeTuneControlRandom(maxit = 30L)
        #ctrl <- makeTuneControlGrid(resolution = 6L)
      } else if (grepl('*.(SVR)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('svr_eps', lower = -4, upper = 4, trafo = function(x) 10^x),
          makeIntegerParam('type', lower = 11, upper = 12)
        )
        ctrl <- makeTuneControlGrid()
      } else if (grepl('*.(brnn)$', x)) {
        pset <- makeParamSet(
          makeIntegerParam('neurons', lower = 2, upper = max(7, floor(nrow(input_std)^(1/3))))
        )
        ctrl <- makeTuneControlGrid()
      } else if (grepl('*.(RRF)$', x)){
        pset <- makeParamSet(
          makeIntegerParam('mtry', lower = floor(length(xvec)/2), upper = length(xvec)),
          makeIntegerParam("maxnodes", lower = floor(nrow(input_std)/5), upper = min(floor(nrow(input_std)*0.75), 1000))
        )
        ctrl <- makeTuneControlGrid()
      }
      res <- 'notrain'#train(learner = lx, task = lre)
      #res <- tuneParams(learner = lx, task = lre, 
      #                  resampling= lrs, #makeResampleDesc('CV', predict = 'both', iters = 10),#lrs, 
      #                  control = ctrl,
      #                  par.set = pset,
      #                  #resample.fun = rmse,
      #                  measures = list(rmse, mae, mape, expvar, spearmanrho))
      vimp <- generateFeatureImportanceData(lre, learner = lx, nmc=100L)
      lres = lre %>>% cpoRegrResiduals(learner = x)
      lres = getTaskData(lres)[,yvec] %>% unlist %>% as.vector
      parallelStop()
      res <- list(tuneResult = res, y = input_std[,yvec] %>% unlist,
                  residuals = lres, varimp = vimp)
      return(res)
    }
    )
  return(mlr_learn)
}


### Extract Model Metric #### 
## Function : triple layered lists
extr_modelres <- function(mm, yvec, modellist = calg1, neighbor, mode = 1){
  if (mode != 1){
    mm.vals <- mm %>% 
      lapply(function(x){ x %>% 
          lapply(function(y){ 
            ym <- moran.test(x = y$residuals, listw = nb2listw(neighbor, zero.policy = TRUE), zero.policy = TRUE)$estimate[1]
            ycon <- c(y$tuneResult$y[1:3], ym)
            return(ycon)}) %>% 
            do.call(c, .) %>% 
            data.frame(measure = c('RMSE', 'MAE', 'MAPE', 'res.MI'),
                        value = .) -> df.cleaned
            return(df.cleaned)
        }) %>% 
      do.call(rbind, .) %>% 
      cbind(Model = rownames(.), .) %>% 
      mutate(YVAR = str_remove(Model, '.regr.(lm|nnet|randomForest|svm).*'),
            Model = str_extract(Model, '(lm|nnet|randomForest|svm)'))
  } else {
    mm.vals <- mm %>% 
      lapply(function(x) x %>% lapply(function(y){ 
              yt <- y$y
              ye <- y$res + y$y
              ymi <- moran.test(ye, nb2listw(neighbor, zero.policy = TRUE), zero.policy = TRUE)$estimate[1]
              ym <- c(rmse(yt, ye),
                      rmse(yt, ye)/(max(yt)-min(yt)),
                      mae(yt, ye),
                      mape(yt, ye),
                      ymi)
              return(ym)}) %>% 
               do.call(c, .) %>% 
               data.frame(measure = c('RMSE', 'NRMSE', 'MAE', 'MAPE', 'res.MI'),
                          value = .)) %>% 
      do.call(rbind, .) %>% 
      cbind(Model = rownames(.), .) %>% 
      mutate(YVAR = str_remove(Model, '.regr.(lm|nnet|randomForest|svm).*'),
             Model = str_extract(Model, '(lm|nnet|randomForest|svm)'))
    
  }    
  return(mm.vals)
}



### POST_HOC MI extraction
posthoc_mi <- function(mi, sfd, yvec, xvec){
  sfd <- sfd %>% mutate_at(.vars = vars(-ncol(.)),
                           .funs = list(~scale(.) %>% as.vector))
  sfd.nw <- 
    nb2listw(spdep::knearneigh(sfd, 
                               ifelse(floor(0.1*nrow(sfd))<30, 30, floor(0.1*nrow(sfd)))) %>% knn2nb,
             zero.policy = TRUE)
  ## 1. Y-vector moran's I
  yvec.mi <- vector('list', length = length(yvec))
  for (i in 1:length(yvec)){
    yvec.mi[[i]] <-  
      moran.test(sfd[,yvec[i]] %>% st_set_geometry(NULL) %>% unlist, 
                 listw = sfd.nw, zero.policy = TRUE)$estimate[1]
  }
  ## 2. Extract results
  res_result <- lapply(mi, function(x) lapply(x, function (y) y[[1]]$y[3])) %>% 
    lapply(function(x) do.call(rbind, x)) %>% 
    lapply(function(x) x %>% data.frame %>%
             cbind(rownames(.), .)) %>% 
    do.call(rbind, .)
  res_resid <- lapply(mi, function(x) lapply(x, function (y) y[[2]])) %>% 
    lapply(function(x) do.call(list, x))# %>% 
  #do.call(rbind, .) %>% 
  #data.frame %>% 
  #cbind(rownames(.), .)
  res_varimp <- lapply(mi, function(x) lapply(x, function (y) rank(y[[3]]$res))) %>% 
    lapply(function(x) do.call(rbind, x)) %>% 
    lapply(function(x) x %>% data.frame %>%
             cbind(rownames(.), .)) %>% 
    do.call(rbind, .) %>% 
    data.frame()
  colnames(res_varimp)[1] <- 'Model'
  res_varimp <- res_varimp %>% 
    cbind(., dependent = rep(yvec, each = 3)) %>% 
    pivot_longer(cols = 2:7, names_to = 'independent', values_to = 'Rank')
  
  res_resid <- res_resid %>% 
    lapply(function(x) 
      lapply(x, function(y) moran.test(y, sfd.nw, zero.policy = TRUE)$estimate[1]) %>% 
        do.call(rbind, .) %>% 
        data.frame %>% 
        cbind(rownames(.), .)
    ) %>% 
    do.call(bind_rows, .)
  
  
  res_total <- cbind(res_result, res_resid) %>% 
    mutate(YVAR = rep(yvec, each = 3),
           MoranI = yvec.mi %>% do.call(c, .) %>% rep(., each = 3)) %>% 
    dplyr::select(-`rownames(.).1`)
  colnames(res_total) <- c('Model', 'Rsquared.plain', 'res.MI', 'YVAR', 'MoranI')
  
  ## 3. sf_analysis
  res_sf <- sf_analysis(sfd, yvec, xvec, mode = 'point', extract.coef = FALSE)
  res_sfc <- sf_analysis(sfd, yvec, xvec, mode = 'point', extract.coef = TRUE)
  
  res_total <- res_sf %>% bind_rows(res_total)
  res_varimp <- res_sfc %>% bind_rows(res_varimp)
  
  posthoc <- list(res_total, res_varimp, res_sf, res_sfc)
  
  return(posthoc)
}


### XSAC


Xmc <- function(dat, xvec){
  datx <- dat %>% st_set_geometry(NULL) %>% .[,xvec]
  datcorr <- cor(datx)
  datcorr <- datcorr[upper.tri(datcorr)]
  corr <- mean(datcorr)
  return(corr)
}

Xmi <- function(dat, xvec, lags = 1){
  if (lags > 1){
    nb <- poly2nb(dat) %>% nb2listw(., zero.policy = T)
  } else {
    nb <- dat %>% poly2nb(.) %>% nblag(2) %>% nblag_cumul() %>% nb2listw(., zero.policy = T)
  }

  xvec %>% split(.,.) %>% lapply(function(x){
  xmi <- moran.test(dat %>% st_set_geometry(NULL) %>% .[,x] %>% unlist,
                    nb, zero.policy = TRUE, randomisation = F)
  return(xmi$estimate[1])
  }) %>% do.call(c,.) %>% mean -> xmi
  return(xmi)
}

Xmi_p <- function(dat, xvec, p = .1){
  nb <- knearneigh(dat, floor(nrow(dat)*p)) %>% knn2nb %>% nb2listw
  xvec %>% split(.,.) %>% lapply(function(x){
  xmi <- moran.test(dat %>% st_set_geometry(NULL) %>% .[,x] %>% unlist,
                    nb, zero.policy = TRUE, randomisation = F)
  return(xmi$estimate[1])
  }) %>% do.call(c,.) %>% mean -> xmi
  return(xmi)
}


Xmcmi <- function(dat, xvec, lags = 1, mode = 'a'){
  if (mode == 'a'){
  xmcmi <-
    data.frame(Mean_corr = Xmc(dat, xvec),
               Mean_MI = Xmi(dat, xvec, lags))
  } else {
  xmcmi <-
    data.frame(Mean_corr = Xmc(dat, xvec),
               Mean_MI = Xmi_p(dat, xvec))

  }
  return(xmcmi)
}
