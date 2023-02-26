rm(list = ls())

# libraries
pacman::p_load(mboost, tidyverse, lubridate, caret, corrplot, earth, ranger,Metrics, ggplot2,zoo)


outputs <- c("total_count", "Ischemic_count","Bleeding_count")

#read  config
if(file.exists("config.yml")){
  conf <- config::get(file = "config.yml")
}else{
  conf <- config::get(file = "config_default.yml")
}


site.name <- conf$site


### ---------------------------------------------------------------------------
### --------------------------- Pre-processing --------------------------------
### ---------------------------------------------------------------------------


###looped three times for fitting models for three outputs
for(output_counter in outputs){
  
  print(paste("fitting models for", output_counter))
  daily <- read.csv(file = file.path(getwd(),"data/daily_level.csv"))

  # Outcome based on output_counter 
  outcome <- daily[[output_counter]]
  daily <- daily %>% select(!contains("count"))

  daily <- na.locf(daily,na.rm = FALSE)
  daily <- na.locf(daily,fromLast = TRUE)
  
  # create daily variables
  year <- year(daily$admission_date)
  date <- daily$admission_date
  daily$admission_date <- NULL
  
  daily <- daily %>% 
    mutate_at(vars(-day_of_month, -day_of_year,-month,-wday,-year,-week_num,
                   -mean_prior_week_total,-median_prior_week_total,
                   -mean_prior_week_ischemic,-median_prior_week_ischemic,
                   -mean_prior_week_bleeding,-median_prior_week_bleeding
                   ), ~ (scale(.) %>% as.vector))
  
  daily <- subset(daily,select = -c(day_of_month, day_of_year,month,year,week_num))
  # create formula
  frm <- paste((colnames(daily)[!grepl("count", colnames(daily)) &  !grepl("prior_week", colnames(daily))]), collapse = " + ")
  
  if(output_counter == 'total_count'){
    frm <- paste0(gsub("\\+ week_prior", "", frm), " + mean_prior_week_total + median_prior_week_total")
  }else if(output_counter == 'Ischemic_count') {
    frm <- paste0(gsub("\\+ week_prior", "", frm), " + mean_prior_week_ischemic + median_prior_week_ischemic")
  }else if(output_counter == 'Bleeding_count') {
    frm <- paste0(gsub("\\+ week_prior", "", frm), " + mean_prior_week_bleeding + median_prior_week_bleeding")
  }
  daily$outcome <- outcome
  ### ---------------------------------------------------------------------------
  ### ------------------------------ Models -------------------------------------
  ### ---------------------------------------------------------------------------
  
  set.seed(32)
  check_models <- FALSE
  
  res_list <- list()
  
  ## from the Metrics package
  metric_funs <- list(
    mse,
    function(a,p) mape(a+1,p+1),   ## Whethere its okay to add 1. Check the usual practice
    cor
    )
  names(metric_funs) <- c("MSE", "MAPE", "COR")
  
  eval_fun <- function(a_tr,a_te,p_tr,p_te,name){
    measures_tr <- sapply(metric_funs, function(metr) metr(a_tr, p_tr))
    names(measures_tr) <- paste0(names(metric_funs), "_train")
    measures_te <- sapply(metric_funs, function(metr) metr(a_te, p_te))
    names(measures_te) <- paste0(names(metric_funs), "_test")
    
    data.frame(t(c(measures_tr,measures_te)), model = name)
    
  }
  
  test_index <- which(year(date) ==  max(year(date)))
  train <- daily[-test_index,]
  test <- daily[test_index,]
  
  tr_year <- year[-test_index]
  #val_folds <- sapply(unique(tr_year), function(y) tr_year == y)
  
  #fixed window false
  val_folds<- matrix(nrow =nrow(train) ,ncol = length(unique(tr_year)) - 1)
  unique_tr_year <- unique(tr_year)
  
 
  for(i in 1:(length(unique_tr_year)-1)){
    val_folds[which(tr_year <= unique_tr_year[i]),i]<- FALSE
    val_folds[which(tr_year >= unique_tr_year[i+1]),i]<- TRUE
  }

    

     
    ### ---------------------------------------------------------------------------
    ### gamboost
    
    print(paste("Fitting Gamboost for",output_counter))
    
    unique_values <- daily %>% summarise_all(n_distinct) %>% t() %>% as.data.frame() 
    colnames(unique_values) <- "count"
    
    bbs_vars <- rownames(unique_values)[(which(unique_values$count > 24))]
    bols_vars <-  rownames(unique_values)[(which(unique_values$count <= 24))]
    
    fts <- trimws(strsplit(frm, "\\+")[[1]])
    
    frm_mboost <- paste(c(paste0("bbs(",fts[fts %in% bbs_vars],")"), 
                          paste0("bols(",fts[fts %in% bols_vars],")") 
                          ),
                        collapse = " + ")
    
    
    try({
		# NegBinomial model
      
		mod <- gamboost(formula = as.formula(paste0("outcome ~ ", frm_mboost)),
						data = train, family = NBinomial(),
						control = boost_control(mstop = 1000L,nu = 0.01))
	 
		cvr <- cvrisk(mod, folds = val_folds)
		mod[mstop(cvr)]
		
		if(check_models){
		  plot(cvr)
		  table(selected(mod))
		  plot(mod)
		  plot(train$outcome, predict(mod, type = "response")[,1], col=rgb(0,0,0,0.4))
		  abline(0, 1, col="red")
		}
		
		pr_tr <- predict(mod, type = "response")[,1]
		pr_te <- predict(mod, newdata = test, type = "response")[,1]
		
		res_gamboostNB <- eval_fun(train$outcome, test$outcome,
								   pr_tr, pr_te, name = "gamboostNB")
		
		#save model
		saveRDS(object = mod,file = paste("./results/gamboostNB_",output_counter,site.name,".rda",sep = ""))
		rm(mod, cvr, pr_tr, pr_te)
   }, silent=TRUE)
    print(paste("Fitting Gamboost Poisson for",output_counter))
    
	try({
		# Poisson model
		mod <- gamboost(formula = as.formula(paste0("outcome ~ ", frm_mboost)),
						data = train, family = Poisson(), control = boost_control(mstop = 1000L, 
																				 nu = 0.01))
		
		cvr <- cvrisk(mod, folds = val_folds)
		
		mod[mstop(cvr)]
		
		if(check_models){
		plot(cvr)
		table(selected(mod))
		plot(mod)
		plot(train$outcome, predict(mod, type = "response")[,1], col=rgb(0,0,0,0.4))
		abline(0, 1, col="red")
		}
		
		pr_tr <- predict(mod, type = "response")[,1]
		pr_te <- predict(mod, newdata = test, type = "response")[,1]
		
		res_gamboostPO <- eval_fun(train$outcome, test$outcome,
								   pr_tr, pr_te, name = "gamboostPO")
		
		#save model
		saveRDS(object = mod,file = paste("./results/gamboostPO",output_counter,"_",site.name,".rda",sep = ""))
		rm(mod, cvr, pr_tr, pr_te)
    }, silent=TRUE)
    ### ---------------------------------------------------------------------------
    ### MARS
    print(paste("Fitting MARS earth for",output_counter))
    
    mod <- earth(as.formula(paste0("outcome ~ ", frm)), data = train,
                 degree = 1)
    
    if(check_models){
    summary(mod)
    # plot non-linear features
    plotmo(mod)
    plot(train$outcome, predict(mod, type = "response")[,1], col=rgb(0,0,0,0.4))
    abline(0, 1, col="red")
    }
    
    pr_tr <- predict(mod, type = "response")[,1]
    pr_te <- predict(mod, newdata = test, type = "response")[,1]
    
    res_earth1 <- eval_fun(train$outcome, test$outcome,
                          pr_tr, pr_te, name = "earth1")
						  
    #save model
    saveRDS(object = mod,file = paste("./results/earth1",output_counter,"_",site.name,".rda",sep = ""))
	rm(mod, cvr, pr_tr, pr_te)
    
    ### ---------------------------------------------------------------------------
    ### random forest
    print(paste("Fitting Random forest for",output_counter))
    mod <- ranger(as.formula(paste0("outcome ~ ", frm)), data = train)
    
    if(check_models){
      mod
      plot(data$outcome, predict(mod, data = train, type = "response")$predictions, 
           col=rgb(0,0,0,0.4))
      abline(0, 1, col="red")
    }
    
    pr_tr <- predict(mod, data = train, type = "response")$predictions
    pr_te <- predict(mod, data = test, type = "response")$predictions
    
    res_rf <- eval_fun(train$outcome, test$outcome,
                       pr_tr, pr_te, name = "rf")
    #save model
    saveRDS(object = mod,file = paste("./results/rf",output_counter,"_",site.name,".rda",sep = ""))
    rm(mod, cvr, pr_tr, pr_te)
    ### ---------------------------------------------------------------------------
    
    res_list<- rbind(res_gamboostNB, res_gamboostPO,res_earth1,res_rf)
  
 
  
 
  
  res_long <- res_list %>% pivot_longer(MSE_train:COR_test) %>% 
    mutate(data = gsub(".*_(train|test)", "\\1", name),
           measure = gsub("(.*)_(train|test)", "\\1", name))
  
  #save residuals
  saveRDS(object = res_long,file = paste("./results/results",output_counter,"_",site.name,".rda",sep = ""))
 
  try({
	  ggplot(res_long, aes(x = model, y = value, colour = model)) + 
		geom_point() + 
		facet_grid(measure ~ data, scales = "free") + 
		theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
	  
	  ggsave(filename = paste("./results/predictions",output_counter,"_",site.name,".pdf",sep = ""), width = 5, height = 5)
  }, silent=TRUE)
   rm(res_long, res_gamboostNB, res_gamboostPO, res_earth1, res_rf, res_list)
}
print("Modeling gamboost is done")

print("Zip the files in the reults folder to be delivered")

## zip all the files in the results folder to zip : to upload
daily <- read.csv(file = file.path(getwd(),"data/daily_level.csv"))
files2zip <- dir(file.path(getwd(),"/results"), full.names = TRUE)
zip_file_name <- paste('westorm-step2-results-', conf$site, "-", Sys.Date(), "-coverage-", min(year), "-", max(year), "-totalcases-", sum(daily$total_count),  sep = "")
zip(zipfile = file.path(getwd(), zip_file_name), files = files2zip, extras = '-j')
print(paste("please upload the zip file:", zip_file_name))
print("Execution is done")