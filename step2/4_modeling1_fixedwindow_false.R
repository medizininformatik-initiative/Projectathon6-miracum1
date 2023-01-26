###############clear Rstudio environment variables ####################
rm(list=ls())
gc()
#.rs.restartR()

start.time <- Sys.time()
##################libraries#######################################
library(dplyr)
library(lubridate)
library(caret)
library(xgboost)
library(zoo)
library(ranger)  

#read  config
if(file.exists("config.yml")) {
  conf <- config::get(file = "config.yml")
} else {
  conf <- config::get(file = "config_default.yml")
}


site.name <- conf$site
######################## Import prepared data  #######
daily <- read.csv(file = file.path(getwd(), "data/daily_level.csv"))
two_day <- read.csv(file = file.path(getwd(), "data/two_day.csv"))
weekly <- read.csv(file = file.path(getwd(), "data/weekly.csv"))
monthly <- read.csv(file = file.path(getwd(), "data/monthly.csv"))

######################################## Modelling #################################
wb <-openxlsx:::createWorkbook()
openxlsx:::addWorksheet(wb, "Daily")

#1.################################### Daily ########################################
	print("Models for daily count")
	daily <- na.locf(daily,na.rm = FALSE)
	daily <- na.locf(daily,fromLast = TRUE)

	####features
	# weather features standardize
	print("Standardize the input features")
	features_daily_weather <- subset(daily, select = -c(admission_date, total_count, count_I63,count_I61,
	                                                    count_I60, Ischemic_count, Bleeding_count, 
	                                                    mean_prior_week_ischemic, median_prior_week_ischemic,
	                                                    mean_prior_week_bleeding, median_prior_week_bleeding,
	                                                    mean_prior_week_total, median_prior_week_total,
	                                                    day_of_month,day_of_year, month, wday, year, week_num))
	
	#features_daily_weather <- scale(features_daily_weather) # default: center = TRUE, scale = TRUE
	
	# calendar and mean prior week feature 
	features_daily_calendar_total_count <- subset(daily, select = c(day_of_month,day_of_year, month, wday, year, week_num, mean_prior_week_total, median_prior_week_total))
	features_daily_calendar_ischemic <- subset(daily, select = c(day_of_month, day_of_year, month, wday, year, week_num, mean_prior_week_ischemic, median_prior_week_ischemic))
	features_daily_calendar_bleeding <- subset(daily, select = c(day_of_month, day_of_year, month, wday, year, week_num, mean_prior_week_bleeding, median_prior_week_bleeding))
	
	# bind weather and calendar features for all three outputs
	features_daily_total <- cbind(features_daily_weather, features_daily_calendar_total_count)
	features_daily_ischemic <- cbind(features_daily_weather, features_daily_calendar_ischemic)
	features_daily_bleeding <- cbind(features_daily_weather, features_daily_calendar_bleeding)
	
	rm(features_daily_weather,features_daily_calendar_total_count,features_daily_calendar_ischemic,features_daily_calendar_bleeding)
	
	# targets for all three outputs
	total_count_daily <- daily$total_count
	Ischemic_count_daily <- daily$Ischemic_count
	Bleeding_count_daily <- daily$Bleeding_count


	print("Split: train and test set")
	# split train and test
	# last (available) year is defined as test set (per protocol 2021)
	test_index <- which(year(daily$admission_date) ==  max(year(daily$admission_date))) 
	# previous (available) years as training-validation set (per protocol 2015-2020)
	train_features_daily <- features_daily_total[-test_index, ]
	test_features_daily <- features_daily_total[test_index, ]


	print("Fitting models for total_count for daily resolution")
	# 1-a.################################################# Models for Total count ###################################
	  train_total_count_daily <- total_count_daily[-test_index]
	  test_total_count_daily <- total_count_daily[test_index]
	  train_daily <- as.data.frame(cbind(train_features_daily, train_total_count_daily))

	print("Poisson")
	### Poisson
	try({
	set.seed(1492)
	  poisson_daily_total_count <- glm(train_total_count_daily ~ ., data = train_daily, family = poisson)
	  # predict
	  preds<-predict(poisson_daily_total_count, newdata = as.data.frame(test_features_daily), type = "response")
	  # metrics
	  rmse<-paste("RMSE of Poisson daily model for total count", RMSE(pred = preds, obs = test_total_count_daily))
	  mae<- paste("MAE of Poisson daily model for total count", MAE(pred = preds, obs = test_total_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE, startRow = 1)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE, startRow = 2)
	  
	  # save model
	  # note: saving path with past is not OS agnostic. Consider using file.path().
	  saveRDS(object = poisson_daily_total_count, file = paste("./results/poisson_daily_total_count_", site.name, ".rda", sep = ""))
	  rm(poisson_daily_total_count)
	}, silent=TRUE)
	print("Random Forest")
	### RF
	  # hyper parameter
	  # mtry defaults to sqrt(n_features) ~ 18; check approx. +/-10% range
	try({
	set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(17, 18, 19, 20)) 

	  # cross-validation
	  timecontrol_cv <- trainControl(method = "timeslice",
	                                 initialWindow = 365,
	                                 horizon = 365,
	                                 fixedWindow = FALSE,
	                                 allowParallel =TRUE,
	                                 verboseIter = TRUE,
	                                 skip = 364)
	  
	  # fit model
	  forest <- train(train_total_count_daily~., 
	                  data = train_daily, 
	                  method = 'rf', 
	                  trControl = timecontrol_cv, 
	                  metric='RMSE',
					  preProcess = c("center","scale"),
	                  tuneGrid = tunegrid)
	
	  # final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  # convert into train and test 
	  X_train = train_features_daily 
	  y_train = train_total_count_daily
	  X_test = test_features_daily
	  y_test = test_total_count_daily
	  
	  # final RF model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_daily_total_count = train(X_train, y_train,
	                                   trControl = timecontrol_cv,
	                                   tuneGrid = final_grid, 
	                                   method = "rf", 
	                                   verbosity = TRUE)
	  
	  
	  # predict
	  preds <- predict(forest_daily_total_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  
	  # metrics
	  rmse <- paste("RMSE of Random forest daily model for total count", 
	              RMSE(pred = preds, obs = test_total_count_daily))
	  mae <- paste("MAE of Random forest daily model for total count",
	              MAE(pred = preds, obs = test_total_count_daily))
	  openxlsx:::writeData(wb = wb, x = rmse, sheet = "Daily", withFilter = FALSE, startRow = 3)
	  openxlsx:::writeData(wb = wb, x = mae, sheet = "Daily", withFilter = FALSE, startRow = 4)
	  
	  # save model
	  # note: saving path with past is not OS agnostic. Consider using file.path().
	  saveRDS(object = forest_daily_total_count, file = paste("./results/forest_daily_total_count_", site.name, ".rda", sep = ""))
	  rm(forest_daily_total_count)
	}, silent=TRUE)
	
	print("Xgboost")
	### XGB
	  # cross-validation method and number of folds
	  # grid space to search for the best hyperparameters
	
	try({
	set.seed(1492)
	  xgbGrid <- expand.grid(nrounds = c(100, 200), 
	                         max_depth = c(1, 3, 6, 9),
	                         colsample_bytree = seq(0.5, 0.9, length.out = 5),
	                         eta = c( .2, .1, .05, .01),
	                         gamma = 0,
	                         min_child_weight = 1,
	                         subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train, 
	                    trControl = timecontrol_cv, 
						tuneGrid = xgbGrid, 
						preProcess = c("center","scale"),
	                    method = "xgbTree", 
	                    verbosity = 0)
	  #final grid 
	  final_grid <- expand.grid(
		nrounds = xgb_model$bestTune$nrounds,
		eta = xgb_model$bestTune$eta,
		max_depth = xgb_model$bestTune$max_depth,
		gamma = xgb_model$bestTune$gamma,
		colsample_bytree = xgb_model$bestTune$colsample_bytree,
		min_child_weight = xgb_model$bestTune$min_child_weight,
		subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_daily_total_count = train(X_train, y_train, 
									trControl = timecontrol_cv,
									tuneGrid = final_grid,
									preProcess = c("center","scale"),
									method = "xgbTree", 
									verbosity = 0)
	  # predict
	  preds<-predict(xgb_daily_total_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  # metrics
	  rmse<-paste("RMSE of XGB daily model for total count",RMSE(pred = preds,obs = test_total_count_daily))
	  mae<- paste("MAE of XGB daily model for total count",MAE(pred = preds,obs = test_total_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 5)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 6)
	  # save model
	  saveRDS(object = xgb_daily_total_count,file = paste("./results/xgb_daily_total_count_",site.name,".rda",sep = ""))
	  rm(xgb_daily_total_count)
	}, silent=TRUE)
	
	print("Support Vector Regression") 
	### SVR
	  # svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  # search grid for hyperparameter tuning
	try({
		set.seed(1492)
		tuneGrid <- expand.grid(C = c(0.25, .5, 1), sigma = 0.1)
		svm_daily_total_count <- train(X_train, y_train,
										 trControl = timecontrol_cv,
										 tuneGrid = tuneGrid,
										 method = "svmRadial",
										 preProcess = c("center","scale"), 
										 verbosity = 0)
			 
		  
		  
		  final_grid <- expand.grid(C =svm_daily_total_count$bestTune$C,sigma=svm_daily_total_count$bestTune$sigma )  
		  # final svr model with chosen hyper parameter
		  print("fitting SVR based on chosen hyperparameter")
		  svm_daily_total_count <- train(X_train, y_train,  
										 trControl = timecontrol_cv,
										 tuneGrid = final_grid,method = "svmRadial",
										 preProcess = c("center","scale"), 
										 verbosity = 0)
		  #predict
		  preds <- predict(svm_daily_total_count, newdata = as.data.frame(test_features_daily), type = "raw")
		  #metrics
		  rmse<-paste("RMSE of SVM daily model for total count",RMSE(pred = preds,obs = test_total_count_daily))
		  mae<- paste("MAE of SVM daily model for total count",MAE(pred = preds,obs = test_total_count_daily))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 7)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 8)
		  #save model
		  saveRDS(object = svm_daily_total_count,file = paste("./results/svm_daily_total_count_",site.name,".rda",sep = ""))
		  rm(svm_daily_total_count)
	}, silent=TRUE)	  
	  

	print("Fitting models for Ischemic_count for daily resolution")
	
	#1-b.################################################# Models for Ischemic count################################### 
	print("split to train and test set")
	##split train and test
	test_index <- which(year(daily$admission_date) ==  max(year(daily$admission_date)))
	train_features_daily <- features_daily_ischemic[-test_index,]
	test_features_daily <- features_daily_ischemic[test_index,]
	

	 train_ischemic_count_daily <- Ischemic_count_daily[-test_index]
	 test_ischemic_count_daily <- Ischemic_count_daily[test_index]
	 train_daily <- as.data.frame(cbind(train_features_daily,train_ischemic_count_daily))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
	  set.seed(1492)
	  poisson_daily_ischmeic_count <- glm(train_ischemic_count_daily ~ ., data = train_daily, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_daily_ischmeic_count)
	  #predict
	  preds <- predict(poisson_daily_ischmeic_count, newdata = as.data.frame(test_features_daily), type = "response")
	  #metrics
	  rmse <- paste("RMSE of Poisson daily model for ischmeic count",RMSE(pred = preds,obs = test_ischemic_count_daily))
	  mae <- paste("MAE of Poisson daily model for ischmeic count",MAE(pred = preds,obs = test_ischemic_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 9)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 10)
	  ##save model
	  saveRDS(object = poisson_daily_ischmeic_count,file = paste("./results/poisson_daily_ischemic_count_",site.name,".rda",sep = ""))
	  rm(poisson_daily_ischmeic_count)
	}, silent=TRUE)
	print("Random Forest")
	### RF
	  # hyper parameter
	try({
	  set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(17,18,19,20))
	  # cross-validation
	  # repeat_cv <- trainControl(method = 'repeatedcv', number = 5, repeats = 3, verboseIter = TRUE, returnData = FALSE)
	  
	  #fit model
	  forest <- train(train_ischemic_count_daily~.,
	                  data = train_daily, 
	                  method = 'rf', 
	                  trControl = timecontrol_cv, 
					  preProcess = c("center","scale"),
	                  metric = 'RMSE', 
	                  tuneGrid = tunegrid)
	  
	  # final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	 
	 # final rf model with chosen hyper parameter
	  X_train = train_features_daily 
	  y_train = train_ischemic_count_daily
	  X_test = test_features_daily 
	  y_test = test_ischemic_count_daily
	  print("fitting forest based on chosen hyperparameter")
	  forest_daily_ischmeic_count = train(X_train, y_train,
										preProcess = c("center","scale"),
										tuneGrid = final_grid,
										method = "rf", verbosity = 0)
	  #predict
	  preds <- predict(forest_daily_ischmeic_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of random forest daily model for ischmeic count", RMSE(pred = preds, obs = test_ischemic_count_daily))
	  mae <- paste("MAE of Poisson daily model for ischmeic count", MAE(pred = preds, obs = test_ischemic_count_daily))
	  openxlsx:::writeData(wb = wb, x = rmse, sheet = "Daily", withFilter = FALSE, startRow = 11)
	  openxlsx:::writeData(wb = wb, x = mae, sheet = "Daily", withFilter = FALSE, startRow = 12)
	  #save model
	  saveRDS(object = forest_daily_ischmeic_count, file = paste("./results/forest_daily_ischemic_count_", site.name, ".rda", sep = ""))
	  rm(forest_daily_ischmeic_count)
	}, silent=TRUE)
	
	print("Xgboost")
	### XGB
	  # cross-validation method and number of folds
	  # xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  # grid space to search for the best hyperparameters
	try({ 
	  set.seed(1492)
	  xgbGrid <- expand.grid(nrounds = c(100,200), 
	                         max_depth = c(1,3,6,9), 
	                         colsample_bytree = seq(0.5, 0.9, length.out = 5), 
	                         eta = c( .2, .1, .05, .01), 
	                         gamma = 0,
	                         min_child_weight = 1,
	                         subsample = 1)
	  # train the model
	  xgb_model = train(X_train, y_train, 
	                    trControl = timecontrol_cv, 
	                    tuneGrid = xgbGrid,
						preProcess = c("center","scale"),
	                    method = "xgbTree", 
	                    verbosity = 0)
	  
	  # tuned grid parameters
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample)
	  
	  # re-fit xgb model with bestTune hyperparameters
	  print("re-fitting xgboost based on chosen hyperparameter")
	  xgb_daily_ischemic_count = train(X_train, y_train, 
	                                   tuneGrid = final_grid,
									   preProcess = c("center","scale"),
	                                   method = "xgbTree", 
	                                   verbosity = 0)
	  #predict
	  preds <- predict(xgb_daily_ischemic_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of XGB daily model for ischmeic count",RMSE(pred = preds,obs = test_ischemic_count_daily))
	  mae <- paste("MAE of XGB daily model for ischmeic count",MAE(pred = preds,obs = test_ischemic_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 13)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 14)
	  #save model
	  saveRDS(object = xgb_daily_ischemic_count,file = paste("./results/xgb_daily_ischemic_count_",site.name,".rda",sep = ""))
	  rm(xgb_daily_ischemic_count)
	}, silent=TRUE)
	
	
	print("Support Vector Regression") 
	### SVR
	  #svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameter
	try({
		set.seed(1492)
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
	  svm_daily_ischemic_count <- train(X_train, y_train,
					    trControl = timecontrol_cv,
					    tuneGrid = tuneGrid,
					    method = "svmRadial",
					    preProcess = c("center","scale"), 
					    verbosity = 0)
	      
	  
	  
	  final_grid <- expand.grid(C =svm_daily_ischemic_count$bestTune$C,sigma=svm_daily_ischemic_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_daily_ischemic_count <- train(X_train, y_train,
	                                    tuneGrid = final_grid,
	                                    method = "svmRadial",
	                                    preProcess = c("center", "scale"),
	                                    verbosity = 0)


	  #predict
	    preds <- predict(svm_daily_ischemic_count, newdata = as.data.frame(test_features_daily), type = "raw")
	    #metrics
	    rmse <- paste("RMSE of SVM daily model for ischmeic count",RMSE(pred = preds,obs = test_ischemic_count_daily))
	    mae <- paste("MAE of SVM daily model for ischmeic count",MAE(pred = preds,obs = test_ischemic_count_daily))
	    openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 15)
	    openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 16)
	
	  #save model
	  saveRDS(object = svm_daily_ischemic_count, file = paste("./results/svm_daily_ischemic_count_",site.name,".rda",sep = ""))
	  rm(svm_daily_ischemic_count)

	  End.time <- Sys.time()	 
	}, silent=TRUE)	 

	print("Fitting models for Bleeding_count for daily resolution")
	#1-c.################################################# Models for Bleeding count###################################
	print("split train and test set")
	##split train and test
	test_index <- which(year(daily$admission_date) ==  max(year(daily$admission_date)))
	train_features_daily <- features_daily_bleeding[-test_index,]
	test_features_daily <- features_daily_bleeding[test_index,]
	
	  train_bleeding_count_daily <- Bleeding_count_daily[-test_index]
	  test_bleeding_count_daily <- Bleeding_count_daily[test_index]
	  train_daily <- as.data.frame(cbind(train_features_daily,train_bleeding_count_daily))

	print("Poisson")
	### Poisson
	try({
		set.seed(1492)
	  #fit the model
	  poisson_daily_bleeding_count <- glm(train_bleeding_count_daily ~ ., data = train_daily, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_daily_bleeding_count)
	  #predict
	  preds <- predict(poisson_daily_bleeding_count, newdata = as.data.frame(test_features_daily), type = "response")
	  #metrics
	  rmse <- paste("RMSE of Poisson daily model for bleeding count", RMSE(pred = preds,obs = test_bleeding_count_daily))
	  mae <- paste("MAE of Poisson daily model for bleeding count", MAE(pred = preds,obs = test_bleeding_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 17)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 18)
	  ##save model
	  saveRDS(object = poisson_daily_bleeding_count,file = paste("./results/poisson_daily_bleeding_count_",site.name,".rda",sep = ""))
	  rm(poisson_daily_bleeding_count)
	}, silent=TRUE)	  
	### ARIMA

	print("Random Forest")
	### RF
	  #hyper parameter
	try({
		set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(17,18,19,20))
	  #cross validation
	  #repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
	  #fit model
	  forest <- train(train_bleeding_count_daily~.,data=train_daily, 
					method='rf', 
					trControl=timecontrol_cv,
					preProcess = c("center","scale"),					
					metric='RMSE',
					tuneGrid =tunegrid )

	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final rf model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  X_train = train_features_daily 
	  y_train = train_bleeding_count_daily
	  X_test = test_features_daily 
	  y_test = test_bleeding_count_daily
	  forest_daily_bleeding_count = train(X_train, y_train,  
											preProcess = c("center","scale"),
											tuneGrid = final_grid,
											method = "rf", 
											verbosity = 0)
	  #predict
	  preds <- predict(forest_daily_bleeding_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of Random forest daily model for bleeding count", RMSE(pred = preds,obs = test_bleeding_count_daily))
	  mae <- paste("MAE of Random forest daily model for bleeding count", MAE(pred = preds,obs = test_bleeding_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 19)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 20)
	  #save model
	  saveRDS(object = forest_daily_bleeding_count,file = paste("./results/forest_daily_bleeding_count_",site.name,".rda",sep = ""))
	  rm(forest_daily_bleeding_count)
	}, silent=TRUE)
	
	print("Xgboost")
	### XGB
	  #convert train and test into DMatrix

	  #cross-validation method and number of folds
	  #xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	try({
		set.seed(1492)
	  xgbGrid  <- expand.grid(nrounds = c(100,200), 
							  max_depth = c(1,3,6,9),
							  colsample_bytree = seq(0.5, 0.9, length.out = 5),
							  eta =c( .2, .1, .05, .01),
							  gamma=0,
							  min_child_weight = 1,
							  subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train,  
						trControl = timecontrol_cv,
						preProcess = c("center","scale"),
						tuneGrid = xgbGrid,
						method = "xgbTree", 
						verbosity = 0)

	  #final grid 
	  final_grid <- expand.grid(
		nrounds = xgb_model$bestTune$nrounds,
		eta = xgb_model$bestTune$eta,
		max_depth = xgb_model$bestTune$max_depth,
		gamma = xgb_model$bestTune$gamma,
		colsample_bytree = xgb_model$bestTune$colsample_bytree,
		min_child_weight = xgb_model$bestTune$min_child_weight,
		subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_daily_bleeding_count = train(X_train, y_train,  
									preProcess = c("center","scale"),
									tuneGrid = final_grid,
									 method = "xgbTree", 
									 verbosity = 0)
	  #predict
	  preds <- predict(xgb_daily_bleeding_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of XGB daily model for bleeding count", RMSE(pred = preds,obs = test_bleeding_count_daily))
	  mae <-  paste("MAE of XGB daily model for bleeding count", MAE(pred = preds,obs = test_bleeding_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Daily", withFilter = FALSE,startRow = 21)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Daily", withFilter = FALSE,startRow = 22)
	  #save model
	  saveRDS(object = xgb_daily_bleeding_count,file = paste("./results/xgb_daily_bleeding_count_",site.name,".rda",sep = ""))
	  rm(xgb_daily_bleeding_count)
	}, silent=TRUE)
	
	print("Support Vector Regression") 
	### SVR
	  #svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	try({	
	set.seed(1492)	
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
	  svm_daily_bleeding_count <- train(X_train, y_train,
					    trControl = timecontrol_cv,
					    tuneGrid = tuneGrid,
					    method = "svmRadial",
					    preProcess = c("center", "scale"),
					    verbosity = 0)
	  
	  final_grid <- expand.grid(C =svm_daily_bleeding_count$bestTune$C,
								sigma=svm_daily_bleeding_count$bestTune$sigma )  
	 
	 #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")

	  svm_daily_bleeding_count <- train(X_train, y_train,
					    tuneGrid = final_grid,
					    method = "svmRadial",
					    preProcess = c("center", "scale"),
					    verbosity = 0)
 
	  #predict
	  preds <- predict(svm_daily_bleeding_count, newdata = as.data.frame(test_features_daily), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of SVM daily model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_daily))
	  mae <- paste("MAE of SVM daily model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_daily))
	  openxlsx:::writeData(wb = wb,x = rmse, sheet = "Daily", withFilter = FALSE, startRow = 23)
	  openxlsx:::writeData(wb = wb,x = mae, sheet = "Daily", withFilter = FALSE, startRow = 24)
	  #save model
	  saveRDS(object = svm_daily_bleeding_count, file = paste("./results/svm_daily_bleeding_count_", site.name, ".rda", sep = ""))
	  rm(svm_daily_bleeding_count)
	  }, silent=TRUE)	

	 
  

#2.####################################################Two day#####################################################
print("Models for Two-day count")
  openxlsx:::addWorksheet(wb, "Two-day")
	two_day <- na.locf(two_day, na.rm = FALSE)
	two_day <- na.locf(two_day, fromLast = TRUE)

	####features and target 
	####features
	#weather features standardize
	print("standardize the input features")
	features_two_day_weather <- subset(two_day,select = -c(daycounter,total_count,count_I63,count_I61,
	                                                       count_I60,Ischemic_count,Bleeding_count,
	                                                       mean_prior_week_ischemic,median_prior_week_ischemic,
	                                                       mean_prior_week_bleeding,median_prior_week_bleeding,
	                                                       mean_prior_week_total,median_prior_week_total)
	                                   )
	#features_two_day_weather <- scale(features_two_day_weather)
	
	###calendar and mean prior week feature 
	features_two_day_calendar_total_count <- subset(two_day,select = c(mean_prior_week_total,median_prior_week_total))
	features_two_day_calendar_ischemic <- subset(two_day,select = c(mean_prior_week_ischemic,median_prior_week_ischemic))
	features_two_day_calendar_bleeding <- subset(two_day,select = c(mean_prior_week_bleeding,median_prior_week_bleeding))
	
	## bind weather and calendar features for all three outputs
	
	features_two_day_total <- cbind(features_two_day_weather,features_two_day_calendar_total_count)
	features_two_day_ischemic <- cbind(features_two_day_weather,features_two_day_calendar_ischemic)
	features_two_day_bleeding <- cbind(features_two_day_weather,features_two_day_calendar_bleeding)
	
	rm(features_two_day_weather,features_two_day_calendar_total_count,features_two_day_calendar_ischemic,features_two_day_calendar_bleeding)
	
	##targets for all three outputs
  total_count_two_day <- two_day$total_count
	Ischemic_count_two_day <- two_day$Ischemic_count
	Bleeding_count_two_day <- two_day$Bleeding_count


	print("split train and test set")
	##split train and test
	##take last 20% rows from two day resolution dataframe
	test_index <- c(nrow(two_day) - floor(nrow(two_day) * 20 * 0.01)):nrow(two_day)
	train_features_two_day <- features_two_day_total[-test_index, ]
	test_features_two_day <- features_two_day_total[test_index, ]


	print("Fitting models for total_count for two_day resolution")
	# 2-a.################################################# Models for Total count###################################
	  train_total_count_two_day <- total_count_two_day[-test_index]
	  test_total_count_two_day <- total_count_two_day[test_index]
	  train_two_day <- as.data.frame(cbind(train_features_two_day,train_total_count_two_day))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
		set.seed(1492)
	  poisson_two_day_total_count <- glm(train_total_count_two_day ~ ., data = train_two_day, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_two_day_total_count)
	  #predict
	  preds<-predict(poisson_two_day_total_count, newdata = as.data.frame(test_features_two_day), type = "response")
	  #metrics
	  rmse <- paste("RMSE of Poisson two_day model for total count", RMSE(pred = preds, obs = test_total_count_two_day))
	  mae <- paste("MAE of Poisson two_day model for total count", MAE(pred = preds, obs = test_total_count_two_day))
	  openxlsx:::writeData(wb = wb, x = rmse,sheet = "Two-day", withFilter = FALSE, startRow = 1)
	  openxlsx:::writeData(wb = wb, x = mae,sheet = "Two-day", withFilter = FALSE, startRow = 2)
	  ##save model
	  saveRDS(object = poisson_two_day_total_count,file = paste("./results/poisson_two_day_total_count_",site.name,".rda",sep = ""))
	  rm(poisson_two_day_total_count)
	}, silent=TRUE)
	  
	  
	  timecontrol_cv_two_day <- trainControl(method = "timeslice",
	                                         initialWindow = 182,
	                                         horizon = 182,
	                                         fixedWindow = FALSE,
	                                         allowParallel = TRUE,
	                                         verboseIter = TRUE,
	                                         skip = 181)

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_two_day 
	  y_train = train_total_count_two_day
	  X_test = test_features_two_day 
	  y_test = test_total_count_two_day
	### RF
	  try({
	  	set.seed(1492)
	  #hyper parameter
	  tunegrid <- expand.grid(mtry=c(17,18,19,20))
	  #cross validation
	  #repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
	  #fit model
	  forest <- train(train_total_count_two_day~.,
							data=train_two_day, 
							preProcess = c("center","scale"),
							method='rf', 
							trControl=timecontrol_cv_two_day, 
							metric='RMSE',
							tuneGrid =tunegrid )
	  
	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_two_day_total_count = train(X_train, y_train,  
									preProcess = c("center","scale"),
									tuneGrid = final_grid,
									method = "rf", verbosity = 0)
	  #predict
	  preds <- predict(forest_two_day_total_count, newdata = as.data.frame(test_features_two_day), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of Random forest two_day model for total count", RMSE(pred = preds,obs = test_total_count_two_day))
	  mae <- paste("MAE of Random forest two_day model for total count", MAE(pred = preds,obs = test_total_count_two_day))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 3)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 4)
	  #save model
	  saveRDS(object = forest_two_day_total_count,file = paste("./results/forest_two_day_total_count_",site.name,".rda",sep = ""))
	  rm(forest_two_day_total_count)
	  }, silent=TRUE)

	  
	print("Xgboost")
	### XGB
	  #cross-validation method and number of folds
	  #xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	try({
	  set.seed(1492)
	  xgbGrid <- expand.grid(nrounds = c(100,200), 
							max_depth = c(1,3,6,9),
							colsample_bytree = seq(0.5, 0.9, length.out = 5),
							eta =c( .2, .1, .05, .01),
							gamma=0,
							min_child_weight = 1,
							subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train,  
						trControl = timecontrol_cv_two_day,
						preProcess = c("center","scale"),
						tuneGrid = xgbGrid,
						method = "xgbTree", 
						verbosity = 0)

	  #final grid 
	  final_grid <- expand.grid(
		nrounds = xgb_model$bestTune$nrounds,
		eta = xgb_model$bestTune$eta,
		max_depth = xgb_model$bestTune$max_depth,
		gamma = xgb_model$bestTune$gamma,
		colsample_bytree = xgb_model$bestTune$colsample_bytree,
		min_child_weight = xgb_model$bestTune$min_child_weight,
		subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_two_day_total_count = train(X_train, y_train,  
									preProcess = c("center","scale"),
									tuneGrid = final_grid,
									method = "xgbTree",
									verbosity = 0)
	  #predict
	  preds <- predict(xgb_two_day_total_count, newdata = as.data.frame(test_features_two_day), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of XGB two_day model for total count",RMSE(pred = preds, obs = test_total_count_two_day))
	  mae <- paste("MAE of XGB two_day model for total count",MAE(pred = preds, obs = test_total_count_two_day))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 5)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 6)
	  #save model
	  saveRDS(object = xgb_two_day_total_count,file = paste("./results/xgb_two_day_total_count_",site.name,".rda",sep = ""))
	  rm(xgb_two_day_total_count)
	}, silent=TRUE)


	print("Support Vector Regression") 
	### SVR
	  #svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	try({
	  set.seed(1492)
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
	  svm_two_day_total_count <- train(X_train, y_train, 
					   trControl = timecontrol_cv_two_day,
					   tuneGrid = tuneGrid,method = "svmRadial",
					   preProcess = c("center","scale"),
					   verbosity = 0)
	  
	  
	  final_grid <- expand.grid(C =svm_two_day_total_count$bestTune$C,sigma=svm_two_day_total_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")

	  svm_two_day_total_count <- train(X_train, y_train,
					   tuneGrid = final_grid,method = "svmRadial",
					   preProcess = c("center", "scale"),
					   verbosity = 0)

	  
          #predict

	  preds <- predict(svm_two_day_total_count, newdata = as.data.frame(test_features_two_day), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of SVM two_day model for total count", RMSE(pred = preds,obs = test_total_count_two_day))
	  mae <- paste("MAE of SVM two_day model for total count", MAE(pred = preds,obs = test_total_count_two_day))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 7)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 8)
	  # save model
	  # not OS agnostic consider file.path() instead
	  saveRDS(object = svm_two_day_total_count, file = paste("./results/svm_two_day_total_count_", site.name, ".rda", sep = ""))
	  rm(svm_two_day_total_count)

	}, silent=TRUE)	  

	print("Fitting models for Ischemic_count for two_day resolution")
	# 2-b.################################################# Models for Ischemic count################################### 
	print("split train and test set")
	##split train and test
	##take last 20% rows from two day resolution dataframe
	test_index <- c(nrow(two_day) - floor(nrow(two_day) * 20 * 0.01)):nrow(two_day)
	train_features_two_day <- features_two_day_ischemic[-test_index,]
	test_features_two_day <- features_two_day_ischemic[test_index,]
	
	
	  train_ischemic_count_two_day <- Ischemic_count_two_day[-test_index]
	  test_ischemic_count_two_day <- Ischemic_count_two_day[test_index]
	  train_two_day <- as.data.frame(cbind(train_features_two_day,train_ischemic_count_two_day))

  try({
  
	### Poisson
	  # fit the model
	  set.seed(1492)
	  poisson_two_day_ischmeic_count <- glm(train_ischemic_count_two_day ~ ., data = train_two_day, family = poisson(link = "log"))
	  # print summary
	  summary(poisson_two_day_ischmeic_count)
	  # predict
	  preds <- predict(poisson_two_day_ischmeic_count, newdata = as.data.frame(test_features_two_day), type = "response")
	  # metrics
	  rmse <- paste("RMSE of poisson two_day model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_two_day))
	  mae <- paste("MAE of poisson two_day model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_two_day))
	  openxlsx:::writeData(wb = wb, x = rmse, sheet = "Two-day", withFilter = FALSE, startRow = 9)
	  openxlsx:::writeData(wb = wb, x = mae, sheet = "Two-day", withFilter = FALSE, startRow = 10)
	  ##save model
	  saveRDS(object = poisson_two_day_ischmeic_count,file = paste("./results/poisson_two_day_ischemic_count_",site.name,".rda",sep = ""))
	  rm(poisson_two_day_ischmeic_count)
    }, silent=TRUE)	print("Poisson")
	### ARIMA

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_two_day
	  y_train = train_ischemic_count_two_day
	  X_test = test_features_two_day 
	  y_test = test_ischemic_count_two_day
	  
	### RF
	  #hyper parameter
	try({
	  set.seed(1492)
	  tunegrid <-expand.grid(mtry=c(17,18,19,20))
	  #cross validation
	  #repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
	  #fit model
	  forest <- train(train_ischemic_count_two_day ~ .,
	                  data = train_two_day, 
	                  method = 'rf',
	                  trControl = timecontrol_cv_two_day, 
					  preProcess = c("center","scale"),
	                  metric = 'RMSE',
	                  tuneGrid = tunegrid)
	  
	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_two_day_ischmeic_count = train(X_train, y_train, 
								preProcess = c("center","scale"),
								tuneGrid = final_grid,
								method = "rf", 
								verbosity = 0)
	  #predict
	  preds <- predict(forest_two_day_ischmeic_count, newdata = as.data.frame(test_features_two_day), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of random forest two_day model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_two_day))
	  mae <- paste("MAE of random forest two_day model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_two_day))
	  openxlsx:::writeData(wb = wb, x = rmse,sheet = "Two-day", withFilter = FALSE, startRow = 11)
	  openxlsx:::writeData(wb = wb, x = mae,sheet = "Two-day", withFilter = FALSE, startRow = 12)
	  #save model
	  saveRDS(object = forest_two_day_ischmeic_count,file = paste("./results/forest_two_day_ischemic_count_",site.name,".rda",sep = ""))
	  rm(forest_two_day_ischmeic_count)
	  }, silent=TRUE)
	  
	print("Xgboost")
	### XGB
	  
	  #cross-validation method and number of folds
	  #xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	 
	try({ 
	  set.seed(1492)
	  xgbGrid   <- expand.grid(nrounds = c(100,200),
								max_depth = c(1,3,6,9),
								colsample_bytree = seq(0.5, 0.9, length.out = 5),
								eta =c( .2, .1, .05, .01),
								gamma=0,
								min_child_weight = 1,
								subsample = 1)
	  # train the model
	  xgb_model = train(X_train, y_train,  
						trControl = timecontrol_cv_two_day,
						preProcess = c("center","scale"),
						tuneGrid = xgbGrid,
						method = "xgbTree", 
						verbosity = 0)
	  
	  # final grid 
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample
	  )
	  # final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_two_day_ischemic_count = train(X_train, y_train,  
									preProcess = c("center","scale"),
									tuneGrid = final_grid,
									method = "xgbTree", 
									verbosity = 0)
	  # predict
	  preds <- predict(xgb_two_day_ischemic_count, newdata = as.data.frame(test_features_two_day), type = "raw")
	  # metrics
	  rmse <- paste("RMSE of XGB two_day model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_two_day))
	  mae <- paste("MAE of XGB two_day model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_two_day))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 13)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 14)
	  # save model
	  saveRDS(object = xgb_two_day_ischemic_count,file = paste("./results/xgb_two_day_ischemic_count_",site.name,".rda",sep = ""))
	  rm(xgb_two_day_ischemic_count)
	}, silent=TRUE)
	
	print("Support Vector Regression") 
	### SVR
	  #svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters+
	  try({
		  set.seed(1492)
		  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)

		  svm_two_day_ischemic_count <- train(X_train, y_train, 
							  trControl = timecontrol_cv_two_day,
							  tuneGrid = tuneGrid,
							  method = "svmRadial",
							  preProcess = c("center", "scale"),
							  verbosity = 0)
		
		  
		  
		  final_grid <- expand.grid(C =svm_two_day_ischemic_count$bestTune$C,sigma=svm_two_day_ischemic_count$bestTune$sigma )  
		  # final svr model with chosen hyper parameter
		  print("fitting SVR based on chosen hyperparameter")
		  svm_two_day_ischemic_count <- train(X_train, y_train, 
										 tuneGrid = final_grid,
										 method = "svmRadial",
										 preProcess = c("center", "scale"), 
										 verbosity = 0)

		  # predict
		
		  preds <- predict(svm_two_day_ischemic_count, newdata = as.data.frame(test_features_two_day), type = "raw")
		  # metrics
		  rmse <- paste("RMSE of SVM two_day model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_two_day))
		  mae <- paste("MAE of SVM two_day model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_two_day))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 15)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 16)
		  # save model
		  saveRDS(object = svm_two_day_ischemic_count,file = paste("./results/svm_two_day_ischemic_count_",site.name,".rda",sep = ""))
		  rm(svm_two_day_ischemic_count)
	}, silent=TRUE)


	print("Fitting models for Bleeding_count for two day resolution")
	# 2-c.################################################# Models for Bleeding count###################################
	print("split train and test set")
	##split train and test
	##take last 20% rows from two day resolution dataframe
	test_index <- c(nrow(two_day) - floor(nrow(two_day) * 20 * 0.01)):nrow(two_day)
	train_features_two_day <- features_two_day_bleeding[-test_index,]
	test_features_two_day <- features_two_day_bleeding[test_index,]
	
	  train_bleeding_count_two_day <- Bleeding_count_two_day[-test_index]
	  test_bleeding_count_two_day <- Bleeding_count_two_day[test_index]
	  train_two_day <- as.data.frame(cbind(train_features_two_day,train_bleeding_count_two_day))

	print("Poisson")
	### Poisson
	  #fit the model
	  try({
	  set.seed(1492)
		  poisson_two_day_bleeding_count <- glm(train_bleeding_count_two_day ~ ., data = train_two_day, family = poisson(link = "log"))
		  #print summary
		  summary(poisson_two_day_bleeding_count)
		  #predict
		  preds<-predict(poisson_two_day_bleeding_count, newdata = as.data.frame(test_features_two_day), type = "response")
		  #metrics
		  rmse<-paste("RMSE of poisson two_day model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_two_day))
		  mae<- paste("MAE of poisson two_day model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_two_day))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 17)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 18)
		  ##save model
		  saveRDS(object = poisson_two_day_bleeding_count,file = paste("./results/poisson_two_day_bleeding_count_",site.name,".rda",sep = ""))
		  rm(poisson_two_day_bleeding_count)
	  }, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into DMatrix
	  X_train = train_features_two_day
	  y_train = train_bleeding_count_two_day
	  X_test = test_features_two_day 
	  y_test = test_bleeding_count_two_day
	### RF
	try({
		set.seed(1492)
		  #hyper parameter
		  tunegrid <- expand.grid(mtry=c(17,18,19,20))
		  #cross validation
		  #repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
		  #fit model
		  forest <- train(train_bleeding_count_two_day~.,
						data=train_two_day, 
						method='rf', 
						trControl=timecontrol_cv_two_day, 
						preProcess = c("center","scale"),
						metric='RMSE',
						tuneGrid =tunegrid )

		  #final grid 
		  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
		  #final xgb model with chosen hyper parameter
		  print("fitting forest based on chosen hyperparameter")
		  forest_two_day_bleeding_count = train(X_train, y_train,  
												tuneGrid = final_grid,
												preProcess = c("center","scale"),
												method = "rf", 
												verbosity = 0)
		  #predict
		  preds <- predict(forest_two_day_bleeding_count, newdata = as.data.frame(test_features_two_day), type = "raw")
		  #metrics
		  rmse <- paste("RMSE of RF two_day model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_two_day))
		  mae <- paste("MAE of RF two_day model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_two_day))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 19)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 20)
		  #save model
		  saveRDS(object = forest_two_day_bleeding_count,file = paste("./results/forest_two_day_bleeding_count_",site.name,".rda",sep = ""))
		  rm(forest_two_day_bleeding_count)
	}, silent=TRUE)	
	print("Xgboost")
	
	### XGB 
	  #cross-validation method and number of folds
	  #xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	 try({
	 set.seed(1492)
		  xgbGrid   <- expand.grid(nrounds = c(100,200), 
		    max_depth = c(1,3,6,9),
		    colsample_bytree = seq(0.5, 0.9, length.out = 5),
		    eta =c( .2, .1, .05, .01),
		    gamma=0,
		    min_child_weight = 1,
		    subsample = 1)
		  #train the model
		  xgb_model = train(X_train, y_train,  
		                    trControl = timecontrol_cv_two_day,
		                    tuneGrid = xgbGrid,
		                    preProcess = c("center","scale"),
		                    method = "xgbTree", 
		                    verbosity = 0)

		  #final grid 
		  final_grid <- expand.grid(
			nrounds = xgb_model$bestTune$nrounds,
			eta = xgb_model$bestTune$eta,
			max_depth = xgb_model$bestTune$max_depth,
			gamma = xgb_model$bestTune$gamma,
			colsample_bytree = xgb_model$bestTune$colsample_bytree,
			min_child_weight = xgb_model$bestTune$min_child_weight,
			subsample = xgb_model$bestTune$subsample
		  )
		  #final xgb model with chosen hyper parameter
		  print("fitting xgboost based on chosen hyperparameter")
		  xgb_two_day_bleeding_count = train(X_train, y_train,
											 preProcess = c("center","scale"),
											 tuneGrid = final_grid,
											 method = "xgbTree", 
											 verbosity = 0)
		  #predict
		  preds <- predict(xgb_two_day_bleeding_count, newdata = as.data.frame(test_features_two_day), type = "raw")
		  #metrics
		  rmse <- paste("RMSE of XGB two_day model for bleeding count", RMSE(pred = preds,obs = test_bleeding_count_two_day))
		  mae <- paste("MAE of XGB two_day model for bleeding count", MAE(pred = preds,obs = test_bleeding_count_two_day))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE,startRow = 21)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE,startRow = 22)
		  #save model
		  saveRDS(object = xgb_two_day_bleeding_count, file = paste("./results/xgb_two_day_bleeding_count_",site.name,".rda",sep = ""))
		  rm(xgb_two_day_bleeding_count)
	}, silent=TRUE)
	
	
	
	print("Support Vector Regression") 
	### SVR
	  #svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	try({
	set.seed(1492)
		  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
		  svm_two_day_bleeding_count <- train(X_train, y_train,
							  trControl = timecontrol_cv_two_day,
							  tuneGrid = tuneGrid,method = "svmRadial",
							  preProcess = c("center","scale"),
							  verbosity = 0)

		  final_grid <- expand.grid(C =svm_two_day_bleeding_count$bestTune$C,sigma=svm_two_day_bleeding_count$bestTune$sigma )  
		  # final svr model with chosen hyper parameter
		  print("fitting SVR based on chosen hyperparameter")
		  svm_two_day_bleeding_count <- train(X_train, y_train,
											  tuneGrid = final_grid,
											  method = "svmRadial",
											  preProcess = c("center" ,"scale"),
											  verbosity = 0)

		
		  # predict

		  preds <- predict(svm_two_day_bleeding_count, newdata = as.data.frame(test_features_two_day), type = "raw")
		  # metrics
		  rmse <- paste("RMSE of SVM two_day model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_two_day))
		  mae <- paste("MAE of SVM two_day model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_two_day))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Two-day", withFilter = FALSE, startRow = 23)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Two-day", withFilter = FALSE, startRow = 24)
		  #save model
		  saveRDS(object = svm_two_day_bleeding_count,file = paste("./results/svm_two_day_bleeding_count_",site.name,".rda",sep = ""))
		  rm(svm_two_day_bleeding_count)
	}, silent=TRUE)
	
 
  


#3.####################################################Weekly######################################################
print("Models for Weekly count")
openxlsx:::addWorksheet(wb, "Weekly")
	weekly <- na.locf(weekly,na.rm = FALSE)
	weekly <- na.locf(weekly,fromLast = TRUE)

	####features and target 
	features_weekly <- subset(weekly,select = -c(year_weekNum,total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count))
	total_count_weekly <- weekly$total_count
	Ischemic_count_weekly <- weekly$Ischemic_count
	Bleeding_count_weekly <- weekly$Bleeding_count

	print("standardize the input features")
	##standardize the input features 
	#features_weekly <- scale(features_weekly)

	print("split train and test set")
	##split train and test
	##take last 20% rows from two day resolution dataframe
	test_index <- which(as.numeric(substr(weekly$year_weekNum,1,4)) == max(as.numeric(substr(weekly$year_weekNum,1,4))) )
	train_features_weekly <- features_weekly[-test_index,]
	test_features_weekly <- features_weekly[test_index,]


	print("Fitting models for total_count for weekly resolution")
	#3-a.################################################# Models for Total count###################################
	  train_total_count_weekly <- total_count_weekly[-test_index]
	  test_total_count_weekly <- total_count_weekly[test_index]
	  train_weekly <- as.data.frame(cbind(train_features_weekly,train_total_count_weekly))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
	set.seed(1492)
	  poisson_weekly_total_count <- glm(train_total_count_weekly ~ ., data = train_weekly, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_weekly_total_count)
	  #predict
	  preds <- predict(poisson_weekly_total_count, newdata = as.data.frame(test_features_weekly), type = "response")
	  #metrics
	  rmse <- paste("RMSE of Poisson weekly model for total count", RMSE(pred = preds,obs = test_total_count_weekly))
	  mae <- paste("MAE of Poisson weekly model for total count", MAE(pred = preds,obs = test_total_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 1)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 2)
	  ##save model
	  saveRDS(object = poisson_weekly_total_count,file = paste("./results/poisson_weekly_total_count_",site.name,".rda",sep = ""))
	  rm(poisson_weekly_total_count)
	}, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_weekly 
	  y_train = train_total_count_weekly
	  X_test = test_features_weekly 
	  y_test = test_total_count_weekly
	### RF
	 try({
	 set.seed(1492)
	  #hyper parameter
	  tunegrid <- expand.grid(mtry=c(5,6,7,8))
	  #cross validation
	  repeat_cv <- trainControl(method='repeatedcv', 
	                            number=5, 
	                            repeats=3,
	                            verboseIter = TRUE,
	                            returnData = FALSE)
	  #fit model
	  forest <- train(train_total_count_weekly~.,
	                  data=train_weekly, 
	                  method='rf', 
	                  trControl=repeat_cv, 
	                  metric='RMSE',
	                  tuneGrid =tunegrid )
	  
	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_weekly_total_count = train(X_train, y_train,  
	                                    tuneGrid = final_grid,
	                                    preProcess = c("center","scale"),
	                                    method = "rf", 
	                                    verbosity = 0)
	  #predict
	  preds <- predict(forest_weekly_total_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of Random forest weekly model for total count", RMSE(pred = preds,obs = test_total_count_weekly))
	  mae <- paste("MAE of Random forest weekly model for total count", MAE(pred = preds,obs = test_total_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 3)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 4)
	  #save model
	  saveRDS(object = forest_weekly_total_count, file = paste("./results/forest_weekly_total_count_",site.name,".rda",sep = ""))
	  rm(forest_weekly_total_count)
	}, silent=TRUE)
	
	
	print("Xgboost")
	### XGB
	  #cross-validation method and number of folds
	try({
	set.seed(1492)
		  xgb_trcontrol = trainControl(method = "cv", 
		                               number = 5, 
		                               repeats = 3,
		                               verboseIter = TRUE,
		                               returnData = FALSE)
		  #grid space to search for the best hyperparameters
		  xgbGrid   <- expand.grid(nrounds = c(100,200), 
								   max_depth = c(1,3,6,9),
								   colsample_bytree = seq(0.5, 0.9, length.out = 5),
								   eta =c( .2, .1, .05, .01),
								   gamma=0,
								   min_child_weight = 1,
								   subsample = 1)
		  #train the model
		  xgb_model = train(X_train, y_train,  
		                    trControl = xgb_trcontrol,
		                    preProcess = c("center","scale"),
		                    tuneGrid = xgbGrid,
		                    method = "xgbTree", 
		                    verbosity = 0)

		  #final grid 
		  final_grid <- expand.grid(
			nrounds = xgb_model$bestTune$nrounds,
			eta = xgb_model$bestTune$eta,
			max_depth = xgb_model$bestTune$max_depth,
			gamma = xgb_model$bestTune$gamma,
			colsample_bytree = xgb_model$bestTune$colsample_bytree,
			min_child_weight = xgb_model$bestTune$min_child_weight,
			subsample = xgb_model$bestTune$subsample
		  )
		  #final xgb model with chosen hyper parameter
		  print("fitting xgboost based on chosen hyperparameter")
		  xgb_weekly_total_count = train(X_train, y_train,  
		                                 preProcess = c("center","scale"),
		                                 tuneGrid = final_grid,
		                                 method = "xgbTree", 
		                                 verbosity = 0)
		  #predict
		  preds <- predict(xgb_weekly_total_count, newdata = as.data.frame(test_features_weekly), type = "raw")
		  #metrics
		  rmse <-paste("RMSE of XGB weekly model for total count", RMSE(pred = preds,obs = test_total_count_weekly))
		  mae <- paste("MAE of XGB weekly model for total count", MAE(pred = preds,obs = test_total_count_weekly))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 5)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 6)
		  #save model
		  saveRDS(object = xgb_weekly_total_count,file = paste("./results/xgb_weekly_total_count_",site.name,".rda",sep = ""))
	  rm(xgb_weekly_total_count)
	}, silent=TRUE) 
	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)

	  svm_weekly_total_count <- train(X_train, y_train,
					  trControl = svr_trcontrol,
					  tuneGrid = tuneGrid,method = "svmRadial",
					  preProcess = c("center","scale"), 
					  verbosity = 0)
  
	  final_grid <- expand.grid(C =svm_weekly_total_count$bestTune$C,sigma=svm_weekly_total_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_weekly_total_count <- train(X_train, y_train,
					  tuneGrid = final_grid,method = "svmRadial",
					  preProcess = c("center", "scale"),
					  verbosity = 0)

	  # predict

	  preds <- predict(svm_weekly_total_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  # metrics
	  rmse <- paste("RMSE of SVM weekly model for total count",RMSE(pred = preds,obs = test_total_count_weekly))
	  mae <- paste("MAE of SVM weekly model for total count",MAE(pred = preds,obs = test_total_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 7)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 8)
	  # save model
	  saveRDS(object = svm_weekly_total_count,file = paste("./results/svm_weekly_total_count_",site.name,".rda",sep = ""))
	  rm(svm_weekly_total_count)
	}, silent=TRUE)
	  

	print("Fitting models for Ischemic_count for weekly resolution")
	#3-b.################################################# Models for Ischemic count ################################### 
	  train_ischemic_count_weekly <- Ischemic_count_weekly[-test_index]
	  test_ischemic_count_weekly <- Ischemic_count_weekly[test_index]
	  train_weekly <- as.data.frame(cbind(train_features_weekly, train_ischemic_count_weekly))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
	set.seed(1492)
	  poisson_weekly_ischmeic_count <- glm(train_ischemic_count_weekly ~ ., data = train_weekly, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_weekly_ischmeic_count)
	  #predict
	  preds<-predict(poisson_weekly_ischmeic_count, newdata = as.data.frame(test_features_weekly), type = "response")
	  #metrics
	  rmse<-paste("RMSE of poisson weekly model for ischemic count",RMSE(pred = preds,obs = test_ischemic_count_weekly))
	  mae<- paste("MAE of poisson weekly model for ischemic count",MAE(pred = preds,obs = test_ischemic_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 9)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 10)
	  ##save model
	  saveRDS(object = poisson_weekly_ischmeic_count,file = paste("./results/poisson_weekly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(poisson_weekly_ischmeic_count)
	}, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_weekly
	  y_train = train_ischemic_count_weekly
	  X_test = test_features_weekly
	  y_test = test_ischemic_count_weekly
	### RF
	  #hyper parameter
	  try({
		  set.seed(1492)
		  tunegrid <- expand.grid(mtry=c(5,6,7,8))
		  #cross validation
		  repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
		  #fit model
		  forest <- train(train_ischemic_count_weekly~.,
		                  data=train_weekly, 
		                  method='rf',
		                  preProcess = c("center","scale"),
		                  trControl=repeat_cv, 
		                  metric='RMSE',tuneGrid =tunegrid )
		  
		  #final grid 
		  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
		  #final xgb model with chosen hyper parameter
		  print("fitting forest based on chosen hyperparameter")
		  forest_weekly_ischmeic_count = train(X_train, y_train,  
		                                       preProcess = c("center","scale"),
		                                       tuneGrid = final_grid,
		                                       method = "rf", 
		                                       verbosity = 0)
		  #predict
		  preds<-predict(forest_weekly_ischmeic_count, newdata = as.data.frame(test_features_weekly), type = "raw")
		  #metrics
		  rmse<-paste("RMSE of random forest weekly model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_weekly))
		  mae<- paste("MAE of random forest weekly model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_weekly))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 11)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 12)
		  #save model
		  saveRDS(object = forest_weekly_ischmeic_count,file = paste("./results/forest_weekly_ischemic_count_",site.name,".rda",sep = ""))
		  rm(forest_weekly_ischmeic_count)
}, silent=TRUE)
	print("Xgboost")
	### XGB
	  
	  #cross-validation method and number of folds
	  try({
	  set.seed(1492)
	  xgb_trcontrol = trainControl(method = "cv", number = 5, repeats = 3,verboseIter = TRUE, returnData = FALSE)
	  #grid space to search for the best hyperparameters
	  xgbGrid   <- expand.grid(nrounds = c(100,200), 
	                           max_depth = c(1,3,6,9), 
	                           colsample_bytree = seq(0.5, 0.9, length.out = 5),
	                           eta =c( .2, .1, .05, .01),
	                           gamma=0,
	                           min_child_weight = 1,
	                           subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train,  
	                    trControl = xgb_trcontrol,
	                    tuneGrid = xgbGrid,
	                    preProcess = c("center","scale"),
	                    method = "xgbTree", 
	                    verbosity = 0)
	  
	  #final grid 
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_weekly_ischemic_count = train(X_train, y_train,  
	                                    trControl = xgb_trcontrol,
	                                    tuneGrid = final_grid,
	                                    method = "xgbTree", 
	                                    verbosity = 0)
	  #predict
	  preds <- predict(xgb_weekly_ischemic_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of XGB weekly model for ischemic count", RMSE(pred = preds,obs = test_ischemic_count_weekly))
	  mae <- paste("MAE of XGB weekly model for ischemic count", MAE(pred = preds,obs = test_ischemic_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 13)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 14)
	  #save model
	  saveRDS(object = xgb_weekly_ischemic_count,file = paste("./results/xgb_weekly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(xgb_weekly_ischemic_count)
}, silent=TRUE)


	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, 0.5, 1), sigma = 0.1)

	  svm_weekly_ischemic_count <- train(X_train, y_train,
	                                     trControl = svr_trcontrol,
	                                     tuneGrid = tuneGrid,
	                                     method = "svmRadial",
	                                     preProcess = c("center","scale"),
	                                     verbosity = 0)

	  final_grid <- expand.grid(C =svm_weekly_ischemic_count$bestTune$C,sigma=svm_weekly_ischemic_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_weekly_ischemic_count <- train(X_train, y_train,
	                                     tuneGrid = final_grid,
	                                     method = "svmRadial",
	                                     preProcess = c("center","scale"), 
	                                     verbosity = 0)

	# predict

	  preds <- predict(svm_weekly_ischemic_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of SVM weekly model for ischemic count",RMSE(pred = preds, obs = test_ischemic_count_weekly))
	  mae <- paste("MAE of SVM weekly model for ischemic count",MAE(pred = preds, obs = test_ischemic_count_weekly))
	  openxlsx:::writeData(wb = wb, x = rmse,sheet = "Weekly", withFilter = FALSE, startRow = 15)
	  openxlsx:::writeData(wb = wb, x = mae,sheet = "Weekly", withFilter = FALSE, startRow = 16)
	  #save model
	  saveRDS(object = svm_weekly_ischemic_count,file = paste("./results/svm_weekly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(svm_weekly_ischemic_count)
	}, silent=TRUE)

	 
	 

	print("Fitting models for Bleeding_count for weekly resolution")
	#3-c.################################################# Models for Bleeding count###################################
	  train_bleeding_count_weekly <- Bleeding_count_weekly[-test_index]
	  test_bleeding_count_weekly <- Bleeding_count_weekly[test_index]
	  train_weekly <- as.data.frame(cbind(train_features_weekly,train_bleeding_count_weekly))

	print("Poisson")
	### Poisson
	try({
	set.seed(1492)
	  #fit the model
	  poisson_weekly_bleeding_count <- glm(train_bleeding_count_weekly ~ ., data = train_weekly, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_weekly_bleeding_count)
	  #predict
	  preds<-predict(poisson_weekly_bleeding_count, newdata = as.data.frame(test_features_weekly), type = "response")
	  #metrics
	  rmse<-paste("RMSE of poisson two_day model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_weekly))
	  mae<- paste("MAE of poisson two_day model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 17)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 18)
	  ##save model
	  saveRDS(object = poisson_weekly_bleeding_count,file = paste("./results/poisson_weekly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(poisson_weekly_bleeding_count)
	 }, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into DMatrix
	  X_train = train_features_weekly
	  y_train = train_bleeding_count_weekly
	  X_test = test_features_weekly 
	  y_test = test_bleeding_count_weekly
	### RF
	  #hyper parameter
	  try({
	  set.seed(1492)
		  tunegrid <- expand.grid(mtry=c(5,6,7,8))
		  #cross validation
		  repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
		  #fit model
		  forest <- train(train_bleeding_count_weekly~.,
		                  data=train_weekly, 
		                  method='rf',
		                  preProcess = c("center","scale"),
		                  trControl=repeat_cv, 
		                  metric='RMSE',
		                  tuneGrid =tunegrid )

		  #final grid 
		  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
		  #final xgb model with chosen hyper parameter
		  print("fitting forest based on chosen hyperparameter")
		  forest_weekly_bleeding_count = train(X_train, y_train, 
		                                       tuneGrid = final_grid,
		                                       preProcess = c("center","scale"),
		                                       method = "rf", 
		                                       verbosity = 0)
		  #predict
		  preds<-predict(forest_weekly_bleeding_count, newdata = as.data.frame(test_features_weekly), type = "raw")
		  #metrics
		  rmse<-paste("RMSE of RF weekly model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_weekly))
		  mae<- paste("MAE of RF weekly model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_weekly))
		  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 19)
		  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 20)
		  #save model
		  saveRDS(object = forest_weekly_bleeding_count,file = paste("./results/forest_weekly_bleeding_count_",site.name,".rda",sep = ""))
		  rm(forest_weekly_bleeding_count)
	}, silent=TRUE)
	print("Xgboost")
	### XGB 
	  #cross-validation method and number of folds
	try({
	set.seed(1492)
   	  xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	  xgbGrid   <- expand.grid(nrounds = c(100,200), max_depth = c(1,3,6,9),colsample_bytree = seq(0.5, 0.9, length.out = 5),eta =c( .2, .1, .05, .01),gamma=0,min_child_weight = 1,subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train,  
	                    trControl = xgb_trcontrol,
	                    preProcess = c("center","scale"),
	                    tuneGrid = xgbGrid,
	                    method = "xgbTree", 
	                    verbosity = 0)

	  #final grid 
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_weekly_bleeding_count = train(X_train, y_train,
	                                    preProcess = c("center","scale"),
	                                    tuneGrid = final_grid,
	                                    method = "xgbTree",
	                                    verbosity = 0)
	  #predict
	  preds<-predict(xgb_weekly_bleeding_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  #metrics
	  rmse<-paste("RMSE of XGB weekly model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_weekly))
	  mae<- paste("MAE of XGB weekly model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE,startRow = 21)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE,startRow = 22)
	  #save model
	  saveRDS(object = xgb_weekly_bleeding_count,file = paste("./results/xgb_weekly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(xgb_weekly_bleeding_count)
}, silent=TRUE)


	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)

	  svm_weekly_bleeding_count <- train(X_train, y_train, 
	                                     trControl = svr_trcontrol,
	                                     tuneGrid = tuneGrid,
	                                     method = "svmRadial",
	                                     preProcess = c("center", "scale"), 
	                                     verbosity = 0)


	  final_grid <- expand.grid(C =svm_weekly_bleeding_count$bestTune$C,sigma=svm_weekly_bleeding_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_weekly_bleeding_count <- train(X_train, y_train, 
					     tuneGrid = final_grid,
					     method = "svmRadial",
					     preProcess = c("center", "scale"), 
					     verbosity = 0)

	  preds <- predict(svm_weekly_bleeding_count, newdata = as.data.frame(test_features_weekly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of SVM weekly model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_weekly))
	  mae<- paste("MAE of SVM weekly model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_weekly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Weekly", withFilter = FALSE, startRow = 23)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Weekly", withFilter = FALSE, startRow = 24)
	  #save model
	  saveRDS(object = svm_weekly_bleeding_count,file = paste("./results/svm_weekly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(svm_weekly_bleeding_count)
}, silent=TRUE)
	 
  

#4.####################################################Monthly#####################################################
print("Models for Monthly count")
openxlsx:::addWorksheet(wb, "Monthly")
	monthly <- na.locf(monthly,na.rm = FALSE)
	monthly <- na.locf(monthly,fromLast = TRUE)

	### features and target 
	features_monthly <- subset(monthly,select = -c(year_month,total_count,count_I63,count_I61,count_I60,Ischemic_count,Bleeding_count,PT.station))
	total_count_monthly <- monthly$total_count
	Ischemic_count_monthly <- monthly$Ischemic_count
	Bleeding_count_monthly<- monthly$Bleeding_count


	print("split train and test set")
	# split to train and test
	# take last 20% rows from two day resolution dataframe
	test_index <- which(as.numeric(substr(monthly$year_month, 1, 4)) == max(as.numeric(substr(monthly$year_month, 1, 4))) )
	train_features_monthly <- features_monthly[-test_index, ]
	test_features_monthly <- features_monthly[test_index, ]


	print("Fitting models for total_count for weekly resolution")
	# 4-a.################################################# Models for Total count###################################
	train_total_count_monthly <- total_count_monthly[-test_index]
	test_total_count_monthly <- total_count_monthly[test_index]
	train_monthly <- as.data.frame(cbind(train_features_monthly, train_total_count_monthly))
	
	print("Poisson")
	### Poisson
	  # fit the model
	try({
	set.seed(1492)
	  poisson_monthly_total_count <- glm(train_total_count_monthly ~ ., data = train_monthly, family = poisson(link = "log"))
	  # print summary
	  summary(poisson_monthly_total_count)
	  # predict
	  preds <- predict(poisson_monthly_total_count, newdata = as.data.frame(test_features_monthly), type = "response")
	  # metrics
	  rmse <- paste("RMSE of Poisson monthly model for total count", RMSE(pred = preds, obs = test_total_count_monthly))
	  mae <- paste("MAE of Poisson monthly model for total count", MAE(pred = preds, obs = test_total_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse, sheet = "Monthly", withFilter = FALSE,startRow = 1)
	  openxlsx:::writeData(wb = wb,x = mae, sheet = "Monthly", withFilter = FALSE,startRow = 2)
	  # save model
	  saveRDS(object = poisson_monthly_total_count, file = paste("./results/poisson_monthly_total_count_", site.name,".rda", sep = ""))
	  rm(poisson_monthly_total_count)
    }, silent=TRUE)

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_monthly
	  y_train = train_total_count_monthly
	  X_test = test_features_monthly 
	  y_test = test_total_count_monthly
	### RF
	  #hyper parameter
	try({
	set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(5,6,7,8))
	  #cross validation
	  repeat_cv <- trainControl(method='repeatedcv', 
	                            number=5, 
	                            repeats=3, 
	                            verboseIter = TRUE, 
	                            returnData = FALSE)
	  #fit model
	  forest <- train(train_total_count_monthly~.,
	                  data = train_monthly, 
	                  method='rf', 
	                  preProcess = c("center","scale"),
	                  trControl = repeat_cv, 
	                  metric='RMSE', 
	                  tuneGrid = tunegrid)
	  
	  # final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  # final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_monthly_total_count = train(X_train, y_train,
	                                     tuneGrid = final_grid,
	                                     preProcess = c("center","scale"),
	                                     method = "rf", 
	                                     verbosity = 0)
	  # predict
	  preds <- predict(forest_monthly_total_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  # metrics
	  rmse <- paste("RMSE of Random forest monthly model for total count",RMSE(pred = preds,obs = test_total_count_monthly))
	  mae <- paste("MAE of Random forest monthly model for total count",MAE(pred = preds,obs = test_total_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 3)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 4)
	  #save model
	  saveRDS(object = forest_monthly_total_count,file = paste("./results/forest_monthly_total_count_",site.name,".rda",sep = ""))
	  rm(forest_monthly_total_count)
	}, silent=TRUE)

	
	print("Xgboost")
	### XGB
	try({
	set.seed(1492)
	  #cross-validation method and number of folds
	  xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	  xgbGrid   <- expand.grid(nrounds = c(100,200), max_depth = c(1,3,6,9),colsample_bytree = seq(0.5, 0.9, length.out = 5),eta =c( .2, .1, .05, .01),gamma=0,min_child_weight = 1,subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train,  
	                    trControl = xgb_trcontrol,
	                    tuneGrid = xgbGrid,
	                    preProcess = c("center","scale"),
	                    method = "xgbTree",
	                    verbosity = 0)

	  #final grid 
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_monthly_total_count = train(X_train, y_train, 
	                                  trControl = xgb_trcontrol,
	                                  tuneGrid = final_grid,
	                                  method = "xgbTree",
	                                  verbosity = 0)
	  #predict
	  preds<-predict(xgb_monthly_total_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of XGB monthly model for total count", RMSE(pred = preds,obs = test_total_count_monthly))
	  mae <- paste("MAE of XGB monthly model for total count", MAE(pred = preds,obs = test_total_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 5)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 6)
	  
	  #save model
	  saveRDS(object = xgb_monthly_total_count,file = paste("./results/xgb_monthly_total_count_",site.name,".rda",sep = ""))
	  rm(xgb_monthly_total_count)
	}, silent=TRUE)
	
	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
	  svm_monthly_total_count <- train(X_train, y_train, 
	                                   trControl = svr_trcontrol, 
	                                   tuneGrid = tuneGrid, 
	                                   method = "svmRadial", 
	                                   preProcess = c("center","scale"), 
	                                   verbosity = 0)



	  final_grid <- expand.grid(C =svm_monthly_total_count$bestTune$C,sigma=svm_monthly_total_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_monthly_total_count <- train(X_train, y_train,  
	                                   tuneGrid = final_grid,method = "svmRadial", 
	                                   preProcess = c("center","scale"), 
	                                   verbosity = 0)

	  preds <- predict(svm_monthly_total_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  #metrics
	  rmse <- paste("RMSE of SVM monthly model for total count", RMSE(pred = preds,obs = test_total_count_monthly))
	  mae <- paste("MAE of SVM monthly model for total count", MAE(pred = preds,obs = test_total_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 7)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 8)
	  #save model
	  saveRDS(object = svm_monthly_total_count,file = paste("./results/svm_monthly_total_count_",site.name,".rda",sep = ""))
	  rm(svm_monthly_total_count)
	}, silent=TRUE)
	  

	print("Fitting models for Ischemic_count for monthly resolution")
	#4-b.################################################# Models for Ischemic count################################### 
	  train_ischemic_count_monthly <- Ischemic_count_monthly[-test_index]
	  test_ischemic_count_monthly <- Ischemic_count_monthly[test_index]
	  train_monthly <- as.data.frame(cbind(train_features_monthly,train_ischemic_count_monthly))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
	set.seed(1492)
	  poisson_monthly_ischmeic_count <- glm(train_ischemic_count_monthly ~ ., data = train_monthly, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_monthly_ischmeic_count)
	  #predict
	  preds<-predict(poisson_monthly_ischmeic_count, newdata = as.data.frame(test_features_monthly), type = "response")
	  #metrics
	  rmse<-paste("RMSE of poisson monthly model for ischemic count",RMSE(pred = preds,obs = test_ischemic_count_monthly))
	  mae<- paste("MAE of poisson monthly model for ischemic count",MAE(pred = preds,obs = test_ischemic_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 9)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 10)
	  ##save model
	  saveRDS(object = poisson_monthly_ischmeic_count,file = paste("./results/poisson_monthly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(poisson_monthly_ischmeic_count)
	}, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into Matrix
	  X_train = train_features_monthly
	  y_train = train_ischemic_count_monthly
	  X_test = test_features_monthly
	  y_test = test_ischemic_count_monthly
	### RF
	  #hyper parameter
	try({
	set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(5,6,7,8))
	  #cross validation
	  repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
	  #fit model
	  forest <- train(train_ischemic_count_monthly~.,
	                  data=train_monthly,
	                  method='rf',
	                  preProcess = c("center","scale"),
	                  trControl=repeat_cv, 
	                  metric='RMSE',
	                  tuneGrid =tunegrid )
	  
	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_monthly_ischmeic_count = train(X_train, y_train,  
	                                        tuneGrid = final_grid,
	                                        preProcess = c("center","scale"),
	                                        method = "rf", 
	                                        verbosity = 0)
	  #predict
	  preds<-predict(forest_monthly_ischmeic_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  #metrics
	  rmse<-paste("RMSE of random forest monthly model for ischemic count",RMSE(pred = preds,obs = test_ischemic_count_monthly))
	  mae<- paste("MAE of random forest monthly model for ischemic count",MAE(pred = preds,obs = test_ischemic_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 11)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 12)
	  #save model
	  saveRDS(object = forest_monthly_ischmeic_count,file = paste("./results/forest_monthly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(forest_monthly_ischmeic_count)
	}, silent=TRUE)

	print("Xgboost")
	### XGB
	  
	  #cross-validation method and number of folds
	try({
	set.seed(1492)
	  xgb_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyperparameters
	  xgbGrid   <- expand.grid(nrounds = c(100,200), max_depth = c(1,3,6,9),colsample_bytree = seq(0.5, 0.9, length.out = 5),eta =c( .2, .1, .05, .01),gamma=0,min_child_weight = 1,subsample = 1)
	  #train the model
	  xgb_model = train(X_train, y_train, 
	                    trControl = xgb_trcontrol,
	                    tuneGrid = xgbGrid,
	                    preProcess = c("center","scale"),
	                    method = "xgbTree",
	                    verbosity = 0)
	  
	  #final grid 
	  final_grid <- expand.grid(
		nrounds = xgb_model$bestTune$nrounds,
		eta = xgb_model$bestTune$eta,
		max_depth = xgb_model$bestTune$max_depth,
		gamma = xgb_model$bestTune$gamma,
		colsample_bytree = xgb_model$bestTune$colsample_bytree,
		min_child_weight = xgb_model$bestTune$min_child_weight,
		subsample = xgb_model$bestTune$subsample
	  )
	  #final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_monthly_ischemic_count = train(X_train, y_train,  
	                                     preProcess = c("center","scale"),
	                                     tuneGrid = final_grid,
	                                     method = "xgbTree", verbosity = 0)
	  #predict
	  preds<-predict(xgb_monthly_ischemic_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  #metrics
	  rmse<-paste("RMSE of XGB monthly model for ischemic count",RMSE(pred = preds,obs = test_ischemic_count_monthly))
	  mae<- paste("MAE of XGB monthly model for ischemic count",MAE(pred = preds,obs = test_ischemic_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 13)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 14)
	  #save model
	  saveRDS(object = xgb_monthly_ischemic_count,file = paste("./results/xgb_monthly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(xgb_monthly_ischemic_count)
}, silent=TRUE)
	
	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)
	  svm_monthly_ischemic_count <- train(X_train, y_train,
	                                      trControl = svr_trcontrol,
	                                      tuneGrid = tuneGrid,
	                                      method = "svmRadial",
	                                      preProcess = c("center", "scale"),
	                                      verbosity = 0)


	  final_grid <- expand.grid(C =svm_monthly_ischemic_count$bestTune$C,sigma=svm_monthly_ischemic_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_monthly_ischemic_count <- train(X_train, y_train,
	                                      tuneGrid = final_grid,method = "svmRadial",
	                                      preProcess = c("center", "scale"),
	                                      verbosity = 0)

	  preds <- predict(svm_monthly_ischemic_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  # metrics
	  rmse <- paste("RMSE of SVM monthly model for ischemic count", RMSE(pred = preds, obs = test_ischemic_count_monthly))
	  mae <- paste("MAE of SVM monthly model for ischemic count", MAE(pred = preds, obs = test_ischemic_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE, startRow = 15)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE, startRow = 16)
	  #save model
	  saveRDS(object = svm_monthly_ischemic_count,file = paste("./results/svm_monthly_ischemic_count_",site.name,".rda",sep = ""))
	  rm(svm_monthly_ischemic_count)
	}, silent=TRUE)

	print("Fitting models for Bleeding_count for monthly resolution")
	#4-c.################################################# Models for Bleeding count###################################
	  train_bleeding_count_monthly <- Bleeding_count_monthly[-test_index]
	  test_bleeding_count_monthly <- Bleeding_count_monthly[test_index]
	  train_monthly <- as.data.frame(cbind(train_features_monthly,train_bleeding_count_monthly))

	print("Poisson")
	### Poisson
	  #fit the model
	try({
	set.seed(1492)
	  poisson_monthly_bleeding_count <- glm(train_bleeding_count_monthly ~ ., data = train_monthly, family = poisson(link = "log"))
	  #print summary
	  summary(poisson_monthly_bleeding_count)
	  #predict
	  preds<-predict(poisson_monthly_bleeding_count, newdata = as.data.frame(test_features_monthly), type = "response")
	  #metrics
	  rmse<-paste("RMSE of poisson Monthly model for bleeding count",RMSE(pred = preds,obs = test_bleeding_count_monthly))
	  mae<- paste("MAE of poisson Monthly model for bleeding count",MAE(pred = preds,obs = test_bleeding_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 17)
	  openxlsx:::writeData(wb = wb,x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 18)
	  ##save model
	  saveRDS(object = poisson_monthly_bleeding_count,file = paste("./results/poisson_monthly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(poisson_monthly_bleeding_count)
	}, silent=TRUE)
	### ARIMA

	print("Random Forest")
	#convert train and test into DMatrix
	  X_train = train_features_monthly
	  y_train = train_bleeding_count_monthly
	  X_test = test_features_monthly 
	  y_test = test_bleeding_count_monthly
	### RF
	  #hyper parameter
	try({
	set.seed(1492)
	  tunegrid <- expand.grid(mtry=c(5,6,7,8))
	  #cross validation
	  repeat_cv <- trainControl(method='repeatedcv', number=5, repeats=3,verboseIter = TRUE,returnData = FALSE)
	  #fit model
	  forest <- train(train_bleeding_count_monthly~.,
	                  data=train_monthly,
	                  method='rf', 
	                  trControl=repeat_cv,
	                  preProcess = c("center","scale"),
	                  metric='RMSE',
	                  tuneGrid =tunegrid )

	  #final grid 
	  final_grid <- expand.grid(mtry = forest$bestTune$mtry)
	  #final xgb model with chosen hyper parameter
	  print("fitting forest based on chosen hyperparameter")
	  forest_monthly_bleeding_count = train(X_train, y_train,
	                                        preProcess = c("center","scale"),
	                                        tuneGrid = final_grid,
	                                        method = "rf", 
	                                        verbosity = 0)
	  
	  # predict
	  preds <- predict(forest_monthly_bleeding_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  
	  # metrics
	  rmse<-paste("RMSE of RF monthly model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_monthly))
	  mae<- paste("MAE of RF monthly model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_monthly))
	  openxlsx:::writeData(wb = wb, x = rmse,sheet = "Monthly", withFilter = FALSE,startRow = 19)
	  openxlsx:::writeData(wb = wb, x = mae,sheet = "Monthly", withFilter = FALSE,startRow = 20)
	  
	  # save model
	  saveRDS(object = forest_monthly_bleeding_count,file = paste("./results/forest_monthly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(forest_monthly_bleeding_count)
	}, silent=TRUE)
	
	
	print("Xgboost")
	### XGB 
	  # cross-validation method and number of folds
	try({
	set.seed(1492)
	  xgb_trcontrol = trainControl(method = "cv",
	                               number = 5, 
	                               repeats = 3,
	                               verboseIter = TRUE,
	                               returnData = FALSE)
	  
	  # grid space to search for the best hyperparameters
	  xgbGrid   <- expand.grid(nrounds = c(100,200), 
	                           max_depth = c(1,3,6,9),
	                           colsample_bytree = seq(0.5, 0.9, length.out = 5),
	                           eta =c( .2, .1, .05, .01),
	                           gamma=0,
	                           min_child_weight = 1,
	                           subsample = 1)
	  # train the model
	  xgb_model = train(X_train, y_train, 
	                    trControl = xgb_trcontrol,
	                    tuneGrid = xgbGrid,
	                    preProcess = c("center","scale"),
	                    method = "xgbTree", 
	                    verbosity = 0)

	  # tuned grid 
	  final_grid <- expand.grid(nrounds = xgb_model$bestTune$nrounds,
	                            eta = xgb_model$bestTune$eta,
	                            max_depth = xgb_model$bestTune$max_depth,
	                            gamma = xgb_model$bestTune$gamma,
	                            colsample_bytree = xgb_model$bestTune$colsample_bytree,
	                            min_child_weight = xgb_model$bestTune$min_child_weight,
	                            subsample = xgb_model$bestTune$subsample
	  )
	  # final xgb model with chosen hyper parameter
	  print("fitting xgboost based on chosen hyperparameter")
	  xgb_monthly_bleeding_count = train(X_train, y_train, 
	                                     preProcess = c("center","scale"),
	                                     tuneGrid = final_grid,
	                                     method = "xgbTree",
	                                     verbosity = 0)
	  #predict
	  preds<-predict(xgb_monthly_bleeding_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  #metrics
	  rmse<-paste("RMSE of XGB monthly model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_monthly))
	  mae<- paste("MAE of XGB monthly model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse, sheet = "Monthly", withFilter = FALSE,startRow = 21)
	  openxlsx:::writeData(wb = wb,x = mae, sheet = "Monthly", withFilter = FALSE,startRow = 22)
	  #save model
	  saveRDS(object = xgb_monthly_bleeding_count,file = paste("./results/xgb_monthly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(xgb_monthly_bleeding_count)
}, silent=TRUE)


	print("Support Vector Regression") 
	### SVR
	try({
	set.seed(1492)
	  svr_trcontrol = trainControl(method = "cv",number = 5, repeats = 3,verboseIter = TRUE,returnData = FALSE)
	  #grid space to search for the best hyper parameters
	  tuneGrid <- expand.grid(C = c(0.25, .5, 1),sigma = 0.1)

	  svm_monthly_bleeding_count <- train(X_train, y_train,
	                                      trControl = svr_trcontrol,
	                                      tuneGrid = tuneGrid,
	                                      method = "svmRadial",
	                                      preProcess = c("center", "scale"), # features are already centered
	                                      verbosity = 0)
	  final_grid <- expand.grid(C =svm_monthly_bleeding_count$bestTune$C,sigma=svm_monthly_bleeding_count$bestTune$sigma )  
	  #final svr model with chosen hyper parameter
	  print("fitting SVR based on chosen hyperparameter")
	  svm_monthly_bleeding_count <- train(X_train, y_train,
	                                      tuneGrid = final_grid,
	                                      method = "svmRadial",
	                                      preProcess = c("center", "scale"), # features should be already centered
	                                      verbosity = 0)

	  preds <- predict(svm_monthly_bleeding_count, newdata = as.data.frame(test_features_monthly), type = "raw")
	  # metrics
	  rmse <- paste("RMSE of SVM monthly model for bleeding count", RMSE(pred = preds, obs = test_bleeding_count_monthly))
	  mae <- paste("MAE of SVM monthly model for bleeding count", MAE(pred = preds, obs = test_bleeding_count_monthly))
	  openxlsx:::writeData(wb = wb,x = rmse, sheet = "Monthly", withFilter = FALSE, startRow = 23)
	  openxlsx:::writeData(wb = wb,x = mae, sheet = "Monthly", withFilter = FALSE, startRow = 24)
	  # save model
	  # path not OS agnostic. consider file.path()
	  saveRDS(object = svm_monthly_bleeding_count,file = paste("./results/svm_monthly_bleeding_count_",site.name,".rda",sep = ""))
	  rm(svm_monthly_bleeding_count)
	}, silent=TRUE)
	 
	 
# save final output	
openxlsx:::saveWorkbook(wb, "Results/ModelMetrics_MIRACUM_WESTORM.xlsx", overwrite = TRUE)

end.time <- Sys.time()

print(end.time - start.time)
print("Modeling with fixed window as False is done")
