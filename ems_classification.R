library(e1071)
library(RMySQL)
library(jsonlite)

source('ems_feature_sets.R')

source('ems_db_interface.R')

classification <- function(){
	
	modes <- vector(mode = 'list', length = 2)
	names(modes) <- c('holdout', 'self')
	
	datasets <- vector(mode = 'list')
	datasets$atmospherical <- get_dataset(1)
	datasets$postrock <- get_dataset(2)
	datasets$techno <- get_dataset(3)
	datasets$ambient <- rbind(datasets$atmospherical, datasets$postrock)
	datasets$ambient_techno <- rbind(datasets$ambient, datasets$techno)
	evals <- vector(mode = 'list', length = length(modes))
	names(evals) <- names(modes)
	
	
	feature_sets <- get_feature_sets()
	
	for(mode in names(modes)){
	
		evals_datasets <- vector(mode = 'list', length = length(datasets))
		names(evals_datasets) <- names(datasets)
	
		for(dataset in names(datasets)){
		
			results_sets <- vector(mode = 'list', length = length(feature_sets))
			names(results_sets) <- names(feature_sets)
			evals_sets <- vector(mode = 'list', length = length(feature_sets))
			names(evals_sets) <- names(feature_sets)
	
			for(set in names(feature_sets)){
			
				if(mode == 'holdout'){
					results_sets[[set]] <- class_svm(datasets[[dataset]], descriptors = feature_sets[[set]])
				}
				else{
					results_sets[[set]] <- class_svm(datasets[[dataset]], descriptors = feature_sets[[set]], self_class = TRUE)

				}
			
				evals_sets[[set]] <- evaluate(results_sets[[set]])
			}
			
			evals_datasets[[dataset]] <- evals_sets	
			
		}
		
		evals[[mode]] <- evals_datasets
		
	}
	
	return(evals)
	
}


class_svm <- function(dataset, descriptors = NULL, self_class = FALSE){
		
	train_test_datasets <- get_train_test_datasets(dataset, descriptors)
	
	# str(train_test_datasets)
	
	model <- svm(artist_id~., data = train_test_datasets$train)
	
	
	if(self_class){
		
		train_test <- rbind(train_test_datasets$train, train_test_datasets$test)
		
		predictions <- predict(model, train_test[,-1])
		ground_truth <- train_test[,1]
		
	}
	else{
		predictions <- predict(model, train_test_datasets$test[,-1])	
		ground_truth <- train_test_datasets$test[,1]
	}
	
	tab <- table(pred = predictions, true <- ground_truth)	
	
	results <- vector(mode = 'list')
	
	results$predictions <- predictions
	results$ground_truth <- ground_truth
	results$confusion_table <- tab
	
	return(results)

}


get_train_test_datasets <- function(dataset, descriptors = NULL){
	
	
	train_test_datasets <- vector(mode = "list")
	
	if(is.null(descriptors)){
		descriptors <- names(dataset)[4:length(names(dataset))]		
	}
	
	train_test <- get_train_test_albums(dataset)
	
	train_dataset <- dataset[which(dataset$album_id %in% train_test$train),]
	test_dataset <- dataset[which(dataset$album_id %in% train_test$test),]
	
	train_dataset <- train_dataset[, c('artist_id', paste(descriptors))]
	test_dataset <- test_dataset[, c('artist_id', paste(descriptors))]
	
	train_dataset$artist_id <- as.factor(train_dataset$artist_id)
	test_dataset$artist_id <- as.factor(test_dataset$artist_id)
	
	train_test_datasets$train <- train_dataset
	train_test_datasets$test <- test_dataset
	
	return(train_test_datasets)
	
}

get_train_test_albums <- function(dataset){
	
	grouped <- split(dataset, as.factor(dataset$artist_id))
	train_test_albums <- vector(mode = 'list', length = 0)
	train_test_albums$train <- numeric(0)
	train_test_albums$test <- numeric(0)
	
	for(artist in unique(dataset$artist_id)){
		
		albums <- unique(grouped[[paste(artist)]]$album_id)
		years <- unlist(lapply(albums, get_album_year))
		ord_albums <- albums[order(years)]
		train <- ord_albums[seq(1, length(ord_albums), 2)]
		test <- ord_albums[seq(2, length(ord_albums), 2)]
		train_test_albums$train <- c(train_test_albums$train, train)
		train_test_albums$test <- c(train_test_albums$test, test)
	}
	
	return(train_test_albums)
	
}

get_album_year <- function(album_id){
	
	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root')
	
	year <- dbGetQuery(db, paste('SELECT year FROM albums_info WHERE id = ', album_id))$year
	
	dbDisconnect(db)
	
	return(year)
	
}

evaluate <- function(class_results){
	
	df <- as.data.frame(class_results$confusion_table)
	names(df) <- c('pred', 'real', 'freq')
	df <- df[, c('real', 'pred', 'freq')]
	
	eval <- vector(mode = 'list')
	all_tp <- 0
	all_prec <- numeric(0)
	all_rec <- numeric(0)
	all_f1 <- numeric(0)
	
	art_ids <- unique(df$real)
	art_names <- unlist(lapply(art_ids, get_artist_name))
	artists <- data.frame('id' = art_ids, 'name' = art_names)
	
	for(artist in art_ids){
		
		real <- df[which(df$real == artist),]
		pred <- df[which(df$pred == artist),]
		
		tp <- sum(real[which(real$pred == artist), 'freq'])
		fn <- sum(real[which(real$pred != artist), 'freq'])
		fp <- sum(pred[which(pred$real != artist), 'freq'])
		
		prec <- tp / (tp + fp)
		rec <- tp / (tp + fn)
		f1 <- (2*prec*rec) / (prec + rec)
		
		name <- get_artist_name(artist)
		
		eval[[name]] <- vector(mode = 'list')
		eval[[name]][['precision']] <- prec
		eval[[name]][['recall']] <- rec
		eval[[name]][['f1-score']] <- f1
		
		other_artists <- art_ids[-which(art_ids == artist)]
		times_with <- numeric(length(other_artists))
		times_by <- numeric(length(other_artists))
		
		for (i in 1:length(times_with)){
				
			times_with[i] = real[which(real$pred == other_artists[i]), 'freq']
			times_by[i] = pred[which(pred$real == other_artists[i]), 'freq']
			
		}
		
		other_artists = replace_artist(other_artists, artists)
		
		eval[[name]][['conf_with']] <- data.frame('name' = other_artists, 
			'times' = times_with)
		eval[[name]][['conf_by']] <- data.frame('name' = other_artists, 
			'times' = times_by)
		
		
		all_tp <- all_tp + tp
		all_prec <- c(all_prec, prec)
		all_rec <- c(all_rec, rec)
		all_f1 <- c(all_f1, f1)
	
	}
	
	eval[['accuracy']] <- all_tp / sum(class_results$confusion_table)
	eval[['precision']] <- mean(all_prec)
	eval[['recall']] <- mean(all_rec)
	eval[['f1-score']] <- mean(all_f1)
	
	
	predictions <- data.frame('real' = class_results$ground_truth, 
		'pred' = class_results$predictions)
	
	predictions$pred <- replace_artist(predictions$pred, artists)
	predictions$real <- replace_artist(predictions$real, artists)
	eval[['predictions']] <-predictions 
	
	confusions <- predictions[which(predictions$real != predictions$pred),]
	paths <- unlist(lapply(row.names(confusions), get_path))
	eval[['confusions']] <- data.frame('path' = paths, confusions)
	

	
	return(eval)
		
}

replace_artist <- function(array, artists){
	
	aux <- character(length(array))
	
	for(i in 1:length(array)){
		
		artist <- array[i]
		name <- as.character(artists[which(artists$id == artist), ]$name)
		aux[i] <- name
		
	}
	
	return(aux)
	
}