library(e1071)
library(RMySQL)


class_svm <- function(dataset, descriptors = NULL){
		
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
	
	model <- svm(artist_id~., data = train_dataset)
	
	prediction <- predict(model, test_dataset[,-1])
	
	tab <- table(pred = prediction, true <- test_dataset[,1])
	
	print(tab)
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
