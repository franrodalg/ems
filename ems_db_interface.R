library(RMySQL)


get_artist_name <- function(artist_id){
	
	db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')
	
	
	artist_name <- dbGetQuery(db, 
		paste('SELECT name FROM artists_info WHERE id = ', artist_id))$name
	
	dbDisconnect(db)
	
	return(artist_name)
	
}

get_descriptors <- function(){
	
	db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')
	
	
	all <- dbReadTable(db, 'excerpts_descriptors')
	
	desc_with_na <- names(all[, colMeans(is.na(all)) != 0])
	descriptors <- names(all)[2:length(names(all))]
	descriptors <- subset(descriptors, ! descriptors %in% desc_with_na)
	
	rm(all)
	
	dbDisconnect(db)
	
	return(descriptors)
	
	
}

get_dataset <- function(dataset_id, descriptors = NULL){
	
	db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')
	
	query <- paste('SELECT albums_artists.*, excerpts_descriptors.* FROM excerpts_descriptors INNER JOIN excerpts ON excerpts_descriptors.excerpt_id  = excerpts.id INNER JOIN albums_artists ON excerpts.album_id = albums_artists.album_id INNER JOIN excerpts_datasets ON excerpts.id = excerpts_datasets.excerpt_id WHERE excerpts_datasets.dataset_id = ' , dataset_id)

	dataset <- dbGetQuery(db, query)

	if(is.null(descriptors)){
		
		descriptors <- get_descriptors()
	}

	dataset <- dataset[, c('excerpt_id', 'artist_id', 'album_id', descriptors)]
	dataset <- dataset[order(dataset$excerpt_id),]
	row.names(dataset) <- dataset$excerpt_id
	
	dbDisconnect(db)
	return(dataset)
	
}