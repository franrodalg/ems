library(RMySQL)


dataset_desc_analysis <- function(dataset_id){
	
	
	db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')
	
	
	all <- dbReadTable(db, 'excerpts_descriptors')
	
	desc_with_na <- names(all[, colMeans(is.na(all)) != 0])
	descriptors <- names(all)[2:length(names(all))]
	descriptors <- subset(descriptors, ! descriptors %in% desc_with_na)
	
	rm(all)
	
	print(length(descriptors))
	
	
	query <- paste('SELECT albums_artists.*, excerpts_descriptors.* FROM excerpts_descriptors INNER JOIN excerpts ON excerpts_descriptors.excerpt_id  = excerpts.id INNER JOIN albums_artists ON excerpts.album_id = albums_artists.album_id INNER JOIN excerpts_datasets ON excerpts.id = excerpts_datasets.excerpt_id WHERE excerpts_datasets.dataset_id = ' , dataset_id)

	dataset <- dbGetQuery(db, query)
	dataset <- dataset[, c('excerpt_id', 'artist_id', 'album_id', descriptors)]
	dataset <- dataset[order(dataset$excerpt_id),]
	row.names(dataset) <- dataset$excerpt_id
	
	print(head(dataset)[1:4])
	
	artists_ids <- unique(dataset$artist_id)
	artists_names <- vector(mode = "list", length(artists_ids))
	names(artists_names) <- artists_ids
	
	print(artists_ids)
	
	for(id in artists_ids){
		query = paste('SELECT name FROM artists_info WHERE id = ', id)
		name <- dbGetQuery(db, query)
		artists_names[[paste(id)]] <- name$name
	
		values = dataset[which(dataset$artist_id == id),]
	
		print(paste("ARTIST: ", artists_names[[paste(id)]]))
	}
	return(dataset)
	
	dbDisconnect(db)	
}










# for (id in artists_ids){
	# query = paste('SELECT name FROM artists_info WHERE id = ', id)
	# name <- dbGetQuery(db, query)
	# artists_names[[paste(id)]] <- name$name
	
	# values = dataset[which(dataset$artist_id == id),]
	
	# print(paste("ARTIST: ", id))
			
	# for(desc in descriptors){
		
		# print(desc)
		
		# values_desc = values[[desc]]
		
		# minimum = min(values_desc)
		# maximum = max(values_desc)
		
		# print(paste("MIN: ", minimum))
		# print(paste("MAX: ", maximum))
		# print(paste("MEAN: ", mean(values_desc)))
		# print(paste("MEDIAN: ", median(values_desc)))
		# print(paste("VAR: ", var(values_desc)))
		
		# if (minimum != maximum){
			# norm_values_desc = (values_desc - minimum)/ (maximum - minimum)
		# }
		# else{
			# norm_values_desc = 0.5
		# }
		
		
		
		# norm_minimum = min(norm_values_desc)
		# norm_maximum = max(norm_values_desc)
		
		# print(paste("NORMALIZED MIN: ", norm_minimum))
		# print(paste("NORMALIZED MAX: ", norm_maximum))
		# print(paste("NORMALIZED MEAN: ", mean(norm_values_desc)))
		# print(paste("NORMALIZED MEDIAN: ", median(norm_values_desc)))
		# print(paste("NORMALIZED VAR: ", var(norm_values_desc)))

		
		# print("")
	
	# }
	
# }

# # for(desc in descriptors){
	

	# # pdf(file = paste('Figures/', dataset_id, '_bxplt_', desc, '.pdf'))
	
	# # boxplot(dataset[[desc]]~artist_id, data=dataset, main=desc, 
  	 # # xlab="Artist", ylab=desc)
	
	# # dev.off()
# # }

