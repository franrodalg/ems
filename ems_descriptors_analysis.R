library(RMySQL)

dataset_id <-  1

db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')

query <- paste('SELECT albums_artists.artist_id, excerpts_descriptors.* FROM excerpts_descriptors INNER JOIN excerpts ON excerpts_descriptors.excerpt_id  = excerpts.id INNER JOIN albums_artists ON excerpts.album_id = albums_artists.album_id INNER JOIN excerpts_datasets ON excerpts.id = excerpts_datasets.excerpt_id WHERE excerpts_datasets.dataset_id = ' , dataset_id)


dataset <- dbGetQuery(db, query)

#artists_ids <- unique(dataset$artist_id)
#artists_names <- vector(mode = "list", length(artists_ids))
#names(artists_names) <- artists_ids

#descriptors <- names(dataset)[3:4]
descriptors <- names(dataset)[3:length(names(dataset))]

for(desc in descriptors){
	cat(desc, '\n')
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

