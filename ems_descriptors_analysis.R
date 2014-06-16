library(RMySQL)
library(e1071)
library(stats)


get_dataset_desc <- function(dataset_id){
	
	
	db <- dbConnect(MySQL(), host = 'localhost', user = 'root', db = 'ems')
	
	
	all <- dbReadTable(db, 'excerpts_descriptors')
	
	desc_with_na <- names(all[, colMeans(is.na(all)) != 0])
	descriptors <- names(all)[2:length(names(all))]
	descriptors <- subset(descriptors, ! descriptors %in% desc_with_na)
	
	rm(all)
		
	query <- paste('SELECT albums_artists.*, excerpts_descriptors.* FROM excerpts_descriptors INNER JOIN excerpts ON excerpts_descriptors.excerpt_id  = excerpts.id INNER JOIN albums_artists ON excerpts.album_id = albums_artists.album_id INNER JOIN excerpts_datasets ON excerpts.id = excerpts_datasets.excerpt_id WHERE excerpts_datasets.dataset_id = ' , dataset_id)

	dataset <- dbGetQuery(db, query)
	dataset <- dataset[, c('excerpt_id', 'artist_id', 'album_id', descriptors)]
	dataset <- dataset[order(dataset$excerpt_id),]
	row.names(dataset) <- dataset$excerpt_id
	
	return(dataset)
	
	
	dbDisconnect(db)
}


get_dataset_summary <- function(dataset){
	
	artists_ids <- unique(dataset$artist_id)
	artists_names <- vector(mode = "list", length(artists_ids))
	names(artists_names) <- artists_ids
	
	complete_summary <- vector(mode = "list", 0)
	
	
	for(id in artists_ids){
		query = paste('SELECT name FROM artists_info WHERE id = ', id)
		name <- dbGetQuery(db, query)
		artists_names[[paste(id)]] <- name$name
	
		values = dataset[which(dataset$artist_id == id),]
	
		summary = data.frame(
			'desc' = descriptors,
			'min' = numeric(length(descriptors)),
			'max' = numeric(length(descriptors)),
			'mean' = numeric(length(descriptors)),
			'median' = numeric(length(descriptors)),
			'std' = numeric(length(descriptors)),
			'var' = numeric(length(descriptors)),
			'skewness' = numeric(length(descriptors)),
			'kurtosis' = numeric(length(descriptors)),
			'iqr' = numeric(length(descriptors)),
			'norm_mean' = numeric(length(descriptors)),
			'norm_median' = numeric(length(descriptors)),
			'norm_var' = numeric(length(descriptors)),
			'norm_skewness' = numeric(length(descriptors)),
			'norm_kurtosis' = numeric(length(descriptors)),
			'norm_iqr' = numeric(length(descriptors))
		)
		
		for(desc in descriptors){
		
			values_desc = values[[desc]]
			
			minimum <- min(values_desc)
			maximum <- max(values_desc)
			
			summary[which(summary$desc == desc), 'min'] = minimum
			summary[which(summary$desc == desc), 'max'] = maximum
			summary[which(summary$desc == desc), 'mean'] = mean(values_desc)
			summary[which(summary$desc == desc), 'median'] = median(values_desc)
			summary[which(summary$desc == desc), 'std'] = sd(values_desc)
			summary[which(summary$desc == desc), 'var'] = var(values_desc)
			summary[which(summary$desc == desc), 'skewness'] = skewness(values_desc)
			summary[which(summary$desc == desc), 'kurtosis'] = kurtosis(values_desc)
			summary[which(summary$desc == desc), 'iqr'] = IQR(values_desc)			
					
		 	if (minimum != maximum){
				norm_values_desc = (values_desc - minimum)/ (maximum - minimum)
			}
			else{
				norm_values_desc = 0.5
			}
		
			summary[which(summary$desc == desc), 'norm_mean'] = mean(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_median'] = median(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_std'] = sd(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_var'] = var(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_skewness'] = skewness(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_kurtosis'] = kurtosis(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_iqr'] = IQR(norm_values_desc)
		
			
		}
		
		complete_summary[[name$name]] <- summary
		
	}
	
	
	dbDisconnect(db)
	
	return(complete_summary)	
}


print_summary <- function(summary){
	
	for(artist in names(summary)){
		
		
		print(paste("ARTIST: ", artist))
		art_summary <- summary[[artist]]
		
		print('STD')
		sum_order <- order(art_summary$norm_std)
		print(art_summary[sum_order[1:20], c('desc', 'norm_std')])
		print('IQR')
		sum_order <- order(art_summary$norm_iqr)
		print(art_summary[sum_order[1:20], c('desc', 'norm_iqr')])
		print('KUR')
		sum_order <- order(abs(art_summary$norm_kurtosis))
		print(art_summary[sum_order[1:20], c('desc', 'norm_kurtosis')])

		
	}
	
	
	
}

plot_dataset <- function(dataset_id, dataset){

	#descriptors <- names(dataset)[4:length(descriptors)]
	descriptors <- names(dataset)[4]
	
	print(descriptors)
	
	for(desc in descriptors){
	
		pdf(file = paste('Figures/', dataset_id, '_bxplt_', desc, '.pdf'))
	
		boxplot(dataset[[desc]]~artist_id, data=dataset, main=desc, 
			xlab="Artist", ylab=desc)
	
		dev.off()
	}

}