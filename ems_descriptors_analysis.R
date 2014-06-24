library(e1071)
library(stats)
library(entropy)

source('ems_db_interface.R')
source('ems_feature_sets.R')
source('ems_collection.R')


get_summary <- function(dataset, crit = 'artist_id'){
	
	
	
	unique_ids <- unique(dataset[[crit]])
	
	unique_names <- vector(mode = "list", length(unique_ids))
	names(unique_names) <- unique_ids
	
	complete_summary <- vector(mode = "list", 0)
	
	init_desc <- 4
	
	if('ambient' %in% names(dataset)){init_desc = init_desc + 1}
	if('dataset_id' %in% names(dataset)){init_desc = init_desc + 1}
	
	descriptors <- names(dataset)[init_desc:length(names(dataset))]
	
	for(id in unique_ids){
		
		if(crit == 'artist_id'){
			name <- get_artist_name(id)
		}
		else if(crit == 'dataset_id'){
			name <- get_dataset_name(id)
		}
		else{
			if(id == 0){
				name <- 'Ambient'
			}
			else{
				name <- 'No ambient'
			}
		}
		
		unique_names[[paste(id)]] <- name
	
		values = dataset[which(dataset[[crit]] == id),]
	
		summary = data.frame(
			'desc' = descriptors,
			#'min' = numeric(length(descriptors)),
			#'max' = numeric(length(descriptors)),
			#'mean' = numeric(length(descriptors)),
			#'median' = numeric(length(descriptors)),
			#'std' = numeric(length(descriptors)),
			#'var' = numeric(length(descriptors)),
			#'skewness' = numeric(length(descriptors)),
			#'kurtosis' = numeric(length(descriptors)),
			#'iqr' = numeric(length(descriptors)),
			'entropy' = numeric(length(descriptors)),
			#'norm_mean' = numeric(length(descriptors)),
			#'norm_median' = numeric(length(descriptors)),
			'norm_std' = numeric(length(descriptors)),
			#'norm_var' = numeric(length(descriptors)),
			#'norm_skewness' = numeric(length(descriptors)),
			#'norm_kurtosis' = numeric(length(descriptors)),
			'norm_iqr' = numeric(length(descriptors))
		)
		
	
		
		for(desc in descriptors){
		
			
		
			values_desc = values[[desc]]
			values_desc = values_desc[!is.nan(values_desc)]
			
			range <- get_descriptor_range(desc)
			
			minimum <- range$min
			maximum <- range$max
			
			#summary[which(summary$desc == desc), 'min'] = min(values_desc)
			#summary[which(summary$desc == desc), 'max'] = max(values_desc)
			#summary[which(summary$desc == desc), 'mean'] = mean(values_desc)
			#summary[which(summary$desc == desc), 'median'] = median(values_desc)
			#summary[which(summary$desc == desc), 'std'] = sd(values_desc)
			#summary[which(summary$desc == desc), 'var'] = var(values_desc)
			#summary[which(summary$desc == desc), 'skewness'] = skewness(values_desc)
			#summary[which(summary$desc == desc), 'kurtosis'] = kurtosis(values_desc)
			#summary[which(summary$desc == desc), 'iqr'] = IQR(values_desc)
			summary[which(summary$desc == desc), 'entropy'] = entropy(values_desc)		
					
		 	if (minimum != maximum){
				norm_values_desc = (values_desc - minimum)/ (maximum - minimum)
			}
			else{
				norm_values_desc = 0.5
			}
		
			#summary[which(summary$desc == desc), 'norm_min'] = min(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_max'] = max(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_mean'] = mean(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_median'] = median(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_std'] = sd(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_var'] = var(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_skewness'] = skewness(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_kurtosis'] = kurtosis(norm_values_desc)
			summary[which(summary$desc == desc), 'norm_iqr'] = IQR(norm_values_desc)
			#summary[which(summary$desc == desc), 'norm_entropy'] = entropy(norm_values_desc)	
			
		}
	
		complete_summary[[name]] <- summary
		
	}

	return(complete_summary)	
}

sort_descriptors <- function(art_summary, stat = 'norm_std', num = 20, reverse = FALSE){
	
	
	valid <- names(art_summary)[2:length(names(art_summary))]
	
	if(! stat %in% valid){
		cat(paste('\'', stat, '\'', ' is no valid statistic.\n', sep = ''))
		cat(paste('Valid statistics: ', paste(valid, collapse = ", ")))
		return(NULL)
	}
	
	sum_order <- order(art_summary[[stat]], decreasing = reverse)
	return(art_summary[sum_order[1:num], c('desc', stat)])
	
}

plot_descriptors_data <- function(dataset, dataset_name, descriptors = NULL, crit = 'artist_id'){
	
	if(is.null(descriptors)){
		
		init_desc <- 4
	
		if('ambient' %in% names(dataset)){init_desc = init_desc + 1}
		if('dataset_id' %in% names(dataset)){init_desc = init_desc + 1}
	
		descriptors <- names(dataset)[init_desc:length(names(dataset))]


	}
	
	unique_ids <- unique(dataset[[crit]])
	unique_ids <- unique_ids[order(unique_ids)]
	
	width <- 10
	height <- 6
	cex <- 0.9
	
	if(crit == 'artist_id'){
		unique_names <- unlist(lapply(unique_ids, get_artist_name))
		if(dataset_name == 'all'){
			width <- 20
			height <- 6
			cex <- 0.75
		}
		else if(dataset_name == 'ambient' || dataset_name == 'no_ambient'){
			width <- 15
			height <- 6
			cex <- 0.75
		}
		
	}
	else if(crit == 'dataset_id')
	{
		unique_names <- unlist(lapply(unique_ids, get_dataset_name))
		if(dataset_name == 'all'){
			width <- 15
			height <- 6
			cex <- 0.85
		}
	}
	else{
		unique_names <- c('No Ambient', 'Ambient')
	}
	
	
	
	for(desc in descriptors){
	
		pdf(file = paste('Figures/dat_', dataset_name, '_', crit,'_bxplt_', desc, '.pdf', sep = ""), width = width, height = height)
	
	
		if(crit == 'artist_id'){
			xlab = 'Artist'
			
			#print(cex)
				
			boxplot(dataset[[desc]]~artist_id, data=dataset, main=desc, 
				xlab=xlab, ylab=desc, names = unique_names, cex.axis = cex)

		}
		else if(crit == 'dataset_id')
		{
			xlab = 'Dataset'
			
			boxplot(dataset[[desc]]~dataset_id, data=dataset, main=desc, 
				xlab=xlab, ylab=desc, names = unique_names, cex.axis = cex)

		}
		else{
			xlab = 'Dataset'
			
			boxplot(dataset[[desc]]~ambient, data=dataset, main=desc, 
				xlab=xlab, ylab=desc, names = unique_names, cex.axis = cex)

		}
			
		dev.off()
	}

	
}

plot_descriptors <- function(dataset_id, descriptors = NULL){

	dataset <- get_dataset(dataset_id, descriptors)
	
	plot_descriptors_data(dataset, paste(dataset_id, descriptors))



}


compute_distances <- function(dataset_ids, distances = NULL, with_fs = FALSE){
	
	
	dataset <- get_dataset(dataset_ids, no_nas = FALSE)	
	
	if(with_fs){
		feature_sets <- get_feature_sets(dataset_ids)
	}
	else{
		feature_sets <- vector(mode ='list')
		feature_sets$all <- get_descriptors(dataset_ids)
	}
	
	sum <- get_dataset_content_summary(dataset)
	
	if(is.null(distances)){
		#distances = c('euclid', 'cor')
		distances = c('cor')
	}
	
	report <- vector(mode = 'list', length(distances))
	names(report) <- distances
	
	for(distance in distances){
		
		dist <- vector(mode = 'list')
		
		for(set in names(feature_sets)){
			
			dataset <- dataset[order(dataset$excerpt_id),]
			
			filt_dataset <- dataset[, feature_sets[[set]]]
			
			mat <- as.matrix(filt_dataset)
			
			dist_matrix <- matrix(nrow = nrow(mat), ncol = nrow(mat))
			row.names(dist_matrix) <- dataset$excerpt_id
			colnames(dist_matrix) <- dataset$excerpt_id
			
			for(excerpt in row.names(dist_matrix)){
				dist_matrix[(which(row.names(dist_matrix) == excerpt)),] <- distancevector(mat, mat[which(row.names(dist_matrix) == excerpt),], d = distance)
			}
			
					
			dist[[set]]$dist_matrix <- dist_matrix
			
			dist_set <- vector(mode = 'list')
			dist_set$dist_matrix <- dist_matrix
	
			dist_set$dist_albums <- get_grouped_distances(dist_matrix, sum$excerpts_per_album)
			dist_set$dist_artists <- get_grouped_distances(dist_matrix, sum$excerpts_per_artist)
		
			dist[[set]] <- dist_set 
		
		}
		
		report[[distance]] <- dist		
				
	}
		
	return(report)
	
}

get_grouped_distances <- function(distance_matrix, grouping){
	
	grouped_distances <- matrix(nrow = length(names(grouping)), ncol = length(names(grouping)))
	row.names(grouped_distances) <- names(grouping)
	colnames(grouped_distances) <- names(grouping)
	
	sorting <- order(type.convert(row.names(grouped_distances)))
	
	grouped_distances <- grouped_distances[sorting, sorting]

	for(group in names(grouping)){
		
		group_rows <- distance_matrix[which(row.names(distance_matrix) %in% grouping[[group]]),]
		
		for(group_2 in names(grouping)){
			
			group_cols <- group_rows[,which(colnames(group_rows) %in% grouping[[group_2]])]
			#print(group_cols)
			
			if(group == group_2){
				group_mean <- mean(group_cols[upper.tri(group_cols)])
			}
			else{
				group_mean <- mean(group_cols)
			}
			
			grouped_distances[which(row.names(grouped_distances) == group), which(colnames(grouped_distances) == group_2)] <- group_mean
		}
		
	}
	
	
	return(grouped_distances)
		
}