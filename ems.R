source('ems_descriptors_analysis.R')
source('ems_classification.R')

get_all_datasets <- function(){
	
	datasets <- vector(mode = 'list')

	dataset_id <- numeric(75)
	ambient <- numeric(75)


	ambient[] <- 1

	dataset_id[] <- 1
	datasets$atm_amb$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(1))

	dataset_id[] <- 5
	datasets$idm_amb$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(5))

	dataset_id[] <- 2
	datasets$pr_amb$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(2))


	ambient <- 0

	dataset_id[] <- 3
	datasets$techno$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(3))

	dataset_id[] <- 4
	datasets$idm$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(4))

	dataset_id[] <- 6
	datasets$nu_jazz$data <- data.frame(ambient = ambient, dataset_id = dataset_id, get_dataset(6))

	datasets$ambient$data <- rbind(datasets$atm_amb$data, datasets$idm_amb$data, datasets$pr_amb$data)
	datasets$no_ambient$data <- rbind(datasets$techno$data, datasets$idm$data, datasets$nu_jazz$data)

	datasets$all$data <- rbind(datasets$ambient$data, datasets$no_ambient$data)
	
	return(datasets)
}

const_summaries <- function(datasets){
	
	for(dataset in names(datasets[1:9])){
	
		if(dataset == 'ambient' || dataset == 'no_ambient'){
			crit = c('dataset_id', 'artist_id')
		}
		else if(dataset == 'all'){
			crit = c('ambient', 'dataset_id', 'artist_id')
		}
		else{
			crit = c('artist_id')
		}
	
		# Compute Summaries
	
		datasets[[dataset]]$sum[[crit[1]]] <- get_summary(datasets[[dataset]]$data, crit = crit[1])
	
		if(dataset == 'ambient'){
		
			datasets[[dataset]]$sum[['artist_id']] <- c(datasets[['atm_amb']]$sum[['artist_id']], datasets[['idm_amb']]$sum[['artist_id']], datasets[['pr_amb']]$sum[['artist_id']])
		}
		else if(dataset == 'no_ambient'){
		
			datasets[[dataset]]$sum[['artist_id']] <- c(datasets[['techno']]$sum[['artist_id']], datasets[['idm']]$sum[['artist_id']], datasets[['nu_jazz']]$sum[['artist_id']])
		
		}
		else if(dataset == 'all'){
			datasets[[dataset]]$sum[['dataset_id']] <- c(datasets[['ambient']]$sum[['dataset_id']], datasets[['no_ambient']]$sum[['dataset_id']])
		
			datasets[[dataset]]$sum[['artist_id']] <- c(datasets[['ambient']]$sum[['artist_id']], datasets[['no_ambient']]$sum[['artist_id']])
		}

	}

	return(datasets)
	
}


obtain_desc_features <- function(datasets){
	
	dataset <- 'all'
	crit = c('ambient', 'dataset_id', 'artist_id')
	
		
	for(c in crit){
	
		# Generate Boxplots
	
		#plot_descriptors_data(datasets[[dataset]]$data, dataset, crit = c)
		
		# Sort descriptors and Create Feature Sets
		
		for(class in names(datasets[[dataset]]$sum[[c]])){

			for(s in c('norm_std', 'norm_iqr', 'entropy')){
				datasets[[dataset]]$desc_feats[[c]][[class]][[s]] <- sort_descriptors(datasets[[dataset]]$sum[[c]][[class]], stat = s, num = 50)
				
			}
		}
	}
	
	return(datasets)
	
}


analyze_disc <- function(datasets){
	
	modes <- vector(mode = 'list', length = 2)
	names(modes) <- c('holdout', 'self')
	
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
					results_sets[[set]] <- class_svm(datasets[[dataset]]$data, descriptors = feature_sets[[set]])
				}
				else{
					results_sets[[set]] <- class_svm(datasets[[dataset]]$data, descriptors = feature_sets[[set]], self_class = TRUE)

				}
			
				evals_sets[[set]] <- evaluate(results_sets[[set]])
			}
			
			evals_datasets[[dataset]] <- evals_sets	
			
		}
		
		evals[[mode]] <- evals_datasets
		
	}
	
	return(evals)
	
	
	
}

analyze_desc <- function(datasets){
	

	ambient <- c('Ambient', 'No ambient')
	dataset <- c('Atmospheric Ambient', 'IDM Ambient', 'Post-Rock Ambient', 'Techno', 'IDM', 'Nu Jazz')
	artist <- c('Michael Stearns', 'Tetsu Inoue', 'Vidna Obmana','Boards of Canada', 'The Album Leaf',  'The American Dollar', 'Plastikman','Jeff Mills', 'Legowelt', 'Aphex Twin', 'Autechre', 'Squarepusher', 'ISAN', 'Monolake', 'Tycho', 'Lemongrass', 'Bonobo', 'Four Tet')

	stats <- c('norm_std', 'norm_iqr', 'entropy')
	
	for(s in stats){
		
		print(s)
		
		#for(c in ambient){
		#	print(c)
		#	test_desc(datasets, 'ambient', c, s)
		#}
		#for(c in dataset){
		#	print(c)
		#	test_desc(datasets, 'dataset_id', c, s)
		#}
		for(c in artist){
			print(c)
			test_desc(datasets, 'artist_id', c, s)
		}
		
		
	}

	
}

test_desc <- function(datasets, crit, class, stat){
	
    
	
	if(crit == 'ambient'){
		class_crit <- 'dataset_id'
	}
	else if(crit == 'dataset_id'){
		class_crit <- 'artist_id'
		
	}
	else if(crit == 'artist_id'){
		class_crit <- 'album_id'
	}
	
	if(class == 'Ambient'){
		
		data <- datasets$ambient$data
	}
	else if(class == 'No ambient'){
		data <- datasets$no_ambient$data
	}
	else if(class == 'Atmospheric Ambient'){
		data <- datasets$atm_amb$data
	}
	else if(class == 'IDM Ambient'){
		data <- datasets$idm_amb$data
	}
	else if(class == 'Post-Rock Ambient'){
		data <- datasets$pr_amb$data
	}
	else if(class == 'Techno'){
		data <- datasets$techno$data
	}
	else if(class == 'IDM'){
		data <- datasets$idm$data
	}
	else if(class == 'Nu Jazz'){
		data <- datasets$pr_amb$data
	}
	else if(class == 'Michael Stearns'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 66),]
	}
	else if(class == 'Tetsu Inoue'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 94),]
	}
	else if(class == 'Vidna Obmana'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 108),]
	}
	else if(class == 'Boards of Canada'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 16),]
	}
	else if(class == 'The Album Leaf'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 95),]
	}
	else if(class == 'The American Dollar'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 169),]
	}
	else if(class == 'Plastikman'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 80),]
	}
	else if(class == 'Jeff Mills'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 52),]
	}
	else if(class == 'Legowelt'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 59),]
	}
	else if(class == 'Aphex Twin'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 6),]
	}
	else if(class == 'Autechre'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 10),]
	}
	else if(class == 'Squarepusher'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 88),]
	}
	else if(class == 'ISAN'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 47),]
	}
	else if(class == 'Monolake'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 67),]
	}
	else if(class == 'Tycho'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 168),]
	}
	else if(class == 'Lemongrass'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 113),]
	}
	else if(class == 'Bonobo'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 17),]
	}
	else if(class == 'Four Tet'){
		
		full_data <- datasets$all$data
		data <- full_data[which(full_data$artist_id == 41),]
	}
	
	
	print(paste('descriptive_', class_crit, '_' ,class, '_', stat, '.txt', sep = ''))
	
	sink(paste('Evaluation/descriptive_',class_crit,'_',class,'_',stat,'.txt', sep = ''))
	

    for(num_desc in c(10,20,30,40,50)){
		
		class_feats <- datasets[['all']]$desc_feats[[crit]][[class]][[stat]]$desc[1:num_desc]
		
		print('Features:')
		print(class_feats)
		print(length(class_feats))
		
				
		feats <- datasets[['all']]$sum[[crit]][[class]]$desc
		print(length(feats))
		no_class_feats <- feats[! feats %in% class_feats]
		print(length(no_class_feats))
				
			
	
		print(paste('NUM DESC: ', num_desc, sep = ''))
					
		print(paste('Criteria: ', crit, '; Class: ' , class, '; Stat: ', stat , sep = ''))
					
		print('DESCRIPTIVE')
					
		results <- class_svm(data, crit = class_crit, descriptors = class_feats, self_class = TRUE)			
				
		print(results)
					
		eval <- simple_evaluation(results)
		print(paste('Accuracy: ', eval$accuracy))
		print(paste('Precision: ', eval$precision))
		print(paste('Recall: ', eval$recall))
		print(paste('F1-Score: ', eval$'f1-score'))
					
		print('NON-DESCRIPTIVE')
					
		acc_no <- numeric(0)
		prec_no <- numeric(0)
		rec_no <- numeric(0)
		f1_no <- numeric(0)
					
		for(i in 1:10){
			
			test_feats <- sample(no_class_feats, num_desc)
			
			results <- class_svm(data, crit = class_crit, descriptors = test_feats, self_class = TRUE)			
				
			#print(results)		
					
			eval <- simple_evaluation(results)
			
			acc_no <- c(acc_no, eval$accuracy)
			prec_no <- c(prec_no, eval$precision)
			rec_no <- c(acc_no, eval$recall)
			f1_no <- c(acc_no, eval$'f1-score')
					
		}
	
		print(paste('Mean Accuracy: ', mean(acc_no)))
		print(paste('Mean Precision: ', mean(prec_no)))
		print(paste('Mean Recall: ', mean(rec_no)))
		print(paste('Mean F1-Score: ', mean(f1_no)))
	
	
	}
	
	sink()
	
}