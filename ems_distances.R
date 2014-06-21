write_distances_csvs <- function(dataset_ids, distances_report){
	
	filename = paste(dataset_ids, collapse = "_")
	filename = paste(filename, sep = "_")
	
	#print(filename)
	
	for(distance in names(distances_report)){
		
		filename_dist = paste(filename, distance, sep = "_")
		
		for(fs in names(distances_report[[distance]])){
			
			filename_fs = paste(filename_dist, fs, sep = "_")
			
			for(group in names(distances_report[[distance]][[fs]])){
				
				filename_group = paste(filename_fs, group, sep = "_")
				filename_csv = paste('Distances/',filename_group, ".csv", sep = "")
				write.csv(report[[distance]][[fs]][[group]], filename_csv)
				
			}
			
		}
		
		
		
		
	}
	
	
	
	
}