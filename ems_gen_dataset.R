source('ems_collection.R')
source('ems_db_interface.R')
library('jsonlite')
library('RMySQL')

gen_dataset <- function(num_albums = 5, num_tracks = 5, 
		min_track_dur = 60, filt_artists = NULL){

	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	report <- gen_report(db = db, min_num_albums = num_albums, 
		min_num_tracks = num_tracks, min_track_dur = min_track_dur, 
		filt_artists = 	filt_artists);
	
	json_report <- toJSON(report, pretty = TRUE, digits = 10, auto_unbox = TRUE)
	
	sink(file = 'dataset_temp.json');
	cat(json_report);
	sink();
	
	dbDisconnect(db)
}

suggest_artists <- function(num_albums = 5, num_tracks = 5, 
		min_track_dur = 60, tag = NULL, avoid_repetition = FALSE){
			
	if(avoid_repetition){
		avoid_artists = get_artists_in_datasets();
	}
	else{
		avoid_artists = NULL;
	}
	
	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	report <- gen_report(db = db, min_num_albums = num_albums, 
		min_num_tracks = num_tracks, min_track_dur = min_track_dur, 
		avoid_artists = avoid_artists, tag = tag);
		
	print_report(report, summary = TRUE)
			
	dbDisconnect(db)
			
}