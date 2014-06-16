source('ems_collection_eval.R')
library('RMySQL')

gen_dataset <- function(num_albums = 5, num_tracks = 5, 
		min_track_dur = 60, filt_artists = NULL){



	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	report <- gen_report(db = db, min_num_albums = num_albums, 
		min_num_tracks = num_tracks, min_track_dur = min_track_dur, 
		filt_artists = 	filt_artists);
	
	json_report <- gen_json(report);
	
	sink(file = 'dataset_temp.json');
	cat(json_report);
	sink();
	
	dbDisconnect(db)
}