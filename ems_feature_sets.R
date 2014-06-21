source('ems_db_interface.R')


get_feature_sets <- function(dataset_ids = NULL){
	
	descriptors <- get_descriptors(dataset_ids)
	
	feature_sets <- vector(mode = 'list')
	
	feature_sets$all <- descriptors
	feature_sets$essentia <- descriptors[grepl('^ess_.', descriptors)]
	feature_sets$mirtoolbox <- descriptors[grepl('^mtb_.', descriptors)]
	feature_sets$echonest <- descriptors[grepl('^en_.', descriptors)]
	feature_sets$dynamics <- get_dynamics(descriptors)
	feature_sets$rhythm <- get_rhythm(descriptors)
	feature_sets$timbre <- get_timbre(descriptors)
	feature_sets$pitch_tonal <- get_tonal(descriptors)
	feature_sets$high_level <- get_high_level(descriptors)
	feature_sets$rhythm_pos <- get_rhythm_pos(descriptors)
	feature_sets$structure <- get_structure(descriptors)
	feature_sets$rhythm_structure <- get_rhythm_structure(descriptors)
	
	#print(feature_sets)
	
	return(feature_sets)
	
	
}

get_dynamics <- function(descriptors){
	
	descriptors[grepl('^+(ess_average_loudness|mtb_low_energy|mtb_rms|en_loudness|en_end_of_fade_in|en_start_of_fade_out|en_energy|ess_dynamic)', descriptors)]
	
}

get_rhythm <- function(descriptors){
	
	descriptors[grepl('^+(ess_bpm|ess_first_peak|ess_second_peak|ess_onset_rate|ess_beats_loudness|mtb_tempo|mtb_event_density|en_tempo|en_time_signature)', descriptors)]
	
	
}

get_timbre <- function(descriptors){
	
	descriptors[grepl('^+(ess_mfcc|ess_zero|ess_spectral|ess_scvalleys|ess_dissonance|mtb_mfcc|mtb_spec|mtb_roughness|mtb_irregularity)', descriptors)]
	
}

get_tonal <- function(descriptors){
	
	descriptors[grepl('^+(ess_pitch|ess_tuning|ess_hpcp|ess_key|mtb_mode|mtb_chroma|mtb_hcdf|mtb_inharmonicity|mtb_tonal_centroid|en_key|en_mode)', descriptors)]
}

get_high_level <- function(descriptors){
	
	descriptors[grepl('^+(ess_danceability|en_acousticness|en_danceability|en_valence|en_liveness|en_speechiness)', descriptors)]	

}

get_rhythm_pos <- function(descriptors){
	
	
	descriptors[grepl('^+(ess_ioi|en_ibi|mtb_ioi|en_bar_|en_beat_|en_tatum_)', descriptors)]	
	
	
}

get_structure <- function(descriptors){
	
	
	descriptors[grepl('^+(en_sections$|en_sec_|en_segments$|en_seg_)', descriptors)]	
	
}

get_rhythm_structure <- function(descriptors){
	
	descriptors[grepl('^.+(_per_)', descriptors)]	
	
}