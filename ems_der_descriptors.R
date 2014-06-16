library('RMySQL')
library('e1071')


get_der_descriptors <- function(excerpt_id, extractor){
	
	if(extractor == 'rhythm_pos'){
		desc <- get_rhythm_pos_desc(excerpt_id)
	}
	else if(extractor == 'en_structure'){
		desc <- get_en_structure_desc(excerpt_id)
	}
	else if(extractor == 'rhythm_structure'){
		desc <- get_rhythm_structure_desc(excerpt_id)
	}
	else{
		return()
	}
	
	desc_json <- gen_json(desc)
	
	sink(file = 'desc_temp.json')
	cat(desc_json)
	sink()
	
	
}


get_rhythm_pos_desc <- function(excerpt_id){
	
	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	d <- compute_rhythm_pos_desc(db, excerpt_id)

	dbDisconnect(db)
	
	return(d)

}

get_en_structure_desc <- function(excerpt_id){
	
	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	d <- compute_en_structure_desc(db, excerpt_id)

	dbDisconnect(db)
	
	return(d)

}

get_rhythm_structure_desc <- function(excerpt_id){
	
	db <- dbConnect(MySQL(), db = 'ems', host = 'localhost', user = 'root');

	d <- compute_rhythm_structure_desc(db, excerpt_id)

	dbDisconnect(db)
	
	return(d)

}

compute_rhythm_pos_desc <- function(db, excerpt_id){
	
	desc <- vector(mode = 'list', length = 0)
	
	query <- paste('SELECT ess_onset_time FROM excerpts_ess_onset_times WHERE excerpt_id = ', excerpt_id)
	ess_onsets <- dbGetQuery(db, query)$ess_onset_time
	ess_ioi <- diff(ess_onsets)
	
	desc$ess_ioi_mean <- mean(ess_ioi)
	desc$ess_ioi_med <- median(ess_ioi)
	desc$ess_ioi_var <- var(ess_ioi)
	desc$ess_ioi_skew <- skewness(ess_ioi)
	desc$ess_ioi_kur <- kurtosis(ess_ioi)
	
	query <- paste('SELECT mtb_onset_time FROM excerpts_mtb_onset_times WHERE excerpt_id = ', excerpt_id)
	mtb_onsets <- dbGetQuery(db, query)$mtb_onset_time
	mtb_ioi <- diff(mtb_onsets)
	
	desc$mtb_ioi_mean <- mean(mtb_ioi)
	desc$mtb_ioi_med <- median(mtb_ioi)
	desc$mtb_ioi_var <- var(mtb_ioi)
	desc$mtb_ioi_skew <- skewness(mtb_ioi)
	desc$mtb_ioi_kur <- kurtosis(mtb_ioi)
	
	query <- paste('SELECT ess_beat_position FROM excerpts_ess_beats_position WHERE excerpt_id = ', excerpt_id)
	ess_beats <- dbGetQuery(db, query)$ess_beat_position
	ess_ibi <- diff(ess_beats)
	
	desc$ess_ibi_mean <- mean(ess_ibi)
	desc$ess_ibi_med <- median(ess_ibi)
	desc$ess_ibi_var <- var(ess_ibi)
	desc$ess_ibi_skew <- skewness(ess_ibi)
	desc$ess_ibi_kur <- kurtosis(ess_ibi)
	
	query <- paste('SELECT en_duration, en_confidence FROM excerpts_en_bars 
		WHERE excerpt_id = ', excerpt_id)
	en_bars <- dbGetQuery(db, query)
	en_bars_dur <- en_bars$en_duration
	en_bars_conf <- en_bars$en_confidence
	
	desc$en_bar_dur_mean <- mean(en_bars_dur)
	desc$en_bar_dur_med <- median(en_bars_dur)
	desc$en_bar_dur_var <- var(en_bars_dur)
	desc$en_bar_dur_skew <- skewness(en_bars_dur)
	desc$en_bar_dur_kur <- kurtosis(en_bars_dur)
	
	desc$en_bar_conf_mean <- mean(en_bars_conf)
	desc$en_bar_conf_med <- median(en_bars_conf)
	desc$en_bar_conf_var <- var(en_bars_conf)
	desc$en_bar_conf_skew <- skewness(en_bars_conf)
	desc$en_bar_conf_kur <- kurtosis(en_bars_conf)
	
	query <- paste('SELECT en_duration, en_confidence FROM excerpts_en_beats 
		WHERE excerpt_id = ', excerpt_id)
	en_beats <- dbGetQuery(db, query)
	en_beats_dur <- en_beats$en_duration
	en_beats_conf <- en_beats$en_confidence
	
	desc$en_beats_dur_mean <- mean(en_beats_dur)
	desc$en_beats_dur_med <- median(en_beats_dur)
	desc$en_beats_dur_var <- var(en_beats_dur)
	desc$en_beats_dur_skew <- skewness(en_beats_dur)
	desc$en_beats_dur_kur <- kurtosis(en_beats_dur)
	
	desc$en_beats_conf_mean <- mean(en_beats_conf)
	desc$en_beats_conf_med <- median(en_beats_conf)
	desc$en_beats_conf_var <- var(en_beats_conf)
	desc$en_beats_conf_skew <- skewness(en_beats_conf)
	desc$en_beats_conf_kur <- kurtosis(en_beats_conf)
	
	query <- paste('SELECT en_duration, en_confidence FROM excerpts_en_tatums 
		WHERE excerpt_id = ', excerpt_id)
	en_tatums <- dbGetQuery(db, query)
	en_tatums_dur <- en_tatums$en_duration
	en_tatums_conf <- en_tatums$en_confidence
	
	desc$en_tatums_dur_mean <- mean(en_tatums_dur)
	desc$en_tatums_dur_med <- median(en_tatums_dur)
	desc$en_tatums_dur_var <- var(en_tatums_dur)
	desc$en_tatums_dur_skew <- skewness(en_tatums_dur)
	desc$en_tatums_dur_kur <- kurtosis(en_tatums_dur)
	
	desc$en_tatums_conf_mean <- mean(en_tatums_conf)
	desc$en_tatums_conf_med <- median(en_tatums_conf)
	desc$en_tatums_conf_var <- var(en_tatums_conf)
	desc$en_tatums_conf_skew <- skewness(en_tatums_conf)
	desc$en_tatums_conf_kur <- kurtosis(en_tatums_conf)
	
	return(desc)
	
}

compute_en_structure_desc <- function(db, excerpt_id){	
	
	
	desc <- vector(mode = 'list', length = 0)
	
	query <- paste('SELECT track_id, start, end FROM excerpts WHERE id = ', excerpt_id)
	ex_info <- dbGetQuery(db, query)

	if (is.na(ex_info$start) || is.na(ex_info$end)){
		query <- paste('SELECT dur_sec FROM tracks_info WHERE id = ', ex_info$track_id)
		dur_sec <- dbGetQuery(db, query)$dur_sec
	}else{
		dur_sec <- (ex_info$end - ex_info$start)
	}
	
	query <- paste('SELECT * 
		FROM excerpts_en_sections 
		WHERE excerpt_id = ', excerpt_id)
	en_sections <- dbGetQuery(db, query)
	
	desc$en_sections <- nrow(en_sections)
	desc$en_sec_rate <- desc$en_sections/dur_sec
	
	desc$en_sec_dur_mean <- mean(en_sections$en_duration)
	desc$en_sec_dur_med <- median(en_sections$en_duration)
	desc$en_sec_dur_var <- var(en_sections$en_duration)
	desc$en_sec_dur_skew <- skewness(en_sections$en_duration)
	desc$en_sec_dur_kur <- kurtosis(en_sections$en_duration)
	
	desc$en_sec_conf_mean <- mean(en_sections$en_confidence)
	desc$en_sec_conf_med <- median(en_sections$en_confidence)
	desc$en_sec_conf_var <- var(en_sections$en_confidence)
	desc$en_sec_conf_skew <- skewness(en_sections$en_confidence)
	desc$en_sec_conf_kur <- kurtosis(en_sections$en_confidence)
	
	desc$en_sec_loud_var <- var(en_sections$en_loudness)
	desc$en_sec_loud_skew <- skewness(en_sections$en_loudness)
	desc$en_sec_loud_kur <- kurtosis(en_sections$en_loudness)
	
	desc$en_sec_key_var <- var(en_sections$en_key)
	
	desc$en_sec_key_conf_var <- var(en_sections$en_key_confidence)
	
	desc$en_sec_mode_var <- var(en_sections$en_mode)
	
	desc$en_sec_mode_conf_var <- var(en_sections$en_mode_confidence)
	
	desc$en_sec_time_sig_var <- var(en_sections$en_time_signature)
	
	desc$en_sec_time_sig_conf_var <- var(en_sections$en_time_signature_confidence)
	
	desc$en_sec_tempo_var <- var(en_sections$en_tempo)
	
	desc$en_sec_tempo_conf_var <- var(en_sections$en_tempo_confidence)
	
	query <- paste('SELECT * FROM excerpts_en_segments WHERE excerpt_id = ', excerpt_id)
	en_segments <- dbGetQuery(db, query)
		
	desc$en_segments <- nrow(en_segments)
	desc$en_seg_rate <- desc$en_segments/dur_sec
	
	desc$en_seg_dur_mean <- mean(en_segments$en_duration)
	desc$en_seg_dur_med <- median(en_segments$en_duration)
	desc$en_seg_dur_var <- var(en_segments$en_duration)
	desc$en_seg_dur_skew <- skewness(en_segments$en_duration)
	desc$en_seg_dur_kur <- kurtosis(en_segments$en_duration)
	
	desc$en_seg_conf_mean <- mean(en_segments$en_confidence)
	desc$en_seg_conf_med <- median(en_segments$en_confidence)
	desc$en_seg_conf_var <- var(en_segments$en_confidence)
	desc$en_seg_conf_skew <- skewness(en_segments$en_confidence)
	desc$en_seg_conf_kur <- kurtosis(en_segments$en_confidence)
	
	desc$en_seg_loud_start_mean <- mean(en_segments$en_loudness_start)
	desc$en_seg_loud_start_med <- median(en_segments$en_loudness_start)
	desc$en_seg_loud_start_var <- var(en_segments$en_loudness_start)
	desc$en_seg_loud_start_skew <- skewness(en_segments$en_loudness_start)
	desc$en_seg_loud_start_kur <- kurtosis(en_segments$en_loudness_start)
	
	desc$en_seg_loud_max_mean <- mean(en_segments$en_loudness_max)
	desc$en_seg_loud_max_med <- median(en_segments$en_loudness_max)
	desc$en_seg_loud_max_var <- var(en_segments$en_loudness_max)
	desc$en_seg_loud_max_skew <- skewness(en_segments$en_loudness_max)
	desc$en_seg_loud_max_kur <- kurtosis(en_segments$en_loudness_max)
	
	desc$en_seg_loud_max_time_mean <- mean(en_segments$en_loudness_max_time)
	desc$en_seg_loud_max_time_med <- median(en_segments$en_loudness_max_time)
	desc$en_seg_loud_max_time_var <- var(en_segments$en_loudness_max_time)
	desc$en_seg_loud_max_time_skew <- skewness(en_segments$en_loudness_max_time)
	desc$en_seg_loud_max_time_kur <- kurtosis(en_segments$en_loudness_max_time)
	
	en_loud_max_pos <- en_segments$en_loudness_max_time / en_segments$en_duration
	
	desc$en_seg_loud_max_pos_mean <- mean(en_loud_max_pos)
	desc$en_seg_loud_max_pos_med <- median(en_loud_max_pos)
	desc$en_seg_loud_max_pos_var <- var(en_loud_max_pos)
	desc$en_seg_loud_max_pos_skew <- skewness(en_loud_max_pos)
	desc$en_seg_loud_max_pos_kur <- kurtosis(en_loud_max_pos)
	
	desc$en_seg_timbre_01_mean <- mean(en_segments$en_timbre_01)
	desc$en_seg_timbre_01_med <- median(en_segments$en_timbre_01)
	desc$en_seg_timbre_01_var <- var(en_segments$en_timbre_01)
	desc$en_seg_timbre_01_skew <- skewness(en_segments$en_timbre_01)
	desc$en_seg_timbre_01_kur <- kurtosis(en_segments$en_timbre_01)
	
	desc$en_seg_timbre_02_mean <- mean(en_segments$en_timbre_02)
	desc$en_seg_timbre_02_med <- median(en_segments$en_timbre_02)
	desc$en_seg_timbre_02_var <- var(en_segments$en_timbre_02)
	desc$en_seg_timbre_02_skew <- skewness(en_segments$en_timbre_02)
	desc$en_seg_timbre_02_kur <- kurtosis(en_segments$en_timbre_02)
	
	desc$en_seg_timbre_03_mean <- mean(en_segments$en_timbre_03)
	desc$en_seg_timbre_03_med <- median(en_segments$en_timbre_03)
	desc$en_seg_timbre_03_var <- var(en_segments$en_timbre_03)
	desc$en_seg_timbre_03_skew <- skewness(en_segments$en_timbre_03)
	desc$en_seg_timbre_03_kur <- kurtosis(en_segments$en_timbre_03)
	
	desc$en_seg_timbre_04_mean <- mean(en_segments$en_timbre_04)
	desc$en_seg_timbre_04_med <- median(en_segments$en_timbre_04)
	desc$en_seg_timbre_04_var <- var(en_segments$en_timbre_04)
	desc$en_seg_timbre_04_skew <- skewness(en_segments$en_timbre_04)
	desc$en_seg_timbre_04_kur <- kurtosis(en_segments$en_timbre_04)
	
	desc$en_seg_timbre_05_mean <- mean(en_segments$en_timbre_05)
	desc$en_seg_timbre_05_med <- median(en_segments$en_timbre_05)
	desc$en_seg_timbre_05_var <- var(en_segments$en_timbre_05)
	desc$en_seg_timbre_05_skew <- skewness(en_segments$en_timbre_05)
	desc$en_seg_timbre_05_kur <- kurtosis(en_segments$en_timbre_05)
	
	desc$en_seg_timbre_06_mean <- mean(en_segments$en_timbre_06)
	desc$en_seg_timbre_06_med <- median(en_segments$en_timbre_06)
	desc$en_seg_timbre_06_var <- var(en_segments$en_timbre_06)
	desc$en_seg_timbre_06_skew <- skewness(en_segments$en_timbre_06)
	desc$en_seg_timbre_06_kur <- kurtosis(en_segments$en_timbre_06)
	
	desc$en_seg_timbre_07_mean <- mean(en_segments$en_timbre_07)
	desc$en_seg_timbre_07_med <- median(en_segments$en_timbre_07)
	desc$en_seg_timbre_07_var <- var(en_segments$en_timbre_07)
	desc$en_seg_timbre_07_skew <- skewness(en_segments$en_timbre_07)
	desc$en_seg_timbre_07_kur <- kurtosis(en_segments$en_timbre_07)
	
	desc$en_seg_timbre_08_mean <- mean(en_segments$en_timbre_08)
	desc$en_seg_timbre_08_med <- median(en_segments$en_timbre_08)
	desc$en_seg_timbre_08_var <- var(en_segments$en_timbre_08)
	desc$en_seg_timbre_08_skew <- skewness(en_segments$en_timbre_08)
	desc$en_seg_timbre_08_kur <- kurtosis(en_segments$en_timbre_08)
	
	desc$en_seg_timbre_09_mean <- mean(en_segments$en_timbre_09)
	desc$en_seg_timbre_09_med <- median(en_segments$en_timbre_09)
	desc$en_seg_timbre_09_var <- var(en_segments$en_timbre_09)
	desc$en_seg_timbre_09_skew <- skewness(en_segments$en_timbre_09)
	desc$en_seg_timbre_09_kur <- kurtosis(en_segments$en_timbre_09)
	
	desc$en_seg_timbre_10_mean <- mean(en_segments$en_timbre_10)
	desc$en_seg_timbre_10_med <- median(en_segments$en_timbre_10)
	desc$en_seg_timbre_10_var <- var(en_segments$en_timbre_10)
	desc$en_seg_timbre_10_skew <- skewness(en_segments$en_timbre_10)
	desc$en_seg_timbre_10_kur <- kurtosis(en_segments$en_timbre_10)
	
	desc$en_seg_timbre_11_mean <- mean(en_segments$en_timbre_11)
	desc$en_seg_timbre_11_med <- median(en_segments$en_timbre_11)
	desc$en_seg_timbre_11_var <- var(en_segments$en_timbre_11)
	desc$en_seg_timbre_11_skew <- skewness(en_segments$en_timbre_11)
	desc$en_seg_timbre_11_kur <- kurtosis(en_segments$en_timbre_11)
	
	desc$en_seg_timbre_12_mean <- mean(en_segments$en_timbre_12)
	desc$en_seg_timbre_12_med <- median(en_segments$en_timbre_12)
	desc$en_seg_timbre_12_var <- var(en_segments$en_timbre_12)
	desc$en_seg_timbre_12_skew <- skewness(en_segments$en_timbre_12)
	desc$en_seg_timbre_12_kur <- kurtosis(en_segments$en_timbre_12)
	
	desc$en_seg_pitch_01_mean <- mean(en_segments$en_pitch_01)
	desc$en_seg_pitch_01_med <- median(en_segments$en_pitch_01)
	desc$en_seg_pitch_01_var <- var(en_segments$en_pitch_01)
	desc$en_seg_pitch_01_skew <- skewness(en_segments$en_pitch_01)
	desc$en_seg_pitch_01_kur <- kurtosis(en_segments$en_pitch_01)
	
	desc$en_seg_pitch_02_mean <- mean(en_segments$en_pitch_02)
	desc$en_seg_pitch_02_med <- median(en_segments$en_pitch_02)
	desc$en_seg_pitch_02_var <- var(en_segments$en_pitch_02)
	desc$en_seg_pitch_02_skew <- skewness(en_segments$en_pitch_02)
	desc$en_seg_pitch_02_kur <- kurtosis(en_segments$en_pitch_02)
	
	desc$en_seg_pitch_03_mean <- mean(en_segments$en_pitch_03)
	desc$en_seg_pitch_03_med <- median(en_segments$en_pitch_03)
	desc$en_seg_pitch_03_var <- var(en_segments$en_pitch_03)
	desc$en_seg_pitch_03_skew <- skewness(en_segments$en_pitch_03)
	desc$en_seg_pitch_03_kur <- kurtosis(en_segments$en_pitch_03)
	
	desc$en_seg_pitch_04_mean <- mean(en_segments$en_pitch_04)
	desc$en_seg_pitch_04_med <- median(en_segments$en_pitch_04)
	desc$en_seg_pitch_04_var <- var(en_segments$en_pitch_04)
	desc$en_seg_pitch_04_skew <- skewness(en_segments$en_pitch_04)
	desc$en_seg_pitch_04_kur <- kurtosis(en_segments$en_pitch_04)
	
	desc$en_seg_pitch_05_mean <- mean(en_segments$en_pitch_05)
	desc$en_seg_pitch_05_med <- median(en_segments$en_pitch_05)
	desc$en_seg_pitch_05_var <- var(en_segments$en_pitch_05)
	desc$en_seg_pitch_05_skew <- skewness(en_segments$en_pitch_05)
	desc$en_seg_pitch_05_kur <- kurtosis(en_segments$en_pitch_05)
	
	desc$en_seg_pitch_06_mean <- mean(en_segments$en_pitch_06)
	desc$en_seg_pitch_06_med <- median(en_segments$en_pitch_06)
	desc$en_seg_pitch_06_var <- var(en_segments$en_pitch_06)
	desc$en_seg_pitch_06_skew <- skewness(en_segments$en_pitch_06)
	desc$en_seg_pitch_06_kur <- kurtosis(en_segments$en_pitch_06)
	
	desc$en_seg_pitch_07_mean <- mean(en_segments$en_pitch_07)
	desc$en_seg_pitch_07_med <- median(en_segments$en_pitch_07)
	desc$en_seg_pitch_07_var <- var(en_segments$en_pitch_07)
	desc$en_seg_pitch_07_skew <- skewness(en_segments$en_pitch_07)
	desc$en_seg_pitch_07_kur <- kurtosis(en_segments$en_pitch_07)
	
	desc$en_seg_pitch_08_mean <- mean(en_segments$en_pitch_08)
	desc$en_seg_pitch_08_med <- median(en_segments$en_pitch_08)
	desc$en_seg_pitch_08_var <- var(en_segments$en_pitch_08)
	desc$en_seg_pitch_08_skew <- skewness(en_segments$en_pitch_08)
	desc$en_seg_pitch_08_kur <- kurtosis(en_segments$en_pitch_08)
	
	desc$en_seg_pitch_09_mean <- mean(en_segments$en_pitch_09)
	desc$en_seg_pitch_09_med <- median(en_segments$en_pitch_09)
	desc$en_seg_pitch_09_var <- var(en_segments$en_pitch_09)
	desc$en_seg_pitch_09_skew <- skewness(en_segments$en_pitch_09)
	desc$en_seg_pitch_09_kur <- kurtosis(en_segments$en_pitch_09)
	
	desc$en_seg_pitch_10_mean <- mean(en_segments$en_pitch_10)
	desc$en_seg_pitch_10_med <- median(en_segments$en_pitch_10)
	desc$en_seg_pitch_10_var <- var(en_segments$en_pitch_10)
	desc$en_seg_pitch_10_skew <- skewness(en_segments$en_pitch_10)
	desc$en_seg_pitch_10_kur <- kurtosis(en_segments$en_pitch_10)
	
	desc$en_seg_pitch_11_mean <- mean(en_segments$en_pitch_11)
	desc$en_seg_pitch_11_med <- median(en_segments$en_pitch_11)
	desc$en_seg_pitch_11_var <- var(en_segments$en_pitch_11)
	desc$en_seg_pitch_11_skew <- skewness(en_segments$en_pitch_11)
	desc$en_seg_pitch_11_kur <- kurtosis(en_segments$en_pitch_11)
	
	desc$en_seg_pitch_12_mean <- mean(en_segments$en_pitch_12)
	desc$en_seg_pitch_12_med <- median(en_segments$en_pitch_12)
	desc$en_seg_pitch_12_var <- var(en_segments$en_pitch_12)
	desc$en_seg_pitch_12_skew <- skewness(en_segments$en_pitch_12)
	desc$en_seg_pitch_12_kur <- kurtosis(en_segments$en_pitch_12)
	
	return(desc)
	
}


count_str <- function(A, B = NULL){
	
	start <- A[1]
	end <- A[1] + A[2]
	
	num <- nrow(subset(B, en_start >= start & en_start < end))
	
	return(num)
}


compute_rhythm_structure_desc <- function(db, excerpt_id){	

	desc <- vector(mode = 'list', length = 0)

	query <- paste('SELECT en_start, en_duration
		FROM excerpts_en_sections 
		WHERE excerpt_id = ', excerpt_id)
	en_sections <- dbGetQuery(db, query)
	
	query <- paste('SELECT en_start, en_duration
		FROM excerpts_en_segments 
		WHERE excerpt_id = ', excerpt_id)
	en_segments <- dbGetQuery(db, query)

	segments_per_section <- apply(en_sections, 1, count_str, B = en_segments)
	
	desc$en_segments_per_section_mean <- mean(segments_per_section)
	desc$en_segments_per_section_med <- median(segments_per_section)
	desc$en_segments_per_section_var <- var(segments_per_section)
	desc$en_segments_per_section_skew <- skewness(segments_per_section)
	desc$en_segments_per_section_kur <- kurtosis(segments_per_section)
		
	query <- paste('SELECT en_start, en_duration
		FROM excerpts_en_bars
		WHERE excerpt_id = ', excerpt_id)
	
	en_bars <- dbGetQuery(db, query)

	bars_per_section <- apply(en_sections, 1, count_str, B = en_bars)
	
	desc$en_bars_per_section_mean <- mean(bars_per_section)
	desc$en_bars_per_section_med <- median(bars_per_section)
	desc$en_bars_per_section_var <- var(bars_per_section)
	desc$en_bars_per_section_skew <- skewness(bars_per_section)
	desc$en_bars_per_section_kur <- kurtosis(bars_per_section)
	
	query <- paste('SELECT en_start, en_duration
		FROM excerpts_en_beats 
		WHERE excerpt_id = ', excerpt_id)
	en_beats <- dbGetQuery(db, query)
	
	beats_per_bar <- apply(en_bars, 1, count_str, B = en_beats)
	
	desc$en_beats_per_bar_mean <- mean(beats_per_bar)
	desc$en_beats_per_bar_med <- median(beats_per_bar)
	desc$en_beats_per_bar_var <- var(beats_per_bar)
	desc$en_beats_per_bar_skew <- skewness(beats_per_bar)
	desc$en_beats_per_bar_kur <- kurtosis(beats_per_bar)
	
	query <- paste('SELECT en_start
		FROM excerpts_en_tatums 
		WHERE excerpt_id = ', excerpt_id)
	en_tatums <- dbGetQuery(db, query)
	
	tatums_per_beat <- apply(en_beats, 1, count_str, B = en_tatums)
	
	desc$en_tatums_per_beat_mean <- mean(tatums_per_beat)
	desc$en_tatums_per_beat_med <- median(tatums_per_beat)
	desc$en_tatums_per_beat_var <- var(tatums_per_beat)
	desc$en_tatums_per_beat_skew <- skewness(tatums_per_beat)
	desc$en_tatums_per_beat_kur <- kurtosis(tatums_per_beat)

	return(desc)	
	
}


gen_json <- function(desc){
	
	
	json_string <- "{\n\"desc\":\n\t{\n"
	
	count <- 0
	
	for(i in names(desc)){
		
		if(!is.na(desc[[i]])){
			json_string <- paste(json_string, "\t\t\"",i,"\": ", desc[[i]], sep = "")
		}
		else{
			json_string <- paste(json_string, "\t\t\"",i,"\": null", sep = "")
		}				
		count <- count + 1
				
		if(count != length(names(desc))){
			json_string <- paste(json_string, ",", sep = "")
		}
		
		json_string <- paste(json_string, "\n", sep = "")
	}
	
	
	json_string <- paste(json_string, "\n\t}\n}")
	
	return(json_string)
	
}

