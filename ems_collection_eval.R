library(RCurl)
library(RMySQL)




gen_report <- function(db, min_num_albums = 0, min_num_tracks = 0, min_track_dur = 0, split_time = NULL, filt_artists = NULL, avoid_artists = NULL, tag = NULL, init_year = NULL, final_year = NULL, merge = FALSE, colab = FALSE, alias = TRUE){
	
	
	albums_artists <- dbReadTable(db, "albums_artists")
	albums_info <- dbReadTable(db, "albums_info")
	albums_freq <- as.data.frame(table(albums_artists$album_id))
	
	albums_artists_no_colab <- albums_artists[albums_artists$album_id %in% albums_freq[albums_freq$Freq == 1, ]$Var1 ,]
	
	artists <- unique(albums_artists_no_colab$artist_id)
	
	#Filter artists
	
	if(!is.null(filt_artists)){
						
		artists <- artists[artists %in% filt_artists]

	}
	
	if(!is.null(avoid_artists)){
						
		artists <- artists[! artists %in% avoid_artists]

	}
	
	# Filter artists by tag
	
	if(!is.null(tag)){	
		
		query = paste("SELECT tags_artists.artist_id FROM tags_artists INNER JOIN tags ON tags_artists.tag_id = tags.id WHERE tags.tag = '", tag, "';", sep = "")
		
		artists_tag <- dbGetQuery(db, query)
		
		artists <- artists[artists %in% artists_tag$artist_id]
		
	}	
	
	# Merge artists with different aliases, or discard them
	
	if(alias){
	
		artists_aliases <- dbReadTable(db, "artists_aliases")
	
		main_artists <- artists[!artists %in% artists_aliases$alias_artist_id]		
				
	}
	else{
		main_artists <- artists
	}
	
	artists_names <- dbGetQuery(db, "SELECT id, name FROM artists_info")	
	
	###
	
	tracks <- get_tracks(db, min_track_dur = min_track_dur, init_year = init_year, final_year = final_year)
	tracks_albums <- dbReadTable(db, "tracks_albums")
	
	filt_tracks_albums <- tracks_albums[tracks_albums$track_id %in% tracks$id,]
	
	repeated_tracks <- as.data.frame(table(filt_tracks_albums$track_id))
	repeated_tracks <- repeated_tracks[repeated_tracks$Freq > 1,]$Var1
	
	
	filt_tracks_albums <- filt_tracks_albums [!filt_tracks_albums$track_id %in% repeated_tracks, ]
	
	
	tracks_per_album <- vector(mode = "list", length(unique(filt_tracks_albums$album_id)))
	names(tracks_per_album) <- unique(filt_tracks_albums$album_id)
	
	for (i in names(tracks_per_album)){
		
		tracks_per_album[[i]] <- filt_tracks_albums[filt_tracks_albums$album_id == strtoi(i),]$track_id
		
		
	}
	
	
	
	# Asign repeated tracks to the shortest album
	
	for (i in repeated_tracks){
		
		albums <- tracks_albums[tracks_albums$track_id == i,]$album_id
		
		lengths <- sapply(albums, get_length, tracks_per_album)
		
		shorter_album <- albums[lengths == min(lengths)]
		
		if(length(shorter_album) == 1){
			album <- shorter_album	
		}
		else{
			album <- min(albums)
		}
		
		tracks_per_album[[paste(album)]] <- c(tracks_per_album[[paste(album)]], list(i))
		
	}
	

	###
	
	
	filt_tracks_per_album <- lapply(tracks_per_album, check_length, min_length = min_num_tracks)
	filt_tracks_per_album <- filt_tracks_per_album[!sapply(filt_tracks_per_album, is.null)]

	filt_albums_artists_no_colab <- albums_artists_no_colab[albums_artists_no_colab$album_id %in% strtoi(names(filt_tracks_per_album)),]

	albums_per_artist <- vector(mode = "list", length = length(main_artists))	
	names(albums_per_artist) <- artists_names[artists_names$id %in% main_artists,]$id
	
	for (i in names(albums_per_artist)){
		
		albums_per_artist[[i]] <- filt_albums_artists_no_colab[filt_albums_artists_no_colab $artist_id == strtoi(i),]$album_id		
		if(alias){
			
			aliases <- artists_aliases[artists_aliases$alias_main_id == strtoi(i),]$alias_artist_id

			for (j in aliases){
				albums_per_artist[[i]] <- c(albums_per_artist[[i]], filt_albums_artists_no_colab[filt_albums_artists_no_colab $artist_id == strtoi(j),]$album_id)
			}
		}
		
	}
	
	filt_albums_per_artist <- lapply(albums_per_artist, check_length, min_length = min_num_albums)
	filt_albums_per_artist <- filt_albums_per_artist[!sapply(filt_albums_per_artist, is.null)]
	
	
	# Generate report
	
	
	report <- vector(mode = "list", length(filt_albums_per_artist))
	names(report) <- names(filt_albums_per_artist)
	
	for(i in names(report)){
		
		name <- artists_names[artists_names$id == strtoi(i),]$name
		
		albums_i <- filt_albums_per_artist[[i]]
		albums_str <- vector(mode = "list", length(albums_i))
		names(albums_str) <- albums_i
			
		for(j in names(albums_str)){
			title <- albums_info[albums_info$id == strtoi(j),]$title
			year <- albums_info[albums_info$id == strtoi(j),]$year
			
			tracks_j <- filt_tracks_per_album[[j]]
			tracks_str <- vector(mode = "list", length(tracks_j))
			names(tracks_str) <- tracks_j
			
			for(k in names(tracks_str)){
				
				track <- tracks[(tracks$id == strtoi(k)) & (tracks$album_id == strtoi(j)),]
					
				track_title <- track$title

				dur <- track$dur
				
				track <- tracks_albums[(tracks_albums$track_id == strtoi(k))  & (tracks_albums$album_id == strtoi(j)) ,]
				
				number <- track$track_number
				path <- track$path
				
				tracks_str[[k]] <- structure(number, title = track_title, duration = dur, path = path)				
				
			}
			
			
			albums_str[[j]] <-  structure(title, year = year, tracks = tracks_str)
		}
		report[[i]] <- structure(name, albums = albums_str)
		
	}
	
	return(report)
	
	
	
}

get_tracks <- function(db, min_track_dur = 0, init_year = NULL, final_year = NULL){
	
	
	query <- paste("SELECT tracks_info.id, tracks_info.title, tracks_info.dur, tracks_albums.album_id FROM (tracks_info LEFT JOIN tracks_other_artists ON tracks_info.id = tracks_other_artists.track_id INNER JOIN tracks_albums ON tracks_albums.track_id = tracks_info.id) WHERE tracks_info.avail = 1 AND tracks_info.version = 0 AND tracks_info.dj_mix = 0 AND tracks_info.dur_sec >= ", min_track_dur, " AND tracks_other_artists.artist_id IS NULL ORDER BY tracks_info.id;")
	
	tracks <- dbGetQuery(db, query)
    
    if(!is.null(init_year) || !is.null(final_year)){
    
        query = "SELECT id FROM albums_info WHERE "
        
        if(!is.null(init_year)){
        
            query = paste(query, "year >= ", init_year, " ")
            
            if(!is.null(final_year)){
            
                query = paste(query, "AND ")
            }
        
        }
        if(!is.null(final_year)){
            
            query = paste(query, "year <= ", final_year)
            
        }
        
        query = paste(query, ";")
        
        albums <- dbGetQuery(db, query)
        
        tracks <- tracks[tracks$album_id %in% albums$id,]
    
    }
    
	return (tracks)

	
}


get_tags_dist <- function(db, report){
	
	artists <- strtoi(names(report))
	
	tags <- dbGetQuery(db, "SELECT tags_artists.artist_id, tags.tag FROM tags_artists INNER JOIN tags ON tags_artists.tag_id = tags.id;")
	
	filt_tags_artists <- tags[tags$artist_id %in% artists,]
	
	#print(filt_tags_artists)
	
	return(table(filt_tags_artists$tag))
	
	
}

check_length <- function(x, min_length = 5){

	if(length(x) >= min_length){
		return(x)
	}
	else{
		return(NULL)
	}
}

get_length <- function(id, table = t){
	
	return(length(table[[paste(id)]]))
	
}



filter_report <- function(report, artists){
	
	aux <- clone(report)
	
	names <- names(aux)
	
	for(i in 1:length(names)){
		
		if(!names[i] %in% artists){
			aux[[names[i]]] <- NULL
		}
				
	}
	
	print(names(aux))
	
	return(aux)
	
	
}


gen_json <- function(report){
	
	flag = FALSE
	
	json_string = "{\n\"artists\":\n\t[\n"
		
	count_i <- 0	
	
	
	for(i in names(report)){
		
		
		json_string <- paste(json_string, "\t\t{\n",sep = "")
		json_string <- paste(json_string, "\t\t\t\"id\": ", i,",\n", sep = "")
		
		artist <- report[[i]]
		
		json_string <- paste(json_string, "\t\t\t\"name\": \"", artist,"\",\n", sep = "")
		
		albums <- attributes(artist)$albums
		
		json_string = paste(json_string, "\t\t\t\"albums\":\n\t\t\t\t[\n", sep = "")
		
		count_j <- 0
		
		for(j in names(albums)){
			
				
			json_string <- paste(json_string, "\t\t\t\t\t{\n",sep = "")
			json_string <- paste(json_string, "\t\t\t\t\t\t\"id\": ", j,",\n", sep = "")	
				
			album <- albums[[j]]
			
			json_string <- paste(json_string, "\t\t\t\t\t\t\"title\": \"", album,"\",\n", sep = "")
				
			year <- attributes(album)$year
			
			json_string <- paste(json_string, "\t\t\t\t\t\t\"year\": ", year,",\n", sep = "")	
			
			tracks <- attributes(album)$tracks
			
			json_string <- paste(json_string, "\t\t\t\t\t\t\"tracks\":\n", sep = "")
			
			json_string <- paste(json_string, "\t\t\t\t\t\t\t[\n", sep = "")
			
			count_k <- 0
			
			for(k in names(tracks)){
				
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t{\n", sep = "")
					
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t\t\"id\": ", k, ",\n", sep = "")	
					
				track <- tracks[[k]]					
				
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t\t\"number\": ", track, ",\n", sep = "")
					
				title <- attributes(track)$title
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t\t\"title\": \"", title, "\",\n", sep = "")
					
				dur <- attributes(track)$duration
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t\t\"dur\": \"", dur,"\",\n", sep = "")
					
				path <- attributes(track)$path
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t\t\"path\": \"", path,"\"\n", sep = "")
									
				json_string <- paste(json_string, "\t\t\t\t\t\t\t\t}\n", sep = "")
				
				count_k <- count_k + 1
				
				if(count_k != length(names(tracks))){
					json_string <- paste(json_string, "\t\t\t\t\t\t\t\t,\n", sep = "")
				}
				
				if(!flag){
					if (length(json_string) > 1){
						print(paste(j, k, title, sep = ", "))
						flag = TRUE
					}
				}
			}
			
			json_string <- paste(json_string, "\t\t\t\t\t\t\t]\n", sep = "")
			json_string <- paste(json_string, "\t\t\t\t\t}\n",sep = "")
			
			count_j <- count_j + 1
			
			if(count_j != length(names(albums))){
				json_string <- paste(json_string, "\t\t\t\t\t,\n", sep = "")
			}
		# #
		
		}
		json_string <- paste(json_string, "\t\t\t\t]\n", sep = "")
		json_string <- paste(json_string, "\t\t}\n",sep = "")
		
		count_i <- count_i + 1
		
		if(count_i != length(names(report))){
			json_string <- paste(json_string, "\t\t,\n", sep = "")
		}
	}
		
	
		
	json_string = paste(json_string, "\t]\n}\n", sep = "")
	
	return(json_string)
	
	
}

print_report <- function(report, summary = FALSE, show_tracks = TRUE){
	
	
	cat("Number of Artists found:", length(report), '\n\n')
	
	for(i in names(report)){
		
		artist <- report[[i]]
		
		cat("Artist ID: ", i, '\n')
		cat("Artist Name: ", artist, '\n')
		
		albums <- attributes(artist)$albums
		
		cat("Number of valid albums: ", length(albums), '\n')
			
		if(!summary){
		
			cat("List of albums:", '\n\n')
		
			for(j in names(albums)){
				
				album <- albums[[j]]
				
				cat("- Album ID: ", j, '\n')
				cat("- Album Title: ", album, '\n')
				cat("- Album Year: ", attributes(album)$year, '\n')
			
				tracks <- attributes(album)$tracks
			
				cat("- Number of valid tracks: ", length(tracks), '\n')
			
				if(show_tracks){	
					cat("- List of tracks: \n\n")
			
					for(k in names(tracks)){
					
						track <- tracks[[k]]
					
						cat("-- Track ID: ", k, '\n')
						cat("-- Track Number: ", track, '\n')
						cat("-- Track Title: ", attributes(track)$title, '\n')
						cat("-- Track Duration: ", attributes(track)$duration, '\n')
						cat("-- Track Path: ", attributes(track)$path, '\n\n')
										
					}
				
				}
				cat('\n')
			
			}
		}
		
		cat('\n')
		
	}
	
	
	
	
}
