import sys, os
import argparse
import ems_db_interface as db_i
import ems_gen_dataset as gen_d
import ems_analyzer as an
import rpy2.robjects as ro
import json
import codecs


audio_extractors = ['freesound', 'echonest', 'mirtoolbox', 'essentia']

der_extractors = ['rhythm_pos', 'en_structure', 'rhythm_structure']

extractors = audio_extractors + der_extractors



def _init_args():

	parser = argparse.ArgumentParser(description = 
		'Generate and Analyze a Music Dataset.')

	parser.add_argument('-ei', '--excerpt_ids', 
		help = 'allows to analyze a series of excerpts individually. '+ \
			'Caution: the analysis will be performed even if the ' + \
			'excerpt had been already analyzed.',
		type = int, nargs = '+')

	group_dataset = parser.add_mutually_exclusive_group()

	group_dataset.add_argument('-dn', '--dataset_name', type=str, 
		help='specifies the name of the dataset to be generated or retrieved')

	group_dataset.add_argument('-di', '--dataset_id', type= int,
		help='specifies the id of the dataset to be retrieved')

	group_action = parser.add_mutually_exclusive_group()

	group_action.add_argument('-dr', '--dataset_removal',
		help='removes all the data stored in the DB related with the ' + \
			'specified dataset if it already exists. ' + \
			'If it does\'t, the program will exit', action= 'store_true')

	group_action.add_argument('-du', '--dataset_update', 
		help='updates all the data stored in the DB related with the ' + \
			'specified dataset if it already exists. ' + \
			'If it does\'t, the program will exit', action= 'store_true')

	parser.add_argument('-ce', '--clean_excerpts',
		help='removes all excerpts stored in the database without ' + \
			'an associated file path ot not previously analyzed ' +\
			'if they are not related to any dataset. ' + \
			'If combined with --dataset_removal, the dataset is removed ' + \
			'firts and afterwards the excerpts are cleaned',
			action = 'store_true')

	group_artists = parser.add_mutually_exclusive_group()

	group_artists.add_argument('-ai', '--artists_ids',
		help='an array containing the IDs of the artists to be included ' + \
			'in the dataset', type = int, nargs = '+')

	group_artists.add_argument('-an', '--artists_names',
		help='an array containing the names of the artists to be included ' + \
			'in the dataset', type = str, nargs = '+')

	parser.add_argument('-n', '--num_artists',
		help='the number of artists to be included ' + \
			'in the dataset', type = int)

	parser.add_argument('-na', '--num_albums',
		help='the number of albums per artist to be included ' + \
			'in the dataset. Default: 5', type = int, default = 5)

	parser.add_argument('-nt', '--num_tracks',
		help='the number of tracks per album to be included ' + \
			'in the dataset. Default: 5', type = int, default = 5)

	parser.add_argument('-td', '--track_duration',
		help='the minimum duration of the tracks to be included ' + \
			'in the dataset. Default: 60', type = int, default = 60)

	parser.add_argument('-ex', '--extractors',
		help='an array containing the names of the feature extractors ' + \
			'to be employed', type = str, nargs = '+', choices = extractors)

	parser.add_argument('-ec', '--excerpt_cut',
		help='specifies the type of cut to be performed to the dataset ' + \
			'tracks in order to obtain the excerpts. Options: \n' + \
			'0.- Full track (default); \n' + \
			'1.- Full track without initial and final silences (pending); \n' + \
			'2.- Only the second half of the first minute (pending) ', 
		type = int, default = 0, choices = [0, 1, 2])

	group_analysis = parser.add_mutually_exclusive_group()

	group_analysis.add_argument('-fa', '--force_analysis',
		help='forces the calculation of the features associated with ' + \
			'the excerpts of the dataset, even if they are already ' + \
			'stored in the database', action = 'store_true')

	group_analysis.add_argument('-sa', '--skip_analysis',
		help='impedes the calculation of the features associated with ' + \
			'the excerpts of the dataset', action = 'store_true')
	
	# Artist Suggestion Mode


	parser.add_argument('-as', '--artist_suggestion',
		help='returns a list of artists included in the collection ' + \
			'that match certain conditions.\n' + \
			'Suggestion: Use it together with -na, -nt, -td, -tg and/or -ar',
		action = 'store_true')

	parser.add_argument('-tg', '--tag', 
		help='allows to filter the artist suggestion mode (-as)' + \
			'by indicating a tag. ' + \
			'Only those artists that match that tag ' + \
			'will be shown.',
		type = str)

	parser.add_argument('-ar', '--avoid_repetition',
		help='in the artist suggestion mode (-as), impedes the system to' +\
			'suggest artists that have been already included in stored datasets',
		action = 'store_true')

	parser.add_argument('-te', '--test',
		help='test mode. Creates dataset and removes it just afterwards',
		action = 'store_true')

	return parser.parse_args()

def _check_args(args):

	# Dataset ID and Name

	if not args.artist_suggestion:
		
		if args.excerpt_ids is None:

			if args.dataset_id is None and args.dataset_name is None \
				and args.clean_excerpts is False:

				print 'No Dataset Name or ID specified'
				print 'Exiting'
				sys.exit(0)

	if args.excerpt_cut != 0:
		print 'Excerpt Cut Mode not yet implemented. Sorry'
		print 'Exiting'
		sys.exit(0)

if __name__=='__main__':

	print ''

	args = _init_args()
	_check_args(args)

	# Check if the user asked for artist suggestion mode

	if args.artist_suggestion:

		print 'Artist Suggestion Mode\n'

		if args.tag is None:
			tag = 'NULL'
		else:
			tag = '\'{}\''.format(args.tag)

		if args.avoid_repetition:
			ar = 'TRUE'
		else:
			ar = 'FALSE'

		print 'Searching for artists that: '
		print '- have at least {} albums '.format(args.num_albums) + \
			'with at least {} tracks of {} seconds length or more'.\
			format(args.num_tracks, args.track_duration)
		print '- have the tag {} associated with them'.\
			format(tag)
		if(args.avoid_repetition):
			print '- have not been included in any other dataset before'
		print ''

		v = ro.r('source("ems_gen_dataset.R");')


		r_call = 'suggest_artists(' + \
			'{}, {}, {}, '.format(args.num_albums, args.num_tracks, 
				args.track_duration) + \
			'tag = {}, avoid_repetition = {})'.\
			format(tag, ar)

		v = ro.r(r_call)

		print 'Exiting'
		sys.exit(0)

	# Check if the Dataset already exists in the database

	if args.dataset_name is not None:

		print 'Trying to retrieve Dataset \'{}\' from the database...'\
			.format(args.dataset_name)
		ret_dataset = db_i.get_dataset_info(dataset_name = args.dataset_name)
		
		if ret_dataset is not None:
			print 'Dataset \'{}\' found in the database with ID = {}\n'\
				.format(args.dataset_name, ret_dataset['id'])
			found = True
		else:
			print 'Dataset \'{}\' not found in the database\
				'.format(args.dataset_name)
			found = False

	elif args.dataset_id is not None:

		print 'Trying to retrieve Dataset with ID = {} from the database...'\
			.format(args.dataset_id)
		ret_dataset = db_i.get_dataset_info(dataset_id = args.dataset_id)
		#
		if ret_dataset is not None:
			print 'Dataset with ID = {} found in the database as \'{}\'\n'\
				.format(args.dataset_id, ret_dataset['name'])
			found = True
		else:
			print 'Dataset {} not found in the database'\
				.format(args.dataset_id)
			print 'Exiting'
			sys.exit(0)

	elif args.excerpt_ids is not None:

		print 'Checking if the specified excerpts are stored in the database...'

		missing = []

		for excerpt in args.excerpt_ids:
			if not db_i.check_excerpt(excerpt):
				missing.append(excerpt)

		if len(missing) > 0:
			print 'Excerpt(s) {} not found in the database'.\
				format(', '.join(map(str,missing)))
			print 'Exiting'
			sys.exit(0)
		else:
			print 'Every excerpt has been successfully found\n'


	elif args.clean_excerpts:
		print 'Removing orphan excerpts'
		db_i.remove_orphans()
		print 'Orphan excerpts successfully removed'
		print 'Exiting'
		sys.exit(0)

	else:
		print 'Invalid execution'
		print 'Exiting'
		sys.exit(0)

	# Decide the action to be performed: 
	# Generate, Update, Remove, proceed to the Analysis section or Exit

	if args.dataset_removal:
		if found:

			print 'Removing data related with the dataset \'{}\'...'\
				.format(ret_dataset['name'])

			db_i.remove_dataset(ret_dataset['id'])
			
			if args.clean_excerpts:
				print 'Removing orphan excerpts...'
				db_i.remove_orphans()

			print 'Dataset \'{}\' successfully removed from the database'\
				.format(ret_dataset['name'])
			print 'Exiting'
			sys.exit(0)

		else:
			print 'No dataset found for removal'
			print 'Exiting'
			sys.exit(0)


	elif args.dataset_update:
		if found:

			# WARNING: NOT DOING WHAT EXPECTED

			print 'Updating data related with the dataset \'{}\'...'\
				.format(ret_dataset['name'])

			print 'Removing data related with the dataset \'{}\'...'\
				.format(ret_dataset['name'])

			db_i.remove_dataset(ret_dataset['id'])
			print 'Dataset \'{}\' successfully removed from the database'\
				.format(ret_dataset['name'])

			print 'Trying to regenerate dataset \'{}\'...\n'\
				.format(ret_dataset['name'])

			dataset_name = ret_dataset['name']

		else:
			print 'No dataset found for updating'
			print 'Exiting'
			sys.exit(0)
	else:
		if args.excerpt_ids is None and not found:
			print 'Trying to generate dataset \'{}\'...\n'\
				.format(args.dataset_name)
			dataset_name = args.dataset_name

	if (args.excerpt_ids is None and (args.dataset_update or not found)):			

		# Generate Tracklist of the dataset

		artists = []

		if args.artists_ids is not None:
			print 'Obtaining artists from the IDs list...\n'

			for a in args.artists_ids:
				print 'Checking if artist with ID = {} is stored '.format(a) + \
					'in the database...'
				artist = db_i.get_artist_info(artist_id = a)
				if artist is None:
					print 'Artist with ID = {} not found'.format(a)
					print 'Exiting'
					sys.exit(0)
				else:
					print 'Artist with ID = {} successfully found\n\
						'.format(a)

				artists.append(a)

		elif args.artists_names is not None:
			print 'Obtaining artists from the Names list...\n'
			
			for a in args.artists_names:
				print 'Checking if artist \'{}\' is stored '.format(a) + \
					'in the database...'
				artist = db_i.get_artist_info(artist_name = a)
				if artist is None:
					print 'Artist \'{}\' not found'.format(a)
					print 'Exiting'
					sys.exit(0)
				else:
					print 'Artist \'{}\' successfully found\n'\
						.format(a)

				artists.append(artist['id'])

		r_ids = 'c({})'.format(', '.join(map(str,artists)))
		
		print 'Selecting the tracks to be included in the dataset...'

		v = ro.r('source("ems_gen_dataset.R");')

		r_call = 'gen_dataset({}, {}, {}, {})'\
			.format(args.num_albums, args.num_tracks, 
				args.track_duration, r_ids)

		v = ro.r(r_call)

		with open('dataset_temp.json') as f:
			report = json.load(f)

		#print report

		os.remove('dataset_temp.json')


		num_valid_artists = len(report)
		valid_artists = []
		for a in report:
			valid_artists.append(a)

		if args.num_artists is not None:
			min_num_artists = args.num_artists
		else:
			min_num_artists = len(artists)

		not_valid = []

		for a in artists:
			if a not in valid_artists:
				not_valid.append(a)

		if num_valid_artists < min_num_artists:
			print 'Not enough valid artists to complete the dataset. ' + \
				'Expected: {}, Retrieved: {}'\
				.format(min_num_artists, num_valid_artists)
			if len(not_valid) > 0:
				print 'Invalid artists: {}'\
					.format(', '.join(map(str, not_valid)))
			sys.exit(0)

		#sys.exit(0)

		dataset = gen_d.gen_dataset(report, min_num_artists, 
			args.num_albums, args.num_tracks)

		print 'Tracks successfully selected...\n'

		print 'Storing the dataset in the database...'

		# TODO: Update previously stored dataset (REMOVE AND RESTORE)


		gen_d.store_dataset_db(dataset = dataset, name = dataset_name, 
			excerpt_cut = args.excerpt_cut)

		ret_dataset = db_i.get_dataset_info(dataset_name = dataset_name)

		missing_paths = db_i.get_excerpts_info(
			dataset_id = ret_dataset['id'], without_path = True)

		# WARNING! Only valid for FULL TRACKS
		# TODO: Generate Excerpts if Cut Mode != 0

		for excerpt in missing_paths:
			excerpt_path = 'Collection/' + db_i.get_path(
				track_id = excerpt['track_id'], album_id = excerpt['album_id'])
			db_i.write_excerpt_path(excerpt['id'], excerpt_path)

		print 'Dataset excerpts successfully stored\n'

		if args.test:

			print 'Removing data related with the dataset \'{}\'...'\
				.format(ret_dataset['name'])

			db_i.remove_dataset(ret_dataset['id'])
			
			if args.clean_excerpts:
				print 'Removing orphan excerpts...'
				db_i.remove_orphans()

			print 'Dataset \'{}\' successfully removed from the database'\
				.format(ret_dataset['name'])
			print 'Exiting'
			sys.exit(0)

	if args.skip_analysis:

		print 'Skipping analysis'
		print 'Exiting'
		sys.exit(0)


	print 'Checking which excerpts need to be analyzed...'

	if args.excerpt_ids is None:

		analyzed = db_i.get_excerpts(
			dataset_id = ret_dataset['id'],
			analyzed = True)

		not_analyzed = db_i.get_excerpts(
			dataset_id = ret_dataset['id'],
			analyzed = False)
			
		if len(not_analyzed) > 0 and len(analyzed) > 0:
			print 'Found {} excerpts that have been already analyzed' \
				.format(len(analyzed)) + \
				' and {} that need to be analyzed\n'\
				.format(len(not_analyzed))
		elif len(analyzed) == 0:
			print 'No excerpt has been previously analyzed\n'
		else:
			print 'Every excerpt has been previously analyzed'
			if not args.force_analysis:
				print 'Exiting'
				sys.exit(0)
			else:
				print 'Excerpts will be re-analyzed\n'

	else:

		not_analyzed = args.excerpt_ids
		analyzed = []


	if args.extractors is not None:
		extractors = args.extractors

	print 'Performing analysis and storing...'

	if len(not_analyzed) > 0:
		an.perform_store_analysis(not_analyzed, extractors)
	if len(analyzed) > 0 and args.force_analysis:
		an.perform_store_analysis(analyzed, extractors)

	print 'Done\n'

	if set(extractors) & set(der_extractors):

		print 'Computing and storing derived descriptors...'

		if len(not_analyzed) > 0:
		 	an.compute_store_der_descriptors(not_analyzed, extractors)
		if len(analyzed) > 0 and args.force_analysis:
		 	an.compute_store_der_descriptors(analyzed, extractors)
		
		print 'Done\n'
