import pymysql
import logging
import ems_db_cfg as cfg
import ems_utils as u

###

def _connect_():
    """Establishes a MySQL connection and returns it with a cursor
    """

    config_const = cfg.read_config(config_file = 'ems_db.cfg')

    host = config_const['host']
    port = config_const['port']
    user = config_const['user']
    pwd = config_const['pwd']
    db = config_const['db']

    conn = pymysql.connect(host=host, port=port, user=user, passwd=pwd, db=db, 
        charset='utf8')

    return conn, conn.cursor()


def _do_query_(cur, query = ""):
    """Performs a SLQ query over the cursor 'cur' and returns it
    """

    cur.execute(query)
    return cur


def _print_query_(cur):
    """Prints the result of the last query performed over the cursor 'cur'
    """
	
    for r in cur.fetchall():
        print(r)

def _close_(conn, cur):
    """Closes connection 'conn' with cursor 'cur'
    """

    cur.close()
    conn.close()

###

# Helper functions

## Next ID obtainers

def _get_next_id_(table):
    """
    """

    conn, cur = _connect_()

    query = "SELECT id FROM " + table + ";"
    cur = _do_query_(cur, query)

    ids = []

    for row in cur.fetchall():
        ids.append(row[0])

    if len(ids) == 0:
        return 1

    max_id = max(ids)

    _close_(conn, cur)

    return max_id + 1

## Query generators

def _gen_insert_query_(table, fields_values):
    """
    """

    v = []

    for i in fields_values.values():
        if i is None:
            v.append('NULL')
        elif isinstance(i, basestring):
            v.append('\'' + i.replace("'", "\\'") + '\'')
        else:
            v.append(str(i))

    query = "INSERT INTO " + table + " (" + \
        ', '.join(fields_values.keys()) + ") VALUES (" + \
        ', '.join(v) + ");"

    return query

def _gen_update_by_id_query_(table, id, fields_values):
    """
    """

    set_fields = []

    for field in fields_values:
        if fields_values[field] is None:
            fields_values[field] = 'NULL'
        elif isinstance(fields_values[field], basestring):
            fields_values[field] = \
                '\'' + fields_values[field].replace("'", "\\'") + '\''
        else:
            fields_values[field] = str(fields_values[field])
        set_fields.append(field + " = " + fields_values[field])

    query = "UPDATE " + table + " SET " + ', '.join(set_fields)

    if table is 'albums_artists':
        query = query + " WHERE album_id = " + str(id) + ";"
    elif table is 'excerpts_descriptors' \
            or table is 'excerpts_meta':
        query = query + " WHERE excerpt_id = " + str(id) + ";"
    else:
        query = query + " WHERE id = " + str(id) + ";"

    return query

def _gen_select_by_id_query_(table, id, fields):
    """
    """
    query = "SELECT " + ', '.join(fields) + " FROM " + table + \
            " WHERE id = " + str(id) + ";"

    return query

def _gen_select_condition_query_(table, fields, condition = None):
    """
    """
    query = "SELECT " + ', '.join(fields) + " FROM " + table
    if condition is None:
        query = query + ";"
    else:
        query = query + " " + condition + ";"

    return query


## Query dictionaries converters

def _convert_artist_info_for_query_(artist_id = None, artist_info = None):
    """
    """

    query_artist_info = {}

    if artist_id is None and artist_info is None:
        return None
    
    if artist_id is not None:
        query_artist_info['id'] = str(artist_id)

    if artist_info is None:
        return query_artist_info

    if 'name' in artist_info.keys():
        if artist_info['name'] is None:
            query_artist_info['name'] = 'NULL'
        else:
            query_artist_info['name'] = "\'" + artist_info['name'] + "\'"

    if 'en_id' in artist_info.keys():
        if artist_info['en_id'] is None:
            query_artist_info['en_id'] = 'NULL'
        else:
            query_artist_info['en_id'] = "\'" + artist_info['en_id'] + "\'"

    if 'mb_id' in artist_info.keys():
        if artist_info['mb_id'] is None:
            query_artist_info['mb_id'] = 'NULL'
        else:
            query_artist_info['mb_id'] = "\'" + artist_info['mb_id'] + "\'"

    if 'sort_name' in artist_info.keys()  :      
        if artist_info['sort_name'] is None:
            query_artist_info['sort_name'] = 'NULL'
        else:
            query_artist_info['sort_name'] = "\'" + artist_info['sort_name'] + "\'"

    if 'area' in artist_info.keys():
        if artist_info['area'] is None:
            query_artist_info['area'] = 'NULL'
        else:
            query_artist_info['area'] = "\'" + artist_info['area'] + "\'"

    if 'begin' in artist_info.keys():
        if artist_info['begin'] is None:
            query_artist_info['begin'] = 'NULL'
        else:
            query_artist_info['begin'] = str(artist_info['begin'])

    if 'end' in artist_info.keys():
        if artist_info['end'] is None:
            query_artist_info['end'] = 'NULL'
        else:
            query_artist_info['end'] = str(artist_info['end'])

    if 'type' in artist_info.keys():
        if artist_info['type'] is None:
            query_artist_info['type'] = 'NULL'
        else:
            query_artist_info['type'] = "\'" + artist_info['type'] + "\'"

    return query_artist_info

def _convert_album_info_for_query_(album_id = None, album_info = None):
    """
    """

    query_album_info = {}


    if album_id is None and album_info is None:
        return None
    
    if album_id is not None:
        query_album_info['id'] = str(album_id)

    if album_info is None:
        return query_album_info

    if 'title' in album_info.keys():
        if album_info['title'] is None:
            query_album_info['title'] = 'NULL'
        else:
            query_album_info['title'] = "\'" + album_info['title'] + "\'"

    if 'year' in album_info.keys():
        if album_info['year'] is None:
            query_album_info['year'] = 'NULL'
        else:
            query_album_info['year'] =  str(album_info['year'])

    if 'mb_id' in album_info.keys():
        if album_info['mb_id'] is None:
            query_album_info['mb_id'] = 'NULL'
        else:
            query_album_info['mb_id'] = "\'" + album_info['mb_id'] + "\'"

    if 'is_com' in album_info.keys():
        if album_info['is_com'] is None:
            query_album_info['is_com'] = 'NULL'
        else:
            query_album_info['is_com'] = str(int(album_info['is_com']))

    if 'path' in album_info.keys():
        if album_info['path'] is None:
            query_album_info['path'] = 'NULL'
        else:
            query_album_info['path'] = "\'" + album_info['path'] + "\'"

    return query_album_info

def _convert_track_info_for_query_(track_id = None, track_info = None):
    """
    """

    query_track_info = {}

    if track_id is None and track_info is None:
        return None
    
    if track_id is not None:
        query_track_info['id'] = str(track_id)

    if track_info is None:
        return query_track_info

    if 'title' in track_info.keys():
        if track_info['title'] is None:
            query_track_info['title'] = 'NULL'
        else:
            query_track_info['title'] = "\'" + track_info['title'] + "\'"

    if 'mb_id' in track_info.keys():
        if track_info['mb_id'] is None:
            query_track_info['mb_id'] = 'NULL'
        else:
            query_track_info['mb_id'] = "\'" + track_info['mb_id'] + "\'"

    if 'dur_sec' in track_info.keys() and track_info['dur_sec'] is not None:
        if 'dur' in track_info.keys() and track_info['dur'] is not None:
            query_track_info['dur_sec'] = str(track_info['dur_sec'])
            query_track_info['dur'] = "\'" + track_info['dur'] + "\'"
        else:
            dur_sec, dur = u.convert_track_duration(dur_sec = track_info['dur_sec'])
            query_track_info['dur_sec'] = str(dur_sec)
            query_track_info['dur'] = "\'" + dur + "\'"
    else:
        if 'dur' in track_info.keys() and track_info['dur'] is not None:
            dur_sec, dur = u.convert_track_duration(dur = track_info['dur'])
            query_track_info['dur_sec'] = str(dur_sec)
            query_track_info['dur'] = "\'" + dur + "\'"

    if 'avail' in track_info.keys():
        if track_info['avail'] is None:
            query_track_info['avail'] = '0'
        else:
            query_track_info['avail'] = str(int(track_info['avail']))

    if 'version' in track_info.keys():
        if track_info['version'] is None:
            query_track_info['version'] = '0'
        else:
            query_track_info['version'] = str(int(track_info['version']))

    if 'original_track' in track_info.keys():
        if track_info['original_track'] is None:
            query_track_info['original_track'] = 'NULL'
        else:
            query_track_info['original_track'] = str(int(track_info['original_track']))
    if 'remix' in track_info.keys():
        if track_info['remix'] is None:
            query_track_info['remix'] = '0'
        else:
            query_track_info['remix'] = str(int(track_info['remix']))

    if 'live' in track_info.keys():
        if track_info['live'] is None:
            query_track_info['live'] = '0'
        else:
            query_track_info['live'] = str(int(track_info['live']))

    if 'acoustic' in track_info.keys():
        if track_info['acoustic'] is None:
            query_track_info['acoustic'] = '0'
        else:
            query_track_info['acoustic'] = str(int(track_info['acoustic']))

    if 'dj_mix' in track_info.keys():
        if track_info['dj_mix'] is None:
            query_track_info['dj_mix'] = '0'
        else:
            query_track_info['dj_mix'] = str(int(track_info['dj_mix']))

    return query_track_info

###

# Structure modifiers

def add_descriptor_column(descriptor):

    query = "SELECT * FROM information_schema.COLUMNS " + \
            "WHERE TABLE_NAME = \'excerpts_descriptors\' " + \
            "AND COLUMN_NAME = \'" + descriptor + "\';"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    if response is None or len(response) == 0:
        query = "ALTER TABLE excerpts_descriptors " + \
                "ADD COLUMN " + descriptor + " float(11) DEFAULT NULL;"
        cur = _do_query_(cur, query)
        conn.commit()
    
    _close_(conn, cur)

###

# Checkers

def check_album(album_id = None, mb_id = None):
    """
    """

    if album_id is None and mb_id is None:

        print "Nothing to check"
        return False

    if mb_id is None:
        query = "SELECT id FROM albums_info WHERE id = " + str(album_id) + ";"
    elif album_id is None:
        query = "SELECT id FROM albums_info WHERE mb_id = \'" + mb_id + "\';"
    else:
        query = "SELECT id FROM albums_info " + \
                "WHERE id = " + str(album_id) + " AND mb_id = \'" + mb_id + "\';"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    if len(response) > 0:
        check = True
    else:
        check = False

    _close_(conn, cur)

    return check

def check_full(excerpt_id):
    """
    """

    query = "SELECT id FROM excerpts " + \
            "WHERE id = " + str(excerpt_id) + " " + \
            "AND start IS NULL AND end IS NULL;"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    if len(response) > 0:
        check = True
    else:
        check = False

    _close_(conn, cur)

    return check

# Adders

def add_artist(artist_id = None, artist_info = None):
    """Adds an entry to the artists table in the database
    """

    if artist_id is None:
        artist_id = _get_next_id_('artists_info')
    
    query_artist_info = _convert_artist_info_for_query_(artist_id, artist_info)

    query = _gen_insert_query_(table = 'artists_info', \
                fields_values = query_artist_info)

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()

    #TODO: Implement a manager for SQL errors (like, for example, id duplicity)

    _close_(conn, cur)

def add_album(album_id = None, album_info = None):
    """Adds an entry to the albums table in the database
    """

    if album_id is None:
        album_id = _get_next_id_('albums_info')

    query_album_info = _convert_album_info_for_query_(album_id, album_info)

    query = _gen_insert_query_(table = 'albums_info', \
                fields_values = query_album_info)

    conn, cur = _connect_()

    try:
        cur = _do_query_(cur, query)
        conn.commit()
    except Exception, ex:
        logging.exception("Error writing album title")
        logging.debug("Skipping!")
        _close_(conn, cur)

    if 'artists' in album_info.keys():
        for artist in album_info['artists']:
            query = "INSERT INTO albums_artists (album_id, artist_id) \
                    VALUES (" + str(album_id) + ", " + str(artist) + ");" 
            cur = _do_query_(cur, query)
    conn.commit()

    #TODO: Implement a manager for SQL errors (like, for example, id duplicity)

    _close_(conn, cur)

def add_track(track_id = None, track_info = None):
    """Adds an entry to the albums table in the database
    """

    if track_id is None:
        track_id = _get_next_id_('tracks_info')

    query_track_info = _convert_track_info_for_query_(track_id, track_info)

    query = _gen_insert_query_(table = 'tracks_info', \
                fields_values = query_track_info)

    conn, cur = _connect_()

    try:
        cur = _do_query_(cur, query)
        conn.commit()
    except Exception, ex:
        logging.exception("Error writing track title")
        logging.debug("Skipping!")
        _close_(conn, cur)

        if track_info['title'] is not "":
            track_info['title'] = ""
            add_track(track_id, track_info)
            return

        conn, cur = _connect_()

    if 'artists' in track_info.keys():
        for artist in track_info['artists']:
            query = "INSERT INTO tracks_other_artists (track_id, artist_id) " + \
                    "VALUES (" + str(track_id) + ", " + str(artist) + ");" 
            cur = _do_query_(cur, query)

    if 'remixers' in track_info.keys():
        for remixer in track_info['remixers']:
            query = "INSERT INTO tracks_remixers (track_id, remixer_id) " +\
                    "VALUES (" + str(track_id) + ", " + str(remixer) + ");" 
            cur = _do_query_(cur, query)
    
    conn.commit()

    #TODO: Implement a manager for SQL errors (like, for example, id duplicity)

    _close_(conn, cur)

def add_album_tracklist(album_id, tracklist):
    """
    """

    if album_id is None:
        print 'Invalid album ID'
        return

    if tracklist is None:
        print 'Invalid tracklist'
        return

    conn, cur = _connect_()

    for track in tracklist:
        query = "INSERT INTO tracks_albums (album_id, track_id, track_number) " + \
                "VALUES (" + str(album_id) + ", " + str(track['track_id']) + ", " + \
                str(track['track_number']) + ");" 
        cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def add_tag(tag_id = None, tag = None):
    """
    """

    if tag_id is None:
        tag_id = _get_next_id_('tags')

    if tag is None:
        tag = 'NULL'
    else:
        tag = '\'' + tag + '\''

    query = "INSERT INTO tags (id, tag) " + \
            "VALUES (" + str(tag_id) + ", " + tag + ");"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()

    _close_(conn, cur)

def add_tag_artist(tag_id, artist_id):
    """
    """

    if tag_id is None:
        print 'Invalid tag ID'
        return

    if artist_id is None:
        print 'Invalid artist ID'
        return

    query = "INSERT INTO tags_artists (tag_id, artist_id) " + \
            "VALUES (" + str(tag_id) + ", " + str(artist_id) + ");"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()

    _close_(conn, cur)

def add_dataset(dataset_id = None, name = "", tracks = [], excerpt_cut = 0):
    """
    """

    if dataset_id is None:
        dataset_id = _get_next_id_('datasets')

    if name is None:
        name = '\'\''
    else:
        name = '\'' + name + '\''

    query = "INSERT INTO datasets (id, name) " + \
            "VALUES (" + str(dataset_id) + ", " + name + ");"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()
    _close_(conn, cur)


    for track in tracks:

        if(excerpt_cut != 0):
            query = "SELECT dur_sec FROM tracks WHERE id = " + \
                    str(track['track_id']) + ";"

            conn, cur = _connect_()
            cur = _do_query_(cur, query)
            response = cur.fetchall()
            _close_(conn, cur)

            if len(response) > 0:

                start, end = u.obtain_excerpt_limits(dur = dur_sec, 
                    excerpt_type = excerpt_type)
                start = str(start)
                end = str(end)
        else:
            start = None
            end = None

        # check if excerpt already stored in DB

        if start is None or end is None:
            query = "SELECT id FROM excerpts" + \
                    " WHERE track_id = " + str(track['track_id']) + \
                    " AND album_id = " + str(track['album_id']) + \
                    " AND start IS NULL AND end IS NULL;"
        else:
            query = "SELECT id FROM excerpts" + \
                    " WHERE track_id = " + str(track['track_id']) + \
                    " AND album_id = " + str(track['album_id']) + \
                    " AND start = " + str(start) + \
                    " AND end = " + str(end) + ";"  

        conn, cur = _connect_()
        cur = _do_query_(cur, query)

        response = cur.fetchall()
        _close_(conn, cur)

        if response is None or len(response) == 0:

            excerpt_info = {}
            excerpt_info['track_id'] = track['track_id']
            excerpt_info['album_id'] = track['album_id']
            excerpt_info['start'] = start
            excerpt_info['end'] = end

            excerpt_id = add_excerpt(excerpt_info)

        else:
            excerpt_id = response[0][0]
        
        add_excerpt_dataset(excerpt_id = excerpt_id, dataset_id = dataset_id)

def add_excerpt(excerpt_info):
    """
    """

    if 'id' not in excerpt_info.keys() or excerpt_info['id'] is None:
        excerpt_id = _get_next_id_('excerpts')
    else:
        excerpt_id = excerpt_info['id']

    if excerpt_info['start'] is None or excerpt_info['end'] is None:
        query = "INSERT INTO excerpts (id, album_id, track_id) " + \
                "VALUES (" + str(excerpt_id) + ", " + \
                    str(excerpt_info['album_id']) + ", " + \
                    str(excerpt_info['track_id']) + ");"

    else:
        query = "INSERT INTO excerpts " + \
                "(id, album_id, track_id, start, end) " + \
                "VALUES (" + str(excerpt_id) + ", " + \
                    str(excerpt_info['album_id']) + ", " + \
                    str(excerpt_info['track_id']) + ", " + \
                    str(excerpt_info['start']) + ", " + \
                    str(excerpt_info['end']) + ");"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()
    _close_(conn, cur)

    return excerpt_id

def add_excerpt_dataset(excerpt_id, dataset_id):
    """
    """

    query = "INSERT INTO excerpts_datasets (dataset_id, excerpt_id) " + \
            "VALUES (" + str(dataset_id) + ", " + str(excerpt_id) + ");"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    conn.commit()
    _close_(conn, cur)

def add_descriptors(excerpt_id, features):
    """
    """

    conn, cur = _connect_()

    query = "SELECT excerpt_id FROM excerpts_descriptors " + \
            "WHERE excerpt_id = " + str(excerpt_id) + ";"

    
    cur = _do_query_(cur, query)
    
    response = cur.fetchall()

    if response is None or len(response) == 0:
        features['excerpt_id'] = excerpt_id
        query = _gen_insert_query_('excerpts_descriptors', features)

    else:
        query = _gen_update_by_id_query_('excerpts_descriptors', 
            excerpt_id, features)

    cur = _do_query_(cur, query)
    conn.commit()
    _close_(conn, cur)

def add_meta(excerpt_id, meta):
    """
    """

    conn, cur = _connect_()

    query = "SELECT excerpt_id FROM excerpts_meta " + \
            "WHERE excerpt_id = " + str(excerpt_id) + ";"

    
    cur = _do_query_(cur, query)
    
    response = cur.fetchall()

    if response is None or len(response) == 0:
        meta['excerpt_id'] = excerpt_id
        query = _gen_insert_query_('excerpts_meta', meta)

    else:
        query = _gen_update_by_id_query_('excerpts_meta', 
            excerpt_id, meta)

    cur = _do_query_(cur, query)
    conn.commit()
    _close_(conn, cur)

def add_excerpt_array_table(excerpt_id, table, array):

    conn, cur = _connect_()

    query = "DELETE FROM " + table + \
            " WHERE excerpt_id = " + str(excerpt_id) + ";"
    cur = _do_query_(cur, query)
    conn.commit()

    for i in array:
        i['excerpt_id'] = excerpt_id
        query = _gen_insert_query_(table, i)
        cur = _do_query_(cur, query)


    conn.commit()
    _close_(conn, cur)

# Editors

def edit_artist(artist_id, artist_info):
    """
    """

    if artist_id is None:
        print 'Invalid track ID'
        return

    if artist_info is None:
        print 'Nothing to Edit'
        return

    query_artist_info = _convert_artist_info_for_query_(artist_info = artist_info)

    query = _gen_update_by_id_query_(table = 'artists_info', id = artist_id, \
                fields_values = query_artist_info)

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def edit_album(album_id, album_info):
    """
    """

    #TODO: artists

    if album_id is None:
        print 'Invalid album ID'
        return

    if album_info is None:
        print 'Nothing to Edit'
        return

    query_album_info = _convert_album_info_for_query_(album_info = album_info)

    query = _gen_update_by_id_query_(table = 'albums_info', id = album_id, \
                fields_values = query_album_info)

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def edit_track(track_id, track_info):
    """
    """

    #TODO: artists and remixers

    if track_id is None:
        print 'Invalid track ID'
        return

    if track_info is None:
        print 'Nothing to Edit'
        return

    query_track_info = _convert_track_info_for_query_(track_info = track_info)

    query = _gen_update_by_id_query_(table = 'tracks_info', id = track_id, \
                fields_values = query_track_info)

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

# File path writers

def write_album_path(album_id, path):
    """
    """

    if album_id is None:
        print 'Invalid Album ID'
        return

    if path is None:
        print 'Nothing to Edit'
        return

    path = path.replace("'", "\\'")

    query = _gen_update_by_id_query_(table = 'albums_info', id = album_id, \
                fields_values = {'path': "\'" + path + "\'"})

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def write_track_album_path(album_id, track_number, path):
    """
    """

    if album_id is None or track_number is None:
        print 'Invalid track'
        return

    if path is None:
        print 'Nothing to Edit'
        return

    path = path.replace("'", "\\'")

    query = "UPDATE tracks_albums SET path = \'" + path + "\'" + \
                " WHERE album_id = " + str(album_id) + " AND "+ \
                "track_number = " + str(track_number) + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def write_excerpt_path(excerpt_id, path):
    """
    """

    if excerpt_id is None:
        print 'Invalid excerpt'
        return

    if path is None:
        print 'Nothing to Edit'
        return

    path = path.replace("'", "\\'")

    query = "UPDATE excerpts SET path = \'" + path + "\'" + \
                " WHERE id = " + str(excerpt_id) + ";"

    
    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

# Get 

def get_artist_name(artist_id):
    """
    """

    query = "SELECT name FROM artists_info WHERE id =  " + str(artist_id) + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    return response[0][0]

def get_artist_info(artist_id = None, artist_name = None):
    """
    """

    if artist_id is None and artist_name is None:
        return get_artists_info()

    query = "SELECT id, name, mb_id, en_id FROM artists_info WHERE "
    if artist_id is not None:
        query = query + "id = " + str(artist_id)
        if artist_name is not None:
            query = query + " AND name = \'{}\'".format(artist_name)
    else:
        query = query + "name = \'{}\'".format(artist_name)
    query = query + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    if response is None or len(response) is 0:
        return None

    artist_info = {}

    artist_info['id'] = response[0][0]
    artist_info['name'] = response[0][1]
    artist_info['mb_id'] = response[0][2]
    artist_info['en_id'] = response[0][3] 

    _close_(conn, cur)

    return artist_info

def get_artists_info():
    """
    """

    query = "SELECT id, name, mb_id, en_id FROM artists_info;"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    if response is None or len(response) is 0:
        return None

    artists = []

    for artist in response:
        artist_info = {}

        artist_info['id'] = artist[0]
        artist_info['name'] = artist[1]
        artist_info['mb_id'] = artist[2]
        artist_info['en_id'] = artist[3] 

        artists.append(artist_info)



    return artists

def get_album_info(album_id):
    """
    """

    query = "SELECT id, title, year, path, mb_id FROM albums_info " + \
            "WHERE id = " + str(album_id) + ";"


    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    if response is None or len(response) is 0:
        return None

    album_info = {}

    album_info['id'] = response[0][0]
    album_info['title'] = response[0][1] 
    album_info['year'] = response[0][2]
    album_info['path'] = response[0][3]
    album_info['mb_id'] = response[0][4]

    query = "SELECT artist_id FROM albums_artists " + \
                "WHERE album_id = " + str(response[0][0]) + ";"
    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    response = cur.fetchall()

    artists = []
    for artist in response:
        artists.append(artist[0])

    album_info['artists'] = artists

    _close_(conn, cur)

    return album_info

def get_albums_info(without_path = False):
    """
    """

    query = "SELECT id, title, year, path, mb_id FROM albums_info"

    if without_path is False:
        query = query + ";"
    else:
        query = query + " WHERE path IS NULL;"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    if response is None or len(response) is 0:
        return None

    albums = []

    for album in response:
        album_info = {}

        album_info['id'] = album[0]
        album_info['title'] = album[1]
        album_info['year'] = album[2]
        album_info['path'] = album[3]
        album_info['mb_id'] = album[4] 

        query = "SELECT artist_id FROM albums_artists " + \
                "WHERE album_id = " + str(album[0]) + ";"
        conn, cur = _connect_()
        cur = _do_query_(cur, query)
        response = cur.fetchall()

        artists = []
        for artist in response:
            artists.append(artist[0])

        album_info['artists'] = artists

        albums.append(album_info)

    return albums

def get_tracks_info(without_path = False, available = True):
    """
    """

    query = "SELECT tracks_info.id, tracks_info.title, tracks_albums.album_id, " + \
            "tracks_albums.track_number, albums_info.path " + \
            "FROM ((tracks_info " + \
            "INNER JOIN tracks_albums " + \
            "ON tracks_info.id = tracks_albums.track_id) " + \
            "INNER JOIN albums_info " + \
            "ON albums_info.id = tracks_albums.album_id)"

    if without_path and available:
        query = query + " WHERE tracks_albums.path IS NULL AND tracks_info.avail = 1;"
    elif without_path:
        query = query + " WHERE tracks_albums.path IS NULL;"
    elif available:
        query = query + " WHERE tracks_info.avail = 1;"
    else:
        query = query + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    tracks = []

    for track in response:
        
        track_info = {}

        track_info['id'] =  track[0]
        track_info['title'] = track[1]
        track_info['album'] = track[2]
        track_info['track_number'] = track[3]
        track_info['album_path'] = track[4]

        query = "SELECT artist_id FROM tracks_other_artists " + \
                "WHERE track_id = " + str(track[0]) + ";"
        conn, cur = _connect_()
        cur = _do_query_(cur, query)
        response = cur.fetchall()

        artists = []
        for artist in response:
            artists.append(int(artist[0]))

        track_info['artists'] = artists

        tracks.append(track_info)

    return tracks

def get_excerpts(dataset_id = None, analyzed = None):
    """
    """

    query = 'SELECT excerpt_id FROM excerpts_datasets'

    if dataset_id is not None or not_analyzed is not None:
        query = query + ' WHERE '
        if dataset_id is not None:
            query = query + 'dataset_id = {}'.format(dataset_id)
            if analyzed is not None:
                query = query + ' AND '
        if analyzed is not None:
            if analyzed:
                query = query + \
                    'excerpt_id IN ' + \
                    '(SELECT excerpt_id FROM excerpts_descriptors)'
            else:
                query = query + \
                    'excerpt_id NOT IN ' + \
                    '(SELECT excerpt_id FROM excerpts_descriptors)'

    query = query + ';'

    conn, cur = _connect_()

    cur = _do_query_(cur, query)

    excerpts = []

    response = cur.fetchall()

    _close_(conn, cur)

    if response is None or len(response) is 0:
        return excerpts

    for excerpt in response:
        excerpts.append(excerpt[0])

    return excerpts

def get_excerpts_info(dataset_id = None, without_path = False):
    """
    """

    if dataset_id is None:

        query = "SELECT id, album_id, track_id, start, end, path " + \
            "FROM excerpts"

        if without_path:
            query = query + " WHERE path is NULL;"
        else:
            query = query + ";"

    else:

        query = "SELECT e.id, e.album_id, e.track_id, e.start, e.end, e.path " +\
            "FROM excerpts e " + \
            "INNER JOIN excerpts_datasets ed " + \
            "ON e.id = ed.excerpt_id " + \
            "WHERE ed.dataset_id = " + str(dataset_id)

        if without_path:
            query = query + " AND path is NULL;"
        else:
            query = query + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    excerpts = []

    for excerpt in response:
        
        excerpt_info = {}

        excerpt_info['id'] =  excerpt[0]
        excerpt_info['album_id'] = excerpt[1]
        excerpt_info['track_id'] = excerpt[2]
        excerpt_info['start'] = excerpt[3]
        excerpt_info['end'] = excerpt[4]
        excerpt_info['path'] = excerpt[5]

        excerpts.append(excerpt_info)

    return excerpts
    
def get_path(album_id, track_id):
    """
    """

    query = "SELECT path FROM tracks_albums " + \
            "WHERE track_id = " + str(track_id) + \
            " AND album_id = " + str(album_id) + ";"

    conn, cur = _connect_()
    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    if len(response) > 0:
        return response[0][0]
    else:
        return None

def get_paths(dataset_id = None, excerpt_ids = None):
    """
    """

    conn, cur = _connect_()

    if dataset_id is None and excerpt_ids is None:

        query = "SELECT track_id, path FROM tracks_albums " + \
                "WHERE path is NOT NULL;"

    elif dataset_id is not None:

        query = "SELECT e.id, e.path FROM excerpts e " + \
                "INNER JOIN excerpts_datasets ed " + \
                "ON e.id = ed.excerpt_id " + \
                "WHERE ed.dataset_id = {};".format(dataset_id)
    else:

        paths = []

        for id in excerpt_ids:
            query = "SELECT id, path FROM excerpts " + \
                "WHERE id = {};".format(id)
            cur = _do_query_(cur, query)
            response = cur.fetchall()
            path = {}
            path['id'] = response[0][0]
            path['path'] = response[0][1]
            paths.append(path)

        _close_(conn, cur)

        return paths


    cur = _do_query_(cur, query)

    response = cur.fetchall()

    _close_(conn, cur)

    paths = []

    for r in response:
        aux = {}
        aux['id'] = r[0]
        aux['path'] = r[1]
        paths.append(aux)

    return paths

def get_descriptors(dataset_ids = None, excerpt_ids = None, descriptors = []):
    """
    """

    # TODO

    if len(descriptors) == 0:
        desc = '*'
    else:
        desc = ', '.join(descriptors)
    query = "SELECT "

def get_tag(tag_id = None, tag = None):
    """
    """

    query = "SELECT id, tag FROM tags"

    if tag_id is not None or tag is not None:


        query = query + " WHERE "

        if tag_id is not None:

            query = query + "id = " + str(tag_id)

            if tag is not None:

                query = query + " AND "
        
        if tag is not None:

            query = query + "tag = " + '\'' + tag + '\''

    else:

        return None

    query = query + ';'

    conn, cur = _connect_()
    cur = _do_query_(cur, query)
    response = cur.fetchall()

    tag = {}

    _close_(conn, cur)

    if len(response) > 0:

        tag['id'] = response[0][0]
        tag['tag'] = response[0][1]

        return tag

    else:

        return None

def get_dataset_info(dataset_id = None, dataset_name = None):
    """
    """

    if dataset_id is None and dataset_name is None:
        query = "SELECT * FROM datasets;"
    else:
        query = "SELECT * FROM datasets WHERE "
        if dataset_id is not None:
            query = query + "id = " + str(dataset_id)
            if dataset_name is not None:
                query = query + " AND name = \'{}\'".format(dataset_name)
        else:
            query = query + "name = \'{}\'".format(dataset_name)
    query = query + ";"

    conn, cur = _connect_()

    cur = _do_query_(cur, query)
    response = cur.fetchall()
    _close_(conn, cur)
    dataset = {}

    if len(response) > 0:

        dataset['id'] = response[0][0]
        dataset['name'] = response[0][1]

        return dataset

    else:

        return None

# Deletions

def remove_dataset(dataset_id):
    """
    """

    if dataset_id is None:
        print 'Nothing to remove'
        return

    conn, cur = _connect_()

    query = 'DELETE FROM excerpts_datasets WHERE dataset_id = {};\
        '.format(dataset_id)

    cur = _do_query_(cur, query)

    query = 'DELETE FROM datasets WHERE id = {};\
        '.format(dataset_id)

    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)

def remove_orphans():
    """
    """

    conn, cur = _connect_()

    query = 'DELETE FROM excerpts WHERE ' + \
        'id NOT IN (SELECT excerpt_id FROM excerpts_descriptors) ' + \
        'AND id NOT IN (SELECT excerpt_id FROM excerpts_datasets) ' + \
        'OR path IS NULL;'


    cur = _do_query_(cur, query)

    conn.commit()

    _close_(conn, cur)
