import ems_db_cfg as cfg
import itertools as it
import ems_db_interface as i
import os


config_const = cfg.read_config(config_file = 'ems_db.cfg', section = 'Files')

main_path = config_const['main_path']

###

def convert_track_duration(dur_sec = None, dur = None):
    """
    """

    if dur_sec is None and dur is None:
        return None, None

    if dur_sec is None:
        duration_min, duration_sec = dur.split(':')
        dur_sec = int(duration_min)*60 + int(duration_sec)

    if dur is None:
        duration_min = dur_sec / 60
        duration_sec = dur_sec % 60

        if duration_min < 10:
            duration_str_min = '0' + str(duration_min)
        else:
            duration_str_min = str(duration_min)

        if duration_sec < 10:
            duration_str_sec = '0' + str(duration_sec)
        else:
            duration_str_sec = str(duration_sec)

        dur = duration_str_min + ":" + duration_str_sec


    return dur_sec, dur


def convert_name(name):
    """
    """

    if name != u"Ex:El":
        name = name.replace(":", ".")
    else:
        name = name.replace(":", "_")

    name = name.replace("?", "")
    name = name.replace("/", "-")

    return name

def convert_track_number(track_number):
    """
    """

    if track_number < 10:
        return "0" + str(track_number)
    else:
        return str(track_number)

###

def construct_artist_path(artists_names):
    """
    """

    perm_names = list(it.permutations(artists_names, len(artists_names)))

    for perm in perm_names:

        path = ', '.join(perm)

        if os.path.isdir(main_path + "/" + path):
            return path

    return None

def construct_album_path(album_info, artist_path):
    """
    """

    if 'title' not in album_info.keys() or album_info['title'] is None:
        print "No album title provided"
        return None

    if 'year' not in album_info.keys() or album_info['year'] is None:
        print "No album year provided"
        return None

    title = convert_name(album_info['title'])

    path = artist_path + "/" + str(album_info['year']) + " - " + title

    if os.path.isdir(main_path + "/" + path):
        return path

    return None

def construct_track_path(track_info, album_path):
    """
    """

    if 'title' not in track_info.keys() or track_info['title'] is None:
        print "No track title provided"
        return None

    if 'track_number' not in track_info.keys() or track_info['track_number'] is None:
        print "No track number provided"
        return None

    title = convert_name(track_info['title'])

    if 'artists_names' in track_info.keys() and len(track_info['artists_names']) > 0:

        artists_names = track_info['artists_names']

        perm_names = list(it.permutations(artists_names, len(artists_names)))

        for perm in perm_names:

            artists = ', '.join(perm)

            path = album_path + "/" + convert_track_number(track_info['track_number']) + \
                " - " + artists + " - " + title + ".mp3"

            if os.path.isfile(main_path + "/" + path):
                return path
             
    else:

        track_number = convert_track_number(track_info['track_number'])

        path = album_path + "/" + convert_track_number(track_info['track_number']) + \
            " - " + title + ".mp3"

        if os.path.isfile(main_path + "/" + path):
            return path
    
    return None


def construct_excerpt_path(excerpt_info = None):
    """
    """

    if excerpt_info['start'] is None or excerpt_info['end'] is None:
        path = i.get_path(track_id = excerpt_info['track_id'], \
                            album_id = excerpt_info['album_id'])
        if os.path.isfile(main_path + "/" + path):
            return path
        else:
            return None
    else:
        path = str(excerpt_info['album_id']) + "_" + \
                str(excerpt_info['track_id']) + "_" + \
                str(excerpt_info['start']) + "_" + \
                str(excerpt_info['end']) + ".mp3"
        if os.path.isfile(main_path + '/Excerpt/' + path):
            return path
        else:
            return None


###

def obtain_excerpt_limits(dur, excerpt_cut = 0):
    """
    """

    # TODO: Pending Silence Cutter

    if excerpt_cut ==  2:
        start = 30
        end = 60
    elif excerpt_cut == 3:
        start = dur - 60
        end = dur - 30
    elif excerpt_cut == 4:
        start = dur/2 - 15
        end = dur/2 + 15
    else:
        return None, None

    return start, end