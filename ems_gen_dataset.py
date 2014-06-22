import json
import random
import ems_db_interface as i


def gen_dataset_json(filename, num_artists = 3, num_albums = 5, num_tracks = 5):
    """
    """

    with open(filename) as f:
        report = json.load(f)

    gen_dataset(report, num_artists, num_albums, num_tracks)


def gen_dataset(report, num_artists = 3, num_albums = 5, num_tracks = 5):
    """
    """

    dataset = {}

    artists = []

    for i in random.sample(report.keys(), num_artists):

        #print(i)


        artist = {}
        artist['id'] = report[i]
        artist['albums'] = []
        for j in random.sample(report[i]['albums'].keys(), num_albums):
            album = {}
            album['id'] = j
            album['tracks'] = []
            for k in random.sample(report[i]['albums'][j]['tracks'].keys(), num_tracks):
                track = {}
                track['id'] = k
                album['tracks'].append(track)
            artist['albums'].append(album)
        artists.append(artist)

    dataset['artists'] = artists

    return dataset

def store_dataset_json(filename, dataset):
    """
    """

    try:

        with open(filename, 'w') as f:
          f.write(json.dumps(dataset, indent = 4))

    except:

        print "Unable to write in JSON file"
        return

    print "JSON file successfully written"


def store_dataset_db(filename = None, dataset = None, name = None,
    excerpt_cut = 0):
    """
    """

    if filename is None and dataset is None:
        print "No valid source for the dataset provided"

    if dataset is None:
        with open(filename) as f:
            dataset = json.load(f)

    if 'name' not in dataset.keys():
        if name is None:
            name = ""
    else:
        name = dataset['name']

    tracks = []

    for artist in dataset['artists']:
        for album in artist['albums']:
            for track in album['tracks']:
                tracks.append({'track_id': track['id'], 'album_id': album['id']})

    i.add_dataset(name = name, tracks = tracks, excerpt_cut = excerpt_cut)


if __name__=="__main__":

    in_f = "ds_postrock_ambient_cand.json"
    out_f = "ds_postrock_ambient.json"

    num_artists = 3
    num_albums = 5
    num_tracks = 5

    #dataset = gen_dataset(in_f, num_artists, num_albums, num_tracks)

    #dataset['name'] = "Atmospheric Ambient Small"

    #store_dataset_json(out_f, dataset)

    store_dataset_db(filename = out_f)
