from essentia.standard import *
import ems_db_interface as i
import pyechonest.track as en_tr
import pyechonest.config as en_cfg
import ConfigParser
import subprocess as sub
import json, yaml, os
from pymatbridge import Matlab
import rpy2.robjects as ro
import numpy as np

###

fs_extractor = os.getcwd() + "/Extractors/streaming_extractor_freesound"

config = ConfigParser.ConfigParser()

config.readfp(open('ems_db.cfg'))
main_path = config.get('Files', 'main_path')


en_cfg.ECHO_NEST_API_KEY = config.get('Echonest', 'key')

mlab = Matlab(matlab='/Applications/MATLAB_R2011a.app/bin/matlab')

# Aux

def _filter_descriptors_(descriptors):

    aux = []

    for d in descriptors:
        if cmp(d,'ess_onset_times') != 0 and \
                cmp(d, 'ess_bpm_intervals') != 0 and \
                cmp(d, 'ess_beats_position') != 0 and \
                cmp(d, 'ess_bpm_estimates') != 0:
            aux.append(d)

    return aux

def _get_descriptors(excerpt_analysis, extractors):
    """
    """
    descriptors = []

    for extractor in extractors:
        if extractor in excerpt_analysis.keys():
            if 'desc' in excerpt_analysis[extractor].keys():
                for descriptor in \
                        sorted(excerpt_analysis[extractor]['desc'].iterkeys()):
                    descriptors.append(descriptor)

    return descriptors


# Formaters

def freesound_formater(analysis):
    """
    """

    res = {}

    meta = {}
    descs = {}

    for cat in analysis.keys():

        if cmp(cat, 'metadata') == 0:
            meta['ess_equal_loudness'] = \
                analysis[cat]['audio_properties']['equal_loudness']
            meta['ess_essentia_version'] = \
                analysis[cat]['version']['essentia']
            res['meta'] = meta
        else:
            for desc in analysis[cat].keys():

                if type(analysis[cat][desc]) is float or \
                        type(analysis[cat][desc]) is list or \
                        type(analysis[cat][desc]) is int:

                    desc_name = 'ess_' + desc
                    descs[desc_name] = analysis[cat][desc]

                elif type(analysis[cat][desc]) is dict:
                    desc_name = 'ess_' + desc + "_"

                    if 'mean' in analysis[cat][desc].keys():
                        if type(analysis[cat][desc]['mean']) is list:
                            for i in range(len(analysis[cat][desc]['mean'])):
                                if (i + 1) < 10:
                                    i_str = '0' + str(i+1)
                                else:
                                    i_str = str(i + 1)

                                desc_name_i = desc_name + i_str + '_mean'
                                descs[desc_name_i] = analysis[cat][desc]['mean'][i]
                        else:
                            descs[desc_name + 'mean'] = analysis[cat][desc]['mean']

                    if 'var' in analysis[cat][desc].keys():
                        if type(analysis[cat][desc]['var']) is list:
                            for i in range(len(analysis[cat][desc]['var'])):
                                if (i + 1) < 10:
                                    i_str = '0' + str(i+1)
                                else:
                                    i_str = str(i + 1)

                                desc_name_i = desc_name + i_str + '_var'
                                descs[desc_name_i] = analysis[cat][desc]['var'][i]
                        else:
                            descs[desc_name + 'var'] = analysis[cat][desc]['var']

    res['desc'] = descs

    return res

def echonest_formater(track):
    """
    """

    analysis = {}

    meta = {}

    if 'id' in dir(track): 
        meta['en_id'] = track.id

    if 'song_id' in dir(track):  
        meta['en_song_id'] = track.song_id

    if 'artist_id' in dir(track):
        meta['en_artist_id'] = track.artist_id

    if 'meta' in dir(track):
        if 'title' in track.meta.keys():
            meta['en_title'] = track.meta['title']
        if 'artist' in track.meta.keys():
            meta['en_artist'] = track.meta['artist']
        if 'album' in track.meta.keys():
            meta['en_album'] = track.meta['album']
        if 'seconds' in track.meta.keys():
            meta['en_duration_seconds'] = track.meta['seconds']
        if 'analyzer_version' in track.meta.keys():
            meta['en_analyzer_version'] = track.meta['analyzer_version']
        if 'sample_rate' in track.meta.keys():
            meta['en_sample_rate'] = track.meta['sample_rate']
        if 'timestamp' in track.meta.keys():
            meta['en_timestamp'] = track.meta['timestamp']
        if 'genre' in track.meta.keys():
            meta['en_genre'] = track.meta['genre']
        if 'bitrate' in track.meta.keys():
            meta['en_bitrate'] = track.meta['bitrate']
        if 'analysis_time' in track.meta.keys():
            meta['en_analysis_time'] = track.meta['analysis_time']
    else:
        if 'title' in dir(track):
            meta['en_title'] = track.title
        if 'artist' in dir(artist):
            meta['en_artist'] = track.artist
        if 'bitrate' in dir(track):
            meta['en_bitrate'] = track.bitrate
        if 'analyzer_version' in dir(track):
            meta['en_analyzer_version'] = track.analyzer_version

    if 'audio_md5' in dir(track):
        meta['en_audio_md5'] = track.audio_md5
    if 'sample_md5' in dir(track):
        meta['en_sample_md5'] = track.sample_md5
    if 'md5' in dir(track):
        meta['en_md5'] = track.md5
    if 'decoder' in dir(track):
        meta['en_decoder'] = track.decoder
    if 'decoder_version' in dir(track):
        meta['en_decoder_version'] = track.decoder_version
    if 'analysis_channels' in dir(track):
        meta['en_analysis_channels'] = track.analysis_channels
    if 'analysis_sample_rate' in dir(track):
        meta['en_analysis_sample_rate'] = track.analysis_sample_rate
    if 'num_samples' in dir(track):
        meta['en_num_samples'] = track.num_samples
    if 'samplerate' in dir(track):
        meta['en_sample_rate'] = track.samplerate
    if 'window_seconds' in dir(track):
        meta['en_window_seconds'] = track.window_seconds
    if 'duration' in dir(track):
        meta['en_duration'] = track.duration

    analysis['meta'] = meta

    desc = {}

    if 'energy' in dir(track):
        desc['en_energy'] = track.energy
    if 'loudness' in dir(track):
        desc['en_loudness'] = track.loudness
    if 'end_of_fade_in' in dir(track):
        desc['en_end_of_fade_in'] = track.end_of_fade_in
    if 'start_of_fade_out' in dir(track):
        desc['en_start_of_fade_out'] = track.start_of_fade_out

    if 'acousticness' in dir(track):
        desc['en_acousticness'] = track.acousticness
    if 'danceability' in dir(track):
        desc['en_danceability'] = track.danceability
    if 'liveness' in dir(track):
        desc['en_liveness'] = track.liveness
    if 'speechiness' in dir(track):
        desc['en_speechiness'] = track.speechiness
    if 'valence' in dir(track):
        desc['en_valence'] = track.valence

    if 'key' in dir(track):
        desc['en_key'] = track.key
        if 'key_confidence' in dir(track):
            desc['en_key_confidence'] = track.key_confidence
    if 'mode' in dir(track):
        desc['en_mode'] = track.mode
        if 'mode_confidence' in dir(track):
            desc['en_mode_confidence'] = track.mode_confidence

    if 'time_signature' in dir(track):
        desc['en_time_signature'] = track.time_signature
        if 'time_signature_confidence' in dir(track):
            desc['en_time_signature_confidence'] = \
                track.time_signature_confidence
    if 'tempo' in dir(track):
        desc['en_tempo'] = track.tempo
        if 'tempo_confidence' in dir(track):
            desc['en_tempo_confidence'] = track.tempo_confidence

    analysis['desc'] = desc

    rhythm = {}

    bars = []

    if 'bars' in dir(track):
        for bar in track.bars:
            b = {}
            if 'start' in bar.keys():
                b['en_start'] = bar['start']
            if 'duration' in bar.keys():
                b['en_duration'] = bar['duration']
            if 'confidence' in bar.keys():
                b['en_confidence'] = bar['confidence']
            bars.append(b)

    rhythm['bars'] = bars

    beats = []

    if 'beats' in dir(track):
        for beat in track.beats:
            b = {}
            if 'start' in beat.keys():
                b['en_start'] = beat['start']
            if 'duration' in beat.keys():
                b['en_duration'] = beat['duration']
            if 'confidence' in beat.keys():
                b['en_confidence'] = beat['confidence']
            beats.append(b)

    rhythm['beats'] = beats

    tatums = []

    if 'tatums' in dir(track):
        for tatum in track.tatums:
            t = {}
            if 'start' in tatum.keys():
                t['en_start'] = tatum['start']
            if 'duration' in tatum.keys():
                t['en_duration'] = tatum['duration']
            if 'confidence' in tatum.keys():
                t['en_confidence'] = tatum['confidence']
            tatums.append(t)

    rhythm['tatums'] = tatums   

    analysis['rhythm'] = rhythm

    structure = {}

    sections = []

    if 'sections' in dir(track):
        for section in track.sections:
            s = {}
            if 'start' in section.keys():
                s['en_start'] = section['start']
            if 'duration' in section.keys():
                s['en_duration'] = section['duration']
            if 'confidence' in section.keys():
                s['en_confidence'] = section['confidence']
            if 'loudness' in section.keys():
                s['en_loudness'] = section['loudness']
            if 'key' in section.keys():
                s['en_key'] = section['key']
                if 'key_confidence' in section.keys():
                    s['en_key_confidence'] = \
                        section['key_confidence']
            if 'mode' in section.keys():
                s['en_mode'] = section['mode']
                if 'mode_confidence' in section.keys():
                    s['en_mode_confidence'] = \
                        section['mode_confidence']
            if 'time_signature' in section.keys():
                s['en_time_signature'] = section['time_signature']
                if 'time_signature_confidence' in section.keys():
                    s['en_time_signature_confidence'] = \
                        section['time_signature_confidence']
            if 'tempo' in section.keys():
                s['en_tempo'] = section['tempo']
                if 'tempo_confidence' in section.keys():
                    s['en_tempo_confidence'] = \
                        section['tempo_confidence']
            sections.append(s)

    structure['sections'] = sections

    segments = []

    if 'segments' in dir(track):
        for segment in track.segments:
            s = {}
            if 'start' in segment.keys():
                s['en_start'] = segment['start']
            if 'duration' in segment.keys():
                s['en_duration'] = segment['duration']
            if 'confidence' in segment.keys():
                s['en_confidence'] = segment['confidence']
            if 'loudness_start' in segment.keys():
                s['en_loudness_start'] = segment['loudness_start']
            if 'loudness_max' in segment.keys():
                s['en_loudness_max'] = segment['loudness_max']
            if 'loudness_max_time' in segment.keys():
                s['en_loudness_max_time'] = segment['loudness_max_time']
            if 'timbre' in segment.keys():
                for i in range(len(segment['timbre'])):
                    if (i + 1) < 10:
                        i_str = '0' + str(i + 1)
                    else:
                        i_str = str(i + 1)
                    s['en_timbre_' + i_str] = segment['timbre'][i]
            if 'pitches' in segment.keys():
                for i in range(len(segment['pitches'])):
                    if (i + 1) < 10:
                        i_str = '0' + str(i + 1)
                    else:
                        i_str = str(i + 1)
                    s['en_pitch_' + i_str] = segment['pitches'][i]
            
            segments.append(s)

    structure['segments'] = segments
    analysis['structure'] = structure
    
    strings = {}

    if 'codestring' in dir(track):
        strings['en_code'] = track.codestring
        if 'code_version' in dir(track):
            strings['en_code_version'] = track.code_version
    if 'echoprintstring' in dir(track):
        strings['en_echoprint'] = track.echoprintstring
        if 'echoprint_version' in dir(track):
            strings['en_echoprint_version'] = track.echoprint_version
    if 'rhythmstring' in dir(track):
        strings['en_rhythm'] = track.rhythmstring
        if 'rhythm_version' in dir(track):
            strings['en_rhythm_version'] = track.rhythm_version
    if 'synchstring' in dir(track):
        strings['en_synch'] = track.synchstring
        if 'synch_version' in dir(track):
            strings['en_synch_version'] = track.synch_version

    analysis['strings'] = strings

    return analysis


# Extractors

def freesound(file_path):
    """
    """

    analysis = {}

    print 'Computing Essentia descriptors with the Freesound extractor...'

    out = 'temp'
    out_f = out + '.yaml'

    try:
        print "Analysing: " + file_path
        sub.check_output([fs_extractor, file_path, out])
        stream = open(out_f, 'r')
        analysis =  yaml.load(stream)

        os.remove(out_f)
        analysis = freesound_formater(analysis)

    except sub.CalledProcessError:
        print "Error computing Essentia descriptors"

    print 'Done'
    return analysis

def echonest(file_path):
    """
    """

    analysis = {}

    print 'Retrieving features from the EchoNest analyzer...'

    try: 
        track = en_tr.track_from_filename(filename = file_path, force_upload = True)
        track.get_analysis()
        analysis = echonest_formater(track)
    except:
        print "Error retrieving EchoNest features"
        return {}

    print 'Done'
    return analysis

def mirtoolbox(file_path):
    """
    """

    analysis = {}

    print 'Computing descriptors from MIRToolbox...'

    mlab.start()

    res = mlab.run_func('mirtoolbox_extractor.m', {'file_path': file_path})
    print res

    mlab.stop()

    with open('mtb_temp.json', 'r') as f:
        analysis = json.load(f)

    print 'Done'
    return analysis

def essentia(file_path):
    """
    """

    print 'Computing Essentia descriptors not included in the Freesound extractor...'

    audio = MonoLoader(filename = file_path)()

    pitch, pitchSalience = PredominantMelody()(audio)

    analysis = {}

    analysis['melodia'] = {}

    analysis['melodia']['mel_pitch'] = pitch
    analysis['melodia']['mel_pitch_salience'] = pitchSalience


    grad = np.gradient(np.array(pitch, dtype = float))

    count = 0
    for g in grad:
        if g != 0: count = count + 1

    analysis['desc'] = {}

    analysis['desc']['ess_pitch_changes'] = count
    analysis['desc']['ess_pitch_changes_rate'] = float(count)/len(grad)

    analysis['desc']['ess_danceability'] = Danceability()(audio)

    analysis['desc']['ess_dynamic_complexity'] = DynamicComplexity()(audio)[0]

    print 'Done'

    return analysis

def _der_desc(excerpt_id, extractor):
    """
    """

    v = ro.r('source("ems_der_descriptors.R");')

    r_call = 'get_der_descriptors({}, \'{}\')'\
            .format(excerpt_id, extractor)

    v = ro.r(r_call)

    with open('desc_temp.json') as f:
        desc = json.load(f)

    os.remove('desc_temp.json')

    return desc


def rhythm_pos(excerpt_id):
    """
    """

    return _der_desc(excerpt_id, 'rhythm_pos')

def en_structure(excerpt_id):
    """
    """

    return _der_desc(excerpt_id, 'en_structure')

def rhythm_structure(excerpt_id):
    """
    """

    return _der_desc(excerpt_id, 'rhythm_structure')

#

def analyze_file(file_path, extractors):
    """
    """

    analysis = {}

    print 'Analyzing file {}'.format(file_path)

    if 'freesound' in extractors:
        analysis['freesound'] = freesound(file_path)
    if 'echonest' in extractors:
        analysis['echonest'] = echonest(file_path)
    if 'mirtoolbox' in extractors:
        analysis['mirtoolbox'] = mirtoolbox(file_path)
    if 'essentia' in extractors:
        analysis['essentia'] = essentia(file_path)

    return analysis

def perform_analysis(excerpt_ids, extractors):
    """
    """

    paths = i.get_paths(excerpt_ids = excerpt_ids)

    analysis = []

    count = 0

    for excerpt in paths:
        
        print '[{}/{}]'.format(count + 1, len(excerpt_ids))

        file_path = main_path + excerpt['path']

        excerpt_analysis = analyze_file(file_path, extractors)
        excerpt_analysis['id'] = excerpt['id']

        analysis.append(excerpt_analysis)

        count = count + 1

    return analysis


def perform_store_analysis(excerpt_ids, extractors):
    """
    """

    print excerpt_ids

    paths = i.get_paths(excerpt_ids = excerpt_ids)

    count = 0

    for excerpt in paths:
        
        print '[{}/{}]'.format(count + 1, len(excerpt_ids))

        file_path = main_path + excerpt['path']

        excerpt_analysis = analyze_file(file_path, extractors)
        excerpt_analysis['id'] = excerpt['id']

        store_excerpt_analysis(excerpt_analysis, extractors)

        count = count + 1    



def compute_der_descriptors_excerpt(excerpt_id, extractors):
    """
    """

    analysis = {}

    print 'Computing derived descriptors for excerpt {}'.format(excerpt_id)

    if 'rhythm_pos' in extractors:
        analysis['rhythm_pos'] = rhythm_pos(excerpt_id)
    if 'en_structure' in extractors:
        analysis['en_structure'] = en_structure(excerpt_id)
    if 'rhythm_structure' in extractors:
        analysis['rhythm_structure'] = rhythm_structure(excerpt_id)

    return analysis

def compute_der_descriptors(excerpt_ids, extractors):
    """
    """

    analysis = []

    count = 0

    for excerpt in excerpt_ids:

        print '\n[{}/{}]\n'.format(count + 1, len(excerpt_ids))

        excerpt_analysis = \
            compute_der_descriptors_excerpt(excerpt, extractors)
        excerpt_analysis['id'] = excerpt

        analysis.append(excerpt_analysis)

        count = count + 1

    return analysis

def compute_store_der_descriptors(excerpt_ids, extractors):

    count = 0

    for excerpt in excerpt_ids:

        print '\n[{}/{}]\n'.format(count + 1, len(excerpt_ids))

        excerpt_analysis = \
            compute_der_descriptors_excerpt(excerpt, extractors)
        excerpt_analysis['id'] = excerpt

        store_excerpt_analysis(excerpt_analysis, extractors)

        count = count + 1

def store_analysis(analysis, extractors, descriptors = None):
    """
    """

    count = 0

    for excerpt_analysis in analysis:
        print '\n[{}/{}]\n'.format(count + 1, len(analysis))
        store_excerpt_analysis(excerpt_analysis, extractors, descriptors)
        count = count + 1

def store_excerpt_analysis(analysis, extractors, descriptors = None):
    """
    """

    print 'Storing analysis for excerpt {}...'.format(analysis['id'])

    if descriptors is None:
        descriptors = _get_descriptors(analysis, extractors)

    if(analysis['id'] < 10):
        str_id = '0' + str(analysis['id'])
    else:
        str_id = str(analysis['id'])


    for extractor in extractors:

        print 'Extractor: ' + extractor

        if extractor in analysis.keys():
                               
            features = {}

            if 'desc' in analysis[extractor].keys():

                print 'Storing Descriptors'

                for k in _filter_descriptors_(descriptors):
                    if k in analysis[extractor]['desc'].keys():
                        features[k] = analysis[extractor]['desc'][k]
                i.add_descriptors(analysis['id'], features)

            if cmp(extractor, 'freesound') == 0:
                if 'desc' in analysis[extractor].keys():
                    if 'ess_onset_times' \
                            in analysis[extractor]['desc'].keys():
                        aux = []

                        for j in analysis[extractor]['desc']['ess_onset_times']:
                            aux.append({'ess_onset_time': + j})

                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_ess_onset_times',
                                aux
                                )
                    if 'ess_beats_position' \
                            in analysis[extractor]['desc'].keys():
                        aux = []

                        for j in analysis[extractor]['desc']['ess_beats_position']:
                            aux.append({'ess_beat_position': + j})

                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_ess_beats_position',
                                aux
                                )
                    if 'ess_bpm_estimates' \
                            in analysis[extractor]['desc'].keys():
                        aux = []

                        for j in analysis[extractor]['desc']['ess_bpm_estimates']:
                            aux.append({'ess_bpm_estimate': + j})

                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_ess_bpm_estimates',
                                aux
                                )
                    if 'ess_bpm_intervals' \
                            in analysis[extractor]['desc'].keys():
                        aux = []

                        for j in analysis[extractor]['desc']['ess_bpm_intervals']:
                            aux.append({'ess_bpm_interval': + j})

                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_ess_bpm_intervals',
                                aux
                                )

            elif cmp(extractor, 'echonest') == 0:
                if 'rhythm' in analysis[extractor].keys():
                    if 'bars' in analysis[extractor]['rhythm'].keys():
                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_en_bars',
                                analysis[extractor]['rhythm']['bars']
                                )
                    if 'beats' in analysis[extractor]['rhythm'].keys():
                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_en_beats',
                                analysis[extractor]['rhythm']['beats']
                                )
                    if 'tatums' in analysis[extractor]['rhythm'].keys():
                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_en_tatums',
                                analysis[extractor]['rhythm']['tatums']
                                )
                if 'structure' in analysis[extractor].keys():
                    if 'sections' in analysis[extractor]['structure'].keys():
                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_en_sections',
                                analysis[extractor]['structure']['sections']
                                )
                    if 'segments' in analysis[extractor]['structure'].keys():
                        i.add_excerpt_array_table(
                                analysis['id'], 
                                'excerpts_en_segments',
                                analysis[extractor]['structure']['segments']
                                )
                if 'strings' in analysis[extractor].keys():
                    with open('Features/strings_'+ str_id + '.json', 'w') as f:
                        json.dump(analysis[extractor]['strings'], f)


            elif cmp(extractor, 'mirtoolbox') == 0:
                if 'onsets' in analysis[extractor].keys():
                    aux = []

                    for j in analysis[extractor]['onsets']:
                        aux.append({'mtb_onset_time': + j})

                    i.add_excerpt_array_table(
                        analysis['id'], 
                        'excerpts_mtb_onset_times',
                        aux
                        )
            
            elif cmp(extractor, 'essentia') == 0:

                if 'melodia' in analysis[extractor].keys():
                    melodia_aux = {}
                    if 'mel_pitch' in analysis[extractor]['melodia']:
                        melodia_aux['mel_pitch'] = \
                            [np.asscalar(m) for m in analysis[extractor]['melodia']['mel_pitch']]
                    if 'mel_pitch_salience' in analysis[extractor]['melodia']:
                        melodia_aux['mel_pitch_salience'] = \
                            [np.asscalar(m) for m in analysis[extractor]['melodia']['mel_pitch_salience']]

                    with open('Features/melodia_'+ str_id + '.json', 'w') as f:
                        json.dump(melodia_aux, f)

            if 'meta' in analysis[extractor].keys():
                print 'Storing Metadata'
                i.add_meta(analysis['id'], analysis[extractor]['meta'])
