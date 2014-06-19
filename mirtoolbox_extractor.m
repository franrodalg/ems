function p = mirtoolbox_extractor(args)

    mtb_features = struct();
    
    audio = miraudio(args.file_path);
    spec = mirspectrum(audio);
    peaks = mirpeaks(spec);    

    % Dynamics

    mtb_features.mtb_rms = mirgetdata(mirrms(audio));
    mtb_features.mtb_rms_median = ...
        mirgetdata(mirrms(audio, 'Median'));

    mtb_features.mtb_low_energy = ...
        mirgetdata(mirlowenergy(audio));
    mtb_features.mtb_low_energy_asr = ...
        mirgetdata(mirlowenergy(audio, 'ASR'));
    
    % Rhythm

    onsets = mironsets(audio);
    mtb_features.mtb_onsets = mirgetdata(onsets);

    mtb_features.mtb_event_density = ...
        mirgetdata(mireventdensity(onsets));
    mtb_features.mtb_tempo = ...
        mirgetdata(mirtempo(onsets));
    mtb_features.mtb_pulse_clarity = ...
        mirgetdata(mirpulseclarity(onsets));

    % Timbre     

    mtb_features.mtb_zcr = ...
        mirgetdata(mirzerocross(audio));
    mtb_features.mtb_spec_rolloff = ...
        mirgetdata(mirrolloff(spec));
    mtb_features.mtb_spec_centroid = ...
        mirgetdata(mircentroid(spec));
    mtb_features.mtb_spec_spread = ...
        mirgetdata(mirspread(spec));
    mtb_features.mtb_spec_skewness = ...
        mirgetdata(mirskewness(spec));
    mtb_features.mtb_spec_kurtosis = ...
        mirgetdata(mirkurtosis(spec));
    mtb_features.mtb_spec_flatness = ...
        mirgetdata(mirflatness(spec));
    mtb_features.mtb_spec_entropy = ...
        mirgetdata(mirentropy(spec));

    mtb_features.mtb_mfcc = ...
         mirgetdata(mirmfcc(spec));
    
    roughness = mirroughness(audio);            
    roughness_stats = mirstat(roughness);
    mtb_features.mtb_roughness_mean = ...
        roughness_stats.Mean;
    mtb_features.mtb_roughness_median = ...
        mirgetdata(mirmedian(roughness));
    mtb_features.mtb_roughness_std = ...
        roughness_stats.Std;
    mtb_features.mtb_roughness_slope = ...
        roughness_stats.Slope;
    mtb_features.mtb_roughness_period_freq = ...
        roughness_stats.PeriodFreq;
    mtb_features.mtb_roughness_period_entropy = ...
        roughness_stats.PeriodEntropy;
    
    mtb_features.mtb_irregularity = ...
        mirgetdata(mirregularity(peaks));

    % Tonal

    mtb_features.mtb_inharmonicity = ...
        mirgetdata(mirinharmonicity(spec));
    
    chroma = mirchromagram(spec);
    mtb_features.mtb_chroma = ...
        mirgetdata(chroma);
    
    key_strength = mirkeystrength(chroma);
    mtb_features.mtb_key_strength = ...
        mirgetdata(key_strength);
    
    mtb_features.mtb_key = ...
        mirgetdata(mirkey(key_strength));

    mtb_features.mtb_mode = ...
        mirgetdata(mirmode(key_strength));
   
    mtb_features.mtb_tonal_centroid = ...
        mirgetdata(mirtonalcentroid(chroma));

    hcdf = mirhcdf(audio);            
    hcdf_stats = mirstat(hcdf);
    mtb_features.mtb_hcdf_mean = ...
        hcdf_stats.Mean;
    mtb_features.mtb_hcdf_median = ...
        mirgetdata(mirmedian(hcdf));
    mtb_features.mtb_hcdf_std = ...
        hcdf_stats.Std;
    mtb_features.mtb_hcdf_slope = ...
        hcdf_stats.Slope;
    mtb_features.mtb_hcdf_period_freq = ...
        hcdf_stats.PeriodFreq;
    mtb_features.mtb_hcdf_period_entropy = ...
        hcdf_stats.PeriodEntropy;            
    

    mirtoolbox_json(mtb_features)

    p = 0;

end