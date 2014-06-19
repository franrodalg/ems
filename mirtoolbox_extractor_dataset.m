function mirtoolbox_extractor_dataset(dataset_id)


    ipython_folder = '/usr/local/bin/';
    
    k = strfind(getenv('PATH'), ipython_folder);
    
    if(isempty(k))
        new_env_path = strcat(getenv('PATH'), ':', ipython_folder);
        setenv('PATH', new_env_path)

    end

    pyerror = 'Python Script Error';

    command = ['ipython ems_write_paths_csv.py ' num2str(dataset_id)];

    status = system(command);
    
    if(status == 0)
        
        paths = read_csv('temp_paths.csv', ';');

        
        json_string = sprintf('[\n');
        
        end_loop = length(paths);
        %end_loop = 1;

        for i = 1:end_loop
        
            mtb_features = struct();
            
            mtb_features.id = paths(i,1);
            disp(mtb_features.id)
            
            audio = miraudio(paths(i,2));
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

        %Tonal

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
            
            
            json_string = ...
                sprintf('%s%s\n', ...
                    json_string, get_json_string(mtb_features));
            if(i < end_loop)
                json_string = ...
                    sprintf('%s,\n', json_string);
            end
        
        end
        
        json_string = sprintf('%s]', json_string);
        
        fid = fopen('mtb_temp.json','wt');
        fprintf(fid, '%s', json_string);
        fclose(fid);
    
    else
        
        disp(pyerror) 
        
    end


end