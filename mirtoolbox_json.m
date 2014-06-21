function p = mirtoolbox_json(mtb_features)

    json_string = sprintf('{\n');
    json_string = sprintf('%s\t"desc":{\n', json_string);

    if isnan(mtb_features.mtb_rms)
        json_string = ...
            sprintf('%s\t\t\t"mtb_rms": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_rms": %f,\n', ...
                json_string, mtb_features.mtb_rms);
    end

    if isnan(mtb_features.mtb_rms_median)
        json_string = ...
            sprintf('%s\t\t\t"mtb_rms_median": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_rms_median": %f,\n', ...
                json_string, mtb_features.mtb_rms_median);
    end

    if isnan(mtb_features.mtb_low_energy)
        json_string = ...
            sprintf('%s\t\t\t"mtb_low_energy": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_low_energy": %f,\n', ...
                json_string, mtb_features.mtb_low_energy);
    end

    if isnan(mtb_features.mtb_low_energy_asr)
        json_string = ...
            sprintf('%s\t\t\t"mtb_low_energy_asr": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_low_energy_asr": %f,\n', ...
                json_string, mtb_features.mtb_low_energy_asr);
    end

    if isnan(mtb_features.mtb_event_density)
        json_string = ...
            sprintf('%s\t\t\t"mtb_event_density": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_event_density": %f,\n', ...
                json_string, mtb_features.mtb_event_density);
    end

    if isnan(mtb_features.mtb_tempo)
        json_string = ...
            sprintf('%s\t\t\t"mtb_tempo": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_tempo": %f,\n', ...
                json_string, mtb_features.mtb_tempo);
    end

    if isnan(mtb_features.mtb_spec_rolloff)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_rolloff": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_rolloff": %f,\n', ...
                json_string, mtb_features.mtb_spec_rolloff);
    end

    if isnan(mtb_features.mtb_spec_centroid)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_centroid": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_centroid": %f,\n', ...
                json_string, mtb_features.mtb_spec_centroid);
    end

    if isnan(mtb_features.mtb_spec_spread)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_spread": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_spread": %f,\n', ...
                json_string, mtb_features.mtb_spec_spread);
    end

    if isnan(mtb_features.mtb_spec_skewness)
        json_string = ...
        sprintf('%s\t\t\t"mtb_spec_skewness": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_skewness": %f,\n', ...
                json_string, mtb_features.mtb_spec_skewness);
    end

    if isnan(mtb_features.mtb_spec_kurtosis)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_kurtosis": null,\n', ...
                json_string);
        else
            json_string = ...
                sprintf('%s\t\t\t"mtb_spec_kurtosis": %f,\n', ...
                    json_string, mtb_features.mtb_spec_kurtosis);
    end

    if isnan(mtb_features.mtb_spec_flatness)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_flatness": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_flatness": %f,\n', ...
                json_string, mtb_features.mtb_spec_flatness);
    end

    if isnan(mtb_features.mtb_spec_entropy)
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_entropy": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_spec_entropy": %f,\n', ...
                json_string, mtb_features.mtb_spec_entropy);
    end

    if isnan(mtb_features.mtb_mfcc)

        for i = 1:13

            if i < 10
                i_str = strcat('0', num2str(i));
            else
                i_str = num2str(i);
            end

            desc = strcat('mtb_mfcc_', i_str);

            json_string = ...
                sprintf('%s\t\t\t"%s": null,\n', ...
                    json_string, desc);
            end
    else


        for i = 1:length(mtb_features.mtb_mfcc)

            if i < 10
                i_str = strcat('0', num2str(i));
            else
                i_str = num2str(i);
            end
       
            desc = strcat('mtb_mfcc_', i_str);

            if isnan(mtb_features.mtb_mfcc)

                json_string = ...
                    sprintf('%s\t\t\t"%s": null,\n', ...
                        json_string, desc);

            else

                json_string = ...
                    sprintf('%s\t\t\t"%s": %s,\n', ...
                        json_string, desc, mtb_features.mtb_mfcc(i));

            end

        end

    end

    if isnan(mtb_features.mtb_roughness_mean)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_mean": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_mean": %f,\n', ...
                json_string, mtb_features.mtb_roughness_mean);
    end

    if isnan(mtb_features.mtb_roughness_median)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_median": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_median": %f,\n', ...
                json_string, mtb_features.mtb_roughness_median);
    end

    if isnan(mtb_features.mtb_roughness_std)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_std": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_std": %f,\n', ...
                json_string, mtb_features.mtb_roughness_std);
    end

    if isnan(mtb_features.mtb_roughness_slope)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_slope": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_slope": %f,\n', ...
                json_string, mtb_features.mtb_roughness_slope);
    end

    if isnan(mtb_features.mtb_roughness_period_freq)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_period_freq": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_period_freq": %f,\n', ...
                json_string, mtb_features.mtb_roughness_period_freq);
    end

    if isnan(mtb_features.mtb_roughness_period_entropy)
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_period_entropy": null,\n', ...
                json_string);
    else
        json_string = ...
            sprintf('%s\t\t\t"mtb_roughness_period_entropy": %f,\n', ...
                json_string, mtb_features.mtb_roughness_period_entropy);
    end
       
    json_string = ...
        sprintf('%s\t\t\t"mtb_irregularity": %s,\n', ...
            json_string, mtb_features.mtb_irregularity);

    json_string = ...
        sprintf('%s\t\t\t"mtb_inharmonicity": %s,\n', ...
            json_string, mtb_features.mtb_inharmonicity);
    
    for i = 1:length(mtb_features.mtb_chroma)
       
       if i < 10
           i_str = strcat('0', num2str(i));
       else
           i_str = num2str(i);
       end
       
       desc = strcat('mtb_chroma_', i_str);
 
       json_string = ...
       sprintf('%s\t\t\t"%s": %f,\n', ...
           json_string, desc, mtb_features.mtb_chroma(i));
        
    end   
        
        
    for i = 1:length(mtb_features.mtb_key_strength)
       
       if i < 10
           i_str = strcat('0', num2str(i));
       else
           i_str = num2str(i);
       end
       
       desc = strcat('mtb_key_strength_', i_str);
 
       json_string = ...
       sprintf('%s\t\t\t"%s": %f,\n', ...
           json_string, desc, mtb_features.mtb_key_strength(i));
        
    end 
       
    json_string = ...
        sprintf('%s\t\t\t"mtb_key": %f,\n', ...
           json_string, mtb_features.mtb_key);       

    json_string = ...
        sprintf('%s\t\t\t"mtb_mode": %f,\n', ...
            json_string, mtb_features.mtb_mode);
       
         
    for i = 1:length(mtb_features.mtb_tonal_centroid)
       
       if i < 10
           i_str = strcat('0', num2str(i));
       else
           i_str = num2str(i);
       end
       
       desc = strcat('mtb_tonal_centroid_', i_str);
 
       json_string = ...
       sprintf('%s\t\t\t"%s": %f,\n', ...
           json_string, desc, mtb_features.mtb_tonal_centroid(i));
        
    end 
    
    
    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_mean": %f,\n', ...
            json_string, mtb_features.mtb_hcdf_mean);
    
    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_median": %f,\n', ...
            json_string, mtb_features.mtb_hcdf_median);
       
    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_std": %f,\n', ...
           json_string, mtb_features.mtb_hcdf_std);       

    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_slope": %f,\n', ...
            json_string, mtb_features.mtb_hcdf_slope);
       
    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_period_freq": %f,\n', ...
           json_string, mtb_features.mtb_hcdf_period_freq);       

    json_string = ...
        sprintf('%s\t\t\t"mtb_hcdf_period_entropy": %f\n', ...
            json_string, mtb_features.mtb_hcdf_period_entropy);
       
       
    
    json_string = sprintf('%s\t\t},\n', json_string);
    
    json_string = sprintf('%s\t"onsets":[\n', json_string);
    
    onsets = mtb_features.mtb_onsets;
    
    end_loop = length(onsets);
    
    for i = 1:end_loop
       if(i < end_loop)
           json_string = sprintf('%s\t\t%f,\n', json_string, onsets(i));
       else
           json_string = sprintf('%s\t\t%f\n', json_string, onsets(i));
       end
    end
    
    json_string = sprintf('%s\t\t]', json_string);
      
    json_string = sprintf('%s\n}', json_string);
    
    fid = fopen('mtb_temp.json','wt');
    fprintf(fid, '%s', json_string);
    fclose(fid);


    p = 0;
end