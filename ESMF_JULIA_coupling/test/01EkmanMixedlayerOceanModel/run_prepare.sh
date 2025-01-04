#!/bin/bash


script_dir=JLCODE/EkmanMixedlayerOceanModel/tools/generate_init_files
config_file=config_EkmanMixedlayerOceanModel.toml

#julia $script_dir/make_blank_config.jl > $config_file

julia $script_dir/make_z_w.jl \
    --output-file "z_w.nc" \
    --z_w 0 -10 -20 -30 -40

if [ ] ; then
julia $script_dir/set_dummy_config.jl \
    --config $config_file           \
    --project-root-dir $( pwd )     \
    --domain-file "CESM_domains/domain.ocn.gx3v7.120323.nc" \
    --Nz_bot-file "" \
    --z_w-file "z_w.nc"   
fi

#julia $script_dir/make_exp_config.jl \
#    --config $config_file 
    
    
