using NCDatasets
using DataStructures
using TOML

project_root_dir = @__DIR__
data_dir = joinpath(project_root_dir, "data")
domain_dir = joinpath(project_root_dir, "CESM_domains")
casename = "Sandbox"
Nz = 20

z_w = - collect(Float64, range(0, stop=100, length=Nz+1)) 



config = OrderedDict{Any, Any}(

    "BASIC" => Dict(
        "calendar"      => "Gregorian",
        "restart"       => false,
        "iter_start"    => 0,
        "iter_advance"  => 10,
        "modeltime_ref" => 0.0, 
        "dt_num"        => 1,
        "dt_den"        => 1,
    ),


    "DRIVER" => Dict(
        "casename"           => casename,
        "caseroot"           => joinpath(project_root_dir, casename, "caseroot"),
        "caserun"            => joinpath(project_root_dir, casename, "caserun"),
        "archive_root"       => joinpath(project_root_dir, casename, "archive"),
        "compute_QFLX_direct_method" => true,
    ),



    "MODEL_MISC" => Dict(
        "timetype"               => "DateTimeNoLeap",
        "init_file"              => joinpath(data_dir, "init_ocn.jld2"),
        "rpointer_file"          => "rpointer.iom",
        "daily_record"           => [],
        "monthly_record"         => ["{ESSENTIAL}", "QFLXT", "QFLXS"],
        "enable_archive"         => true,
    ),

    "MODEL_CORE" => Dict(


        "topo_file"                    => joinpath(data_dir, "Nz_bot.nc"),
        "cdata_file"                   => joinpath(data_dir, "forcing.g37.nc"),

        "cdata_beg_time"               => "0001-01-01 00:00:00",
        "cdata_end_time"               => "0002-01-01 00:00:00",
        "cdata_align_time"             => "0001-01-01 00:00:00",

        "z_w"                          => z_w,

        "substeps"                     => 8,
        "MLD_scheme"                   => "static",
        "Qflx"                         => "on",
        "Qflx_finding"                 => "off",
        "convective_adjustment"        => "on",
        "advection_scheme"             => "ekman_AGA2020",

        "UVSFC_scheme"                 => "static",

        "weak_restoring"               => "off",
        "τwk_TEMP"                     => 86400.0 * 365 * 1000,
        "τwk_SALT"                     => 86400.0 * 365 * 1000,


        "τ_frz"                        => 3600.0,
        "Ekman_layers"                 => 5,
        "Returnflow_layers"            => 28,
    
        "transform_vector_field"       => true,
    ),

    "DOMAIN" => Dict(
        
        "domain_file"                  => joinpath(domain_dir, "domain.ocn.gx3v7.120323.nc"),

        "Nx" => 60,
        "Ny" => 10,
        "Nz" => 5,

        "sNx" => 20,
        "sNy" => 10,

        "nSx" => 1, # number of tiles per processor
        "nSy" => 1,

        "nPx" => 3, # number of processors. 
        "nPy" => 1,

        "OLx" => 3,
        "OLy" => 3,


        "coord" => "latlon",

        "dx" => 1.0,
        "dy" => 1.0,

        "beg_x" => 150.0,
        "beg_y" => 20.0,

    ),
)

open("config_EkmanMixedlayerOceanModel.toml", "w") do io
    TOML.print(io, config)
end
