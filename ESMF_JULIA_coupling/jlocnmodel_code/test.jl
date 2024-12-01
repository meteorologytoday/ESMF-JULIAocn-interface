using MPI

MPI.Init()


include("OceanModel/JlOceanModel.jl")


model = OceanModel.createOceanModel("model_config.toml"; comm=MPI.COMM_WORLD)


println(OceanModel.getDomainInfo(model))
