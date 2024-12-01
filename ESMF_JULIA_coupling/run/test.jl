using MPI

MPI.Init()


include("OceanModel/JlOceanModel.jl")


model = OceanModel.createOceanModel("model_config.toml"; comm=MPI.COMM_WORLD)


params = zeros(Int64, 13)
OceanModel.getDomainInfo(model, params)
println(params)
