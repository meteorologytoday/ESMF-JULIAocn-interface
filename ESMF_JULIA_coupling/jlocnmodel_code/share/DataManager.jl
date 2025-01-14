module DataManager

    using NCDatasets
    using Dates

    export DataUnit, DataTable, regVariable!
    export Recorder, setNewNCFile!, record!, avgAndOutput! 

    const missing_value = 1e20

    include("DataUnit.jl")
    include("DataTable.jl")
    include("Recorder.jl")

end
