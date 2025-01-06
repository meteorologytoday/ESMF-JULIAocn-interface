include("../../share/Domains.jl")
include("../../share/DataManager.jl")


module Parallelization

    using ..Domains  
    using ..DataManager
    using MPI
    using Formatting

    export JobDistributionInfo, getYsplitInfoByRank, syncField!
    export printJobDistributionInfo


    mutable struct YSplitInfo
        pull_fr_rng      :: UnitRange
        pull_to_rng      :: UnitRange
        push_fr_rng      :: UnitRange
        push_to_rng      :: UnitRange
        
        pull_fr_rng_bnd  :: AbstractArray{Union{UnitRange, Nothing}}
        pull_to_rng_bnd  :: AbstractArray{Union{UnitRange, Nothing}}
        push_fr_rng_bnd  :: AbstractArray{Union{UnitRange, Nothing}}
        push_to_rng_bnd  :: AbstractArray{Union{UnitRange, Nothing}}

        pull_fr_rng_cpl :: UnitRange
        pull_to_rng_cpl :: UnitRange
        push_fr_rng_cpl :: UnitRange
        push_to_rng_cpl :: UnitRange

    end

    mutable struct JobDistributionInfo

        overlap       :: Int64
        comm_size     :: Int64
        remote_ranks  :: Array{Int64, 1}
        rank_to_idx   :: Dict{Int64, Int64}

        # This is between master and slaves
        y_split_infos :: AbstractArray{YSplitInfo, 1}
        
        comm :: MPI.Comm
        Nx :: Int64
        Ny :: Int64
        Ny_inner :: Int64  # inneer block
        master_rank :: Int64

        function JobDistributionInfo(;
            Nx       :: Int64,
            Ny       :: Int64,
            comm     :: MPI.Comm,
            overlap  :: Int64 = 3,
            master_rank :: Int64 = 0,
        )
            comm_size = MPI.Comm_size(comm)

            if ! ( 0 <= master_rank <= (comm_size-1))
                throw(ErrorException("master_rank out of range"))
            end

            remote_ranks = []
            for rank in 0:(comm_size-1)
                if rank == master_rank
                    continue
                end
                push!(remote_ranks, rank)
            end

            y_split_infos = Array{YSplitInfo}(undef, comm_size)

            Ny_inner, (
                pull_fr_rngs,
                pull_to_rngs,
                push_fr_rngs,
                push_to_rngs,
                pull_fr_rngs_bnd,
                pull_to_rngs_bnd,
                push_fr_rngs_bnd,
                push_to_rngs_bnd, 
                pull_fr_rngs_cpl,
                pull_to_rngs_cpl,
                push_fr_rngs_cpl,
                push_to_rngs_cpl,
            ) = calParallizationRange(N=Ny, P=comm_size, L=overlap)
      
            rank_to_idx = Dict() 
            for i = 0:(comm_size-1)

                idx = i+1
                rank_to_idx[i] = idx

                y_split_infos[idx] = YSplitInfo(
                    pull_fr_rngs[idx],
                    pull_to_rngs[idx],
                    push_fr_rngs[idx],
                    push_to_rngs[idx],
                    pull_fr_rngs_bnd[idx, :],
                    pull_to_rngs_bnd[idx, :],
                    push_fr_rngs_bnd[idx, :],
                    push_to_rngs_bnd[idx, :],
                    pull_fr_rngs_cpl[idx],
                    pull_to_rngs_cpl[idx],
                    push_fr_rngs_cpl[idx],
                    push_to_rngs_cpl[idx],
                )

            end
          
            return new(
                overlap,
                comm_size,
                remote_ranks,
                rank_to_idx,
                y_split_infos,
                comm,
                Nx,
                Ny,
                Ny_inner,
                master_rank,
            ) 

        end

    end

    function calParallizationRange(;
        N = Integer,     # Total grids
        P = Integer,     # Number of procs
        L = Integer,     # Overlapping grids
    )

        if ! (N >= max(1, L) * P)
            throw(ErrorException("Condition must be satisfied: N >= max(1, L) * P"))
        end

        # 2025/01/03 forced
        if N % P != 0
            throw(ErrorException("N must be a multiple of P"))
        end

        n̄ = floor(Integer, N / P)
        R = N - n̄ * P

        # "Here" is slave, "there" is master.
        # So "pull" means receiving data from master.
        # and "push" means sending data to master.

        pull_fr_rngs = Array{Union{UnitRange, Nothing}}(undef, P)
        pull_to_rngs = Array{Union{UnitRange, Nothing}}(undef, P)
        push_fr_rngs = Array{Union{UnitRange, Nothing}}(undef, P)
        push_to_rngs = Array{Union{UnitRange, Nothing}}(undef, P)
        
        # 1: lower latitude side (south), 2: higher latitude side (north)
        pull_fr_rngs_bnd = Array{Union{UnitRange, Nothing}}(undef, P, 2)
        pull_to_rngs_bnd = Array{Union{UnitRange, Nothing}}(undef, P, 2)
        push_fr_rngs_bnd = Array{Union{UnitRange, Nothing}}(undef, P, 2)
        push_to_rngs_bnd = Array{Union{UnitRange, Nothing}}(undef, P, 2)


        pull_fr_rngs_cpl = Array{Union{UnitRange, Nothing}}(undef, P)
        pull_to_rngs_cpl = Array{Union{UnitRange, Nothing}}(undef, P)
        push_fr_rngs_cpl = Array{Union{UnitRange, Nothing}}(undef, P)
        push_to_rngs_cpl = Array{Union{UnitRange, Nothing}}(undef, P)
 


        cnt = 1
        for p = 1:P
            m = (p <= R) ? n̄ + 1 : n̄  # assigned grids

            # Used between Master and Slave
            pull_fr_rngs[p] = cnt-L:cnt+m-1+L
            pull_to_rngs[p] = 1:length(pull_fr_rngs[p])
            push_fr_rngs[p] = L+1:L+m
            push_to_rngs[p] = cnt:cnt+m-1


            # Boundary
            pull_fr_rngs_bnd[p, 1] = cnt-L:cnt-1
            pull_fr_rngs_bnd[p, 2] = cnt+m:cnt+m+L-1

            pull_to_rngs_bnd[p, 1] = 1:L
            pull_to_rngs_bnd[p, 2] = L+m+1:L+m+L

            push_fr_rngs_bnd[p, 1] = L+1:L+L
            push_fr_rngs_bnd[p, 2] = L+m-L+1:L+m

            push_to_rngs_bnd[p, 1] = cnt:cnt+L-1
            push_to_rngs_bnd[p, 2] = cnt+m-L:cnt+m-1


            # Between x2o => x2o_local  (pull)
            # and     o2x_local => o2x  (push)
            L_south = L
            L_north = L
            if p == 1
                L_south = 0
            end
            if p == P
                L_north = 0
            end
            
            pull_fr_rngs_cpl[p] = 1:m
            pull_to_rngs_cpl[p] = L_south+1:L_south+m
            push_fr_rngs_cpl[p] = L_south+1:L_south+m
            push_to_rngs_cpl[p] = 1:m

            cnt += m
        end

        # South pole and north pole do not have boundaries
        pull_fr_rngs_bnd[1, 1] = nothing
        pull_to_rngs_bnd[1, 1] = nothing
        push_fr_rngs_bnd[1, 1] = nothing
        push_to_rngs_bnd[1, 1] = nothing

        pull_fr_rngs_bnd[end, 2] = nothing
        pull_to_rngs_bnd[end, 2] = nothing
        push_fr_rngs_bnd[end, 2] = nothing
        push_to_rngs_bnd[end, 2] = nothing

        # Adjust the first and last range (south pole and north pole)
        pull_fr_rngs[1] = (pull_fr_rngs[1][1]+L):pull_fr_rngs[1][end]
        pull_fr_rngs[end] = pull_fr_rngs[end][1]:(pull_fr_rngs[end][end]-L)

        pull_to_rngs[1] = 1:length(pull_fr_rngs[1])
        pull_to_rngs[end] = 1:length(pull_fr_rngs[end])
 
        push_fr_rngs[1] = 1:length(push_fr_rngs[1])


        # Change range because to southmost boundary is trimmed
        if P > 1
            pull_to_rngs_bnd[1, 2] = pull_to_rngs_bnd[1, 2] .- L
            push_fr_rngs_bnd[1, 2] = push_fr_rngs_bnd[1, 2] .- L
        end

        return n̄, (pull_fr_rngs,
               pull_to_rngs,
               push_fr_rngs,
               push_to_rngs,
               pull_fr_rngs_bnd,
               pull_to_rngs_bnd,
               push_fr_rngs_bnd,
               push_to_rngs_bnd,
               pull_fr_rngs_cpl,
               pull_to_rngs_cpl,
               push_fr_rngs_cpl,
               push_to_rngs_cpl,
        )


    end

    function getYsplitInfoByRank(
        jdi :: JobDistributionInfo,
        rank :: Integer,
    )
        idx = jdi.rank_to_idx[rank]  # rank 0 == first range
        #println("idx=$idx", ";", jdi.y_split_infos)
        return jdi.y_split_infos[idx]
    end


    mutable struct SyncInfo
        vars         :: AbstractArray{DataUnit, 1}
        y_split_info :: AbstractArray{DataUnit, 1}
    end

    function syncField!(
        vars         :: AbstractArray{DataUnit},
        jdi          :: JobDistributionInfo,
        direction    :: Symbol,
        sync_type    :: Symbol;
        vars_master  :: Union{AbstractArray{DataUnit}, Nothing} = nothing,
    )

        comm = jdi.comm
        local_rank = MPI.Comm_rank(comm)
        is_master = local_rank == jdi.master_rank
        
        reqs = Array{MPI.Request}(undef,0)

        if is_master
            if vars_master === nothing
                throw(ErrorException("The variable `vars_master` should be provided because `is_master` == true."))
            end
            if length(vars_master) != length(vars)
                throw(ErrorException("The variable `vars` and `vars_master` should have the same length."))
            end
        end

        if direction == :S2M  # Slave to master
            if sync_type == :BLOCK

                if is_master

                    # For master, because the local data cannot be convenient exchanged through
                    # MPI, we have to do the exchange manually. Therefore, the code has two parts.
                    # First part is putting the request of remote workers, and the second part is
                    # exchange locally.

                    # Part 1 : Put request. Pulling data from remote
                    for (i, _rank) in enumerate(jdi.remote_ranks)
                        for (j, var) in enumerate(vars_master)

                            v = view(var.data, :, :, getYsplitInfoByRank(jdi, _rank).push_to_rng)
                            push!(reqs, MPI.Irecv!(v, comm; source=_rank, tag=j))

                        end
                    end
                    
                    # Part 2 : Directly pull data from local 
                    y_split_info = getYsplitInfoByRank(jdi, local_rank)
                    for (j, (var_local, var_master)) in enumerate(zip(vars, vars_master))
                        v_master = view(var_master.data, :, :, y_split_info.push_to_rng)
                        v_local  = view(var_local.data,  :, :, y_split_info.push_fr_rng)
                        v_master .= v_local
                    end
 
                else
                    for (j, var) in enumerate(vars)
                        v = view(var.data, :, :, getYsplitInfoByRank(jdi, local_rank).push_fr_rng) 
                        push!(reqs, MPI.Isend(v, comm; dest=jdi.master_rank, tag=j))
                    end
                end
            end

            if sync_type == :BND
                
                if is_master
                    
                    # Here we are doing two parts. Same reason as mentioned in the comments right above. 
                    
                    # Part 1 : Put request. Pulling data from remote
                    for (i, _rank) in enumerate(jdi.remote_ranks)
                        for (k, push_to_rng_bnd) in enumerate(getYsplitInfoByRank(jdi, _rank).push_to_rng_bnd)
                            
                            # Sometimes, there are no bounday to be push
                            if push_to_rng_bnd === nothing
                                continue
                            end
                            
                            for (j, var) in enumerate(vars_master)
                                v = view(var.data, :, :, push_to_rng_bnd) 
                                push!(reqs, MPI.Irecv!(v, comm; source=_rank, tag=k*length(vars_master) + j))
                            end

                        end
                    end
                    
                    # Part 2 : Directly pull data from local
                    y_split_info = getYsplitInfoByRank(jdi, local_rank) 
                    for (k, (push_fr_rng_bnd, push_to_rng_bnd)) in enumerate(zip(
                        y_split_info.push_fr_rng_bnd,
                        y_split_info.push_to_rng_bnd,
                    ))
                       
                        if push_to_rng_bnd === nothing
                            continue
                        end
                        
                        for (j, (var_local, var_master)) in enumerate(zip(vars, vars_master))
                            v_master = view(var_master.data, :, :, push_to_rng_bnd) 
                            v_local = view(var_local.data, :, :, push_fr_rng_bnd)
                            v_master .= v_local 
                        end

                    end

                else

                    for (k, push_fr_rng_bnd) in enumerate(getYsplitInfoByRank(jdi, local_rank).push_fr_rng_bnd)
                        if push_fr_rng_bnd === nothing
                            continue
                        end
                        for (j, var) in enumerate(vars)
                            v = view(var.data, :, :, push_fr_rng_bnd) 
                            push!(reqs, MPI.Isend(v, comm; dest=jdi.master_rank, tag=k*length(vars) + j))
                        end
                    end
                end
            end


        elseif direction == :M2S  # Master to slave

            if sync_type == :BLOCK
                if is_master
                    
                    # Part 1 : Put request. Sending data to remote
                    for (i, _rank) in enumerate(jdi.remote_ranks)
                        for (j, var) in enumerate(vars_master)
                            v = view(var.data, :, :, getYsplitInfoByRank(jdi, _rank).pull_fr_rng) 
                            push!(reqs, MPI.Isend(v, comm; dest=i, tag=j))
                        end
                    end

                    # Part 2 : Directly send data to local
                    y_split_info = getYsplitInfoByRank(jdi, local_rank)
                    for (j, (var_local, var_master)) in enumerate(zip(vars, vars_master))
                        v_master = view(var_master.data, :, :, y_split_info.pull_fr_rng) 
                        v_local  = view(var_local.data,  :, :, y_split_info.pull_fr_rng) 
                        v_local .= v_master
                    end

                else
                    for (j, var) in enumerate(vars)
                        v = view(var.data, :, :, getYsplitInfoByRank(jdi, local_rank).pull_to_rng) 
                        push!(reqs, MPI.Irecv!(v, comm; source=jdi.master_rank, tag=j))
                    end
                end
            end

            if sync_type == :BND
                
                if is_master

                    # Part 1: Sending data to remote
                    for (i, _rank) in enumerate(jdi.remote_ranks)
                        for (k, pull_fr_rng_bnd) in enumerate(getYsplitInfoByRank(jdi, _rank).pull_fr_rng_bnd)

                            if pull_fr_rng_bnd === nothing
                                continue
                            end

                            for (j, var) in enumerate(vars_master)
                                v = view(var.data, :, :, pull_fr_rng_bnd) 
                                push!(reqs, MPI.Isend(v, comm; dest=_rank, tag=k * length(vars_master) + j))
                            end
                        end
                    end

                    # Part 2: Directly send data to local
                    y_split_info = getYsplitInfoByRank(jdi, local_rank) 
                    for (k, (pull_fr_rng_bnd, pull_to_rng_bnd)) in enumerate(zip(
                        y_split_info.pull_fr_rng_bnd,
                        y_split_info.pull_to_rng_bnd,
                    ))

                        if pull_fr_rng_bnd === nothing
                            continue
                        end
                        for (j, (var_local, var_master)) in enumerate(zip(vars, vars_master))
                            v_master = view(var_master.data, :, :, pull_fr_rng_bnd) 
                            v_local  = view(var_local.data,  :, :, pull_to_rng_bnd) 
                            v_local .= v_master
                        end
                    end

                else # Slave

                    for (k, pull_to_rng_bnd) in enumerate(getYsplitInfoByRank(jdi, local_rank).pull_to_rng_bnd)
                        if pull_to_rng_bnd === nothing
                            continue
                        end
                        for (j, var) in enumerate(vars)
                            v = view(var.data, :, :, pull_to_rng_bnd) 
                            push!(reqs, MPI.Irecv!(v, comm; source=jdi.master_rank, tag=k*length(vars) + j))
                        end
                    end
                end
            end
        end

        MPI.Waitall!(reqs)
        MPI.Barrier(comm) 
    end

    function printJobDistributionInfo(jdi :: JobDistributionInfo)
        println(format("overlap  = {:d}", jdi.overlap))
        println(format("nworkers = {:d}", jdi.nworkers))
        println(format("wranks   = {:s}", string(jdi.wranks)))
        println(format("wrank_to_idx = {:s}", string(jdi.wrank_to_idx)))

        for (i, y_split_info) in enumerate(jdi.y_split_infos)
            println(format("[{:d}] pull_fr_rng = {:s}", i, string(y_split_info.pull_fr_rng)))
            println(format("[{:d}] pull_to_rng = {:s}", i, string(y_split_info.pull_to_rng)))
            println(format("[{:d}] push_fr_rng = {:s}", i, string(y_split_info.push_fr_rng)))
            println(format("[{:d}] push_to_rng = {:s}", i, string(y_split_info.push_to_rng)))

            for j = 1:length(y_split_info.pull_fr_rng_bnd)
                println(format("[{:d}] pull_fr_rng_bnd[{:d}] = {:s}", i, j, string(y_split_info.pull_fr_rng_bnd[j])))
                println(format("[{:d}] pull_to_rng_bnd[{:d}] = {:s}", i, j, string(y_split_info.pull_to_rng_bnd[j])))
                println(format("[{:d}] push_fr_rng_bnd[{:d}] = {:s}", i, j, string(y_split_info.push_fr_rng_bnd[j])))
                println(format("[{:d}] push_to_rng_bnd[{:d}] = {:s}", i, j, string(y_split_info.push_to_rng_bnd[j])))
            end
        end
    end

    function createDomain(jdi :: JobDistributionInfo)

        rank = MPI.Comm_rank(jdi.comm) 

        domain = Domains.Domain(
            0, # This number will be updated
            0, # This number will be updated
            jdi.Nx,
            Int64(jdi.Ny / jdi.comm_size),
            0,             # OLx
            jdi.overlap,   # OLy
            1,
            1,
            1,            # nPx :: Integer
            jdi.comm_size, # nPy :: Integer
            jdi.Nx, #  :: Integer
            jdi.Ny, #  :: Integer
            1,
            nothing,
            nothing,
        )
 
        Domains.setDomain!(
            domain;
            rank=rank,
            number_of_pet=jdi.comm_size,
        )

        return domain
    end 
end
