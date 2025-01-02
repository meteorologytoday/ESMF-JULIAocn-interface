! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
#define FILENAME "mod_esmf_ocn.F90"
!
!-----------------------------------------------------------------------
!     OCN gridded component code 
!-----------------------------------------------------------------------
!
module mod_esmf_ocn
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  use ESMF
  use NUOPC
  use NUOPC_Model,                                                  &
      NUOPC_SetServices          => SetServices,                    &
      NUOPC_Label_SetClock       => label_SetClock,                 &
      NUOPC_Label_Advance        => label_Advance,                  &
      NUOPC_Label_Datanitialize => label_DataInitialize
!
  use mod_types


! ---- ESMF_PY BEGIN ----
    USE, INTRINSIC :: ISO_C_BINDING
! ---- ESMF_PY END ----
 


!
  implicit none

! ---- ESMF_PY BEGIN ----

    INTERFACE
      INTEGER FUNCTION MPI_Comm_c2f(c_handle) bind(C, name="f_MPI_Comm_c2f")
        IMPORT :: C_PTR
        TYPE(C_PTR), VALUE :: c_handle
      END FUNCTION
    END INTERFACE

    INTERFACE
      SUBROUTINE MARCOISCOOL_PRINTCALENDAR(cal_ptr, timestr_ptr) BIND(C, name="MARCOISCOOL_PRINTCALENDAR")
        IMPORT :: C_PTR
        TYPE(C_PTR), VALUE :: cal_ptr
        TYPE(C_PTR), VALUE :: timestr_ptr
      END SUBROUTINE MARCOISCOOL_PRINTCALENDAR
    END INTERFACE


    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_INIT(thread_id, comm) BIND(C, name="MARCOISCOOL_JLMODEL_INIT")
        import :: C_INT
        INTEGER(C_INT), VALUE :: thread_id
        INTEGER(C_INT), VALUE :: comm
      END SUBROUTINE MARCOISCOOL_JLMODEL_INIT
    END INTERFACE

    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_FINAL(thread_id, comm) BIND(C, name="MARCOISCOOL_JLMODEL_FINAL")
        import :: C_INT
        INTEGER(C_INT), VALUE :: thread_id
        INTEGER(C_INT), VALUE :: comm
      END SUBROUTINE MARCOISCOOL_JLMODEL_FINAL
    END INTERFACE

    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_getDomainInfo(           &
        sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr,         &
        myXGlobalLo, myYGlobalLo                                    &
    ) BIND(C, name="MARCOISCOOL_JLMODEL_getDomainInfo")
        import :: C_INT
        INTEGER(C_INT), intent(inout) :: sNx
        INTEGER(C_INT), intent(inout) :: sNy
        INTEGER(C_INT), intent(inout) :: OLx
        INTEGER(C_INT), intent(inout) :: OLy
        INTEGER(C_INT), intent(inout) :: nSx
        INTEGER(C_INT), intent(inout) :: nSy
        INTEGER(C_INT), intent(inout) :: nPx
        INTEGER(C_INT), intent(inout) :: nPy
        INTEGER(C_INT), intent(inout) :: Nx
        INTEGER(C_INT), intent(inout) :: Ny
        INTEGER(C_INT), intent(inout) :: Nr
        INTEGER(C_INT), intent(inout) :: myXGlobalLo
        INTEGER(C_INT), intent(inout) :: myYGlobalLo
      END SUBROUTINE MARCOISCOOL_JLMODEL_getDomainInfo
    END INTERFACE

    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_sendInfo2Model(            &
        msg                                                     &
    ) BIND(C, name="MARCOISCOOL_JLMODEL_sendInfo2Model")
        IMPORT 
        TYPE(C_PTR), VALUE :: msg
      END SUBROUTINE MARCOISCOOL_JLMODEL_sendInfo2Model
    END INTERFACE



! ---- ESMF_PY END ----

!

  private

!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
  public :: OCN_SetServices
!
  contains
!

  subroutine OCN_SetServices(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
!
  rc = ESMF_SUCCESS

  call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! set entry point for methods that require specific implementation
  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p1"/),       &
                               userRoutine=OCN_Init1, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE,       &
                               phaseLabelList=(/"IPDv00p2"/),       &
                               userRoutine=OCN_Init2, rc=rc)
    
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock,  &
                            specRoutine=OCN_SetClock, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance,   &
                            specRoutine=OCN_Run, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! ---- ESMF_PY BEGIN ----

  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE,         &
                               phaseLabelList=(/"FPDV00p1"/),       &
                               userRoutine=OCN_Final, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  
  ! ---- ESMF_PY END ----

  end subroutine OCN_SetServices
!
!-----------------------------------------------------------------------
!     Initialization phase 1, set import/export fields
!-----------------------------------------------------------------------
!
  subroutine OCN_Init1(gcomp, importState, exportState, clock, rc)

  TYPE(ESMF_GridComp) :: gcomp
  TYPE(ESMF_State)    :: importState, exportState
  TYPE(ESMF_Clock)    :: clock

  TYPE(ESMF_VM) :: vm
  INTEGER :: mpicomtmp
  INTEGER,                     INTENT(  OUT) :: rc

  ! Local variables
  TYPE(ESMF_GridComp), POINTER :: p_gcomp
  TYPE(ESMF_State),    POINTER :: p_importState
  TYPE(ESMF_State),    POINTER :: p_exportState
  TYPE(ESMF_Clock),    POINTER :: p_clock
  ! Time hackery
  TYPE(ESMF_Time) :: startTime
  TYPE(ESMF_Time) :: stopTime
  TYPE(ESMF_TimeInterval) :: couplingInterval
  ! decomposition hackery
  INTEGER :: ids, ide, jds, jde, kds, kde
  INTEGER :: ims, ime, jms, jme, kms, kme
  INTEGER :: ips, ipe, jps, jpe, kps, kpe
  INTEGER :: domdesc
  LOGICAL :: bdy_mask(4)
  CHARACTER(LEN=256) :: couplingIntervalString
  
  rc = ESMF_SUCCESS

  ! Call WRF "init" routine, which, for a DM_PARALLEL run, will recognize 
  ! that ESMF has already called MPI_INIT and respond appropriately.  
  !! call wrf_init( no_init1=.TRUE. )

  call NUOPC_Advertise(exportState,                                 &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_Advertise(importState,                                 &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_Advertise(importState,                                 &
      StandardName="surface_net_downward_shortwave_flux",           &
      name="rsns", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return


  end subroutine
!
!-----------------------------------------------------------------------
!     Initialization phase 2 
!-----------------------------------------------------------------------
!
  subroutine OCN_Init2(gcomp, importState, exportState, clock, rc)

  USE ISO_C_BINDING
  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: importState, exportState
  type(ESMF_Clock)     :: clock
  integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declaration
!-----------------------------------------------------------------------
!
  type(ESMF_Field) :: field
  type(ESMF_DistGrid) :: distGrid
  type(ESMF_Grid) :: ocnGridIn
  type(ESMF_Grid) :: ocnGridOut

  integer :: myThid = 1, mpisize, f_comm
  integer, TARGET :: comm
  integer :: localPet, petCount
  character(ESMF_MAXSTR) :: gname
  

!
  type(ESMF_VM) :: vm
  
  
  TYPE(ESMF_Time), TARGET     :: currTime
  TYPE(ESMF_TimeInterval)     :: timeStep     ! how long to run in this call
  TYPE(ESMF_Calendar), TARGET :: esmCal
  CHARACTER(LEN=1024), TARGET :: timestr
  rc = ESMF_SUCCESS

  call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                  mpiCommunicator=comm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call MPI_Comm_size(comm, mpisize)
  print *, "!!!!!!!!!!!!!!! MPI Communicator = ", comm, "; mpisize = ", mpisize, "; localpet = ", localpet
! ---- ESMF_PY BEGIN ----

    print *, "Try to print time in Init2"
 
      ! Get the clock detail information
      call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep,   &
                         rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=FILENAME)) return

      call ESMF_TimeGet(currTime, calendar=esmCal, &
         timeStringISOFrac=timestr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__)) return  ! bail out

      timestr = trim(timestr) // C_NULL_CHAR

    call MARCOISCOOL_PRINTCALENDAR(C_LOC(currTime), C_LOC(timestr))

    print *, "Calling function: MARCOISCOOL_JLMODEL_INIT"
     
    !comm_ptr = C_LOC(comm)
    !f_comm = MPI_Comm_c2f(comm_ptr)
    !print *, "f_comm = ", f_comm
    CALL MARCOISCOOL_JLMODEL_INIT(myThid, comm)
! ---- ESMF_PY END ----


  PRINT *, "setting grid arrays..."
  call OCN_SetGridArrays(gcomp, petCount, localPet, ocnGridIn,rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  PRINT *, "setting grid arrays finished ..."
  
  ocnGridOut = ocnGridIn
  PRINT *, "copy grid arrays finished ..."

   field = ESMF_FieldCreate(name="sst", grid=ocnGridOut,             &
     typekind=ESMF_TYPEKIND_R8, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return
   call NUOPC_Realize(exportState, field=field, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return

   field = ESMF_FieldCreate(name="pmsl", grid=ocnGridIn,             &
     typekind=ESMF_TYPEKIND_R8, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return
   call NUOPC_Realize(importState, field=field, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return

   field = ESMF_FieldCreate(name="rsns", grid=ocnGridIn,             &
     typekind=ESMF_TYPEKIND_R8, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return
   call NUOPC_Realize(importState, field=field, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
       line=__LINE__, file=FILENAME)) return

   !call OCN_SetInitData(gcomp, ocnGridIn, ocnGridOut, rc)
   !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
   !    line=__LINE__, file=FILENAME)) return

  PRINT *, "Exiting OCN_Init2"


  end subroutine

! ---- ESMF_PY BEGIN ----
  subroutine OCN_Final(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer :: myThid = 1
    integer :: comm, localPet, petCount
    character(ESMF_MAXSTR) :: gname

    type(ESMF_VM) :: vm
    
    print *, "[Fortran Code] Finalizing..."
      
    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
        line=__LINE__, file=FILENAME)) return

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                    mpiCommunicator=comm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
        line=__LINE__, file=FILENAME)) return


    CALL MARCOISCOOL_JLMODEL_FINAL(myThid, comm)
  
  end subroutine
! ---- ESMF_PY END ----

!
!
!-----------------------------------------------------------------------
!     Ocean Check Import Fields
!-----------------------------------------------------------------------
!
  subroutine OCN_CheckImport(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: modelClock, driverClock
  type(ESMF_TimeInterval) :: ocnTimeStep
  type(ESMF_Time) :: ocnStartTime
  type(ESMF_Time) :: ocnEndTime
  
  rc = ESMF_SUCCESS

  end subroutine
!
!-----------------------------------------------------------------------
!     Ocean Set Clock  
!-----------------------------------------------------------------------
!
  subroutine OCN_SetClock(gcomp, rc)

  type(ESMF_GridComp)  :: gcomp
  integer, intent(out) :: rc

  type(ESMF_Clock)     :: clock
  type(ESMF_Clock)     :: modelClock, driverClock
  type(ESMF_TimeInterval) :: ocnTimeStep
  type(ESMF_Time) :: ocnStartTime
  type(ESMF_Time) :: ocnEndTime
  type(ESMF_TimeInterval) :: stabilityTimeStep
  
  rc = ESMF_SUCCESS

  call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_TimeIntervalSet(stabilityTimeStep, m=20, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
  subroutine OCN_Run(gcomp, rc)

  use, intrinsic :: iso_c_binding
  TYPE(ESMF_GridComp)       :: gcomp
  INTEGER, INTENT(  OUT)    :: rc

  type(ESMF_State),    TARGET:: importState
  type(ESMF_State),    TARGET:: exportState
  type(ESMF_Clock),    TARGET:: clock

  TYPE(ESMF_GridComp), POINTER :: p_gcomp
  TYPE(ESMF_State),    POINTER :: p_importState
  TYPE(ESMF_State),    POINTER :: p_exportState
  TYPE(ESMF_Clock),    POINTER :: p_clock

  ! Local variables
  TYPE(ESMF_Time) :: currTime, nextTime
  TYPE(ESMF_TimeInterval) :: timeStep     ! how long to run in this call
  CHARACTER(LEN=256) :: timeStr
  TYPE(ESMF_StateIntent_Flag) :: stateintent
  INTEGER :: itemCount
  INTEGER :: iLoopOCN = 1
 
  CHARACTER(LEN=4096), TARGET :: msg_to_model
  CHARACTER(LEN=40) :: timestr_compact
  INTEGER :: TIME_YY, TIME_MM, TIME_DD, TIME_H, TIME_M, TIME_S
  
  rc = ESMF_SUCCESS

  ! Get the clock and import/export states
  call NUOPC_ModelGet(gcomp, modelClock=clock,                      &
                      importState=importState,                      &
                      exportState=exportState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  ! Get the clock detail information
  call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep,   &
                     rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  nextTime = currTime + timeStep

  call ESMF_ClockPrint(clock, options="currTime", &
         preString="### [ClockPrint] Current Time: ", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  call ESMF_TimePrint(currTime,                                     &
         preString="##### Current Time: ", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out


  call ESMF_TimePrint(nextTime,                                     &
         preString="##### Next    Time: ", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  call ESMF_TimeGet(currTime, YY=TIME_YY, MM=TIME_MM, DD=TIME_DD,   &
                    H=TIME_H, M=TIME_M, S=TIME_S,                   &
                    rc=rc &
  )
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  
  write (timestr_compact,                                               &
         '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
         TIME_YY, TIME_MM, TIME_DD, TIME_H, TIME_M, TIME_S
 
  write (msg_to_model, '(A,A,A)') '{"subject":"sendTime", "time": "', trim(timestr_compact), '"}'
  msg_to_model = trim(msg_to_model) // C_NULL_CHAR
  call MARCOISCOOL_JLMODEL_sendInfo2Model(C_LOC(msg_to_model))

  print *, "OCN iLoop is: ", iLoopOCN

  !! call OCN_Get(gcomp, iLoopOCN, rc)

  iLoopOCN = iLoopOCN + 1

  end subroutine

  subroutine OCN_SetGridArrays(gcomp, petCount, localPet, gridIn,rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp), intent(inout) :: gcomp
  integer, intent(in) :: localPet 
  integer, intent(in) :: petCount 
  type(ESMF_Grid) :: gridIn
  integer, intent(inout) :: rc
  type(ESMF_VM) :: vm
  character(ESMF_MAXSTR) :: cname

! ---- ESMF_PY BEGIN ----
  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy
  INTEGER Nr
!  INTEGER Nx, Ny, Nr, i, j, bi, bj
  INTEGER myXGlobalLo, myYGlobalLo, localN
! ---- ESMF_PY END ----

  integer :: myThid = 1
  integer :: k, m, n, p, iG, jG, tile
  character(ESMF_MAXSTR) :: name
  real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
  type(ESMF_Array) :: arrX, arrY
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  integer, allocatable :: deBlockList(:,:,:)
  integer :: localDECount, j
  character(ESMF_MAXSTR) ::  ofile

  integer :: nx = 10
  integer :: ny = 10
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  rc = ESMF_SUCCESS

! ---- ESMF_PY BEGIN ----
  CALL MARCOISCOOL_JLMODEL_getDomainInfo(                           &
                       sNx, sNy, OLx, OLy,                          &
                       nSx, nSy, nPx, nPy, Nx, Ny, Nr,              &
                       myXGlobalLo, myYGlobalLo                     &
  )

  print *, "sNx = ", sNx 
  print *, "sNy = ", sNy
  print *, "OLx = ", OLx
  print *, "OLy = ", OLy
  print *, "nSx = ", nSx 
  print *, "nSy = ", nSy
  print *, "nPx = ", nPx
  print *, "nPy = ", nPy
  print *, "Nx = ", Nx
  print *, "Ny = ", Ny
  print *, "Nr = ", Nr
  print *, "myXGlobalLo = ", myXGlobalLo
  print *, "myYGlobalLo = ", myYGlobalLo
! ---- ESMF_PY END ----

!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
!
  if (.not.allocated(deBlockList)) then
    allocate(deBlockList(2,2,1:nPx*nPy))
  end if



  deBlockList(:,1,1) = (/ 1, 1/)
  deBlockList(:,2,1) = (/ 5, ny/)
  deBlockList(:,1,2) = (/ 6, 1/)
  deBlockList(:,2,2) = (/nx, ny/)

!
!  do tile = 1, (nPx*nPy)
!    deBlockList(1,1,tile) = mpi_myXGlobalLo(tile)
!    deBlockList(1,2,tile) = mpi_myXGlobalLo(tile)+sNx-1
!    deBlockList(2,1,tile) = mpi_myYGlobalLo(tile) 
!    deBlockList(2,2,tile) = mpi_myYGlobalLo(tile)+sNy-1
!  end do


!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!
  distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                 maxIndex=(/ nx, ny /),             &
                                 deBlockList=deBlockList,           &
                                 rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
!
  staggerLoc = ESMF_STAGGERLOC_CENTER ! Icross
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
  ! Icross
  gridIn = ESMF_GridCreate(distgrid=distGrid,                       &
                           indexflag=ESMF_INDEX_GLOBAL,             &
                           name="ocn_grid",                         &
                           rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate coordinates 
!-----------------------------------------------------------------------
!
  call ESMF_GridAddCoord(gridIn, staggerLoc=staggerLoc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for masking (not for cell corners)
!-----------------------------------------------------------------------
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_MASK,                &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Allocate items for grid area (only for cell center)
!-----------------------------------------------------------------------
!
  call ESMF_GridAddItem(gridIn, staggerLoc=staggerLoc,              &
                        itemflag=ESMF_GRIDITEM_AREA,                &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get pointers and set coordinates for the grid 
!-----------------------------------------------------------------------
! 
  call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
  print *, "localDECount is: ", localDECount, " localPet is: ", localPet

  do j = 0, localDECount-1

    print *, "j is: ", j
    call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                         coordDim=1, farrayPtr=ptrX, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                         line=__LINE__, file=FILENAME)) return
!
    call ESMF_GridGetCoord(gridIn, staggerLoc=staggerLoc, localDE=j,&
                         coordDim=2, farrayPtr=ptrY, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,  &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Fill the pointers    
!-----------------------------------------------------------------------
!

    print *, "!!!!!!!!!!!!! localpet = ", localpet
    print *, "!!!!!!!!!!!!! j = ", j
    print *, "Size of array ptrX dim1: ", size(ptrX, 1)
    print *, "Size of array ptrX dim2: ", size(ptrX, 2)
    print *, "Size of array ptrX dim1 lbound: ", lbound(ptrX, 1)
    print *, "Size of array ptrX dim1 ubound: ", ubound(ptrX, 1)
    print *, "Size of array ptrX dim2 lbound: ", lbound(ptrX, 2)
    print *, "Size of array ptrX dim2 ubound: ", ubound(ptrX, 2)

    do n = 1, nx/nPx  ! nx/nPx = size of tile
      do m = 1, ny
        localN = localPet * (nx/nPx) + n
        !! TODO: pseudo mesh
        !print *, "loop n is: ", n, ", localN is: ", localN, ", m is: ", m
        !print *, "ocnMesh n is: ", n, " m is: ", m
        ptrX(localN,m) = 0 + (360.0d0/nx)*(localN-0.5d0)
        ptrY(localN,m) = -90 + (180.0d0/ny)*(m-0.5d0)
        !print *, "ocean meshX is: ", ptrX(localN,m)
        !print *, "ocean meshY is: ", ptrY(localN,m)
      end do
    end do
    print *, "ocean array filled!  localpet =", localpet
!
!-----------------------------------------------------------------------
!     Nullify pointers 
!-----------------------------------------------------------------------
!
    if (associated(ptrY)) then
      nullify(ptrY)
    end if
    if (associated(ptrX)) then
      nullify(ptrX)
    end if
    print *, "ptr nullified!"

    call ESMF_GridCompSet(gcomp, grid=gridIn, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                line=__LINE__, file=FILENAME)) return



  end do


!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
    if ( (debugLevel >= 1)) then

      print *, "Output ocean grid for localpet = ", localpet
      call ESMF_GridGetCoord(gridIn,                                &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                             coordDim=1, array=arrX, rc=rc)
      print *, "localpet= ", localpet, "; after getcoordx rc=", rc
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return

      call ESMF_GridGetCoord(gridIn,                                &
                             staggerLoc=ESMF_STAGGERLOC_CENTER,     &
                             coordDim=2, array=arrY, rc=rc)
      print *, "localpet= ", localpet, "; after getcoordy rc=", rc
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return

      write (ofile, "(A6,I3.3,A3)") "ocn_xa", localPet, ".nc"
      print *, "Ouptut file: ", ofile, "; localpet = ", localpet, "; rc=", rc
      call ESMF_ArrayWrite(arrX, filename=ofile,                    &
                           status=ESMF_FILESTATUS_REPLACE, rc=rc)
      print *, "afte rc = ", rc
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return


      write (ofile, "(A6,I3.3,A3)") "ocn_ya", localPet, ".nc"
      print *, "Ouptut file: ", ofile, "; localpet = ", localpet, "; rc=", rc
      call ESMF_ArrayWrite(arrY, filename=ofile,                    &
                           status=ESMF_FILESTATUS_REPLACE, rc=rc)
      print *, "afte rc = ", rc
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,&
                             line=__LINE__, file=FILENAME)) return

        print *, "Finished output ocean grid for localpet = ", localpet
    end if

  print *, "OCN debug finished!"
!
  end subroutine OCN_SetGridArrays
!
!-----------------------------------------------------------------------
!     Set the initial data value
!-----------------------------------------------------------------------
!
  subroutine OCN_SetInitData(gcomp, gridIn, gridOut, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp), intent(inout) :: gcomp
  type(ESMF_Grid) :: gridIn
  type(ESMF_Grid) :: gridOut
  integer, intent(inout) :: rc
  type(ESMF_VM) :: vm

  type(ESMF_State) :: exportState
  type(ESMF_State) :: importState
  integer :: myThid = 1
  integer :: k, m, n, p, iG, jG
  character(ESMF_MAXSTR) :: name
  real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:)
  real(ESMF_KIND_R8), pointer :: ptrIn(:,:)
  real(ESMF_KIND_R8), pointer :: ptrOut(:,:)
  real(ESMF_KIND_R8), pointer :: ptrIn2(:,:)
  type(ESMF_Array) :: arrX, arrY
  type(ESMF_StaggerLoc) :: staggerLoc
  type(ESMF_DistGrid) :: distGrid
  integer, allocatable :: meshType(:)
  character(ESMF_MAXSTR), allocatable :: meshTypeName(:)
  type(ESMF_Field) :: fieldIn
  type(ESMF_Field) :: fieldOut
  type(ESMF_Field) :: fieldIn2

  integer :: ii, jj
  integer :: nx = 10
  integer :: ny = 10
  character(ESMF_MAXSTR) :: cname, ofile
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname,                   &
                        exportState=exportState,                    &
                        importState=importState,                    &
                        rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_GridGet(gridIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_StateGet(importState, "pmsl", fieldIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_FieldGet(fieldIn, farrayPtr=ptrIn, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  do ii = 1, Nx
    do jj = 1, Ny
      ptrIn(ii,jj) = 0.001d0*ii + 1.0d0*ii
    end do
  end do

  call ESMF_StateGet(exportState, "sst", fieldOut, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_FieldGet(fieldOut, farrayPtr=ptrOut, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  do ii = 1, Nx
    do jj = 1, Ny
      ptrOut(ii,jj) = 0.001d0*ii + 1.0d0*ii
    end do
  end do

  call ESMF_StateGet(importState, "rsns", fieldIn2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_FieldGet(fieldIn2, farrayPtr=ptrIn2, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  do ii = 1, Nx
    do jj = 1, Ny
      ptrIn2(ii,jj) = 0.00001d0*ii + 100.0d0*ii
    end do
  end do

  if (debugLevel >= 1) then
    write (ofile, "(A9)") "rsnsOCN.nc"
    call ESMF_FieldWrite(fieldIn2, trim(ofile), rc=rc)
  end if

  end subroutine OCN_SetInitData

  subroutine OCN_Get(gcomp, iLoop, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
  implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
  type(ESMF_GridComp) :: gcomp
  integer :: iLoop
  integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  character(ESMF_MAXSTR) ::  ofile
  type(ESMF_Field) :: field
  type(ESMF_State) :: importState
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
  call ESMF_StateGet(importState, 'pmsl', field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
  if (debugLevel >= 1) then
    write (ofile, "(A7,I2.2,A3)") "pmslOCN", iLoop, ".nc"
    call ESMF_FieldWrite(field, trim(ofile), rc=rc)
  end if

  end subroutine OCN_Get

end module mod_esmf_ocn
