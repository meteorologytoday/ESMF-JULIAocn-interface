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


! ---- ESMF_JL BEGIN ----
    USE, INTRINSIC :: ISO_C_BINDING
! ---- ESMF_JL END ----
 


!
  implicit none

! ---- ESMF_JL BEGIN ----

    INTEGER, PARAMETER :: DIRECTION_CPL2COMP = 0
    INTEGER, PARAMETER :: DIRECTION_COMP2CPL = 1

    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_RUN( &
            importState_ptr,              &
            exportState_ptr,              &
            timeStep_ptr                 &
        ) BIND(C, name="MARCOISCOOL_JLMODEL_RUN")
        IMPORT :: C_PTR
        TYPE(C_PTR), VALUE :: importState_ptr
        TYPE(C_PTR), VALUE :: exportState_ptr
        TYPE(C_PTR), VALUE :: timeStep_ptr
      END SUBROUTINE MARCOISCOOL_JLMODEL_RUN
    END INTERFACE

    INTERFACE
        TYPE(C_PTR) FUNCTION MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8( &
            varname,              &
            arr_size,             &
            direction             &
        ) BIND(C, name="MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8")
        IMPORT :: C_PTR, C_INT, C_DOUBLE
        TYPE(C_PTR), VALUE :: varname
        INTEGER(C_INT), VALUE :: arr_size
        INTEGER(C_INT), VALUE :: direction
      END FUNCTION MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8
    END INTERFACE


    INTERFACE
        SUBROUTINE MARCOISCOOL_JLMODEL_GETPUT_VARIABLE_REAL8( &
            varname,              &
            ptr,                  &
            arr_size,             &
            direction             &
        ) BIND(C, name="MARCOISCOOL_JLMODEL_GETPUT_VARIABLE_REAL8")
        IMPORT :: C_PTR, C_INT, C_DOUBLE
        TYPE(C_PTR), VALUE :: varname
        REAL(C_DOUBLE), DIMENSION(*) :: ptr
        INTEGER(C_INT), VALUE :: arr_size
        INTEGER(C_INT), VALUE :: direction
      END SUBROUTINE MARCOISCOOL_JLMODEL_GETPUT_VARIABLE_REAL8
    END INTERFACE


    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_REGISTER_VARIABLE_REAL8( &
            varname,              &
            ptr,                  &
            length                &
        ) BIND(C, name="MARCOISCOOL_JLMODEL_REGISTER_VARIABLE_REAL8")
        IMPORT :: C_PTR, C_INT, C_DOUBLE
        TYPE(C_PTR), VALUE :: varname
        REAL(C_DOUBLE), DIMENSION(*) :: ptr
        INTEGER(C_INT), VALUE :: length
      END SUBROUTINE MARCOISCOOL_JLMODEL_REGISTER_VARIABLE_REAL8
    END INTERFACE



    INTERFACE
      INTEGER FUNCTION MPI_Comm_c2f(c_handle) bind(C, name="f_MPI_Comm_c2f")
        IMPORT :: C_PTR
        TYPE(C_PTR), VALUE :: c_handle
      END FUNCTION
    END INTERFACE

    INTERFACE
      SUBROUTINE MARCOISCOOL_JLMODEL_REGISTER_TIME(cal_ptr, starttime_ptr, stoptime_ptr, timeinterval_s) BIND(C, name="MARCOISCOOL_JLMODEL_REGISTER_TIME")
        IMPORT :: C_PTR, C_INT
        TYPE(C_PTR), VALUE :: cal_ptr
        TYPE(C_PTR), VALUE :: starttime_ptr
        TYPE(C_PTR), VALUE :: stoptime_ptr
        INTEGER(C_INT), VALUE :: timeinterval_s
      END SUBROUTINE MARCOISCOOL_JLMODEL_REGISTER_TIME
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



! ---- ESMF_JL END ----

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

  ! ---- ESMF_JL BEGIN ----

  call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_FINALIZE,         &
                               phaseLabelList=(/"FPDV00p1"/),       &
                               userRoutine=OCN_Final, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return
  
  ! ---- ESMF_JL END ----

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
  
  real(ESMF_KIND_R8), pointer :: ptr(:,:)
  CHARACTER(LEN=1024), TARGET :: dtype, varname

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
! ---- ESMF_JL BEGIN ----

    print *, "Calling function: MARCOISCOOL_JLMODEL_INIT"
    CALL MARCOISCOOL_JLMODEL_INIT(myThid, comm)
! ---- ESMF_JL END ----

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

    print *, "Test if I can do this"

  call CPL_talk_COMP(gcomp, "sst", DIRECTION_COMP2CPL, localPet, rc)
  call CPL_talk_COMP(gcomp, "rsns", DIRECTION_CPL2COMP, localPet, rc)



  !print *, "Register sst"
  !call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
  !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
  !    line=__LINE__, file=FILENAME)) return
  ! varname = trim("sst") // C_NULL_CHAR
  ! call MARCOISCOOL_JLMODEL_REGISTER_VARIABLE_REAL8(  &
  !      C_LOC(varname),                         &
  !      ptr,                                    &
  !      100                                    &
  ! ) 





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

! ---- ESMF_JL BEGIN ----
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
! ---- ESMF_JL END ----

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
  type(ESMF_TimeInterval) :: stabilityTimeStep
  type(ESMF_TimeInterval), TARGET :: timeStep
  type(ESMF_Time), TARGET :: startTime, stopTime
  INTEGER(ESMF_KIND_I4) :: timeinterval_s

  CHARACTER(LEN=100), TARGET :: startTimeStr, stopTimeStr
  INTEGER :: TIME_YY, TIME_MM, TIME_DD, TIME_H, TIME_M, TIME_S
 
  rc = ESMF_SUCCESS

  call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_TimeIntervalSet(stabilityTimeStep, m=10, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

! Pass Calendar, begin Time, and timestep

  print *, "So we have to redo ModelGet, otherwise I will get"
  print *, "the old timestep."
  print *, "It seems compsetclock messed up the clock."
  call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  call ESMF_ClockGet(clock, startTime=startTime, stopTime=stopTime, &
                            timeStep=timeStep,   &
                     rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  call ESMF_TimeIntervalGet(timeStep, s=timeinterval_s, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out


! Make StartTimeStr
   call ESMF_TimeGet(startTime, timeStringISOFrac=startTimeStr)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  startTimeStr = trim(startTimeStr) // C_NULL_CHAR

! Make StopTimeStr
  call ESMF_TimeGet(stopTime, timeStringISOFrac=stopTimeStr)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=__FILE__)) return  ! bail out

  stopTimeStr = trim(stopTimeStr) // C_NULL_CHAR

  CALL MARCOISCOOL_JLMODEL_REGISTER_TIME( &
         C_LOC(startTime), C_LOC(startTimeStr), &
         C_LOC(stopTimeStr), timeinterval_s &
  ) 
  print *, "Leaving OCN_SetClock"
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
  TYPE(ESMF_TimeInterval), TARGET :: timeStep     ! how long to run in this call
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

  
  !write (timestr_compact,                                               &
  !       '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)') &
  !       TIME_YY, TIME_MM, TIME_DD, TIME_H, TIME_M, TIME_S
 
  !write (msg_to_model, '(A,A,A)') '{"subject":"sendTime", "time": "', trim(timestr_compact), '"}'
  !msg_to_model = trim(msg_to_model) // C_NULL_CHAR
  !call MARCOISCOOL_JLMODEL_sendInfo2Model(C_LOC(msg_to_model))

  print *, "OCN iLoop is: ", iLoopOCN

  !! call OCN_Get(gcomp, iLoopOCN, rc)

    call MARCOISCOOL_JLMODEL_RUN( &
        C_LOC(importState),       &
        C_LOC(exportState),       &
        C_LOC(timeStep)           &
    )


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

! ---- ESMF_JL BEGIN ----
  INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy
  INTEGER Nr
!  INTEGER Nx, Ny, Nr, i, j, bi, bj
  INTEGER myXGlobalLo, myYGlobalLo, localN
  INTEGER, ALLOCATABLE :: arr_myXGlobalLo(:), arr_myYGlobalLo(:)
! ---- ESMF_JL END ----

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

!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
! The `vm` here is important in later passing myXGlobalLo and myYGlobalLo
!
  call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return


! ---- ESMF_JL BEGIN ----
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

  if (.not. allocated(arr_myXGlobalLo)) then
    allocate(arr_myXGlobalLo(nPx*nPy))
    allocate(arr_myYGlobalLo(nPx*nPy))
  end if

  ! It is a less clean way to get array of myXYGlobalLo
  call ESMF_VMAllGatherV(vm, sendData=(/ myXGlobalLo /),            &
                         sendCount=1, recvData=arr_myXGlobalLo,     &
                         recvCounts=(/ (1, k = 0, petCount-1) /),   &
                         recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                         rc=rc)
!
  call ESMF_VMAllGatherV(vm, sendData=(/ myYGlobalLo /),            &
                         sendCount=1, recvData=arr_myYGlobalLo,     &
                         recvCounts=(/ (1, k = 0, petCount-1) /),   &
                         recvOffsets=(/ (k, k = 0, petCount-1) /),  &
                         rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return



  print *, "arr_myXGlobalLo = ", arr_myXGlobalLo
  print *, "arr_myYGlobalLo = ", arr_myYGlobalLo
! ---- ESMF_JL END ----

!
!
  if (.not.allocated(deBlockList)) then
    allocate(deBlockList(2,2,1:nPx*nPy))
  end if



!  deBlockList(:,1,1) = (/ 1, 1/)
!  deBlockList(:,2,1) = (/ 5, ny/)
!  deBlockList(:,1,2) = (/ 6, 1/)
!  deBlockList(:,2,2) = (/nx, ny/)

  print *, "Assining deBlockList"
!
  do tile = 1, (nPx*nPy)

    deBlockList(1,1,tile) = arr_myXGlobalLo(tile)
    deBlockList(2,1,tile) = arr_myYGlobalLo(tile)
 
    deBlockList(1,2,tile) = arr_myXGlobalLo(tile)+sNx-1
    deBlockList(2,2,tile) = arr_myYGlobalLo(tile)+sNy-1

  end do


  print *, "Create distGrid"
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
  
  print *, "Set staggering type"
!
!-----------------------------------------------------------------------
!     Set staggering type 
!-----------------------------------------------------------------------
!
!
  staggerLoc = ESMF_STAGGERLOC_CENTER ! Icross
  
  print *, "Create ESMF Grid"
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

  print *, "Allocate coordinates"
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


  subroutine CPL_talk_COMP(gcomp, varname, direction, localpet, rc)
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
  character(len=*) , TARGET:: varname
  integer, intent(out) :: rc
  integer, intent(in) :: direction, localpet

  
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
  type(c_ptr) :: c_ptr_comp
  real(ESMF_KIND_R8), pointer :: ptr(:, :), ptr_comp1d(:), cont_ptr(:, :)
!  real(ESMF_KIND_R8), allocatable :: cont_arr(:)
  integer :: arr_size_x, arr_size_y
  integer :: i, j, lowbnd_x, lowbnd_y
  type(ESMF_Field) :: field
  type(ESMF_State) :: state
!
  rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
  print *, "[CPL_talk_COMP] Here. Localpet=", localpet
  if (direction .eq. DIRECTION_CPL2COMP) then
    call ESMF_GridCompGet(gcomp, importState=state, rc=rc)
  elseif (direction .eq. DIRECTION_COMP2CPL) then
    call ESMF_GridCompGet(gcomp, exportState=state, rc=rc)
  else
    print *, "ERROR: Unknown direction : ", direction
  end if
  print *, "[CPL_talk_COMP] Check"
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return
!
!-----------------------------------------------------------------------
!     Get field
!-----------------------------------------------------------------------
!
  print *, "[CPL_talk_COMP] Get field"
  call ESMF_StateGet(state, varname, field, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
                         line=__LINE__, file=FILENAME)) return

  print *, "[CPL_talk_COMP] Get ptr"
  call ESMF_FieldGet(field, farrayPtr=ptr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
      line=__LINE__, file=FILENAME)) return

  varname = trim(varname)
  arr_size_x = SIZE(ptr, 1)
  arr_size_y = SIZE(ptr, 2)
  print *, "varname = ", trim(varname)
  print *, "arr_size_x = ", arr_size_x
  print *, "arr_size_y = ", arr_size_y

  lowbnd_x = LBOUND(ptr, 1)
  lowbnd_y = LBOUND(ptr, 2)

  !ALLOCATE( cont_arr(arr_size_x * arr_size_y) )
  c_ptr_comp = MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8(C_LOC(varname), arr_size_x * arr_size_y, direction)
  CALL c_f_pointer(c_ptr_comp, ptr_comp1d, [arr_size_x*arr_size_y])

  print *, "[CPL_talk_COMP] Ready to run loop. localpet=", localpet
  !c_ptr_comp = MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8(C_LOC(varname), arr_size_x * arr_size_y)
  !CALL MARCOISCOOL_JLMODEL_GETPUT_VARIABLE_REAL8( &
  !  C_LOC(varname),                               &
  !  cont_arr,                                     &
  !  arr_size_x * arr_size_y,                      &
  !  direction &
  !)

  if (direction .eq. DIRECTION_CPL2COMP) then
     do i = 1, arr_size_x
     do j = 1, arr_size_y
       ptr(i + (lowbnd_x-1), j + (lowbnd_y-1)) = i + (j-1)*arr_size_x
       ptr_comp1d(i + (j-1)*arr_size_x) = ptr(i + (lowbnd_x-1), j + (lowbnd_y-1))
     end do 
     end do
  elseif (direction .eq. DIRECTION_COMP2CPL) then
     do i = 1, arr_size_x
     do j = 1, arr_size_y
       ptr(i + (lowbnd_x-1), j + (lowbnd_y-1)) = ptr_comp1d(i + (j-1)*arr_size_x)
     end do 
     end do

     do i = 1, arr_size_x
     do j = 1, arr_size_y
       print *, "[", localpet,"] (", i, ", ", j, ") = ", ptr(i + (lowbnd_x-1), j + (lowbnd_y-1))
     end do 
     end do

  end if

  !NULLIFY(ptr)
  !NULLIFY(ptr_comp)
  print *, "[CPL_talk_COMP] Done"
!
  !if (debugLevel >= 1) then
  !  write (ofile, "(A7,I2.2,A3)") "pmslOCN", iLoop, ".nc"
  !  call ESMF_FieldWrite(field, trim(ofile), rc=rc)
  !end if

  end subroutine CPL_talk_COMP

end module mod_esmf_ocn
