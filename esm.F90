!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

   use ESMF
   use NUOPC
   use NUOPC_Driver, &
             driverSS => SetServices

   use esm_types
   use esm_shared, only : CheckErr
   
   use ATM, only: atmSS => SetServices
   use OCN, only: ocnSS => SetServices
   use RTM, only: rtmSS => SetServices
   use esm_cpl, only: CPL_SetServices
!

   use NUOPC_Connector, only: cplSS => SetServices

   implicit none

   private

   ! Module specific variables
   character(len=*), parameter :: u_FILE_u = __FILE__

   public SetServices
  
   contains
!
!===============================================================================
!   
   subroutine SetServices(driver, rc)
      type(ESMF_GridComp)         :: driver
      integer, intent(out)        :: rc
    
      rc = ESMF_SUCCESS

!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      ! derive from NUOPC_Driver
      call NUOPC_CompDerive(driver, driverSS, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      ! specialize driver
      call NUOPC_CompSpecialize(driver,specLabel=label_SetModelServices,&
                                specRoutine=SetModelServices, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
    
      call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
                                specRoutine=SetRunSequence, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

!-----------------------------------------------------------------------
!     Set verbosity
!-----------------------------------------------------------------------
!
      ! set driver verbosity
      if (debugLevel > 4) then
         call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
      else if(debugLevel == 0) then
         call NUOPC_CompAttributeSet(driver, name="Verbosity", value="off", rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return  
      else
         call NUOPC_CompAttributeSet(driver, name="Verbosity", value="low", rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return             
      end if
   end subroutine
!
!===============================================================================
!
   subroutine SetModelServices(driver, rc)
      type(ESMF_GridComp)  :: driver
      integer, intent(out) :: rc

      ! local variables
      type(ESMF_GridComp)           :: child
      type(ESMF_CplComp)            :: connector
      integer                       :: petCount, localPet
      integer                       :: i, j
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get PETs info
!-----------------------------------------------------------------------
!
      ! get the petCount
      call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
      
      ! query the Component for its localPet
      call ESMF_GridCompGet(driver, localPet=localPet, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!      
!-----------------------------------------------------------------------
!     Set application clock 
!-----------------------------------------------------------------------
!
      restarted = .false.
      if (esmStartTime /= esmRestartTime) then
         restarted = .true.
      end if
!
      if (restarted) then
         esmClock = ESMF_ClockCreate(esmTimeStep,                       &
                                     esmRestartTime,                    &
                                     stopTime=esmStopTime,              &
                                     name='ESM_clock', rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return                            
      else
         esmClock = ESMF_ClockCreate(esmTimeStep,                       &
                                     esmStartTime,                      &
                                     stopTime=esmStopTime,              &
                                     name='ESM_clock', rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return                            
      end if

!
      call ESMF_GridCompSet(driver, clock=esmClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!     
!-----------------------------------------------------------------------
!     SetServices for model components 
!-----------------------------------------------------------------------
!
      ! SetServices for ATM
      call NUOPC_DriverAddComp(driver, "ATM", atmSS,                    &
                  petList=models(Iatmos)%petList(:), comp=child, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
       
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      ! SetServices for OCN
      call NUOPC_DriverAddComp(driver, "OCN", ocnSS,                    &
                   petList=models(Iocean)%petList(:), comp=child, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
       
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      ! SetServices for RTM
      call NUOPC_DriverAddComp(driver, "RTM", rtmSS,                    &
                   petList=models(Iriver)%petList(:), comp=child, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_CompAttributeSet(child, name="Verbosity", value="1", rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return      
!
!-----------------------------------------------------------------------
!     SetServices for connector components 
!-----------------------------------------------------------------------
!
      do i = 1, nModels
         do j = 1, nModels
            if (connectors(i,j)%modActive) then
               call NUOPC_DriverAddComp(driver,                         &
                           srcCompLabel=trim(models(i)%name),           &
                           dstCompLabel=trim(models(j)%name),           &
                           compSetServicesRoutine=CPL_SetServices,      &
                           comp=connector, rc=rc)
               if (CheckErr(rc,__LINE__,u_FILE_u)) return

               if (debugLevel > 0) then
                  call ESMF_AttributeSet(connector, name="Verbosity",   &
                                     value="high", rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
               end if
            end if
         end do
      end do
! 
   end subroutine
!
!===============================================================================
!
   subroutine SetRunSequence(driver, rc)
      type(ESMF_GridComp)  :: driver
      integer, intent(out) :: rc

      ! local variables
      character(ESMF_MAXSTR)              :: name
      type(ESMF_Time)                     :: startTime
      type(ESMF_Time)                     :: stopTime
      type(ESMF_TimeInterval)             :: timeStep
      type(ESMF_Clock)                    :: internalClock
!      
      rc = ESMF_SUCCESS
!    
      ! query the driver for its name
      call ESMF_GridCompGet(driver, name=name, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
      
      ! ingest FreeFormat run sequence
      call NUOPC_DriverIngestRunSequence(driver, runSeqFF,              &
                                   autoAddConnectors=.false., rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      ! create clock for fast ATM-OCN interaction
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
       
      call ESMF_ClockGet(internalClock, timeStep=timeStep,              &
                         startTime=startTime, stopTime=stopTime, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      name = trim(connectors(Iatmos,Iocean)%name)//'_clock'
!
      internalClock = ESMF_ClockCreate(name=trim(name),                 &
                                       timeStep=timeStep/int(24/ATM_dt),&
                                       startTime=startTime,             &
                                       stopTime=stopTime,               &
                                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
       
      ! install clock for ATM-OCN coupling in slot 2
      call NUOPC_DriverSetRunSequence(driver, slot=2, clock=internalClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
  end subroutine
!
!===============================================================================
!
end module
