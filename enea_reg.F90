!=======================================================================
! ENEA-REG COUPLER 
! Based on ESMF library
! Copyright (c) 2013-2025 Alessandro Anav, Ufuk Turuncoglu, Gianmaria Sannino
! Licensed under the MIT License. 
!=======================================================================

program enea_reg

  !-----------------------------------------------------------------------------
  ! Generic ESM application driver
  !-----------------------------------------------------------------------------

   use ESMF
   use NUOPC
   use ESM, only: esmSS => SetServices

   use esm_config
   use esm_shared, only : CheckErr

   implicit none
  
   character(len=*), parameter :: u_FILE_u = __FILE__

   integer                 :: rc, urc
   type(ESMF_GridComp)     :: esmComp
   type(ESMF_VM)           :: vm
   TYPE(ESMF_LogKind_flag) :: LogKindFlag
!
!-----------------------------------------------------------------------
!  Initialize ESMF coupling.
!-----------------------------------------------------------------------
!
!  It is optimal to have a single log file using ESMF_LOGKIND_SINGLE
!  combining messages from all PETs. However, it is not supported on
!  some platforms.  If this is the case, use ESMF_LOGKIND_MULTI to
!  create one log file per PET. This is kind of annoying. In
!  applications using s large number of processors, opening a large
!  number of log files and writing messages can be a bottleneck.
!
!  LogKindFlag=ESMF_LOGKIND_SINGLE
   LogKindFlag=ESMF_LOGKIND_MULTI

!-----------------------------------------------------------------------
!     Initialize ESMF framework
!-----------------------------------------------------------------------
!
  ! Initialize ESMF
   call ESMF_Initialize(logkindflag=LogKindFlag,                        &
            defaultCalkind=ESMF_CALKIND_GREGORIAN, vm=vm, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_LogWrite("STARTING ENEA-REG", ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------
!     Create component 
!-----------------------------------------------------------------------
!
   ! Create the earth system Component
   esmComp = ESMF_GridCompCreate(name="ENEA-REG", rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Read main configuration file 
!-----------------------------------------------------------------------
!
   call read_config(vm, rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!  
!-----------------------------------------------------------------------
!     Add additional fields to NUOPC field dictionary 
!-----------------------------------------------------------------------
!
! extend the NUOPC Field Dictionary to cover new "silly" Fields to/from RTM
   call NUOPC_FieldDictionaryAddEntry("field_to_rtm",                   &
                                      "sillyUnit", rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
     
   call NUOPC_FieldDictionaryAddEntry("field_from_rtm",                 &
                                      "sillyUnit2", rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Add additional fields to NUOPC field dictionary 
!-----------------------------------------------------------------------
!
   call set_field_dir(vm, rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
        
!-----------------------------------------------------------------------
!     Register component 
!-----------------------------------------------------------------------
!
   ! SetServices for the earth system Component
   call ESMF_GridCompSetServices(esmComp, esmSS, userRc=urc, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
   if (CheckErr(urc,__LINE__,u_FILE_u))                                 &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

!-----------------------------------------------------------------------
!     Initialize component 
!-----------------------------------------------------------------------
!    
   ! Call Initialize for the earth system Component
   call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
   if (CheckErr(urc,__LINE__,u_FILE_u))                                 &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Wait for finishing initialize phase
!-----------------------------------------------------------------------
!
   call ESMF_LogWrite("ENEA-REG IS INITIALIZED; READY TO RUN",          &
                       ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
   call ESMF_VMBarrier(vm, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Run component 
!-----------------------------------------------------------------------
!
   call ESMF_LogWrite("ENEA-REG: RUNNING", ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
   if (CheckErr(urc,__LINE__,u_FILE_u))                                 &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Finalize component 
!-----------------------------------------------------------------------
!
   call ESMF_LogWrite("ENEA-REG: FINALIZING SIMULATION",                &
                       ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
   if (CheckErr(urc,__LINE__,u_FILE_u))                                 &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
!          
!-----------------------------------------------------------------------
!     Destroy the earth system Component
!-----------------------------------------------------------------------
!
   call ESMF_LogWrite("ENEA-REG: DESTROYING COMPONENTS",                &
                       ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

   call ESMF_GridCompDestroy(esmComp, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Finalize ESMF framework 
!-----------------------------------------------------------------------
!
   call ESMF_LogWrite("ENEA-REG FINISHED", ESMF_LOGMSG_INFO, rc=rc)
   if (CheckErr(rc,__LINE__,u_FILE_u))                                  &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
   call ESMF_Finalize(rc=rc)
   
   print*,'ENEA-REG SIMULATION COMPLETED'
!
end program
