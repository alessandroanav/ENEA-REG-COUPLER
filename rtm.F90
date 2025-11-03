!=======================================================================
! ENEA-REG COUPLER 
! Based on ESMF library
! Copyright (c) 2013-2025 Alessandro Anav, Ufuk Turuncoglu, Gianmaria Sannino
! Licensed under the MIT License. 
!=======================================================================
!
!-----------------------------------------------------------------------
!     RTM gridded component code 
!-----------------------------------------------------------------------
!
module RTM
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
   use ESMF
   use NUOPC
   use NUOPC_Model, &
          modelSS    => SetServices
!
   use esm_types
   use esm_shared
!
   implicit none
   private
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
   public :: SetServices
   ! Module specific variables
   character(len=*), parameter :: u_FILE_u = __FILE__      
!
   contains
!
!===============================================================================
!   
   subroutine SetServices(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register NUOPC generic routines    
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(gcomp, modelSS, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Register initialize routine (P 1/2) for specific implementation   
!-----------------------------------------------------------------------
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p1"/),       &
                                   userRoutine=RTM_SetInitializeP1,     &
                                   rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p2"/),       &
                                   userRoutine=RTM_SetInitializeP2,     &
                                   rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Attach phase independent specializing methods
!     Setting the slow and fast model clocks  
!-----------------------------------------------------------------------
! 
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=label_DataInitialize,         &
                                specRoutine=RTM_DataInit, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance,         &
                                specRoutine=RTM_ModelAdvance, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Register finalize routine    
!-----------------------------------------------------------------------
! 
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_FINALIZE,  &
                                      userRoutine=RTM_SetFinalize,      &
                                      rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
   end subroutine SetServices
!
!===============================================================================
!   
   subroutine RTM_SetInitializeP1(gcomp, importState, exportState,      &
                                     clock, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Set import fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iriver)%importField, dim=1)
         call NUOPC_Advertise(importState,                              &
             StandardName=trim(models(Iriver)%importField(i)%long_name),&
             name=trim(models(Iriver)%importField(i)%short_name), rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do 
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iriver)%exportField, dim=1)
         call NUOPC_Advertise(exportState,                              &
             StandardName=trim(models(Iriver)%exportField(i)%long_name),&
             name=trim(models(Iriver)%exportField(i)%short_name), rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do
!
   end subroutine RTM_SetInitializeP1
!
!===============================================================================
!   
   subroutine RTM_SetInitializeP2(gcomp, importState, exportState,      &
                                     clock, rc)

!     USE CMF_CTRL_MPI_MOD,        ONLY: CMF_MPI_INIT_ALESS
      USE CMF_DRV_CONTROL_MOD,     ONLY: CMF_DRV_INPUT, CMF_DRV_INIT, CMF_DRV_END
      USE YOS_CMF_INPUT,           ONLY: LRESTART 
      USE YOS_CMF_PROG,            ONLY: D2RIVOUT, D2FLDOUT
      USE YOS_CMF_DIAG,            ONLY: D2RIVOUT_AVG, D2FLDOUT_AVG 

      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: comm, localPet, petCount
      type(ESMF_VM) :: vm
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount,         &
                      mpiCommunicator=comm, rc=rc) 
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Initialize the gridded component 
!-----------------------------------------------------------------------
!
!      CALL CMF_MPI_INIT_ALESS(comm)     ! ALESS, make it working on a single cpu now
       CALL CMF_DRV_INPUT
       CALL CMF_DRV_INIT
       if (LRESTART) then 
          D2RIVOUT_AVG(:,:) = D2RIVOUT(:,:)
          D2FLDOUT_AVG(:,:) = D2FLDOUT(:,:)
       end if 
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data 
!-----------------------------------------------------------------------
!
      call RTM_SetGridArrays(gcomp, localPet, rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states
!-----------------------------------------------------------------------
!
      call RTM_SetStates(gcomp, rc)
!
   end subroutine RTM_SetInitializeP2
!
!===============================================================================
!   
   subroutine RTM_DataInit(gcomp, rc)

      USE CMF_DRV_ADVANCE_MOD
      USE CMF_CALC_DIAG_MOD,       ONLY: CMF_DIAG_RESET
      USE YOS_CMF_INPUT,           ONLY: LRESTART 

      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      real*8  :: dstart, dend
      integer :: istart, iend, phase, localPet, petCount
      character(ESMF_MAXSTR) :: cname, msgString, str1, str2
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_TimeInterval) :: timeStep, timeFrom, timeTo
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component clock
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!      
!-----------------------------------------------------------------------
!  Export initialization or restart fields.
!-----------------------------------------------------------------------
!
      call RTM_Export(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!      
   end subroutine RTM_DataInit
!
!===============================================================================
!   
   subroutine RTM_SetGridArrays(gcomp, localPet, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
!     dimention & time
      USE YOS_CMF_INPUT,      ONLY: NX,    NY
      USE YOS_CMF_MAP,        ONLY: D1LON, D1LAT, LSMASK, GRIDAREA
      USE PARKIND1,           ONLY: JPRM
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer :: localPet 
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, k, cpus_per_dim(2), dx, dy
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_StaggerLoc) :: staggerLoc(2)
      integer, pointer :: ptrM(:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      character (len=40) :: name

      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Compute model resolution
!-----------------------------------------------------------------------

      dx = abs(D1LON(1)-D1LON(2)) 
      dy = abs(D1LAT(1)-D1LAT(2)) 
            
!-----------------------------------------------------------------------
!     Create DistGrid based on model domain decomposition
!     Currently RTM only works on single node (sequential)
!-----------------------------------------------------------------------
!
      cpus_per_dim = 1
!
      distGrid = ESMF_DistGridCreate(minIndex=(/ 1, 1 /),               &
                                     maxIndex=(/ NX, NY /),             &
                                     regDecomp=cpus_per_dim,            &
                                     rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create ESMF Grid
!-----------------------------------------------------------------------
!
      models(Iriver)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                        gridEdgeLWidth=(/0,0/),         &
                                        gridEdgeUWidth=(/0,0/),         &
                                        coordSys=ESMF_COORDSYS_SPH_DEG, &
                                        coordTypeKind=ESMF_TYPEKIND_R8, &                                                 
                                        indexflag=ESMF_INDEX_GLOBAL,    &
                                        name="rtm_grid",                &
                                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iriver)%mesh)) then
         allocate(models(Iriver)%mesh(2))
         models(Iriver)%mesh(1)%gtype = Icross
         models(Iriver)%mesh(2)%gtype = Idot
      end if
!      
!-----------------------------------------------------------------------
!     Set mask value for land points 
!-----------------------------------------------------------------------
!
     models(Iriver)%isLand  = 0
     models(Iriver)%isOcean = 1
!     
!-----------------------------------------------------------------------
!     Allocate coordinates, loop over component grid location type 
!-----------------------------------------------------------------------
!
      staggerLoc = (/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /)
      MESH_LOOP : do k = 1, size(staggerLoc, dim=1)

         ! allocate coordinates
         call ESMF_GridAddCoord(models(Iriver)%grid,                    &
                             staggerLoc=staggerLoc(k), rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Allocate items for masking and area only for centers  
         if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
            call ESMF_GridAddItem(models(Iriver)%grid,                  &
                                  staggerLoc=staggerLoc(k),             &
                                  itemflag=ESMF_GRIDITEM_MASK,          &
                                  rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Allocate items for grid area  
            call ESMF_GridAddItem(models(Iriver)%grid,                  &
                                  staggerLoc=staggerLoc(k),             &
                                  itemflag=ESMF_GRIDITEM_AREA,          &
                                  rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
         endif
          
         ! get pointers and set coordinates for the grid
         ! x coordinate  
         call ESMF_GridGetCoord(models(Iriver)%grid,                    &
                                staggerLoc=staggerLoc(k),               &
                                coordDim=1,                             &
                                farrayPtr=ptrX,                         &
                                rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! y coordinate 
         call ESMF_GridGetCoord(models(Iriver)%grid,                    &
                                staggerLoc=staggerLoc(k),               &
                                coordDim=2,                             &
                                farrayPtr=ptrY,                         &
                                rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! add mask and area only in cell centers
         if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
            ! mask           
            call ESMF_GridGetItem (models(Iriver)%grid,                 &
                                   staggerLoc=staggerLoc(k),            &
                                   itemflag=ESMF_GRIDITEM_MASK,         &
                                   farrayPtr=ptrM,                      &
                                   rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! cell area
            call ESMF_GridGetItem (models(Iriver)%grid,                 &
                                   staggerLoc=staggerLoc(k),            &
                                   itemflag=ESMF_GRIDITEM_AREA,         &
                                   farrayPtr=ptrA,                      &
                                   rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Debug: write size of pointers    
            name = GRIDDES(models(Iriver)%mesh(1)%gtype)
            if (debugLevel > 3) then
               write(*,30) localPet, 1, adjustl("PTR/RTM/GRD/"//name),  &
                    lbound(ptrX, dim=1), ubound(ptrX, dim=1),           &
                    lbound(ptrX, dim=2), ubound(ptrX, dim=2)
            end if            
         end if

         ! Fill the pointers    
         if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
            do i = 1, NY
               ptrX(:,i) = D1LON(:)
            end do
            do i = 1, NX
               ptrY(i,:) = D1LAT(:)
            end do
            ptrM(:,:) = LSMASK(:,:)
            ptrA(:,:) = GRIDAREA(:,:)
         else if (staggerLoc(k) == ESMF_STAGGERLOC_CORNER) then
            do i = 1, NY
               ptrX(:,i) = D1LON(:) - 0.5 * dx
            end do
            do i = 1, NX
               ptrY(i,:) = D1LAT(:) - 0.5 * dy
            end do          
         end if 
           
         ! nullify pointers 
         if (associated(ptrX)) nullify (ptrX)
         if (associated(ptrY)) nullify (ptrY)
         if (associated(ptrM)) nullify (ptrM)    
         if (associated(ptrA)) nullify (ptrA)   
      end do MESH_LOOP
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSet(gcomp, grid=models(Iriver)%grid, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 30   format(" PET(",I3.3,") - DE(",I2.2,") - ", A20, " : ", 4I8)
!
   end subroutine RTM_SetGridArrays
!
!===============================================================================
!   
   subroutine RTM_SetStates(gcomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: k, localPet, petCount, itemCount, localDECount
      integer :: item, localDE
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Field) :: field
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc 
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, importState=importState,             &
                            exportState=exportState, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set array descriptor
!-----------------------------------------------------------------------
!
      call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8,      &
                             rank=2, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iriver)%grid,                            &
                        localDECount=localDECount,                      &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get list of export fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      if (.not. allocated(itemNameList)) then
         allocate(itemNameList(itemCount))
      end if
      call ESMF_StateGet(exportState, itemNameList=itemNameList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create export fields 
!-----------------------------------------------------------------------
!
      ITEM_OUT: do item = 1, itemCount
         k = get_varid(models(Iriver)%exportField, trim(itemNameList(item)))

         ! Set staggering type 
         if (models(Iriver)%exportField(k)%gtype == Icross) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
         else
            staggerLoc = ESMF_STAGGERLOC_CORNER   
         end if
 
         ! Create field 
         field = ESMF_FieldCreate(models(Iriver)%grid,                  &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(item)),           &
                               rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Put data into state 
         do localDE = 0, localDECount-1

            ! Get pointer from field 
            call ESMF_FieldGet(field, localDe=localDE, farrayPtr=ptr2d, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Initialize pointer 
            ptr2d = MISSING_R8

            ! Nullify pointer to make sure that it does not point 
            ! on a random part in the memory 
            if (associated(ptr2d)) then
               nullify(ptr2d)
            end if
         end do

         ! Add field export state
         call NUOPC_Realize(exportState, field=field, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do ITEM_OUT
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList) 
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      if (.not. allocated(itemNameList)) then
         allocate(itemNameList(itemCount))
      end if
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create import fields 
!-----------------------------------------------------------------------
!
      ITEM_IN: do item = 1, itemCount
         k = get_varid(models(Iriver)%importField, trim(itemNameList(item)))

         ! Set staggering type 
         if (models(Iriver)%importField(k)%gtype == Icross) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
         else
            staggerLoc = ESMF_STAGGERLOC_CORNER   
         end if

         ! Create field 
         field = ESMF_FieldCreate(models(Iriver)%grid,                  &
                               arraySpec,                               &
                               staggerloc=staggerLoc,                   &
                               name=trim(itemNameList(item)),           &
                               rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Put data into state 
         do localDE = 0, localDECount-1

            ! Get pointer from field 
            call ESMF_FieldGet(field, localDe=localDE, farrayPtr=ptr2d, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
            
            ! Initialize pointer 
            ptr2d = MISSING_R8
            
            ! Nullify pointer to make sure that it does not point 
            ! on a random part in the memory 
            if (associated(ptr2d)) then
               nullify(ptr2d)
            end if
         end do
      
         ! Add field import state
         call NUOPC_Realize(importState, field=field, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do ITEM_IN
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
!
   end subroutine RTM_SetStates
!
!===============================================================================
!   
   subroutine RTM_ModelAdvance(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      USE CMF_DRV_ADVANCE_MOD
      USE CMF_CALC_DIAG_MOD,       ONLY: CMF_DIAG_RESET
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      real*8  :: dstart, dend 
      integer :: istart, iend, phase, localPet, petCount
      character(ESMF_MAXSTR) :: cname, msgString, str1, str2
      character(len=160)     :: msgString1      
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep, timeFrom, timeTo
      type(ESMF_Time) :: startTime, stopTime, currTime, refTime
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock,             &
                            currentPhase=phase, importState=importState,&
                            exportState=exportState, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get start, stop and current time and time step
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, timeStep=timeStep,                      &
                         startTime=startTime, stopTime=stopTime,        &
                         currTime=currTime, refTime=refTime, rc=rc) 
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Calculate run time
!-----------------------------------------------------------------------
!
      timeFrom = currTime-esmStartTime
!
      call ESMF_TimeIntervalGet(timeFrom, d_r8=dstart, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      timeTo = timeFrom+timeStep
      call ESMF_TimeIntervalGet(timeTo, d_r8=dend, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      if (ceiling(dstart) == floor(dstart)) then
         istart = int(dstart)+1 
      else
         write(msgString,'(A,I3)') trim(cname)//                        &
              ': time step of the CaMa model must be defined '//        &
              'as day increments. check istart!' 
         call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
         return
      end if
!
      if (ceiling(dend) == floor(dend)) then
         iend = int(dend)
      else
         write(msgString,'(A,I3)') trim(cname)//                        &
              ': time step of the CaMa model must be defined '//        &
              'as day increments. check iend!'
         call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
         return
      end if
!      
!-----------------------------------------------------------------------
!     Write time information to PET
!-----------------------------------------------------------------------
!     
    call ESMF_ClockPrint(clock, options="currTime", &
         preString="------>Advancing RTM from: ", unit=msgString1, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
    
    call ESMF_LogWrite(msgString1, ESMF_LOGMSG_INFO, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockPrint(clock, options="stopTime", &
         preString="---------------------> to: ", unit=msgString1, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
    
    call ESMF_LogWrite(msgString1, ESMF_LOGMSG_INFO, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Debug: write time information 
!-----------------------------------------------------------------------
!
      if (localPet == 0) then
         call ESMF_TimeGet(currTime,                                    &
                           timeStringISOFrac=str1, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
         call ESMF_TimeGet(currTime+timeStep,                           &
                           timeStringISOFrac=str2, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
#if defined(__INTEL_COMPILER)         
         PRINT*, '\033[36m ****************** ADVANCING RTM ****************** \033[0m' 
         write(*,50) trim(str1), trim(str2), phase, istart, iend
         PRINT*, '\033[36m *************************************************** \033[0m' 
#else
         PRINT*, '****************** ADVANCING RTM ******************' 
         write(*,50) trim(str1), trim(str2), phase, istart, iend
         PRINT*, '***************************************************' 
#endif           
      end if
!
!-----------------------------------------------------------------------
!     Get import fields 
!-----------------------------------------------------------------------
!
      call RTM_Import(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return      
!
!-----------------------------------------------------------------------
!     Run RTM component
!-----------------------------------------------------------------------
!
      CALL CMF_DRV_ADVANCE(iend-istart+1) 
!
!-----------------------------------------------------------------------
!     Put export fields
!-----------------------------------------------------------------------
!
      call RTM_Export(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      !*** reset CaMa variables
      CALL CMF_DIAG_RESET
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
#if defined(__INTEL_COMPILER) 
 50   format('\033[36m Running RTM Component: ',A,' --> ',A,' Phase: ',I1,      &
             ' [',I5,'-',I5, ']',' \033[0m') 
#else             
 50   format(' Running RTM Component: ',A,' --> ',A,' Phase: ',I1,      &
             ' [',I5,'-',I5, ']')
#endif
!
   end subroutine RTM_ModelAdvance
!
!===============================================================================
!   
   subroutine RTM_SetFinalize(gcomp, importState, exportState,       &
                                 clock, rc)

      USE CMF_DRV_CONTROL_MOD,     ONLY: CMF_DRV_END
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!          
!-----------------------------------------------------------------------
!     Call model finalize routines
!-----------------------------------------------------------------------
!
      CALL CMF_DRV_END
!
   end subroutine RTM_SetFinalize
!
!===============================================================================
!   
   subroutine RTM_Import(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      USE YOS_CMF_INPUT,        ONLY: NX,    NY
      USE YOS_CMF_PROG,         ONLY: D2RUNOFF,D2ROFSUB
      USE PARKIND1,             ONLY: JPRB
      USE CMF_CTRL_FORCING_MOD
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: id, n, m
      integer :: iyear, iday, imonth, ihour
      integer :: localPet, petCount, itemCount, localDECount,localDE,item
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8) :: sfac, addo
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
! ALESS (
      real(ESMF_KIND_R8), save, allocatable :: runoff_stored(:,:), drainage_stored(:,:)
      logical, save :: firsttime_soff = .true. 
      logical, save :: firsttime_uoff = .true. 
      REAL(KIND=JPRB)      :: buff_runoff(NX, NY), buff_drainage(NX, NY)
! ALESS )
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_Field) :: field
      type(ESMF_State) :: importState
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock,             &
                            importState=importState, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get current time 
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iriver)%grid,                            &
                        localDECount=localDECount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get list of import fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(importState, itemCount=itemCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      if (.not. allocated(itemNameList)) then
         allocate(itemNameList(itemCount))
      end if
      if (.not. allocated(itemTypeList)) then
         allocate(itemTypeList(itemCount))
      end if
      call ESMF_StateGet(importState, itemNameList=itemNameList,        &
                         itemTypeList=itemTypeList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Loop over excahange fields 
!-----------------------------------------------------------------------
!
      ITEM_LOOP: do item = 1, itemCount

         id = get_varid(models(Iriver)%importField, itemNameList(item))

         ! Get field
         call ESMF_StateGet(importState, trim(itemNameList(item)),      &
                            field, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Loop over decomposition elements (DEs) 
         DE_LOOP: do localDE = 0, localDECount-1

            ! Get pointer /from field
            call ESMF_FieldGet(field, localDE=localDE, farrayPtr=ptr, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Debug: write size of pointers    
            if (debugLevel > 3) then
               write(*,60) localPet, localDE, adjustl("PTR/RTM/IMP/"//itemNameList(item)),&
                  lbound(ptr, dim=1), ubound(ptr, dim=1),               &
                  lbound(ptr, dim=2), ubound(ptr, dim=2)
               write(*,60) localPet, localDE, adjustl("IND/RTM/IMP/"//itemNameList(item)),&
                  1, NX, 1, NY
            end if

            ! Allocate array to store runoff of the day before 
            if (.not. allocated(runoff_stored)) then
               allocate(runoff_stored(NX, NY))
               runoff_stored(:,:) = .0 
            end if
            if (.not. allocated(drainage_stored)) then
               allocate(drainage_stored(NX, NY))
               drainage_stored(:,:) = .0
            end if

            ! Set offset and scale factor
            sfac = models(Iriver)%importField(id)%scale_factor
            addo = models(Iriver)%importField(id)%add_offset
            
            ! Put data to ATM component variable
            select case (trim(adjustl(itemNameList(item))))

               case ('rnof')
                  do m = 1, NY
                     do n = 1, NX
                        if (ptr(n,m) < 0.0d0 .or. ptr(n,m) > 1.0d0) then
                           buff_runoff(n,m) = 0.0d0
                        else
                           buff_runoff(n,m) = (ptr(n,m)*sfac)+addo
                        end if
                     end do
                  end do 

               case ('snof')
                  do m = 1, NY
                     do n = 1, NX
                        if (ptr(n,m) < 0.0d0 .or. ptr(n,m) > 1.0d0) then
                           buff_drainage(n,m) = 0.0d0
                        else
                           buff_drainage(n,m) = (ptr(n,m)*sfac)+addo
                        end if
                     end do
                  end do 

               case ('soff')
                  if (firsttime_soff) then
                    !print*, 'ALESS, Getting surface runoff (first time) at year, day, hour: ',iyear,iday,ihour 
                     do m = 1, NY
                        do n = 1, NX
                           buff_runoff(n,m)   = ( (ptr(n,m)*sfac)+addo) / 86400. ! Convert mm cumulated into one day to mm/s
                           runoff_stored(n,m) = ptr(n,m)
                           if (buff_runoff(n,m) < 0.0d0)   buff_runoff(n,m)   = 0.0d0 
                           if (runoff_stored(n,m) < 0.0d0) runoff_stored(n,m) = 0.0d0 
                        end do
                     end do
                     firsttime_soff = .false.
                  else 
                    !print*, 'ALESS, Getting surface runoff at year, day, hour: ',iyear,iday,ihour 
                     do m = 1, NY
                        do n = 1, NX 
                           buff_runoff(n,m)   = (((ptr(n,m) - runoff_stored(n,m)) *sfac)+addo) / 86400. ! Decumulate and convert mm cumulated into one day to m/s 
                           runoff_stored(n,m) = ptr(n,m)
                           if (buff_runoff(n,m) < 0.0d0)   buff_runoff(n,m)   = 0.0d0 
                           if (runoff_stored(n,m) < 0.0d0) runoff_stored(n,m) = 0.0d0 
                        end do
                     end do
                  end if
!-
               case ('uoff')
                  if (firsttime_uoff) then
                    !print*, 'ALESS, Getting drainage (first time) at year, day, hour: ',iyear,iday,ihour 
                     do m = 1, NY
                        do n = 1, NX 
                           buff_drainage(n,m)           = ( (ptr(n,m)*sfac)+addo) / 86400. ! Convert mm cumulated into one day to mm/s
                           drainage_stored(n,m) = ptr(n,m)
                           if (buff_drainage(n,m) < 0.0d0)   buff_drainage(n,m)   = 0.0d0 
                           if (drainage_stored(n,m) < 0.0d0) drainage_stored(n,m) = 0.0d0 
                        end do
                     end do
                     firsttime_uoff = .false.
                  else 
                     !print*, 'ALESS, Getting drainage at year, day, hour: ',iyear,iday,ihour 
                     do m = 1, NY
                        do n = 1, NX 
                           buff_drainage(n,m)           = (((ptr(n,m) - drainage_stored(n,m)) *sfac)+addo) / 86400. ! Decumulate and convert mm cumulated into one day to m/s 
                           drainage_stored(n,m) = ptr(n,m)
                           if (buff_drainage(n,m) < 0.0d0)   buff_drainage(n,m)   = 0.0d0 
                           if (drainage_stored(n,m) < 0.0d0) drainage_stored(n,m) = 0.0d0
                        end do
                     end do
                  end if

               ! Here I should have runoff and drainage already interpolated on CaMa grid. Thus, I use the CaMa function to use runoff data to CaMa-Flood 
               ! use runoff data without any interporlation. map resolution & runoff resolution should be same 
               CALL CONV_RESOL_NEW(buff_runoff,   D2RUNOFF) 
               CALL CONV_RESOL_NEW(buff_drainage, D2ROFSUB)  
!
            end select
            
            ! Nullify pointer to make sure that it does not point 
            ! on a random part in the memory 
            if (associated(ptr)) then
               nullify(ptr)
            end if
!
         end do DE_LOOP
         
         ! Debug: write field in netCDF format    
         if (debugLevel == 3) then
            write(ofile,80) 'rtm_import', trim(itemNameList(item)),     &
                            iyear, imonth, iday, ihour, localPet
            call ESMF_FieldWrite(field, trim(ofile)//'.nc', overwrite=.true., rc=rc) 
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
         end if
!
      end do ITEM_LOOP
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
      if (allocated(itemTypeList)) deallocate(itemTypeList)
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 60   format(' PET(',I3,') - DE(',I2,') - ', A20, ' : ', 4I8)
 70   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 80   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2)
!
   end subroutine RTM_Import
!
!===============================================================================
!   
   subroutine RTM_Export(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      USE YOS_CMF_INPUT,      ONLY: NX,NY
      USE PARKIND1,           ONLY: JPRM
      USE CMF_UTILS_MOD,      ONLY: vecD2mapR
      USE YOS_CMF_DIAG,       ONLY: D2RIVOUT_AVG, D2FLDOUT_AVG
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp) :: gcomp
      integer, intent(out) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: m, n
      integer :: iyear, iday, imonth, ihour
      integer :: petCount, localPet, itemCount, localDECount,localDE,item
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_Field) :: field
      type(ESMF_State) :: exportState
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)

      ! Local variables
      REAL(KIND=JPRM)             :: R2OUT_RIVOUT(NX,NY), R2OUT_FLDOUT(NX,NY) 
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, name=cname, clock=clock,             &
                            exportState=exportState, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get current time 
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iriver)%grid,                            &
                        localDECount=localDECount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get list of export fields 
!-----------------------------------------------------------------------
!
      call ESMF_StateGet(exportState, itemCount=itemCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return 
!
      if (.not. allocated(itemNameList)) then
         allocate(itemNameList(itemCount))
      end if
      if (.not. allocated(itemTypeList)) then
         allocate(itemTypeList(itemCount))
      end if
      call ESMF_StateGet(exportState, itemNameList=itemNameList,        &
                         itemTypeList=itemTypeList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Loop over export fields 
!-----------------------------------------------------------------------
!
      ITEM_LOOP: do item = 1, itemCount
         
         ! Get export field 
         call ESMF_StateGet(exportState, trim(itemNameList(item)),      &
                         field, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Loop over decomposition elements (DEs) 
         DE_LOOP: do localDE = 0, localDECount-1

            ! Get pointer 
            call ESMF_FieldGet(field, localDE=localDE, farrayPtr=ptr, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Set initial value to missing 
            ptr = MISSING_R8
 
            ! convert CaMa 1Dvector to 2Dmap
            CALL vecD2mapR(D2RIVOUT_AVG, R2OUT_RIVOUT)             !! average river discharge
            CALL vecD2mapR(D2FLDOUT_AVG, R2OUT_FLDOUT)             !! average floodplain discharge

            select case (trim(adjustl(itemNameList(item)))) 
               case ('rdis')
                  do m = 1, NY
                     do n = 1, NX
                        ptr(n,m) =  R2OUT_RIVOUT(n,m) + R2OUT_FLDOUT(n,m)
                        if (ptr(n,m) < 0.0d0) ptr(n,m)           = 0.0d0
                     end do
                  end do
            end select
            
            ! Nullify pointer to make sure that it does not point 
            ! on a random part in the memory 
            if (associated(ptr)) then
               nullify(ptr)
            end if
         end do DE_LOOP

         ! Sets the TimeStamp Attribute according to clock
         ! on all the Fields in export state 
         call NUOPC_SetTimestamp(exportState, clock, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
         
         ! Debug: write field in netCDF format   
         if (debugLevel == 3) then
            write(ofile,90) 'rtm_export', trim(itemNameList(item)),     &
                        iyear, imonth, iday, ihour, localPet
            call ESMF_FieldWrite(field, trim(ofile)//'.nc', overwrite=.true., rc=rc) 
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
         end if
!
      end do ITEM_LOOP
!
!-----------------------------------------------------------------------
!     Deallocate arrays    
!-----------------------------------------------------------------------
!
      if (allocated(itemNameList)) deallocate(itemNameList)
      if (allocated(itemTypeList)) deallocate(itemTypeList)
!
!-----------------------------------------------------------------------
!     Format definition 
!-----------------------------------------------------------------------
!
 80   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 90   format(A10,'_',A,'_',I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2)
!
   end subroutine RTM_Export
!
!===============================================================================
!   
end module RTM
