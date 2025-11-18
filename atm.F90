!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2019 Ufuk Turuncoglu
! Licensed under the MIT License. 
!=======================================================================
!
!-----------------------------------------------------------------------
!     ATM gridded component code 
!-----------------------------------------------------------------------
!
module ATM
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
   use ESMF
   use NUOPC
   use NUOPC_Model,                                                     &
       modelSS                    => SetServices,                       &
       NUOPC_Label_CheckImport    => label_CheckImport
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
!     Register initialize routines (Phase 1 and Phase 2)  
!-----------------------------------------------------------------------
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p1"/),       &
                                   userRoutine=ATM_SetInitializeP1,     &
                                   rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSetEntryPoint(gcomp,                               &
                                   methodflag=ESMF_METHOD_INITIALIZE,   &
                                   phaseLabelList=(/"IPDv00p2"/),       &
                                   userRoutine=ATM_SetInitializeP2,     &
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
                                specRoutine=ATM_DataInit, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=label_SetClock,        &
                                specRoutine=ATM_SetClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(gcomp,                                  &
                                specLabel=NUOPC_Label_CheckImport,      &
                                specPhaseLabel="RunPhase1",             &
                                specRoutine=ATM_CheckImport, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance,   &
                                specRoutine=ATM_ModelAdvance, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Register finalize routine    
!-----------------------------------------------------------------------
! 
      call ESMF_GridCompSetEntryPoint(gcomp,                            &
                                      methodflag=ESMF_METHOD_FINALIZE,  &
                                      userRoutine=ATM_SetFinalize,      &
                                      rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
   end subroutine SetServices
!
!===============================================================================
!    
   subroutine ATM_SetInitializeP1(gcomp, importState, exportState,      &
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
      do i = 1, ubound(models(Iatmos)%importField, dim=1)
        call NUOPC_Advertise(importState,                               &
             StandardName=trim(models(Iatmos)%importField(i)%long_name),&
             name=trim(models(Iatmos)%importField(i)%short_name), rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do 
!
!-----------------------------------------------------------------------
!     Set export fields 
!-----------------------------------------------------------------------
!
      do i = 1, ubound(models(Iatmos)%exportField, dim=1)
        call NUOPC_Advertise(exportState,                               &
             StandardName=trim(models(Iatmos)%exportField(i)%long_name),&
             name=trim(models(Iatmos)%exportField(i)%short_name), rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
      end do
!
   end subroutine ATM_SetInitializeP1
!
!===============================================================================
!   
   subroutine ATM_SetInitializeP2(gcomp, importState, exportState,      &
                                     clock, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_wrf_top, only : wrf_init 
      use module_domain,  only : head_grid
!
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
       call wrf_set_dm_communicator(comm)
       call wrf_init()

! ALESS ( 
!-----------------------------------------------------------------------
!     Check if prec_acc_dt and mean_diag are used in WRF 
!-----------------------------------------------------------------------

      if (head_grid%prec_acc_dt .eq. 0) then
#if defined(__INTEL_COMPILER)         
         PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m'    
         PRINT*, '\033[31m prec_acc_dt in namelist.input must be > 0 \033[0m'
         PRINT*, '\033[31m *************************************************** \033[0m'
#else
         PRINT*, '**************** WRF ERROR ************************'    
         PRINT*, 'prec_acc_dt in namelist.input must be > 0'
         PRINT*, '***************************************************'
#endif         
         call ESMF_LogWrite("prec_acc_dt in namelist.input must be > 0", ESMF_LOGMSG_ERROR)
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if 

      if (head_grid%mean_diag .eq. 0) then
#if defined(__INTEL_COMPILER)         
         PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m'    
         PRINT*, '\033[31m mean_diag in namelist.input must be 1 \033[0m'
         PRINT*, '\033[31m *************************************************** \033[0m'  
#else
         PRINT*, '**************** WRF ERROR ************************'    
         PRINT*, 'mean_diag in namelist.input must be 1'
         PRINT*, '***************************************************'  
#endif             
         call ESMF_LogWrite("mean_diag in namelist.input must be 1", ESMF_LOGMSG_ERROR)
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if 

!-----------------------------------------------------------------------
!     Check if prec_acc_dt and mean_diag are equal to ATM-OCN coupling frequency 
!-----------------------------------------------------------------------

      ! Note that prec_acc_dt and mean_diag_interval are in minutes, while coupling frequency is in hours.
      ! So we multiply by 60 the coupling frequency

      ! Check if the accumulation period for precipitation is equal to ATM-OCN coupling frequency.
      ! If not, the simulation ends with an error message
      if ( head_grid%prec_acc_dt .ne. (60.*ATM_dt) ) then
#if defined(__INTEL_COMPILER)         
         PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m'    
         PRINT*, '\033[31m prec_acc_dt (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent \033[0m'
         PRINT*, '\033[31m *************************************************** \033[0m'   
#else
         PRINT*, '**************** WRF ERROR ************************'    
         PRINT*, 'prec_acc_dt (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent'
         PRINT*, '***************************************************'   
#endif               
         call ESMF_LogWrite("prec_acc_dt (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent", ESMF_LOGMSG_ERROR)
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if 

      ! Check if the averaging period is equal to ATM-OCN coupling frequency.
      ! If not, the simulation ends with an error message
      ! Unlike prec_acc_dt, mean_diag_interval is defined as an integer
      if ( head_grid%mean_diag_interval .ne. int(60.*ATM_dt) ) then
#if defined(__INTEL_COMPILER)      
         PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m'    
         PRINT*, '\033[31m mean_diag_interval (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent \033[0m'
         PRINT*, '\033[31m *************************************************** \033[0m'  
#else
         PRINT*, '**************** WRF ERROR ************************'    
         PRINT*, 'mean_diag_interval (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent'
         PRINT*, '***************************************************'  
#endif                    
         call ESMF_LogWrite("mean_diag_interval (namelist.input) and ATM-OCN coupling frequencies (namelist.rc) are not consistent", ESMF_LOGMSG_ERROR)
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if 

! ALESS )
!
!-----------------------------------------------------------------------
!     Set-up grid and load coordinate data
!-----------------------------------------------------------------------
!
      call ATM_SetGridArrays(gcomp, rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set-up fields and register to import/export states
!-----------------------------------------------------------------------
!
      call ATM_SetStates(gcomp, rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
   end subroutine ATM_SetInitializeP2
!
!===============================================================================
!   
   subroutine ATM_SetGridArrays(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_domain, only : find_grid_by_id
      use module_domain_type
      use module_drv, only : drv_allocate
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, pe,Im, Jm
      integer :: ids, ide, jds, jde, kds, kde,                          &
                 ims, ime, jms, jme, kms, kme,                          &
                 ips, ipe, jps, jpe, kps, kpe
      integer :: localPet, petCount, localDEcount, localDE 
      integer :: minIndex(2), maxIndex(2)
      integer :: cLB(2), cUB(2), eLB(2), eUB(2), tLB(2), tUB(2)
      integer :: LBi, UBi, LBj, UBj
      integer, allocatable :: ipatchStarts(:), jpatchStarts(:)
      integer, allocatable :: ipatchEnds(:), jpatchEnds(:)
      integer, allocatable :: deBlockList(:,:,:)
      real(ESMF_KIND_R8), pointer :: ptrX(:,:), ptrY(:,:), ptrA(:,:)
      integer(ESMF_KIND_I4), pointer :: ptrM(:,:)
      character(len=256) :: msg2
!
      type(ESMF_VM) :: vm
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_StaggerLoc) :: staggerLoc(2)
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
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get variables related with grid partitioning
!     ids, ide, jds, jde, kds, kde => domain extent
!     ims, ime, jms, jme, kms, kme => memory extent
!     ips, ipe, jps, jpe, kps, kpe => patch extent
!-----------------------------------------------------------------------
!
      call get_ijk_from_grid(head_grid, ids, ide, jds, jde, kds, kde,   &
                                        ims, ime, jms, jme, kms, kme,   &
                                        ips, ipe, jps, jpe, kps, kpe)
!
!-----------------------------------------------------------------------
!     Allocate import arrays in WRF side
!-----------------------------------------------------------------------
!
      call drv_allocate(head_grid)
!
!-----------------------------------------------------------------------
!     Calculate patchs and number of CPUs in each direction 
!-----------------------------------------------------------------------
!
      ! Starting patch
      if (.not. allocated(ipatchStarts)) then
        allocate(ipatchStarts(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchStarts)) then
        allocate(jpatchStarts(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm,                                        &
     &                       sendData=(/ips/),                          &
     &                       sendCount=1,                               &
     &                       recvData=IpatchStarts,                     &
     &                       recvCounts =(/(1, k=0, petCount-1)/),      &
     &                       recvOffsets=(/(k, k=0, petCount-1)/),      &
     &                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_VMAllGatherV(vm,                                        &
     &                       sendData=(/jps/),                          &
     &                       sendCount=1,                               &
     &                       recvData=JpatchStarts,                     &
     &                       recvCounts =(/(1, k=0, petCount-1)/),      &
     &                       recvOffsets=(/(k, k=0, petCount-1)/),      &
     &                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      ! Ending patch
      if (.not. allocated(ipatchEnds)) then
        allocate(ipatchEnds(0:petCount-1))
      end if
!
      if (.not. allocated(jpatchEnds)) then
        allocate(jpatchEnds(0:petCount-1))
      end if
!
      call ESMF_VMAllGatherV(vm,                                        &
     &                       sendData=(/min(ide-1,ipe)/),               &
     &                       sendCount=1,                               &
     &                       recvData=IpatchEnds,                       &
     &                       recvCounts =(/(1, k=0, petCount-1)/),      &
     &                       recvOffsets=(/(k, k=0, petCount-1)/),      &
     &                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
      call ESMF_VMAllGatherV(vm,                                        &
     &                       sendData=(/min(jde-1,jpe)/),               &
     &                       sendCount=1,                               &
     &                       recvData=JpatchEnds,                       &
     &                       recvCounts =(/(1, k=0, petCount-1)/),      &
     &                       recvOffsets=(/(k, k=0, petCount-1)/),      &
     &                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create ESMF DistGrid based on model domain decomposition
!-----------------------------------------------------------------------
!

      if (.not.allocated(deBlockList)) THEN
         allocate(deBlockList(2,2,petCount))
      end if

      do pe = 1, petCount
         deBlockList(1,1,pe) = IpatchStarts(pe-1)
         deBlockList(2,1,pe) = JpatchStarts(pe-1)
         deBlockList(1,2,pe) = IpatchEnds(pe-1)
         deBlockList(2,2,pe) = JpatchEnds(pe-1)
      end do

      Im = maxval(IpatchEnds)
      Jm = maxval(JpatchEnds)
      minIndex = (/1, 1/)
      maxIndex = (/Im, Jm/)
 
      distGrid = ESMF_DistGridCreate(minIndex=minIndex,                 &
                                     maxIndex=maxIndex,                 &
                                     deBlockList=deBlockList,           &
                                     rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Deallocate temporay arrays    
!-----------------------------------------------------------------------
!
      if (allocated(ipatchStarts)) then
        deallocate(ipatchStarts)
      end if
      if (allocated(jpatchStarts)) then
        deallocate(jpatchStarts)
      end if
      if (allocated(ipatchEnds)) then
        deallocate(ipatchEnds)
      end if
      if (allocated(jpatchEnds)) then
        deallocate(jpatchEnds)
      end if
      if (allocated(deBlockList)) then
        deallocate(deBlockList)
      end if
!
!-----------------------------------------------------------------------
!     Create component Grid
!-----------------------------------------------------------------------
!
      models(Iatmos)%grid = ESMF_GridCreate(distgrid=distGrid,          &
                                        coordSys=ESMF_COORDSYS_SPH_DEG, &
                                        coordTypeKind=ESMF_TYPEKIND_R8, &      
                                        indexflag=ESMF_INDEX_GLOBAL,    &
                                        name="atm_grid2d",              &
                                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
                             
      call ESMF_GridGet(models(Iatmos)%grid,                            &
     &                  localDECount=localDEcount,                      &
     &                  rc=rc)          
      if (CheckErr(rc,__LINE__,u_FILE_u)) return 
!                                                    
!-----------------------------------------------------------------------
!     Define component grid (dot and cross points)
!-----------------------------------------------------------------------
!
      if (.not. allocated(models(Iatmos)%mesh)) then
         allocate(models(Iatmos)%mesh(2))
         models(Iatmos)%mesh(1)%gtype = Icross
         models(Iatmos)%mesh(2)%gtype = Idot
      end if
!      
!-----------------------------------------------------------------------
!     Set mask value for land and ocean 
!-----------------------------------------------------------------------
!
      models(Iatmos)%isLand  = 1
      models(Iatmos)%isOcean = 0
!
!-----------------------------------------------------------------------
!     Allocate coordinates, loop over component grid location type 
!-----------------------------------------------------------------------
!
      staggerLoc = (/ ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER /)
      MESH_LOOP : do k = 1, size(staggerLoc, dim=1)
      
         ! allocate coordinates
         call ESMF_GridAddCoord(models(Iatmos)%grid,                    &
     			        staggerLoc=staggerLoc(k),                       &
     			        rc=rc)  
         if (CheckErr(rc,__LINE__,u_FILE_u)) return      
         
         ! Allocate items for masking and area only for centers  
         if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
            call ESMF_GridAddItem(models(Iatmos)%grid,                  &
     			          staggerLoc=staggerLoc(k),                     &
     			          itemflag=ESMF_GRIDITEM_MASK,                  &
     			          rc=rc)  
            if (CheckErr(rc,__LINE__,u_FILE_u)) return      
         
            ! Allocate items for grid area  
            call ESMF_GridAddItem(models(Iatmos)%grid,                  &
     			          staggerLoc=staggerLoc(k),                     &
     			          itemflag=ESMF_GRIDITEM_AREA,                  &
     			          rc=rc)  
            if (CheckErr(rc,__LINE__,u_FILE_u)) return  
         end if
                             
         ! get pointers and set coordinates for the grid
         do localDE = 0, localDEcount-1
         
            ! x coordinate                        
            call ESMF_GridGetCoord(models(Iatmos)%grid,                 &
                                   coordDim=1,                          &
                                   staggerLoc=staggerLoc(k),            &
                                   localDE=localDE,                     &
                                   farrayPtr=ptrX,                      &
                                   exclusiveLBound=eLB,                 &
                                   exclusiveUBound=eUB,                 &
                                   computationalLBound=cLB,             &
                                   computationalUBound=cUB,             &
                                   totalLBound=tLB,                     &
                                   totalUBound=tUB,                     &
                                   rc=rc)  
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
             
            ! y coordinate                        
            call ESMF_GridGetCoord(models(Iatmos)%grid,                 &
                                   coordDim=2,                          &
                                   staggerLoc=staggerLoc(k),            &
                                   localDE=localDE,                     &
                                   farrayPtr=ptrY,                      &
                                   exclusiveLBound=eLB,                 &
                                   exclusiveUBound=eUB,                 &
                                   computationalLBound=cLB,             &
                                   computationalUBound=cUB,             &
                                   totalLBound=tLB,                     &
                                   totalUBound=tUB,                     &
                                   rc=rc)  
            if (CheckErr(rc,__LINE__,u_FILE_u)) return   
                                        
            ! add mask and area only in cell centers
            if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
               ! mask   
               call ESMF_GridGetItem(models(Iatmos)%grid,               &
                                     itemflag=ESMF_GRIDITEM_MASK,       &
                                     staggerLoc=staggerLoc(k),          &
                                     localDE=localDE,                   &
                                     farrayPtr=ptrM,                    &
                                     rc=rc)  
               if (CheckErr(rc,__LINE__,u_FILE_u)) return
                             
               ! cell area  
               call ESMF_GridGetItem(models(Iatmos)%grid,               &
                                     itemflag=ESMF_GRIDITEM_AREA,       &
                                     staggerLoc=staggerLoc(k),          &
                                     localDE=localDE,                   &
                                     farrayPtr=ptrA,                    &
                                     rc=rc)  
               if (CheckErr(rc,__LINE__,u_FILE_u)) return                                                    
            end if
            
            !  Fill the pointers  
            LBi = lbound(ptrX, 1)
            UBi = ubound(ptrX, 1)
            LBj = lbound(ptrX, 2)
            UBj = ubound(ptrX, 2)  

            if (staggerLoc(k) == ESMF_STAGGERLOC_CENTER) then
               do j = LBj, UBj
                  do i = LBi, UBi
                     ptrX(i,j) = real(head_grid%xlong(i,j),    ESMF_KIND_R8)
                     ptrY(i,j) = real(head_grid%xlat(i,j),     ESMF_KIND_R8)
                     ptrM(i,j) = real(head_grid%landmask(i,j), ESMF_KIND_R8)
                     ptrA(i,j) = real(head_grid%dx/head_grid%msftx(i,j) * &
                                 head_grid%dy/head_grid%msfty(i,j), ESMF_KIND_R8)                     
                  end do
               end do
               write(msg2, fmt='(A,4I8,4L)') 'ATM Center - LBi, UBi, LBj, UBj =',   &
     &            LBi, UBi, LBj, UBj, head_grid%bdy_mask(1), head_grid%bdy_mask(2), &
     &            head_grid%bdy_mask(3), head_grid%bdy_mask(4)
               call ESMF_LogWrite(trim(msg2), ESMF_LOGMSG_INFO)
            else if (staggerLoc(k) == ESMF_STAGGERLOC_CORNER) then
               do j = LBj, UBj
                  do i = LBi, UBi
                     ptrX(i,j) = real(0.5*(head_grid%xlong_v(i-1,j)+head_grid%xlong_v(i,j)), ESMF_KIND_R8)
                     ptrY(i,j) = real(0.5*(head_grid%xlat_u(i,j-1)+head_grid%xlat_u(i,j)), ESMF_KIND_R8)
                  end do
               end do  
               if (head_grid%bdy_mask(1)) then
                  ptrX(LBi,:) = 2.0d0*ptrX(LBi+1,:)-ptrX(LBi+2,:)
                  ptrY(LBi,:) = 2.0d0*ptrY(LBi+1,:)-ptrY(LBi+2,:)
               end if
               if (head_grid%bdy_mask(2)) then
                  ptrX(UBi,:) = 2.0d0*ptrX(UBi-1,:)-ptrX(UBi-2,:)
                  ptrY(UBi,:) = 2.0d0*ptrY(UBi-1,:)-ptrY(UBi-2,:)
               end if
               if (head_grid%bdy_mask(3)) then
                  ptrX(:,LBj) = 2.0d0*ptrX(:,LBj+1)-ptrX(:,LBj+2)
                  ptrY(:,LBj) = 2.0d0*ptrY(:,LBj+1)-ptrY(:,LBj+2)
               end if
               if (head_grid%bdy_mask(4)) then
                  ptrX(:,UBj) = 2.0d0*ptrX(:,UBj-1)-ptrX(:,UBj-2)
                  ptrY(:,UBj) = 2.0d0*ptrY(:,UBj-1)-ptrY(:,UBj-2)
               end if   
               write(msg2, fmt='(A,4I8,4L)') 'ATM Corner - LBi, UBi, LBj, UBj =',   &
     &            LBi, UBi, LBj, UBj, head_grid%bdy_mask(1), head_grid%bdy_mask(2), &
     &            head_grid%bdy_mask(3), head_grid%bdy_mask(4)

            end if
            
            ! nullify pointers 
            if (associated(ptrX)) nullify (ptrX)
            if (associated(ptrY)) nullify (ptrY)
            if (associated(ptrM)) nullify (ptrM)    
            if (associated(ptrA)) nullify (ptrA)                                       
         end do                                              			
      end do MESH_LOOP
!
!-----------------------------------------------------------------------
!     Assign grid to gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSet(gcomp, grid=models(Iatmos)%grid, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return 
!
   end subroutine ATM_SetGridArrays
!
!===============================================================================
!   
   subroutine ATM_SetStates(gcomp, rc)
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
      integer :: item, k, localPet, petCount, itemCount, localDE, localDECount
      real*8, dimension(:,:), pointer :: ptr
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
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
      call ESMF_GridGet(models(Iatmos)%grid,                            &
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
!
      call ESMF_StateGet(exportState, itemNameList=itemNameList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create export fields 
!-----------------------------------------------------------------------
!
      ITEM_OUT: do item = 1, itemCount
         k = get_varid(models(Iatmos)%exportField, trim(itemNameList(item)))

         ! Set staggering type 
         if (models(Iatmos)%exportField(k)%gtype == Icross) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
         else
            staggerLoc = ESMF_STAGGERLOC_CORNER   
         end if

         ! Create field 
         field = ESMF_FieldCreate(models(Iatmos)%grid,                  &
                                  arraySpec,                            &
                                  staggerloc=staggerLoc,                &
                                  name=trim(itemNameList(item)),        &
                                  rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return


         ! Put data into state 
         do localDE = 0, localDECount-1

            ! Get pointer from field 
            call ESMF_FieldGet(field, localDe=localDE, farrayPtr=ptr, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Initialize pointer 
            ptr = MISSING_R8

            ! Nullify pointer to make sure that it does not
            ! point on a random part in the memory 
            if (associated(ptr)) then
               nullify(ptr)
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
!
      call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create import fields 
!-----------------------------------------------------------------------
!
      ITEM_IN: do item = 1, itemCount
         k = get_varid(models(Iatmos)%importField, trim(itemNameList(item)))
         
         ! Set staggering type 
         if (models(Iatmos)%importField(k)%gtype == Icross) then
            staggerLoc = ESMF_STAGGERLOC_CENTER
         else
            staggerLoc = ESMF_STAGGERLOC_CORNER   
         end if

         ! Create field 
         field = ESMF_FieldCreate(models(Iatmos)%grid,                  &
                                  arraySpec,                            &
                                  staggerloc=staggerLoc,                &
                                  name=trim(itemNameList(item)),        &
                                  rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Put data into state 
         do localDE = 0, localDECount-1

            ! Get pointer from field 
            call ESMF_FieldGet(field, localDe=localDE, farrayPtr=ptr, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! Initialize pointer 
            ptr = MISSING_R8

            ! Nullify pointer to make sure that it does
            ! not point on a random part in the memory 
            if (associated(ptr)) then
               nullify(ptr)
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
   end subroutine ATM_SetStates
!
!===============================================================================
!   
   subroutine ATM_DataInit(gcomp, rc)
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
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
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
      call ATM_Export(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return

   end subroutine ATM_DataInit
!
!===============================================================================
!   
   subroutine ATM_SetClock(gcomp, rc)

      use module_wrf_top , only : calendar

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
      integer :: ref_year,   str_year,   end_year
      integer :: ref_month,  str_month,  end_month
      integer :: ref_day,    str_day,    end_day
      integer :: ref_hour,   str_hour,   end_hour
      integer :: ref_minute, str_minute, end_minute
      integer :: ref_second, str_second, end_second
      integer :: localPET
!
      type(ESMF_Calendar) :: cal
      type(ESMF_Clock) :: cmpClock 
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime, currTime
      type(ESMF_Time) :: cmpRefTime, cmpStartTime, cmpStopTime
      character(ESMF_MAXSTR) :: str1, str2
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!  Querry the Virtual Machine (VM) parallel environmemt for the MPI
!  communicator handle and current node rank.
!-----------------------------------------------------------------------
!
      CALL ESMF_GridCompGet (gcomp,                                     &
     &                       localPet=localPET,                         &
     &                       rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create gridded component clock 
!-----------------------------------------------------------------------
!
      if (trim(calendar) == 'gregorian') then
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar),                  &
                                  rc=rc)
      else if (trim(calendar) == 'noleap') then
        cal = ESMF_CalendarCreate(ESMF_CALKIND_NOLEAP,                  &
                                  name=trim(calendar),                  &
                                  rc=rc)
      else
        cal = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN,               &
                                  name=trim(calendar),                  &
                                  rc=rc)
      end if
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set reference time
!     place head_grid%id for 1
!-----------------------------------------------------------------------
!
      call nl_get_simulation_start_year(1, ref_year)
      call nl_get_simulation_start_month(1, ref_month)
      call nl_get_simulation_start_day(1, ref_day)
      call nl_get_simulation_start_hour(1, ref_hour)
      call nl_get_simulation_start_minute(1, ref_minute)
      call nl_get_simulation_start_second(1, ref_second)
!
!      write(*, fmt="(A,6I5)") "REF TIME :: ", ref_year, ref_month, ref_day, ref_hour, ref_minute, ref_second
!
      call ESMF_TimeSet(cmpRefTime,                                     &
                        yy=ref_year,                                    &
                        mm=ref_month,                                   &
                        dd=ref_day,                                     &
                        h=ref_hour,                                     &
                        m=ref_minute,                                   &
                        s=ref_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set start time
!-----------------------------------------------------------------------
!
      call nl_get_start_year(1, str_year)
      call nl_get_start_month(1, str_month)
      call nl_get_start_day(1, str_day)
      call nl_get_start_hour(1, str_hour)
      call nl_get_start_minute(1, str_minute)
      call nl_get_start_second(1, str_second)
!
!      write(*, fmt="(A,6I5)") "STR TIME :: ", str_year, str_month, str_day, str_hour, str_minute, str_second
!
      call ESMF_TimeSet(cmpStartTime,                                   &
                        yy=str_year,                                    &
                        mm=str_month,                                   &
                        dd=str_day,                                     &
                        h=str_hour,                                     &
                        m=str_minute,                                   &
                        s=str_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Set stop time
!-----------------------------------------------------------------------
!
      call nl_get_end_year(1, end_year)
      call nl_get_end_month(1, end_month)
      call nl_get_end_day(1, end_day)
      call nl_get_end_hour(1, end_hour)
      call nl_get_end_minute(1, end_minute)
      call nl_get_end_second(1, end_second)
!
!      write(*, fmt="(A,6I5)") "END TIME :: ", end_year, end_month, end_day, end_hour, end_minute, end_second
!
      call ESMF_TimeSet(cmpStopTime,                                    &
                        yy=end_year,                                    &
                        mm=end_month,                                   &
                        dd=end_day,                                     &
                        h=end_hour,                                     &
                        m=end_minute,                                   &
                        s=end_second,                                   &
                        calendar=cal,                                   &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get component clock
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=cmpClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_ClockGet(cmpClock, timeStep=timeStep,                   &
                         currTime=currTime, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Compare driver time vs. component time
!-----------------------------------------------------------------------
!
      if (restarted) then
        startTime = esmRestartTime
      else
        startTime = esmStartTime
      end if
!
! ALESS (
      call ESMF_TimeGet(cmpStartTime,                                   &
                      timeString=str1, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
      
      call ESMF_TimeGet(startTime,                                      &
                      timeString=str2, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!      if (cmpStartTime /= startTime) then
       if (trim(str1) .ne. trim(str2)) then
          IF (localPET.eq.0) THEN
#if defined(__INTEL_COMPILER)              
             PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m' 
#else
             PRINT*, '**************** WRF ERROR ************************' 
#endif                 
             write(*,50) trim(str1), trim(str2)
#if defined(__INTEL_COMPILER)                  
             PRINT*, '\033[31m *************************************************** \033[0m'  
#else
             PRINT*, '***************************************************'  
#endif             
          END IF                 

!        call ESMF_TimePrint(cmpStartTime, options="string isofrac", rc=rc)
!        if (CheckErr(rc,__LINE__,u_FILE_u)) return

!        call ESMF_TimePrint(startTime, options="string isofrac", rc=rc)
!        if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and ATM start times do not match: '//             &
             'please check the config files')
        return
      end if
!
      call ESMF_TimeGet(cmpStopTime,                                    &
                      timeString=str1, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
      
      call ESMF_TimeGet(esmStopTime,                                    &
                      timeString=str2, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!      if (cmpStopTime /= esmStopTime) then
       if (trim(str1) .ne. trim(str2)) then
          IF (localPET.eq.0) THEN 
#if defined(__INTEL_COMPILER)              
             PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m' 
#else
             PRINT*, '**************** WRF ERROR ************************' 
#endif                 
             write(*,51) trim(str1), trim(str2)
#if defined(__INTEL_COMPILER)                  
             PRINT*, '\033[31m *************************************************** \033[0m'  
#else
             PRINT*, '***************************************************'  
#endif  
          END IF                

!       call ESMF_TimePrint(cmpStopTime, options="string", rc=rc)
!       if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!       call ESMF_TimePrint(esmStopTime, options="string", rc=rc)
!       if (CheckErr(rc,__LINE__,u_FILE_u)) return

        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and ATM stop times do not match: '//              &
             'please check the config files')
        return
      end if
! ALESS )
!
      if (cal /= esmCal) then
        call ESMF_CalendarPrint(cal, options="calkindflag", rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
        call ESMF_CalendarPrint(esmCal, options="calkindflag", rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
        call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc,              &
             msg='ESM and ATM calendars do not match: '//               &
             'please check the config files')
        return
      end if
!
!-----------------------------------------------------------------------
!     Modify component clock time step 
!-----------------------------------------------------------------------
!
      call ESMF_ClockSet(cmpClock, name='atm_clock',                    &
                         timeStep=timeStep/int(24/ATM_dt), rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
#if defined(__INTEL_COMPILER)  
 50   format('\033[31m ATM start time: ',A,' does not match ESM start time ',A, ' \033[0m')
 51   format('\033[31m ATM stop  time: ',A,' does not match ESM stop  time ',A, ' \033[0m')
#else 
50   format('ATM start time: ',A,' does not match ESM start time ',A)
51   format('ATM stop  time: ',A,' does not match ESM stop  time ',A)
#endif 
!
   end subroutine ATM_SetClock
!
!===============================================================================
!   
   subroutine ATM_CheckImport(gcomp, rc)
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
      integer :: item, itemCount, localPet, rsec
      logical :: atCorrectTime
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
!
      type(ESMF_VM) :: vm
      type(ESMF_Time)  :: currTimeCmp, currTimeDrv
      type(ESMF_Time)  :: strTimeCmp
      type(ESMF_TimeInterval) :: timeStepCmp, timeStepDrv
      type(ESMF_Clock) :: modelClock, driverClock
      type(ESMF_Field) :: field
      type(ESMF_State) :: importState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query component
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Query driver and model clocks
!-----------------------------------------------------------------------
!
      call NUOPC_ModelGet(gcomp, driverClock=driverClock,               &
                          modelClock=modelClock, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get current time and time step out of the clock
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(driverClock, currTime=currTimeDrv,             &
                         timeStep=timeStepDrv, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_ClockGet(modelClock, currTime=currTimeCmp,              &
                         startTime=strTimeCmp, timeStep=timeStepCmp,    &
                         rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Check import field or not? 
!-----------------------------------------------------------------------
!
      call ESMF_TimeIntervalGet(mod((currTimeCmp-strTimeCmp),           &
                                    esmTimeStep/int(ATM_dt)),           &
                                    s=rsec, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      if (rsec == 0) then
!
!-----------------------------------------------------------------------
!     Query component for its clock and importState
!-----------------------------------------------------------------------
!
         call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
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
!
         call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Check fields in the importState
!-----------------------------------------------------------------------
!
         if (itemCount > 0) then
!
            do item = 1, itemCount
!
               call ESMF_StateGet(importState, itemName=trim(itemNameList(item)),&
                         field=field, rc=rc)
               if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
               if (cplType == 1) then
                  atCorrectTime = NUOPC_IsAtTime(field, currTimeCmp, rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
                  call print_timestamp(field, currTimeCmp, localPet, "ATM", rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
               else
                  atCorrectTime = NUOPC_IsAtTime(field, currTimeCmp+timeStepCmp,&
                                       rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
                  call print_timestamp(field, currTimeCmp+timeStepCmp,  &
                             localPet, "ATM", rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
               end if
!
               if (.not. atCorrectTime) then
                  call ESMF_LogSetError(ESMF_RC_ARG_BAD,                &
                              msg="NUOPC INCOMPATIBILITY DETECTED: "//  &
                              "Import Fields not at correct time",      &
                              line=__LINE__, file=u_FILE_u,             &
                              rcToReturn=rc)
                  return
               end if
!
            end do
         end if
!
      end if
!
   end subroutine ATM_CheckImport
!
!===============================================================================
!   
   subroutine ATM_ModelAdvance(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid
      use module_wrf_top, only : wrf_run
      use MYESMF_TimeMod, only : MYESMF_Time
      use module_symbols_util, only : WRFU_TimeSet
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
      integer :: localPet, petCount, phase
      integer :: yy, mm, dd, h, m, s
      character(ESMF_MAXSTR) :: str1, str2
      character(len=160)          :: msgString
!
      type(MYESMF_Time) :: timeFrom2, timeTo2
!     
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: timeFrom1, timeTo1
      type(ESMF_Time) :: refTime, startTime, stopTime, currTime
      type(ESMF_State) :: importState, exportState
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Get gridded component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGet(gcomp, clock=clock, importState=importState,&
                            exportState=exportState, currentPhase=phase,&
                            vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get start, stop and current time and time step
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock, timeStep=timeStep, refTime=refTime,     &
                         startTime=startTime, stopTime=stopTime,        &
                         currTime=currTime, rc=rc) 
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Calculate run time
!-----------------------------------------------------------------------
!
      if (restarted .and. (currTime-esmRestartTime) < timeStep) then
        timeFrom1 = esmRestartTime
      else
        timeFrom1 = currTime
      end if
      timeTo1 = timeFrom1+timeStep
!
!-----------------------------------------------------------------------
!     Write time information to PET
!-----------------------------------------------------------------------
!     
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", unit=msgString, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
    
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
    
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Debug: write time information 
!-----------------------------------------------------------------------
!
      if (localPet == 0) then
        call ESMF_TimeGet(currTime,                                     &
                          timeStringISOFrac=str1, rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
        call ESMF_TimeGet(currTime+timeStep,                            &
                          timeStringISOFrac=str2, rc=rc)
        if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
#if defined(__INTEL_COMPILER)         
         PRINT*, '\033[32m ****************** ADVANCING ATM ****************** \033[0m' 
         write(*,50) trim(str1), trim(str2), phase
         PRINT*, '\033[32m *************************************************** \033[0m' 
#else
         PRINT*, '****************** ADVANCING ATM ******************' 
         write(*,50) trim(str1), trim(str2), phase
         PRINT*, '***************************************************' 
#endif         
!
      end if
!
!-----------------------------------------------------------------------
!     Convert ESMF_Time to MYESMF_Time
!     MYESMF_Time is the old version of ESMF_Time implemented in WRF
!-----------------------------------------------------------------------
!
      call ESMF_TimeGet(timeFrom1,                                      &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call WRFU_TimeSet(timeFrom2,                                      &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_TimeGet(timeTo1,                                        &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call WRFU_TimeSet(timeTo2,                                        &
                        yy=yy,                                          &
                        mm=mm,                                          &
                        dd=dd,                                          &
                        h=h,                                            &
                        m=m,                                            &
                        s=s,                                            &
                        rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get import fields 
!-----------------------------------------------------------------------
!
      call ATM_Import(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Run ATM component
!-----------------------------------------------------------------------
!
      head_grid%start_subtime = timeFrom2 
      head_grid%stop_subtime = timeTo2
!
      call wrf_run()
!
!-----------------------------------------------------------------------
!     Put export fields 
!-----------------------------------------------------------------------
!
      call ATM_Export(gcomp, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
#if defined(__INTEL_COMPILER)   
 50   format('\033[32m Running ATM Component: ',A,' --> ',A,' Phase: ',I1,' \033[0m')
#else
 50   format('Running ATM Component: ',A,' --> ',A,' Phase: ',I1)
#endif 
!
   end subroutine ATM_ModelAdvance
!
!===============================================================================
!   
   subroutine ATM_SetFinalize(gcomp, importState, exportState,          &
                                 clock, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_wrf_top, only : wrf_finalize
!
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
       CALL wrf_finalize( no_shutdown=.TRUE. )

   end subroutine ATM_SetFinalize
!
!===============================================================================
!   
   subroutine ATM_Import(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_drv, only : wrf_import
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
      integer :: IminP, ImaxP, JminP, JmaxP
      integer :: LBi, UBi, LBj, UBj
      integer :: item, i, j, id
      integer :: iyear, iday, imonth, ihour, iminute, isec
      integer :: localPet, petCount, itemCount, localDE, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)
      real(ESMF_KIND_R8) :: sfac, addo
      real(ESMF_KIND_R8), pointer :: ptr2d(:,:)
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
!     Query WRF grid object
!-----------------------------------------------------------------------
!
      IminP = head_grid%sp31
      ImaxP = head_grid%ep31
      JminP = head_grid%sp33
      JmaxP = head_grid%ep33
      if (head_grid%ed31 == ImaxP) ImaxP = ImaxP-1
      if (head_grid%ed33 == JmaxP) JmaxP = JmaxP-1
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
!     Get number of local DEs
!-----------------------------------------------------------------------
! 
      call ESMF_GridGet(models(Iatmos)%grid,                            &
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
!
         id = get_varid(models(Iatmos)%importField, itemNameList(item))

         ! Get field
         call ESMF_StateGet(importState, trim(itemNameList(item)),      &
                            field, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return


         ! Loop over decomposition elements (DEs) 
         DE_LOOP: do localDE = 0, localDECount-1

            ! Get pointer from field
            call ESMF_FieldGet(field, localDE=localDE, farrayPtr=ptr2d, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! size of the pointer
            LBi = lbound(ptr2d,1)
            UBi = ubound(ptr2d,1)
            LBj = lbound(ptr2d,2)
            UBj = ubound(ptr2d,2)

            ! Debug: write size of pointers    
            if (debugLevel > 3) then
               write(*,60) localPet, localDE, adjustl("PTR/ATM/IMP/"//itemNameList(item)),&
                  lbound(ptr2d, dim=1), ubound(ptr2d, dim=1),               &
                  lbound(ptr2d, dim=2), ubound(ptr2d, dim=2)
               write(*,60) localPet, localDE, adjustl("IND/ATM/IMP/"//itemNameList(item)),&
                  IminP, ImaxP, JminP, JmaxP 
            end if

            ! Set offset and scale factor 
            sfac = models(Iatmos)%importField(id)%scale_factor
            addo = models(Iatmos)%importField(id)%add_offset

            ! Put data to ATM component variable
            select case (trim(adjustl(itemNameList(item))))
            
               !     Import from OCN 
               ! ALESS ( 
               ! Change to remove the Atlantic buffer zone from coupling
               case ('sst')
                  do j = JminP, JmaxP
                     do i = IminP, ImaxP
                        if (ptr2d(i,j) < TOL_R8) then
                           wrf_import%sst(i,j) = (ptr2d(i,j)*sfac)+addo
                        else
                           wrf_import%sst(i,j) = MISSING_R8
                        end if
                     end do
                  end do
            end select
#if 0
               case ('sst')
                  do j = JminP, JmaxP
                     if (i > 96) then  ! 54 in case of MedCordex_15km and 96 for MedCordex_12km
                        do i = IminP, ImaxP
                           if (ptr2d(i,j) < TOL_R8) then
                              wrf_import%sst(i,j) = (ptr2d(i,j)*sfac)+addo
                           else
                              wrf_import%sst(i,j) = MISSING_R8
                           end if
                        end do
                     end if
                  end do
            end select
#endif               
! ALESS )
!

            ! Nullify pointer to make sure that it does not 
            ! point on a random part in the memory 
            if (associated(ptr2d)) then
               nullify(ptr2d)
            end if
!
         end do DE_LOOP

         ! Debug: write field in netCDF format    
         if (debugLevel == 3) then
            call ESMF_GridCompGet(gcomp, name=cname, clock=clock,       &
                            importState=importState, vm=vm, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,            &
                        dd=iday, h=ihour, m=iminute, s=isec, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            write(ofile,80) 'atm_import', trim(itemNameList(item)),     &
                             iyear, imonth, iday, ihour, iminute, isec
            call ESMF_FieldWrite(field, trim(ofile)//'.nc',             &
                             variableName='data', overwrite=.true.,     &
                             rc=rc)
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
 80   format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I2.2)
!
   end subroutine ATM_Import
!
!===============================================================================
!   
   subroutine ATM_Export(gcomp, rc)
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
      use module_domain, only : head_grid, get_ijk_from_grid
      use module_model_constants, only : stbolt
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
      integer :: LBi, UBi, LBj, UBj      
      integer :: i, j, k
      integer :: iyear, iday, imonth, ihour, iminute, isec
      integer :: petCount, localPet, item, itemCount, localDE, localDECount
      character(ESMF_MAXSTR) :: cname, ofile
      character(ESMF_MAXSTR), allocatable :: itemNameList(:)

      real(ESMF_KIND_R8), parameter :: eps=1.0e-10
      real(ESMF_KIND_R8), pointer :: ptr2d(:,:)
      integer(ESMF_KIND_I8) :: tstep
!
      type(ESMF_VM) :: vm
      type(ESMF_Clock) :: clock
      type(ESMF_Time) :: currTime
      type(ESMF_Field) :: field
      type(ESMF_State) :: exportState
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
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
! ALESS ( 
!-----------------------------------------------------------------------
!     Get current time
!-----------------------------------------------------------------------

      call ESMF_ClockGet(clock, currTime=currTime,                      &
                         advanceCount=tstep, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_TimeGet(currTime, yy=iyear, mm=imonth,                  &
                        dd=iday, h=ihour, m=iminute, s=isec, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
! ALESS )
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

      ITEM_LOOP: do item = 1, itemCount
!
         k = get_varid(models(Iatmos)%exportField, trim(itemNameList(item)))


         ! Check rank of the export field
         if (models(Iatmos)%exportField(k)%rank .ne. 2) then
#if defined(__INTEL_COMPILER)      
            PRINT*, '\033[31m **************** WRF ERROR ************************ \033[0m'    
            PRINT*, '\033[31m The exported variable is not 2D:  \033[0m', models(Iatmos)%exportField(k)%short_name
            PRINT*, '\033[31m *************************************************** \033[0m'  
#else
            PRINT*, '**************** WRF ERROR ************************'    
            PRINT*, ' The expored variable is not 2D: ',models(Iatmos)%exportField(k)%short_name
            PRINT*, '***************************************************'  
#endif                    
            call ESMF_LogWrite("The WRF expored variable is not 2D:", ESMF_LOGMSG_ERROR)
            call ESMF_Finalize(endflag=ESMF_END_ABORT)      
         end if

         ! Get number of local DEs
         call ESMF_GridGet(models(Iatmos)%grid,                         &
                           localDECount=localDECount, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Get field from export state 
         call ESMF_StateGet(exportState, trim(itemNameList(item)),      &
                            field, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return

         ! Loop over decomposition elements (DEs) 
         DE_LOOP: do localDE = 0, localDECount-1

            ! Get pointer from field 
            call ESMF_FieldGet(field, localDE=localDE, farrayPtr=ptr2d, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            ! size of the pointer
            LBi = lbound(ptr2d,1)
            UBi = ubound(ptr2d,1)
            LBj = lbound(ptr2d,2)
            UBj = ubound(ptr2d,2)
 
            ! Set initial value to missing 
            ptr2d = MISSING_R8

            ! Put data to export field 
            ! if (localPet == 0) print*, 'ALESS (WRF_Put) at ',iyear,imonth,iday,ihour,iminute,isec
            select case (trim(adjustl(itemNameList(item)))) 

               case ('psfc')
                  do j = LBj, UBj
                     do i = LBi, UBi
!                       ptr2d(i,j) = head_grid%psfc_mean(i,j)
                        ptr2d(i,j) = head_grid%pmsl_mean(i,j)
                     end do
                  end do

               case ('sflx') 
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = head_grid%qfx_mean(i,j) -          &
                                    (head_grid%PREC_ACC_C(i,j) + head_grid%PREC_ACC_NC(i,j)) / (head_grid%prec_acc_dt * 60.) ! 60 is used to convert minutes to seconds
                     end do
                  end do

               case ('nflz')
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = ( head_grid%glw_mean(i,j) - head_grid%lwupb_mean(i,j) ) -  &
                                       head_grid%lh_mean (i,j) - head_grid%hfx_mean(i,j)
                     end do
                  end do
               
               case ('swrd') 
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = head_grid%swdnb_mean(i,j) - head_grid%swupb_mean(i,j)
                     end do
                  end do

               case ('wndu')
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j)= head_grid%u10_mean(i,j)*head_grid%cosa(i,j)-        &
                                    head_grid%v10_mean(i,j)*head_grid%sina(i,j)
                     end do
                  end do
                     
               case ('wndv') 
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = head_grid%v10_mean(i,j)*head_grid%cosa(i,j)+        &
                                     head_grid%u10_mean(i,j)*head_grid%sina(i,j)
                     end do
                  end do

               case ('soff')
!                 if (localPet == 0) print*, 'ALESS, Sending surface runoff at',iyear,iday,ihour,iminute 
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = head_grid%SFCRUNOFF(i,j)
                     end do
                  end do

               case ('uoff') 
!                 if (localPet == 0) print*, 'ALESS, Sending sub-surface runoff at',iyear,iday,ihour,iminute 
                  do j = LBj, UBj
                     do i = LBi, UBi
                        ptr2d(i,j) = head_grid%UDRUNOFF(i,j)
                     end do
                  end do

            end select

            ! Nullify pointer to make sure that it does not
            ! point on a random part in the memory 
            if (associated(ptr2d)) then
               nullify(ptr2d)
            end if
!
         end do DE_LOOP

         ! Debug: write field in netCDF format    
         if (debugLevel == 3) then
            write(ofile,100) 'atm_export', trim(itemNameList(item)),    &
                         iyear, imonth, iday, ihour, iminute, isec
            call ESMF_FieldWrite(field, trim(ofile)//'.nc',&
                             variableName='data', overwrite=.true.,     &
                             rc=rc)
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
 90   format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I1)
 100  format(A10,'_',A,'_',                                             &
             I4,'-',I2.2,'-',I2.2,'_',I2.2,'_',I2.2,'_',I2.2)
!
   end subroutine ATM_Export
!
!===============================================================================
!   
   integer function locate(xx,x)
!
!-----------------------------------------------------------------------
!     Locate a value in a sorted array
!     https://github.com/astrofrog/fortranlib
!-----------------------------------------------------------------------
!
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      real, dimension(:), intent(in) :: xx
      real, intent(in) :: x
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: n,jl,jm,ju
      logical :: ascnd
!
      n=size(xx)
      ascnd = (xx(n) >= xx(1))
      jl=0
      ju=n+1
      do
         if (ju-jl <= 1) exit
         jm=(ju+jl)/2
         if (ascnd .eqv. (x >= xx(jm))) then
            jl=jm
         else
            ju=jm
         end if
      end do
!
      if (x == xx(1)) then
         locate = 1
      else if (x == xx(n)) then
         locate = n-1
      else if(ascnd.and. (x > xx(n) .or. x < xx(1))) then
         locate = -1
      else if(.not.ascnd.and. (x < xx(n) .or. x > xx(1))) then
         locate = -1
      else
         locate = jl
      end if
!
   end function locate
!
!===============================================================================
!   
end module
