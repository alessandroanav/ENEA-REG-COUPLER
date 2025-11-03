!=======================================================================
! ENEA-REG COUPLER 
! Based on ESMF library
! Copyright (c) 2013-2025 Alessandro Anav, Ufuk Turuncoglu, Gianmaria Sannino
! Licensed under the MIT License. 
!=======================================================================
!
!-----------------------------------------------------------------------
!     CPL gridded component code 
!-----------------------------------------------------------------------
!
module esm_cpl
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
   use ESMF
   use NUOPC
   use NUOPC_Connector, only :                                          &
       NUOPC_SetServices     => SetServices,                            &
       NUOPC_Label_ComputeRH => label_ComputeRouteHandle,               &
       NUOPC_Label_ExecuteRH => label_ExecuteRouteHandle,               &
       NUOPC_Label_ReleaseRH => label_ReleaseRouteHandle,               &
       NUOPC_ConnectorGet, NUOPC_ConnectorSet
!
   use esm_types
   use esm_utils
   use esm_shared,      only : CheckErr

!
   implicit none
   
   private

   ! Module specific variables
   character(len=*), PRIVATE, parameter :: u_FILE_u = __FILE__
!
!-----------------------------------------------------------------------
!     Public subroutines 
!-----------------------------------------------------------------------
!
   public :: CPL_SetServices   
!
   contains
!
!===============================================================================
!   
   subroutine CPL_SetServices(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Register generic methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompDerive(ccomp, NUOPC_SetServices, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Attach specializing methods 
!-----------------------------------------------------------------------
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ComputeRH, &
                                specRoutine=CPL_ComputeRH, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ExecuteRH, &
                                specRoutine=CPL_ExecuteRH, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call NUOPC_CompSpecialize(ccomp, specLabel=NUOPC_Label_ReleaseRH, &
                                specRoutine=CPL_ReleaseRH, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
   end subroutine CPL_SetServices
!
!===============================================================================
!   
   subroutine CPL_ComputeRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      logical :: rh1Exist, rh2Exist
      integer :: i, j, localPet, petCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, etSrc, etDst, grSrc, grDst
      integer :: srcCount, dstCount, itemCount, srcTermProcessing
      integer :: srcMaskVal, dstMaskVal
      character(ESMF_MAXSTR) :: cname, fname, rname, msgString
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      integer (ESMF_KIND_I4) :: LandValue(1), SeaValue(1)
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_Grid) :: srcGrid, dstGrid
      type(ESMF_StaggerLoc) :: srcSLoc, dstSLoc
      type(ESMF_UnmappedAction_Flag) :: unmap
      type(ESMF_RegridMethod_Flag) :: regridMethod
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field) :: srcField, dstField
      type(ESMF_FieldBundle) :: dstFields, srcFields
      type(ESMF_FieldStatus_Flag)    :: FieldStatus
      type(ESMF_ExtrapMethod_Flag)   :: extrapMethod
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
      !  Query the coupler for the Virtual Machine (VM) parallel environmemt.
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      !  Get current parallel node rank and number of nodes.
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      !  Set source and destination couple model indices.
      do i = 1, nModels
         do j = 1, nModels
            if (connectors(i,j)%modActive .and.                         &
              trim(connectors(i,j)%name) == trim(cname)) then       
               iSrc = i
               iDst = j
            end if
         end do
      end do
!
!-----------------------------------------------------------------------
!  Exchange land-sea mask flag.
!-----------------------------------------------------------------------
!
      LandValue(1)=models(iSrc)%isLand
      CALL ESMF_VMBroadcast (vm,                                        &
     &                       bcstData=LandValue,                        &
     &                       count=1,                                   &
     &                       rootPet=0,                                 &
     &                       rc=rc)
     IF (CheckErr(rc,__LINE__,u_FILE_u)) return
     models(iSrc)%isLand=LandValue(1)
!
      SeaValue(1)=models(iSrc)%isOcean
      CALL ESMF_VMBroadcast (vm,                                        &
     &                       bcstData=SeaValue,                         &
     &                       count=1,                                   &
     &                       rootPet=0,                                 &
     &                       rc=rc)
      IF (CheckErr(rc,__LINE__,u_FILE_u)) return
      models(iSrc)%isOcean=SeaValue(1)
!
      LandValue(1)=models(iDst)%isLand
      CALL ESMF_VMBroadcast (vm,                                        &
     &                       bcstData=LandValue,                        &
     &                       count=1,                                   &
     &                       rootPet=0,                                 &
     &                       rc=rc)
      IF (CheckErr(rc,__LINE__,u_FILE_u)) return
      models(iDst)%isLand=LandValue(1)
!
      SeaValue(1)=models(iDst)%isOcean
      CALL ESMF_VMBroadcast (vm,                                        &
     &                       bcstData=SeaValue,                         &
     &                       count=1,                                   &
     &                       rootPet=0,                                 &
     &                       rc=rc)
      IF (CheckErr(rc,__LINE__,u_FILE_u)) return
      models(iDst)%isOcean=SeaValue(1)
!
!-----------------------------------------------------------------------
!     Set source and destination masks for connector 
!-----------------------------------------------------------------------
!
      if (connectors(iSrc,iDst)%modInteraction == Ioverocn) then
        srcMaskVal = models(iSrc)%isLand
        dstMaskVal = models(iDst)%isLand
      else if (connectors(iSrc,iDst)%modInteraction == Ioverlnd) then
        srcMaskVal = models(iSrc)%isOcean
        dstMaskVal = models(iDst)%isOcean
      end if
!
!-----------------------------------------------------------------------
!     Get size of source and destination field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get source and destination fields
!-----------------------------------------------------------------------
!
      DEFINE: if (srcCount == dstCount .and. dstCount > 0) then
!
!        Allocate list arrays and routehandle 
         allocate(srcList(srcCount))
         allocate(dstList(dstCount))
!
!        Get source and destination fields.
         call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
         IF (CheckErr(rc,__LINE__,u_FILE_u)) return
!
         call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
         IF (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
         FIELDS: do i = 1, srcCount
!
!           Get source and destination field index.
            idSrc = get_varid(models(iSrc)%exportField, srcList(i))
            idDst = get_varid(models(iDst)%importField, dstList(i))
!
!           Get regrid method for interpolation. 
            itSrc = models(iSrc)%exportField(idSrc)%itype
            itDst = models(iDst)%importField(idDst)%itype
!
            if (itSrc /= itDst) then
               write(msgString,'(a)') trim(cname)//                     &
                  ': src and dst field interpolation type does not match!'
               call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
               return
            end if
!
!           Get extrapolation method for unmapped destination points.
            etSrc = models(iSrc)%exportField(idSrc)%etype
            etDst = models(iDst)%importField(idDst)%etype
!
            if (etSrc /= etDst) then
               write(msgString,'(a)') trim(cname)//                     &
                  ': src and dst field extrapolation type does not match!'
               call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
               return
            end if            
!
!           Get grid type 
            grSrc = models(iSrc)%exportField(idSrc)%gtype
            grDst = models(iDst)%importField(idDst)%gtype
!
!           Get source field object from bundle.
            fname = trim(models(iSrc)%exportField(idSrc)%short_name)
!
            call ESMF_FieldBundleGet(srcFields, trim(fname),            &
                               field=srcField, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            call ESMF_FieldGet(srcField, grid=srcGrid,                  &
                               status=FieldStatus,                      &
                               staggerloc=srcSLoc, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return          
!
            IF (FieldStatus.ne.ESMF_FIELDSTATUS_COMPLETE) THEN
               rc=ESMF_RC_OBJ_BAD
               IF (localPET.eq.0) THEN
                  IF (FieldStatus.eq.ESMF_FIELDSTATUS_EMPTY) THEN
                     msgString='ESMF_FIELDSTATUS_EMPTY'
                  ELSE IF (FieldStatus.eq.ESMF_FIELDSTATUS_GRIDSET) THEN
                     msgString='ESMF_FIELDSTATUS_GRIDSET'
                  END IF
                  WRITE (*,20) 'Source Field: ', TRIM(fname),           &
                                 TRIM(msgString)
               END IF
               IF (CheckErr(rc,__LINE__,u_FILE_u)) return
            END IF
!
!           Get destination field object from bundle.
            call ESMF_FieldBundleGet(dstFields, trim(fname),            &
                               field=dstField, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            call ESMF_FieldGet(dstField, grid=dstGrid,                  &
                               status=FieldStatus,                      &
                               staggerloc=dstSLoc, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            IF (FieldStatus.ne.ESMF_FIELDSTATUS_COMPLETE) THEN
               rc=ESMF_RC_OBJ_BAD
               IF (localPET.eq.0) THEN
                  IF (FieldStatus.eq.ESMF_FIELDSTATUS_EMPTY) THEN
                     msgString='ESMF_FIELDSTATUS_EMPTY'
                  ELSE IF (FieldStatus.eq.ESMF_FIELDSTATUS_GRIDSET) THEN
                     msgString='ESMF_FIELDSTATUS_GRIDSET'
                  END IF
                  WRITE (*,20) 'Destination Field: ', TRIM(fname),      &
                                 TRIM(msgString)
               END IF
               IF (CheckErr(rc,__LINE__,u_FILE_u)) return
            END IF  
!            
!           Create 1st RouteHandle name 
            Rname='rh_'//                                               &
                      TRIM(GRIDDES  (grSrc))//'_'//                     &
                      TRIM(GRIDDES  (grDst))//'_'//                     &
                      TRIM(INTPDES  (itSrc))//'_'//                     &
                      TRIM(EXTRDES  (etSrc))//'_'//                     &
                      TRIM(cname)              
!
            call ESMF_StateGet(state, itemSearch=trim(Rname),           &
                               itemCount=itemCount, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return

            if (itemCount.le.0) then
               rh1Exist=.false.
            else
               rh1Exist=.true.
            end if
            rh2Exist=.false.
!
            RH1_EXIST: if (.not. rh1Exist) then
               unmap = ESMF_UNMAPPEDACTION_IGNORE            

!              Set interpolation method.
               if (itSrc == Ibilin) then
                  regridMethod = ESMF_REGRIDMETHOD_BILINEAR
               else if (itSrc == Instod) then
                  regridMethod = ESMF_REGRIDMETHOD_NEAREST_STOD
               else if (itSrc == Indtos) then
                  regridMethod = ESMF_REGRIDMETHOD_NEAREST_DTOS
               else if (itSrc == Icons1) then
                  regridMethod = ESMF_REGRIDMETHOD_CONSERVE  
               else if (itSrc == Icons2) then
                  regridMethod = ESMF_REGRIDMETHOD_CONSERVE_2ND          
               else
                  write(msgString,'(a)') trim(cname)//': selected '//   &
                    'interpolation type is not supported! '//INTPDES(itSrc)
                  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if
!
!              Set extrapolation method.
               IF (etSrc.eq.Enone) THEN
                  extrapMethod=ESMF_EXTRAPMETHOD_NONE
               ELSE IF (etSrc.eq.Enear) THEN
                  extrapMethod=ESMF_EXTRAPMETHOD_NEAREST_STOD
               ELSE IF (etSrc.eq.Einvd) THEN
                  extrapMethod=ESMF_EXTRAPMETHOD_NEAREST_IDAVG
               ELSE IF (etSrc.eq.Ecreep) THEN
                  extrapMethod=ESMF_EXTRAPMETHOD_CREEP
               ELSE
                  WRITE (msgString,'(a)') TRIM(cname)//': selected '//  &
                             'extrapolation type is not supported! '//  &
                              EXTRDES(itSrc)
                  CALL ESMF_LogWrite (TRIM(msgString),ESMF_LOGMSG_ERROR)
                  CALL ESMF_Finalize (endflag=ESMF_END_ABORT)
               END IF
!  
               srcTermProcessing = 0
!
!              Debug: print out exchange fields    
               if ( localPet == 0) then
                  write(*,40) trim(cname),                              &
                  trim(models(iSrc)%exportField(idSrc)%short_name),     &
                  trim(GRIDDES(models(iSrc)%exportField(idSrc)%gtype)), &
                  trim(models(iDst)%importField(idDst)%short_name),     &
                  trim(GRIDDES(models(iDst)%importField(idDst)%gtype)), &
                  trim(INTPDES(models(iSrc)%exportField(idSrc)%itype)), &
                  trim(EXTRDES(models(iSrc)%exportField(idSrc)%etype)), &
                  rh1Exist, rh2Exist 
               end if
!     
!              Create RouteHandle
               INTERP1: if(itSrc == Icons1 .or. itSrc == Icons1) then
                  ! Create 1st RouteHandle for conservative regridding
                  call ESMF_FieldRegridStore(srcField=srcField,         &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/srcMaskVal/),          &
                                 dstMaskValues=(/dstMaskVal/),          &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridmethod,             &
                                 normType=ESMF_NORMTYPE_FRACAREA,       &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return                  
!
                  ! Add name to 1st routehandle    
                  call ESMF_RouteHandleSet(routeHandle,                 &
                               name=trim(Rname), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
                  ! Add routehandle to the state    
                  call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
               else if (itSrc == Instod .or. itSrc == Indtos) then 
                  call ESMF_FieldRegridStore(srcField=srcField,         &
                                 dstField=dstField,                     &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridmethod,             &
                                 extrapMethod=extrapMethod,             &
                                 extrapNumLevels=1,                     &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return                              
                  ! Add name to 1st routehandle    
                  call ESMF_RouteHandleSet(routeHandle,                 &
                               name=trim(Rname), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
                  ! Add routehandle to the state    
                  call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!                  
               else
                  call ESMF_FieldRegridStore(srcField=srcField,         &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/srcMaskVal/),          &
                                 dstMaskValues=(/dstMaskVal/),          &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=regridmethod,             &
                                 extrapMethod=extrapMethod,             &
                                 extrapNumLevels=1,                     &
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return                  
!
                  ! Add name to 1st routehandle    
                  call ESMF_RouteHandleSet(routeHandle,                 &
                               name=trim(Rname), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
                  ! Add routehandle to the state    
                  call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return
                  
               end if INTERP1   
!               
            end if RH1_EXIST               
! 
            INTERP2: if(itSrc == Icons1 .or. itSrc == Icons1) then
               ! Conservative methods require a second routehandle
               Rname='rh_'//                                            &
                      TRIM(GRIDDES  (grSrc))//'_'//                     &
                      TRIM(GRIDDES  (grDst))//'_'//                     &
                      TRIM(INTPDES  (Instod))//'_'//                    &
                      TRIM(EXTRDES  (etSrc))//'_'//                     &
                      TRIM(cname)              
!
               call ESMF_StateGet(state, itemSearch=trim(Rname),        &
                            itemCount=itemCount, rc=rc)
               if (CheckErr(rc,__LINE__,u_FILE_u)) return

               if (itemCount.le.0) then
                  rh2Exist=.false.
               else
                  rh2Exist=.true.
               end if
                  
               RH2_EXIST: if (.not. rh2Exist) then            
                  ! Create 2nd RouteHandle for conservative regridding
                  call ESMF_FieldRegridStore(srcField=srcField,         &
                                 dstField=dstField,                     &
                                 srcMaskValues=(/srcMaskVal/),          &
                                 dstMaskValues=(/dstMaskVal/),          &
                                 unmappedaction=unmap,                  &
                                 routeHandle=routeHandle,               &
                                 regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD,&
                                 srcTermProcessing=srcTermProcessing,   &
                                 ignoreDegenerate=.true.,               &
                                 rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return  
            
                  ! Add name to 2nd routehandle
                  call ESMF_RouteHandleSet(routeHandle,                 &
                            name=trim(Rname), rc=rc)                    
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return  
            
                  ! Add routehandle to the state 
                  call ESMF_StateAdd(state, (/ routeHandle /), rc=rc)
                  if (CheckErr(rc,__LINE__,u_FILE_u)) return                 
               end if RH2_EXIST
!                 
            end if INTERP2    
!                                  
         end do FIELDS
!
      end if DEFINE  
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 20   format (' CPL_ComputeRH - ',a,                                    &
     &        '''',a,''' has an incorrect status',/,22x,a)
!
 40   format(A10,': routehandle ',A4,'[',A,'] to ',A4,'[',A,']',        &
             ' >> interp ',A, ' extrap ', A, ' - ',L1,' - ',L1)
!
   end subroutine CPL_ComputeRH
!
!===============================================================================
!   
   subroutine CPL_ExecuteRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: localPet, petCount
      integer :: i, j, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, etSrc, etDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: msgString, cname, fname, rname
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_RouteHandle) :: routeHandle
      type(ESMF_Field) :: srcField, dstField
      type(ESMF_FieldBundle) :: dstFields, srcFields
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
!     Query the coupler for the Virtual Machine (VM) parallel environmemt.
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!     Get current parallel node rank and number of nodes.
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      !  Set source and destination couple model indices.
      do i = 1, nModels
         do j = 1, nModels
            if (connectors(i,j)%modActive .and.                         &
              trim(connectors(i,j)%name) == trim(cname)) then       
               iSrc = i
               iDst = j
            end if
         end do
      end do
!      
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Get name of fields 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      allocate(dstList(dstCount))
      call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      FIELDS: do i = 1, srcCount
!
!        Get source and destination field index 
         idSrc = get_varid(models(iSrc)%exportField, srcList(i))
         idDst = get_varid(models(iDst)%importField, dstList(i))
!
!        Get interpolation type 
         itSrc = models(iSrc)%exportField(idSrc)%itype
         itDst = models(iDst)%importField(idDst)%itype

         if (itSrc /= itDst) then
            write(msgString,'(a)') trim(cname)//                        &
               ': src and dst field interpolation type does not match!'
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
            return
         end if
!
!        Get extrapolation method for unmapped destination points.
         etSrc = models(iSrc)%exportField(idSrc)%etype
         etDst = models(iDst)%importField(idDst)%etype
 
         if (etSrc /= etDst) then
            write(msgString,'(a)') trim(cname)//                        &
               ': src and dst field extrapolation type does not match!'
            call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_ERROR)
            return
         end if    
!
!        Get grid type 
         grSrc = models(iSrc)%exportField(idSrc)%gtype
         grDst = models(iDst)%importField(idDst)%gtype
!
!        Get source and destination field
         fname = trim(models(iSrc)%exportField(idSrc)%short_name)

         call ESMF_FieldBundleGet(srcFields, trim(fname),               &
                               field=srcField, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
         call ESMF_FieldBundleGet(dstFields, trim(fname),               &
                               field=dstField, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!        Debug: print out exchange fields    
         if ( localPet == 0 ) then
            write(*,60) trim(cname),                                    &
                  trim(models(iSrc)%exportField(idSrc)%short_name),     &
                  trim(GRIDDES(models(iSrc)%exportField(idSrc)%gtype)), &
                  trim(models(iDst)%importField(idDst)%short_name),     &
                  trim(GRIDDES(models(iDst)%importField(idDst)%gtype)), &
                  trim(INTPDES(models(iSrc)%exportField(idSrc)%itype)), &
                  trim(EXTRDES(models(iSrc)%exportField(idSrc)%etype))                  
         end if                            
!
         ! Perform conservative interpolation
         REGRID: if(itSrc == Icons1 .or. itSrc == Icons1) then
            ! First, use nearestood interpolation
            Rname='rh_'//                                               &
                   TRIM(GRIDDES  (grSrc))//'_'//                        &
                   TRIM(GRIDDES  (grDst))//'_'//                        &
                   TRIM(INTPDES  (Instod))//'_'//                       &
                   TRIM(EXTRDES  (etSrc))//'_'//                        &
                   TRIM(cname)    

            call ESMF_StateGet(state, trim(Rname), routeHandle, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
            
            call ESMF_FieldRegrid(srcField, dstField, routeHandle,      &
                                  zeroregion=ESMF_REGION_SELECT,        &
                                  termorderflag=ESMF_TERMORDER_SRCSEQ,  &
                                  rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return 
!
            ! Then performs conservative interpolation
            Rname='rh_'//                                               &
                   TRIM(GRIDDES  (grSrc))//'_'//                        &
                   TRIM(GRIDDES  (grDst))//'_'//                        &
                   TRIM(INTPDES  (itSrc))//'_'//                        &
                   TRIM(EXTRDES  (etSrc))//'_'//                        &
                   TRIM(cname)   
                    
            call ESMF_StateGet(state, trim(Rname), routeHandle, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
            
            call ESMF_FieldRegrid(srcField, dstField, routeHandle,      &
                                  zeroregion=ESMF_REGION_SELECT,        &
                                  termorderflag=ESMF_TERMORDER_SRCSEQ,  &
                                  rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return                              
         else
            ! Other interpolations
            Rname='rh_'//                                               &
                   TRIM(GRIDDES  (grSrc))//'_'//                        &
                   TRIM(GRIDDES  (grDst))//'_'//                        &
                   TRIM(INTPDES  (itSrc))//'_'//                        &
                   TRIM(EXTRDES  (etSrc))//'_'//                        &
                   TRIM(cname)   
                    
            call ESMF_StateGet(state, trim(Rname), routeHandle, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
            
            call ESMF_FieldRegrid(srcField, dstField, routeHandle,      &
                                  zeroregion=ESMF_REGION_SELECT,        &
                                  termorderflag=ESMF_TERMORDER_SRCSEQ,  &
                                  rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return                                      
         endif REGRID        
!
      end do FIELDS
!
!-----------------------------------------------------------------------
!     Deallocate temporary arrays
!-----------------------------------------------------------------------
!
      deallocate(srcList)
      deallocate(dstList)
!
!-----------------------------------------------------------------------
!     Formats 
!-----------------------------------------------------------------------
!
 50   format(A10,': tstamp ',A4,' [',I4,'-',I2.2,'-',                   &
             I2.2,'_',I2.2,'_',I2.2,'] to ',A4,' [',I4,'-',I2.2,'-',    &
             I2.2,'_',I2.2,'_',I2.2,']')
 60   format(A10,': regrid ',A4,' [',A,'] to ',A4,' [',A,']',' >> ',A, ' ',A)
 70   format(" PET(",I3.3,") - ",A," = ",E14.5," (",A,")")
 80   format(A10,': redist ',A4,' [',A,'] to ',A4,' [',A,']')
!
   end subroutine CPL_ExecuteRH
!
!===============================================================================
!   
   subroutine CPL_ReleaseRH(ccomp, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_CplComp) :: ccomp
      integer, intent(out) :: rc
!     
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      logical :: rh1Exist, rh2Exist
      integer :: i, j, localPet, petCount
      integer :: itemCount, srcCount, dstCount
      integer :: iSrc, iDst, idSrc, idDst, itSrc, itDst, etSrc, etDst, grSrc, grDst
      character(ESMF_MAXSTR), pointer :: srcList(:), dstList(:)
      character(ESMF_MAXSTR) :: cname, rname
!
      type(ESMF_VM) :: vm
      type(ESMF_State) :: state
      type(ESMF_FieldBundle) :: srcFields, dstFields
      type(ESMF_RouteHandle) :: routeHandle
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query coupler component
!-----------------------------------------------------------------------
!
!     Query the coupler for the Virtual Machine (VM) parallel environmemt.
      call ESMF_CplCompGet(ccomp, name=cname, vm=vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!     Get current parallel node rank and number of nodes.
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      do i = 1, nModels
         do j = 1, nModels
            if (connectors(i,j)%modActive .and.                         &
              trim(connectors(i,j)%name) == trim(cname)) then
               iSrc = i
               iDst = j
            end if
         end do
      end do
!
!-----------------------------------------------------------------------
!     Get size of field list
!-----------------------------------------------------------------------
!
      call NUOPC_ConnectorGet(ccomp, srcFields=srcFields,               &
                              dstFields=dstFields, state=state, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(srcFields, fieldCount=srcCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldCount=dstCount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Allocate list arrays and routehandle 
!-----------------------------------------------------------------------
!
      allocate(srcList(srcCount))
      allocate(dstList(dstCount))
!
!-----------------------------------------------------------------------
!     Query field lists
!-----------------------------------------------------------------------
!
      call ESMF_FieldBundleGet(srcFields, fieldNameList=srcList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      call ESMF_FieldBundleGet(dstFields, fieldNameList=dstList, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Loop over exchange fields 
!-----------------------------------------------------------------------
!
      FIELDS: do i = 1, srcCount
!
!        Get source and destination field index 
         idSrc = get_varid(models(iSrc)%exportField, srcList(i))
         idDst = get_varid(models(iDst)%importField, dstList(i))
!
!        Get interpolation type 
         itSrc = models(iSrc)%exportField(idSrc)%itype
         itDst = models(iDst)%importField(idDst)%itype
!
!        Get grid type 
         grSrc = models(iSrc)%exportField(idSrc)%gtype
         grDst = models(iDst)%importField(idDst)%gtype
!
!        Get extrapolation method for unmapped destination points.
         etSrc = models(iSrc)%exportField(idSrc)%etype
         etDst = models(iDst)%importField(idDst)%etype         
!
!-----------------------------------------------------------------------
!     Release 1st routehandle
!-----------------------------------------------------------------------

         rname='rh_'//                                                  &
                      TRIM(GRIDDES  (grSrc))//'_'//                     &
                      TRIM(GRIDDES  (grDst))//'_'//                     &
                      TRIM(INTPDES  (itSrc))//'_'//                     &
                      TRIM(EXTRDES  (etSrc))//'_'//                     &
                      TRIM(cname)    
!
         call ESMF_StateGet(state, itemSearch=trim(rname),              &
                         itemCount=itemCount, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
         rh1Exist = .true.
         if (itemCount <= 0) rh1Exist = .false.
!
!        Release routehandle
         if (rh1Exist) then
            call ESMF_StateGet(state, trim(rname),                      &
                           routeHandle, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
         end if
!         
!-----------------------------------------------------------------------
!     Release 2nd routehandle
!-----------------------------------------------------------------------

         if(itSrc == Icons1 .or. itSrc == Icons1) then
            rname='rh_'//                                               &
                      TRIM(GRIDDES  (grSrc))//'_'//                     &
                      TRIM(GRIDDES  (grDst))//'_'//                     &
                      TRIM(INTPDES  (Instod))//'_'//                    &
                      TRIM(EXTRDES  (etSrc))//'_'//                     &
                      TRIM(cname)                         
!
            call ESMF_StateGet(state, itemSearch=trim(rname),           &
                         itemCount=itemCount, rc=rc)
            if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
            rh2Exist = .true.
            if (itemCount <= 0) rh2Exist = .false.
!
!           Release routehandle
            if (rh2Exist) then
               call ESMF_StateGet(state, trim(rname),                    &
                              routeHandle, rc=rc)
               if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
               call ESMF_FieldBundleRegridRelease(routeHandle, rc=rc)
               if (CheckErr(rc,__LINE__,u_FILE_u)) return
            end if
         end if         
!
      end do FIELDS
!
   end subroutine CPL_ReleaseRH
!
!===============================================================================
!   
end module esm_cpl
