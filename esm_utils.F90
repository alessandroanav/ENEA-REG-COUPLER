!=======================================================================
! Regional Earth System Model (RegESM)
! Copyright (c) 2013-2019 Ufuk Turuncoglu
! Licensed under the MIT License.
!=======================================================================
!
!-----------------------------------------------------------------------
!     Module file for generic utilities
!-----------------------------------------------------------------------
!
module esm_utils
!
!-----------------------------------------------------------------------
!     Used module declarations 
!-----------------------------------------------------------------------
!
   use ESMF
!
   use esm_types
   use esm_shared, only : CheckErr
!
   implicit none
  
   ! Module specific variables
   character(len=*), parameter :: u_FILE_u = __FILE__
!
!-----------------------------------------------------------------------
!     Interfaces 
!-----------------------------------------------------------------------
!
   interface UTIL_VMGlobalBroadcast 
      module procedure UTIL_VMGlobalBroadcastI4
   end interface UTIL_VMGlobalBroadcast
!
   contains
!
!===============================================================================
!   
   function UTIL_FieldCreate(field, fname, initVal, dstLandMask, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_Field) :: UTIL_FieldCreate 
! 
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: fname
      real(ESMF_KIND_R8), intent(in) :: initVal
      integer(ESMF_KIND_I4), intent(in) :: dstLandMask      
      integer, intent(out) :: rc 
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      integer :: i, j, k, localDECount
      integer :: cLbnd(2), cUbnd(2)
      real(ESMF_KIND_R8), dimension(:,:), pointer :: ptr2d
      integer(ESMF_KIND_I4), dimension(:,:), pointer :: msk2d
      integer(ESMF_KIND_I4), allocatable, dimension(:,:) :: tlw, tuw
!
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: distGrid
      type(ESMF_ArraySpec) :: arraySpec
      type(ESMF_StaggerLoc) :: staggerLoc      
!
      rc = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!     Query field
!-----------------------------------------------------------------------
!
      call ESMF_FieldGet(field, arrayspec=arraySpec,                    &
                         grid=grid, staggerloc=staggerLoc, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Query grid
!-----------------------------------------------------------------------
!
      call ESMF_GridGet(grid, distgrid=distGrid,                        &
                        localDECount=localDECount, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Allocate arrays for totalLWidth, totalUWidth and query field 
!-----------------------------------------------------------------------
!
      if (.not. allocated(tlw)) then
         allocate(tlw(2,localDECount))
         allocate(tuw(2,localDECount))
      end if
!
      call ESMF_FieldGet(field, totalLWidth=tlw, totalUWidth=tuw, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Create field from base field attributes 
!-----------------------------------------------------------------------
!
      if (localDECount == 1) then
         UTIL_FieldCreate = ESMF_FieldCreate(grid, arraySpec,           &
                                            staggerloc=staggerLoc,      &
                                            totalLWidth=tlw(:,1),       &
                                            totalUWidth=tuw(:,1),       &
                                            name=trim(fname), rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return                                   
      else
         UTIL_FieldCreate = ESMF_FieldCreate(grid, arraySpec,           &
                                            staggerloc=staggerLoc,      &
                                            name=trim(fname), rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return                                   
      end if
!
      do k = 0, localDECount-1

         ! Get pointer from field 
         call ESMF_FieldGet(UTIL_FieldCreate, localDe=k,farrayPtr=ptr2d,&
                         computationalLBound=cLbnd,                     &
                         computationalUBound=cUbnd, rc=rc)
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!        Get pointer from grid (mask item) 
         call ESMF_GridGetItem(grid, ESMF_GRIDITEM_MASK,                &
                            staggerloc=staggerLoc,                      &
                            localDe=k, farrayPtr=msk2d, rc=rc) 
         if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
         ! Initialize pointer 
         do i = cLbnd(1), cUbnd(1)
            do j = cLbnd(2), cUbnd(2)
               if (msk2d(i,j) /= dstLandMask) then
                  ptr2d(i,j) = initVal
               else
                  ptr2d(i,j) = MISSING_R8
               end if
            end do
         end do
!
         ! Nullify pointer to make sure that it does not point on a random 
         ! part in the memory 
         if (associated(ptr2d)) then
            nullify(ptr2d)
         end if
         if (associated(msk2d)) then
            nullify(msk2d)
         end if
!
      end do
!
!-----------------------------------------------------------------------
!     Deallocate temporary fields
!-----------------------------------------------------------------------
!
      if (allocated(tlw)) then
         deallocate(tlw)
         deallocate(tuw)
      end if
!
!-----------------------------------------------------------------------
!     Check consistency of the created field 
!-----------------------------------------------------------------------
!
      call ESMF_FieldValidate(UTIL_FieldCreate, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!      
   end function UTIL_FieldCreate
!
!===============================================================================
!   
   subroutine UTIL_VMGlobalBroadcastI4(var, rootPet, rc)
      implicit none
!
!-----------------------------------------------------------------------
!     Imported variable declarations 
!-----------------------------------------------------------------------
!
      integer, intent(inout) :: var
      integer, intent(in) :: rootPet
      integer, intent(inout) :: rc
!
!-----------------------------------------------------------------------
!     Local variable declarations 
!-----------------------------------------------------------------------
!
      type(ESMF_VM) :: vm
      integer :: var_local(1)
!
!-----------------------------------------------------------------------
!     Get global VM 
!-----------------------------------------------------------------------
! 
      call ESMF_VMGetGlobal(vm, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
!-----------------------------------------------------------------------
!     Broadcast data 
!-----------------------------------------------------------------------
!
      var_local(1) = var
!
      call ESMF_VMBroadcast(vm, bcstData=var_local, count=1,            &
                            rootPet=rootPet, rc=rc)
      if (CheckErr(rc,__LINE__,u_FILE_u)) return
!
      var = var_local(1)
!
   end subroutine UTIL_VMGlobalBroadcastI4
!
!===============================================================================
!   
end module esm_utils
