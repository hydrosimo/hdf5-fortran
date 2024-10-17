! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! This example extends an HDF5 dataset popolated by compounds.

program compound

use hdf5 ! This module contains all necessary modules
use iso_c_binding

implicit none

integer, parameter :: int_k1 = selected_int_kind(2)  ! This should map to INTEGER*1 on most modern processors
integer, parameter :: int_k4 = selected_int_kind(4)  ! This should map to INTEGER*2 on most modern processors
integer, parameter :: int_k8 = selected_int_kind(9)  ! This should map to INTEGER*4 on most modern processors
integer, parameter :: int_k16 = selected_int_kind(18) ! This should map to INTEGER*8 on most modern processors
integer, parameter :: r_k4 = selected_real_kind(6,37) ! This should map to REAL*4 on most modern processors
integer, parameter :: r_k8 = selected_real_kind(15,307) ! This should map to REAL*8 on most modern processors

character(len=20), parameter :: filename = "compound.h5"
!dataset rank is 2 and name is "ExtendibleArray"
character(len=20), parameter :: dsetname = "extendible_compound"
integer :: RANK = 2, rank_arr = 1
integer(hid_t) :: file_id       ! File identifier
integer(hid_t) :: dset_id       ! Dataset identifier
integer(hid_t) :: dataspace     ! Dataspace identifier
integer(hid_t) :: memspace      ! Memory dataspace identifier
integer(hid_t) :: crp_list      ! Dataset creation property identifier

!dataset dimensions at creation time
integer(hsize_t), dimension(1:2) :: dims = (/1,1/)
!data dimensions
integer(hsize_t), dimension(1:2) :: dimsc = (/1,1/)
integer(hsize_t), dimension(1:2) :: dimsm
!Maximum dimensions
integer(hsize_t), dimension(1:2) :: maxdims
integer(hsize_t), dimension(1:2) :: offset
integer(hsize_t), dimension(1:2) :: count
! Variables for reading and writing
integer(hsize_t) :: dim_r
integer(hsize_t) :: dim_c
integer, dimension(1:1,1:1)  :: data1
integer(hsize_t), dimension(1) :: data_dims
!Size of data in the file
integer(hsize_t), dimension(1:2) :: size
!general purpose integer
integer(hsize_t) :: i, j
!flag to check operation success
integer :: error
!Variables used in reading data back
integer(hsize_t), dimension(1:2) :: dimsr, maxdimsr
integer :: rankr, ii, jj
integer :: dt_start(8), dt_end(8), dt_start_tot(8), dt_end_tot(8)
real :: start_time, finish_time, elapsed_time, hours, minutes, seconds
real :: start_time_tot, finish_time_tot, elapsed_time_tot
real :: sph=3600, ms=60
real (kind=4) :: cnt_perc, step=10
real (kind=4) :: perc
! Compound declaration:
type comp_type
  character(len=1), dimension(1:7) :: chr
  integer(kind=int_k16) :: a
  real(kind=r_k8) :: b
  real(kind=r_k8) :: c
  integer(kind=int_k16), dimension(1:4) :: d
end type comp_type
type(comp_type), target, dimension(1) :: caronte
integer(hid_t) :: car_id     ! file datatype identifier

integer(hsize_t), dimension(1) :: tdims1=(/7/)
integer(hsize_t), dimension(1) :: tdims1a=(/4/)
integer(hid_t) :: arr1_id      ! /* nested array datatype id	*/
integer(hid_t) :: arr2_id
type(c_ptr) :: f_ptr

call cpu_time(start_time)
call date_and_time(values = dt_start)

dim_r = 1
dim_c = 10

!Initialize FORTRAN predefined datatypes
call h5open_f(error)
!create a new file using default properties.
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
!create the data space with unlimited dimensions.
maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)
call h5screate_simple_f(RANK, dims, dataspace, error, maxdims)
!modify dataset creation properties, i.e. enable chunking
call h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
call h5pset_layout_f(crp_list, H5D_CHUNKED_F, error)
call h5pset_chunk_f(crp_list, RANK, dimsc, error)
! create the memory data type: gestisce il dato e ne determina la natura:
call h5tcreate_f(H5T_COMPOUND_F, H5OFFSETOF(C_LOC(caronte(1)), C_LOC(caronte(2))), car_id, error)
! creo e scrivo un array annidato all'interno del dataset
call h5tarray_create_f(H5T_NATIVE_CHARACTER, rank_arr, tdims1, arr1_id, error)
call h5tinsert_f(car_id, "chr", H5OFFSETOF(C_LOC(caronte(1)),C_LOC(caronte(1)%chr)), arr1_id, error)
call h5tinsert_f(car_id, "a", H5OFFSETOF(C_LOC(caronte(1)),C_LOC(caronte(1)%a)), h5kind_to_type(int_k1,H5_INTEGER_KIND), error)
call h5tinsert_f(car_id, "b", H5OFFSETOF(C_LOC(caronte(1)),C_LOC(caronte(1)%b)), h5kind_to_type(r_k8,H5_REAL_KIND), error)
call h5tinsert_f(car_id, "c", H5OFFSETOF(C_LOC(caronte(1)),C_LOC(caronte(1)%c)), h5kind_to_type(r_k8,H5_REAL_KIND), error)
! create an array of integer datatype
call h5tarray_create_f(h5kind_to_type(int_k16, H5_INTEGER_KIND), rank_arr, tdims1a, arr2_id, error)
call h5tinsert_f(car_id, "d", H5OFFSETOF(C_LOC(caronte(1)),C_LOC(caronte(1)%d)), arr2_id, error)
call h5dcreate_f(file_id, dsetname, car_id, dataspace, &
     dset_id, error, crp_list)
call h5pclose_f(crp_list, error)
call h5sclose_f(dataspace, error)

! Inizializzazione del compound
i = 0
caronte(1)%chr(1:7)(1:1) = (/'a','b','c','d','e','f','g'/)
caronte(1)%a = i
caronte(1)%b = i*2
caronte(1)%c = 1./REAL(i+1)
caronte(1)%d(1:4) = (/i,i*2,i*3,i*4/)
!Write data array to dataset
f_ptr = C_LOC(caronte(1))
call h5dwrite_f(dset_id, car_id, f_ptr, error)

! extend the dataset. 
write(6,*) 'Extending dataset started:'
! I open the temporary dataspace "memspace"
dimsm = (/1,1/)
call h5screate_simple_f(2, dimsm, memspace, error)
cnt_perc=10
do ii=1,dim_r
  do jj=1,dim_c
    caronte(1)%chr(1:7)(1:1) = (/'a','b','c','d','e','f','g'/)
    caronte(1)%a = jj
    caronte(1)%b = jj*2
    caronte(1)%c = 1./REAL(jj+1)
    caronte(1)%d(1:4) = (/jj,jj*2,jj*3,jj*4/)
    size(1:2)   = (/1,1+jj/)
    call h5dset_extent_f(dset_id, size, error)
    offset(1:2) = (/0,jj/)
    count(1:2)  = (/1,1/)
    call h5dget_space_f(dset_id, dataspace, error)
    call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
        offset, count, error)
    data_dims(1:2) = (/1,1/)
    f_ptr = C_LOC(caronte(1))
    call h5dwrite_f(dset_id, car_id, f_ptr, error, &
          memspace, dataspace)
    ! se non facciamo questo close datapace la RAM cresce vertiginosamente.
    call h5sclose_f(dataspace, error)
    perc=real(ii*jj)/real(dim_r*dim_c)
    if (floor(perc*100) >= cnt_perc) then   
        write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
        cnt_perc=cnt_perc+step       
    end if
  end do
end do
! i close the temporary dataspace "memspace"
call h5sclose_f(memspace, error)

write(6,*) 'Extending dataset finished!'
  
call h5dclose_f(dset_id, error)
call h5fclose_f(file_id, error)

call cpu_time(finish_time)
call date_and_time(values = dt_end)
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m cpu time :', int(hours),'h',int(minutes), &
  'm',seconds,'s'//achar(27)//'[0m'
start_time=real(dt_start(7))+real(dt_start(8))/1000+real(dt_start(6))*ms+real(dt_start(5))*sph
finish_time=real(dt_end(7))+real(dt_end(8))/1000+real(dt_end(6))*ms+real(dt_end(5))*sph
elapsed_time=finish_time-start_time
hours=elapsed_time/sph
minutes=mod(elapsed_time, sph)/ms
seconds=mod(minutes*ms, ms)
write(6,'(a,i4,a1,i3,a1,f6.2,a)') ''//achar(27)//'[38;2;10;156;253m wall-clock time :', int(hours),'h',int(minutes), &
  'm',seconds,'s'//achar(27)//'[0m'

CALL h5close_f(error)

end program compound
