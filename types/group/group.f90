!************************************************************
!
!  This example shows how to create, open, and close a group.
!
!  This file is intended for use with HDF5 Library version 1.8
!
!************************************************************
PROGRAM main

  USE HDF5
  use iso_c_binding
  use iso_fortran_env
  IMPLICIT NONE

  integer, parameter :: int_k1 = selected_int_kind(2)  ! This should map to INTEGER*1 on most modern processors
  integer, parameter :: int_k4 = selected_int_kind(4)  ! This should map to INTEGER*2 on most modern processors
  integer, parameter :: int_k8 = selected_int_kind(9)  ! This should map to INTEGER*4 on most modern processors
  integer, parameter :: int_k16 = selected_int_kind(18) ! This should map to INTEGER*8 on most modern processors
  integer, parameter :: r_k4 = selected_real_kind(6,37) ! This should map to REAL*4 on most modern processors
  integer, parameter :: r_k8 = selected_real_kind(15,307) ! This should map to REAL*8 on most modern processors

  CHARACTER(LEN=16), PARAMETER :: filename   = "group.h5"
  character(len=20), parameter :: dsetname1 = "a"
  character(len=20), parameter :: dsetname2 = "b"
  character(len=20), parameter :: dsetname3 = "c"
  character(len=20), parameter :: dsetname4 = "d"
  INTEGER(HID_T) :: file, group1, group2 ! Handles
  INTEGER :: hdferr
  integer(hid_t) :: dset_id1, dset_id2, dset_id3, dset_id4      ! Dataset1 identifier
  integer(hsize_t), dimension(1:2) :: maxdims
  integer :: RANK = 2, rank_arr = 1
  integer(hsize_t), dimension(1:2) :: dims = (/1,1/)
  integer(hid_t) :: dataspace1, dataspace2, dataspace3, dataspace4   ! Dataspace identifier
  integer(hid_t) :: crp_list      ! Dataset creation property identifier
  integer(hsize_t), dimension(1:2) :: dimsc = (/1,1/)
  integer(hsize_t), dimension(1:2) :: dimsm
  integer(hsize_t) :: dim_r
  integer(hsize_t) :: dim_c
  integer :: rankr, ii, jj
  integer(INT32), dimension(1,1) :: a, c
  real(REAL32), dimension(1,1) :: b
  INTEGER(HSIZE_T), DIMENSION(1:2) :: data_dims
  INTEGER(HID_T) :: memspace  
  INTEGER(HSIZE_T), DIMENSION(1:2) :: size
  INTEGER(HSIZE_T), DIMENSION(1:2) :: offset
  INTEGER(HSIZE_T), DIMENSION(1:2) :: count
  INTEGER(SIZE_T) ::num_points = 1  ! Number of selected points
  INTEGER(HSIZE_T) , DIMENSION(1,1) :: coord
  TYPE(hdset_reg_ref_t_f) , DIMENSION(1) :: ref  
  INTEGER(HSIZE_T), DIMENSION(1) :: ref_size
  
  integer :: dt_start(8), dt_end(8), dt_start_tot(8), dt_end_tot(8)
  real :: start_time, finish_time, elapsed_time, hours, minutes, seconds
  real :: start_time_tot, finish_time_tot, elapsed_time_tot
  real :: sph=3600, ms=60
  real (kind=4) :: cnt_perc, step=10
  real (kind=4) :: perc

  call cpu_time(start_time)
  call date_and_time(values = dt_start)

  dim_r = 1
  dim_c = 10

  ! Initialize FORTRAN interface.
  !
  CALL h5open_f(hdferr)
  !
  ! Create a new file using the default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, hdferr)
  !
  ! Create a group named "G1" in the file.
  !
  ! CALL h5gcreate_f(file, "/G1", group1, hdferr)
  ! CALL h5gcreate_f(file, "/G1/G2", group2, hdferr)
  
  !create the data space with unlimited dimensions.
  maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)
  call h5screate_simple_f(RANK, dims, dataspace1, hdferr, maxdims)
  call h5screate_simple_f(RANK, dims, dataspace2, hdferr, maxdims)
  call h5screate_simple_f(RANK, dims, dataspace3, hdferr, maxdims)
  call h5screate_simple_f(RANK, dims, dataspace4, hdferr, maxdims)
  !modify dataset creation properties, i.e. enable chunking
  call h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, hdferr)
  call h5pset_layout_f(crp_list, H5D_CHUNKED_F, hdferr)
  call h5pset_chunk_f(crp_list, RANK, dimsc, hdferr)
  ! Create the datasets with default properties.
  call h5dcreate_f(file, dsetname1, H5T_NATIVE_INTEGER, dataspace1, &
                 dset_id1, hdferr, crp_list)
  call h5dcreate_f(file, dsetname2, H5T_NATIVE_REAL, dataspace2, &
                 dset_id2, hdferr, crp_list)
  call h5dcreate_f(file, dsetname3, H5T_NATIVE_INTEGER, dataspace3, &
                 dset_id3, hdferr, crp_list)
  call h5dcreate_f(file, dsetname4, H5T_STD_REF_DSETREG, dataspace4, &
                 dset_id4, hdferr, crp_list)
  !
  ! Close the group.  The handle "group" can no longer be used.
  !
  ! call h5pclose_f(crp_list, hdferr)
  ! call h5sclose_f(dataspace1, hdferr)
  ! call h5sclose_f(dataspace2, hdferr)
  ! call h5sclose_f(dataspace3, hdferr)
  ! call h5sclose_f(dataspace4, hdferr)

  a(1,1) = 1
  b(1,1) = real(2)
  c(1,1) = 3
  !Write data array to dataset
  data_dims(1:2) = (/1,1/)
  CALL h5dwrite_f(dset_id1, H5T_NATIVE_INTEGER, a, data_dims, hdferr)
  CALL h5dwrite_f(dset_id2, H5T_NATIVE_REAL, b, data_dims, hdferr)
  CALL h5dwrite_f(dset_id3, H5T_NATIVE_INTEGER, c, data_dims, hdferr)
  ! CALL h5dwrite_f(dset_id4, H5T_NATIVE_INTEGER, d, data_dims, hdferr)
  
  ! CALL h5sselect_none_f(dset_id1, hdferr)
  coord(1,1) = a(1,1)
  WRITE(6,*) 'MINNIE'
  CALL h5sselect_none_f(dataspace1, hdferr)
  CALL h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, rank, num_points,&
                               coord, hdferr)
  WRITE(6,*) 'PAPERINO'
  CALL h5rcreate_f(file, dsetname1, dataspace1, ref(1), hdferr)
  WRITE(6,*) 'PIPPO'
  ! Write dataset with the references.
  !
  ref_size(1) = 1
  CALL h5dwrite_f(dset_id4, H5T_STD_REF_DSETREG, ref, ref_size, hdferr)
  WRITE(6,*) 'PLUTO'
  
  dimsm = (/1,1/)
  call h5screate_simple_f(2, dimsm, memspace, hdferr)
  cnt_perc=10
  do ii=1,dim_r
    do jj=1,dim_c
      a(1,1) = jj
      b(1,1) = real(jj)/3
      c(1,1) = jj*3
      size(1:2) = (/1,1+jj/)
      call h5dset_extent_f(dset_id1, size, hdferr)
      call h5dset_extent_f(dset_id2, size, hdferr)
      call h5dset_extent_f(dset_id3, size, hdferr)
      offset(1:2) = (/0,jj/)
      count(1:2)  = (/1,1/)
      call h5dget_space_f(dset_id1, dataspace1, hdferr)
      call h5dget_space_f(dset_id2, dataspace2, hdferr)
      call h5dget_space_f(dset_id3, dataspace3, hdferr)
      call h5sselect_hyperslab_f(dataspace1, H5S_SELECT_SET_F, &
          offset, count, hdferr)
      call h5sselect_hyperslab_f(dataspace2, H5S_SELECT_SET_F, &
          offset, count, hdferr)
      call h5sselect_hyperslab_f(dataspace3, H5S_SELECT_SET_F, &
          offset, count, hdferr)
      data_dims(1:2) = (/1,1/)
      call h5dwrite_f(dset_id1, H5T_NATIVE_INTEGER, a, data_dims, hdferr, &
            memspace, dataspace1)
      call h5dwrite_f(dset_id2, H5T_NATIVE_REAL, b, data_dims, hdferr, &
            memspace, dataspace2)
      call h5dwrite_f(dset_id3, H5T_NATIVE_INTEGER, c, data_dims, hdferr, &
            memspace, dataspace3)
      ! se non facciamo questo close datapace la RAM cresce vertiginosamente.
      call h5sclose_f(dataspace1, hdferr)
      call h5sclose_f(dataspace2, hdferr)
      call h5sclose_f(dataspace3, hdferr)
      perc=real(ii*jj)/real(dim_r*dim_c)
      if (floor(perc*100) >= cnt_perc) then   
          write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
          cnt_perc=cnt_perc+step       
      end if
    end do 
  end do

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
  
  ! CALL h5gclose_f(group1,hdferr)
  ! CALL h5gclose_f(group2,hdferr)
  !
  ! Close and release resources.
  !
  ! CALL h5gclose_f(group1, hdferr)
  CALL h5fclose_f(file , hdferr)

END PROGRAM main
