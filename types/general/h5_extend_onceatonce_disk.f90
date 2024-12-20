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
! This example extends an HDF5 dataset. It is used in the HDF5 Tutorial.

PROGRAM H5_EXTEND

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  
  CHARACTER(LEN=20), PARAMETER :: filename = "extend_once_disk.h5"
  !dataset rank is 2 and name is "ExtendibleArray"
  CHARACTER(LEN=15), PARAMETER :: dsetname = "ExtendibleArray"
  INTEGER :: RANK = 2
  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: dset_id       ! Dataset identifier
  INTEGER(HID_T) :: dataspace     ! Dataspace identifier
  INTEGER(HID_T) :: memspace      ! Memory dataspace identifier
  INTEGER(HID_T) :: crp_list      ! Dataset creation property identifier

  !dataset dimensions at creation time
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dims = (/1,1/)
  !data dimensions
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsc = (/1,1/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsm
  !Maximum dimensions
  INTEGER(HSIZE_T), DIMENSION(1:2) :: maxdims
  INTEGER(HSIZE_T), DIMENSION(1:2) :: offset
  INTEGER(HSIZE_T), DIMENSION(1:2) :: count
  ! Variables for reading and writing
  INTEGER(HSIZE_T) :: dim_r
  INTEGER(HSIZE_T) :: dim_c
  INTEGER, DIMENSION(1:1,1:1)  :: data1
  INTEGER, DIMENSION(:,:), allocatable :: data2
  INTEGER(HSIZE_T), DIMENSION(1:2) :: data_dims
  !Size of data in the file
  INTEGER(HSIZE_T), DIMENSION(1:2) :: size
  !general purpose integer
  INTEGER(HSIZE_T) :: i, j
  !flag to check operation success
  INTEGER :: error
  !Variables used in reading data back
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsr, maxdimsr
  INTEGER :: rankr, ii, jj
!   INTEGER, DIMENSION(1:3,1:10)  :: rdata

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
  allocate(data2(dim_r, dim_c))

  WRITE(6,*) "Filling values in RAM started"
  do i=1,dim_r
    do j=1,dim_c
        data2(i,j) = j
    end do
  end do
  WRITE(6,*) "Filling values in RAM finished"

  !Initialize FORTRAN predefined datatypes
  CALL h5open_f(error)
  !Create a new file using default properties.
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
  !Create the data space with unlimited dimensions.
  maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)
  CALL h5screate_simple_f(RANK, dims, dataspace, error, maxdims)
  !Modify dataset creation properties, i.e. enable chunking
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
  CALL h5pset_chunk_f(crp_list, RANK, dimsc, error)
  !Create a dataset with 3X3 dimensions using cparms creation properties.
  CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
       dset_id, error, crp_list )
  CALL h5sclose_f(dataspace, error)
  !Fill data array with 1's
  DO i = 1, dims(1)
     DO j = 1, dims(2)
        data1(i,j) = 1
     END DO
  END DO
  !Write data array to dataset
  data_dims(1:2) = (/1,1/)
  CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data1, data_dims, error)

  !Extend the dataset. Dataset becomes 10 x 3.
  WRITE(6,*) "Extending dataset started"
cnt_perc=10
do ii=1,dim_r
    do jj=1,dim_c
        size(1:2)   = (/1,1+jj/)
        CALL h5dset_extent_f(dset_id, size, error)
        offset(1:2) = (/0,jj/)
        count(1:2)  = (/1,1/)
        dimsm = (/1,1/)
        CALL h5screate_simple_f(2, dimsm, memspace, error)

        !Write to 3x7 extended part of dataset
        CALL h5dget_space_f(dset_id, dataspace, error)
        CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
            offset, count, error)

        data_dims(1:2) = (/1,1/)
        ! WRITE(6,*) "H5Dwrite started"
        CALL H5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data2(ii,jj), data_dims, error, &
            memspace, dataspace)
        perc=real(ii*jj)/real(dim_r*dim_c)
            if (floor(perc*100) >= cnt_perc) then   
                write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
                cnt_perc=cnt_perc+step       
            end if
    end do
end do

  WRITE(6,*) "Extending dataset finished"
  
  !Close the objects that were opened.
  CALL h5sclose_f(dataspace, error)
  CALL h5pclose_f(crp_list, error)
  CALL h5dclose_f(dset_id, error)
  CALL h5fclose_f(file_id, error)

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

!   !read the data back
!   !Open the file.
!   CALL h5fopen_f (filename, H5F_ACC_RDONLY_F, file_id, error)
!   !Open the  dataset.
!   CALL h5dopen_f(file_id, dsetname, dset_id, error)
!   !Get dataset's dataspace handle.
!   CALL h5dget_space_f(dset_id, dataspace, error)
!   !Get dataspace's rank.
!   CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)
!   !Get dataspace's dimensions.
!   CALL h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)
!   !Get creation property list.
!   CALL h5dget_create_plist_f(dset_id, crp_list, error)
!   ! Fill read buffer with zeroes
!   rdata(1:dimsr(1),1:dimsr(2)) = 0
!   !create memory dataspace
!   CALL h5screate_simple_f(rankr, dimsr, memspace, error)
!   !Read data
!   data_dims(1:2) = (/3,10/)
!   CALL H5dread_f(dset_id, H5T_NATIVE_INTEGER, rdata, data_dims, &
!        error, memspace, dataspace)

!   WRITE(*,'(A)') "Dataset:"
!   DO i = 1, dimsr(1)
!      WRITE(*,'(100(I0,1X))') rdata(i,1:dimsr(2))
!   END DO
  
  
!   !Close the objects that were opened.
!   CALL h5sclose_f(dataspace, error)
!   CALL h5sclose_f(memspace, error)
!   CALL h5pclose_f(crp_list, error)
!   CALL h5dclose_f(dset_id, error)
!   CALL h5fclose_f(file_id, error)
  !Close FORTRAN predefined datatypes
  CALL h5close_f(error)

END PROGRAM H5_EXTEND
