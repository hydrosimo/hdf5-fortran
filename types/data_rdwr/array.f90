! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!    Copyright by The HDF Group.                                               *
!    All rights reserved.                                                      *
!                                                                              *
!    This file is part of HDF5.  The full HDF5 copyright notice, including     *
!    terms governing use, modification, and redistribution, is contained in    *
!    the COPYING file, which can be found at the root of the source code       *
!    distribution tree, or in https://www.hdfgroup.org/licenses.               *
!    If you do not have access to either file, you may request a copy from     *
!    help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!    NEWS: 
!    Stiamo considerando matrici vettori dichiarando dim_r (rows) e dim_c (columns)
!    Questo programma:
!    - crea un intero dataset sulla RAM (vettore di 1s);
!    - lo scrive in HDF5;
!    - riapre il file HDF5;
!    - seleziona tutti gli elementi e cambia il valore;
!    - richiude il file;
!    - riapre il file e lo legge;
!    - seleziona tutti gli elementi;
!    - cambia il valore (x10);
!    - chiude tutto.

program wrr_arr

use hdf5 ! This module contains all necessary modules

implicit none

character(len=20), parameter :: filename1 = 'vect.h5' ! File name
character(len=20), parameter :: dsetname1 = 'db1'    ! Dataset name
integer, parameter :: RANK = 2   ! Dataset rank
integer(size_t), parameter :: NUMP = 1 ! Number of points selected
integer(hid_t) :: file1_id       ! File1 identifier
integer(hid_t) :: file2_id       ! File2 identifier
integer(hid_t) :: dset1_id       ! Dataset1 identifier
integer(hid_t) :: dset2_id       ! Dataset2 identifier
integer(hid_t) :: dataspace1     ! Dataspace identifier
integer(hid_t) :: dataspace2     ! Dataspace identifier
integer(hid_t) :: memspace       ! memspace identifier
integer(hsize_t), dimension(1) :: dimsm = (/1/)   ! Memory dataspace dimensions
integer(hsize_t), dimension(2) :: dimsf           ! File dataspace dimensions
integer(hsize_t), dimension(RANK,NUMP) :: coord   ! Elements coordinates in the file
integer, dimension(:,:), allocatable :: buf1, buf2, bufnew ! Data buffers
integer, dimension(1) :: val  ! Values to write
integer :: memrank = 1        ! Rank of the dataset in memory
integer :: i, j
integer :: error  ! Error flag
integer(hsize_t), dimension(2) :: data_dims
integer(hsize_t), dimension(1) :: val_dim
integer(hsize_t) :: dim_r = 1
integer(hsize_t) :: dim_c = 1000000000  ! dimensione vettore

integer :: dt_start(8), dt_end(8), dt_start_tot(8), dt_end_tot(8)
real :: start_time, finish_time, elapsed_time, hours, minutes, seconds
real :: start_time_tot, finish_time_tot, elapsed_time_tot
real :: sph=3600, ms=60
real (kind=4) :: cnt_perc, step=10
real (kind=4) :: perc

call cpu_time(start_time)
call date_and_time(values = dt_start)
    
dimsf = (/dim_r, dim_c/)
data_dims = (/dim_r, dim_c/)
allocate(buf1(dim_r, dim_c))

! Create two files containing identical datasets. Write 0's to one
! and 1's to the other.
!
! Data initialization.
write(6, *) '---------------------------'
write(6, *) 'data initialization started'
cnt_perc=10
do i = 1, dim_r
     do j = 1, dim_c
          buf1(i,j) = 1;
          perc=real(i*j)/real(dim_r*dim_c)
          if (floor(perc*100) >= cnt_perc) then   
              write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
          cnt_perc=cnt_perc+step       
          end if
     end do
end do
write(6, *) 'data initialization completed'

! Initialize FORTRAN interface.
call h5open_f(error)

! Create file1, file2  using default properties.
call h5fcreate_f(filename1, H5F_ACC_TRUNC_F, file1_id, error)

! Create the data space for the  datasets.
call h5screate_simple_f(RANK, dimsf, dataspace1, error)

! Create the datasets with default properties.
call h5dcreate_f(file1_id, dsetname1, H5T_NATIVE_INTEGER, dataspace1, &
                 dset1_id, error)

! Write the datasets.
call h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, buf1, data_dims, error)
write(6, *) ''
write(6, *) '---------------------------'
write(6, *) 'first write completed'

! Close the dataspace for the datasets.
call h5sclose_f(dataspace1, error)

! Close the datasets.
call h5dclose_f(dset1_id, error)

! Close the files.
call h5fclose_f(file1_id, error)
write(6, *) ''
write(6, *) '---------------------------'
write(6, *) 'first close completed'

! Open the two files.  Select two points in one file, write values to
! those point locations, then do H5Scopy and write the values to the
! other file.  Close files.

! Open the files.
call h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)

! Open the  datasets.
call h5dopen_f(file1_id, dsetname1, dset1_id, error)

! Get dataset1's dataspace identifier.
call h5dget_space_f(dset1_id, dataspace1, error)

! Create memory dataspace.
call h5screate_simple_f(memrank, dimsm, memspace, error)
write(6, *) ''
write(6, *) '---------------------------'
write(6, *) 're-opening completed'

! Set the selected point positions. Because Fortran array index starts
! from 1, so add one to the actual select points in C.
write(6, *) ''
write(6, *) '---------------------------'
write(6, *) 'Selection and replacing started'
cnt_perc=10
do i = 1, dim_r
     do j = 1, dim_c
          ! write(6, *)
          ! write(6, *), '------------------------------'
          coord(1,1) = i
          coord(2,1) = j
          ! write(6, *), 'i, j', i, j
          call h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, RANK, NUMP,&
                                    coord, error)
          ! Write value into the selected points in dataset1.
          ! Uso val_dim anziche data_dims(modificato come 2,4) obbligatoriamente come un vettore, non vuole uno scalare, ma sarebbe il nostro buffer.
          val_dim = 1 
          val = i+j
          call H5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, val, val_dim, error, &
                         mem_space_id=memspace, file_space_id=dataspace1)
          perc=real(i*j)/real(dim_r*dim_c)
          if (floor(perc*100) >= cnt_perc) then   
               write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
               cnt_perc=cnt_perc+step       
          end if
     end do
end do
write(6, *) ''
write(6, *) '---------------------------'
write(6, *) 'Selection and replacing completed'

! Close the dataspace for the datasets.
call h5sclose_f(dataspace1, error)

! Close the memoryspace.
call h5sclose_f(memspace, error)

! Close the datasets.
call h5dclose_f(dset1_id, error)

! Close the files.
call h5fclose_f(file1_id, error)

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

! -----------------------------------------------------------------------------------------
! Reading an all dataset 

! CALL h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)
! CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)
! allocate(bufnew(dim_value, dim_value))

! CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)

! write(6,*) "The data in dataset read is: "
!      do i = 1, data_dims(1)
!          print *, (bufnew(i,j), j = 1,data_dims(2))
!      end do

! CALL h5dclose_f(dset1_id, error)
! CALL h5fclose_f(file1_id, error)

! deallocate(bufnew)


! -----------------------------------------------------------------------------------------
! Reading an element once

call cpu_time(start_time)
call date_and_time(values = dt_start)

call h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)
call h5dopen_f(file1_id, dsetname1, dset1_id, error)
call h5dget_space_f(dset1_id, dataspace1, error)
call h5screate_simple_f(memrank, dimsm, memspace, error)
allocate(bufnew(1,1))

write(6,*) 
write(6,*) '-------------------------------'
write(6,*) 'Reading and writing process: '
cnt_perc=10
do i = 1, dim_r
     do j = 1, dim_c
          coord(1,1) = i
          coord(2,1) = j
          call h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, RANK, NUMP,&
                                   coord, error)
          ! Write value into the selected points in dataset1.
          ! Uso val_dim anziche data_dims(modificato come 2,4) obbligatoriamente come un vettore, non vuole uno scalare, ma sarebbe il nostro buffer.
          val_dim = 1
          call h5dread_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, val_dim, error, mem_space_id=memspace, file_space_id=dataspace1)
          ! write(6, *), 'val, i, j = ', i, j, bufnew(1,1)
          bufnew=bufnew*10
          call H5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, val_dim, error, mem_space_id=memspace, file_space_id=dataspace1)
          perc=real(i*j)/real(dim_r*dim_c)
          if (floor(perc*100) >= cnt_perc) then   
               write(6,'(a,i6,a)') ''//achar(27)//'[38;2;253;252;187m ',int(cnt_perc),'% completed'//achar(27)//'[0m'
               cnt_perc=cnt_perc+step       
          end if
     end do
end do

call h5dclose_f(dset1_id, error)
call h5fclose_f(file1_id, error)

! Close FORTRAN interface.
!
call h5close_f(error)

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

end program wrr_arr
