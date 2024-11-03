program table

use hdf5
use h5tb

implicit none

integer(hid_t) :: file_id       ! File identifier
integer(hid_t) :: dset_id       ! Dataset identifier
CHARACTER(LEN=20), PARAMETER :: filename = "ex_table.h5"
CHARACTER(LEN=15), PARAMETER :: dsetname = "table"
CHARACTER(LEN=15), PARAMETER :: title = "esercizio"
integer(HSIZE_T) :: nfields         ! fields 
integer(HSIZE_T) :: nrecords        ! records
integer(SIZE_T)  :: type_size       ! type size
character(LEN=*), dimension(nfields) :: field_names
integer(SIZE_T), dimension(nfields)  :: field_offset
integer(HID_T), dimension(nfields)   :: field_types
integer(HSIZE_T):: chunk_size      ! chunk size
integer :: compress                 ! compress
integer :: errcode                              ! error code

CALL h5open_f(error)
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
CALL h5tbmake_table_f(title, file_id, dsetname, nfields, &
                        nrecords, type_size, field_names, field_offset, &
                        field_types, chunk_size, compress, errcode)










end program




subroutine h5tbmake_table_f(table_title, loc_id, dset_name, nfields, &
                            nrecords, type_size, field_names, field_offset, &
                            field_types, chunk_size, compress, errcode) 
  implicit none
  character(LEN=*), intent(IN) :: table_title     ! name of the table
  integer(HID_T), intent(IN) :: loc_id            ! file or group identifier 
  character(LEN=*), intent(IN) :: dset_name       ! name of the dataset 
  integer(HSIZE_T), intent(IN) :: nfields         ! fields 
  integer(HSIZE_T), intent(IN) :: nrecords        ! records
  integer(SIZE_T), intent(IN) :: type_size        ! type size
  character(LEN=*), dimension(nfields), intent(IN) :: field_names
                                                  ! field names
  integer(SIZE_T), dimension(nfields), intent(IN) :: field_offset
                                                  ! field offset
  integer(HID_T), dimension(nfields), intent(IN) :: field_types
                                                  ! field types
  integer(HSIZE_T), intent(IN) :: chunk_size      ! chunk size
  integer, intent(IN) :: compress                 ! compress
  integer :: errcode                              ! error code
end subroutine h5tbmake_table_f