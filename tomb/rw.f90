! In questo programma creo un dataset di curve in ambiente HDF5. In primo luogo popolo il dataset, in secondo luogo la leggo e
! stampo a schermo i valori grazie ad una subroutine generale di lettura dei dataset HDF5.

program hdf5_rw
  ! devo poi creare il link nella stringa di compilazione del programma mettendo il percorso completo
  use hdf5                
  implicit none
  integer                        :: i, ii, status
  integer, parameter             :: num_links = 5, num_points = 100
  real, dimension(num_points)    :: x, y
  character(len = 20)            :: filename
  integer(HID_T)                 :: file_id, group_id, dataspace_id, dataset_id
  integer(HSIZE_T)               :: dims(2)
  real, dimension(num_points, 2):: data
  ! variabile allocabile per la stampa a schermo
  real, dimension(:), allocatable:: A, B  

!!! INIZIO LA LETTURA DEL FILE HDF5:
  ! esempio di creazione delle curve, calcolo del seno:
  do i = 1, num_points
    x(i) = real(i)
    ! y(i) = sin(real(i))
    y(i) = real(i)
  end do

  ! inizializzo la libreria HDF5
  call h5open_f(status)
    ! nome del file HDF5:
    filename = 'curves.h5'
      ! creo file HDF5:
      call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, status)
        ! Creo gruppo per le curve:
        call h5gcreate_f(file_id, 'curves', group_id, status)
          ! le salvo e stampo a schermo:
          do i = 1, num_links
            ! nome del dataset e creazione spazio dati:
            write(filename, '(a, i0)') 'curve_', i
            dims = (/num_points, 2/)
            call h5screate_simple_f(2, dims, dataspace_id, status)
              ! creo il dataset:
              call h5dcreate_f(group_id, trim(filename), H5T_IEEE_F32LE, dataspace_id, dataset_id, status)
                ! scrivo sia x che y in HDF5:
		data(:, 1) = x
                data(:, 2) = y+real(i*i)
                call h5dwrite_f(dataset_id, H5T_NATIVE_REAL, data, dims, status)
              ! chiudo il dataset:
              call h5dclose_f(dataset_id, status)
          end do
        ! chiudo gruppo e file HDF5:
        call h5gclose_f(group_id, status)
      call h5fclose_f(file_id, status)
  ! chiudo libreria HDF5:
  call h5close_f(status)

!!! INIZIO LA LETTURA DEL FILE HDF5:
  ! lettura dei dati da HDF5 attraverso una subroutine creata appositamente per leggere i dati scritti in precedenza in HDF5:
  call read_hdf5('curves.h5', 'curves/curve_1', A, B)
  ! stampa dei dati
  print *, 'A: ', A
  print *, 'B: ', B

  call read_hdf5('curves.h5', 'curves/curve_5', A, B)
  ! stampa dei dati
  print *, 'B: ', B

!!! SUBROUTINE GENERALE PER LA LETTURA DI FILE HDF5:
contains

  subroutine read_hdf5(file_name, dataset_name, var1, var2)
    use hdf5
    implicit none
    character(len=*), intent(in)                :: file_name, dataset_name
    real, dimension(:), allocatable, intent(out):: var1, var2
    integer                                     :: status
    integer(HID_T)                              :: file_id, dataset_id, dataspace_id
    integer(HSIZE_T)                            :: dims(2)
    integer(HSIZE_T)                            :: maxdims(2)
    real, dimension(:,:), allocatable           :: data

    ! inizializzo libreria HDF5:
    call h5open_f(status)
      ! apro file HDF5:
      call h5fopen_f(file_name, H5F_ACC_RDONLY_F, file_id, status)
        ! apro dataset:
        call h5dopen_f(file_id, dataset_name, dataset_id, status)
          ! creo spazio per dataset:
          call h5dget_space_f(dataset_id, dataspace_id, status)
          call h5sget_simple_extent_dims_f(dataspace_id, dims, maxdims, status)
            ! allocazione dell'array dei dati:
            allocate(data(dims(1), dims(2)))
              ! leggo i dati e popolo data:
              call h5dread_f(dataset_id, H5T_NATIVE_REAL, data, dims, status)
        ! chiudo dataset:
        call h5dclose_f(dataset_id, status)
      ! chiudo file:
      call h5fclose_f(file_id, status)
    ! chiudo libreria:
    call h5close_f(status)
    ! popolo le variabili:
    allocate(var1(dims(1)))
    allocate(var2(dims(1)))
      var1 = data(:, 1)
      var2 = data(:, 2)
            deallocate(data)
  end subroutine read_hdf5

end program hdf5_rw

