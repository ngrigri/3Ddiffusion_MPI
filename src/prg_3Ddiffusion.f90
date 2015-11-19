!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- prg_3Ddiffusion - (June and August 2015)
!!
!!- author@mail: Nicolas.Grima@univ-brest.fr
!!
!!- Modified, improved by:
!!- Date                 :
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PROGRAM prg_fsp

  !///////////////////!
  !- USE association -!
  !///////////////////!
  USE mod_precision      ! Set here the Real and Integer precision
  USE mod_configure      ! configuration parameters
  USE mod_cst            ! all the constants
  USE mod_lun            ! Logical Unit Number
  USE mod_memory         ! To have access to memory information
  USE mod_namelist       ! read namelist file and set parameters
  USE mod_mpi            ! all the mpi routines
  USE mod_3Ddiffusion    ! 3D diffusion init and calculation
  USE mod_output         ! call -> netcf output routines

  !////////////////!
  !- DECLARATIONS -!
  !////////////////!
  IMPLICIT NONE          ! All the variables have to be declared

  !---------------------------------------!
  !- The two main arrays of the programm -!
  !---------------------------------------!
  REAL(kind = rprec), DIMENSION(:, :, :), POINTER :: &
       ff                                          , &
       fn

  !/////////////!
  !- MAIN PART -!
  !/////////////!

  !=======================!
  !- MPI: INITIALIZATION -!
  !=======================!
  CALL sub_mpi_initialisation()

  IF (rank == iZero) THEN
     !-----------------!
     !- Print version -!
     !-----------------!
     WRITE(lun_standard,*)''
     WRITE(lun_standard,*)'===================================================='
     WRITE(lun_standard,*)'= -o0)   '//TRIM(APPNAME)//' v'//TRIM(VERSION)//'     (0o- ='
     WRITE(lun_standard,*)'===================================================='
     WRITE(lun_standard,*)''
     !---------------------!
     !- Machine Precision -!
     !---------------------!
     CALL sub_machine_precision(lun_standard)
  ENDIF

  !====================!
  !- Reading namelist -!
  !====================!
  IF (rank == iZero) THEN
     CALL sub_read_namelist(.TRUE.)
  ELSE
     CALL sub_read_namelist(.FALSE.)
  ENDIF

  !============================================================!
  !- MPI: TEST the number of processes required ans available -!
  !============================================================!
  CALL  sub_mpi_verif_proc_avail()

  !==============================!
  !- MPI:  DOMAIN DECOMPOSITION -!
  !==============================!
  !- mpi: Topology creation
  CALL sub_mpi_topology()
  !- mpi: Domain coordinates and array indices
  CALL sub_mpi_domain()
  !- mpi: To know its neighbours
  CALL sub_mpi_neighbours()

  !================================================!
  !- Dynamic memory Allocation and Initialization -!
  !================================================!
  CALL sub_3Ddiff_initialisation(ff, fn)

  !==============================!
  !- MPI: derived type creation -!
  !==============================!
  CALL sub_mpi_derived_type(ff)

  !===================================================!
  !- NETCDF OUTPUT FILE: CREATION and INITIALIZATION -!
  !===================================================!
  IF (ncoutput) CALL mod_output_init()

  !=============!
  !- MAIN LOOP -!
  !=============!
  CALL sub_3Ddiff_mainloop(ff, fn)

  !------------------------!
  !- CLOSE NETCDF FILE(s) -!
  !------------------------!
  IF (ncoutput) CALL mod_output_close()

  IF (rank == iZero) THEN
     !-----------------!
     !- Print version -!
     !-----------------!
     WRITE(lun_standard,*)''
     WRITE(lun_standard,*)'====================================================='
     WRITE(lun_standard,*)'= -o0)    '//TRIM(APPNAME)//' v'//TRIM(VERSION)//'     (0o- ='
     WRITE(lun_standard,*)'====================================================='
     WRITE(lun_standard,*)''
  ENDIF

  !=====================!
  !- MPI: Finalization -!
  !=====================!
  CALL sub_mpi_finalize()

END PROGRAM prg_fsp
