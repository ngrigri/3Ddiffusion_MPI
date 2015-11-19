!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- mod_namelist - (January - 2013)
!!
!!- Author: Nicolas.Grima@univ-brest.fr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_namelist

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision
  USE mod_cst
  USE mod_lun

  !===============================!
  !- DECLARATIONS of GLOBAL DATA -!
  !===============================!
  IMPLICIT NONE

  !----------!
  ! FSP item !
  !----------!
  !- FSP item pramaters -!
  logical               :: ncoutput
  INTEGER(kind = iprec) :: dim_x       = 64
  INTEGER(kind = iprec) :: halo_x      = iOne
  INTEGER(kind = iprec) :: dim_y       = 64
  INTEGER(kind = iprec) :: halo_y      = iOne
  INTEGER(kind = iprec) :: dim_z       = 64
  INTEGER(kind = iprec) :: halo_z      = iOne

  REAL(kind = rprec) :: time_max= 0.1_rprec

  NAMELIST/PARAM/ & !
       ncoutput , &
       time_max , &
       dim_x    , &
       dim_y    , &
       dim_z    , &
       halo_x   , &
       halo_y   , &
       halo_z     

  INTEGER(kind = iprec) :: ndims     = iThree
  INTEGER(kind = iprec) :: nb_proc_x = iOne
  INTEGER(kind = iprec) :: nb_proc_y = iOne
  INTEGER(kind = iprec) :: nb_proc_z = iOne

  NAMELIST/NML_MPI/ &
       ndims      , &
       nb_proc_x  , &
       nb_proc_y  , &
       nb_proc_z

  CHARACTER(len = 256)  :: ncFileName = '3Ddiff_output.nc'
  INTEGER(kind = iprec) :: frequency  = iOneHundred
  LOGICAL               :: largeFile = .FALSE.

  NAMELIST/NC_OUTPUT/ &
       ncFileName   , &
       frequency    , &
       largeFile

  REAL(kind = rprec) :: kappa, dt, dx, dy, dz, time, L
       
  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!

CONTAINS

  !********************************************************************!
  !- sub_read_namelist - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-best.fr
  !********************************************************************!
  !
  !
  !
  !
  !
  !********************************************************************!

  SUBROUTINE sub_read_namelist( &
       verbose                 ) ! optional argument to select verbose mode or not

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    LOGICAL, OPTIONAL, INTENT(in) :: verbose

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    LOGICAL :: print_info

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_read_namelist'

    !- Test if the optional argument is present -!
    IF (PRESENT(verbose)) THEN
       IF (verbose) THEN
          print_info = .TRUE.
       ELSE
          print_info = .FALSE.
       ENDIF
    ELSE
       print_info = .FALSE.
    ENDIF

    !- print message in lun_standard -!
    IF (print_info) THEN
       WRITE(lun_standard,*)''
       WRITE(lun_standard,*)'============'
       WRITE(lun_standard,*)'= NAMELIST ='
       WRITE(lun_standard,*)'============'
    ENDIF
    !-----------------------------!
    !- Opening the namelist file -!
    !-----------------------------!
    OPEN(unit=lun_nml, File='namelist', ACTION='READ')
    WRITE(lun_standard,*)'--- Successful Opening ---'

    !------------------------------------!
    !- Reading namelist file item PARAM -!
    !------------------------------------!
    IF (print_info) THEN
       WRITE(lun_standard,*)''
       WRITE(lun_standard,*)'- Reading PARAM item:'
       WRITE(lun_standard,*)' '
       WRITE(lun_standard,*)'  - PARAM:'
    ENDIF
    REWIND(unit=lun_nml)
    READ(unit=lun_nml, nml=PARAM)


    IF (print_info) THEN
       WRITE(lun_standard,*)'   - ncoutput              =', ncoutput
       WRITE(lun_standard,*)'   - time_max              =', time_max
       WRITE(lun_standard,*)'   - dim_x (global domain) =', dim_x
       WRITE(lun_standard,*)'   - dim_y (global domain) =', dim_y
       WRITE(lun_standard,*)'   - dim_z (global domain) =', dim_z
       WRITE(lun_standard,*)'   - halo_x                =', halo_x
       WRITE(lun_standard,*)'   - halo_y                =', halo_y
       WRITE(lun_standard,*)'   - halo_z                =', halo_z
    ENDIF


    IF ((halo_x < iOne).OR.(halo_y < iOne).OR.(halo_z < iOne)) THEN
       WRITE(lun_error,*)'Error: halo_x, halo_y, halo_z &
            & have to be greater than 0 (zero).'
       STOP
    ENDIF

    IF (print_info) THEN
       WRITE(lun_standard,*)' '
       WRITE(lun_standard,*)'  - NML_MPI:'
    ENDIF
    REWIND(unit=lun_nml)
    READ(unit=lun_nml, nml=NML_MPI)

    IF (print_info) THEN
       WRITE(lun_standard,*)'   - ndims = ', ndims
       WRITE(lun_standard,*)'   - nb_proc_x =', nb_proc_x
       WRITE(lun_standard,*)'   - nb_proc_y =', nb_proc_y
       WRITE(lun_standard,*)'   - nb_proc_z =', nb_proc_z
    ENDIF

    IF (nb_proc_x < iOne) THEN
       WRITE(lun_error,*)'Error: nb_proc_x < iOne !'
       WRITE(lun_error,*)'Error: nb_proc_x = ', nb_proc_x
       STOP
    ENDIF

    IF (nb_proc_y < iOne) THEN
       WRITE(lun_error,*)'Error: nb_proc_y < iOne !'
       WRITE(lun_error,*)'Error: nb_proc_y = ', nb_proc_y
       STOP
    ENDIF

    IF (nb_proc_z < iOne) THEN
       WRITE(lun_error,*)'Error: nb_proc_z < iOne !'
       WRITE(lun_error,*)'Error: nb_proc_z = ', nb_proc_z
       STOP
    ENDIF

    IF (mod(dim_x,nb_proc_x) /= iZero) THEN
       WRITE(lun_error,*)'Error: mod(dim_x,nb_proc_x) /= iZero !'
       WRITE(lun_error,*)'Error: dim_x = ', dim_x
       WRITE(lun_error,*)'Error: nb_proc_x =', nb_proc_x
       WRITE(lun_error,*)'Error:  mod(dim_x,nb_proc_x)=',  &
            mod(dim_x,nb_proc_x)
       STOP
    ENDIF

    IF (mod(dim_y,nb_proc_y) /= iZero) THEN
       WRITE(lun_error,*)'Error: mod(dim_y,nb_proc_y) /= iZero !'
       WRITE(lun_error,*)'Error: dim_y = ', dim_y
       WRITE(lun_error,*)'Error: nb_proc_y =', nb_proc_y
       WRITE(lun_error,*)'Error:  mod(dim_y,nb_proc_y)=',  &
            mod(dim_y,nb_proc_y)
       STOP
    ENDIF

    IF (mod(dim_z,nb_proc_z) /= iZero) THEN
       WRITE(lun_error,*)'Error: mod(dim_z,nb_proc_z) /= iZero !'
       WRITE(lun_error,*)'Error: dim_z = ', dim_z
       WRITE(lun_error,*)'Error: nb_proc_z =', nb_proc_z
       WRITE(lun_error,*)'Error:  mod(dim_z,nb_proc_z)=',  &
            mod(dim_z,nb_proc_z)
       STOP
    ENDIF

    IF (ncoutput) THEN

       IF (print_info) THEN
          WRITE(lun_standard,*)' '
          WRITE(lun_standard,*)'  - NC_OUTPUT:'
       ENDIF

       REWIND(unit=lun_nml)
       READ(unit=lun_nml, nml=NC_OUTPUT)

       IF (print_info) THEN
          WRITE(lun_standard,*)' '
          WRITE(lun_standard,*)'   - ncFileName =', TRIM(ncFileName)
          WRITE(lun_standard,*)'   - Frequency  =', frequency
          WRITE(lun_standard,*)'   - LargeFile  =', largeFile
       ENDIF

    ENDIF

    !!write(lun_dbg,*)' -db- Exit sub_read_namelist'

    CALL sub_namelist_initialization()

  END SUBROUTINE sub_read_namelist


  SUBROUTINE sub_namelist_initialization()

    !- some initialization -! (namelist ? or mod_cst.f90 ?)
    L     = rOne
    dx    = L/REAL(dim_x - iTwo, kind = rprec)  !! -2 is not generic suppose halo_x = 1 !!
    dy    = L/REAL(dim_y - iTwo, kind = rprec)
    dz    = L/REAL(dim_z - iTwo, kind = rprec)
    kappa = 0.1_rprec
    dt    = 0.1_rprec * dx * dx / kappa;
    time  = rZero

  END SUBROUTINE sub_namelist_initialization

END MODULE mod_namelist
