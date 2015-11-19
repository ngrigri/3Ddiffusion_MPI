!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- ApplicationName - (Month - Year)
!!
!!- contact: Author@Mail
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! lun = Logical Unit Number
!!
!! Add here LUN used in the write or open routines
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_lun

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision

  !================!
  !- DECLARATIONS -!
  !================!
  IMPLICIT NONE

  INTEGER(kind=iprec), PARAMETER :: &
       lun_error       =  0       , & ! error messages
       lun_nml         =  4       , & ! namelist
       lun_standard    =  6       , & ! Fortran standard output
       lun_dbg         =  6       , & ! debuging messages
       lun_mpi         = 10       , & ! added to rank (rank + lun_mpi)
       lun_memory      = 60       , & ! memory log
       lun_dummy       = 99           ! a dummy L. U. N.

END MODULE mod_lun
!!***
