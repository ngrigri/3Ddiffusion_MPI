!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - mod_precision - (Month - Year)
!! 
!! author: Nicolas.Grima@univ-brest.fr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! This module is a necessary module to define the integer and
!! real precision.
!!
!! Remember that is it better to define explicitly the variable precision
!! in the code source than in the compiler options !!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_precision

  !================!
  !- DECLARATIONS -!
  !================!
  IMPLICIT NONE

  !===========================!
  !- INTEGER KIND DEFINITION -!
  !===========================!
  INTEGER(kind=4), PARAMETER :: &
       ishort=4               , & ! Here we decided that single precision is 4
       ilong =8                   ! and double precision is 8 
  !                               ! (IEEE norm, except some cray machines)

  INTEGER(kind=ishort), PARAMETER :: &
       iprec=ishort               ! We decided that single precsion for 
  !                               ! Integer variables is right.

  !========================!
  !- REAL KIND DEFINITION -!
  !========================!
  !- We use here the F90 intinsinc function KIND to determine the real precision -!
  INTEGER(kind=ishort), PARAMETER :: &
       rshort=KIND(1.0)            , & !  Real single precision
       rlong =KIND(1.0d0)              !  Real double precision

  !- Quad precision is defined here if it is required and available.
  !- Quad precision improve the stability of the results for the same
  !- compiler on different platforms (Intel versus AMD for example).
!!$#if defined quad
!!$  INTEGER(kind=ishort), PARAMETER :: qprec = selected_real_kind(p=33)
!!$#else
!!$  INTEGER(kind=ishort), PARAMETER :: qprec = rlong
!!$#endif

  !- By default compilation will be made in double-precision for real variables. -!
  INTEGER(kind=ishort), PARAMETER :: &
       rprec=rlong ! If needed, you can change rlong by rshort (not tested).

  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!

CONTAINS

  !********************************************************************!
  !- sub_machine_precision - (Month - Year)
  !
  !- contact: Author@mail
  !********************************************************************!
  !
  !
  !
  !
  !
  !********************************************************************!

  SUBROUTINE sub_machine_precision(lun) ! lun = logical unit number (opt)    

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: lun

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!
    INTEGER(kind = iprec) :: local_lun = 6_iprec

    IF (PRESENT(lun)) THEN
       local_lun = lun
    ENDIF

    WRITE(local_lun,*)''
    WRITE(local_lun,*)'====================='
    WRITE(local_lun,*)'= Machine precision ='
    WRITE(local_lun,*)'====================='
    WRITE(local_lun,*)'  -            iprec:', iprec
    WRITE(local_lun,*)'  -            rprec:', rprec
!!$    WRITE(local_lun,*)'  -            qprec:', qprec
    WRITE(local_lun,*)'  -     range(iprec):', RANGE(0_iprec)
    WRITE(local_lun,*)'  -     range(rprec):', RANGE(0._rprec)
    WRITE(local_lun,*)'  - Precision(rprec):', PRECISION(1.0_rprec)
    WRITE(local_lun,*)'  -   Spacing(rprec):', SPACING(1.0_rprec)
    WRITE(local_lun,*)'  -   EPSILON(rprec):', EPSILON(1.0_rprec)

!!$    IF (qprec > 8 ) THEN
!!$       WRITE(local_lun,*)''
!!$       WRITE(local_lun,*)'-- QUADRUPLE PRECISION IS ACTIVATED --'
!!$       WRITE(local_lun,*)'        quad value = ', qprec
!!$    ENDIF

  END SUBROUTINE sub_machine_precision

END MODULE mod_precision
!!***
