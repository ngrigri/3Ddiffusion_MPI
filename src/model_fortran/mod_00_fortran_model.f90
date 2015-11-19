!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- mod_Module_Name - (Month - Year)
!! (mod_Module_File_Name.f90 and mod_Module_Name have to be identical)
!!
!!- Author: Author@Mail
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_Module_Name

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

  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!

CONTAINS

  !********************************************************************!
  !- sub_Subroutine_Name - (Month - Year)
  !
  !- contact: Author@mail
  !********************************************************************!
  !
  !
  !
  !********************************************************************!

  SUBROUTINE sub_Subroutine_Name( &
       arg_Name1                , & ! input  argument, name, unit, etc
       arg_Name2                )   ! output argument, name, unit, etc

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL   (kind = rprec), INTENT(in)  :: arg_name1
    INTEGER(kind = iprec), INTENT(out) :: arg_name2

    !===================================!
    !- DECLARATIONS of LOCAL VARIABLES -!
    !===================================!

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_Subroutine_Name'

    !======================!
    !- TITLE of a SECTION -!
    !======================!

    !-------------------------!
    !- SUBTITLE of a SECTION -!
    !-------------------------!

    !- comments -!

    !!write(lun_dbg,*)' -db- Exit sub_Subroutine_Name'

  END SUBROUTINE sub_Subroutine_Name

END MODULE mod_Module_Name
