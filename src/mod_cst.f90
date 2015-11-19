!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- ApplicationName - (Month - Year)
!!
!!- contact: Author@Mail
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! All constants and/or parameters of the program 
!! have to be declared in this module...
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module mod_cst

  !===================!
  !- USE association -!
  !===================!
  USE mod_precision

  !=======================================!
  !- PARAMETER (or constant) DECLARATION -!
  !=======================================!
  IMPLICIT none

  !---------------------------------------!
  !- i before a constnat means "integer" -!
  !---------------------------------------!
  INTEGER(kind = iprec), PARAMETER :: iZero        =       0_iprec
  INTEGER(kind = iprec), PARAMETER :: iOne         =       1_iprec
  INTEGER(kind = iprec), PARAMETER :: iTwo         =       2_iprec
  INTEGER(kind = iprec), PARAMETER :: iThree       =       3_iprec
  INTEGER(kind = iprec), PARAMETER :: iFour        =       4_iprec
  INTEGER(kind = iprec), PARAMETER :: iFive        =       5_iprec
  INTEGER(kind = iprec), PARAMETER :: iSix         =       6_iprec
  INTEGER(kind = iprec), PARAMETER :: iSeven       =       7_iprec
  INTEGER(kind = iprec), PARAMETER :: iEight       =       8_iprec
  INTEGER(kind = iprec), PARAMETER :: iNine        =       9_iprec
  INTEGER(kind = iprec), PARAMETER :: iTen         =      10_iprec
  INTEGER(kind = iprec), PARAMETER :: iTwelve      =      12_iprec
  INTEGER(kind = iprec), PARAMETER :: iTwenty      =      20_iprec
  INTEGER(kind = iprec), PARAMETER :: iThirtyTwo   =      32_iprec
  INTEGER(kind = iprec), PARAMETER :: iSixtyFour   =      64_iprec
  INTEGER(kind = iprec), PARAMETER :: iOneHundred  =     100_iprec
  INTEGER(kind = iprec), PARAMETER :: iOneMillion  = 1000000_iprec
  INTEGER(kind = iprec), PARAMETER :: iHigh        = 9999999_iprec

  !------------------------------------!
  !- r before a constnat means "real" -!
  !------------------------------------!
  REAL(kind = rprec)   , PARAMETER :: rZero        =    0.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rHalf        =    0.5_rprec
  REAL(kind = rprec)   , PARAMETER :: rQuarter     =    0.25_rprec
  REAL(kind = rprec)   , PARAMETER :: rOne         =    1.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rTwo         =    2.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rThree       =    3.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rFour        =    4.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rFive        =    5.0_rprec
  REAL(kind = rprec)   , PARAMETER :: reight       =    8.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rTen         =   10.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rOneHundred  =  100.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rcircle      =  360.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rOneThousand = 1000.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rOneThousandTwentyFour =  1024.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rMax         = 1.e20_rprec

  !-----------------------------------------------------!
  !- Values to mask data or for NetCDF file mask value -!
  !-----------------------------------------------------!
  REAL(kind = rprec), PARAMETER :: mask_value    = rMax
  REAL(kind = rprec), PARAMETER :: missing_value = rMax

  !--------------------------------------------------!
  !- Default values for example the namelist values -!
  !--------------------------------------------------!
  INTEGER(kind=iprec), PARAMETER :: i_defp =  iOne
  INTEGER(kind=iprec), PARAMETER :: i_defn = -iOne
  INTEGER(kind=iprec), PARAMETER :: i_high = iHigh
  REAL(kind=rprec)   , PARAMETER :: r_def  = rZero
  CHARACTER(len = 4) , PARAMETER :: c_def  = 'NONE'
  LOGICAL            , PARAMETER :: l_def  = .FALSE.

  !------------------------------!
  !- Some geo-physical constant -!
  !------------------------------!
  REAL(kind=rprec)   , PARAMETER :: Pi = ACOS(-1._rprec)    ! in radian

  REAL(kind=rprec)   , PARAMETER :: earth_radius = 6378.137 ! at equator in km

end module mod_cst
