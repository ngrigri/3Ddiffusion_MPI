!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - Ariane - (May - 2007)
!! 
!! bruno.blanke@univ-brest.fr and nicolas.grima@univ-brest.fr
!! 
!! This software is a computer program whose purpose is 
!! the computation of 3D streamlines in a given velocity field 
!! (as the output of an Ocean General Circulation Model) and 
!! subsequent water masses analyses.
!! 
!! This software is governed by the CeCILL license under French law and
!! abiding by the rules of distribution of free software.  You can  use, 
!! modify and/ or redistribute the software under the terms of the CeCILL
!! license as circulated by CEA, CNRS and INRIA at the following URL
!! "http://www.cecill.info". 
!! 
!! As a counterpart to the access to the source code and  rights to copy,
!! modify and redistribute granted by the license, users are provided only
!! with a limited warranty  and the software's author,  the holder of the
!! economic rights,  and the successive licensors  have only  limited
!! liability. 
!! 
!! In this respect, the user's attention is drawn to the risks associated
!! with loading,  using,  modifying and/or developing or reproducing the
!! software by the user in light of its specific status of free software,
!! that may mean  that it is complicated to manipulate,  and  that  also
!! therefore means  that it is reserved for developers  and  experienced
!! professionals having in-depth computer knowledge. Users are therefore
!! encouraged to load and test the software's suitability as regards their
!! requirements in conditions enabling the security of their systems and/or 
!! data to be ensured and,  more generally, to use and operate it in the 
!! same conditions as regards security. 
!! 
!! The fact that you are presently reading this means that you have had
!! knowledge of the CeCILL license and that you accept its terms.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_which_type

  USE mod_precision

  IMPLICIT NONE

  INTERFACE sub_which_type
    MODULE PROCEDURE &
         sub_I4_type, &
         sub_I8_type, &
         sub_R4_type, &
         sub_R8_type
  END INTERFACE

CONTAINS

  SUBROUTINE sub_I4_type(var, ctype)
    INTEGER(kind = ishort), INTENT(in)  :: var
    CHARACTER(len=2)      , INTENT(out) :: ctype

    ctype = 'I4'
  END SUBROUTINE sub_I4_type

  SUBROUTINE sub_I8_type(var, ctype)
    INTEGER(kind = ilong), INTENT(in)  :: var
    CHARACTER(len=2)     , INTENT(out) :: ctype

    ctype = 'I8'
  END SUBROUTINE sub_I8_type

  SUBROUTINE sub_R4_type(var, ctype)
    REAL(kind = rshort), INTENT(in)  :: var
    CHARACTER(len=2)   , INTENT(out) :: ctype

    ctype = 'R4'
  END SUBROUTINE sub_R4_type

  SUBROUTINE sub_R8_type(var, ctype)
    REAL(kind = rlong), INTENT(in)  :: var
    CHARACTER(len=2)  , INTENT(out) :: ctype

    ctype = 'R8'
  END SUBROUTINE sub_R8_type

END MODULE mod_which_type
