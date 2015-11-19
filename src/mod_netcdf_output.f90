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
!=========================================================================
!!****h* pvmod/mod_netcdf_output
!! NAME
!!   mod_netcdf_output
!!
!! FUNCTION
!!   .
!!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
!!
!! AUTHOR
!!   * Origin  : Nicolas Grima
!! 
!! CREATION DATE
!!   * November 2005
!!
!! HISTORY
!!   Date (dd/mm/yyyy/) - Modification(s)
!!
!! ARGUMENTS
!!   * ncid (input): the netcdf file ID
!!
!! TODO
!!   
!!
!! USED BY
!!   
!!
!! SOURCE
!!=======================================================================
MODULE mod_netcdf_output

  USE netcdf

  USE mod_precision
  USE mod_cst
  USE mod_lun
  USE mod_which_type

  IMPLICIT NONE

  !------------------------------------------------------------------!
  !- This is a generic interface to add attributes to a variable    -!
  !- in a Necdf File.                                               -!
  !------------------------------------------------------------------!
  INTERFACE sub_netcdf_generic_put_att
    MODULE PROCEDURE                 &
         sub_netcdf_logical_put_att, &
         sub_netcdf_char_put_att   , &
         sub_netcdf_int_put_att    , &
         sub_netcdf_int1D_put_att  , &
         sub_netcdf_real_put_att   , &
         sub_netcdf_real1D_put_att
  END INTERFACE

  !------------------------------------------------------------------!
  !- This is a generic interface to write an integer or real variable
  ! (scalar, 1D, 2D, 3D) in a Necdf File.                        
  !------------------------------------------------------------------!
  INTERFACE sub_netcdf_generic_write_var
    MODULE PROCEDURE                           &
         sub_netcdf_write_1D_char_var        , &
         sub_netcdf_write_scal_int_var       , &
         sub_netcdf_write_1D_int_var         , &
         sub_netcdf_write_2D_int_var         , &
         sub_netcdf_write_3D_int_var         , &
         sub_netcdf_write_scal_real_var      , &
         sub_netcdf_write_1D_real_var        , &
         sub_netcdf_write_2D_real_var        , &
         sub_netcdf_write_3D_real_var        , &
         sub_netcdf_write_4D_real_var
  END INTERFACE
  !!***
CONTAINS

  SUBROUTINE sub_netcdf_add_rank_in_filename(rank, ncFileName)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in)    :: rank
    CHARACTER(len=*)   , INTENT(inout) :: ncFileName ! *.nc

    !- local variables -!
    INTEGER(kind = iprec), PARAMETER :: nb_digit = iFour
    INTEGER(kind = iprec) :: size_string
    CHARACTER(len=48)     :: cmft

    !-------------!
    ! Code begins !
    !-------------!
    size_string = LEN_TRIM(ncFileName)
    IF (ncFileName(size_string-iTwo:size_string) /= '.nc') THEN
       WRITE(lun_error,*)'Error: sub_netcdf_add_rank_in_filename'
       WRITE(lun_error,*)'NetCDF file name has to finish with .nc'
       WRITE(lun_error,*)' ncFileName(size_string-iTwo:size_string) =', &
            ncFileName(size_string-iTwo:size_string)
       STOP
    END IF

    WRITE(cmft, 1000) size_string-iThree, nb_digit, nb_digit
    WRITE(ncFileName,FMT=TRIM(cmft)) &
         ncFileName(1:size_string-iThree),'_',rank,'.nc'

1000 FORMAT('(A',I2,',A1,I',I2,'.',I2,',A3)')

  END SUBROUTINE sub_netcdf_add_rank_in_filename


  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_create()
  !! NAME
  !!   sub_netcdf_create()
  !!
  !! FUNCTION
  !!   Create a netcdf file.
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * INPUTS:
  !!       - c_filename: the netcdf file name.
  !!   * OUTPUT:
  !!       - ncid      : return the netcdf file ID.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_create (c_filename, ncid, large_file)
    USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    CHARACTER(len = *)    , INTENT(in)  :: c_filename
    INTEGER(kind  = iprec), INTENT(out) :: ncid
    LOGICAL, OPTIONAL, INTENT(in) :: large_file

    LOGICAL :: Logic_Large_File

    !- local variables -!
    INTEGER(kind = iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    !- Netcdf call -!

    IF (.NOT.PRESENT(large_file)) THEN
       Logic_Large_File = .FALSE.
    ELSE
       Logic_Large_File = large_file
    ENDIF

    IF (Logic_Large_File) THEN
       is_err = nf90_create(      &
            path  = c_filename  , & ! NetCDF filename created.
            cmode = NF90_64BIT_OFFSET, & ! 64_bits record, Large File
            ncid  = ncid          & ! NetCDF file identificator.
            )
       WRITE(*,*)' mod_netcdf_output: a 64BIT netcdf file is created'
    ELSE
       is_err = nf90_create(      &
            path  = c_filename  , & ! NetCDF filename created.
            cmode = NF90_CLOBBER, & ! 32_bits record, overwrite file
            ncid  = ncid          & ! NetCDF file identificator.
            )
    ENDIF


    !- Test if there's a problem during the NetCDF call -!
    IF (is_err /= nf90_noerr) THEN
       WRITE(LUN_ERROR,*) ''
       WRITE(LUN_ERROR,*) 'Error: mod_netcdf_output: sub_netcdf_create: problem to create netcdf file!'
       WRITE(LUN_ERROR,*) ' filename = ', TRIM(c_filename)
       STOP
       !!NG    ELSE
       !!NG      WRITE(*,*)''
       !!NG      WRITE(*,*)'------- Netcdf File - Successful Creation -------'
       !!NG      WRITE(*,*) TRIM(c_filename), ' -  Netcdf ID:', ncid
    ENDIF

  END SUBROUTINE sub_netcdf_create

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_dimensions()
  !! NAME
  !!   sub_netcdf_dimensions()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * INPUTS:
  !!       - 
  !!   * OUTPUT:
  !!       - 
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_dimensions(ncid, dims_name, dims, dims_id)
    USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)              , INTENT(in)  :: ncid
    
    CHARACTER(len = *)   , DIMENSION(:), INTENT(in)  :: dims_name
    INTEGER(kind = iprec), DIMENSION(:), INTENT(in)  :: dims
    INTEGER(kind = iprec), DIMENSION(:), INTENT(out) :: dims_id

    !- local variables -!
    INTEGER(kind = iprec) :: is_loop

    !-------------!
    ! Code begins !
    !-------------!
    DO is_loop = 1, SIZE(dims_name, dim=1)

      CALL sub_netcdf_1dim(ncid    , & 
           TRIM(dims_name(is_loop)), &
           dims(is_loop)           , &
           dims_id(is_loop)          &
           )

    ENDDO
      
  END SUBROUTINE sub_netcdf_dimensions

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_1dim()
  !! NAME
  !!   sub_netcdf_1dim()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * INPUTS:
  !!       - 
  !!   * OUTPUT:
  !!       - 
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_1dim(ncid, dim_name, dim, dim_id )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)          , INTENT(in)  :: ncid
    
    CHARACTER(len = *)  , INTENT(in)   :: dim_name
    INTEGER(kind = iprec), INTENT(in)  :: dim
    INTEGER(kind = iprec), INTENT(out) :: dim_id

    !- local variables -!
    INTEGER(kind = iprec) :: dim_size
    INTEGER(kind = iprec) :: is_err

    !! NG 29 april 2010
    IF (dim == 0) THEN
       dim_size= NF90_UNLIMITED
    ELSE
       dim_size = dim
    END IF
    !! NG 29 april 2010

    !-------------!
    ! Code begins !
    !-------------!
    is_err=NF90_def_dim(         &
         ncid  = ncid          , &
         name  = TRIM(dim_name), &
         len   = dim_size      , &
         dimid = dim_id          &
         )

    !- Test if there's a problem during the NetCDF call -!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_1dim: problem with Net&
           &cdf dimensions!'
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Dimension name  = ', TRIM(dim_name)
      WRITE(LUN_ERROR,*) ' Dimension value = ', dim
      STOP
!!NG    ELSE
!!NG      WRITE(*,*)''
!!NG      WRITE(*,*)'  - Dimension: ', TRIM(dim_name), ' is recorded in Ne&
!!NG           &tCDF file: ', ncid, 'with value ', dim
    ENDIF

  END SUBROUTINE sub_netcdf_1dim

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_var_and_att_def()
  !! NAME
  !!   sub_netcdf_var_and_att_def()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_var_and_att_def( &
       ncid             , &
       var_name         , &
       var_type         , &
       var_dimids       , &
       var_id           , &
       att_title        , &
       att_longname     , &
       att_units        , &
       att_valid_min    , &
       att_valid_max    , &
       att_missing_value  )
    USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)              , INTENT(in)  :: ncid
    CHARACTER(len = *)                 , INTENT(in)  :: var_name
    CHARACTER(len = 2)                 , INTENT(in)  :: var_type
    INTEGER(kind = iprec), DIMENSION(:), INTENT(in)  :: var_dimids
    INTEGER(kind = iprec)              , INTENT(out) :: var_id

    CHARACTER(len = *), OPTIONAL       , INTENT(in)  :: att_title
    CHARACTER(len = *), OPTIONAL       , INTENT(in)  :: att_longname
    CHARACTER(len = *), OPTIONAL       , INTENT(in)  :: att_units
    REAL(kind = rprec), OPTIONAL       , INTENT(in)  :: att_valid_min
    REAL(kind = rprec), OPTIONAL       , INTENT(in)  :: att_valid_max
    REAL(kind = rprec), OPTIONAL       , INTENT(in)  :: att_missing_value

    !- local variables -!
    INTEGER(kind = iprec) :: is_err
    INTEGER(kind = iprec) :: is_type

    !-------------!
    ! Code begins !
    !-------------!
    IF    (var_type == 'I4' ) THEN
       is_type = NF90_INT
    ELSEIF (var_type == 'I8' ) THEN
       WRITE(LUN_ERROR,*)'mod_netcdf_output: sub_netcdf_var_and_att_def: I8 is &
            &not supported by NetCDF ! We Stop.'
       STOP
    ELSEIF (var_type == 'R4' ) THEN
       is_type = NF90_FLOAT
    ELSEIF (var_type == 'R8' ) THEN
       is_type = NF90_DOUBLE
    ELSEIF (var_type == 'CC' ) THEN
       is_type = NF90_CHAR
    ELSE
       WRITE(LUN_ERROR,*)'mod_netcdf_output: sub_netcdf_var_and_att_def: ', &
            & var_type, ' is not supported by this module! We Stop.'
       STOP
    ENDIF

    !- NetCDF Call to Define a New Variable in a NetCDF File -!
    is_err=NF90_def_var(      &
         ncid   = ncid      , &
         name   = var_name  , &
         xtype  = is_type   , &
         dimids = var_dimids, &
         varid  = var_id      &
         )
    !- Test if there's a problem during the NetCDF call -!
    IF (is_err /= nf90_noerr) THEN
       WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_var_and_att_def: probl&
            &em with Netcdf var!' 
       WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
       WRITE(LUN_ERROR,*) ' Dimension name  = ', TRIM(var_name)
       WRITE(LUN_ERROR,*) ' Type            = ', var_type
       WRITE(LUN_ERROR,*) ' Dimensions      = ', var_dimids
       STOP
    ENDIF

    IF (PRESENT(att_title)) THEN
       CALL sub_netcdf_generic_put_att( &
            ncid     , &
            var_id   , &
            'title'  , &
            att_title  &
            )
    ENDIF

    IF (PRESENT(att_longname)) THEN
       CALL sub_netcdf_generic_put_att(&
            ncid        , &
            var_id      , &
            'longname'  , &
            att_longname  &
            )
    ENDIF

    IF (PRESENT(att_units)) THEN
       CALL sub_netcdf_generic_put_att(&
            ncid     , &
            var_id   , &
            'units'  , &
            att_units  &
            )
    ENDIF

    IF (PRESENT(att_valid_min)) THEN
       CALL sub_netcdf_generic_put_att(&
            ncid         , &
            var_id       , &
            'valid_min'  , &
            att_valid_min  &
            )
    ENDIF

    IF (PRESENT(att_valid_max)) THEN
       CALL sub_netcdf_generic_put_att(&
            ncid         , &
            var_id       , &
            'valid_max'  , &
            att_valid_max  &
            )
    ENDIF

    IF (PRESENT(att_missing_value)) THEN
       CALL sub_netcdf_generic_put_att(&
            ncid             , &
            var_id           , &
            'missing_value'  , &
            att_missing_value  &
            )
    ENDIF

  END SUBROUTINE sub_netcdf_var_and_att_def
  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_logicalput_att()
  !! NAME
  !!   sub_netcdf_logical_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_logical_put_att ( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value  &
       )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!    
    INTEGER(kind = iprec)          , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: var_id
    CHARACTER(len = *)             , INTENT(in) :: att_name
    LOGICAL                        , INTENT(in) :: att_value

    !- local variable -!
    CHARACTER(len = 7) :: c_log
    INTEGER(kind=iprec)  :: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!

    !- WARNING - WARNING - WARNING - WARNING - WARNING - WARNING -!
    !---------------------------------------------------------!
    !- It is not possible to put logical in netcdf Attribute -!
    !-                      ???? WHY ?????                   -!
    !- We tranform logical to character                      -!
    !---------------------------------------------------------!
    IF (att_value) THEN
      c_log = '.TRUE.'
    ELSE
      c_log = '.FALSE.'
    ENDIF
    !- WARNING - WARNING - WARNING - WARNING - WARNING - WARNING -!

    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err = NF90_put_att(     &
         ncid   = ncid       , &
         varid  = varid      , &
         name   = att_name   , &
         values = TRIM(c_log)  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_char_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', att_value
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_logical_put_att


  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_char_put_att()
  !! NAME
  !!   sub_netcdf_char_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_char_put_att ( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!    
    INTEGER(kind = iprec)          , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: var_id
    CHARACTER(len = *)             , INTENT(in) :: att_name
    CHARACTER(len = *)             , INTENT(in) :: att_value

    !- local variable -!
    INTEGER(kind=iprec)  :: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!
    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err=NF90_put_att(           &
         ncid   = ncid           , &
         varid  = varid          , &
         name   = att_name       , &
         values = TRIM(att_value)  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_char_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', TRIM(att_value)
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_char_put_att

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_char_put_att()
  !! NAME
  !!   sub_netcdf_char_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
!!$  SUBROUTINE sub_netcdf_char1D_put_att ( &
!!$       ncid     , &
!!$       var_id   , &
!!$       att_name , &
!!$       att_value  &
!!$       )
!!$  USE netcdf
!!$
!!$    !-------------!
!!$    ! Declaration !
!!$    !-------------!
!!$    !- arguments -!    
!!$    INTEGER(kind = iprec)           , INTENT(in) :: ncid
!!$    INTEGER(kind = iprec), OPTIONAL , INTENT(in) :: var_id
!!$    CHARACTER(len = *)              , INTENT(in) :: att_name
!!$    CHARACTER(len = *),  INTENT(in) :: att_value
!!$
!!$    !- local variable -!
!!$    INTEGER(kind=iprec)  :: is_err
!!$    INTEGER(kind = iprec) :: varid
!!$
!!$    !-------------!
!!$    ! Code begins !
!!$    !-------------!
!!$    !-----------------------------------------------------------!
!!$    !- If var_id is not define attribute is a global attribute -!
!!$    !-----------------------------------------------------------!
!!$    IF (present(var_id)) THEN
!!$      varid = var_id
!!$    ELSE
!!$      varid = NF90_GLOBAL
!!$    ENDIF
!!$
!!$    !---------------------------------------------------------------!
!!$    !- NetCDF Call to put attribute for this new variable recorded -!
!!$    !---------------------------------------------------------------!
!!$    is_err=NF90_put_att(              &
!!$         ncid   = ncid              , &
!!$         varid  = varid             , &
!!$         name   = att_name          , &
!!$         values = TRIM(att_value) )
!!$
!!$    !----------------------------------------------------!
!!$    !- Test if there's a problem during the NetCDF call -!
!!$    !----------------------------------------------------!
!!$    IF (is_err /= nf90_noerr) THEN
!!$      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_char_put_att: probl&
!!$           &em with Netcdf attribute!' 
!!$      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
!!$      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
!!$      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
!!$      WRITE(LUN_ERROR,*) ' Attribute value = ', TRIM(att_value)
!!$      STOP
!!$    ENDIF
!!$
!!$  END SUBROUTINE sub_netcdf_char1D_put_att


  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_int_put_att()
  !! NAME
  !!   sub_netcdf_int_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_int_put_att( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value  &
    )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)          , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: var_id
    CHARACTER(len = *)             , INTENT(in) :: att_name
    INTEGER(kind = iprec)          , INTENT(in) :: att_value

    !- local variable -!
    INTEGER(kind = iprec):: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!
    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err=NF90_put_att(     &
         ncid   = ncid     , &
         varid  = varid    , &
         name   = att_name , &
         values = att_value  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_int_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', att_value
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_int_put_att
  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_int1D_put_att()
  !! NAME
  !!   sub_netcdf_int1D_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * December 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_int1D_put_att( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value  &
    )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)              , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL    , INTENT(in) :: var_id
    CHARACTER(len = *)                 , INTENT(in) :: att_name
    INTEGER(kind = iprec), DIMENSION(:), INTENT(in) :: att_value

    !- local variable -!
    INTEGER(kind = iprec):: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!
    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err=NF90_put_att(     &
         ncid   = ncid     , &
         varid  = varid    , &
         name   = att_name , &
         values = att_value(:)  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_int_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', att_value(:)
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_int1D_put_att

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_real_put_att()
  !! NAME
  !!   sub_netcdf_real_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_real_put_att( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value  &
       )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)          , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: var_id
    CHARACTER(len = *)             , INTENT(in) :: att_name
    REAL(kind = rprec)             , INTENT(in) :: att_value

    !- local variable -!
    INTEGER(kind=iprec)  :: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!
    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err=NF90_put_att(     &
         ncid   = ncid     , &
         varid  = varid   , &
         name   = att_name , &
         values = att_value  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_real_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', att_value
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_real_put_att
  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_real1D_put_att()
  !! NAME
  !!   sub_netcdf_real1D_put_att()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * December 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_real1D_put_att( &
       ncid     , &
       var_id   , &
       att_name , &
       att_value  &
       )
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)           , INTENT(in) :: ncid
    INTEGER(kind = iprec), OPTIONAL , INTENT(in) :: var_id
    CHARACTER(len = *)              , INTENT(in) :: att_name
    REAL(kind = rprec), DIMENSION(:), INTENT(in) :: att_value

    !- local variable -!
    INTEGER(kind=iprec)  :: is_err
    INTEGER(kind = iprec) :: varid

    !-------------!
    ! Code begins !
    !-------------!
    !-----------------------------------------------------------!
    !- If var_id is not define attribute is a global attribute -!
    !-----------------------------------------------------------!
    IF (PRESENT(var_id)) THEN
      varid = var_id
    ELSE
      varid = NF90_GLOBAL
    ENDIF

    !---------------------------------------------------------------!
    !- NetCDF Call to put attribute for this new variable recorded -!
    !---------------------------------------------------------------!
    is_err=NF90_put_att(     &
         ncid   = ncid     , &
         varid  = varid   , &
         name   = att_name , &
         values = att_value(:)  &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_real_put_att: probl&
           &em with Netcdf attribute!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable ID     = ', varid
      WRITE(LUN_ERROR,*) ' Attribute name  = ', TRIM(att_name)
      WRITE(LUN_ERROR,*) ' Attribute value = ', att_value(:)
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_real1D_put_att

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_end_def()
  !! NAME
  !!   sub_netcdf_end_def()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_end_def (ncid)
  USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)          , INTENT(in) :: ncid

    !- local variable -!
    INTEGER(kind=iprec)  :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    !---------------------------------------------------------------!
    !- NetCDF Call to specified that dimension, variable definitions
    !- and variable attribute specification is finished and swith the
    !- netcdf file in Data mode.                                    
    !---------------------------------------------------------------!
    is_err =  NF90_enddef(ncid)

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_end_def: probl&
           &em with Netcdf EndDef!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid, ' Netcdf error :', is_err
      WRITE(LUN_ERROR,*) ' Try to resubmit your Ariane experiment with'
      WRITE(LUN_ERROR,*) ' output_netcdf_large_file = .TRUE. in the namelist file (ARIANE item)'
      STOP
    ELSE
      WRITE(*,*)'  --- Dimension definitions are finished ---'
    ENDIF

  END SUBROUTINE sub_netcdf_end_def

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_1D_int_var()
  !! NAME
  !!   sub_netcdf_write_1D_int_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_1D_char_var( &
       ncid  , &
       varid , &
       values  )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)         , INTENT(in) :: ncid
    INTEGER(kind = iprec)         , INTENT(in) :: varid
    CHARACTER(len=*), DIMENSION(:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var( &
         ncid   = ncid   , &
         varid  = varid  , &
         values = values )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_1D_char_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' Error number    = ', is_err
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(:)
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_write_1D_char_var
  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_scal_int_var()
  !! NAME
  !!   sub_netcdf_write_scal_int_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_scal_int_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec), INTENT(in) :: ncid
    INTEGER(kind = iprec), INTENT(in) :: varid
    INTEGER(kind = iprec), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(    &
         ncid   = ncid      , &
         varid  = varid     , &
         values = values      &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_scal_int_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_write_scal_int_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_1D_int_var()
  !! NAME
  !!   sub_netcdf_write_1D_int_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_1D_int_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)              , INTENT(in) :: ncid
    INTEGER(kind = iprec)              , INTENT(in) :: varid
    INTEGER(kind = iprec), DIMENSION(:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(       &
         ncid   = ncid         , &
         varid  = varid        , &
         values = values         &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_1D_int_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1)
      STOP
    ENDIF

  END SUBROUTINE sub_netcdf_write_1D_int_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_2D_int_var()
  !! NAME
  !!   sub_netcdf_write_2D_int_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_2D_int_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)                , INTENT(in) :: ncid
    INTEGER(kind = iprec)                , INTENT(in) :: varid
    INTEGER(kind = iprec), DIMENSION(:,:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(          &
         ncid   = ncid            , &
         varid  = varid           , &
         values = values            &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_2D_int_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1,1)
      STOP
    ENDIF
   
  END SUBROUTINE sub_netcdf_write_2D_int_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_3D_int_var()
  !! NAME
  !!   sub_netcdf_write_3D_int_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_3D_int_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)                  , INTENT(in) :: ncid
    INTEGER(kind = iprec)                  , INTENT(in) :: varid
    INTEGER(kind = iprec), DIMENSION(:,:,:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(             &
         ncid   = ncid               , &
         varid  = varid              , &
         values = values               &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_3D_int_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1,1,1)
      STOP
    ENDIF
     
  END SUBROUTINE sub_netcdf_write_3D_int_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_scal_real_var()
  !! NAME
  !!   sub_netcdf_write_scal_real_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_scal_real_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec), INTENT(in) :: ncid
    INTEGER(kind = iprec), INTENT(in) :: varid
    REAL(kind = rprec)   , INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(    &
         ncid   = ncid      , &
         varid  = varid     , &
         values = values      &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_scal_real_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values
      STOP
    ENDIF
 
  END SUBROUTINE sub_netcdf_write_scal_real_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_1D_real_var()
  !! NAME
  !!   sub_netcdf_write_1D_real_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_1D_real_var( &
       ncid  , &
       varid , &
       values, &
       start   &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)              , INTENT(in) :: ncid
    INTEGER(kind = iprec)              , INTENT(in) :: varid
    REAL(kind = rprec)   , DIMENSION(:), INTENT(in) :: values

    INTEGER(kind=iprec), DIMENSION(:), OPTIONAL, INTENT(in) :: start
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    IF (PRESENT(start)) THEN
    is_err = NF90_put_var(  &
         ncid   = ncid    , &
         varid  = varid   , &
         values = values  , &
         start  = start     &
         )
    ELSE
    is_err = NF90_put_var(  &
         ncid   = ncid    , &
         varid  = varid   , &
         values = values    &
         )
    END IF

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_1D_real_var: &
           &problem with Netcdf put var!', is_err
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1)
      STOP
    ENDIF
   
  END SUBROUTINE sub_netcdf_write_1D_real_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_2D_real_var()
  !! NAME
  !!   sub_netcdf_write_2D_real_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_2D_real_var( &
       ncid  , &
       varid , &
       values  &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)                , INTENT(in) :: ncid
    INTEGER(kind = iprec)                , INTENT(in) :: varid
    REAL(kind = rprec)   , DIMENSION(:,:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(          &
         ncid   = ncid            , &
         varid  = varid           , &
         values = values            &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_2D_real_var: &
           &problem with Netcdf put var!', is_err
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1,1)
      STOP
    ENDIF
     
  END SUBROUTINE sub_netcdf_write_2D_real_var

  !!***  
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_3D_real_var()
  !! NAME
  !!   sub_netcdf_write_3D_real_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_3D_real_var( &
       ncid  , &
       varid , &
       values, &
       start   &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)                  , INTENT(in) :: ncid
    INTEGER(kind = iprec)                  , INTENT(in) :: varid
    INTEGER(kind = iprec), DIMENSION(:)    , INTENT(in) :: start
    REAL(kind = rprec)   , DIMENSION(:,:,:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(             &
         ncid   = ncid               , &
         varid  = varid              , &
         values = values             , &
         start  = start                &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_3D_real_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' Error           = ', is_err
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Values          = ', values(1,1,1)
      STOP
    ENDIF
     
  END SUBROUTINE sub_netcdf_write_3D_real_var

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_write_4D_real_var()
  !! NAME
  !!   sub_netcdf_write_4D_real_var()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2008
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_write_4D_real_var( &
       ncid  , &
       varid , &
       values, &
       start   &
       )
  USE netcdf
    
    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec)                    , INTENT(in) :: ncid
    INTEGER(kind = iprec)                    , INTENT(in) :: varid
    INTEGER(kind = iprec), DIMENSION(:)      , INTENT(in) :: start
    REAL(kind = rprec)   , DIMENSION(:,:,:,:), INTENT(in) :: values
    
    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    is_err = NF90_put_var(             &
         ncid   = ncid               , &
         varid  = varid              , &
         values = values             , &
         start  = start                &
         )

    !----------------------------------------------------!
    !- Test if there's a problem during the NetCDF call -!
    !----------------------------------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_write_4D_real_var: &
           &problem with Netcdf put var!' 
      WRITE(LUN_ERROR,*) ' Error           = ', is_err
      WRITE(LUN_ERROR,*) ' NetCDF file ID  = ', ncid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', varid
      WRITE(LUN_ERROR,*) ' Variable    ID  = ', start
      WRITE(LUN_ERROR,*) ' Values          = ', values(1,1,1,1)
      STOP
    ENDIF
     
  END SUBROUTINE sub_netcdf_write_4D_real_var

  !!***
  !=========================================================================
  !!****f* mod_netcdf_output/sub_netcdf_close()
  !! NAME
  !!   sub_netcdf_close()
  !!
  !! FUNCTION
  !!   Close a netcdf file.
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_close(ncid)
    USE netcdf

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in) :: ncid

    !- local variables -!
    INTEGER(kind=iprec) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    !- Netcdf call -!
    is_err = nf90_close(ncid = ncid)

    !- Test if there's a problem -!
    IF (is_err /= nf90_noerr) THEN
       WRITE(LUN_ERROR,*) 'mod_netcdf_output: sub_netcdf_close: problem to close netcdf file!'
       WRITE(LUN_ERROR,*) ' ncid = ', ncid, ' error: ', is_err
       WRITE(LUN_ERROR,*) ' Try to resubmit your Ariane experiment with'
       WRITE(LUN_ERROR,*) ' output_netcdf_large_file = .TRUE. in the namelist file (ARIANE item)'
       STOP
       !!NG     ELSE
       !!NG      WRITE(*,*)''
       !!NG      WRITE(*,*)'------- Netcdf File - Successful Closing Netcdf ID:'&
       !!NG           &, ncid, '-------'
    ENDIF

  END SUBROUTINE sub_netcdf_close

!!***
END MODULE mod_netcdf_output
