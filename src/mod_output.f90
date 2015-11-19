MODULE mod_output

  USE mod_precision
  USE mod_cst
  USE mod_lun
  USE mod_mpi
  USE mod_namelist
  USE mod_netcdf_output

  INTEGER(kind = iprec) :: ncid

  CHARACTER(len = 8)   , DIMENSION(:), ALLOCATABLE :: ncdims_name
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: ncdims
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: ncdims_id
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: start
  INTEGER(kind = iprec) :: var_id
  INTEGER(kind = iprec) :: nb_record
  CHARACTER(len = 16) :: var_name
  CHARACTER(len =  2) :: var_type

 CONTAINS

   !======================================================!
   !- NETCDF OUTPUT FILE(s): CREATION and INITIALIZATION -!
   !======================================================!
   SUBROUTINE mod_output_init()

     ALLOCATE(ncdims_name(ndims + iOne))
     ALLOCATE(     ncdims(ndims + iOne))
     ALLOCATE(  ncdims_id(ndims + iOne))
     ALLOCATE(      start(ndims + iOne))

     !-----------------!
     !- NETCDF OUTPUT -!
     !-----------------!
     CALL sub_netcdf_add_rank_in_filename(rank, ncFileName)

     CALL sub_netcdf_create(ncFileName, ncid, largeFile)

     ncdims_name(:) = [     'X',      'Y',      'Z',   'T']
     ncdims(:)      = [subdim_x, subdim_y, subdim_z, iZero]
     CALL sub_netcdf_dimensions( &
          ncid                 , &
          ncdims_name          , &
          ncdims               , &
          ncdims_id            )

     var_name = 'diffusion'
     var_type = 'R8'
     CALL sub_netcdf_var_and_att_def( &
          ncid                      , &
          var_name                  , &
          var_type                  , &
          ncdims_id                 , &
          var_id                    )

     CALL sub_netcdf_end_def(ncid)

     nb_record = iOne

   END SUBROUTINE mod_output_init

   !================================!
   !- WRITE DATA in NECTDF FILE(s) -!
   !================================!
   SUBROUTINE mod_output_write(r3Darray)

     REAL(kind= rprec), DIMENSION(:,:,:), INTENT(in) :: r3Darray

     start = [iOne, iOne, iOne, nb_record]

     CALL sub_netcdf_generic_write_var(ncid, var_id, r3Darray, start)

     nb_record = nb_record + iOne


   END SUBROUTINE mod_output_write

   !========================!
   !- CLOSE NECTDF FILE(s) -!
   !========================!
   SUBROUTINE mod_output_close()

     CALL sub_netcdf_close(ncid)
     nb_record = nb_record + iOne

   END SUBROUTINE mod_output_close

END MODULE mod_output
