!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!- mod_Module_Name - (Month - Year)
!! (mod_Module_File_Name.f90 and mod_Module_Name have to be identical)
!!
!!- Author: Author@Mail
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! Everything for dynamic memory allocation
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_memory

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

  INTEGER(kind=ilong), PRIVATE :: &
       memory_r   = iZero       , & ! memory for allocatated real values
       memory_i   = iZero       , & ! memory for allocatated integer vales
       memory     = iZero       , & ! memory_r + memory_i
       memory_max = iZero

  !====================================!
  !- DECLARATIONS of INTERFACE BLOCKS -!
  !====================================!
  INTERFACE sub_memory_allocate
     MODULE PROCEDURE                      &
          sub_memory_allocate_i1D        , &
          sub_memory_allocate_i2D        , &
          sub_memory_allocate_r1D        , &
          sub_memory_allocate_r2D        , &
          sub_memory_allocate_r3D        , &
          sub_memory_allocate_r3D_pointer
  END INTERFACE sub_memory_allocate

  INTERFACE sub_memory_deallocate
     MODULE PROCEDURE                        &
          sub_memory_deallocate_i1D        , &
          sub_memory_deallocate_i2D        , &
          sub_memory_deallocate_r1D        , &
          sub_memory_deallocate_r2D        , &
          sub_memory_deallocate_r3D        , &
          sub_memory_deallocate_r3D_pointer
  END INTERFACE sub_memory_deallocate


CONTAINS

  !********************************************************************!
  !- sub_open_memory_file - (January - 2013)
  !
  !- contact: Author@mailNicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Open a memory log file
  !
  !********************************************************************!
  SUBROUTINE sub_open_memory_file()

    INTEGER(kind = iprec) :: ios

    OPEN (                      &
         UNIT = lun_memory    , & 
         FILE = 'memory.log'  , &
         FORM = "formatted"   , &
         ACCESS = "sequential", &
         ACTION = "write"     , &
         POSITION = "rewind"  , &
         IOSTAT = ios)

    IF (IOS /= 0 ) THEN
       WRITE(lun_error,*)':-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-('
       WRITE(lun_error,*)'mod_memory: problem to open ariane memory log file'
       WRITE(lun_error,*)':-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-('
       STOP
    ENDIF

    WRITE(UNIT = lun_memory, FMT = 1000 ) &
         'Total'                        , &
         'Real'                         , &
         'Integer'                      , &
         'Array size'                   , &
         'Array type'                   , &
         'Array Name'                   , &
         'Routine calling'

1000 FORMAT(A12,1x,A12,1x,A12,1x,A12,1x,A10,1x,A12,1x,A12)

    CALL sub_memory(0,' ','','')

  END SUBROUTINE sub_open_memory_file

  !********************************************************************!
  !- sub_memory - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Write memory information in the memory log file
  !
  ! to plot memory log file with matlab :
  !------------------------------------
  ! [memory, mem_r, mem_i, mem, tabname, subname] = ...
  ! textread('ariane_memory.log','%d %d %d %d %s %s',-1);
  ! plot((memory/1024)/1024);
  ! hold on
  ! plot ((mem_r/1024)/1024,'r');
  ! plot ((mem_i/1024)/1024,'g');
  ! title('Ariane memory log in MegaBytes (MB)');
  ! ylabel('Memory in MB');
  ! xlabel('Numbre of allocate and deallocate');
  !********************************************************************!
  SUBROUTINE sub_memory( &
       mem_size        , &
       mem_type        , &
       mem_var         , &
       mem_sub           )

    !! CALL sub_memory(size(),'','','')

    INTEGER(kind = iprec) :: mem_size
    CHARACTER(LEN = 1)    :: mem_type
    CHARACTER(LEN = *)    :: mem_var
    CHARACTER(LEN = *)    :: mem_sub

    CHARACTER(LEN=10)     :: array_type

    IF (TRIM(mem_type) == 'r') THEN
       array_type='integer'
       mem_size = mem_size * rprec
       memory_r = memory_r + mem_size
    ELSEIF (TRIM(mem_type) == 'i') THEN
       array_type='real'
       mem_size = mem_size * iprec
       memory_i = memory_i + mem_size
    ELSEIF (TRIM(mem_type) == ' ') THEN
       array_type='----------'
    ELSE
       STOP
    ENDIF

    memory = memory_r + memory_i

    IF (memory > memory_max )  memory_max=memory

    WRITE(UNIT = lun_memory, FMT = 1001 ) &
         memory          , &
         memory_r        , &
         memory_i        , &
         mem_size        , &
         TRIM(array_type), &
         TRIM(mem_var)   , &
         TRIM(mem_sub)

1001 FORMAT(I12,1x,I12,1x,I12,1x,I12,1x,A10,1x,A,1x,A)

  END SUBROUTINE sub_memory

  !********************************************************************!
  !- sub_close_memory_file - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Close the memory log file
  !
  !********************************************************************!
  SUBROUTINE sub_close_memory_file()

    CLOSE(UNIT = lun_memory)

    WRITE(lun_standard,*)''
    WRITE(lun_standard,FMT = 1002)'Maximum of allocated memory is (in octets): ', &
         memory_max

1002 FORMAT(A,1x,I12)

  END SUBROUTINE sub_close_memory_file

  !********************************************************************!
  !- sub_memory_allocate_i1D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for an integer vector
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_i1D( &
       array_i1D                    , &
       dim1                         )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: array_i1D
    INTEGER(kind = iprec), INTENT(in) :: dim1

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_i1D'

    IF (.NOT.(ALLOCATED(array_i1D))) THEN
       ALLOCATE(array_i1D(dim1))
       CALL sub_memory(SIZE(array_i1D),'i','array_i1D','sub_memory_allocate_i1D')
    ELSE
       IF (SIZE(array_i1D,1) == dim1) THEN
          WRITE(lun_standard,*)''
          WRITE(lun_standard,*)'-----------------------------------------'
          WRITE(lun_standard,*)'Warning: array i1D is already allocated !'
          WRITE(lun_standard,*)'         but the size is the same :-)    '
          WRITE(lun_standard,*)'-----------------------------------------'
       ELSE
          WRITE(lun_error,*)''
          WRITE(lun_error,*)'Error: array i1D is already allocated !'
          WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
          WRITE(lun_error,*)''
          STOP
       ENDIF
    ENDIF

  END SUBROUTINE sub_memory_allocate_i1D

  !********************************************************************!
  !- sub_memory_allocate_i2D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for an integer 2D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_i2D( &
       array_i2D                    , &
       dim1                         , &
       dim2                         )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array_i2D
    INTEGER(kind = iprec), INTENT(in)     :: dim1
    INTEGER(kind = iprec), INTENT(in)     :: dim2

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_i2D'

    IF (.NOT.(ALLOCATED(array_i2D))) THEN
       ALLOCATE(array_i2D(dim1,dim2))
       CALL sub_memory(SIZE(array_i2D),'i','array_i2D','sub_memory_allocate_i2D')
    ELSE
       IF ((SIZE(array_i2D,1) == dim1).AND.(SIZE(array_i2D,2) == dim2)) THEN
          WRITE(lun_standard,*)''
          WRITE(lun_standard,*)'-----------------------------------------'
          WRITE(lun_standard,*)'Warning: array i2D is already allocated !'
          WRITE(lun_standard,*)'         but the size is the same :-)    '
          WRITE(lun_standard,*)'-----------------------------------------'
       ELSE
          WRITE(lun_error,*)''
          WRITE(lun_error,*)'Error: array i2D is already allocated !'
          WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
          WRITE(lun_error,*)''
          STOP
       ENDIF
    ENDIF

  END SUBROUTINE sub_memory_allocate_i2D

  !********************************************************************!
  !- sub_memory_allocate_r1D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for a real vector
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_r1D( &
       array_r1D                    , &
       dim1                         )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL   (kind = rprec), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: array_r1D
    INTEGER(kind = iprec), INTENT(in) :: dim1

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_r1D'

    IF (.NOT.(ALLOCATED(array_r1D))) THEN
       ALLOCATE(array_r1D(dim1))
       CALL sub_memory(SIZE(array_r1D),'r','array_r1D','sub_memory_allocate_r1D')
    ELSE
       IF (SIZE(array_r1D,1) == dim1) THEN
          WRITE(lun_standard,*)''
          WRITE(lun_standard,*)'-----------------------------------------'
          WRITE(lun_standard,*)'Warning: array r1D is already allocated !'
          WRITE(lun_standard,*)'         but the size is the same :-)    '
          WRITE(lun_standard,*)'-----------------------------------------'
       ELSE
          WRITE(lun_error,*)''
          WRITE(lun_error,*)'Error: array r1D is already allocated !'
          WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
          WRITE(lun_error,*)''
          STOP
       ENDIF
    ENDIF

  END SUBROUTINE sub_memory_allocate_r1D

  !********************************************************************!
  !- sub_memory_allocate_r2D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for a real 2D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_r2D( &
       array_r2D                    , &
       dim1                         , &
       dim2                         )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL   (kind = rprec), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array_r2D
    INTEGER(kind = iprec), INTENT(in)     :: dim1
    INTEGER(kind = iprec), INTENT(in)     :: dim2

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_r2D'

    IF (.NOT.(ALLOCATED(array_r2D))) THEN
       ALLOCATE(array_r2D(dim1,dim2))
       CALL sub_memory(SIZE(array_r2D),'r','array_r2D','sub_memory_allocate_r2D')
    ELSE
       IF ((SIZE(array_r2D,1) == dim1).AND.(SIZE(array_r2D,2) == dim2)) THEN
          WRITE(lun_standard,*)''
          WRITE(lun_standard,*)'-----------------------------------------'
          WRITE(lun_standard,*)'Warning: array r2D is already allocated !'
          WRITE(lun_standard,*)'         but the size is the same :-)    '
          WRITE(lun_standard,*)'-----------------------------------------'
       ELSE
          WRITE(lun_error,*)''
          WRITE(lun_error,*)'Error: array r2D is already allocated !'
          WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
          WRITE(lun_error,*)''
          STOP
       ENDIF
    ENDIF

  END SUBROUTINE sub_memory_allocate_r2D

  !********************************************************************!
  !- sub_memory_allocate_r3D - (June - 2015)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for a real 3D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_r3D( &
       array_r3D                    , &
       dim1                         , &
       dim2                         , &
       dim3                         )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL   (kind = rprec), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: array_r3D
    INTEGER(kind = iprec), INTENT(in)     :: dim1
    INTEGER(kind = iprec), INTENT(in)     :: dim2
    INTEGER(kind = iprec), INTENT(in)     :: dim3

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_r2D'

    IF (.NOT.(ALLOCATED(array_r3D))) THEN
       ALLOCATE(array_r3D(dim1,dim2, dim3))
       CALL sub_memory(SIZE(array_r3D),'r','array_r3D','sub_memory_allocate_r3D')
    ELSE
       IF ((SIZE(array_r3D,1) == dim1).AND.(SIZE(array_r3D,2) == dim2).AND.(SIZE(array_r3D,3) == dim3)) THEN
          WRITE(lun_standard,*)''
          WRITE(lun_standard,*)'-----------------------------------------'
          WRITE(lun_standard,*)'Warning: array r3D is already allocated !'
          WRITE(lun_standard,*)'         but the size is the same :-)    '
          WRITE(lun_standard,*)'-----------------------------------------'
       ELSE
          WRITE(lun_error,*)''
          WRITE(lun_error,*)'Error: array r3D is already allocated !'
          WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
          WRITE(lun_error,*)''
          STOP
       ENDIF
    ENDIF

  END SUBROUTINE sub_memory_allocate_r3D

  !********************************************************************!
  !- sub_memory_allocate_r3D_pointer - (June - 2015)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! Allocate dynamic memory for a real 3D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_allocate_r3D_pointer( &
       array_r3D                            , &
       dim1                                 , &
       dim2                                 , &
       dim3                                 )

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL   (kind = rprec), DIMENSION(:,:,:), POINTER, INTENT(inout) :: array_r3D
    INTEGER(kind = iprec), INTENT(in)     :: dim1
    INTEGER(kind = iprec), INTENT(in)     :: dim2
    INTEGER(kind = iprec), INTENT(in)     :: dim3

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_allocate_r2D'

    !IF (.NOT.(ALLOCATED(array_r3D))) THEN
       ALLOCATE(array_r3D(dim1,dim2, dim3))
       CALL sub_memory(SIZE(array_r3D),'r','array_r3D','sub_memory_allocate_r3D')
    !ELSE
    !   IF ((SIZE(array_r3D,1) == dim1).AND.(SIZE(array_r3D,2) == dim2).AND.(SIZE(array_r3D,3) == dim3)) THEN
    !      WRITE(lun_standard,*)''
    !      WRITE(lun_standard,*)'-----------------------------------------'
    !      WRITE(lun_standard,*)'Warning: array r3D is already allocated !'
    !      WRITE(lun_standard,*)'         but the size is the same :-)    '
    !      WRITE(lun_standard,*)'-----------------------------------------'
    !   ELSE
    !      WRITE(lun_error,*)''
    !      WRITE(lun_error,*)'Error: array r3D is already allocated !'
    !      WRITE(lun_error,*)'Please submit sub_memory_deallocate before'
    !     WRITE(lun_error,*)''
    !      STOP
    !   ENDIF
    !ENDIF

  END SUBROUTINE sub_memory_allocate_r3D_pointer

  !********************************************************************!
  !- sub_memory_allocate_i1D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for an integer vector
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_i1D(array_i1D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: array_i1D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_i1D'

    IF (ALLOCATED(array_i1D)) THEN
       CALL sub_memory(-SIZE(array_i1D),'i','array_i1D','sub_memory_deallocate_i1D')
       DEALLOCATE(array_i1D)
    ENDIF

  END SUBROUTINE sub_memory_deallocate_i1D

  !********************************************************************!
  !- sub_memory_allocate_i2D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for an integer 2D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_i2D(array_i2D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    INTEGER(kind = iprec), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array_i2D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_i2D'

    IF (ALLOCATED(array_i2D)) THEN
       CALL sub_memory(-SIZE(array_i2D),'i','array_i2D','sub_memory_deallocate_i2D')
       DEALLOCATE(array_i2D)
    ENDIF

  END SUBROUTINE sub_memory_deallocate_i2D

  !********************************************************************!
  !- sub_memory_allocate_r1D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for a real vector
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_r1D(array_r1D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec), DIMENSION(:), ALLOCATABLE, INTENT(inout) :: array_r1D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_r1D'

    IF (ALLOCATED(array_r1D)) THEN
       CALL sub_memory(-SIZE(array_r1D),'r','array_r1D','sub_memory_deallocate_r1D')
       DEALLOCATE(array_r1D)
    ENDIF

  END SUBROUTINE sub_memory_deallocate_r1D

  !********************************************************************!
  !- sub_memory_allocate_r2D - (January - 2013)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for a real 2D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_r2D(array_r2D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec), DIMENSION(:,:), ALLOCATABLE, INTENT(inout) :: array_r2D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_r2D'

    IF (ALLOCATED(array_r2D)) THEN
       CALL sub_memory(-SIZE(array_r2D),'r','array_r2D','sub_memory_deallocate_r2D')
       DEALLOCATE(array_r2D)
    ENDIF

  END SUBROUTINE sub_memory_deallocate_r2D


  !********************************************************************!
  !- sub_memory_allocate_r3D - (June - 2015)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for a real 3D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_r3D(array_r3D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec), DIMENSION(:,:,:), ALLOCATABLE, INTENT(inout) :: array_r3D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_r=3D'

    IF (ALLOCATED(array_r3D)) THEN
       CALL sub_memory(-SIZE(array_r3D),'r','array_r3D','sub_memory_deallocate_r3D')
       DEALLOCATE(array_r3D)
    ENDIF

  END SUBROUTINE sub_memory_deallocate_r3D

  !********************************************************************!
  !- sub_memory_allocate_r3D_pointer - (June - 2015)
  !
  !- contact: Nicolas.Grima@univ-brest.fr
  !********************************************************************!
  !
  ! DeAllocate dynamic memory for a real 3D array
  !
  !********************************************************************!
  SUBROUTINE sub_memory_deallocate_r3D_pointer(array_r3D)

    !=============================!
    !- DECLARATIONS of ARGUMENTS -!
    !=============================!
    REAL(kind = rprec), DIMENSION(:,:,:), POINTER, INTENT(inout) :: array_r3D

    !=======================!
    != .oO) MAIN PART (Oo. =!
    !=======================!
    !!write(lun_dbg,*)' -db- Enter in sub_memory_deallocate_r=3D'

    !IF (ALLOCATED(array_r3D)) THEN
       CALL sub_memory(-SIZE(array_r3D),'r','array_r3D','sub_memory_deallocate_r3D')
       DEALLOCATE(array_r3D)
    !ENDIF

  END SUBROUTINE sub_memory_deallocate_r3D_pointer

END MODULE mod_memory

