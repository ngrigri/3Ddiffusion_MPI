!! Nicolas.grima@univ-brest.fr
!! June and August 2015
!! Inspired from IDRIS  MPI TP8 course
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE mod_mpi

  !///////////////////!
  !- USE association -!
  !//////////////////=!
  USE mpi
  USE mod_precision
  USE mod_cst
  USE mod_lun
  USE mod_namelist

  !///////////////=!
  !- DECLARATIONS -!
  !///////////////=!
  IMPLICIT NONE

  !- PARAMETER -!
  INTEGER(kind = iprec), PARAMETER  :: NB_VOISINS = 6 ! Number of neighbours
  INTEGER(kind = iprec), PARAMETER  :: N=1, E=2, S=3, W=4, T=5, B=6
  INTEGER(kind = iprec), PARAMETER  :: etiquette = 1000

  !- INTEGER -!
  INTEGER(kind = iprec) :: rank             ! Rank of each processes
  INTEGER(kind = iprec) :: nbprocs          ! Number of processes
  INTEGER(kind = iprec) :: comm3d           ! Topology communicator
  INTEGER(kind = iprec) :: slide_for_west   !
  INTEGER(kind = iprec) :: slide_from_west  !
  INTEGER(kind = iprec) :: slide_for_est    !
  INTEGER(kind = iprec) :: slide_from_est   !
  INTEGER(kind = iprec) :: slide_for_north  ! 
  INTEGER(kind = iprec) :: slide_from_north !
  INTEGER(kind = iprec) :: slide_for_south  !
  INTEGER(kind = iprec) :: slide_from_south !
  INTEGER(kind = iprec) :: slide_xy         !
  INTEGER(kind = iprec) :: code             ! 
  INTEGER(kind = iprec) :: sx               !
  INTEGER(kind = iprec) :: ex               !
  INTEGER(kind = iprec) :: sy               !
  INTEGER(kind = iprec) :: ey               !
  INTEGER(kind = iprec) :: sz               !
  INTEGER(kind = iprec) :: ez               !
  INTEGER(kind = iprec) :: subdim_x         !
  INTEGER(kind = iprec) :: subdim_y         !
  INTEGER(kind = iprec) :: subdim_z         !
  INTEGER(kind = iprec) :: subdim_x_halo    !
  INTEGER(kind = iprec) :: subdim_y_halo    !
  INTEGER(kind = iprec) :: subdim_z_halo    !

  !- Arrays 1D -!
  INTEGER(kind = iprec), DIMENSION(NB_VOISINS)     :: voisin
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: coords
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: nbp
  INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims

  !- Logicals -!
  LOGICAL, DIMENSION(:), ALLOCATABLE :: periods
  LOGICAL :: mpi_verbose = .TRUE. ! Print info in fort.* files

CONTAINS

  !/////////////////////=!
  !- MPI INITIALIZATION -!
  !/////////////////////=!
  SUBROUTINE sub_mpi_initialisation()

    !----------------------!
    !- MPI initialization -!
    !----------------------!
    CALL MPI_INIT(code)

    !-------------------!
    !- To know my Rank -!
    !-------------------!
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, code)

    !---------------------------------------------!
    !- To know the number of processes available -!
    !---------------------------------------------!
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nbprocs, code)

    !-----------------------------!
    !- Dynamic memory allocation -!
    !-----------------------------!
    ALLOCATE(    nbp(ndims))
    ALLOCATE(   dims(ndims))
    ALLOCATE( coords(ndims))
    ALLOCATE(periods(ndims))

  END SUBROUTINE sub_mpi_initialisation

  !/////////////////////////////////////////////////////////////!
  !- MPI VERIFY THE NUMBER of PROCESSES AVAILABLE AND REQUIRED -!
  !/////////////////////////////////////////////////////////////!
  SUBROUTINE sub_mpi_verif_proc_avail()

    !--------------------------------------------------------------!
    !- Test if number of processes required = processes available -!
    !--------------------------------------------------------------!
    IF (nbprocs /= (nb_proc_x * nb_proc_y * nb_proc_z)) THEN
       WRITE(lun_error,*)'Error: Number of processes > &
            & nb_proc_x * nb_proc_y * nb_proc_z (namelist)'
       WRITE(lun_error,*)'Error: Number of processes = ', nbprocs
       WRITE(lun_error,*)'Error: nb_proc_x * nb_proc_y * nb_proc_z =', &
            nb_proc_x * nb_proc_y * nb_proc_z
       STOP
    ENDIF

  END SUBROUTINE sub_mpi_verif_proc_avail

  !//////////////////////////!
  !- MPI CARTESIAN TOPOLOGY -!
  !//////////////////////////!
  SUBROUTINE sub_mpi_topology()

    !- MPI constant -!
    LOGICAL, PARAMETER :: reorganisation = .FALSE.

    nbp(:)  = [nb_proc_x, nb_proc_y, nb_proc_z]
    dims(:) = [dim_x, dim_y, dim_z]

    !------------------------!
    !- Periodic grid or not -!
    !------------------------!
    periods(:) = .FALSE.

    !---------------------!
    !- Creating topology -!
    !---------------------!
    CALL MPI_CART_CREATE( &
         MPI_COMM_WORLD , &
         ndims          , &
         nbp(:)         , &
         periods        , &
         reorganisation , &
         comm3d         , &
         code           )

    !--------------------------------------!
    !- Print the value of each neighbours -!
    !--------------------------------------!
    IF (mpi_verbose) THEN
       WRITE(rank + lun_mpi,*) ''
       WRITE(rank + lun_mpi,*) 'Execution code 3Ddiffusion avec ', nbprocs, ' processus MPI'
       WRITE(rank + lun_mpi,*) 'Taille du domaine global: dim_x=', dim_x, 'dim_y=', dim_y, 'dim_z=', dim_z
       WRITE(rank + lun_mpi,*) 'Dimension de la topologie : ', &
            nbp(1), ' suivant x, ', nbp(2), ' suivant y', nbp(3), ' suivant z'
       WRITE(rank + lun_mpi,*) '----------------------------------------------------------------'
    END IF

  END SUBROUTINE sub_mpi_topology

  !//////////////////////////!
  !- MPI DOMAIN COORDINATES -!
  !//////////////////////////!
  SUBROUTINE sub_mpi_domain()

    !------------------------------------------!
    !- To know my coordinates in the topology -!
    !------------------------------------------!
    CALL MPI_CART_COORDS( &
         comm3d         , &
         rank           , &
         ndims          , &
         coords         , &
         code           )

    !----------------------------!
    !- To know my array indices -!
    !----------------------------!
    !- X -! (lines)
    sx = (coords(1)    *dims(1))/nbp(1)+1
    ex = ((coords(1)+1)*dims(1))/nbp(1)
    subdim_x = ex - sx + 1
    subdim_x_halo = subdim_x + 2 * halo_x

    !- Y -! (columns)
    sy = (coords(2)    *dims(2))/nbp(2)+1
    ey = ((coords(2)+1)*dims(2))/nbp(2)
    subdim_y = ey - sy + 1
    subdim_y_halo = subdim_y + 2 * halo_y

    !- Z -!
    sz = (coords(3)    *dims(3))/nbp(3)+1
    ez = ((coords(3)+1)*dims(3))/nbp(3)
    subdim_z = ez - sz + 1
    subdim_z_halo = subdim_z + 2 * halo_z

    IF (mpi_verbose) THEN
       WRITE(rank + lun_mpi,*) ''
       WRITE(rank + lun_mpi,*) 'Array indices (whithout Halo):'
       WRITE(rank + lun_mpi,*) '    - In x          :, ',sx, ex
       WRITE(rank + lun_mpi,*) '    - In y          :, ',sy, ey
       WRITE(rank + lun_mpi,*) '    - In z          :, ',sz, ez
       WRITE(rank + lun_mpi,*) '    - subdim_x      :', subdim_x
       WRITE(rank + lun_mpi,*) '    - subdim_y      :', subdim_y
       WRITE(rank + lun_mpi,*) '    - subdim_y      :', subdim_y
       WRITE(rank + lun_mpi,*) '    - subdim_x_halo :', subdim_x_halo
       WRITE(rank + lun_mpi,*) '    - subdim_y_halo :', subdim_y_halo
       WRITE(rank + lun_mpi,*) '    - subdim_y_halo :', subdim_y_halo
    ENDIF

  END SUBROUTINE sub_mpi_domain

  !//////////////////!
  !- MPI NEIGHBOURS -!
  !//////////////////!
  SUBROUTINE sub_mpi_neighbours()

    !------------------!
    !- Initialization -!
    !------------------!
    voisin(:) = MPI_PROC_NULL

    !-------------------------!
    !- To know my neighbours -!
    !-------------------------!
    !- North and South -!
    CALL MPI_CART_SHIFT(comm3d, 0, 1, voisin(N), voisin(S), code)

    !- West and East -!
    CALL MPI_CART_SHIFT(comm3d, 1, 1, voisin(W), voisin(E), code)

    !- Top and Bottom -!
    CALL MPI_CART_SHIFT(comm3d, 2, 1, voisin(B), voisin(T), code)

    IF (mpi_verbose) THEN
       WRITE(rank + lun_mpi,*) ''
       WRITE(rank + lun_mpi,*) 'My Neighbours:'
       WRITE(rank + lun_mpi,'(A,I3)') '    - North :', voisin(N)
       WRITE(rank + lun_mpi,'(A,I3)') '    - South :', voisin(S)
       WRITE(rank + lun_mpi,'(A,I3)') '    - West  :', voisin(W)
       WRITE(rank + lun_mpi,'(A,I3)') '    - East  :', voisin(E)
       WRITE(rank + lun_mpi,'(A,I3)') '    - Top   :', voisin(T)
       WRITE(rank + lun_mpi,'(A,I3)') '    - Bottom:', voisin(B)
    ENDIF

  END SUBROUTINE sub_mpi_neighbours

  !////////////////////!
  !- MPI DERIVED TYPE -!
  !////////////////////!
  SUBROUTINE sub_mpi_derived_type(f_p)

    REAL(kind=8), DIMENSION(:,:,:), POINTER, INTENT(in) :: f_p

    INTEGER(kind = iprec), DIMENSION(ndims) :: &
         array_shape                         , &
         subarray_shape                      , &
         starts

    array_shape(:) = SHAPE(f_p)

    IF (mpi_verbose) THEN
       WRITE(rank + lun_mpi,*)' '
       WRITE(rank + lun_mpi,*)'Derived Type (mpi):'
       WRITE(rank + lun_mpi,'(A,3(I5))') &
            '    - array shape (with halo)  :', array_shape(:)
       WRITE(rank + lun_mpi,'(A,3(I5))') &
            '    - halo_x, halo_y, halo_z   :', halo_x, halo_y, halo_z
    ENDIF

    !===========!
    !- W <-> E -!
    !===========!
    IF ((nb_proc_y > iOne).AND.(halo_y > iZero)) THEN

       subarray_shape(:) = [array_shape(1), halo_y, array_shape(3)]
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)''
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '  - W <-> E subarray shape     :', subarray_shape(:)
       ENDIF

       !----------------------------!
       ! ENVOI VERS LE VOISIN OUEST !
       !----------------------------!
       starts(:)         = [iZero, halo_y, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_for_west            , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_for_west, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '     - slide_for_west created  :', starts(:)
       ENDIF

       !---------------------------!
       ! RECEPTION DU VOISIN OUEST !
       !---------------------------!
       starts(:) = [iZero,  iZero, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_from_west           , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_from_west, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '     - slide_from_west created :', starts(:)
       ENDIF

       !--------------------------!
       ! ENVOI VERS LE VOISIN EST !
       !--------------------------!
       starts(:) = [iZero, array_shape(2) - 2 * halo_y, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_for_est             , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_for_est, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '     - slide_for_est created   :', starts(:)
       ENDIF

       !-------------------------!
       ! RECEPTION DU VOISIN EST !
       !-------------------------!
       starts(:) = [iZero, array_shape(2) - halo_y, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_from_est            , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_from_est, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '     - slide_from_est created  :', starts(:)
       ENDIF

    ENDIF

    !===========!
    !- N <-> S -!
    !===========!
    IF ((nb_proc_x > iOne).AND.(halo_x > iZero)) THEN

       subarray_shape(:) = [halo_x, array_shape(2), array_shape(3)]
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)''
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '  - N <-> S subarray shape     :', subarray_shape(:)
       ENDIF

       !---------------------------!
       ! ENVOI VERS LE VOISIN NORD !
       !---------------------------!
       starts(:)         = [halo_x, iZero, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_for_north           , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_for_north, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '    - slide_for_north created  :', starts(:)
       ENDIF

       !--------------------------!
       ! RECEPTION DU VOISIN NORD !
       !--------------------------!
       starts(:)         = [iZero,  iZero, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_from_north          , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_from_north, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '    - slide_from_north created :', starts(:)
       ENDIF

       !--------------------------!
       ! ENVOI VERS LE VOISIN SUD !
       !--------------------------!
       starts(:)         = [array_shape(1) - 2 * halo_x, iZero, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_for_south           , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_for_south, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '    - slide_for_south created  :', starts(:)
       ENDIF

       !-------------------------!
       ! RECEPTION DU VOISIN SUD !
       !-------------------------!
       starts(:)         = [array_shape(1) - halo_x, iZero, iZero]
       CALL MPI_TYPE_CREATE_SUBARRAY( &
            ndims                     , &
            array_shape(:)            , &
            subarray_shape(:)         , &
            starts(:)                 , &
            MPI_ORDER_FORTRAN         , &
            MPI_DOUBLE_PRECISION      , &
            slide_from_south          , &
            code                      )
       CALL MPI_TYPE_COMMIT(slide_from_south, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,'(A,3(I3))') &
               '    - slide_from_south created :', starts(:)
       ENDIF

    ENDIF

    !===========!
    !- T <-> B -!
    !===========!
    IF ((nb_proc_z > iOne).AND.(halo_z > iZero)) THEN

       !---------------------------------------------!
       ! ENVOI VERS LES VOISINS DU DESSUS et DESSOUS !
       !---------------------------------------------!
       CALL MPI_TYPE_CONTIGUOUS                     ( &
            array_shape(1) * array_shape(2) * halo_z, &
            MPI_DOUBLE_PRECISION                    , &
            slide_xy                                , &
            code                                    )
       CALL MPI_TYPE_COMMIT(slide_xy, code)
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - slide_xy created'
       ENDIF
    ENDIF

  END SUBROUTINE sub_mpi_derived_type

  !///////////////////////////////////////!
  !- MPI SEND-RECIEVE BETWEEN NEIGHBOURS -!
  !///////////////////////////////////////!
  SUBROUTINE sub_mpi_communication(f_p)

    REAL(kind=8), DIMENSION(:,:,:), POINTER, INTENT(inout) :: f_p

    !Constantes MPI
    INTEGER, DIMENSION(MPI_STATUS_SIZE)  :: statut

    IF (mpi_verbose) THEN
       WRITE(rank + lun_mpi,*)' '
       WRITE(rank + lun_mpi,*)'Communications MPI_SENDREC:'
    ENDIF

    mpi_verbose=.FALSE.

    !======================!
    !- WEST <-> EST (y/j) -!
    !======================!
    IF ((nb_proc_y > iOne).AND.(halo_y > iZero)) THEN
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'  - W -> E exchanges:'
       ENDIF
       !----------------------------------------------!
       !- Envoi au voisin W et reception du voisin W -!
       !----------------------------------------------!
       CALL MPI_SENDRECV   ( &
            f_p(:,:,:)     , &
            1              , &
            slide_for_west , &
            voisin(W)      , &
            etiquette      , &
            f_p(:,:,:)     , &
            1              , &
            slide_from_west, &
            voisin(W)      , &
            etiquette      , &
            MPI_COMM_WORLD , &
            statut         , &
            code           )

       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - West exchanges'
       ENDIF

       !----------------------------------------------!
       !- Envoi au voisin E et reception du voisin E -!
       !----------------------------------------------!
       CALL MPI_SENDRECV  ( &
            f_p(:,:,:)    , &
            1             , &
            slide_for_est , &
            voisin(E)     , &
            etiquette     , &
            f_p(:,:,:)    , &
            1             , &
            slide_from_est, &
            voisin(E)     , &
            etiquette     , &
            MPI_COMM_WORLD, &
            statut        , &
            code          )

       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - East exchanges'
       ENDIF
    ENDIF

    !========================!    
    !- North <-> South (x/i)-!
    !========================!
    IF ((nb_proc_x > iOne).AND.(halo_x > iZero)) THEN
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'  - N <-> S exchanges'
       ENDIF

       !----------------------------------------------!
       !- Envoi au voisin N et reception du voisin N -!
       !----------------------------------------------!
       CALL MPI_SENDRECV    ( &
            f_p             , &
            1               , &
            slide_for_north , &
            voisin(N)       , &
            etiquette       , &
            f_p             , &
            1               , &
            slide_from_north, &
            voisin(N)       , &
            etiquette       , &
            MPI_COMM_WORLD  , &
            statut          , &
            code            )

       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - North exchanges'
       ENDIF

       !----------------------------------------------!
       !- Envoi au voisin S et reception du voisin S -!
       !----------------------------------------------!
       CALL MPI_SENDRECV    ( &
            f_p             , &
            1               , &
            slide_for_south , &
            voisin(S)       , &
            etiquette       , &
            f_p             , &
            1               , &
            slide_from_south, &
            voisin(S)       , &
            etiquette       , &
            MPI_COMM_WORLD  , &
            statut          , &
            code            )

       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - South exchanges '
       ENDIF
    ENDIF

    !========================!
    !- TOP <-> Bottom (k/z) -!
    !========================!
    IF ((nb_proc_z > iOne).AND.(halo_z > iZero)) THEN
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'T <-> B exchanges'
       ENDIF
       !----------------------------------------------!
       !- Envoi au voisin T et reception du voisin T -!
       !----------------------------------------------!
       CALL MPI_SENDRECV        ( &
            f_p(:,:,sz)         , &
            1                   , &
            slide_xy            , &
            voisin(T)           , &
            etiquette           , &
            f_p(:,:,sz - halo_z), &
            1                   , &
            slide_xy            , &
            voisin(T)           , &
            etiquette           , &
            MPI_COMM_WORLD      , &
            statut              , &
            code                )

       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - Top exchanges'
       ENDIF

       !----------------------------------------------!
       !- Envoi au voisin B et reception du voisin B -!
       !----------------------------------------------!
       CALL MPI_SENDRECV    ( &
            f_p(:,:,ez - halo_z + iOne), &
            1               , &
            slide_xy        , &
            voisin(B)       , &
            etiquette       , &
            f_p(:,:,ez + iOne), &
            1               , &
            slide_xy        , &
            voisin(B)       , &
            etiquette       , &
            MPI_COMM_WORLD  , &
            statut          , &
            code            )
       IF (mpi_verbose) THEN
          WRITE(rank + lun_mpi,*)'    - Bottom exchanges'
       ENDIF

    ENDIF

  END SUBROUTINE sub_mpi_communication

  !///////////////////////!
  !- BOUNDARY CONDITIONS -!
  !///////////////////////!
  SUBROUTINE sub_mpi_boundary_conditions(ff)

    REAL(kind=8), DIMENSION(:,:,:), POINTER, INTENT(inout) :: ff

    IF (voisin(N) ==  MPI_PROC_NULL) THEN
       ff(:,sy-halo_y:sy-iOne,:) = ff(:,sy:sy+halo_y-iOne,:)
    ENDIF

    IF (voisin(S) ==  MPI_PROC_NULL) THEN
       ff(:,ey+iOne:ey+halo_y,:) = ff(:,ey-halo_y+iOne:ey,:)
    ENDIF

    IF (voisin(W) ==  MPI_PROC_NULL) THEN
       ff(sx-halo_x:sx-iOne,:,:) = ff(sx:sx+halo_x-iOne,:,:)
    ENDIF

    IF (voisin(E) ==  MPI_PROC_NULL) THEN
       ff(ex+iOne:ex+halo_x,:,:) = ff(ex-halo_x+iOne:ex,:,:)
    ENDIF

    IF (voisin(T) ==  MPI_PROC_NULL) THEN
       ff(:,:,sz-halo_z:sz-iOne) = ff(:,:,sz:sz+halo_z-iOne)
    ENDIF

    IF (voisin(B) ==  MPI_PROC_NULL) THEN
       ff(:,:,ez+iOne:ez+halo_z) = ff(:,:,ez-halo_z+iOne:ez)
    ENDIF

  END SUBROUTINE sub_mpi_boundary_conditions

  !====================!
  !- MPI FINALIZATION -!
  !====================!
  SUBROUTINE sub_mpi_finalize

    ! Desactivation de MPI
    CALL MPI_FINALIZE(code)

  END SUBROUTINE sub_mpi_finalize

  !==============!
  !- MPI MAXVAL -!
  !==============!
  SUBROUTINE sub_mpi_maxval(val, f_p)

    REAL(kind = rprec), INTENT(OUT) :: val
    REAL(kind = rprec), DIMENSION(:,:,:), POINTER, INTENT(IN) :: f_p

    CALL MPI_REDUCE(MAXVAL(f_p(sx:ex,sy:ey,sz:ez)), val, &
         iOne, MPI_DOUBLE_PRECISION, MPI_MAX, iZero, comm3d, code)

  END SUBROUTINE sub_mpi_maxval

END MODULE mod_mpi
