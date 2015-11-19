MODULE mod_3Ddiffusion

  USE mod_precision
  USE mod_configure
  USE mod_cst
  USE mod_lun
  USE mod_namelist
  USE mod_memory
  USE mod_mpi
  USE mod_output

CONTAINS

  !====================================!
  !- ARRAY INITILIZATON (First guess) -!
  !====================================!
  SUBROUTINE sub_3Ddiff_initialisation(f_p,fn_p)

    IMPLICIT NONE
    REAL(kind = rprec), INTENT(out), DIMENSION(:, :, :), POINTER :: f_p
    REAL(kind = rprec), INTENT(out), DIMENSION(:, :, :), POINTER :: fn_p

    INTEGER(kind =iprec) :: ix, iy, iz
    REAL(kind = rprec) :: xx, yy, zz, kk
    REAL(kind = rprec) :: maxval_fp

    ALLOCATE( f_p(sx-halo_x:ex+halo_x, sy-halo_y:ey+halo_y, sz-halo_z:ez+halo_z))
    ALLOCATE(fn_p(sx-halo_x:ex+halo_x, sy-halo_y:ey+halo_y, sz-halo_z:ez+halo_z))

    fn_p(:,:,:) = rZero

    kk = rTwo*Pi

	DO iz = sz-1, ez+1
       DO iy = sy-1, ey+1
          DO ix = sx-1, ex+1

             xx = dx*(REAL(ix - iOne - halo_x, kind=rprec) + rHalf)
             yy = dy*(REAL(iy - iOne - halo_y, kind=rprec) + rHalf)
             zz = dz*(REAL(iz - iOne - halo_z, kind=rprec) + rHalf)

             f_p(ix,iy,iz)           = &
                  0.125_rprec        * &
                  (rOne - COS(kk*xx))* &
                  (rOne - COS(kk*yy))* &
                  (rOne - COS(kk*zz))
          END DO
       END DO
	END DO

  END SUBROUTINE sub_3Ddiff_initialisation

  !============!
  !- MAINLOOP -!
  !============!
  SUBROUTINE sub_3Ddiff_mainloop(f_p, fn_p)

    IMPLICIT NONE

    REAL(kind = rprec), INTENT(inout), DIMENSION(:, :, :), POINTER :: f_p
    REAL(kind = rprec), INTENT(inout), DIMENSION(:, :, :), POINTER :: fn_p

    REAL(kind = rprec), DIMENSION(:, :, :), POINTER :: temp_p
    REAL(kind = rprec) :: coeff_east_west, coeff_north_south, coeff_top_bottom, coeff_center
    REAL(kind = rprec) :: maxval_fp, maxval_fnp
    INTEGER(kind = iprec) :: icnt
	INTEGER(kind = iprec) :: ix, iy, iz

    coeff_east_west   = kappa*dt/(dx*dx)
    coeff_north_south = kappa*dt/(dy*dy)
    coeff_top_bottom  = kappa*dt/(dz*dz)
    coeff_center      = rOne - (rTwo * coeff_east_west + rTwo * coeff_north_south + rTwo * coeff_top_bottom)

    IF (rank == iZero) THEN

       WRITE(lun_standard,*)''
       WRITE(lun_standard,*) "3D Diffusion (fortran)"
       WRITE(lun_standard,*)''
       WRITE(lun_standard,"(A, I4, A, I4, A, I4, A,E13.5,A,E13.5,A,E13.5)") &
            "X:", dim_x, ", Y:", dim_y, ", Z:", dim_z, ", &
            kappa:", kappa, ", dt:", dt, ", dx:", dx
       WRITE(*,*)" coeffs:", coeff_east_west, coeff_north_south, coeff_top_bottom, coeff_center
       WRITE(lun_standard,*)''

    ENDIF

    !-------------------------!
    !- MAIN Loop (time loop) -!
    !-------------------------!
    icnt = iZero
    time = rZero

    CALL sub_mpi_maxval(maxval_fp, f_p)

    IF (rank == iZero) THEN
       WRITE(lun_standard,"(A,I5,A,F7.4,A,F7.4,A)") &
            "time after iteration ", icnt+1, ": ", time, &
            " sec (maxval= ", maxval_fp,')'
    ENDIF

    IF (ncoutput) CALL mod_output_write(f_p(sx:ex,sy:ey,sz:ez))

    DO
       icnt = icnt + iOne

       CALL sub_mpi_communication(f_p)

       DO iz = sz, ez
          DO iy = sy, ey
             DO ix = sx, ex
                fn_p(ix,iy,iz) = &
                     coeff_center      * f_p(ix,iy,iz)   + &
                     coeff_east_west   * f_p(ix+1,iy,iz) + coeff_east_west   * f_p(ix-1,iy,iz) + &
                     coeff_north_south * f_p(ix,iy+1,iz) + coeff_north_south * f_p(ix,iy-1,iz) + &
                     coeff_top_bottom  * f_p(ix,iy,iz+1) + coeff_top_bottom  * f_p(ix,iy,iz-1)
             END DO
          END DO
       END DO

       CALL sub_mpi_boundary_conditions(fn_p)

       temp_p => f_p
       f_p    => fn_p
       fn_p   => temp_p

       time = time + dt

       CALL sub_mpi_maxval(maxval_fp, f_p)
       CALL sub_mpi_maxval(maxval_fnp, fn_p)

       IF (rank == iZero) THEN
          IF((ncoutput).AND.(MODULO(icnt,frequency) .EQ. 0)) THEN
             WRITE(lun_standard,"(A,I5,A,F7.4,A,F16.12,A,F7.4,A)") &
                  "time after iteration ", icnt, ": ", time, &
                  "sec (maxval f_p= ",maxval_fp ,', maxval fn_p',maxval_fnp,')'
          END IF
       END IF

       IF((ncoutput).AND.(MODULO(icnt,frequency) .EQ. 0)) THEN
          CALL mod_output_write(f_p(sx:ex,sy:ey,sz:ez))
       ENDIF

       IF ((time + rHalf*dt >= time_max) .OR. (icnt >= 1000)) EXIT

    END DO

    IF (rank == iZero) THEN
       WRITE(lun_standard,"(A,I5,A,F16.12,A,F7.4,A)") &
                  "time after iteration ", icnt, ": ", time, &
                  " sec (maxval= ", maxval_fp,')'
    ENDIF

  END SUBROUTINE sub_3Ddiff_mainloop

END MODULE mod_3Ddiffusion
