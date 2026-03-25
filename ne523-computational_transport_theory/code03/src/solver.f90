MODULE solver

USE globals
IMPLICIT NONE

CONTAINS
SUBROUTINE transport_solver()

print*, "will solve D.O here"

call sweep()

END SUBROUTINE transport_solver


SUBROUTINE sweep()
integer :: k, starting_corner, x_id, y_id, mat_id
real :: current_mu, current_eta, current_w

real, dimension(n_x_cells) :: psi_y_boundary
real :: psi_x_in, psi_x_out, psi_y_in, psi_y_out, psi_center
real :: dx, dy, source, sigma_t

!print*, cross_sections
! Q1: mu > 0, eta > 0
print*, "Q1"
do k = 1, angles_per_octant
    current_mu = angular_quadrature(1, k)
    current_eta = angular_quadrature(2, k)
    current_w = angular_quadrature(3, k)

    psi_y_boundary = 0.0

    do y_id = 1, n_y_cells          ! top to bottom
        dy = y_cell_sizes(y_id)
        psi_x_in = 0.0

        !do x_id = n_x_cells, 1, -1  ! right to left
        do x_id = 1, n_x_cells  ! right to left
            dx = x_cell_sizes(x_id)
            source = source_array(x_id, y_id)
            mat_id = material_array(x_id, y_id)
            sigma_t = cross_sections(1, mat_id)
            !print*, "mat_id", mat_id, "sigma_t", sigma_t

            psi_y_in = psi_y_boundary(x_id)

            call ddsolve(psi_x_in, psi_y_in, current_mu, current_eta, dx, dy, &
                source, sigma_t, psi_x_out, psi_y_out, psi_center)
            print*, x_id, y_id, psi_center

            scalar_flux_array(x_id, y_id) = scalar_flux_array(x_id, y_id) + psi_center * current_w

            psi_x_in = psi_x_out
            psi_y_in = psi_y_out

            psi_y_boundary(x_id) = psi_y_out
        end do
    end do
end do


! Q2: mu < 0, eta > 0
print*, "Q2"
do k = 1, angles_per_octant
    current_mu = -angular_quadrature(1, k)
    current_eta = angular_quadrature(2, k)
    current_w = angular_quadrature(3, k)

    psi_y_boundary = 0.0

    do y_id = 1, n_y_cells      ! top to bottom
        dy = y_cell_sizes(y_id)
        psi_x_in = 0.0

        !do x_id = 1, n_x_cells  ! left to right
        do x_id = n_x_cells, 1, -1  ! left to right
            dx = x_cell_sizes(x_id)
            source = source_array(x_id, y_id)
            sigma_t = cross_sections(1, material_array(x_id, y_id))

            psi_y_in = psi_y_boundary(x_id)

            call ddsolve(psi_x_in, psi_y_in, current_mu, current_eta, dx, dy, &
                source, sigma_t, psi_x_out, psi_y_out, psi_center)
            print*, x_id, y_id, psi_center

            scalar_flux_array(x_id, y_id) = scalar_flux_array(x_id, y_id) + psi_center * current_w

            psi_x_in = psi_x_out
            psi_y_in = psi_y_out

            psi_y_boundary(x_id) = psi_y_out
        end do
    end do
end do


! Q3: mu < 0, eta < 0
print*, "Q3"
do k = 1, angles_per_octant
    current_mu = -angular_quadrature(1, k)
    current_eta = -angular_quadrature(2, k)
    current_w = angular_quadrature(3, k)

    psi_y_boundary = 0.0

    do y_id = n_y_cells, 1, -1      ! bottom to top
        dy = y_cell_sizes(y_id)
        psi_x_in = 0.0

        !do x_id = 1, n_x_cells      ! left to right
        do x_id = n_x_cells, 1, -1      ! left to right
            dx = x_cell_sizes(x_id)
            source = source_array(x_id, y_id)
            sigma_t = cross_sections(1, material_array(x_id, y_id))

            psi_y_in = psi_y_boundary(x_id)

            call ddsolve(psi_x_in, psi_y_in, current_mu, current_eta, dx, dy, &
                source, sigma_t, psi_x_out, psi_y_out, psi_center)
            print*, x_id, y_id, psi_center

            scalar_flux_array(x_id, y_id) = scalar_flux_array(x_id, y_id) + psi_center * current_w

            psi_x_in = psi_x_out
            psi_y_in = psi_y_out

            psi_y_boundary(x_id) = psi_y_out
        end do
    end do
end do


! Q4: mu > 0, eta < 0
print*, "Q4"
do k = 1, angles_per_octant
    current_mu = angular_quadrature(1, k)
    current_eta = -angular_quadrature(2, k)
    current_w = angular_quadrature(3, k)

    psi_y_boundary = 0.0

    do y_id = n_y_cells, 1, -1      ! bottom to top
        dy = y_cell_sizes(y_id)
        psi_x_in = 0.0

        !do x_id = n_x_cells, 1, -1  ! right to left
        do x_id = 1, n_x_cells  ! right to left
            dx = x_cell_sizes(x_id)
            source = source_array(x_id, y_id)
            sigma_t = cross_sections(1, material_array(x_id, y_id))

            psi_y_in = psi_y_boundary(x_id)

            call ddsolve(psi_x_in, psi_y_in, current_mu, current_eta, dx, dy, &
                source, sigma_t, psi_x_out, psi_y_out, psi_center)
            print*, x_id, y_id, psi_center

            scalar_flux_array(x_id, y_id) = scalar_flux_array(x_id, y_id) + psi_center * current_w

            psi_x_in = psi_x_out
            psi_y_in = psi_y_out

            psi_y_boundary(x_id) = psi_y_out
        end do
    end do
end do


! writing to output
open(unit=output_unit, file=output_file, position="APPEND")
write(output_unit, '(//, A)') "Discrete Ordinates Method Solution"
write(output_unit, '(A)') "   i   j   Cell-Averaged Scalar Flux"

do y_id = 1, n_y_cells
    do x_id = 1, n_x_cells
        write(output_unit, '(I4, I4, X, ES14.6)') x_id, y_id, scalar_flux_array(x_id, y_id)
    end do
end do

close(output_unit)
END SUBROUTINE sweep


SUBROUTINE ddsolve(psi_x_in, psi_y_in, mu, eta, dx, dy, source, sigma_t, &
    psi_x_out, psi_y_out, psi_center)

    ! Solves the diamond difference neutron balance equation.
    !
    ! This subroutine solves the diamond-difference neturons balance equation
    ! for both directions (x, y), for a single ordinate, in a single cell. This
    ! subroutine overwrites the values for psi_x_out, psi_y_out, and psi_center
    ! to "return" the results of the calculation.
    !
    ! Parameters
    ! ----------
    !   psi_x_in : incoming angular flux in the x direction
    !   psi_y_in : incoming angular flux in the y direction
    !   mu       : cosine of the angle between streaming direction and x-axis
    !   eta      : cosine of the angle between streaming direction and y-axis
    !   dx       : width of the cell in the x direction
    !   dy       : width of the cell in the y direction
    !   source   : source strength in the cell
    !   sigma_t  : total macroscopic cross-section in the cell
    !
    ! Returns
    ! -------
    !   psi_x_out  : outgoing angular flux in the x direction
    !   psi_y_out  : outgoing angular flux in the y direction
    !   psi_center : cell-centered, angluar flux for the given cell

    real :: psi_x_in, psi_y_in, psi_x_out, psi_y_out, psi_center
    real :: mu, eta, dx, dy, source, sigma_t

    psi_center = source + (2 * abs(mu)) / dx * psi_x_in + (2 * abs(eta)) / dy * psi_y_in
    psi_center = psi_center / (sigma_t + (2 * abs(mu)) / dx + (2 * abs(eta)) / dy)

    psi_x_out = 2 * psi_center - psi_x_in
    psi_y_out = 2 * psi_center - psi_y_in

END SUBROUTINE ddsolve
END MODULE solver
