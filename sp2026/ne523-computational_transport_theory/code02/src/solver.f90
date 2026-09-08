MODULE solver
IMPLICIT NONE

CONTAINS
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
