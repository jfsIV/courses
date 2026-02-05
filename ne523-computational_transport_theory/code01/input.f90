MODULE input

USE globals
IMPLICIT NONE

CONTAINS
SUBROUTINE input_data()
    IMPLICIT NONE

    open(input_unit, file=input_file, status="old")
    read(input_unit, *) n_x_cells, n_y_cells

    allocate(x_cell_sizes(n_x_cells))
    allocate(y_cell_sizes(n_y_cells))

    read(input_unit, *) x_cell_sizes
    read(input_unit, *) y_cell_sizes

    read(input_unit, *) angles_per_octant

    allocate(angular_quadrature(3, angles_per_octant))

    read(input_unit, *) angular_quadrature
    read(input_unit, *) n_materials

    allocate(cross_sections(2, n_materials))

    read(input_unit, *) cross_sections
    read(input_unit, *) boundary_conditions

    allocate(material_array(n_x_cells, n_y_cells))
    allocate(source_array(n_x_cells, n_y_cells))

    read(input_unit, *) material_array
    read(input_unit, *) source_array

    close(input_unit)

END SUBROUTINE input_data


SUBROUTINE input_check()
    IMPLICIT NONE

    print*, "----------------------------"
    print*, " Beginning Input Validation "
    print*, "----------------------------"

    call validate_n_cells()
    call validate_x_cell_size()
    call validate_y_cell_size()
    call validate_angles_per_octant()
    call validate_angular_quadrature()
    call validate_n_materials()
    call validate_cross_section()
    call validate_boundary_conditions()
    call validate_material_assignment()
    call validate_source_assignment()

    print*, "----------------------------"
    print*, "    All Inputs Validated    "
    print*, "----------------------------"
END SUBROUTINE input_check


SUBROUTINE input_echo()
    IMPLICIT NONE

    call echo_discrete_ordinates()
    call echo_boundary_conditions()
    call echo_cell_data()
    
END SUBROUTINE input_echo


! checker subroutines
SUBROUTINE check_positive(var)
    IMPLICIT NONE
    class(*) :: var

    select type (var)
    type is (integer)
        if (var < 0) call throw_error("encountered a non-positive value")
    type is (real)
        if (var < 0) call throw_error("encountered a non-positive value")
    class default
        call throw_error("non-float/real variable encountered")
    end select
END SUBROUTINE check_positive


SUBROUTINE check_nonnegative(var)
    IMPLICIT NONE
    class(*) :: var

    select type (var)
    type is (integer)
        if (var < 0) call throw_error("encountered a negative value")
    type is (real)
        if (var < 0) call throw_error("encountered a negative value")
    class default
        call throw_error("non-float/real variable encountered")
    end select
END SUBROUTINE check_nonnegative


! validation subroutines
SUBROUTINE validate_n_cells()
    IMPLICIT NONE
    print*, "Validating number of cells"
    call check_positive(n_x_cells)
    call check_positive(n_y_cells)
    print*, "    Passed all checks"
END SUBROUTINE validate_n_cells


SUBROUTINE validate_x_cell_size()
    IMPLICIT NONE
    integer :: i

    print*, "Validating cell size in x"
    do i = 1, n_x_cells
        call check_positive(x_cell_sizes(i))
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_x_cell_size


SUBROUTINE validate_y_cell_size
    IMPLICIT NONE
    integer :: i

    print*, "Validating cell size in y"
    do i = 1, n_y_cells
        call check_positive(y_cell_sizes(i))
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_y_cell_size


SUBROUTINE validate_angles_per_octant()
    IMPLICIT NONE
    print*, "Validating angles per octant"
    call check_positive(angles_per_octant)
    print*, "    Passed all checks"
END SUBROUTINE validate_angles_per_octant


SUBROUTINE validate_angular_quadrature()
    IMPLICIT NONE
    integer :: i, j

    print*, "Validating angular quadature"
    do j = 1, angles_per_octant 
        do i = 1, 3
            call check_nonnegative(angular_quadrature(i,j))
            if (angular_quadrature(i,j) > 1.57079) then
                call throw_error("Quadrature point cannot be > pi/2")
            end if
        end do
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_angular_quadrature


SUBROUTINE validate_n_materials()
    IMPLICIT NONE
    print*, "Validating number of materials"
    call check_positive(n_materials)
    print*, "    Passed all checks"
END SUBROUTINE validate_n_materials


SUBROUTINE validate_cross_section()
    IMPLICIT NONE
    integer :: i
    real :: total_xs, scattering_xs

    print*, "Validating cross sections"
    do i = 1, n_materials
        total_xs = cross_sections(1, i)
        scattering_xs = cross_sections(2, i)

        call check_positive(total_xs)
        call check_positive(scattering_xs)

        if (scattering_xs > total_xs) then
            print*, "I am making this a check because I think I should, but I &
                know cross sections have (n,xn) reactions folded into the &
                scattering cross sections. By this convention, you could have &
                a region of Be with a higher scattering XS than total XS. This &
                is due to our modeling, but I wanted to mention it."
            call throw_error("Scattering XS cannot be > total XS")
        end if
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_cross_section


SUBROUTINE validate_boundary_conditions()
    IMPLICIT NONE
    integer :: i

    print*, "Validating boundary conditions"
    do i = 1, 4
        if (boundary_conditions(i) .ne. 0 .and. boundary_conditions(i) .ne. 1) then
            call throw_error("Boundary conditions need to be 0 or 1")
        end if
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_boundary_conditions


SUBROUTINE validate_material_assignment()
    IMPLICIT NONE
    integer :: i, j

    print*, "Validating material assignment"
    do j = 1, n_y_cells
        do i = 1, n_x_cells
            call check_positive(material_array(i,j))
            if (material_array(i,j) > n_materials) then
                call throw_error("Entered a non-existant material")
            end if
        end do
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_material_assignment


SUBROUTINE validate_source_assignment()
    IMPLICIT NONE
    integer :: i, j

    print*, "Validating source assignment"
    do j = 1, n_y_cells
        do i = 1, n_x_cells
            call check_positive(source_array(i,j))
        end do
    end do
    print*, "    Passed all checks"
END SUBROUTINE validate_source_assignment


! begin echo subroutines
SUBROUTINE echo_discrete_ordinates()
    integer :: i

    open(unit=output_unit, file=output_file, status="old", position="append") 

    write(output_unit, *)
    write(output_unit, "(A)") "Discrete Ordinates/Octant"
    write(output_unit, "(A)") "  n      mu     eta       w"
    do i = 1, angles_per_octant
        write(output_unit, "(I3)", advance="no") i
        write(output_unit, "(F8.4)", advance="no") angular_quadrature(1,i)
        write(output_unit, "(F8.4)", advance="no") angular_quadrature(2,i)
        write(output_unit, "(F8.4)", advance="no") angular_quadrature(3,i)
        write(output_unit, *)
    end do

    close(output_unit)
END SUBROUTINE echo_discrete_ordinates

    
SUBROUTINE echo_boundary_conditions()
    integer :: i

    open(unit=output_unit, file=output_file, status="old", position="append") 

    write(output_unit, *)
    write(output_unit, "(A)") "Boundary Conditions"
    write(output_unit, "(A)") "  Left   Right    Bottom    Top"
    do i = 1, 4
        write(output_unit, "(I3)", advance="no") boundary_conditions(i)
        write(output_unit, "(A)", advance="no") " "
    end do
    write(output_unit, *)

    close(output_unit)
END SUBROUTINE echo_boundary_conditions


SUBROUTINE echo_cell_data()
    integer :: i, j

    open(unit=output_unit, file=output_file, status="old", position="append") 

    write(output_unit, *)
    write(output_unit, "(A)") "Computational Cell Data"
    write(output_unit, "(A)") "  i  j  Material   dx   dy   SigmaT   SigmaS   Source"
    do j = 1, n_y_cells
        do i = 1, n_x_cells
            write(output_unit, "(I3)", advance="no") i
            write(output_unit, "(I3)", advance="no") j 
            write(output_unit, "(I3)", advance="no") material_array(i, j)
            write(output_unit, "(F8.4)", advance="no") x_cell_sizes(i)
            write(output_unit, "(F8.4)", advance="no") y_cell_sizes(j)
            write(output_unit, "(F8.4)", advance="no") cross_sections(1, material_array(i,j))
            write(output_unit, "(F8.4)", advance="no") cross_sections(2, material_array(i,j))
            write(output_unit, "(F8.4)", advance="no") source_array(i, j)

            write(output_unit, *)
        end do
    end do

    close(output_unit)
END SUBROUTINE echo_cell_data

END MODULE INPUT
