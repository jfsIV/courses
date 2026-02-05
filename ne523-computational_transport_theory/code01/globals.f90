MODULE globals
IMPLICIT NONE

! I/O
character(len=*), parameter :: output_file = "output.txt"
character(len=*), parameter :: input_file  = "sample.i"
integer, parameter :: output_unit = 99
integer, parameter :: input_unit  = 98

! input data
integer :: n_x_cells, n_y_cells, angles_per_octant, n_materials
real, dimension(:), allocatable :: x_cell_sizes, y_cell_sizes
real, dimension(:,:), allocatable :: angular_quadrature, cross_sections
integer, dimension(4) :: boundary_conditions
integer, dimension(:, :), allocatable :: material_array
real, dimension(:, :), allocatable :: source_array


CONTAINS
SUBROUTINE writeout(output)
    ! this only works for monotyped arguments and automatically writes a new line
    class(*) :: output

    open(unit=output_unit, file=output_file, status="old", position="append") 

    select type (output)
    type is (character(*))
        write(output_unit, "(A)") trim(output)
    type is (real)
        write(output_unit, *) output
    type is (integer)
        write(output_unit, *) output
    type is (complex)
        write(output_unit, *) output
    type is (logical)
        write(output_unit, *) output
    end select

    close(unit=output_unit)

END SUBROUTINE writeout

SUBROUTINE throw_error(errmsg)
    character(len=*), intent(in) :: errmsg

    print*, "--------------------------------------------------"
    print*, "|           A FATAL ERROR HAS OCCURED            |"
    print*, "--------------------------------------------------"
    print*, errmsg
    print*, "Terminating the program"
    STOP

END SUBROUTINE throw_error

END MODULE globals
