MODULE input

USE globals

CONTAINS
SUBROUTINE input_data()
    integer :: iostat

    open(input_unit, file=input_file, status="old", iostat=iostat)
    read(input_unit, *, iostat=iostat) n_x_cells, n_y_cells

    allocate(x_cell_sizes(I))
    allocate(y_cell_sizes(J))

    read(input_unit, *, iostat=iostat) x_cell_sizes
    read(input_unit, *, iostat=iostat) y_cell_sizes

    read(input_unit, *, iostat=iostat) angles_per_octant

    print*, I, J, K

    allocate(angular_quadrature(3, K))

    read(input_unit, *, iostat=iostat) angular_quadrature

    read(input_unit, *, iostat=iostat) n_materials

    allocate(cross_sections(2, n_materials))

    read(input_unit, *, iostat=iostat) cross_sections

    read(input_unit, *, iostat=iostat) boundary_conditions

    allocate(material_array(n_x_cells, n_y_cells))
    allocate(source_array(n_x_cells, n_y_cells))

    read(input_unit, *, iostat=iostat) material_array
    read(input_unit, *, iostat=iostat) source_array

    close(input_unit)

END SUBROUTINE input_data

END MODULE INPUT
