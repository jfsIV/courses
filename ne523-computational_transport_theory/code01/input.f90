MODULE input

USE globals

CONTAINS
SUBROUTINE input_data()
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
    

END SUBROUTINE input_check

END MODULE INPUT
