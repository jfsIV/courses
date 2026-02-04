PROGRAM sn_2d

USE version_data
USE globals
USE input_parser

IMPLICIT NONE

    character(*), parameter :: output_file = "output.txt"
    character(*), parameter :: input_file  = "sample.i"

    integer, parameter :: output_unit = 99
    integer, parameter :: input_unit  = 98

    open(unit=output_unit, file=output_file, status="REPLACE") 
    
    ! version_data writing to output
    CALL write_version_data()

    ! reading the input data
    character(len=128), allocatable :: lines(:)
    integer :: nlines
    CALL input_data(input_file, input_unit, lines, nlines)

END PROGRAM sn_2d
