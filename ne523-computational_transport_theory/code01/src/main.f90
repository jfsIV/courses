PROGRAM sn_2d

USE versioning
USE globals
USE input
USE solver

IMPLICIT NONE

! input/output file names
integer :: arg_count
character(len=64) :: arg_value

arg_count = command_argument_count()

if (arg_count > 1) then
    call throw_error("Only one command line argument expected")
end if

if (arg_count == 1) then
    call get_command_argument(1, arg_value)
    input_file = trim(arg_value)
end if

if (arg_count == 0) then
    input_file = "sample.i"
end if


! creating the output file
open(output_unit, file=output_file, status="REPLACE") 
close(output_unit)


! actual code
CALL version_data()

CALL input_data()
CALL input_check()
CALL input_echo()

CALL transport_solver()

END PROGRAM sn_2d
