PROGRAM sn_2d

USE versioning
USE globals
USE input
USE solver

IMPLICIT NONE

real :: time_start, time_end
integer :: arg_count
character(len=64) :: arg_value

CALL cpu_time(time_start)  ! runtime

! input/output file names
arg_count = command_argument_count()

select case (arg_count)
    case (2:)
        call throw_error("Only one command line argument expected")
    case (1)
        call get_command_argument(1, arg_value)
        input_file = trim(arg_value)
    case (0)
        input_file = "sample.in"
end select


! creating the output file
open(output_unit, file=output_file, status="REPLACE")

! actual code
CALL version_data()

CALL input_data()
CALL input_check()
CALL input_echo()

CALL transport_solver()

! rumtime calculation
CALL cpu_time(time_end)
CALL update_runtime(time_end - time_start)

close(output_unit)
END PROGRAM sn_2d
