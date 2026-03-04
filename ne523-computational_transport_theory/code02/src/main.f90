PROGRAM test

USE solver

IMPLICIT NONE

character(len=64) :: input_file, output_file, arg_value
integer :: arg_count, glip
real :: psi_x_in, psi_y_in, psi_x_out, psi_y_out, psi_center
real :: mu, eta, dx, dy, source, sigma_t
character(len=64) :: str_c, str_x, str_y

! settings i/o file names
input_file = "test.in"
arg_count = command_argument_count()

if (arg_count .ne. 0) then
    call get_command_argument(1, arg_value)
    input_file = trim(arg_value)
end if

glip = len_trim(input_file) - 3
output_file = input_file(:glip)//".out"

! reading data
open(99, file=input_file, status="old")

read(99, *) psi_x_in, psi_y_in
read(99, *) mu, eta
read(99, *) dx, dy
read(99, *) source
read(99, *) sigma_t

close(99)

! running the diamond differencing solver
call ddsolve(psi_x_in, psi_y_in, mu, eta, dx, dy, source, sigma_t, &
        psi_x_out, psi_y_out, psi_center)

write(str_c, "(F10.6)") psi_center
write(str_x, "(F10.6)") psi_x_out
write(str_y, "(F10.6)") psi_y_out

! printing to output
open(98, file=output_file)
write(98, *) "Angular fluxes for cell i,j"
write(98, *) "---------------------------"
write(98, *) "Center        : " // trim(str_c)
write(98, *) "Outgoing in x : " // trim(str_x)
write(98, *) "Outgoing in y : " // trim(str_y)
close(98)

END PROGRAM test
