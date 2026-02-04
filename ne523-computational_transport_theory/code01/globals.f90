module globals
IMPLICIT NONE

CONTAINS

    SUBROUTINE writeout(output_text)
        character(*) :: output_text

        character(*), parameter :: output_file = "output.txt"
        integer, parameter :: output_unit = 99


        open(unit=output_unit, file=output_file, status="old", &
            position="append") 
        write(output_unit,"(A)") trim(output_text)
        close(unit=output_unit)

    END SUBROUTINE writeout

END MODULE globals
