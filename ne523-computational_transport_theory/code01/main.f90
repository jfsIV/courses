PROGRAM sn_2d

USE versioning
USE globals
USE input

IMPLICIT NONE
    open(output_unit, file=output_file, status="REPLACE") 
    close(output_unit)
    
    CALL version_data()

    CALL input_data()
    CALL input_check()

END PROGRAM sn_2d
