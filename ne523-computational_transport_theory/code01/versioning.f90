MODULE versioning

USE globals

IMPLICIT NONE

    character(len=8), parameter :: code_name      = "sn_2d"
    character(len=8), parameter :: version_number = "v0.1.0"
    character(len=19), parameter :: author_name   = "Joseph F. Specht IV"

    character(len=8) :: date_str
    character(len=6) :: time_str

    character(len=4) :: year_str
    character(len=2) :: month_str, day_str, hour_str, min_str, sec_str

    character(len=100) :: date_time_str

CONTAINS
    SUBROUTINE version_data()
        CALL DATE_AND_TIME(DATE=date_str, TIME=time_str)

        year_str = date_str(1:4)
        month_str = date_str(5:6)
        day_str = date_str(7:8)

        hour_str = time_str(1:2)
        min_str = time_str(3:4)
        sec_str = time_str(5:6)

        date_time_str = "Executed  : "//hour_str//":"//min_str//":"//sec_str &
            //" on "//month_str//"/"//day_str//"/"//year_str

        call writeout("Code Name : "//code_name)
        call writeout("Version   : "//version_number)
        call writeout("Author    : "//author_name)
        call writeout(date_time_str)

    END SUBROUTINE version_data

END MODULE versioning
