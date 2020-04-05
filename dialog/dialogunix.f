        subroutine psrgetenv(log,name)
        character*(*) log,name
        call getenv(log,name)
        return
        end
