        subroutine psrgetdir(log,name,ilen)
C return drectory spec from logical name for UNIX
        character*(*) log,name
        integer ilen

        call getenv(log,name)
        ilen=length(name)
        if(ilen.eq.0) then
           call getcwd(name)
           ilen=length(name)
           ilen=ilen+1
           name(ilen:ilen)='/'
        endif
        return
        end
