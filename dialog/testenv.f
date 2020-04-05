      PROGRAM test_getenv
            CHARACTER(len=255) :: homedir
            CALL getenv("fitorbitdir", homedir)
            WRITE (*,*) TRIM(homedir)
      END PROGRAM
