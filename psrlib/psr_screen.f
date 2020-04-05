        SUBROUTINE PSR_SCREEN(VAL)
        INTEGER VAL
        LOGICAL INIT,SCREEN
        COMMON /SCR_HND/ INIT,SCREEN
        IF (VAL .EQ. 1) THEN
          SCREEN = .TRUE.
        ELSE
          SCREEN = .FALSE.
          INIT   = .FALSE.
          CALL KILL_SCREEN
        END IF
        END


        SUBROUTINE SCREEN_HANDLER(RETURNED_STRING)
C
C  This program is the first development test for the Pulsar Group
C  screen input to OLAF
C
C MXB 8-8-90
C  Definitions
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1500 SCREEN_IMAGE,RETURNED_STRING
1000  END
C
C
C
        SUBROUTINE KILL_SCREEN
C
1000  END
C
        subroutine INIT_SCREEN
C
c
c  subroutine to initialise the olaf screen handler
C  mxb 8-8-90
1000  END
C
      SUBROUTINE SCREEN_INIT(STATUS)
      END
