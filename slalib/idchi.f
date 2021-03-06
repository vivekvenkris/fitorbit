      SUBROUTINE sla__IDCHI (STRING, NPTR, NVEC, DIGIT)
*+
*     - - - - - -
*      I D C H I
*     - - - - - -
*
*  Internal routine used by INTIN
*
*  Identify next character in string
*
*  Given:
*     STRING      char        string
*     NPTR        int         pointer to character to be identified
*
*  Returned:
*     NPTR        int         incremented unless end of field
*     NVEC        int         vector for identified character
*     DIGIT       double      double precision digit if 0-9
*
*     NVEC takes the following values:
*
*      1     0-9
*      2     space or TAB   !!! n.b. ASCII TAB assumed !!!
*      3     +
*      4     -
*      5     ,
*      6     else
*      7     outside string
*
*  If the character is not 0-9, DIGIT is either unaltered or
*  is set to an arbitrary value.
*
*  P.T.Wallace   Starlink   22 December 1992
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      CHARACTER*(*) STRING
      INTEGER NPTR,NVEC
      DOUBLE PRECISION DIGIT

      CHARACTER K
      INTEGER NCHAR

*  Character/vector tables
      INTEGER NCREC
      PARAMETER (NCREC=14)
      CHARACTER KCTAB(NCREC)
      INTEGER KVTAB(NCREC)
      DATA KCTAB/'0','1','2','3','4','5','6','7','8','9',
     :           ' ', '+','-',','/
      DATA KVTAB/10*1,2,3,4,5/



*  Handle pointer outside field
      IF (NPTR.LT.1.OR.NPTR.GT.LEN(STRING)) THEN
         NVEC=7
      ELSE

*     Not end of field: identify character
         K=STRING(NPTR:NPTR)
         DO NCHAR=1,NCREC
            IF (K.EQ.KCTAB(NCHAR)) THEN

*           Recognized
               NVEC=KVTAB(NCHAR)
               DIGIT=DBLE(NCHAR-1)
               GO TO 2300
            END IF
         END DO

*     Not recognized: check for TAB   !!! n.b. ASCII assumed !!!
         IF (K.EQ.CHAR(9)) THEN

*        TAB: treat as space
            NVEC=2
         ELSE

*        Unrecognized
            NVEC=6
         END IF

*     Increment pointer
 2300    CONTINUE
         NPTR=NPTR+1
      END IF

      END
