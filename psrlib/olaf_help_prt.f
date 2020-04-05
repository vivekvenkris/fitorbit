*DECK OLAF_HELP_PRINT_TOPICS
C
C
C
      SUBROUTINE OLAF_HELP_PRINT_TOPICS (PATH, STATUS)
C
C This routine prints a list of the topics available.  This is basically
C a listing of the help directory.
C
C Arguments:
C  PATH    Input   CHARACTER*(*)  The path name o the help directory.
C  STATUS  Output  INTEGER        Zero if successful, else greater than
C                                 zero.
C
C      Version 1.0   4th November, 1987
C
C Declare the routine's arguments.
C
      CHARACTER PATH*(*)
      INTEGER   STATUS
C
C Do a directory listing preceeded by a header.
C
      CALL GIVEOS ('echo ''The following topics are available'';'//
     &             'echo '''';ls -C '//PATH//'|more -21', STATUS)
C
C End of subroutine OLAF_HELP_PRINT_TOPICS.
C
      END
