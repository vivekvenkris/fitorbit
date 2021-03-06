c     returns an error message MSSGE of the last FORTRAN IO error that occured
      subroutine erform(ioerr, messge)
      integer I,ioerr,nerr
      parameter (nerr=2*82)
      character*132 messge 
      character*132 errmsgs(nerr)
      
C     not finished  - here are the error messages
      data (errmsgs(I),I=1,20) /
     & '*** FORTRAN I/O ERROR 900:',
     &    'ERROR IN FORMAT',
     & '*** FORTRAN I/O ERROR 901:',
     &    'NEGATIVE UNIT NUMBER SPECIFIED',
     & '*** FORTRAN I/O ERROR 902:',
     &    'FORMATTED I/O ATTEMPTED ON UNFORMATTED FILE',
     & '*** FORTRAN I/O ERROR 903:',
     &    'UNFORMATTED I/O ATTEMPTED ON FORMATTED FILE',
     & '*** FORTRAN I/O ERROR 904:',
     &    'DIRECT I/O ATTEMPTED ON SEQUENTIAL FILE',
     & '*** FORTRAN I/O ERROR 905:',
     &    'ERROR IN LIST-DIRECTED READ OF LOGICAL DATA',
     & '*** FORTRAN I/O ERROR 906:',
     &    'ILLEGAL SEQUENTIAL I/O TO TERMINAL ATTEMPTED',
     & '*** FORTRAN I/O ERROR 907:',
     &    'ERROR IN LIST-DIRECTED I/O READ OF CHARACTER DATA',
     & '*** FORTRAN I/O ERROR 908:',
     &    'COULD NOT OPEN FILE SPECIFIED',
     & '*** FORTRAN I/O ERROR 909:',
     &    'SEQUENTIAL I/O ATTEMPTED ON DIRECT ACCESS FILE' /
      data (errmsgs(I),I=21,40) /
     & '*** FORTRAN I/O ERROR 910:',
     &    'ACCESS PAST END OF RECORD ATTEMPTED',
     & '*** FORTRAN I/O ERROR 911:',
     &    'CALL TO FUNCTION PERFORMING I/O IN I/O LIST',
     & '*** FORTRAN I/O ERROR 912:',
     &    'ERROR IN LIST I/O READ OF COMPLEX DATA',
     & '*** FORTRAN I/O ERROR 913:',
     &    'OUT OF FREE SPACE',
     & '*** FORTRAN I/O ERROR 914:',
     &    'ACCESS OF UNCONNECTED UNIT ATTEMPTED',
     & '*** FORTRAN I/O ERROR 915:',
     &    'READ UNEXPECTED CHARACTER',
     & '*** FORTRAN I/O ERROR 916:',
     &    'ERROR IN READ OF LOGICAL DATA',
     & '*** FORTRAN I/O ERROR 917:',
     &    'OPEN WITH NAMED SCRATCH FILE ATTEMPTED',
     & '*** FORTRAN I/O ERROR 918:',
     &    'OPEN OF EXISTING FILE WITH STATUS= ''NEW'' ATTEMPTED',
     & 'no message 919',' ' /
      data (errmsgs(I),I=41,60) /
     & '*** FORTRAN I/O ERROR 920:',
     &    'OPEN OF FILE CONNECTED TO DIFFERENT UNIT ATTEMPTED',
     & '*** FORTRAN I/O ERROR 921:',
     &    'UNFORMATTED OPEN WITH BLANK SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 922:',
     &    'READ ON ILLEGAL RECORD ATTEMPTED',
     & '*** FORTRAN I/O ERROR 923:',
     &    'OPEN WITH ILLEGAL FORM SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 924:',
     &    'CLOSE OF SCRATCH FILE WITH STATUS= ''KEEP'' ATTEMPTED',
     & '*** FORTRAN I/O ERROR 925:',
     &    'OPEN WITH ILLEGAL STATUS SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 926:',
     &    'CLOSE WITH ILLEGAL STATUS SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 927:',
     &    'OPEN WITH ILLEGAL ACCESS SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 928:',
     &    'OPEN OF SEQUENTIAL FILE WITH RECL SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 929:',
     &    'OPEN OF DIRECT FILE WITH NO RECL SPECIFIER ATTEMPTED' /
      data (errmsgs(I),I=61,80) /
     & '*** FORTRAN I/O ERROR 930:',
     &    'OPEN WITH RECL LESS THAN 1 ATTEMPTED',
     & '*** FORTRAN I/O ERROR 931:',
     &    'OPEN WITH ILLEGAL BLANK SPECIFIER ATTEMPTED',
     & '*** FORTRAN I/O ERROR 932:',
     &    'TOO MANY UNITS OPEN AT ONCE',
     & '*** FORTRAN I/O ERROR 933:',
     &    'SEQUENTIAL END-OF-FILE WITH NO "END=" SPECIFIER',
     & '*** FORTRAN I/O ERROR 934:',
     &    'OPEN OF READONLY FILE WITH ACCESS=''APPEND'' ATTEMPTED',
     & '*** FORTRAN I/O ERROR 935:',
     &    'INTERNAL LIBRARY ERROR',
     & '*** FORTRAN I/O ERROR 936:',
     &    'APPEND I/O ATTEMPTED ON SEQUENTIAL ONLY FILE/DEVICE',
     & '*** FORTRAN I/O ERROR 937:',
     &    'ILLEGAL RECORD NUMBER SPECIFIED',
     & '*** FORTRAN I/O ERROR 938:',
     &    'LIST-DIRECTED I/O OF UNKNOWN TYPE',
     & '*** FORTRAN I/O ERROR 939:',
     &    'OPEN OF INACCESSIBLE FILE ATTEMPTED' /
      data (errmsgs(I),I=81,100) /
     & '*** FORTRAN I/O ERROR 940:',
     &    'ERROR IN ATTEMPTED OPEN',
     & '*** FORTRAN I/O ERROR 941:',
     &    'ERROR IN SEQUENTIAL UNFORMATTED READ',
     & '*** FORTRAN I/O ERROR 942:',
     &    'ERROR IN LIST-DIRECTED READ - CHARACTER DATA READ ',
     & '*** FORTRAN I/O ERROR 943:',
     &    'ERROR IN DIRECT FORMATTED READ',
     & '*** FORTRAN I/O ERROR 944:',
     &    'RECORD TOO LONG IN DIRECT UNFORMATTED I/O',
     & '*** FORTRAN I/O ERROR 945:',
     &    'ERROR IN FORMATTED I/O',
     & '*** FORTRAN I/O ERROR 946:',
     &    'ERROR IN LIST-DIRECTED I/O',
     & '*** FORTRAN I/O ERROR 947:',
     &    'FORMAT DESCRIPTOR NOT COMPATIBLE FOR ITEM TYPE',
     & '*** FORTRAN I/O ERROR 948:',
     &    'WRITE TO WRITE-PROTECTED FILE ATTEMPTED',
     & '*** FORTRAN I/O ERROR 949:',
     &    'READ FROM READ-PROTECTED FILE ATTEMPTED' /
      data (errmsgs(I),I=101,120) /
     & '*** FORTRAN RANGE ERROR 950:',
     &    'PARAMETER OUT OF BOUNDS AT STATEMENT NUMBER %d',
     & '*** FORTRAN RANGE ERROR 951:',
     & 'LABEL OUT OF BOUNDS IN ASSIGNED GOTO AT STATEMENT NUMBER %d',
     & '*** FORTRAN RANGE ERROR 952:',
     &    'ZERO INCREMENT VALUE IN DO LOOP AT STATEMENT NUMBER %d',
     & '*** FORTRAN I/O ERROR 953:',
     &    'NO REPEATABLE EDIT DESCRIPTOR IN FORMAT STRING',
     & '*** FORTRAN I/O ERROR 954:',
     &    'ILLEGAL USE OF EMPTY FORMAT',
     & '*** FORTRAN I/O ERROR 955:',
     &    'OPEN WITH NO FILE= AND STATUS = OLD OR NEW',
     & '*** FORTRAN I/O ERROR 956:',
     &    'FILE SYSTEM ERROR',
     & '*** FORTRAN I/O ERROR 957:',
     &    'FORMAT DESCRIPTOR INCOMPATIBLE WITH ITEM IN I/O LIST',
     & '*** FORTRAN I/O ERROR 958:',
     &    'FORMAT DESCRIPTOR INCOMPATIBLE WITH CHARACTER IN I/O LIST',
     & '*** FORTRAN I/O ERROR 959:',
     & 'FORMAT DESCRIPTOR INCOMPATIBLE WITH LOGICAL ITEM IN I/O LIST' /
      data (errmsgs(I),I=121,140) /
     & '*** FORTRAN I/O ERROR 960:',
     &    'FORMAT ERROR : MISSING STARTING LEFT PARENTHESIS',
     & '*** FORTRAN I/O ERROR 961:',
     &    'FORMAT ERROR : INVALID FORMAT DESCRIPTOR',
     & '***FORMAT ERROR : UNEXPECTED CHARACTER FOUND FOLLOWING A ',
     &    'NUMBER IN THE FORMAT STRING',
     & '*** FORTRAN I/O ERROR 963:',
     &    'FORMAT ERROR : TRYING TO SCALE UNSCALABLE FORMAT SPECIFIER',
     & '*** FORTRAN I/O ERROR 964:',
     &    'FORMAT ERROR : PARENTHESES TOO DEEPLY NESTED.',
     & '*** FORTRAN I/O ERROR 965:',
     &    'FORMAT ERROR : INVALID TAB SPECIFIER.',
     & '*** FORTRAN I/O ERROR 966:',
     &    'FORMAT ERROR : INVALID BLANK SPECIFIER',
     & '*** FORTRAN I/O ERROR 967:',
     &    'FORMAT ERROR : SPECIFIER EXPECTED BUT END OF FORMAT FOUND',
     & '*** FORTRAN I/O ERROR 968:',
     &    'FORMAT ERROR : MISSING SEPARATOR',
     & '*** FORTRAN I/O ERROR 969:',
     &    'FORMAT ERROR : DIGIT EXPECTED' /
      data (errmsgs(I),I=141,160) /
     & '***FORMAT ERROR : PERIOD EXPECTED IN FLOATING ',
     &    'POINT FORMAT DESCRIPTOR',
     & '*** FORTRAN I/O ERROR 971:',
     &    'FORMAT ERROR : UNBALANCED PARENTHESES',
     & '*** FORTRAN I/O ERROR 972:',
     &    'FORMAT ERROR : INVALID STRING IN FORMAT',
     & '*** FORTRAN I/O ERROR 973:',
     &    'RECORD LENGTH DIFFERENT IN SUBSEQUENT OPEN',
     & '*** FORTRAN I/O ERROR 974:',
     &    'RECORD ACCESSED PAST END OF INTERNAL FILE (VARIABLE)',
     & '*** FORTRAN I/O ERROR 975:',
     &    'ILLEGAL NEW FILE NUMBER REQUESTED IN FSET FUNCTION',
     & '*** FORTRAN I/O ERROR 976:',
     &    'UNEXPECTED CHARACTER IN "NAMELIST" READ',
     & '*** FORTRAN I/O ERROR 977:',
     &    'ILLEGAL SUBSCRIPT OR SUBSTRING IN "NAMELIST" READ',
     & '*** FORTRAN I/O ERROR 978:',
     &    'TOO MANY VALUES IN "NAMELIST" READ',
     & '*** FORTRAN I/O ERROR 979:',
     &    'VARIABLE NOT IN "NAMELIST" GROUP' /
      data (errmsgs(I),I=161,164) /
     & '*** FORTRAN I/O ERROR 980:',
     &    '"NAMELIST" I/O ATTEMPTED ON UNFORMATTED FILE',
     & '*** FORTRAN I/O ERROR 981:',
     &    'VALUE OUT OF RANGE IN NUMERIC READ'/

      messge = errmsgs(2*(ioerr-899)-1)//errmsgs(2*(ioerr-899))
      end




