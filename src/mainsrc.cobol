       ID DIVISION.
       PROGRAM-ID.    MAINCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MAINMAP                   PIC X(7) VALUE 'MAINCOB'
       01  PAYMAP                    PIC X(7) VALUE 'PAYMCOB'
       01  LISTMAP                   PIC X(7) VALUE 'LISTCOB'
       01  ADMINMAP                  PIC X(7) VALUE 'KAJCCOB'
       01  FLAG                      PIC X(5) VALUE 'FLAG:'
       01  FLAG-ANSWER               PIC X(9) VALUE 'TELLNOONE'
       COPY TESTMSD.
       COPY DFHAID.
       LINKAGE SECTION.
       01  DFHCOMMAREA PIC X(100).
       PROCEDURE DIVISION.
           IF EIBAID = DFHCLEAR THEN
               EXEC CICS RETURN END-EXEC.
           IF EIBAID = DFHPF3 THEN
               EXEC CICS XCTL PROGRAM(PAYMAP) END-EXEC.
           IF EIBAID = DFHPF4 THEN
               EXEC CICS XCTL PROGRAM(LISTMAP) END-EXEC.
           IF EIBAID = DFHPF5 THEN
               EXEC CICS XCTL PROGRAM(ADMINMAP) END-EXEC.
           EXEC CICS
           SEND MAP('HOMEMAP') MAPSET('TESTMSD') ERASE
           END-EXEC.
           EXEC CICS RETURN
               TRANSID(EIBTRNID)
           END-EXEC.
