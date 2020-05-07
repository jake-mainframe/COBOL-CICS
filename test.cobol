       ID DIVISION.
       PROGRAM-ID.    TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  YOUR-ID                           PIC X(6)
       01  CUSTOMER-MASTER-RECORD.
      *
           05  CM-CUSTOMER-NUMBER            PIC X(6).
           05  CM-BALANCE                    PIC 9(3).
      *
       COPY TESTMSD.
       PROCEDURE DIVISION.
               EXEC CICS
                   RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
               END-EXEC.
               MOVE LOW-VALUES TO TESTMAPO
               EXEC CICS ASSIGN USERID(MAPA01O) END-EXEC
               EXEC CICS ASSIGN USERID(YOUR-ID) END-EXEC
               EXEC CICS
               READ FILE('CUSTMAS')
                    INTO(CUSTOMER-MASTER-RECORD)
                    RIDFLD(YOUR-ID)
               END-EXEC.
               EXEC CICS
               SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
               END-EXEC.
               EXEC CICS RETURN END-EXEC.
