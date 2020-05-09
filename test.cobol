       ID DIVISION.
       PROGRAM-ID.    TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SECRET-MESSAGE                    PIC X(9) VALUE 'TELLNOONE'
       01  WS-COMM.
           05  USER-ID                       PIC X(6)
           05  OLD-BALANCE                   PIC 9(3)
       01  SUB-BALANCE                       PIC 9(3)
      *
       01  CUSTOMER-MASTER-RECORD.
      *
           05  CM-CUSTOMER-NUMBER            PIC X(6).
           05  CM-BALANCE                    PIC 9(3).
      *
       COPY TESTMSD.
       COPY DFHAID.
       LINKAGE SECTION.
       01  DFHCOMMAREA PIC X(100).
       PROCEDURE DIVISION.
               IF EIBAID = DFHCLEAR THEN
                   EXEC CICS RETURN END-EXEC.
               END-IF
               IF EIBAID = DFHPF12 THEN
                   MOVE DFHCOMMAREA TO WS-COMM
                   EXEC CICS
                   RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   MOVE MAPE01O TO SUB-BALANCE
                   MOVE MAPA01I TO MAPA01O
                   EXEC CICS
                   READ FILE('CUSTMAS')
                        INTO(CUSTOMER-MASTER-RECORD)
                        RIDFLD(USER-ID)
                   END-EXEC
                   MOVE CM-BALANCE TO OLD-BALANCE
                   COMPUTE OLD-BALANCE = OLD-BALANCE - SUB-BALANCE
                   PERFORM FILL-IN-MAP
                   EXEC CICS
                   SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                       TRANSID(EIBTRNID) COMMAREA(WS-COMM)
                   END-EXEC.
               END-IF
               IF EIBAID = DFHPF5 THEN
                   MOVE DFHCOMMAREA TO WS-COMM
                   EXEC CICS
                   RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   MOVE MAPA01O TO USER-ID
                   DISPLAY USER-ID
                   PERFORM BALANCE-REF
                   PERFORM FILL-IN-MAP
                   EXEC CICS
                   SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                       TRANSID(EIBTRNID) COMMAREA(WS-COMM)
                   END-EXEC.
               END-IF
               IF EIBCALEN = 0 THEN
                   EXEC CICS
                   RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   EXEC CICS ASSIGN USERID(USER-ID) END-EXEC
                   PERFORM BALANCE-REF
                   PERFORM FILL-IN-MAP
                   EXEC CICS
                   SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                   TRANSID(EIBTRNID) COMMAREA(WS-COMM)
                   END-EXEC.
               END-IF
       BALANCE-REF SECTION.
               EXEC CICS
               READ FILE('CUSTMAS')
               INTO(CUSTOMER-MASTER-RECORD)
               RIDFLD(USER-ID)
               END-EXEC.
               MOVE CM-BALANCE TO OLD-BALANCE
       FILL-IN-MAP SECTION.
               MOVE USER-ID TO MAPA01O
               MOVE OLD-BALANCE TO MAPB01O.
       FILL-IN-MAP-EXIT.
               EXIT.
