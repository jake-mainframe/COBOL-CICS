       ID DIVISION.
       PROGRAM-ID.    PAYMCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SECRET-MESSAGE                    PIC X(9) VALUE 'TELLNOONE'
       01  WS-COMM.
           05  USER-ID                       PIC X(6)
       01  SUB-BALANCE                       PIC 9(3)
       01  TARG-ID                           PIC X(6)
       01  MAINMAP                   PIC X(7) VALUE 'MAINCOB'
      *
       01  CUSTOMER-MASTER-RECORD.
      *
           05  CM-CUSTOMER-NUMBER            PIC X(6).
           05  CM-BALANCE                    PIC 9(3).
      *
       01  TARGET-MASTER-RECORD.
      *
           05  TG-CUSTOMER-NUMBER            PIC X(6).
           05  TG-BALANCE                    PIC 9(3).
      *
       COPY TESTMSD.
       COPY DFHAID.
       LINKAGE SECTION.
       01  DFHCOMMAREA PIC X(100).
       PROCEDURE DIVISION.
               IF EIBAID = DFHCLEAR THEN
                   EXEC CICS RETURN END-EXEC.
               END-IF
               IF EIBAID = DFHPF2 THEN
                   EXEC CICS XCTL PROGRAM(MAINMAP) END-EXEC.
               IF EIBAID = DFHPF12 THEN
                   MOVE DFHCOMMAREA TO WS-COMM
                   EXEC CICS
                   RECEIVE MAP('PAYMMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   MOVE MAPE01O TO SUB-BALANCE
                   MOVE MAPC01O TO TARG-ID
                   PERFORM WRITE-BALAN
                   PERFORM FILL-IN-MAP
                   EXEC CICS
                   SEND MAP('PAYMMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                       TRANSID('PAYM') COMMAREA(WS-COMM)
                   END-EXEC.
               END-IF
               IF EIBAID = DFHPF5 THEN
                   MOVE DFHCOMMAREA TO WS-COMM
                   EXEC CICS
                   RECEIVE MAP('PAYMMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   MOVE MAPA01O TO USER-ID
                   PERFORM BALANCE-REF
                   PERFORM FILL-IN-MAP
                   EXEC CICS
                   SEND MAP('PAYMMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                       TRANSID('PAYM') COMMAREA(WS-COMM)
                   END-EXEC.
               END-IF
               EXEC CICS ASSIGN USERID(USER-ID) END-EXEC
               PERFORM BALANCE-REF
               IF CM-BALANCE = 999 THEN
                  MOVE 'FLAG:RICHAF' TO MAPF01O.
               PERFORM FILL-IN-MAP
               EXEC CICS
               SEND MAP('PAYMMAP') MAPSET('TESTMSD') ERASE
               END-EXEC
               EXEC CICS RETURN
               TRANSID('PAYM') COMMAREA(WS-COMM)
               END-EXEC.
       WRITE-BALAN SECTION.
               PERFORM BALANCE-REF
               COMPUTE CM-BALANCE = CM-BALANCE - SUB-BALANCE
               EXEC CICS
               REWRITE FILE('CUSTMAS')
               FROM(CUSTOMER-MASTER-RECORD)
               END-EXEC
               PERFORM BALANCE-TAR
               COMPUTE TG-BALANCE = TG-BALANCE + SUB-BALANCE
               EXEC CICS
               REWRITE FILE('CUSTMAS')
               FROM(TARGET-MASTER-RECORD)
               END-EXEC
       BALANCE-REF SECTION.
               EXEC CICS
               READ FILE('CUSTMAS')
               INTO(CUSTOMER-MASTER-RECORD)
               RIDFLD(USER-ID)
               UPDATE
               END-EXEC.
       BALANCE-TAR SECTION.
               EXEC CICS
               READ FILE('CUSTMAS')
               INTO(TARGET-MASTER-RECORD)
               RIDFLD(TARG-ID)
               UPDATE
               END-EXEC.
       FILL-IN-MAP SECTION.
               MOVE USER-ID TO MAPA01O
               MOVE CM-BALANCE TO MAPB01O.
       FILL-IN-MAP-EXIT.
               EXIT.
