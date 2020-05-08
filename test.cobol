       ID DIVISION.
       PROGRAM-ID.    TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ATT                               PIC X(1)
       01  OLD-BALANCE                       PIC 9(3)
       01  SUB-BALANCE                       PIC 9(3)
       01  NEW-BALANCE                       PIC 9(3)
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
               MOVE EIBAID TO ATT.
               IF ATT = DFHCLEAR THEN
                   EXEC CICS RETURN END-EXEC.
               END-IF
               IF ATT = DFHPF12 THEN
                   EXEC CICS
                   RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
                   END-EXEC
                   PERFORM FILL-IN-MAP
                   MOVE 'TEST' TO MAPD01O
                   EXEC CICS
                   SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
                   END-EXEC
                   EXEC CICS RETURN
                       TRANSID(EIBTRNID)
                   END-EXEC.
               END-IF
               EXEC CICS
               RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
               END-EXEC.
               PERFORM FILL-IN-MAP.
               EXEC CICS
               SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
               END-EXEC.
               EXEC CICS RETURN
                   TRANSID(EIBTRNID)
               END-EXEC.
       FILL-IN-MAP SECTION.
               MOVE LOW-VALUES TO TESTMAPO.
               EXEC CICS ASSIGN USERID(MAPA01O) END-EXEC.
               EXEC CICS
               READ FILE('CUSTMAS')
                    INTO(CUSTOMER-MASTER-RECORD)
                    RIDFLD(MAPA01O)
               END-EXEC.
               MOVE CM-BALANCE TO MAPB01O.
       FILL-IN-MAP-EXIT.
               EXIT.
       SEND-IN-TRA SECTION.
               MOVE LOW-VALUES TO TESTMAPO.
               EXEC CICS
               READ FILE('CUSTMAS')
                    INTO(CUSTOMER-MASTER-RECORD)
                    RIDFLD(MAPA01O)
               END-EXEC.
               MOVE CM-BALANCE TO OLD-BALANCE.
       SEND-IN-TRA-EXIT.
               EXIT.
