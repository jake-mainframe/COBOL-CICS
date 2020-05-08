       ID DIVISION.
       PROGRAM-ID.    TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
               EXEC CICS
               RECEIVE MAP('TESTMAP') MAPSET('TESTMSD') NOHANDLE
               END-EXEC.
               PERFORM FILL-IN-MAP.
               EXEC CICS
               SEND MAP('TESTMAP') MAPSET('TESTMSD') ERASE
               END-EXEC.
       RESEND-IT.
               IF EIBAID = DFHENTER
                   PERFORM SEND-IN-TRA
               IF EIBAID NOT = DFHENTER
                   GO TO RESEND-IT.
               EXEC CICS RETURN END-EXEC.
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
               MOVE MAPD01O TO SUB-BALANCE.
               COMPUTE NEW-BALANCE = OLD-BALANCE - SUB-BALANCE.
               MOVE NEW-BALANCE TO MAPB01O.
       SEND-IN-TRA-EXIT.
               EXIT.
