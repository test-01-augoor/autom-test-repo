      *--------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *--------------------------------------------------------------
       PROGRAM-ID.      PARTMAIN.
       AUTHOR.          WARRIORS.
      *--------------------------------------------------------------
      ***************************************************************
      ***************************************************************
      * Workshop:                   FINAL EXAM
      * Developer:                  Hartanto
      * Created:                    2020-09-18
      * Modified:
      * Developer Contact:
      * V R M:                      V0R0M4
      *  Version Level
      *  Release Level
      *  Modification Level
      ***************************************************************
      ***************************************************************
      * Modifications
      * 2020-09-25 Hartanto  V0R0M4
      *   Fix the name of some variables. Thanks Dave!
      * 2020-09-22 Hartanto  V0R0M3
      *   Fill the rest of the error message with spaces.
      * 2020-09-20 Hartanto  V0R0M2
      *   Make it so that all subroutine returns a status code of 0
      *   before printing anything. And pass in the value for FPARTSUP
      *   directly instead.
      ***************************************************************
      ***************************************************************
      *--------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *--------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FPARTSUP ASSIGN TO DPARTSUP
               FILE STATUS IS FPARTSUP-ST.
           SELECT FPARTS ASSIGN TO DPARTS
               FILE STATUS IS FPARTS-ST.
           SELECT FSUPPS ASSIGN TO DSUPPS
               FILE STATUS IS FSUPPS-ST.
           SELECT FADDRS ASSIGN TO DADDRS
               FILE STATUS IS FADDRS-ST.
           SELECT FPO ASSIGN TO DPO
               FILE STATUS IS FPO-ST.
           SELECT FOUTPUT ASSIGN TO DOUTPUT
               FILE STATUS IS FOUTPUT-ST.
           SELECT FERROR ASSIGN TO DERROR
               FILE STATUS IS FERROR-ST.
      *-------------------------------------------------------------
       DATA DIVISION.
      *-------------------------------------------------------------
       FILE SECTION.
       FD  FPARTSUP.
           COPY 'PARTSUPP'.
       FD  FPARTS.
           COPY 'PARTS'.
       FD  FSUPPS.
           COPY 'SUPLIERS'.
       FD  FADDRS.
           COPY 'ADRESSES'.
       FD  FPO.
           COPY 'PURCHRDS'.
       FD  FOUTPUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS OU-OUTPUT.
       01  OU-OUTPUT                   PIC X(473).
       FD  FERROR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 563 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS OU-ERROR.
       01  OU-ERROR.
           05  OU-ERROUTPUT            PIC X(473).
           05  OU-ERRMESSAGE           PIC X(090).
      *-------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------
      *Return Code**************************************************
      * Three part of return expected from each called subroutine
      *   WS-XXXXXXXX-RETURN-CODE where 0 indicates all good.
      *   WS-XXXXXXXX-RETURN-MESSAGE to indicate any error message
      *      found during the processing of the data. Each error
      *      will have a capacity of 30 to a maximum of 90 char.
      *      If there are more than 3 error, an "INVALID RECORD"
      *      is returned instead.
      *   WS-XXXXXXXX-ERROR-NUMBER return the amount of error.
      *
      *   WS-TOTAL-ERROR-NUMBER will be used later to compute the
      *   total error from all subroutine.
      *   WS-ERROR-LENGTH will be used to determine the length of the
      *   error message submitted by the subroutine.
      *   WS-ERROR-LCTR will be used to count the starting point
      *   of the empty space where the error should be printed
      *   in OU-ERRMESSAGE.
      **************************************************************
       01 CALL-RETURN-CODE.
           05 WS-ERROR-NUMBER.
              10 WS-PARTEDIT-ERROR-NUMBER     PIC 9(03).
              10 WS-SUPPEDIT-ERROR-NUMBER     PIC 9(03).
              10 WS-ADDREDIT-ERROR-NUMBER     PIC 9(03).
              10 WS-POEDIT-ERROR-NUMBER       PIC 9(03).
           05 WS-RETURN-CODE.
              10 WS-PARTEDIT-RETURN-CODE      PIC 9(01).
                88 PARTEDIT-OK                          VALUE 0.
              10 WS-SUPPEDIT-RETURN-CODE      PIC 9(01).
                88 SUPPEDIT-OK                          VALUE 0.
              10 WS-ADDREDIT-RETURN-CODE      PIC 9(01).
                88 ADDREDIT-OK                          VALUE 0.
              10 WS-POEDIT-RETURN-CODE        PIC 9(01).
                88 POEDIT-OK                            VALUE 0.
           05 WS-RETURN-MESSAGE.
              10 WS-PARTEDIT-RETURN-MESSAGE   PIC X(90).
              10 WS-SUPPEDIT-RETURN-MESSAGE   PIC X(90).
              10 WS-ADDREDIT-RETURN-MESSAGE   PIC X(90).
              10 WS-POEDIT-RETURN-MESSAGE     PIC X(90).
       01  TEMP-CALC.
           05 WS-ERROR-LCTR                PIC 9(04) VALUE 1.
           05 WS-ERROR-LENGTH              PIC 9(04).
           05 WS-TOTAL-ERROR-NUMBER        PIC 9(04).
      *Switches*****************************************************
      * To indicate the end of file of PARTSUP and the status code
      * for every of the file used here
      **************************************************************
       01 PROGRAM-SWITCHES.
           05 FPARTSUP-EOF                 PIC X(01) VALUE 'N'.
             88 NO-MORE-FPARTSUP                     VALUE 'Y'.
           05 FPARTSUP-ST                  PIC X(02).
             88 FPARTSUP-OK                          VALUE '00'.
           05 FPARTS-ST                    PIC X(02).
             88 FPARTS-OK                            VALUE '00'.
           05 FSUPPS-ST                    PIC X(02).
             88 FSUPPS-OK                            VALUE '00'.
           05 FADDRS-ST                    PIC X(02).
             88 FADDRS-OK                            VALUE '00'.
           05 FPO-ST                       PIC X(02).
             88 FPO-OK                               VALUE '00'.
           05 FOUTPUT-ST                   PIC X(02).
             88 FOUTPUT-OK                           VALUE '00'.
           05 FERROR-ST                    PIC X(02).
             88 FERROR-OK                            VALUE '00'.
      *-------------------------------------------------------------
       PROCEDURE DIVISION.
      *-------------------------------------------------------------
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-MAIN-PARTSUP UNTIL NO-MORE-FPARTSUP.
           PERFORM 900-CLOSE.
           PERFORM 900-CALL-PRINT-REPORT.
           GOBACK.
       100-HOUSEKEEPING.
           PERFORM 110-OPEN-FILES.
           PERFORM 210-READ-PARTSUP.
       110-OPEN-FILES.
           OPEN INPUT FPARTSUP.
            IF NOT FPARTSUP-OK
              DISPLAY "PARTSUPP FILE PROBLEM"
              GOBACK.
           OPEN OUTPUT FPARTS
            IF NOT FPARTS-OK
              DISPLAY 'PARTS FILE PROBLEM'
              GOBACK.
           OPEN OUTPUT FSUPPS
            IF NOT FSUPPS-OK
              DISPLAY 'SUPPLIERS FILE PROBLEM'
              GOBACK.
           OPEN OUTPUT FADDRS
            IF NOT FADDRS-OK
              DISPLAY 'ADDRESS FILE PROBLEM'
              GOBACK.
           OPEN OUTPUT FPO
            IF NOT FPO-OK
              DISPLAY 'PO FILE PROBLEM'
              GOBACK.
           OPEN OUTPUT FOUTPUT
            IF NOT FOUTPUT-OK
              DISPLAY 'OUTPUT FILE PROBLEM'
              GOBACK.
           OPEN OUTPUT FERROR
            IF NOT FERROR-OK
              DISPLAY 'ERROR FILE PROBLEM'
              GOBACK.
      *Main Data File Processing************************************
      * Perform all the subrouting checking first, if everything is
      * okay, then write records. If got error, and error >= 4, move
      * "INVALID RECORD" to ERRMESSAGE. Otherwise, print the
      * ERRMESSAGE from the subroutine
      **************************************************************
       200-MAIN-PARTSUP.
           MOVE 0 TO WS-ERROR-NUMBER.
           MOVE 0 TO WS-RETURN-CODE.
           MOVE SPACES TO WS-RETURN-MESSAGE.
           MOVE 0 TO TEMP-CALC.
           MOVE 1 TO WS-ERROR-LCTR.
           PERFORM 220-PART-PROCESS.
           PERFORM 220-SUPP-PROCESS.
           PERFORM 220-ADDR-PROCESS.
           PERFORM 220-PO-PROCESS.
           IF PARTEDIT-OK AND SUPPEDIT-OK AND ADDREDIT-OK AND POEDIT-OK
               WRITE OU-PARTS
               WRITE OU-SUPPLIERS
               WRITE OU-SUPP-ADDRESSES-MAIN
               WRITE OU-PO-MAIN
               MOVE PART-SUPP-ADDR-PO TO OU-OUTPUT
               WRITE OU-OUTPUT
           ELSE
               PERFORM 200-PRINT-BUGGY-RECORD
           END-IF.
           PERFORM 210-READ-PARTSUP.
      *Print Buggy Records******************************************
      * Calculate the total number of error found on the subroutines.
      * If more than 3 error, straight out print INVALID RECORD as the
      * error message. However if less than 3, every error message
      * is printed individually.
      **************************************************************
       200-PRINT-BUGGY-RECORD.
           MOVE PART-SUPP-ADDR-PO TO OU-ERROUTPUT.
           COMPUTE WS-TOTAL-ERROR-NUMBER = WS-PARTEDIT-ERROR-NUMBER
               + WS-SUPPEDIT-ERROR-NUMBER + WS-POEDIT-ERROR-NUMBER +
               WS-ADDREDIT-ERROR-NUMBER.
           IF WS-TOTAL-ERROR-NUMBER >= 4 THEN
               MOVE 'INVALID RECORD' TO OU-ERRMESSAGE
           ELSE
               IF WS-PARTEDIT-ERROR-NUMBER NOT = 0 THEN
                   COMPUTE WS-ERROR-LENGTH =
                       WS-PARTEDIT-ERROR-NUMBER * 30
                   MOVE WS-PARTEDIT-RETURN-MESSAGE(1:WS-ERROR-LENGTH)
                       TO OU-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
                   ADD WS-ERROR-LENGTH TO WS-ERROR-LCTR
               END-IF
               IF WS-SUPPEDIT-ERROR-NUMBER NOT = 0 THEN
                   COMPUTE WS-ERROR-LENGTH =
                       WS-SUPPEDIT-ERROR-NUMBER * 30
                   MOVE WS-SUPPEDIT-RETURN-MESSAGE(1:WS-ERROR-LENGTH)
                       TO OU-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
                   ADD WS-ERROR-LENGTH TO WS-ERROR-LCTR
               END-IF
               IF WS-ADDREDIT-ERROR-NUMBER NOT = 0 THEN
                   COMPUTE WS-ERROR-LENGTH =
                       WS-ADDREDIT-ERROR-NUMBER * 30
                   MOVE WS-ADDREDIT-RETURN-MESSAGE(1:WS-ERROR-LENGTH)
                       TO OU-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
                   ADD WS-ERROR-LENGTH TO WS-ERROR-LCTR
               END-IF
               IF WS-POEDIT-ERROR-NUMBER NOT = 0 THEN
                   COMPUTE WS-ERROR-LENGTH =
                       WS-POEDIT-ERROR-NUMBER * 30
                   MOVE WS-POEDIT-RETURN-MESSAGE(1:WS-ERROR-LENGTH)
                       TO OU-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
                   ADD WS-ERROR-LENGTH TO WS-ERROR-LCTR
               END-IF
               IF WS-ERROR-LCTR < 91 THEN
                   COMPUTE WS-ERROR-LENGTH =
                       (3 - WS-TOTAL-ERROR-NUMBER) * 30
                   MOVE SPACES TO
                       OU-ERRMESSAGE(WS-ERROR-LCTR:WS-ERROR-LENGTH)
               END-IF
           END-IF.
           WRITE OU-ERROR.
           DISPLAY "BUGGY RECORD".
      *Reading PARTSUP**********************************************
      * This paragraph handles the reading of PARTSUPP for separation
      * and to be passed over to multiple subroutine
      **************************************************************
       210-READ-PARTSUP.
           READ FPARTSUP
               AT END MOVE "Y" TO FPARTSUP-EOF
           END-READ.
           IF FPARTSUP-OK OR NO-MORE-FPARTSUP
               NEXT SENTENCE
           ELSE
               DISPLAY 'PARTSUP FILE PROBLEM'
               PERFORM 900-CLOSE
               GOBACK
           END-IF.
      *Call Subroutine***********************************************
      * Each of the 220-XXXX-PROCESS paragraph will call a subroutine
      * to process the data and ensuring its validity. If the data is
      * valid, they are printed to the corresponding data file.
      **************************************************************
       220-PART-PROCESS.
           MOVE PARTS IN FPARTSUP TO OU-PARTS.
           CALL "PARTEDIT" USING PART-NUMBER,
                               PART-NAME,
                               WEEKS-LEAD-TIME,
                               VEHICLE-MODEL,
                               VEHICLE-MAKE,
                               VEHICLE-YEAR,
                               WS-PARTEDIT-RETURN-CODE,
                               WS-PARTEDIT-RETURN-MESSAGE,
                               WS-PARTEDIT-ERROR-NUMBER.
       220-SUPP-PROCESS.
           MOVE SUPPLIERS IN FPARTSUP TO OU-SUPPLIERS.
           CALL "SUPPEDIT" USING SUPPLIER-CODE,
                               SUPPLIER-TYPE,
                               SUPPLIER-NAME,
                               SUPPLIER-PERF,
                               SUPPLIER-RATING,
                               SUPPLIER-STATUS,
                               SUPPLIER-ACT-DATE,
                               WS-SUPPEDIT-RETURN-CODE,
                               WS-SUPPEDIT-RETURN-MESSAGE,
                               WS-SUPPEDIT-ERROR-NUMBER.
       220-ADDR-PROCESS.
           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX = 3
               MOVE SUPP-ADDRESS(ADDR-IDX)
                   TO OU-SUPP-ADDRESS(ADDR-IDX)
           END-PERFORM.
           CALL "ADDREDIT" USING SUPP-ADDRESS(1),
                               SUPP-ADDRESS(2),
                               SUPP-ADDRESS(3),
                               WS-ADDREDIT-RETURN-CODE,
                               WS-ADDREDIT-RETURN-MESSAGE,
                               WS-ADDREDIT-ERROR-NUMBER.
       220-PO-PROCESS.
           PERFORM VARYING PO-IDX FROM 1 BY 1 UNTIL PO-IDX = 3
               MOVE PURCHASE-ORDER(PO-IDX)
                   TO OU-PURCHASE-ORDER(PO-IDX)
           END-PERFORM.
           CALL "POEDIT" USING PURCHASE-ORDER(1),
                              PURCHASE-ORDER(2),
                              PURCHASE-ORDER(3),
                              WS-POEDIT-RETURN-CODE,
                              WS-POEDIT-RETURN-MESSAGE,
                              WS-POEDIT-ERROR-NUMBER.
      *Closing Functions********************************************
      * This last two paragraphs will close all files ever opened
      * and then call PRTRPT which will print the report of the data
      **************************************************************
       900-CLOSE.
           CLOSE FPARTSUP, FPARTS, FSUPPS, FADDRS, FPO, FOUTPUT, FERROR.
       900-CALL-PRINT-REPORT.
      *     CALL "PRTRPT".
