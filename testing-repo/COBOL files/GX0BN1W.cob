       PROCESS
               APOST.
       IDENTIFICATION DIVISION.

       PROGRAM-ID.               GX0BN1W.
      * Description: Selection List TGCTD
       AUTHOR.                   GENEXUS 18_0_0-165820
                                 FOR IBM COBOL/400.
       DATE-WRITTEN.             4 de Noviembre de 2022.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.          IBM-AS400.
       OBJECT-COMPUTER.          IBM-AS400.
       SPECIAL-NAMES.            I-O-FEEDBACK IS DISPLAY-FEEDBACK
                                 DECIMAL-POINT IS COMMA
                                 .

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F0-TGCTD ASSIGN TO DATABASE-ITGCTD INDEXED
                  ACCESS DYNAMIC RECORD EXTERNALLY-DESCRIBED-KEY
                   STATUS V-FS.
           SELECT GX-CRT-FILE ASSIGN TO WORKSTATION-SW005308-SI
                  TRANSACTION ACCESS DYNAMIC RELATIVE GX-SFLRELRECNBR
                  STATUS GX-WS-STATUS.
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       FD  F0-TGCTD LABEL RECORDS ARE OMITTED.
       01  F0-TGCTD-RCD.
           COPY DDSR-ALL-FORMATS OF ITGCTD.
       FD  GX-CRT-FILE LABEL RECORDS ARE OMITTED.
       01  GX-CRT-RECORD.
           COPY DDSR-ALL-FORMATS OF SW005308 .
       WORKING-STORAGE SECTION.

       01  GX-INT-FMT-DATE.
           05 YY                          PIC  9(004).
           05 MM                          PIC  9(002).
           05 DD                          PIC  9(002).
       01  GX-USR-FMT-DATE.
           05 DD                          PIC  9(002).
           05 MM                          PIC  9(002).
           05 YY                          PIC  9(002).
       01  GX-USR-FMT-DATE-W-C.
           05 DD                          PIC  9(002).
           05 MM                          PIC  9(002).
           05 YY                          PIC  9(004).
       01  GX-EXT-DTIME.
           05 YY                          PIC  9(004).
           05 GX-EXT-DTIME-S1             PIC  X.
           05 MM                          PIC  9(002).
           05 GX-EXT-DTIME-S2             PIC  X.
           05 DD                          PIC  9(002).
           05 GX-EXT-DTIME-S3             PIC  X.
           05 HH                          PIC  9(002).
           05 GX-EXT-DTIME-D1             PIC  X.
           05 MIN                         PIC  9(002).
           05 GX-EXT-DTIME-D2             PIC  X.
           05 SS                          PIC  9(002).
           05 GX-EXT-DTIME-D3             PIC  X.
           05 MS                          PIC  9(006).
       01  GX-INT-FMT-DTIME.
           05 YY                          PIC  9(004).
           05 FILLER                      PIC  X(001) VALUE '-'.
           05 MM                          PIC  9(002).
           05 FILLER                      PIC  X(001) VALUE '-'.
           05 DD                          PIC  9(002).
           05 FILLER                      PIC  X(001) VALUE '-'.
           05 HH                          PIC  9(002).
           05 FILLER                      PIC  X(001) VALUE '.'.
           05 MIN                         PIC  9(002).
           05 FILLER                      PIC  X(001) VALUE '.'.
           05 SS                          PIC  9(002).
           05 FILLER                      PIC  X(001) VALUE '.'.
           05 MS                          PIC  X(006) VALUE '000000'.
       01  GX-BOOLEAN                     PIC S9(001)       VALUE 1.
           88 GX-TRUE                                       VALUE 1.
           88 GX-FALSE                                      VALUE 0.
       01  GX-FIRST-TIME                  PIC  X(001)       VALUE '1'.
       01  GX-COMMIT                      PIC S9      COMP-3.
       01  GX-ROLLBACK                    PIC S9      COMP-3.
       01  GX-RETURN                      PIC S9      COMP-3.
       01  GX-EXIT-LEVEL                  PIC S9      COMP-3.
       01  GX-ALARM                       PIC S9      COMP-3.
       01  GX-QCMDEXC-PTR                 PIC S9(003) COMP-3 VALUE 1.
       01  GX-QCMDEXC-CMD                 PIC  X(3000).
       01  GX-QCMDEXC-CMDLEN              PIC S9(10)V9(5) COMP-3.
       01  GX-STATUS-VARIABLES.
           05 GX-CSRROW                   PIC S9(002) COMP-4.
           05 GX-CSRCOL                   PIC S9(002) COMP-4.
           05 GX-LREC                     PIC S9(004) COMP-4.
           05 GX-SFL-PAGE-COUNT           PIC S9(004) COMP-4.
           05 GX-SFLRELRECNBR             PIC  9(004) COMP-4.
           05 GX-WS-STATUS                PIC  X(002).
           05 GX-SFLRECNBR                PIC S9(004) COMP-4.
           05 GX-S-SFLRECNBR              PIC S9(004) COMP-4.
           05 GX-NBR-OF-SELECTIONS        PIC S9(003) COMP-3.
           05 GX-WS-FMT                   PIC  X(010).
           05 GX-MODETXT                  PIC  X(010).
           05 GX-MODE-FLAG                PIC  X(003).
              88 INSERT-MODE                                VALUE 'INS'.
              88 UPDATE-MODE                                VALUE 'UPD'.
              88 DELETE-MODE                                VALUE 'DLT'.
              88 DISPLAY-MODE                               VALUE 'DSP'.
           05 MISCELANEOUS-FLAGS.
              10 FILLER                   PIC 1.
              10 FILLER                   PIC 1.
                 88 INSRCD                                  VALUE B'1'.
                 88 NO-INSRCD                               VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 DELRCD                                  VALUE B'1'.
                 88 NO-DELRCD                               VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 MODIFIED-MARK-ON                        VALUE B'1'.
                 88 MODIFIED-MARK-OFF                       VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 RECORD-HAS-ERRORS                       VALUE B'1'.
                 88 RECORD-HAS-NO-ERRORS                    VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 ALWAYS-ON                               VALUE B'1'.
              10 FILLER                   PIC 1.
                 88 ALARM-ON                                VALUE B'1'.
                 88 ALARM-OFF                               VALUE B'0'.
              10 FILLER        OCCURS  3  PIC 1.
           05 SUBFILE-FLAGS.
              10 FILLER                   PIC 1.
              10 FILLER                   PIC 1.
                 88 DISPLAY-SFL                             VALUE B'1'.
                 88 NO-DISPLAY-SFL                          VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 DISPLAY-SFLCTL                          VALUE B'1'.
                 88 NO-DISPLAY-SFLCTL                       VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 SFL-END                                 VALUE B'1'.
                 88 NO-SFL-END                              VALUE B'0'.
           05 LEVEL-FLAGS.
              10 FILLER                   PIC 1.
                 88 ERRORS-FOUND                            VALUE B'1'.
                 88 NO-ERRORS-FOUND                         VALUE B'0'.
              10 FILLER                   PIC 1.
              10 FILLER                   PIC 1.
                 88 MORE-SFL-REC                            VALUE B'1'.
                 88 NO-MORE-SFL-REC                         VALUE B'0'.
              10 FILLER                   PIC 1.
              10 FILLER                   PIC 1.
              10 FILLER                   PIC 1.
                 88 RECORD-INSERTED                         VALUE B'1'.
                 88 RECORD-NOT-INSERTED                     VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 RECORD-UPDATED                          VALUE B'1'.
                 88 RECORD-NOT-UPDATED                      VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 RECORD-DELETED                          VALUE B'1'.
                 88 RECORD-NOT-DELETED                      VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 RECORD-DISPLAYED                        VALUE B'1'.
                 88 RECORD-NOT-DISPLAYED                    VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 RECORD-PROCESSED                        VALUE B'1'.
                 88 NO-RECORD-PROCESSED                     VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 LOOKUP-AVAILABLE                        VALUE B'1'.
                 88 LOOKUP-NOT-AVAILABLE                    VALUE B'0'.
              10 FILLER                   PIC 1.
                 88 SELECTION-DISPLAYED                     VALUE B'1'.
                 88 SELECTION-NOT-DISPLAYED                 VALUE B'0'.
       01  MSG-DESCRIPTION                PIC  X(078).
       01  FILLER.
           05 MSG-ARRAY       OCCURS 100.
              10 MSG-TYPE                 PIC  X(001).
                 88 ERROR-MSG                               VALUE '1'.
                 88 WARNING-MSG                             VALUE '0'.
              10 MSG-TEXT                 PIC  X(078).
       01  MSG-COUNTER                    PIC  9(003) COMP-4.
       01  MSG-COUNTER-LOOP               PIC  9(003) COMP-4.
      * --- Standard variables for scr program
       01  GX-MATCHING-FLAG PIC X.
           88 GX-MATCHING        VALUE '1'.
           88 GX-NO-MATCHING     VALUE '0'.
       01  GX-RESTART-FLAG PIC X.
           88 GX-RESTART         VALUE '1'.
           88 GX-NO-RESTART      VALUE '0'.
       01  GX-REPOSITION-FLAG PIC X.
           88 GX-REPOSITION      VALUE '1'.
           88 GX-NO-REPOSITION   VALUE '0'.
       01  GX-LOADED-FLAG     PIC X.
           88 GX-LOADED          VALUE '1'.
           88 GX-NOT-LOADED      VALUE '0'.
       01  V-FS                     PIC  XX.
       01  GX-IO-CODE               PIC S9(0004) COMP-3.
           88 GX-IO-EOF                                 VALUE  100.
           88 GX-IO-LOCKED-FILE                         VALUE -001.
           88 GX-IO-LOCKED-RCD                          VALUE -002.
           88 GX-IO-DUP-KEY                             VALUE -003.
           88 GX-IO-BAD-RCD-LEN                         VALUE -004.
           88 GX-IO-MISSING-READ                        VALUE -005.
           88 GX-IO-OTHER-ERRORS                        VALUE -999.
           88 GX-RCD-FOUND                              VALUE    0.
           88 GX-IO-OK                                  VALUE    0.
      * --- Variables for referenced routines.
       01  GX-SFL-RCDCNT                  PIC S9(004).
       01  GX-SFL-UR                      PIC S9(002).
       01  GX-SFL-LR                      PIC S9(002).

       01  GX-OVRDBF.
           02 FILLER                   PIC X(012) VALUE 'OVRDBF FILE('.
           02 GX-ODBF-FILE             PIC X(021).
           02 FILLER                   PIC X(009) VALUE ') TOFILE('.
           02 GX-ODBF-TOFILE           PIC X(021).
           02 FILLER                   PIC X(008) VALUE ') SHARE('.
           02 GX-ODBF-SHARE            PIC X(004).
           02 FILLER                   PIC X(009) VALUE ') LVLCHK('.
           02 GX-ODBF-LVLCHK           PIC X(004).
           02 FILLER                   PIC X(010) VALUE ') WAITRCD('.
           02 GX-ODBF-WAITRCD          PIC X(005).
           02 FILLER                   PIC X(001) VALUE ')'.
       01  GX-RST-SCR                     PIC S9      COMP-3.
       01  WS-FEEDBACK-AREA.
           05 FILLER                      PIC  X(144).
           05 FILLER.
              10 FILLER                   PIC  X(002).
              10 WS-AID                   PIC  X(001).
              10 WS-CURSOR.
                 15 WS-ROW                PIC  X(001).
                 15 WS-COL                PIC  X(001).
              10 FILLER                   PIC  X(006).
              10 WS-FIRST-SFL-REC         PIC  9(004) COMP-4.
              10 FILLER                   PIC  X(019).
              10 FILLER                   PIC  X(067).
       01  WS-AID-BYTE-SAVED              PIC  X(002).
       01  WS-AID-BYTE.
           07 FILLER                      PIC  X(001) VALUE LOW-VALUE.
           07 WS-AID-BYTE-HEX             PIC  X(001).
       01  WS-AID-BYTE-REDEFINED
           REDEFINES WS-AID-BYTE          PIC S9(04) COMP-4.
           88  WS-KEY-F1                             VALUE +49.
           88  WS-KEY-F2                             VALUE +50.
           88  WS-KEY-F3                             VALUE +51.
           88  WS-KEY-F4                             VALUE +52.
           88  WS-KEY-F5                             VALUE +53.
           88  WS-KEY-F6                             VALUE +54.
           88  WS-KEY-F7                             VALUE +55.
           88  WS-KEY-F8                             VALUE +56.
           88  WS-KEY-F9                             VALUE +57.
           88  WS-KEY-F10                            VALUE +58.
           88  WS-KEY-F11                            VALUE +59.
           88  WS-KEY-F12                            VALUE +60.
           88  WS-KEY-F13                            VALUE +177.
           88  WS-KEY-F14                            VALUE +178.
           88  WS-KEY-F15                            VALUE +179.
           88  WS-KEY-F16                            VALUE +180.
           88  WS-KEY-F17                            VALUE +181.
           88  WS-KEY-F18                            VALUE +182.
           88  WS-KEY-F19                            VALUE +183.
           88  WS-KEY-F20                            VALUE +184.
           88  WS-KEY-F21                            VALUE +185.
           88  WS-KEY-F22                            VALUE +186.
           88  WS-KEY-F23                            VALUE +187.
           88  WS-KEY-F24                            VALUE +188.
           88  WS-KEY-CLEAR                          VALUE +189.
           88  WS-KEY-ENTER                          VALUE +241 +63.
           88  WS-KEY-HELP                           VALUE +243.
           88  WS-KEY-PAGE-UP                        VALUE +244.
           88  WS-KEY-PAGE-DOWN                      VALUE +245.
           88  WS-KEY-PRINT                          VALUE +246.
           88  WS-KEY-REC-BKSP                       VALUE +248.
           88  WS-KEY-AUTO-ENTER                     VALUE +63.
       01  WS-ROW-BYTE.
           07 FILLER                      PIC  X(001).
           07 WS-ROW-BYTE-HEX             PIC  X(001).
       01  WS-ROW-POS
           REDEFINES WS-ROW-BYTE          PIC  9(002) COMP-4.
       01  WS-COL-BYTE.
           07 FILLER                      PIC  X(001).
           07 WS-COL-BYTE-HEX             PIC  X(001).
       01  WS-COL-POS
           REDEFINES WS-COL-BYTE          PIC  9(002) COMP-4.
       01  GX-INDICATORS.
           05 FILLER       OCCURS 99      PIC 1 INDICATOR 1.
       01  FILLER REDEFINES GX-INDICATORS.
           05 GX-ATT-IND.
              10 IND-ARRAY OCCURS 79      PIC 1.
           05 MISCELANEOUS-INDICATORS.
              10 FILLER    OCCURS 10      PIC 1.
           05 SUBFILE-INDICATORS.
              10 FILLER    OCCURS 10      PIC 1.
       01  GX-S-CPOS.
           05 GX-S-ROW                PIC S9(003).
           05 GX-S-COL                PIC S9(003).
      * --- User and temporary variables.
       01  GX-MSGDTA                      PIC X(256) VALUE SPACES.
       01  GX-MSGID                       PIC X(7) VALUE SPACES.
       01  GX-MSGTYPE                     PIC X(1) VALUE SPACES.
       01  GX-RETRY-ANS                   PIC X(1) VALUE SPACES.
       01  GX-RETRY-MSG                   PIC X(256) VALUE SPACES.
       01  GX-LOCK-COUNT                  PIC S9(3) COMP-3 VALUE ZEROES.
       01  GX-IO-OP                       PIC X(10) VALUE SPACES.
       01  GX-IO-XFI                      PIC X(256) VALUE SPACES.
       01  L002-EXIT                      PIC S9(1) COMP-3 VALUE ZEROES.
       01  GX-LVL0002                     PIC S9(1) COMP-3 VALUE ZEROES.
       01  F0-TGCTD-EOF                   PIC S9(1) COMP-3 VALUE ZEROES.
       01  W-FETCH-DONE-ON-0419           PIC S9(1) COMP-3 VALUE ZEROES.
       01  MB0002-N                       PIC S9(3) COMP-3 VALUE ZEROES.
       01  MB0002-C                       PIC S9(3) COMP-3 VALUE ZEROES.
       01   M-MB0002-A-R .
           05 FILLER PIC X(078) VALUE 'F3=Salir  F24=Más teclas' .
           05 FILLER PIC X(078) VALUE 'F10=WARNING - Message text for ''
      -    'menu'' lang. ''spa'' not found  F24=Más teclas' .
           05 FILLER PIC X(078) VALUE 'F12=Cancelar  F24=Más teclas' .
       01 M-MB0002-A REDEFINES M-MB0002-A-R .
           02 M-MB0002-A-02 OCCURS 3 .
              03 MB0002-A PIC X(78)  .
       01  GX-MB-EXIT                     PIC S9(1) COMP-3 VALUE ZEROES.
       01  GX-TMPINT                      PIC S9(18) COMP-3 VALUE
            ZEROES.
       01  GX-CSRSET                      PIC S9(1) COMP-3 VALUE ZEROES.
       01  GX-ACT-SFL-REC                 PIC S9(5) COMP-3 VALUE ZEROES.
       01  GXO-V7PTGFCHVIG                PIC X(8) VALUE ZEROES.
       01  GXO-V6CCTDID                   PIC 9(3) VALUE ZEROES.
       01  GXO-V5CHRID                    PIC 9(4) VALUE ZEROES.
       01  GX-SAVSFLREC                   PIC S9(5) COMP-3 VALUE ZEROES.
       01  FK-L0002-N                     PIC S9(3) COMP-3 VALUE ZEROES.
       01  FK-L0002-C                     PIC S9(3) COMP-3 VALUE ZEROES.
       01   M-FK-L0002-A-R .
           05 FILLER PIC X(078) VALUE 'F3=Salir  F5=Renovar  F24=Más tec
      -    'las' .
           05 FILLER PIC X(078) VALUE 'F10=WARNING - Message text for ''
      -    'menu'' lang. ''spa'' not found  F24=Más teclas' .
           05 FILLER PIC X(078) VALUE 'F12=Cancelar  F24=Más teclas' .
       01 M-FK-L0002-A REDEFINES M-FK-L0002-A-R .
           02 M-FK-L0002-A-02 OCCURS 3 .
              03 FK-L0002-A PIC X(78)  .
       01  GX-KEEP-SP                     PIC S9(5) COMP-3 VALUE ZEROES.
       01  GXV-V5CHRID                    PIC 9(4) VALUE ZEROES.
       01  GXV-V6CCTDID                   PIC 9(3) VALUE ZEROES.
       01  GXV-V9GX-ERR                   PIC S9(3) VALUE ZEROES.
       01  GXV-V10GX-EMSG                 PIC X(70) VALUE SPACES.
       01  GXV-V11PGMDESC                 PIC X(30) VALUE SPACES.
       01  GXV-HRID                       PIC 9(4) VALUE ZEROES.
       01  GXV-CTDID                      PIC 9(3) VALUE ZEROES.
       01  GXV-TGFCHVIG                   PIC X(8) VALUE ZEROES.
       01  GXV-GXPDOP                     PIC X(2) VALUE SPACES.
       01  GXV-GXOPTF                     PIC X(1) VALUE SPACES.
       01  GX-MODIFIED                    PIC S9(1) COMP-3 VALUE ZEROES.
       01  GXV-V8PHRID                    PIC 9(4) VALUE ZEROES.
       01  GXV-V7PTGFCHVIG                PIC X(8) VALUE ZEROES.
       01  PARENT-MODE-0002               PIC X(3) VALUE SPACES.
       01  CUR-SFL-RCD-NBR-0002           PIC S9(5) COMP-3 VALUE ZEROES.
       01  SHR-SFL-RCD-CNT-0002           PIC S9(5) COMP-3 VALUE ZEROES.
       01  EXC-SFL-RCD-CNT-0002           PIC S9(5) COMP-3 VALUE ZEROES.
       01  STS-VARIABLES-0002             PIC X(500) VALUE SPACES.
      * --- String constants.
       LINKAGE SECTION.
       01  GXL-V7PTGFCHVIG                PIC X(8).
       01  GXL-V8PHRID                    PIC 9(4).
       PROCEDURE DIVISION USING
                                GXL-V7PTGFCHVIG
                                GXL-V8PHRID
                                .
       MAIN.

           MOVE GXL-V7PTGFCHVIG TO GXV-V7PTGFCHVIG
           MOVE GXL-V8PHRID TO GXV-V8PHRID
      * --- Variables initialization
           SET  NO-INSRCD            TO TRUE
           SET  NO-DELRCD            TO TRUE
           SET  MODIFIED-MARK-OFF    TO TRUE
           SET  RECORD-HAS-NO-ERRORS TO TRUE
           SET  ALWAYS-ON            TO TRUE
           MOVE ZEROES               TO GX-ALARM
           MOVE ZEROES               TO GX-RST-SCR
           MOVE ZEROES               TO GX-INDICATORS
           MOVE ZEROES               TO WS-ROW-POS
           MOVE ZEROES               TO WS-COL-POS,
           MOVE ZEROES               TO WS-FIRST-SFL-REC
           MOVE ZEROES               TO WS-AID-BYTE-REDEFINED
           MOVE ZEROES               TO MSG-COUNTER
           MOVE ZEROES               TO GX-SFLRELRECNBR
           MOVE ZEROES               TO GX-S-CPOS

           MOVE 1 TO GX-MODIFIED
           MOVE ZEROES TO GX-MODIFIED
           MOVE SPACES TO PARENT-MODE-0002
           MOVE ZEROES TO CUR-SFL-RCD-NBR-0002
           MOVE ZEROES TO SHR-SFL-RCD-CNT-0002
           MOVE ZEROES TO EXC-SFL-RCD-CNT-0002
           MOVE SPACES TO STS-VARIABLES-0002
           MOVE SPACES TO GXV-GXOPTF
           MOVE SPACES TO GXV-GXPDOP
           MOVE ZEROES TO GXV-TGFCHVIG
           MOVE ZEROES TO GXV-CTDID
           MOVE ZEROES TO GXV-HRID
           MOVE 'Selection List TGCTD' TO GXV-V11PGMDESC
           MOVE SPACES TO GXV-V10GX-EMSG
           MOVE ZEROES TO GXV-V9GX-ERR
           MOVE ZEROES TO GXV-V6CCTDID
           MOVE ZEROES TO GXV-V5CHRID
           MOVE 0 TO GX-COMMIT
           MOVE 0 TO GX-ROLLBACK
           MOVE 0 TO GX-EXIT-LEVEL
           MOVE 0 TO GX-RETURN
      * --- End of initialization
           IF GX-FIRST-TIME = '1'
              OPEN I-O GX-CRT-FILE
           END-IF
           PERFORM CLR-SCREEN
           IF GX-FIRST-TIME = '1'
              MOVE 'ITGCTD' TO GX-ODBF-FILE
              MOVE 'ITGCTD' TO GX-ODBF-TOFILE
              MOVE '*NO' TO GX-ODBF-SHARE
              MOVE ' ' TO GX-ODBF-WAITRCD
              MOVE '*NO' TO GX-ODBF-LVLCHK
              MOVE GX-OVRDBF TO GX-QCMDEXC-CMD
              MOVE 110 TO GX-QCMDEXC-CMDLEN
              CALL 'QCMDEXC' USING GX-QCMDEXC-CMD
                                   GX-QCMDEXC-CMDLEN
              MOVE 'opening' TO GX-IO-OP
              MOVE 'F0-TGCTD' TO GX-IO-XFI
              MOVE 0 TO GX-LOCK-COUNT
              PERFORM TEST AFTER UNTIL GX-IO-CODE NOT LESS ZERO
                 OPEN INPUT F0-TGCTD
                 PERFORM GX-FILE-STATUS-ANALYSIS
                 PERFORM GX-IOERR-LOOP
              END-PERFORM
           END-IF
           SET GX-NO-MATCHING   TO TRUE
           SET GX-NO-RESTART    TO TRUE
           SET GX-NO-REPOSITION TO TRUE
           SET GX-NOT-LOADED    TO TRUE
           SET DISPLAY-MODE     TO TRUE
           MOVE ZEROES TO SUBFILE-FLAGS
                          LEVEL-FLAGS

           MOVE 0 TO GX-KEEP-SP
           MOVE 3 TO FK-L0002-C
           MOVE 1 TO FK-L0002-N
           PERFORM T0001-EVENT
              THRU T0001-EVENT-EXIT
           IF GX-RETURN = 0
              PERFORM T0002-MAIN
           END-IF
           IF MSG-COUNTER GREATER ZERO
              PERFORM T0002-DSPWSCR
              MOVE GX-SFLRELRECNBR TO GX-SAVSFLREC
              PERFORM GX-MSG-MANAGER
              PERFORM GX-WAITMSG
              MOVE GX-SAVSFLREC TO GX-SFLRELRECNBR
           END-IF
           MOVE GXV-V7PTGFCHVIG TO GXL-V7PTGFCHVIG
           MOVE GXV-V8PHRID TO GXL-V8PHRID
           MOVE '0' TO GX-FIRST-TIME
           GOBACK
           CONTINUE.

       T0002-MAIN.

           MOVE GXV-V5CHRID TO GXO-V5CHRID
           MOVE GXV-V6CCTDID TO GXO-V6CCTDID
           MOVE GXV-V7PTGFCHVIG TO GXO-V7PTGFCHVIG
           SET GX-LOADED TO TRUE
           PERFORM T0002-RFRSHSCR
              THRU T0002-RFRSHSCR-EXIT
           PERFORM UNTIL GX-RETURN = 1 OR GX-EXIT-LEVEL = 1
      * Refresh logic
              IF GX-RESTART OR
                 GXV-V5CHRID NOT EQUAL GXO-V5CHRID OR
                 GXV-V6CCTDID NOT EQUAL GXO-V6CCTDID OR
                 GXV-V7PTGFCHVIG NOT EQUAL GXO-V7PTGFCHVIG
                 IF GX-LOADED
                    PERFORM T0002-CLOSE
                    PERFORM T0002-CLR-SFL
                 END-IF
                 PERFORM T0002-RFRSHSCR
                    THRU T0002-RFRSHSCR-EXIT
                 SET GX-LOADED TO TRUE
              END-IF
      * Update active subfile record
              IF GX-ACT-SFL-REC = GX-SFLRELRECNBR
                 MOVE 'B0100' TO GX-WS-FMT
                 PERFORM READ-SUBFILE-EQ
                 PERFORM T0002-RWR-SFL
              END-IF
              MOVE -1 TO GX-ACT-SFL-REC
              PERFORM TEST AFTER UNTIL NO-ERRORS-FOUND
                 SET NO-ERRORS-FOUND TO TRUE
                 MOVE 'Selection List TGCTD' TO GXV-V11PGMDESC
                 MOVE 0 TO GXV-V9GX-ERR
                 PERFORM CLR-SCREEN
                 PERFORM T0002-MAINSCR
                 MOVE 'B0100C' TO GX-WS-FMT
                 PERFORM READ-SCREEN
                 MOVE ZEROES TO GX-CSRROW
                 MOVE ZEROES TO GX-CSRCOL
                 MOVE 6 TO GX-SFL-UR
                 MOVE 9 TO GX-SFL-LR
                 MOVE EXC-SFL-RCD-CNT-0002 TO GX-SFL-RCDCNT
                 PERFORM GX-SFL-ACTRCD
                 MOVE 'B0101' TO GX-WS-FMT
                 PERFORM READ-SCREEN
                 MOVE V5CHRID OF B0101-I TO GXV-V5CHRID
                 MOVE V6CCTDID OF B0101-I TO GXV-V6CCTDID
                 MOVE WS-ROW-POS TO GX-S-ROW
                 MOVE WS-COL-POS TO GX-S-COL
                 MOVE ZEROES     TO GX-CSRSET
              END-PERFORM
      * Read active subfile record
              IF EXC-SFL-RCD-CNT-0002 GREATER 0 AND NOT WS-KEY-PAGE-DOWN
                 MOVE GX-SFLRECNBR TO GX-SFLRELRECNBR
                 PERFORM T0002-RDE-SFL
                 MOVE GX-SFLRELRECNBR TO GX-ACT-SFL-REC
              END-IF
              EVALUATE TRUE
                 WHEN WS-KEY-ENTER
                    EVALUATE TRUE
                       WHEN GX-NOT-LOADED
                          SET GX-RESTART TO TRUE
                       WHEN GXV-V5CHRID EQUAL GXO-V5CHRID AND
                            GXV-V6CCTDID EQUAL GXO-V6CCTDID AND
                            GXV-V7PTGFCHVIG EQUAL GXO-V7PTGFCHVIG
                          PERFORM T0002-EVENT
                             THRU T0002-EVENT-EXIT
                          CONTINUE
                    END-EVALUATE
                 WHEN WS-KEY-F3
                    MOVE 1 TO GX-RETURN
                 WHEN WS-KEY-F12
                    MOVE 1 TO GX-EXIT-LEVEL
                 WHEN WS-KEY-F24
                    COMPUTE GX-TMPINT = FK-L0002-N / FK-L0002-C
                    COMPUTE FK-L0002-N = FK-L0002-N - (GX-TMPINT *
            FK-L0002-C ) + 1
                 WHEN WS-KEY-F10
                    PERFORM T0002-MENUBAR
                 WHEN WS-KEY-PAGE-DOWN
                    PERFORM T0002-LOAD-SFL
                 WHEN WS-KEY-F5
                    SET GX-RESTART TO TRUE
                    SET GX-REPOSITION TO TRUE
                 WHEN OTHER
                    MOVE 'Tecla de función no válida en este momento.'
            TO MSG-DESCRIPTION
                    PERFORM GX-ADD-MSG
              END-EVALUATE
           END-PERFORM
           IF GX-LOADED
              PERFORM T0002-CLOSE
              PERFORM T0002-CLR-SFL
           END-IF
           CONTINUE.

       T0002-MAIN-EXIT.

           CONTINUE.

       T0002-MAINSCR.

           MOVE FK-L0002-A (FK-L0002-N) TO GXFKL OF GXFKR-O
           MOVE 'GXFKR' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           PERFORM GX-MSG-MANAGER
           PERFORM T0002-DSPWSCR
           CONTINUE.

       T0002-DSPWSCR.

           MOVE 'B0101' TO GX-WS-FMT
           MOVE GXV-V11PGMDESC TO V11PGMDESC OF B0101-O
           MOVE GXV-V5CHRID TO V5CHRID OF B0101-O
           MOVE GXV-V6CCTDID TO V6CCTDID OF B0101-O
           PERFORM WRITE-SCREEN
           IF EXC-SFL-RCD-CNT-0002 NOT GREATER 0 AND GX-LOADED
              MOVE 'EL0200' TO GX-WS-FMT
              PERFORM WRITE-SCREEN
           END-IF
           MOVE 'B0100C' TO GX-WS-FMT
           SET DISPLAY-SFLCTL TO TRUE
           IF EXC-SFL-RCD-CNT-0002 GREATER 0
              SET DISPLAY-SFL    TO TRUE
           ELSE
              SET NO-DISPLAY-SFL TO TRUE
           END-IF
           IF GX-LREC = 1
              SET SFL-END TO TRUE
           ELSE
              SET NO-SFL-END TO TRUE
           END-IF
           MOVE GX-SFLRECNBR TO SFLRCD OF B0100C-O
           COMPUTE CSRROW OF B0100C-O = GX-CSRROW - 0
           COMPUTE CSRCOL OF B0100C-O = GX-CSRCOL - 0
           PERFORM WRITE-SCREEN
           CONTINUE.

       T0002-INIT.

           MOVE 'Selection List TGCTD' TO GXV-V11PGMDESC
           MOVE 0 TO GXV-V9GX-ERR
           MOVE -1 TO GX-ACT-SFL-REC
           CONTINUE.

       T0002-INIT-EXIT.

           CONTINUE.

       T0002-RFRSHSCR.

           PERFORM T0002-INIT
              THRU T0002-INIT-EXIT
           MOVE GXV-V5CHRID TO GXO-V5CHRID
           MOVE GXV-V6CCTDID TO GXO-V6CCTDID
           MOVE GXV-V7PTGFCHVIG TO GXO-V7PTGFCHVIG
           PERFORM T0002-START
           PERFORM T0002-FETCH
           SET GX-NO-RESTART TO TRUE
           PERFORM T0002-LOAD-SFL
           CONTINUE.

       T0002-RFRSHSCR-EXIT.

           CONTINUE.

       T0002-MENUBAR.

           MOVE 3 TO MB0002-C
           MOVE 1 TO MB0002-N
           PERFORM TEST AFTER
              UNTIL GX-MB-EXIT = 1 OR GX-RETURN = 1 OR GX-EXIT-LEVEL = 1
              MOVE 0 TO GX-MB-EXIT
              MOVE SPACES TO GXV-GXPDOP
              PERFORM T0002-MENUBARSCR
              MOVE 'MB0300' TO GX-WS-FMT
              PERFORM READ-SCREEN
              MOVE ZEROES TO GX-CSRROW
              MOVE ZEROES TO GX-CSRCOL
              EVALUATE TRUE
                 WHEN WS-KEY-ENTER
                    EVALUATE TRUE
                       WHEN WS-ROW-POS = 001 AND
                            WS-COL-POS >= 003 AND
                            WS-COL-POS <= 010
                          PERFORM T0002-PULLDN-0001
                            UNTIL GX-MB-EXIT = 1 OR
                                  GX-RETURN = 1 OR
                                  GX-EXIT-LEVEL = 1
                       WHEN OTHER
                          MOVE 'Posición del cursor no válida.' TO
            MSG-DESCRIPTION
                          PERFORM GX-ADD-MSG
                    END-EVALUATE
                    MOVE 0 TO GX-EXIT-LEVEL
                 WHEN WS-KEY-F12
                    MOVE 1 TO GX-EXIT-LEVEL
                 WHEN WS-KEY-F10
                    MOVE 1 TO GX-MB-EXIT
                 WHEN WS-KEY-F3
                    MOVE 1 TO GX-RETURN
                 WHEN OTHER
                    MOVE 'Tecla de función no válida en este momento.'
            TO MSG-DESCRIPTION
                    PERFORM GX-ADD-MSG
              END-EVALUATE
           END-PERFORM
           MOVE 0 TO GX-EXIT-LEVEL
           CONTINUE.

       T0002-MENUBARSCR.

           MOVE MB0002-A (MB0002-N) TO GXFKL OF GXFKR-O
           MOVE 'GXFKR' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           PERFORM GX-MSG-MANAGER
           MOVE 'MB0300' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           CONTINUE.

       T0002-PULLDN-0001.

           MOVE MB0002-A (MB0002-N) TO GXFKL OF GXFKR-O
           MOVE 'GXFKR' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           PERFORM GX-MSG-MANAGER
           MOVE 'W001' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           MOVE 'PD0400' TO GX-WS-FMT
           MOVE GXV-GXPDOP TO GXPDOP OF PD0400-O
           PERFORM WRITE-SCREEN
           MOVE 'PD0400' TO GX-WS-FMT
           PERFORM READ-SCREEN
           MOVE ZEROES TO GX-CSRROW
           MOVE ZEROES TO GX-CSRCOL
           MOVE GXPDOP OF PD0400-I TO GXV-GXPDOP
           EVALUATE TRUE
              WHEN WS-KEY-ENTER
                 MOVE 1 TO GX-MB-EXIT
                 EVALUATE TRUE
                    WHEN GXV-GXPDOP = '1'
                       CONTINUE
                       SET GX-RESTART TO TRUE
                       SET GX-REPOSITION TO TRUE
                    WHEN GXV-GXPDOP = SPACES
                       CONTINUE
                    WHEN OTHER
                       MOVE 0 TO GX-MB-EXIT
                       MOVE 'La opción no está permitida.' TO
            MSG-DESCRIPTION
                       PERFORM GX-ADD-MSG
                 END-EVALUATE
              WHEN WS-KEY-F12
                 MOVE 1 TO GX-EXIT-LEVEL
              WHEN WS-KEY-F10
                 MOVE 1 TO GX-MB-EXIT
              WHEN WS-KEY-F3
                 MOVE 1 TO GX-RETURN
              WHEN OTHER
                 MOVE 'Tecla de función no válida en este momento.' TO
            MSG-DESCRIPTION
                 PERFORM GX-ADD-MSG
           END-EVALUATE
           CONTINUE.

       T0002-GROUP.

           MOVE 0 TO W-FETCH-DONE-ON-0419
           MOVE TGFCHVIG OF F0-TGCTD-RCD TO GXV-TGFCHVIG
           MOVE CTDID OF F0-TGCTD-RCD TO GXV-CTDID
           MOVE HRID OF F0-TGCTD-RCD TO GXV-HRID
           IF GXV-HRID NOT LESS GXV-V5CHRID
              IF GXV-V6CCTDID = ZEROES OR ( GXV-CTDID NOT LESS
            GXV-V6CCTDID )
                 SET GX-MATCHING TO TRUE
                 PERFORM T0003-EVENT
                    THRU T0003-EVENT-EXIT
                 PERFORM T0002-GROUP-UPDATE
                 CONTINUE
              END-IF
              CONTINUE
           END-IF
           PERFORM T0002-FETCH
           CONTINUE.

       T0002-GROUP-EXIT.

           CONTINUE.

       T0002-GROUP-UPDATE.

           CONTINUE.

       T0002-START.

           MOVE GXV-V7PTGFCHVIG TO TGFCHVIG OF F0-TGCTD-RCD
           MOVE GXV-V5CHRID TO HRID OF F0-TGCTD-RCD
           PERFORM TEST AFTER UNTIL GX-IO-CODE NOT LESS ZERO
              START F0-TGCTD KEY IS >=
                                  TGFCHVIG OF F0-TGCTD-RCD
                                  HRID OF F0-TGCTD-RCD
                 INVALID KEY
                    CONTINUE
              END-START
              PERFORM GX-FILE-STATUS-ANALYSIS
              PERFORM GX-IOERR-LOOP
           END-PERFORM

           CONTINUE.

       T0002-FETCH.

           MOVE 'TGFchVig, Id. Cmb Horaria Red Celular' TO GX-IO-XFI
           MOVE 'reading' TO GX-IO-OP
           READ F0-TGCTD NEXT
              AT END
                CONTINUE
           END-READ
           PERFORM GX-FILE-STATUS-ANALYSIS
           PERFORM GX-IOERR
           MOVE 0 TO F0-TGCTD-EOF
           IF NOT GX-IO-OK
              MOVE ZEROES TO TGFCHVIG OF F0-TGCTD-RCD
              MOVE ZEROES TO CTDID OF F0-TGCTD-RCD
              MOVE ZEROES TO HRID OF F0-TGCTD-RCD
              MOVE 1 TO F0-TGCTD-EOF
           END-IF
           MOVE 1 TO W-FETCH-DONE-ON-0419
           CONTINUE.

       T0002-CLOSE.

           CONTINUE.

       T0001-EVENT.

      * Event Start
      * Assignment to property Caption ignored.
           CONTINUE.

       T0001-EVENT-EXIT.

           CONTINUE.

       T0002-EVENT.

      * Event Enter
           MOVE GXV-HRID TO GXV-V8PHRID
           MOVE 1 TO GX-RETURN
           GO TO T0002-EVENT-EXIT
           CONTINUE.

       T0002-EVENT-EXIT.

           CONTINUE.

       T0003-EVENT.

      * Event Load
           PERFORM T0002-WRT-SFL
           CONTINUE.

       T0003-EVENT-EXIT.

           CONTINUE.

       T0002-CLR-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE ZEROES TO EXC-SFL-RCD-CNT-0002
                 MOVE 'B0100C' TO GX-WS-FMT
                 SET NO-DISPLAY-SFLCTL TO TRUE
                 SET NO-DISPLAY-SFL    TO TRUE
                 MOVE 1 TO SFLRCD OF B0100C-O
                 PERFORM WRITE-SCREEN
           END-EVALUATE
           CONTINUE.

       T0002-WRT-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE 'B0100' TO GX-WS-FMT
                 MOVE GXV-HRID TO A3297 OF B0100-O
                 MOVE GXV-CTDID TO A2124 OF B0100-O
                 ADD 1 TO EXC-SFL-RCD-CNT-0002
                 MOVE EXC-SFL-RCD-CNT-0002 TO GX-SFLRELRECNBR
                 PERFORM WRITE-SUBFILE
                 MOVE ZEROES TO GX-ATT-IND
           END-EVALUATE
           ADD 1 TO GX-SFL-PAGE-COUNT
           IF GX-SFL-PAGE-COUNT = 1
              MOVE GX-SFLRELRECNBR TO GX-SFLRECNBR
           END-IF
           CONTINUE.

       T0002-LOAD-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 CONTINUE
           END-EVALUATE
           SET MODIFIED-MARK-OFF TO TRUE
           MOVE ZEROES TO GX-SFL-PAGE-COUNT
           MOVE 0 TO L002-EXIT
           MOVE 1 TO GX-LVL0002
           PERFORM T0002-GROUP
              THRU T0002-GROUP-EXIT
             UNTIL NOT ( F0-TGCTD-EOF = 0 AND
                         GX-SFL-PAGE-COUNT < 4 AND
                         TGFCHVIG OF F0-TGCTD-RCD = GXV-V7PTGFCHVIG )
           MOVE 0 TO GX-LVL0002
           IF NOT ( F0-TGCTD-EOF = 0 AND
                    TGFCHVIG OF F0-TGCTD-RCD = GXV-V7PTGFCHVIG )
           MOVE 1 TO F0-TGCTD-EOF
           END-IF
           MOVE F0-TGCTD-EOF TO GX-LREC
           CONTINUE.

       T0002-RDM-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE 'B0100' TO GX-WS-FMT
                 SET MORE-SFL-REC TO TRUE
                 PERFORM READ-SUBFILE-NEXT-MODIFIED
                 IF MORE-SFL-REC
                    MOVE A3297 OF B0100-I TO GXV-HRID
                    MOVE A2124 OF B0100-I TO GXV-CTDID
                    CONTINUE
                 END-IF
           END-EVALUATE
           CONTINUE.

       T0002-MVM-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE A3297 OF B0100-I TO GXV-HRID
                 MOVE A2124 OF B0100-I TO GXV-CTDID
           END-EVALUATE
           CONTINUE.

       T0002-RDE-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE 'B0100' TO GX-WS-FMT
                 PERFORM READ-SUBFILE-EQ
                 MOVE A3297 OF B0100-I TO GXV-HRID
                 MOVE A2124 OF B0100-I TO GXV-CTDID
           END-EVALUATE
           CONTINUE.

       T0002-RWR-SFL.

           EVALUATE TRUE
              WHEN DISPLAY-MODE
                 MOVE 'B0100' TO GX-WS-FMT
                 MOVE GXV-HRID TO A3297 OF B0100-O
                 MOVE GXV-CTDID TO A2124 OF B0100-O
                 PERFORM REWRITE-SUBFILE
           END-EVALUATE
           CONTINUE.

       T0002-ADDNEWRCD.

           SET INSRCD TO TRUE
           PERFORM T0002-WRT-SFL
           MOVE GX-SFLRELRECNBR TO GX-SFLRECNBR
           CONTINUE.

      * --- Standard variables for scr program
      * --- Referenced routines
       REWRITE-SUBFILE.

           MOVE SUBFILE-FLAGS      TO SUBFILE-INDICATORS
           MOVE MISCELANEOUS-FLAGS TO MISCELANEOUS-INDICATORS
           REWRITE SUBFILE GX-CRT-RECORD
                   FORMAT IS GX-WS-FMT
                   INDICATORS ARE GX-INDICATORS
                   INVALID KEY
                      CONTINUE
           END-REWRITE
           MOVE ZEROES TO GX-ATT-IND.

       READ-SUBFILE-NEXT-MODIFIED.

           READ SUBFILE GX-CRT-FILE
                NEXT MODIFIED
                FORMAT IS GX-WS-FMT
                AT END
                   SET NO-MORE-SFL-REC TO TRUE
           END-READ
           IF GX-WS-STATUS NOT EQUAL '00'
              SET NO-MORE-SFL-REC TO TRUE
           END-IF
           MOVE ZEROES TO GX-ATT-IND.
       WRITE-SUBFILE.

           MOVE SUBFILE-FLAGS      TO SUBFILE-INDICATORS
           MOVE MISCELANEOUS-FLAGS TO MISCELANEOUS-INDICATORS
           WRITE SUBFILE GX-CRT-RECORD
                 FORMAT IS GX-WS-FMT
                 INDICATORS ARE GX-INDICATORS
                 INVALID KEY
                    CONTINUE
           END-WRITE.
       GX-IOERR.

           MOVE SPACES TO MSG-DESCRIPTION
           EVALUATE TRUE
              WHEN GX-IO-OK OR GX-IO-EOF
                 CONTINUE
              WHEN GX-IO-LOCKED-FILE OR
                   GX-IO-LOCKED-RCD
                 STRING 'Otro usuario traba los datos de' GX-IO-XFI
                     DELIMITED BY SIZE INTO MSG-DESCRIPTION
                 PERFORM SEND-ERROR-MSG-WS
              WHEN OTHER
                 STRING 'Error (' V-FS ') ' GX-IO-OP ' ' GX-IO-XFI
                    DELIMITED BY SIZE INTO MSG-DESCRIPTION
                 PERFORM SEND-ERROR-MSG-WS
           END-EVALUATE.
       GX-SFL-ACTRCD.
           IF WS-FIRST-SFL-REC = 0
              MOVE GX-SFL-RCDCNT TO WS-FIRST-SFL-REC
           END-IF
           MOVE 3 TO GXV-V9GX-ERR
           MOVE 'Posición del cursor no válida.' TO GXV-V10GX-EMSG
           EVALUATE TRUE
           WHEN WS-ROW-POS < GX-SFL-UR
              MOVE WS-FIRST-SFL-REC TO GX-SFLRECNBR
           WHEN WS-ROW-POS > GX-SFL-LR OR
                WS-FIRST-SFL-REC + WS-ROW-POS - GX-SFL-UR >
            GX-SFL-RCDCNT
              MOVE GX-SFL-RCDCNT    TO GX-SFLRECNBR
           WHEN OTHER
              COMPUTE GX-SFLRECNBR = WS-FIRST-SFL-REC +
                                     WS-ROW-POS -
                                     GX-SFL-UR
              MOVE 0 TO GXV-V9GX-ERR
              MOVE SPACES TO GXV-V10GX-EMSG
           END-EVALUATE
           IF GX-SFLRECNBR = 0
              MOVE 1 TO GX-SFLRECNBR
           END-IF.
       READ-SUBFILE-EQ.

           READ SUBFILE GX-CRT-FILE
                FORMAT IS GX-WS-FMT
                INVALID KEY
                   CONTINUE
           END-READ
           MOVE ZEROES TO GX-ATT-IND.
       GX-WAITMSG.

           MOVE "Oprima Intro para continuar ..." TO GXFKL OF GXFKR-O
           MOVE 'GXFKR' TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           MOVE WS-AID-BYTE TO WS-AID-BYTE-SAVED
           PERFORM READ-SCREEN
               TEST AFTER
               UNTIL WS-KEY-ENTER
           MOVE WS-AID-BYTE-SAVED TO WS-AID-BYTE
           CONTINUE.
       GX-ABORT.

           MOVE 'S'       TO GX-MSGTYPE
           CALL 'GXMSGS'  USING GX-MSGID
                                GX-MSGDTA
                                GX-MSGTYPE
           STOP RUN.
       GX-IOERR-LOOP.

           EVALUATE TRUE
              WHEN GX-IO-OK OR GX-IO-EOF
                 CONTINUE
              WHEN GX-IO-LOCKED-FILE OR
                   GX-IO-LOCKED-RCD
                 ADD 1 TO GX-LOCK-COUNT
                 IF GX-LOCK-COUNT >= 10
                    MOVE SPACES TO GX-RETRY-MSG
                    STRING 'Otro usuario traba los datos de' GX-IO-XFI
                        DELIMITED BY SIZE INTO GX-RETRY-MSG
                    CALL 'GXAR' USING GX-RETRY-MSG GX-RETRY-ANS
                    IF GX-RETRY-ANS = 'R'
                       MOVE 0 TO GX-LOCK-COUNT
                    ELSE
                       MOVE 'GXM9999' TO GX-MSGID
                       MOVE 'Proceso cancelado por el usuario.' TO
            GX-MSGDTA
                       GO TO GX-ABORT
                    END-IF
                 END-IF
              WHEN OTHER
                 MOVE 'GXM9999' TO GX-MSGID
                 MOVE SPACES    TO GX-MSGDTA
                 STRING 'Error (' V-FS ') ' GX-IO-OP ' ' GX-IO-XFI
                     DELIMITED BY SIZE INTO GX-MSGDTA
                 GO TO GX-ABORT
           END-EVALUATE.
       CLR-SCREEN.
           MOVE 'CLRSCR'           TO GX-WS-FMT
           PERFORM WRITE-SCREEN.
       WRITE-SCREEN.
           IF GX-ALARM NOT EQUAL 0
              SET ALARM-ON TO TRUE
           ELSE
              SET ALARM-OFF TO TRUE
           END-IF
           MOVE SUBFILE-FLAGS      TO SUBFILE-INDICATORS
           MOVE MISCELANEOUS-FLAGS TO MISCELANEOUS-INDICATORS
           WRITE GX-CRT-RECORD
                 FORMAT IS GX-WS-FMT
                 INDICATORS ARE GX-INDICATORS.
       READ-SCREEN.
           SET MORE-SFL-REC TO TRUE
           READ GX-CRT-FILE
               FORMAT IS GX-WS-FMT
               INDICATORS ARE GX-INDICATORS
           ACCEPT WS-FEEDBACK-AREA FROM DISPLAY-FEEDBACK
           MOVE WS-ROW TO WS-ROW-BYTE-HEX
           MOVE WS-COL TO WS-COL-BYTE-HEX
           MOVE WS-AID TO WS-AID-BYTE-HEX
           MOVE ZEROES TO GX-ALARM
           MOVE ZEROES TO GX-ATT-IND.
       GX-MSG-MANAGER.
           IF MSG-COUNTER NOT GREATER ZERO
              MOVE SPACES TO MSG-DESCRIPTION
              PERFORM GX-ADD-MSG
           END-IF

           SET  DISPLAY-SFLCTL     TO TRUE
           SET  NO-SFL-END         TO TRUE
           SET  NO-DISPLAY-SFL     TO TRUE
           MOVE 'INFLIN'           TO GX-WS-FMT
           PERFORM WRITE-SCREEN

           MOVE MSG-COUNTER TO MSG-COUNTER-LOOP
           PERFORM UNTIL MSG-COUNTER-LOOP EQUAL ZERO
              MOVE MSG-COUNTER-LOOP TO GX-SFLRELRECNBR
              MOVE MSG-TEXT (MSG-COUNTER-LOOP) TO MSGTXT OF MSGREC-O
              IF ERROR-MSG (MSG-COUNTER-LOOP)
                 SET RECORD-HAS-ERRORS TO TRUE
              ELSE
                 SET RECORD-HAS-NO-ERRORS TO TRUE
              END-IF
              MOVE SUBFILE-FLAGS      TO SUBFILE-INDICATORS
              MOVE MISCELANEOUS-FLAGS TO MISCELANEOUS-INDICATORS
              WRITE SUBFILE GX-CRT-RECORD
                    FORMAT IS 'MSGREC'
                    INDICATORS ARE GX-INDICATORS
                    INVALID KEY CONTINUE
              END-WRITE
              SUBTRACT 1 FROM MSG-COUNTER-LOOP
           END-PERFORM

           SET  DISPLAY-SFLCTL     TO TRUE
           SET  DISPLAY-SFL        TO TRUE
           SET  SFL-END            TO TRUE
           MOVE 'INFLIN'           TO GX-WS-FMT
           PERFORM WRITE-SCREEN
           MOVE ZERO               TO MSG-COUNTER.
       GX-ADD-MSG.
           ADD 1 TO MSG-COUNTER
           IF MSG-COUNTER GREATER 100
              MOVE 100 TO MSG-COUNTER
           END-IF
           MOVE MSG-DESCRIPTION TO MSG-TEXT (MSG-COUNTER)
           SET WARNING-MSG (MSG-COUNTER) TO TRUE.
       GX-ADD-ERR-MSG.
           PERFORM GX-ADD-MSG
           SET ERROR-MSG (MSG-COUNTER) TO TRUE.
       SEND-ERROR-MSG-WS.
           PERFORM GX-ADD-ERR-MSG
           SET RECORD-HAS-ERRORS TO TRUE
           IF NO-ERRORS-FOUND
              MOVE GX-SFLRELRECNBR TO GX-SFLRECNBR
              SET ERRORS-FOUND TO TRUE
           END-IF.
      * --- End referenced routines
       GX-FILE-STATUS-ANALYSIS.

           EVALUATE TRUE
              WHEN V-FS = '00' OR
                   V-FS = '41' OR
                   V-FS = '42' OR
                   V-FS = '43' OR
                   V-FS = '94' OR
                   V-FS = '95'
                 SET GX-IO-OK           TO TRUE

              WHEN V-FS = '10' OR
                   V-FS = '23' OR
                   V-FS = '46'
                 SET GX-IO-EOF          TO TRUE

              WHEN V-FS = '92' OR
                   V-FS = '90'
                 SET GX-IO-LOCKED-FILE  TO TRUE

              WHEN V-FS = '9D'
                 SET GX-IO-LOCKED-RCD   TO TRUE

              WHEN V-FS = '22'
                 SET GX-IO-DUP-KEY      TO TRUE

              WHEN V-FS = '43' OR
                   V-FS = '94' OR
                   V-FS = '9S'
                 SET GX-IO-MISSING-READ TO TRUE

              WHEN V-FS = '04'
                 SET GX-IO-BAD-RCD-LEN  TO TRUE

              WHEN OTHER
                 SET GX-IO-OTHER-ERRORS TO TRUE
           END-EVALUATE
           CONTINUE.
      * WARNING - Message text for 'menu' lang. 'spa' not found
