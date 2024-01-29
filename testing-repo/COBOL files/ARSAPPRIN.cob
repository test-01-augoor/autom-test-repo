       PROCESS
               APOST.
       IDENTIFICATION DIVISION.

       PROGRAM-ID.               RSAPPRIN.
      * Description: Sap Print Block
       AUTHOR.                   GENEXUS 18_0_0-165820
                                 FOR IBM COBOL/400.
       DATE-WRITTEN.             4 de Noviembre de 2022.


       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.          IBM-AS400.
       OBJECT-COMPUTER.          IBM-AS400.
       SPECIAL-NAMES.            I-O-FEEDBACK IS DISPLAY-FEEDBACK
                                 OPEN-FEEDBACK IS GX-OPEN-FEEDBACK
                                 DECIMAL-POINT IS COMMA
                                 .

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GX-PRINTER
                  ASSIGN TO PRINTER-DESCARTE .
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       FD  GX-PRINTER
           LABEL RECORDS ARE STANDARD.
       01  GX-PRN-RECORD    PIC X(96).
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
      * --- Standard variables for proc program
       01  W-PRN-FEEDBACK.
           10 FILLER                PIC X(02).
           10 FILLER                PIC X(10).
           10 FILLER                PIC X(59).
           10 W-PRN-LPP             PIC 9(02)   COMP-4.
           10 FILLER                PIC 9(02)   COMP-4.
           10 FILLER                PIC X(32).
           10 W-PRN-OVERFLOW        PIC 9(02)   COMP-4.
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
       01  W-LINE-TO-PRINT       PIC  X(300)        VALUE SPACES.
       01  W-LINES-TO-ADVANCE    PIC S9(003) COMP-3 VALUE 0.
       01  RPT-PAGE-LENGTH       PIC S9(003) COMP-3 VALUE 0.
       01  RPT-TOP-MARGIN        PIC S9(003) COMP-3 VALUE 0.
       01  RPT-BOTTOM-MARGIN     PIC S9(003) COMP-3 VALUE 0.
       01  RPT-PRNCMD-STRUC.
           05  RPT-PRNCMD-LINE   PIC  X(300)        VALUE SPACES.
           05  RPT-PRNCMD-PTR    PIC S9(003) COMP-3 VALUE 1.
           05  RPT-PRNCMD-INT    PIC  9(003) BINARY VALUE 0.
           05  FILLER REDEFINES RPT-PRNCMD-INT.
               10 FILLER         PIC  X(001).
               10 RPT-PRNCMD-CHR PIC  X(001).
       01  RPT-LINES-TO-ADVANCE  PIC S9(003) COMP-3 VALUE ZERO.
       01  RPT-W-LINE-COUNTER    PIC S9(003) COMP-3 VALUE 1.
       01  FILLER                PIC S9(001) COMP-3 VALUE 1.
           88 RPT-NO-RCDS-FOUND                     VALUE 1.
           88 RPT-RCDS-FOUND                        VALUE 0.
       01  FILLER                PIC S9(001) COMP-3 VALUE 1.
           88 RPT-HEADER-ON                         VALUE 0.
           88 RPT-HEADER-OFF                        VALUE 1.
       01  FILLER                PIC S9(001) COMP-3 VALUE 1.
           88 RPT-FIRST-PAGE                        VALUE 0.
           88 RPT-NOT-FIRST-PAGE                    VALUE 1.
       01  RPT-NO-RCDS-FOUND-MSG PIC  X(080)        VALUE
                                 'No hay datos que satisfagan el informe
      -    '.'.
      * Lines to be printed.
       01  LN0200 .
           05 C000 PIC ZZZZZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C009 PIC ZZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C015 PIC ZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C020 PIC Z9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C023 PIC ZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C028 PIC ZZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C034 PIC ZZZZ9 .
       01  LN0201 .
           05 C000 PIC ZZZZZZZZZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C013 PIC ZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C018 PIC ZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C023 PIC ZZZ9 .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C028 PIC X(008) .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C037 PIC X(010) .
           05 FILLER PIC X(002) VALUE SPACES.
           05 C049 PIC X(002) .
           05 FILLER PIC X(001) VALUE SPACES.
           05 C052 PIC ZZZZZZZZZZZ9 .
       01  LN0202 .
           05 C000 PIC X(070) .
      * End of lines.
      * --- Variables for referenced routines.
      * --- User and temporary variables.
       01  GX-ERRPGM                      PIC X(10) VALUE SPACES.
       01  GX-MSGDTA                      PIC X(256) VALUE SPACES.
       01  GX-MSGID                       PIC X(7) VALUE SPACES.
       01  GX-MSGTYPE                     PIC X(1) VALUE SPACES.
       01  GXV-V31ARCHIVO                 PIC X(8) VALUE SPACES.
       01  GXV-V32DIRMAIL                 PIC X(253) VALUE SPACES.
       01  GXV-V33NOMARCH                 PIC X(10) VALUE SPACES.
       01  GXV-V34TEXTO                   PIC X(254) VALUE SPACES.
       01  GXV-V35GX-ERR                  PIC S9(3) VALUE ZEROES.
       01  GXV-V36GX-EMSG                 PIC X(70) VALUE SPACES.
       01  GXV-V37GX-LINE                 PIC 9(6) VALUE ZEROES.
       01  GXV-V38GX-PAGE                 PIC 9(6) VALUE ZEROES.
       01  GXV-V39GX-OUT                  PIC X(3) VALUE SPACES.
       01  GXV-V30PMSGERR                 PIC X(70) VALUE SPACES.
       01  GXV-V16PFLGADD                 PIC X(1) VALUE SPACES.
       01  GXV-V29PWCTNROREL              PIC 9(12) VALUE ZEROES.
       01  GXV-V17PLCIDDEST               PIC 9(5) VALUE ZEROES.
       01  GXV-V18PLCIDSTK                PIC 9(5) VALUE ZEROES.
       01  GXV-V25PSMOID                  PIC 9(4) VALUE ZEROES.
       01  GXV-V24PSFCID                  PIC 9(2) VALUE ZEROES.
       01  GXV-V15PCOLID                  PIC 9(4) VALUE ZEROES.
       01  GXV-V19PMKTID                  PIC 9(5) VALUE ZEROES.
       01  GXV-V23PSAID                   PIC 9(8) VALUE ZEROES.
       01  GXV-V28PUSRID                  PIC X(10) VALUE SPACES.
       01  GXV-V21PPGNAME                 PIC X(8) VALUE SPACES.
       01  GXV-V22PPROCID                 PIC 9(4) VALUE ZEROES.
       01  GXV-V20PORDTAREA               PIC 9(4) VALUE ZEROES.
       01  GXV-V26PTAREA                  PIC 9(4) VALUE ZEROES.
       01  GXV-V27PTRAMITE                PIC 9(12) VALUE ZEROES.
      * --- String constants.
       LINKAGE SECTION.
       01  GXL-V27PTRAMITE                PIC 9(12).
       01  GXL-V26PTAREA                  PIC 9(4).
       01  GXL-V20PORDTAREA               PIC 9(4).
       01  GXL-V22PPROCID                 PIC 9(4).
       01  GXL-V21PPGNAME                 PIC X(8).
       01  GXL-V28PUSRID                  PIC X(10).
       01  GXL-V23PSAID                   PIC 9(8).
       01  GXL-V19PMKTID                  PIC 9(5).
       01  GXL-V15PCOLID                  PIC 9(4).
       01  GXL-V24PSFCID                  PIC 9(2).
       01  GXL-V25PSMOID                  PIC 9(4).
       01  GXL-V18PLCIDSTK                PIC 9(5).
       01  GXL-V17PLCIDDEST               PIC 9(5).
       01  GXL-V29PWCTNROREL              PIC 9(12).
       01  GXL-V16PFLGADD                 PIC X(1).
       01  GXL-V30PMSGERR                 PIC X(70).
       PROCEDURE DIVISION USING
                                GXL-V27PTRAMITE
                                GXL-V26PTAREA
                                GXL-V20PORDTAREA
                                GXL-V22PPROCID
                                GXL-V21PPGNAME
                                GXL-V28PUSRID
                                GXL-V23PSAID
                                GXL-V19PMKTID
                                GXL-V15PCOLID
                                GXL-V24PSFCID
                                GXL-V25PSMOID
                                GXL-V18PLCIDSTK
                                GXL-V17PLCIDDEST
                                GXL-V29PWCTNROREL
                                GXL-V16PFLGADD
                                GXL-V30PMSGERR
                                .
       MAIN.

           MOVE GXL-V27PTRAMITE TO GXV-V27PTRAMITE
           MOVE GXL-V26PTAREA TO GXV-V26PTAREA
           MOVE GXL-V20PORDTAREA TO GXV-V20PORDTAREA
           MOVE GXL-V22PPROCID TO GXV-V22PPROCID
           MOVE GXL-V21PPGNAME TO GXV-V21PPGNAME
           MOVE GXL-V28PUSRID TO GXV-V28PUSRID
           MOVE GXL-V23PSAID TO GXV-V23PSAID
           MOVE GXL-V19PMKTID TO GXV-V19PMKTID
           MOVE GXL-V15PCOLID TO GXV-V15PCOLID
           MOVE GXL-V24PSFCID TO GXV-V24PSFCID
           MOVE GXL-V25PSMOID TO GXV-V25PSMOID
           MOVE GXL-V18PLCIDSTK TO GXV-V18PLCIDSTK
           MOVE GXL-V17PLCIDDEST TO GXV-V17PLCIDDEST
           MOVE GXL-V29PWCTNROREL TO GXV-V29PWCTNROREL
           MOVE GXL-V16PFLGADD TO GXV-V16PFLGADD
           MOVE GXL-V30PMSGERR TO GXV-V30PMSGERR
      * --- Variables initialization
           MOVE 'PRN' TO GXV-V39GX-OUT
           MOVE ZEROES TO GXV-V38GX-PAGE
           MOVE ZEROES TO GXV-V37GX-LINE
           MOVE SPACES TO GXV-V36GX-EMSG
           MOVE ZEROES TO GXV-V35GX-ERR
           MOVE SPACES TO GXV-V34TEXTO
           MOVE SPACES TO GXV-V33NOMARCH
           MOVE SPACES TO GXV-V32DIRMAIL
           MOVE SPACES TO GXV-V31ARCHIVO
           MOVE 0 TO GX-COMMIT
           MOVE 0 TO GX-ROLLBACK
           MOVE 0 TO GX-EXIT-LEVEL
           MOVE 0 TO GX-RETURN
      * --- End of initialization
           MOVE 72 TO RPT-PAGE-LENGTH
           MOVE 1 TO RPT-TOP-MARGIN
           MOVE 4 TO RPT-BOTTOM-MARGIN
           PERFORM RPT-INITIALIZATION
           PERFORM RPT-MAIN
           CONTINUE.

       END-OF-MAIN.

           MOVE GXV-V27PTRAMITE TO GXL-V27PTRAMITE
           MOVE GXV-V26PTAREA TO GXL-V26PTAREA
           MOVE GXV-V20PORDTAREA TO GXL-V20PORDTAREA
           MOVE GXV-V22PPROCID TO GXL-V22PPROCID
           MOVE GXV-V21PPGNAME TO GXL-V21PPGNAME
           MOVE GXV-V28PUSRID TO GXL-V28PUSRID
           MOVE GXV-V23PSAID TO GXL-V23PSAID
           MOVE GXV-V19PMKTID TO GXL-V19PMKTID
           MOVE GXV-V15PCOLID TO GXL-V15PCOLID
           MOVE GXV-V24PSFCID TO GXL-V24PSFCID
           MOVE GXV-V25PSMOID TO GXL-V25PSMOID
           MOVE GXV-V18PLCIDSTK TO GXL-V18PLCIDSTK
           MOVE GXV-V17PLCIDDEST TO GXL-V17PLCIDDEST
           MOVE GXV-V29PWCTNROREL TO GXL-V29PWCTNROREL
           MOVE GXV-V16PFLGADD TO GXL-V16PFLGADD
           MOVE GXV-V30PMSGERR TO GXL-V30PMSGERR
           MOVE '0' TO GX-FIRST-TIME
           GOBACK
           CONTINUE.

       RPT-DISPLAY-OUTPUT.

           IF RPT-NO-RCDS-FOUND
              MOVE RPT-NO-RCDS-FOUND-MSG TO W-LINE-TO-PRINT
              COMPUTE RPT-LINES-TO-ADVANCE = 1
              PERFORM RPT-PRINT-LINE

              IF RPT-HEADER-OFF
                 MOVE SPACES TO W-LINE-TO-PRINT
                 COMPUTE RPT-LINES-TO-ADVANCE = RPT-PAGE-LENGTH -
            RPT-BOTTOM-MARGIN - GXV-V37GX-LINE + 1
                 PERFORM RPT-PRINT-LINE

                 CONTINUE
              END-IF
           ELSE
              IF RPT-HEADER-OFF
                 MOVE SPACES TO W-LINE-TO-PRINT
                 COMPUTE RPT-LINES-TO-ADVANCE = RPT-PAGE-LENGTH -
            RPT-BOTTOM-MARGIN - GXV-V37GX-LINE + 1
                 PERFORM RPT-PRINT-LINE

                 CONTINUE
              END-IF
              PERFORM RPT-FOOTER
              CONTINUE
           END-IF

           CLOSE GX-PRINTER
           IF GXV-V39GX-OUT NOT EQUAL 'PRN'
              MOVE 1 TO GX-QCMDEXC-PTR
              STRING 'DSPSPLF FILE(DESCARTE) '
                             'SPLNBR(*LAST)'
                     DELIMITED BY SIZE
                     INTO GX-QCMDEXC-CMD
                     POINTER GX-QCMDEXC-PTR
              COMPUTE GX-QCMDEXC-CMDLEN = GX-QCMDEXC-PTR - 1
              CALL 'QCMDEXC' USING GX-QCMDEXC-CMD
                                   GX-QCMDEXC-CMDLEN

              MOVE 1 TO GX-QCMDEXC-PTR
              STRING 'DLTSPLF FILE(DESCARTE) '
                             'SPLNBR(*LAST)'
                     DELIMITED BY SIZE
                     INTO GX-QCMDEXC-CMD
                     POINTER GX-QCMDEXC-PTR
              COMPUTE GX-QCMDEXC-CMDLEN = GX-QCMDEXC-PTR - 1
              CALL 'QCMDEXC' USING GX-QCMDEXC-CMD
                                   GX-QCMDEXC-CMDLEN
           END-IF

           CONTINUE.

       RPT-INITIALIZATION.

           MOVE 'PRN' TO GXV-V39GX-OUT
           MOVE ZEROES TO GXV-V38GX-PAGE
           CONTINUE.

       RPT-MAIN.

           MOVE 0 TO GXV-V37GX-LINE
           MOVE 0 TO GXV-V35GX-ERR
           MOVE 1 TO GX-QCMDEXC-PTR
           IF GXV-V39GX-OUT NOT EQUAL 'PRN'
              STRING 'OVRPRTF FILE(DESCARTE) '
                             'HOLD(*YES) '
                             'PAGESIZE(72 96) '
                             'OVRFLW(72) '
                             'SHARE(*NO)'
                     DELIMITED BY SIZE
                     INTO GX-QCMDEXC-CMD
                     POINTER GX-QCMDEXC-PTR
           ELSE
              STRING 'OVRPRTF FILE(DESCARTE) '
                             'HOLD(*NO) '
                             'PAGESIZE(72 96) '
                             'OVRFLW(72) '
                             'SHARE(*NO)'
                     DELIMITED BY SIZE
                     INTO GX-QCMDEXC-CMD
                     POINTER GX-QCMDEXC-PTR
           END-IF

           COMPUTE GX-QCMDEXC-CMDLEN = GX-QCMDEXC-PTR - 1
           CALL 'QCMDEXC' USING GX-QCMDEXC-CMD
                                GX-QCMDEXC-CMDLEN

           OPEN OUTPUT GX-PRINTER
           ACCEPT W-PRN-FEEDBACK FROM GX-OPEN-FEEDBACK
           MOVE W-PRN-LPP OF W-PRN-FEEDBACK TO RPT-PAGE-LENGTH

           SET  RPT-NO-RCDS-FOUND   TO TRUE
           SET  RPT-HEADER-ON      TO TRUE
           SET  RPT-FIRST-PAGE      TO TRUE
           MOVE ZEROES TO GXV-V38GX-PAGE
           PERFORM RPT-HEADER
           MOVE GXV-V23PSAID TO C000  OF  LN0200
           MOVE GXV-V19PMKTID TO C009  OF  LN0200
           MOVE GXV-V15PCOLID TO C015  OF  LN0200
           MOVE GXV-V24PSFCID TO C020  OF  LN0200
           MOVE GXV-V25PSMOID TO C023  OF  LN0200
           MOVE GXV-V18PLCIDSTK TO C028  OF  LN0200
           MOVE GXV-V17PLCIDDEST TO C034  OF  LN0200
           MOVE LN0200 TO W-LINE-TO-PRINT
           COMPUTE W-LINES-TO-ADVANCE = 1
           PERFORM RPT-CHECK-EOP

           PERFORM RPT-HEADER
           MOVE GXV-V27PTRAMITE TO C000  OF  LN0201
           MOVE GXV-V26PTAREA TO C013  OF  LN0201
           MOVE GXV-V20PORDTAREA TO C018  OF  LN0201
           MOVE GXV-V22PPROCID TO C023  OF  LN0201
           MOVE GXV-V21PPGNAME TO C028  OF  LN0201
           MOVE GXV-V28PUSRID TO C037  OF  LN0201
           MOVE GXV-V16PFLGADD TO C049  OF  LN0201
           MOVE GXV-V29PWCTNROREL TO C052  OF  LN0201
           MOVE LN0201 TO W-LINE-TO-PRINT
           COMPUTE W-LINES-TO-ADVANCE = 1
           PERFORM RPT-CHECK-EOP

           PERFORM RPT-HEADER
           MOVE GXV-V30PMSGERR TO C000  OF  LN0202
           MOVE LN0202 TO W-LINE-TO-PRINT
           COMPUTE W-LINES-TO-ADVANCE = 1
           PERFORM RPT-CHECK-EOP

           MOVE 'DESCARTE' TO GXV-V33NOMARCH
           MOVE 'srodriguezgonzalez@mail.Antel.com.uy' TO GXV-V32DIRMAIL
           MOVE 'registro SAP a revisar' TO GXV-V34TEXTO
           MOVE 'test' TO GXV-V31ARCHIVO
           CALL 'ACMMAIL' USING GXV-V33NOMARCH GXV-V32DIRMAIL
            GXV-V34TEXTO GXV-V31ARCHIVO
                ON OVERFLOW
                MOVE 'ACMMAIL' TO GX-ERRPGM
                GO TO GX-CALL-ERROR
           END-CALL
           PERFORM RPT-DISPLAY-OUTPUT
           CONTINUE.

       RPT-HEADER.

           IF RPT-HEADER-ON
              SET RPT-HEADER-OFF TO TRUE
              IF RPT-NOT-FIRST-PAGE

                 MOVE SPACES TO W-LINE-TO-PRINT
                 COMPUTE RPT-LINES-TO-ADVANCE = 1
                 PERFORM RPT-PRINT-LINE

                 PERFORM RPT-FOOTER

                 IF GXV-V37GX-LINE NOT GREATER RPT-PAGE-LENGTH
                    MOVE SPACES TO W-LINE-TO-PRINT
                    COMPUTE RPT-LINES-TO-ADVANCE = 1 + RPT-PAGE-LENGTH -
            GXV-V37GX-LINE
                    PERFORM RPT-PRINT-LINE

                 END-IF
              END-IF
              MOVE SPACES TO W-LINE-TO-PRINT
              COMPUTE RPT-LINES-TO-ADVANCE = 1
              PERFORM RPT-PRINT-LINE

              MOVE 2 TO GXV-V37GX-LINE
              SET RPT-NOT-FIRST-PAGE TO TRUE
           END-IF
           CONTINUE.

       RPT-FOOTER.

           CONTINUE.

       RPT-CHECK-EOP.

           SET RPT-RCDS-FOUND TO TRUE
           IF GXV-V37GX-LINE + W-LINES-TO-ADVANCE GREATER
              RPT-PAGE-LENGTH - RPT-BOTTOM-MARGIN

              COMPUTE W-LINES-TO-ADVANCE = RPT-PAGE-LENGTH -
                                           RPT-BOTTOM-MARGIN -
                                           GXV-V37GX-LINE
              SET RPT-HEADER-ON TO TRUE
              SET RPT-NOT-FIRST-PAGE TO TRUE
           END-IF

           EVALUATE TRUE
              WHEN W-LINES-TO-ADVANCE NOT GREATER 0
                 MOVE 0 TO RPT-LINES-TO-ADVANCE
                 PERFORM RPT-PRINT-LINE
              WHEN OTHER
                 MOVE 1 TO RPT-LINES-TO-ADVANCE
                 PERFORM RPT-PRINT-LINE
                 SUBTRACT 1 FROM W-LINES-TO-ADVANCE
                 MOVE SPACES TO W-LINE-TO-PRINT
                 PERFORM RPT-PRINT-LINE W-LINES-TO-ADVANCE TIMES
           END-EVALUATE
           CONTINUE.

       RPT-PRINT-LINE.

           WRITE GX-PRN-RECORD FROM W-LINE-TO-PRINT
                 BEFORE ADVANCING RPT-LINES-TO-ADVANCE LINES
           ADD RPT-LINES-TO-ADVANCE TO GXV-V37GX-LINE

           CONTINUE.
      * --- Standard variables for proc program
      * --- Referenced routines
       GX-ABORT.

           MOVE 'S'       TO GX-MSGTYPE
           CALL 'GXMSGS'  USING GX-MSGID
                                GX-MSGDTA
                                GX-MSGTYPE
           STOP RUN.
       GX-CALL-ERROR.

           MOVE 'GXM9999' TO GX-MSGID
           MOVE SPACES    TO GX-MSGDTA
           STRING 'Error al llamar a ' GX-ERRPGM '.'
                  DELIMITED BY SIZE INTO GX-MSGDTA
           GO TO GX-ABORT.
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
