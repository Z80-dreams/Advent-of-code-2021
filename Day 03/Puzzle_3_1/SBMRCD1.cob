      ******************************************************************
      * Author: MARIA ASPVIK
      * Date: 3 DEC, 2021
      * Purpose: JUST FOR FUN, ADVENT OF CODE 2021
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBMRCD01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT1 ASSIGN TO "INPUT1"
       ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT1.
       01 INPUT-BITPATTERN.
           05 IB-BITS                              PIC 9(12).
           05 LINEBREAK                            PIC X.
      *
       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 EOF-SWITCH                           PIC X VALUE 'N'.
           05 ERROR-SWITCH                         PIC X VALUE 'N'.
      *
       01 WS-BITS.
           05 WS-BIT-TABLE PIC 9 VALUE ZERO OCCURS 12 TIMES.
      *
       01 WS-BITS-SUBS                             PIC 99   VALUE ZERO.
      *
       01 BIT-COUNTER.
           05 BC-TABLE  PIC 9(5) VALUE ZERO OCCURS 12 TIMES.
      *
       01  COUNTERS.
           05 TOTAL-COUNTER                        PIC 9(5) VALUE ZERO.
           05 COMPARE-COUNTER                      PIC 9(5) VALUE ZERO.
      *
       01 GAMMA-RATE.
           05 GAMMA-BITS PIC 9 VALUE ZERO OCCURS 12 TIMES.
           05 GAMMA-TOTAL                          PIC 9(6) VALUE ZERO.
      *
       01 EPSILON-RATE.
           05 EPSILON-BITS PIC 9 VALUE ZERO OCCURS 12 TIMES.
           05 EPSILON-TOTAL                        PIC 9(6) VALUE ZERO.
      *
       01 TOTAL-RATE                               PIC 9(12).
      *
       01 DISPLAY-DATA.
           05 GAMMA-DISPLAY                        PIC Z(11)9.
           05 EPSILON-DISPLAY                      PIC Z(11)9.
           05 TOTAL-DISPLAY                        PIC Z(11)9.

       01 BIT-VALUES.
           05 BV-1  PIC 9(4) VALUE 2048.
           05 BV-2  PIC 9(4) VALUE 1024.
           05 BV-3  PIC 9(4) VALUE  512.
           05 BV-4  PIC 9(4) VALUE  256.
           05 BV-5  PIC 9(4) VALUE  128.
           05 BV-6  PIC 9(4) VALUE   64.
           05 BV-7  PIC 9(4) VALUE   32.
           05 BV-8  PIC 9(4) VALUE   16.
           05 BV-9  PIC 9(4) VALUE    8.
           05 BV-10 PIC 9(4) VALUE    4.
           05 BV-11 PIC 9(4) VALUE    2.
           05 BV-12 PIC 9(4) VALUE    1.
      *
       01 BIT-VALUES-T REDEFINES BIT-VALUES.
           05 BIT-VALUES-T-TABLE PIC 9(4) OCCURS 12 TIMES.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT1.
           PERFORM 200-READ-NEXT UNTIL EOF-SWITCH = 'Y'.
           CLOSE INPUT1.
           PERFORM 300-SUMMARIZE-LOOP.
           PERFORM 500-PRINT-BITS.
           PERFORM 600-DISPLAY-SUMMARY.
           STOP RUN.

       200-READ-NEXT.
           READ INPUT1 NEXT
           AT END MOVE 'Y' TO EOF-SWITCH
           NOT AT END PERFORM 250-BIT-LOOPS.

       250-BIT-LOOPS.
           MOVE IB-BITS TO WS-BITS
           PERFORM
           VARYING WS-BITS-SUBS FROM 1 BY 1 UNTIL WS-BITS-SUBS > 12
           ADD WS-BIT-TABLE(WS-BITS-SUBS) TO BC-TABLE(WS-BITS-SUBS)
           END-PERFORM.
           ADD 1 TO TOTAL-COUNTER.
      *
       300-SUMMARIZE-LOOP.
           COMPUTE COMPARE-COUNTER = TOTAL-COUNTER / 2.
           PERFORM 400-SUMMARIZE
           VARYING WS-BITS-SUBS FROM 1 BY 1 UNTIL WS-BITS-SUBS > 12.
      *
       400-SUMMARIZE.
           IF BC-TABLE(WS-BITS-SUBS) IS GREATER THAN COMPARE-COUNTER
               MOVE 1 TO GAMMA-BITS(WS-BITS-SUBS)
               MOVE 0 TO EPSILON-BITS(WS-BITS-SUBS)
           ELSE
               MOVE 0 TO GAMMA-BITS(WS-BITS-SUBS)
               MOVE 1 TO EPSILON-BITS(WS-BITS-SUBS).
      *
           COMPUTE GAMMA-TOTAL = GAMMA-TOTAL + GAMMA-BITS(WS-BITS-SUBS)
      -     * BIT-VALUES-T-TABLE(WS-BITS-SUBS).
      *
           COMPUTE EPSILON-TOTAL = EPSILON-TOTAL
      -     + EPSILON-BITS(WS-BITS-SUBS)
      -     * BIT-VALUES-T-TABLE(WS-BITS-SUBS).
      *
       500-PRINT-BITS.
           PERFORM VARYING WS-BITS-SUBS FROM 1 BY 1
           UNTIL WS-BITS-SUBS > 12
           DISPLAY "BIT " WS-BITS-SUBS " COUNT IS "
      -     BC-TABLE(WS-BITS-SUBS) " TOTAL IS "
            TOTAL-COUNTER " GAMMA  IS "
      -     GAMMA-BITS(WS-BITS-SUBS) " EPSILON IS "
      -     EPSILON-BITS(WS-BITS-SUBS)
           END-PERFORM.
      *
       600-DISPLAY-SUMMARY.
           COMPUTE TOTAL-RATE = GAMMA-TOTAL * EPSILON-TOTAL.
      *
           MOVE GAMMA-TOTAL TO GAMMA-DISPLAY.
           MOVE EPSILON-TOTAL TO EPSILON-DISPLAY.
           MOVE TOTAL-RATE TO TOTAL-DISPLAY.
      *
           DISPLAY "EPSILON TOTAL ==> " EPSILON-DISPLAY.
           DISPLAY "  GAMMA TOTAL ==> " GAMMA-DISPLAY.
           DISPLAY "   FINAL CODE ==> " TOTAL-DISPLAY.
