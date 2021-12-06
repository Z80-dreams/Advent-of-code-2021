      ******************************************************************
      * Author: MARIA ASPVIK
      * Date: 3 DEC, 2021
      * Purpose: JUST FOR FUN, ADVENT OF CODE 2021
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBMRCD02.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT2 ASSIGN TO "INPUT2"
       ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT2.
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
           05 WS-BITS-COUNT                        PIC S9(5)
                                                   VALUE 10000.
           05 WS-BIT-TABLE OCCURS 1 TO 10000 TIMES
           DEPENDING ON WS-BITS-COUNT
           INDEXED BY WS-BIT-TABLE-INDEX.
               10 WS-BELONGS-TO                    PIC X VALUE 'B'.
               10 WS-BIT-ROW-FLAT                  PIC 9(12).
               10 WS-BIT-ROW REDEFINES WS-BIT-ROW-FLAT
               OCCURS 12 TIMES
               INDEXED BY WS-BIT-ROW-INDEX.
                   15 WS-BIT-ROW-BIT               PIC 9.
      *
       01 BIT-COUNTER.
           05 BC-TABLE OCCURS 12 TIMES
           INDEXED BY BC-TABLE-INDEX.
               10 BC-TABLE-COUNT-ONES-O2           PIC 9(5) VALUE ZERO.
               10 BC-TABLE-COUNT-ZEROS-O2          PIC 9(5) VALUE ZERO.
               10 BC-TABLE-COUNT-ONES-CO2          PIC 9(5) VALUE ZERO.
               10 BC-TABLE-COUNT-ZEROS-CO2         PIC 9(5) VALUE ZERO.
               10 BC-MOST-COMMON-BIT-O2            PIC 9    VALUE ZERO.
               10 BC-LEAST-COMMON-BIT-CO2          PIC 9    VALUE ZERO.
      *
       01  COUNTERS.
           05 TOTAL-COUNTER                        PIC 9(5) VALUE ZERO.
           05 COMPARE-COUNTER                      PIC 9(5) VALUE ZERO.
      *
       01 O2-RATE.
           05 O2-BITS-FLAT                         PIC 9(12) VALUE ZERO.
           05 O2-BITS REDEFINES O2-BITS-FLAT       PIC 9
           OCCURS 12 TIMES
           INDEXED BY O2-BITS-INDEX.
           05 O2-TOTAL                             PIC 9(6) VALUE ZERO.
      *
       01 CO2-RATE.
           05 CO2-BITS-FLAT                        PIC 9(12) VALUE ZERO.
           05 CO2-BITS REDEFINES CO2-BITS-FLAT     PIC 9    VALUE ZERO
           OCCURS 12 TIMES
           INDEXED BY CO2-BITS-INDEX.
           05 CO2-TOTAL                            PIC 9(6) VALUE ZERO.
      *
       01 TOTAL-RATE                               PIC 9(12).
      *
       01 DISPLAY-DATA.
           05 O2-DISPLAY                           PIC Z(11)9.
           05 CO2-DISPLAY                          PIC Z(11)9.
           05 TOTAL-DISPLAY                        PIC Z(11)9.
           05 BIT-COUNTERS-DISPLAY OCCURS 12 TIMES
           INDEXED BY BC-COUNTERS-DISPLAY-INDEX.
               10 BCC-DISPLAY                      PIC Z(4)9.
      *
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
           PERFORM 100-READ-FILE.
           PERFORM 300-FIND-GROUPS.
           PERFORM 800-COMPUTE-BIT-VALUES.
           STOP RUN.

       100-READ-FILE.
           OPEN INPUT INPUT2.
           SET WS-BIT-TABLE-INDEX TO 0.
           PERFORM 200-READ-NEXT UNTIL EOF-SWITCH = 'Y'.
           CLOSE INPUT2.
           SET WS-BITS-COUNT TO WS-BIT-TABLE-INDEX.
           DISPLAY WS-BITS-COUNT " RECORDS WAS READ IN!".
      *
       200-READ-NEXT.
           READ INPUT2 NEXT
           AT END MOVE 'Y' TO EOF-SWITCH
           NOT AT END PERFORM 250-APPEND-TABLE.
      *
       250-APPEND-TABLE.
           SET WS-BIT-TABLE-INDEX UP BY 1.
      *    MOVE TO THE CORRECT ROW IN THE TABLE.
           MOVE IB-BITS TO WS-BIT-ROW-FLAT(WS-BIT-TABLE-INDEX).
      *
       300-FIND-GROUPS.
           PERFORM VARYING WS-BIT-ROW-INDEX FROM 1 BY 1
           UNTIL WS-BIT-ROW-INDEX > 12
           PERFORM 310-COUNT-OCCURANCES
           PERFORM 320-FIND-COMMON-BIT
           PERFORM 350-FILTER-BITS
           END-PERFORM.
           PERFORM 800-COMPUTE-BIT-VALUES.
           MOVE O2-TOTAL TO O2-DISPLAY.
           MOVE CO2-TOTAL TO CO2-DISPLAY.
           MOVE TOTAL-RATE TO TOTAL-DISPLAY.
           DISPLAY "   O2 RATE ==> " O2-BITS-FLAT
                   "  O2 RATE ==> " O2-DISPLAY.
           DISPLAY "  CO2 RATE ==> " CO2-BITS-FLAT
                   " CO2 RATE ==> " CO2-DISPLAY.
           DISPLAY "TOTAL RATE ==> " TOTAL-DISPLAY.
      *
       310-COUNT-OCCURANCES.
           INITIALIZE TOTAL-COUNTER.
           INITIALIZE BC-TABLE-COUNT-ONES-O2(WS-BIT-ROW-INDEX).
           INITIALIZE BC-TABLE-COUNT-ONES-CO2(WS-BIT-ROW-INDEX).
           INITIALIZE BC-TABLE-COUNT-ZEROS-O2(WS-BIT-ROW-INDEX).
           INITIALIZE BC-TABLE-COUNT-ZEROS-CO2(WS-BIT-ROW-INDEX).
           PERFORM 315-ADD-BIT-TO-COUNTER
           VARYING WS-BIT-TABLE-INDEX FROM 1 BY 1
           UNTIL WS-BIT-TABLE-INDEX > WS-BITS-COUNT.
      *
       315-ADD-BIT-TO-COUNTER.
           IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) NOT EQUALS 'N' AND
              WS-BELONGS-TO(WS-BIT-TABLE-INDEX) NOT EQUALS 'C'
               IF WS-BIT-ROW-BIT(WS-BIT-TABLE-INDEX, WS-BIT-ROW-INDEX)
                   = 1
                   ADD 1 TO BC-TABLE-COUNT-ONES-O2(WS-BIT-ROW-INDEX)
               ELSE
                   ADD 1 TO BC-TABLE-COUNT-ZEROS-O2(WS-BIT-ROW-INDEX)
               END-IF
           END-IF.
               IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) NOT EQUALS 'N' AND
              WS-BELONGS-TO(WS-BIT-TABLE-INDEX) NOT EQUALS 'O'
               IF WS-BIT-ROW-BIT(WS-BIT-TABLE-INDEX, WS-BIT-ROW-INDEX)
                   = 1
                   ADD 1 TO BC-TABLE-COUNT-ONES-CO2(WS-BIT-ROW-INDEX)
               ELSE
                   ADD 1 TO BC-TABLE-COUNT-ZEROS-CO2(WS-BIT-ROW-INDEX)
               END-IF
           END-IF.
           IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) NOT EQUALS 'N'
               ADD 1 TO TOTAL-COUNTER
           END-IF.
      *
       320-FIND-COMMON-BIT.
           IF BC-TABLE-COUNT-ONES-O2(WS-BIT-ROW-INDEX)
               IS GREATER THAN OR EQUAL TO
               BC-TABLE-COUNT-ZEROS-O2(WS-BIT-ROW-INDEX)
               MOVE 1 TO BC-MOST-COMMON-BIT-O2(WS-BIT-ROW-INDEX)
           ELSE
               MOVE 0 TO BC-MOST-COMMON-BIT-O2(WS-BIT-ROW-INDEX)
           END-IF.
           IF BC-TABLE-COUNT-ZEROS-CO2(WS-BIT-ROW-INDEX)
               IS LESS THAN OR EQUAL TO
               BC-TABLE-COUNT-ONES-CO2(WS-BIT-ROW-INDEX)
               MOVE 0 TO BC-LEAST-COMMON-BIT-CO2(WS-BIT-ROW-INDEX)
           ELSE
               MOVE 1 TO BC-LEAST-COMMON-BIT-CO2(WS-BIT-ROW-INDEX)
           END-IF.
           PERFORM 321-DISPLAY-COUNTING.
      *
       321-DISPLAY-COUNTING.
           DISPLAY "==============================================="
           DISPLAY "COUNTING BIT " WS-BIT-ROW-INDEX.
           DISPLAY "NUMBER OF ONES IN O2 CODE   ==> "
                   BC-TABLE-COUNT-ONES-O2(WS-BIT-ROW-INDEX)
           DISPLAY "NUMBER OF ZEROS IN O2 CODE  ==> "
                   BC-TABLE-COUNT-ZEROS-O2(WS-BIT-ROW-INDEX)
           DISPLAY "NUMBER OF ONES IN CO2 CODE  ==> "
                   BC-TABLE-COUNT-ONES-CO2(WS-BIT-ROW-INDEX)
           DISPLAY "NUMBER OF ZEROS IN CO2 CODE ==> "
                   BC-TABLE-COUNT-ZEROS-CO2(WS-BIT-ROW-INDEX)
           DISPLAY "TOTAL NUMBER OF BITS        ==> " TOTAL-COUNTER.
           DISPLAY "MOST COMMON BIT IN O2 IS    ==> "
                   BC-MOST-COMMON-BIT-O2(WS-BIT-ROW-INDEX)
           DISPLAY "LEAST COMMON BIT IS CO2     ==> "
                   BC-LEAST-COMMON-BIT-CO2(WS-BIT-ROW-INDEX).
           DISPLAY "-----------------------------------------------".
      *
       350-FILTER-BITS.
               PERFORM 370-SORT-ONE-BIT VARYING WS-BIT-TABLE-INDEX
               FROM 1 BY 1 UNTIL WS-BIT-TABLE-INDEX IS GREATER THAN
               WS-BITS-COUNT.
      *
       370-SORT-ONE-BIT.
           IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS NOT EQUAL TO 'C' AND
               WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS NOT EQUAL TO 'N'
               IF BC-MOST-COMMON-BIT-O2(WS-BIT-ROW-INDEX) IS EQUAL TO
                   WS-BIT-ROW-BIT(WS-BIT-TABLE-INDEX WS-BIT-ROW-INDEX)
                   MOVE 'O' TO WS-BELONGS-TO(WS-BIT-TABLE-INDEX)
                   MOVE WS-BIT-ROW-FLAT(WS-BIT-TABLE-INDEX)
                   TO O2-BITS-FLAT
               ELSE
                   IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS EQUAL TO 'O'
                       MOVE 'N' TO WS-BELONGS-TO(WS-BIT-TABLE-INDEX)
                   END-IF
               END-IF
           END-IF.
      *
           IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS NOT EQUAL TO 'O' AND
               WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS NOT EQUAL TO 'N'
               IF BC-LEAST-COMMON-BIT-CO2(WS-BIT-ROW-INDEX) IS EQUAL TO
                   WS-BIT-ROW-BIT(WS-BIT-TABLE-INDEX, WS-BIT-ROW-INDEX)
                   MOVE 'C' TO WS-BELONGS-TO(WS-BIT-TABLE-INDEX)
                   MOVE WS-BIT-ROW-FLAT(WS-BIT-TABLE-INDEX)
                   TO CO2-BITS-FLAT
               ELSE
                   IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS EQUAL TO 'C'
                       MOVE 'N' TO WS-BELONGS-TO(WS-BIT-TABLE-INDEX)
                   END-IF
               END-IF
           END-IF.
      *
           IF WS-BELONGS-TO(WS-BIT-TABLE-INDEX) IS NOT EQUAL TO 'N'
               PERFORM 371-PRINT-ROW
           END-IF.
      *
       371-PRINT-ROW.
           DISPLAY "CODE ==> " WS-BELONGS-TO(WS-BIT-TABLE-INDEX)
           " BITPATTERN ==> " WS-BIT-ROW-FLAT(WS-BIT-TABLE-INDEX).
      *
       800-COMPUTE-BIT-VALUES.
           PERFORM 850-COMPUTE-SINGLE-BIT VARYING WS-BIT-ROW-INDEX
           FROM 1 BY 1 UNTIL WS-BIT-ROW-INDEX > 12.
           COMPUTE TOTAL-RATE = O2-TOTAL * CO2-TOTAL.
      *
       850-COMPUTE-SINGLE-BIT.
           COMPUTE O2-TOTAL = O2-TOTAL +
           O2-BITS(WS-BIT-ROW-INDEX) *
           BIT-VALUES-T-TABLE(WS-BIT-ROW-INDEX).
      *
           COMPUTE CO2-TOTAL = CO2-TOTAL +
           CO2-BITS(WS-BIT-ROW-INDEX) *
           BIT-VALUES-T-TABLE(WS-BIT-ROW-INDEX).
