      ******************************************************************
      * Author: MARIA ASPVIK
      * Date: 6 DEC 2021
      * Purpose: JUST FOR FUN
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQUID-BINGO-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BINGO-BOARDS ASSIGN TO "BOARDS1"
           ORGANIZATION IS SEQUENTIAL.
           SELECT BINGO-NUMBERS ASSIGN TO "NUMBERS1"
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD BINGO-BOARDS.
           01 BB-ROWS.
               05 NEXT-ROW                         PIC 9(15).
               05 BB-LINEBREAK                     PIC X.
      *
           FD BINGO-NUMBERS.
           01 BN-ROWS.
               05 NEXT-NUMBER                      PIC 99.
               05 BN-LINEBREAK                     PIC X.
       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 BB-EOF-SWITCH                        PIC X VALUE 'N'.
           05 BN-EOF-SWITCH                        PIC X VALUE 'N'.
           05 BINGO-FOUND-SWITCH                   PIC X VALUE 'N'.
      *
       01 COUNTERS.
           05 RECORD-COUNTER                       PIC 9(5) VALUE 99999.

       01 BB-LIST OCCURS 1 TO 99999 TIMES
           DEPENDING ON RECORD-COUNTER
           INDEXED BY BB-LIST-INDEX.
           05 BBL-ROWS OCCURS 5 TIMES
               INDEXED BY BBL-ROWS-INDEX.
               10 BBL-FLAT-ROW                     PIC 9(15) VALUE ZERO.
               10 BBL-COLUMNS REDEFINES BBL-FLAT-ROW
                   OCCURS 5 TIMES
                   INDEXED BY BBL-COLUMNS-INDEX.
                   15 BBL-CELL                     PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-READ-INPUT-BOARDS.
            DISPLAY "NUMBER OF RECORDS READ ==> " RECORD-COUNTER.
            STOP RUN.
      *
       100-READ-INPUT-BOARDS.
           OPEN INPUT BINGO-BOARDS.
           SET BB-LIST-INDEX TO 1.
           SET BBL-ROWS-INDEX TO 0.
           SET BBL-COLUMNS-INDEX TO 1.
           PERFORM 110-READ-NEXT-BOARD UNTIL BB-EOF-SWITCH = 'Y'.
           CLOSE BINGO-BOARDS.
           SET BB-LIST-INDEX DOWN BY 1.
           SET RECORD-COUNTER TO BB-LIST-INDEX.
      *
       110-READ-NEXT-BOARD.
           READ BINGO-BOARDS NEXT RECORD
           AT END MOVE 'Y' TO BB-EOF-SWITCH
           NOT AT END PERFORM 120-INSERT-RECORD.
      *
       120-INSERT-RECORD.
           SET BBL-ROWS-INDEX UP BY 1.
           MOVE NEXT-ROW TO BBL-FLAT-ROW(BB-LIST-INDEX BBL-ROWS-INDEX).
           IF BBL-ROWS-INDEX IS GREATER THAN OR EQUAL TO 5
               SET BB-LIST-INDEX UP BY 1
               SET BBL-ROWS-INDEX TO 0
           END-IF.

       END PROGRAM SQUID-BINGO-1.
