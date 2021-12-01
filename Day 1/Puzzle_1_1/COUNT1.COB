      ******************************************************************
      * Author: MARIA ASPVIK
      * Date: DEC 1, 2021
      * Purpose: JUST FOR FUN
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-1-1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFL1 ASSIGN TO "INPUTFILE.TXT"
           ORGANISATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD INPUTFL1.
       01 INPUTFL.
           05 INPFL-DEPTH              PIC X(6).
      *    RECORD LENGTH IS 6, 5 DIGITS PLUS ONE LINEBREAK.

       WORKING-STORAGE SECTION.

       01 DEPTH-DATA.
           05 DD-DEPTH                 PIC s9(5) VALUE ZERO.
           05 DD-DEPTH-P               PIC s9(5) VALUE 99999.
           05 DD-INCREASES             PIC S9(4) VALUE ZERO.

       01 EDITED-DATA.
           05 ED-DEPTH             PIC ZZZZ9.
           05 ED-INCREASES         PIC ZZZ9.

       01 SWITCHES.
               05 SW-EOF               PIC X VALUE 'N'.
               05 SW-DEPTH-INC         PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           OPEN INPUT INPUTFL1.
           PERFORM 100-READ-NEXT UNTIL SW-EOF = 'Y'.
           CLOSE INPUTFL1.
           MOVE DD-INCREASES TO ED-INCREASES.
           DISPLAY "NUMBER OF INCREASES ==> " ED-INCREASES.
           STOP RUN.

       100-READ-NEXT.
           READ INPUTFL1 NEXT
               AT END MOVE 'Y' TO SW-EOF
               NOT AT END PERFORM 200-COMPARE.

       200-COMPARE.
           MOVE INPFL-DEPTH TO DD-DEPTH.
           MOVE DD-DEPTH TO ED-DEPTH.

           IF DD-DEPTH IS GREATER THAN DD-DEPTH-P
               MOVE 'Y' TO SW-DEPTH-INC
               ADD 1 TO DD-INCREASES
               DISPLAY ED-DEPTH " ==> INCREASED, "
      -         "NUMBER OF INCREASES ARE " DD-INCREASES

           ELSE
               DISPLAY ED-DEPTH " ==> DECREASED".

           MOVE DD-DEPTH TO DD-DEPTH-P.
           MOVE 'N' TO SW-DEPTH-INC.
