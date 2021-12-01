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
           05 DD-DEPTH.
               10 DD-DEPTH-1               PIC s9(5) VALUE 99999.
               10 DD-DEPTH-2               PIC s9(5) VALUE 99999.
               10 DD-DEPTH-3               PIC s9(5) VALUE 99999.
           05 DD-DEPTH-WINDOW.
               10 DD-DW-1                  PIC s9(7) VALUE 9999999.
               10 DD-DW-2                  PIC s9(7) VALUE 9999999.
           05 DD-INCREASES                 PIC S9(4) VALUE ZERO.

       01 EDITED-DATA.
           05 ED-DW-1                      PIC ZZZZZZ9.
           05 ED-DW-2                      PIC ZZZZZZ9.
           05 ED-INCREASES                 PIC ZZZ9.

       01 SWITCHES.
               05 SW-EOF                   PIC X VALUE 'N'.
               05 SW-DEPTH-INC             PIC X VALUE 'N'.

       01 PRINT-HEADER.
           05 FILLER                       PIC X(7)  VALUE "DEPTH  ".
           05 FILLER                       PIC X(5)  VALUE SPACE.
           05 FILLER                       PIC X(9)  VALUE "STATUS   ".
           05 FILLER                       PIC X(10) VALUE "       INC".

       77 BLANK-LINE                       PIC X(21) VALUE SPACE.

       01 PRINT-AREA.
           05 PA-DEPTH-WINDOW              PIC X(7) VALUE SPACE.
           05 ARROW                        PIC X(5) VALUE " ==> ".
           05 PA-STATUS                    PIC X(9) VALUE SPACE.
           05 FILLER                       PIC X(6) VALUE SPACE.
           05 PA-INCREASES                 PIC X(4) VALUE SPACE.

       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           OPEN INPUT INPUTFL1.
           DISPLAY PRINT-HEADER.
           PERFORM 100-READ-NEXT UNTIL SW-EOF = 'Y'.
           CLOSE INPUTFL1.
           DISPLAY BLANK-LINE.
           DISPLAY "NUMBER OF INCREASES ==> " ED-INCREASES.
           STOP RUN.

       100-READ-NEXT.
           READ INPUTFL1 NEXT
               AT END MOVE 'Y' TO SW-EOF
               NOT AT END PERFORM 200-COMPARE.

       200-COMPARE.
           MOVE INPFL-DEPTH TO DD-DEPTH-1.
           ADD DD-DEPTH-1 DD-DEPTH-2 DD-DEPTH-3 GIVING DD-DW-1.
           MOVE DD-DW-1 TO ED-DW-1.
           MOVE ED-DW-1 TO PA-DEPTH-WINDOW.

           IF DD-DW-1 IS GREATER THAN DD-DW-2
               MOVE 'Y' TO SW-DEPTH-INC
               ADD 1 TO DD-INCREASES
               MOVE DD-INCREASES TO ED-INCREASES
               MOVE "INCREASED" TO PA-STATUS
           ELSE
               MOVE "DECREASED" TO PA-STATUS.

           MOVE ED-INCREASES TO PA-INCREASES.

           DISPLAY PRINT-AREA.

           MOVE DD-DEPTH-2 TO DD-DEPTH-3.
           MOVE DD-DEPTH-1 TO DD-DEPTH-2.

           MOVE DD-DW-1 TO DD-DW-2.
           MOVE 'N' TO SW-DEPTH-INC.
