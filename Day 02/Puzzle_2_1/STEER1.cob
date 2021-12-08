      ******************************************************************
      * Author: Maria Aspvik
      * Date: 2 Dc, 2021
      * Purpose: Just for fun
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. STEER-1.
      *
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT INPUTFL1 ASSIGN TO "INFL1"
           ORGANISATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
      *
       DATA DIVISION.
        FILE SECTION.
         FD INPUTFL1.
       01 INRECORD.
           05 IR-CONTROL-WORD              PIC X(7).
           05 IR-AMOUNT                    PIC 9(3).
           05 LINEBREAK                    PIC X.
      *
       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 SW-EOF                       PIC X       VALUE 'N'.
           05 SW-ERROR                     PIC X       VALUE 'N'.
      *
       01 MOVEMENT-DIRECTION               PIC X(7).
           88 MOVE-UP                                  VALUE "     up".
           88 MOVE-DOWN                                VALUE "   down".
           88 MOVE-FORWARD                             VALUE "forward".
      *
       01 STEERING-DATA.
           05 SD-DEPTH-CURRENT  COMP-3     PIC S9(5)   VALUE ZERO.
           05 SD-LENGTH-CURRENT COMP-3     PIC S9(5)   VALUE ZERO.
           05 SD-CODE           COMP-3     PIC S9(10)  VALUE ZERO.
      *
       01 EDITED-DATA.
           05 ED-DEPTH                     PIC z(4)9-  VALUE ZERO.
           05 ED-LENGTH                    PIC Z(4)9-  VALUE ZERO.
           05 ED-CODE                      PIC Z(9)9-  VALUE ZERO.
           05 BLANK-LINE                   PIC X(10)   VALUE SPACE.
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           OPEN INPUT INPUTFL1.
           PERFORM 100-READ-NEXT UNTIL SW-EOF = 'Y'.
           CLOSE INPUTFL1.
           COMPUTE SD-CODE = SD-DEPTH-CURRENT * SD-LENGTH-CURRENT.
           MOVE SD-DEPTH-CURRENT TO ED-DEPTH.
           MOVE SD-LENGTH-CURRENT TO ED-LENGTH.
           MOVE SD-CODE TO ED-CODE.
           DISPLAY "DEPTH ==> " ED-DEPTH " LENGTH ==> " ED-LENGTH.
           DISPLAY BLANK-LINE.
           DISPLAY "CODE TO OPEN CALENDAR IS ==> " ED-CODE.
           STOP RUN.
           STOP RUN.
      *
       100-READ-NEXT.
           READ INPUTFL1 NEXT
               AT END MOVE 'Y' TO SW-EOF
               NOT AT END PERFORM 200-UPDATE.
      *
       200-UPDATE.
           MOVE IR-CONTROL-WORD TO MOVEMENT-DIRECTION.
      *
           IF SW-ERROR = 'N' AND SW-EOF = 'N'
               EVALUATE TRUE
                   WHEN MOVE-UP
                       SUBTRACT IR-AMOUNT FROM SD-DEPTH-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR
                   WHEN MOVE-DOWN
                       ADD IR-AMOUNT TO SD-DEPTH-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR
                   WHEN MOVE-FORWARD
                       ADD IR-AMOUNT TO SD-LENGTH-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR
           ELSE
               DISPLAY "SOMETHING WENT WRONG. HALTING."
               STOP RUN.
