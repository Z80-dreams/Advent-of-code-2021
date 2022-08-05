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
           SELECT INPUTFL1 ASSIGN TO "INFL2"
           ORGANIZATION IS SEQUENTIAL
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
           05 SD-DEPTH-CURRENT  COMP-3     PIC S9(7)   VALUE ZERO.
           05 SD-LENGTH-CURRENT COMP-3     PIC S9(7)   VALUE ZERO.
           05 SD-AIM-CURRENT    COMP-3     PIC S9(7)   VALUE ZERO.
           05 SD-AIM-DEPTH-C    COMP-3     PIC S9(7)   VALUE ZERO.
           05 SD-CODE           COMP-3     PIC S9(14)  VALUE ZERO.
      *
       01 EDITED-DATA.
           05 ED-DEPTH                     PIC z(6)9-.
           05 ED-LENGTH                    PIC Z(6)9-.
           05 ED-AIM                       PIC Z(6)9-.
           05 ED-CODE                      PIC Z(13)9-.
           05 BLANK-LINE                   PIC X(10)   VALUE SPACE.
       PROCEDURE DIVISION.
       000-MAIN-PROCEDURE.
           OPEN INPUT INPUTFL1.
           PERFORM 100-READ-NEXT UNTIL SW-EOF = 'Y'.
           CLOSE INPUTFL1.
           COMPUTE SD-CODE = SD-DEPTH-CURRENT * SD-LENGTH-CURRENT.
           MOVE SD-DEPTH-CURRENT TO ED-DEPTH.
           MOVE SD-LENGTH-CURRENT TO ED-LENGTH.
           MOVE SD-AIM-CURRENT TO ED-AIM.
           MOVE SD-CODE TO ED-CODE.
           DISPLAY "DEPTH ==> " ED-DEPTH " LENGTH ==> " ED-LENGTH
      -    " AIM ==> " ED-AIM.
           DISPLAY BLANK-LINE.
           DISPLAY "CODE TO OPEN CALENDAR IS ==> " ED-CODE.
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
                       SUBTRACT IR-AMOUNT FROM SD-AIM-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR
                   WHEN MOVE-DOWN
                       ADD IR-AMOUNT TO SD-AIM-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR
                   WHEN MOVE-FORWARD
                       PERFORM 300-MOVE-FORWARD
      *
           ELSE
               PERFORM 999-ERROR-PROCEDURE.
      *
       300-MOVE-FORWARD.
           ADD IR-AMOUNT TO SD-LENGTH-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR.
           COMPUTE SD-AIM-DEPTH-C = IR-AMOUNT * SD-AIM-CURRENT
                           ON SIZE ERROR
                               MOVE 'Y' TO SW-ERROR.
           ADD SD-AIM-DEPTH-C TO SD-DEPTH-CURRENT.
      *S
       999-ERROR-PROCEDURE.
           DISPLAY "SOMETHING WENT WRONG. HALTING."
               STOP RUN.
