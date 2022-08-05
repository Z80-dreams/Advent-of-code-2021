      ******************************************************************
      * Author:  MARIA ASPVIK
      * Date:    2022-08-05
      * Purpose: JUST FOR FUN
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. LINEPGM.
        AUTHOR MARIA ASPVIK.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * INPUT FILE
      *    SELECT IN-FILE ASSIGN TO "TESTINPUT"
           SELECT IN-FILE ASSIGN TO "INPUT"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL

           FILE STATUS IS W-INFILE-STATUS.
      * OUTPUT FILE
           SELECT OUT-FILE ASSIGN TO "OUTPUT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS W-OUTFILE-STATUS.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      * INPUT FILE
       FD IN-FILE
       RECORD IS VARYING FROM 1 TO 80 CHARACTERS.
       01 F-INREC PIC X(080).
      * OUTPUT FILE
       FD OUT-FILE
       RECORDING MODE IS FIXED.
       01 F-OUTREC PIC X(5000).
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
      * INPUT FILE STATUS FIELDS
       01 W-INFILE-STATUS                PIC XX.
           88 W-INFILE-OK                          VALUE '00'.
           88 W-INFILE-EOF                         VALUE '10'.
       01 W-INFILE-SWITCH                PIC X.
           88 W-INFILE-OPEN                        VALUE 'O'.
           88 W-INFILE-CLOSED                      VALUE 'C'.
           88 W-INFILE-ERROR                       VALUE 'E'.
       01 W-INFILE-NUMBER-OF-RECORDS     PIC 9(06).
      *
      * OUTPUT FILE STATUS FIELDS
       01 W-OUTFILE-STATUS               PIC XX.
           88 W-OUTFILE-OK                         VALUE '00'.
           88 W-OUTFILE-EOF                        VALUE '10'.
       01 W-OUTFILE-SWITCH               PIC X.
           88 W-OUTFILE-OPEN                       VALUE 'O'.
           88 W-OUTFILE-CLOSED                     VALUE 'C'.
           88 W-OUTFILE-ERROR                      VALUE 'E'.
       01 W-OUTFILE-NUMBER-OF-RECORDS    PIC 9(06).
      *
      * POINTS
       01 W-POINT-ALPHANUMERIC.
           05 W-POINT-ALPHANUMERIC-X     PIC X(06).
           05 W-POINT-ALPHANUMERIC-Y     PIC X(06).
      *
       01 W-POINTS-NUMERIC.
          05 W-START-POINT.
             10 W-START-POINT-X          PIC 9(06).
             10 W-START-POINT-Y          PIC 9(06).
      *
          05 W-END-POINT.
             10 W-END-POINT-X            PIC 9(06).
             10 W-END-POINT-Y            PIC 9(06).
      *
          05 W-INTERMEDIARY-POINT.
             10 W-INTERMEDIARY-POINT-X   PIC 9(06).
             10 W-INTERMEDIARY-POINT-Y   PIC 9(06).
      *
      * INDEXES
       01 W-IX-INREC                     PIC 9(04).
      *
      * COUNTERS
       01 W-TALLY-COUNTER                PIC 9(02) VALUE ZERO.
       01 W-TWO-LINES-MEET               COMPUTATIONAL-5
                                         PIC 9(09) VALUE ZERO.
       01 W-DISPLAY-NUM.
          05 W-DISPLAY-ROW               OCCURS 1000 TIMES
             INDEXED BY W-DISPLAY-ROW-IX.
             10 DUMMY                    PIC X      VALUE SPACE.
             10 W-DISPLAY-XY             PIC Z(03)9.
       01 W-COUNTER-POINTS.
          05 W-COUNTER-POINTS-ROW        OCCURS 1000 TIMES
             INDEXED BY W-COUNTER-POINTS-ROW-IX.
             10 W-COUNTER-POINTS-COLUMN  OCCURS 1000 TIMES
                INDEXED BY W-COUNTER-POINTS-COLUMN-IX.
                15 W-COUNTER-POINTS-XY   COMPUTATIONAL-5
                                         PIC 9(04)  VALUE ZERO.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       MAIN-PROCEDURE SECTION.
           PERFORM INIT-FILES
           PERFORM INIT-VARIABLES
           PERFORM OPEN-INPUT-FILE
           PERFORM OPEN-OUTPUT-FILE
           IF NOT W-INFILE-ERROR
               PERFORM LOAD-POINTS
               PERFORM COUNT-MIN-TWO-LINES
           END-IF
           IF W-INFILE-OPEN AND W-OUTFILE-OPEN
               PERFORM CLOSE-INPUT-FILE
               PERFORM CLOSE-OUTPUT-FILE
           END-IF
           PERFORM PRINT-STATS
           CONTINUE.
       MAIN-PROCEDURE-END.
           GOBACK.

       INIT-VARIABLES SECTION.
           INITIALIZE W-COUNTER-POINTS
           INITIALIZE W-TALLY-COUNTER
           INITIALIZE W-TWO-LINES-MEET
           CONTINUE.
       INIT-VARIABLES-END.
           EXIT.

       INIT-FILES SECTION.
           SET W-INFILE-CLOSED          TO TRUE
           SET W-OUTFILE-CLOSED         TO TRUE
           MOVE ZERO                    TO W-INFILE-NUMBER-OF-RECORDS
           MOVE ZERO                    TO W-OUTFILE-NUMBER-OF-RECORDS
           MOVE ZERO                    TO W-IX-INREC
           CONTINUE.
       INIT-END.
           EXIT.

       OPEN-INPUT-FILE SECTION.
           IF W-INFILE-CLOSED THEN
               OPEN INPUT IN-FILE
               IF W-INFILE-OK THEN
                   SET W-INFILE-OPEN    TO TRUE
               ELSE
                   DISPLAY 'ERROR WHILE OPENING INPUT FILE: '
                       W-INFILE-STATUS
                   SET W-INFILE-ERROR   TO TRUE
               END-IF
           END-IF
           CONTINUE.
       OPEN-INPUT-FILE-END.
           EXIT.

       READ-INPUT-RECORD SECTION.
           READ IN-FILE NEXT RECORD
           IF W-INFILE-OK OR W-INFILE-EOF
              IF W-INFILE-OK
                 ADD 1 TO W-INFILE-NUMBER-OF-RECORDS
              END-IF
           ELSE
              DISPLAY 'ERROR WHILE READING INPUT FILE'
              PERFORM ERROR-HANDLING
           END-IF

           CONTINUE.
       READ-INPUT-RECORD-END.
           EXIT.

       CLOSE-INPUT-FILE SECTION.
           IF W-INFILE-OPEN THEN
               CLOSE IN-FILE
           END-IF
           CONTINUE.
       CLOSE-INPUT-FILE-END.
           EXIT.

       OPEN-OUTPUT-FILE SECTION.
           IF W-OUTFILE-CLOSED THEN
               OPEN OUTPUT OUT-FILE
               IF W-OUTFILE-OK THEN
                   SET W-OUTFILE-OPEN    TO TRUE
               ELSE
                   DISPLAY 'ERROR WHILE OPENING OUTPUT FILE: '
                       W-OUTFILE-STATUS
                   SET W-OUTFILE-ERROR   TO TRUE
               END-IF
           END-IF
           CONTINUE.
       OPEN-OUTPUT-FILE-END.
           EXIT.

       WRITE-OUTPUT-RECORD SECTION.
           WRITE F-OUTREC
           IF W-OUTFILE-OK
              ADD 1 TO W-OUTFILE-NUMBER-OF-RECORDS
           ELSE
              DISPLAY 'ERROR WHILE WRITING OUTPUT FILE'
              PERFORM ERROR-HANDLING
           END-IF
           CONTINUE.
       WRITE-OUTPUT-RECORD-END.
           EXIT.

       CLOSE-OUTPUT-FILE SECTION.
           IF W-OUTFILE-OPEN THEN
               CLOSE OUT-FILE
           END-IF
           CONTINUE.
       CLOSE-OUTPUT-FILE-END.
           EXIT.

       LOAD-POINTS SECTION.
           INITIALIZE W-INFILE-NUMBER-OF-RECORDS
           PERFORM UNTIL W-INFILE-EOF
              PERFORM READ-INPUT-RECORD
              IF W-INFILE-OK
                 PERFORM FIND-START-AND-END-POINTS
                 PERFORM FIND-INTERMEDIARY-POINTS
              END-IF
           END-PERFORM
           PERFORM PRINT-COUNTER-TABLE
           CONTINUE.
       LOAD-POINTS-END.
           EXIT.

       FIND-START-AND-END-POINTS SECTION.
           INITIALIZE W-TALLY-COUNTER
      * FINDING THE FORMAT IS 'FIRST-X,FIRST-Y -> LAST-X,LAST-Y'
           INSPECT F-INREC TALLYING W-TALLY-COUNTER
              FOR CHARACTERS BEFORE INITIAL '->'
           UNSTRING F-INREC(1:W-TALLY-COUNTER) DELIMITED BY ','
              INTO W-POINT-ALPHANUMERIC-Y
                   W-POINT-ALPHANUMERIC-X
           MOVE W-POINT-ALPHANUMERIC-X    TO W-START-POINT-X
           MOVE W-POINT-ALPHANUMERIC-Y    TO W-START-POINT-Y
           ADD 1 TO W-START-POINT-X
           ADD 1 TO W-START-POINT-Y
           ADD 3 TO W-TALLY-COUNTER
           UNSTRING F-INREC(W-TALLY-COUNTER:) DELIMITED BY ','
              INTO W-POINT-ALPHANUMERIC-Y
                   W-POINT-ALPHANUMERIC-X
           MOVE W-POINT-ALPHANUMERIC-X    TO W-END-POINT-X
           MOVE W-POINT-ALPHANUMERIC-Y    TO W-END-POINT-Y
           ADD 1 TO W-END-POINT-X
           ADD 1 TO W-END-POINT-Y
      * DISPLAY, UNCOMMENT BELOW TO PRINT START AND END POINTS
      *    DISPLAY 'START: ' W-START-POINT-X ' BY ' W-START-POINT-Y
      *            ' END: '  W-END-POINT-X   ' BY ' W-END-POINT-Y
           CONTINUE.
       FIND-START-AND-END-POINTS-END.
           EXIT.

       FIND-INTERMEDIARY-POINTS SECTION.
           MOVE W-START-POINT TO W-INTERMEDIARY-POINT
      *    IF W-START-POINT-X IS EQUAL TO W-END-POINT-X OR
      *       W-START-POINT-Y IS EQUAL TO W-END-POINT-Y
      * FIRST, WE SAVE THE START POINT
              PERFORM ADD-INTERMEDIARY-POINT-TO-COUNTER
              PERFORM UNTIL W-INTERMEDIARY-POINT
              IS EQUAL TO W-END-POINT
                 IF W-END-POINT-X
                 IS LESS THAN W-INTERMEDIARY-POINT-X
                     SUBTRACT 1 FROM W-INTERMEDIARY-POINT-X
                     ON SIZE ERROR PERFORM ERROR-HANDLING
                 END-IF
                 IF W-END-POINT-X
                 IS GREATER THAN W-INTERMEDIARY-POINT-X
                     ADD 1 TO W-INTERMEDIARY-POINT-X
                     ON SIZE ERROR PERFORM ERROR-HANDLING
                 END-IF
                 IF W-END-POINT-Y
                 IS LESS THAN W-INTERMEDIARY-POINT-Y
                    SUBTRACT 1 FROM W-INTERMEDIARY-POINT-Y
                     ON SIZE ERROR PERFORM ERROR-HANDLING
                 END-IF
                 IF W-END-POINT-Y
                 IS GREATER THAN W-INTERMEDIARY-POINT-Y
                     ADD 1 TO W-INTERMEDIARY-POINT-Y
                     ON SIZE ERROR PERFORM ERROR-HANDLING
                 END-IF
                 PERFORM ADD-INTERMEDIARY-POINT-TO-COUNTER
              END-PERFORM
      *    END-IF
           CONTINUE.
       FIND-INTERMEDIARY-POINTS-END.
           EXIT.

       ADD-INTERMEDIARY-POINT-TO-COUNTER SECTION.
           SET W-COUNTER-POINTS-ROW-IX
            TO W-INTERMEDIARY-POINT-X
            SET W-COUNTER-POINTS-COLUMN-IX
            TO W-INTERMEDIARY-POINT-Y
           ADD 1 TO W-COUNTER-POINTS-XY(W-COUNTER-POINTS-ROW-IX,
                                        W-COUNTER-POINTS-COLUMN-IX)
      * UNCOMMENT BELOW TO PRINT ALL INTERMEDIARY POINTS.
      *    DISPLAY 'INTERMEDIARY POINT: ' W-INTERMEDIARY-POINT-X
      *            ' BY ' W-INTERMEDIARY-POINT-Y
      *            ' COUNT '
      *            W-COUNTER-POINTS-XY(W-COUNTER-POINTS-ROW-IX,
      *                                W-COUNTER-POINTS-COLUMN-IX)
           CONTINUE.
       ADD-POINT-TO-COUNTER-END.
           EXIT.

       PRINT-COUNTER-TABLE SECTION.
           MOVE ' TABLE OF COUNTED LINES: ' TO F-OUTREC
           PERFORM WRITE-OUTPUT-RECORD
           PERFORM WITH TEST AFTER VARYING W-COUNTER-POINTS-ROW-IX
           FROM 1 BY 1 UNTIL W-COUNTER-POINTS-ROW-IX
           IS EQUAL TO 1000
              PERFORM WITH TEST AFTER
              VARYING W-COUNTER-POINTS-COLUMN-IX
              FROM 1 BY 1 UNTIL W-COUNTER-POINTS-COLUMN-IX
              IS EQUAL TO 1000
                 MOVE W-COUNTER-POINTS-XY
                     (W-COUNTER-POINTS-ROW-IX,
                      W-COUNTER-POINTS-COLUMN-IX)
                 TO W-DISPLAY-XY(W-COUNTER-POINTS-COLUMN-IX)
              END-PERFORM
              MOVE W-DISPLAY-NUM TO F-OUTREC
              PERFORM WRITE-OUTPUT-RECORD
           END-PERFORM
           CONTINUE.
       PRINT-COUNTER-TABLE-END.
           EXIT.

       COUNT-MIN-TWO-LINES SECTION.
           PERFORM WITH TEST AFTER VARYING W-COUNTER-POINTS-ROW-IX
           FROM 1 BY 1 UNTIL W-COUNTER-POINTS-ROW-IX
           IS EQUAL TO 1000
              PERFORM WITH TEST AFTER
              VARYING W-COUNTER-POINTS-COLUMN-IX
              FROM 1 BY 1 UNTIL W-COUNTER-POINTS-COLUMN-IX
              IS EQUAL TO 1000
                 IF W-COUNTER-POINTS-XY(W-COUNTER-POINTS-ROW-IX,
                                        W-COUNTER-POINTS-COLUMN-IX)
                    IS GREATER THAN 1
                    ADD 1 TO W-TWO-LINES-MEET
                 END-IF
              END-PERFORM
           END-PERFORM

           CONTINUE.
       COUNT-MIN-TWO-LINES-END.
           EXIT.

       PRINT-STATS SECTION.
           DISPLAY '**************** BATCH RUN SUMMARY ****************'
           DISPLAY ' NUMBER OF RECORDS READ ==> '
                   W-INFILE-NUMBER-OF-RECORDS
           DISPLAY ' NUMBER OF RECORDS WRITTEN ==> '
                   W-OUTFILE-NUMBER-OF-RECORDS
           DISPLAY ' NUMBER OF INTERSECTION POINTS ==> '
                   W-TWO-LINES-MEET
           DISPLAY '***************************************************'
           CONTINUE.
       PRINT-STATS-END.
           EXIT.

       ERROR-HANDLING SECTION.
           DISPLAY 'SOMETHING WENT WRONG!'
           DISPLAY ' NUMBER OF RECORDS READ: '
                   W-INFILE-NUMBER-OF-RECORDS
                   ' FILE STATUS: '
                   W-INFILE-STATUS
           DISPLAY ' NUMBER OF RECORDS WRITTEN: '
                   W-OUTFILE-NUMBER-OF-RECORDS
                   ' FILE STATUS: '
                   W-OUTFILE-STATUS
           PERFORM CLOSE-INPUT-FILE
           PERFORM CLOSE-OUTPUT-FILE
           CONTINUE.
       ERROR-HANDLING-END.
           GOBACK.

       END PROGRAM LINEPGM.
