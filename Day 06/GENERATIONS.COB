      ******************************************************************
      * Author:    MARIA ASPVIK
      * Date:      2022-08-06
      * Purpose:   JUST FOR FUN
      * Tectonics: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERATIONS.
       AUTHOR. MARIA ASPVIK.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT INPUT-FILE ASSIGN TO "../TEST_INPUT.TXT"
           SELECT INPUT-FILE ASSIGN TO "../REAL_INPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS W-INPUT-FILE-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO "../OUTPUT.TXT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS W-OUTPUT-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 F-INPUT-FILE-RECORD                PIC X(9999).
       FD OUTPUT-FILE.
       01 F-OUTPUT-FILE-RECORD               PIC X(201).
      *
       WORKING-STORAGE SECTION.
      * STATUS FIELDS
       01 W-INPUT-FILE-STATUS                PIC XX.
         88 W-INPUT-FILE-OK                               VALUE '00'.
         88 W-INPUT-FILE-EOF                              VALUE '10'.
      *
       01 W-INPUT-FILE-MODE                  PIC X.
         88 W-INPUT-FILE-OPEN                             VALUE 'O'.
         88 W-INPUT-FILE-CLOSED                           VALUE 'C'.
         88 W-INPUT-FILE-ERROR                            VALUE 'E'.
      *
       01 W-OUTPUT-FILE-STATUS               PIC XX.
         88 W-OUTPUT-FILE-OK                              VALUE '00'.
         88 W-OUTPUT-FILE-EOF                             VALUE '10'.
      *
       01 W-OUTPUT-FILE-MODE                 PIC X.
         88 W-OUTPUT-FILE-OPEN                            VALUE 'O'.
         88 W-OUTPUT-FILE-CLOSED                          VALUE 'C'.
         88 W-OUTPUT-FILE-ERROR                           VALUE 'E'.
      *
       01 W-ERROR-MESSAGE.
         05 W-ERROR-CODE                     PIC 99.
            88 W-NO-ERROR                                 VALUE ZERO.
         05 FILLER                           PIC X(03)    VALUE ' - '.
         05 W-ERROR-TEXT                     PIC X(80)    VALUE SPACE.
      *
      * COUNTER FIELDS
       01 W-DAY-COUNTER                      PIC 9(04)    VALUE ZERO.
      *
       01 W-FISH-COUNTERS.
          05 W-FISH-COUNTER                  OCCURS 10 TIMES
                                             INDEXED BY
                                             W-FISH-COUNTER-IX
                                             COMP-5
                                             PIC 9(17)    VALUE ZERO.
          05 W-FISH-TOTAL                    COMP-5
                                             PIC 9(18)    VALUE ZERO.
      *
       01 W-INPUT-STRING-SIZE                PIC 9(06)    VALUE ZERO.
       01 W-INPUT-STRING-IX                  PIC 9(06)    VALUE ZERO.
      *
      * DISPLAY FIELDS
       01 W-DISPLAY-FISH-HEADER.
         05 FILLER                           PIC X(04)    VALUE 'DAY:'.
         05 FILLER                           PIC X(13)    VALUE SPACE.
         05 FILLER                           PIC X(18)    VALUE '0 DAY'.
         05 FILLER                           PIC X(18)    VALUE '1 DAY'.
         05 FILLER                           PIC X(18)    VALUE '2 DAY'.
         05 FILLER                           PIC X(18)    VALUE '3 DAY'.
         05 FILLER                           PIC X(18)    VALUE '4 DAY'.
         05 FILLER                           PIC X(18)    VALUE '5 DAY'.
         05 FILLER                           PIC X(18)    VALUE '6 DAY'.
         05 FILLER                           PIC X(18)    VALUE '7 DAY'.
         05 FILLER                           PIC X(18)    VALUE '8 DAY'.
         05 FILLER                           PIC X        VALUE SPACE.
         05 FILLER                           PIC X(20)    VALUE 'TOTAL'.
         05 FILLER                           PIC X        VALUE X'0A'.
       01 W-DISPLAY-FISH-ROW.
         05 W-DISPLAY-DAYS                   PIC Z(03)9.
         05 W-DISPLAY-FISH-COUNTER           OCCURS 9 TIMES
                                             INDEXED BY
                                             W-DISPLAY-FISH-COUNTER-IX
                                             PIC Z(17)9.
         05 W-DISPLAY-FISH-TOTAL             PIC Z(18)9.
         05 FILLER                           PIC X(15)    VALUE SPACE.
         05 FILLER                           PIC X        VALUE X'0A'.
      *
      * CONSTANTS
      * PUZZLE PART 1 USED 80 DAYS, PART 2 USED 256 DAYS.
       01 W-MAX-DAY                          COMP-5
      *                                      PIC 9(09)    VALUE 80.
                                             PIC 9(09)    VALUE 256.
       01 LF                                 PIC X        VALUE X'0A'.
       01 CR                                 PIC X        VALUE X'0D'.
      * TEMPORARY DATA ITEMS
       01 W-TMP-FISH-COUNTER1                COMP-5
                                             PIC 9(17)    VALUE ZERO.
       01 W-TMP-FISH-COUNTER2                COMP-5
                                             PIC 9(17)    VALUE ZERO.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE SECTION.
           PERFORM OPEN-INPUT
           PERFORM INIT
           PERFORM CLOSE-INPUT
           PERFORM OPEN-OUTPUT
           PERFORM WRITE-HEADER
           PERFORM WRITE-LINE
               VARYING W-DAY-COUNTER
               FROM 0 BY 1
               UNTIL W-DAY-COUNTER IS GREATER THAN W-MAX-DAY
           PERFORM CLOSE-OUTPUT
           CONTINUE.
       MAIN-PROCEDURE-END.
           GOBACK.

       INIT SECTION.
           MOVE ZERO TO W-ERROR-CODE
           INITIALIZE W-DAY-COUNTER
                      W-FISH-COUNTERS
                      W-INPUT-STRING-SIZE.
           READ INPUT-FILE NEXT RECORD
           IF NOT W-INPUT-FILE-OK
               SET W-INPUT-FILE-ERROR TO TRUE
               MOVE 31 TO W-ERROR-CODE
               MOVE 'ERROR WHILE READING INPUT RECORD'
                   TO W-ERROR-TEXT
               PERFORM ERROR-HANDLING
           END-IF
           INSPECT F-INPUT-FILE-RECORD
               TALLYING W-INPUT-STRING-SIZE
               FOR ALL CHARACTERS
           PERFORM VARYING W-INPUT-STRING-IX
               FROM 1 BY 1
               UNTIL W-INPUT-STRING-IX EQUAL W-INPUT-STRING-SIZE
                   IF F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1) EQUAL ','
                   OR F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1) EQUAL ' '
                   OR F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1) EQUAL CR
                   OR F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1) EQUAL LF
                       CONTINUE
                   ELSE
                       IF F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1)
                           IS NUMERIC
                           SET W-FISH-COUNTER-IX
                           TO F-INPUT-FILE-RECORD(W-INPUT-STRING-IX:1)
                           SET W-FISH-COUNTER-IX UP BY 1
                           ADD 1 TO W-FISH-COUNTER(W-FISH-COUNTER-IX)
                       ELSE
                           MOVE 32 TO W-ERROR-CODE
                           STRING 'NON-NUMERIC INPUT CHAR DETECTED!'
                                   ' CHAR: '
                                   F-INPUT-FILE-RECORD(
                                        W-INPUT-STRING-IX:1)
                                   INTO W-ERROR-TEXT
                           PERFORM ERROR-HANDLING
                       END-IF
                   END-IF
           END-PERFORM
           CONTINUE.
       INIT-END.
           EXIT.

       WRITE-HEADER SECTION.
           MOVE SPACE TO F-OUTPUT-FILE-RECORD
           MOVE W-DISPLAY-FISH-HEADER TO F-OUTPUT-FILE-RECORD
           WRITE F-OUTPUT-FILE-RECORD
           IF NOT W-OUTPUT-FILE-OK
               SET W-OUTPUT-FILE-ERROR TO TRUE
               MOVE 41 TO W-ERROR-CODE
               MOVE 'WRITE OF HEADER FAILED' TO W-ERROR-TEXT
               PERFORM ERROR-HANDLING
           END-IF
           CONTINUE.
       DISPLAY-FISH-END.
           EXIT.

       WRITE-LINE SECTION.
           PERFORM PREPARE-FISH-ROW
           MOVE SPACE TO F-OUTPUT-FILE-RECORD
           MOVE W-DISPLAY-FISH-ROW TO F-OUTPUT-FILE-RECORD
           WRITE F-OUTPUT-FILE-RECORD
           IF NOT W-OUTPUT-FILE-OK
               MOVE 51 TO W-ERROR-CODE
               STRING 'WRITE OF LINE FOR DAY NUMBER '
                      W-DAY-COUNTER
                      ' FAILED'
                      INTO W-ERROR-TEXT
           END-IF
           PERFORM CALCULATE-NEXT-DAY
           CONTINUE.
       WRITE-LINE-END.
           EXIT.

       PREPARE-FISH-ROW SECTION.
           INITIALIZE W-FISH-TOTAL
           PERFORM
               WITH TEST AFTER
               VARYING W-DISPLAY-FISH-COUNTER-IX
               FROM 1 BY 1
               UNTIL W-DISPLAY-FISH-COUNTER-IX EQUAL 9
                   MOVE W-FISH-COUNTER(W-DISPLAY-FISH-COUNTER-IX)
                   TO W-DISPLAY-FISH-COUNTER(W-DISPLAY-FISH-COUNTER-IX)
                   ADD W-FISH-COUNTER(W-DISPLAY-FISH-COUNTER-IX)
                   TO W-FISH-TOTAL
           END-PERFORM
           MOVE W-FISH-TOTAL TO W-DISPLAY-FISH-TOTAL
           MOVE W-DAY-COUNTER TO W-DISPLAY-DAYS
           CONTINUE.
       PREPARE-FISH-ROW-END.
           EXIT.

       CALCULATE-NEXT-DAY SECTION.
           SET W-FISH-COUNTER-IX TO 1
           MOVE W-FISH-COUNTER(W-FISH-COUNTER-IX) TO W-TMP-FISH-COUNTER1
           PERFORM
               WITH TEST AFTER
               VARYING W-FISH-COUNTER-IX
               FROM 1 BY 1
               UNTIL W-FISH-COUNTER-IX EQUAL 9
                   SET W-FISH-COUNTER-IX UP BY 1
                   MOVE W-FISH-COUNTER(W-FISH-COUNTER-IX)
                   TO W-TMP-FISH-COUNTER2
                   SET W-FISH-COUNTER-IX DOWN BY 1
                   MOVE W-TMP-FISH-COUNTER2
                   TO W-FISH-COUNTER(W-FISH-COUNTER-IX)
           END-PERFORM
           SET W-FISH-COUNTER-IX TO 9
           MOVE W-TMP-FISH-COUNTER1 TO W-FISH-COUNTER(W-FISH-COUNTER-IX)
           SET W-FISH-COUNTER-IX TO 7
           ADD W-TMP-FISH-COUNTER1 TO W-FISH-COUNTER(W-FISH-COUNTER-IX)
           ON SIZE ERROR PERFORM FISH-ADD-ERROR
           CONTINUE.
       CALULATE-NEXT-DAY-END.
           EXIT.

       FISH-ADD-ERROR SECTION.
           MOVE 53 TO W-ERROR-CODE
           STRING 'NUMBER IS TOO BIG TO ADD: '
                  W-TMP-FISH-COUNTER1
                  ' + '
                  W-FISH-COUNTER(W-FISH-COUNTER-IX)
                  INTO W-ERROR-TEXT
           PERFORM ERROR-HANDLING
           CONTINUE.
       FISH-ADD-ERROR-END.
           EXIT.

       OPEN-INPUT SECTION.
           OPEN INPUT INPUT-FILE
           IF NOT W-INPUT-FILE-OK
               SET W-INPUT-FILE-ERROR TO TRUE
               MOVE 11 TO W-ERROR-CODE
               MOVE 'ERROR WHILE OPENING INPUT FILE' TO W-ERROR-TEXT
               PERFORM ERROR-HANDLING
           END-IF
           SET W-INPUT-FILE-OPEN TO TRUE
           CONTINUE.
       OPEN-INPUT-END.
           EXIT.

       CLOSE-INPUT SECTION.
           IF W-INPUT-FILE-OPEN
               CLOSE INPUT-FILE
               IF NOT W-INPUT-FILE-OK
                   SET W-INPUT-FILE-ERROR TO TRUE
                   MOVE 12 TO W-ERROR-CODE
                   MOVE 'ERROR WHILE CLOSING INPUT FILE'
                       TO W-ERROR-TEXT
                   PERFORM ERROR-HANDLING
               END-IF
               SET W-INPUT-FILE-CLOSED TO TRUE
           END-IF
           CONTINUE.
       CLOSE-INPUT-END.
           EXIT.

       OPEN-OUTPUT SECTION.
           OPEN OUTPUT OUTPUT-FILE
           IF NOT W-OUTPUT-FILE-OK
               SET W-OUTPUT-FILE-ERROR TO TRUE
               MOVE 21 TO W-ERROR-CODE
               MOVE 'ERROR WHILE OPENING OUTPUT FILE' TO W-ERROR-TEXT
               PERFORM ERROR-HANDLING
           END-IF
           SET W-OUTPUT-FILE-OPEN TO TRUE
           CONTINUE.
       OPEN-OUTPUT-END.
           EXIT.

       CLOSE-OUTPUT SECTION.
           IF W-OUTPUT-FILE-OPEN
               CLOSE OUTPUT-FILE
               IF NOT W-OUTPUT-FILE-OK
                   SET W-OUTPUT-FILE-ERROR TO TRUE
                   MOVE 22 TO W-ERROR-CODE
                   MOVE 'ERROR WHILE CLOSING OUTPUT FILE'
                       TO W-ERROR-TEXT
                   PERFORM ERROR-HANDLING
               END-IF
               SET W-OUTPUT-FILE-CLOSED TO TRUE
           END-IF
           CONTINUE.
       CLOSE-OUTPUT-END.
           EXIT.

       ERROR-HANDLING SECTION.
           DISPLAY 'SOMETHING WENT WRONG!'
           DISPLAY W-ERROR-MESSAGE
           IF W-INPUT-FILE-ERROR
               DISPLAY 'INPUT FILE ERROR, FILE CODE IS: '
               W-INPUT-FILE-STATUS
               CLOSE INPUT-FILE
           END-IF
           IF W-OUTPUT-FILE-ERROR
               DISPLAY 'OUTPUT FILE ERROR, FILE CODE IS: '
               W-OUTPUT-FILE-STATUS
               CLOSE OUTPUT-FILE
           END-IF
           IF W-INPUT-FILE-OPEN
               CLOSE INPUT-FILE
           END-IF
           IF W-OUTPUT-FILE-OPEN
               CLOSE OUTPUT-FILE
           END-IF
           CONTINUE.
       ERROR-HANDLING-END.
           GOBACK.


       END PROGRAM GENERATIONS.
