       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATCHING-PROGRAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  INPUT-FILE-1     ASSIGN TO "INPUTFILE1.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  INPUT-FILE-2     ASSIGN TO "INPUTFILE2.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUTPUT-FILE-1    ASSIGN TO "OUTPUTFILE1.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUTPUT-FILE-2    ASSIGN TO "OUTPUTFILE2.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  OUTPUT-FILE-3    ASSIGN TO "OUTPUTFILE3.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD INPUT-FILE-1.
              01 INPUTRECORD1            PIC X(80).
           FD INPUT-FILE-2.
              01 INPUTRECORD2            PIC X(80).

           FD OUTPUT-FILE-1.
              01 OUTPUTRECORD1           PIC X(80).
           FD OUTPUT-FILE-2.
              01 OUTPUTRECORD2           PIC X(80).
           FD OUTPUT-FILE-3.
              01 OUTPUTRECORD3           PIC X(80).
       WORKING-STORAGE SECTION.
       01  WK-KEY-VALUE                  PIC X(8).
       01  WK-INPUT1-REC.
           05 WK-INPUT1-KEY              PIC X(8).
           05 FILLER                     PIC X(72).
       01  WK-INPUT2-REC.
           05 WK-INPUT2-KEY              PIC X(8).
           05 FILLER                     PIC X(72).
       01  WK-EOF-FL1                    PIC X(1)     VALUE  "N".
       01  WK-EOF-FL2                    PIC X(1)     VALUE  "N".

       PROCEDURE DIVISION.
       MAIN.
           PERFORM  INITIAL-RTN.
           PERFORM  MATCH-RTN.
           PERFORM  FINAL-RTN.

           STOP RUN.
       MAIN-EXIT.

       INITIAL-RTN                      SECTION.
           OPEN INPUT                   INPUT-FILE-1
                                        INPUT-FILE-2.
           OPEN OUTPUT                  OUTPUT-FILE-1
                                        OUTPUT-FILE-2
                                        OUTPUT-FILE-3.
           PERFORM READ-FL1-RTN.
           PERFORM READ-FL2-RTN.

       INITIAL-RTN-EXIT.
       EXIT.

       MATCH-RTN                        SECTION.
           PERFORM UNTIL (WK-EOF-FL1 = "Y")  AND
                         (WK-EOF-FL2 = "Y")
             IF  WK-INPUT1-KEY  <  WK-INPUT2-KEY  THEN
                 WRITE    OUTPUTRECORD1      FROM  WK-INPUT1-REC
                 PERFORM  READ-FL1-RTN
             ELSE
                 IF  WK-INPUT1-KEY  =  WK-INPUT2-KEY  THEN
                     WRITE    OUTPUTRECORD3  FROM  WK-INPUT1-REC
                     PERFORM  READ-FL1-RTN
                     PERFORM  READ-FL2-RTN
                 ELSE
                     WRITE    OUTPUTRECORD2  FROM  WK-INPUT2-REC
                     PERFORM  READ-FL2-RTN
                 END-IF
             END-IF
           END-PERFORM.
       MATCH-RTN-EXIT.
       EXIT.

       FINAL-RTN                        SECTION.
           CLOSE                        INPUT-FILE-1
                                        INPUT-FILE-2
                                        OUTPUT-FILE-1
                                        OUTPUT-FILE-2
                                        OUTPUT-FILE-3.
       FINAL-RTN-EXIT.
       EXIT.

       READ-FL1-RTN                     SECTION.
           READ  INPUT-FILE-1           INTO  WK-INPUT1-REC
                AT END  MOVE  "Y"       TO    WK-EOF-FL1.

           IF WK-EOF-FL1  =  "Y"  THEN
              MOVE  HIGH-VALUE          TO    WK-INPUT1-KEY
           END-IF.

       READ-FL1-RTN-EXIT.
       EXIT.

       READ-FL2-RTN                     SECTION.
           READ  INPUT-FILE-2           INTO  WK-INPUT2-REC
                AT END  MOVE  "Y"       TO    WK-EOF-FL2.

           IF WK-EOF-FL2  =  "Y"  THEN
              MOVE  HIGH-VALUE          TO    WK-INPUT2-KEY
           END-IF.

       READ-FL2-RTN-EXIT.
       EXIT.
