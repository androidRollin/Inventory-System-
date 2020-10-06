       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES.
      *INSTALLATION. S500.
      *DATE-WRITTEN. OCTOBER 7,2019.
      *DATE-COMPILED. OCTOBER 7,2019.
      *SECURITY. EXCLUSIVE FOR SIR BABY BOY.
      *REMARKS. FINALS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO "C:\FCOB\STORAGE.TXT".
           SELECT OUTFILE ASSIGN TO "C:\FCOB\RESULT.TXT".
           SELECT TOTFILE ASSIGN TO "C:\FCOB\TOTS.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 INREC.
           05 PRODUCTCODE PIC X(7).
           05 PRODUCTNAME PIC A(15).
           05 QUANTITY PIC 9(3).
           05 PRICE PIC 9(5).
       FD OUTFILE.
       01 OUTREC.
           05 PCODE PIC X(7).
           05 PNAME PIC A(15).
           05 QTY PIC ZZ9.
           05 P PIC ZZ,999.
       FD TOTFILE.
       01 TOT-REC.
           05 FILLER PIC X(80).    
       WORKING-STORAGE SECTION.
       01 HDG1.
           05 FILLER PIC X(32).
           05 FILLER PIC X(16) VALUE "SALES  INVENTORY".
           05 FILLER PIC X(32).    
       01 HDG2.
           05 FILLER PIC X(16).
           05 FILLER PIC X(5) VALUE "CODE ".
           05 FILLER PIC X(8).
           05 FILLER PIC X(14) VALUE "PRODUCT  NAME ".
           05 FILLER PIC X(7).
           05 FILLER PIC X(3) VALUE "QTY".
           05 FILLER PIC X(6).
           05 FILLER PIC X(5) VALUE "PRICE".   
           05 FILLER PIC X(16).    
       01 DISP-REC.
           05 FILLER PIC X(16).
           05 DPCODE PIC X(7).
           05 FILLER PIC X(6).
           05 DPNAME PIC A(15).
           05 FILLER PIC X(6).
           05 DQTY PIC ZZ9.
           05 FILLER PIC X(6).
           05 DP PIC ZZ,999.   
           05 FILLER PIC X(16).
       01 ASSETS.
           05 FILLER PIC X(20).
           05 FILLER PIC X(13) VALUE "RECORDS NUM: ".
           05 A-COUNT PIC ZZ9.
           05 FILLER PIC X(44).
           05 FILLER PIC X(80).
           05 FILLER PIC X(20).
           05 FILLER PIC X(11) VALUE "TOTAL QTY: ".
           05 A-QNUM PIC ZZZ999.
           05 FILLER PIC X(43).
           05 FILLER PIC X(80).
           05 FILLER PIC X(20).
           05 FILLER PIC X(13) VALUE "ASSET(PESO): ".
           05 A-TPRICE PIC ZZ,ZZ9,999.
           05 FILLER PIC X(39).
           05 FILLER PIC X(80). 
       01 NOTFOUND.
           05 FILLER PIC X(15).
           05 FILLER PIC X(22) VALUE " PRODUCT NOT AVAILABLE".
           05 FILLER PIC X(3).
       01 INCODE PIC X(7).
       01 BLNK-HDG.
           05 FILLER PIC X(80).
       01 EOF PIC A(3) VALUE "NO".
       01 ANS PIC A VALUE "Y".
       01 DEL PIC X(7) VALUE SPACES.
       01 OTHER-DETAILS.
           05 CHOICE PIC 9.
           05 NWPROD PIC X(7) VALUE SPACES.
           05 NWNAM PIC X(15) VALUE SPACES.
           05 NWQTY PIC 9(3) VALUE ZEROES.
           05 NWPRI PIC 9(5) VALUE ZEROES.
           05 EOF1 PIC X(3) VALUE "NO".
       01 RECORD-TOT.
           05 COUNTS PIC 999 VALUE ZEROES.
           05 QNUM PIC 9(6) VALUE ZEROES.
           05 T-PRICE PIC 9(8) VALUE ZEROES.     
       SCREEN SECTION.   
       01 SCR.
           05 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM CHOICE-RTN.
           DISPLAY SCR.
           DISPLAY HDG1
                   HDG2.
           IF CHOICE = 1
           PERFORM FIRST-CHOICE
           ELSE IF CHOICE = 2
           PERFORM SECOND-CHOICE
           ELSE IF CHOICE = 3
           PERFORM THIRD-CHOICE
           ELSE 
           DISPLAY SCR
           PERFORM CHOICE-RTN.
           STOP RUN.
       CHOICE-RTN.
           DISPLAY SCR.
           DISPLAY (5, 35) "SALES INVENTORY".
           DISPLAY (7, 10) "[1] SEARCH FOR A RECORD".
           DISPLAY (8, 10) "[2] ADD A NEW RECORD"
           DISPLAY (9, 10) "[3] SHOW TOTAL ASSETS" 
           DISPLAY (11, 10) "INPUT: "
           ACCEPT (11, 17) CHOICE.
           DISPLAY SCR.    
       FIRST-CHOICE.   
           OPEN INPUT INFILE
           OUTPUT OUTFILE.
           READ INFILE AT END MOVE "YES" TO EOF.
           PERFORM INPUT-RTN.
           PERFORM PROC-RTN UNTIL EOF IS EQUAL TO "YES".
           PERFORM NTFOUND. 
           CLOSE INFILE, OUTFILE.
       SECOND-CHOICE.
           OPEN EXTEND INFILE.
           PERFORM EDIT-RTN.
           CLOSE INFILE.         
       THIRD-CHOICE.
           OPEN INPUT INFILE
           OUTPUT TOTFILE.
           READ INFILE AT END MOVE "YES" TO EOF.
           PERFORM ADD-RTN UNTIL EOF IS EQUAL TO "YES".
           MOVE QNUM TO A-QNUM.
           MOVE COUNTS TO A-COUNT.
           MOVE T-PRICE TO A-TPRICE.
           WRITE TOT-REC FROM ASSETS AFTER ADVANCING 3.
           DISPLAY SCR.
           DISPLAY HDG1
                   BLNK-HDG.
           DISPLAY ASSETS.
       ADD-RTN. 
           ADD 1 TO COUNTS.
           ADD QUANTITY TO QNUM. 
           ADD PRICE TO T-PRICE.
           READ INFILE AT END MOVE "YES" TO EOF.
       EDIT-RTN.
           DISPLAY (5, 20) "ENTER CODE:".
           ACCEPT (5, 35) NWPROD.
           DISPLAY (7, 20) "ENTER PRODUCT NAME:".
           ACCEPT (7, 40) NWNAM.
           DISPLAY (9, 20) "ENTER QTY:".
           ACCEPT (9, 35) NWQTY.
           DISPLAY (11, 20) "ENTER PRICE:".
           ACCEPT (11, 35) NWPRI.
           MOVE NWPROD TO PRODUCTCODE.
           MOVE NWNAM TO PRODUCTNAME.
           MOVE NWQTY TO QUANTITY.
           MOVE NWPRI TO PRICE. 
           WRITE INREC. 
       DISPLAY-ALL-RTN.
           READ INFILE AT END MOVE "YES" TO EOF.
           PERFORM MOVE-RTN.
           DISPLAY DISP-REC.
       INPUT-RTN.
           DISPLAY SCR.
           DISPLAY (2, 35) "SALES INVENTORY".
           DISPLAY (5, 10) "ENTER PRODUCT CODE:".
           ACCEPT (5, 30) INCODE.
           DISPLAY SCR.
       PROC-RTN.    
           IF INCODE IS EQUAL TO PRODUCTCODE
            PERFORM MOVE-RTN
            WRITE OUTREC
            DISPLAY HDG1
                    HDG2
            DISPLAY DISP-REC
            CLOSE INFILE
            STOP RUN
           ELSE 
            READ INFILE AT END MOVE "YES" TO EOF
            END-READ.          
       NTFOUND.
           DISPLAY NOTFOUND. 
       MOVE-RTN.   
            MOVE PRODUCTNAME TO PNAME.
            MOVE PRODUCTCODE TO PCODE.
            MOVE QUANTITY TO QTY.
            MOVE PRICE TO P.
            MOVE PRODUCTNAME TO DPNAME.
            MOVE PRODUCTCODE TO DPCODE.
            MOVE QUANTITY TO DQTY.
            MOVE PRICE TO DP.
                   
