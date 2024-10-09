      * This program will demonstrate COBOL's major data structures
      * and control structures.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. dataStructure.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Data structure 1 (Strings)
       01 USER-BALANCE   PIC X(20) VALUE '$10000'.
       01 USER-INPUT   PIC X(20).
      * Data structure 2 (Records)
       01 WS-BANKINFO.
           05 WS-CARDNUMBER PIC 9(19) VALUE '8395 1384 0335 6297'.
           05 WS-NAME  PIC A(8) VALUE 'John Doe'.
       PROCEDURE DIVISION.
           DISPLAY "Would you like to see your current balance? (Y/N)".
           ACCEPT USER-INPUT.
           DISPLAY " ".
           DISPLAY "You chose: " USER-INPUT.
           DISPLAY " ".
      
      * Control structure 1 (While loop)     
           PERFORM UNTIL USER-INPUT = 'Y' OR USER-INPUT = 'N'
              DISPLAY "ERROR: Enter Y or N"
           DISPLAY "Would you like to see your current balance? (Y/N)"
              ACCEPT USER-INPUT
              DISPLAY " "
              DISPLAY "You chose: " USER-INPUT
              DISPLAY " "
           END-PERFORM.
      
      * Control structure 2 (IF-ELSE statement)
           IF USER-INPUT = 'Y'
              THEN
                 DISPLAY "CARD NUMBER : " WS-CARDNUMBER
                 DISPLAY "NAME : " WS-NAME
                 DISPLAY "BALANCE : " USER-BALANCE
           ELSE
              DISPLAY " "
              DISPLAY "Have a nice day!"
              DISPLAY " "
           END-IF. 
           STOP RUN.
       