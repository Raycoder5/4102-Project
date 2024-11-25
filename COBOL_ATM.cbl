      * November/25/2024 || ITCS 4102-091 || COBOL Cobras
      * This program will demonstrate the basic capabilities of an ATM
      * for an imaginary bank called COBOL bank. 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL_ATM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "card.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TempAccountFile ASSIGN TO "temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  AccountFile.
       01  AccountData.
           02 AccountNumber       PIC 9(8).
           02 AccountName         PIC X(20).
           02 CardNumber          PIC 9(16).
           02 CVV                 PIC 999.
           02 ExpirationDate.
               03 MM              PIC 99.
               03 YY              PIC 9999.
           02 AccountBalance      PIC 9(10)V99.

       FD  TempAccountFile.
       01  TempAccountData.
           02 AccountNumber       PIC 9(8).
           02 AccountName         PIC X(20).
           02 CardNumber          PIC 9(16).
           02 CVV                 PIC 999.
           02 ExpirationDate.
               03 MM              PIC 99.
               03 YY              PIC 9999.
           02 AccountBalance      PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       01  WS-USER-INPUT           PIC X(20).
       01  WS-TRANSACTION-AMOUNT   PIC 9(10)V99.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-TEMP-BALANCE         PIC 9(10)V99.
       01  WS-ACCOUNT-NUMBER       PIC 9(8) VALUE 0.
       01  WS-SEARCH-NUMBER        PIC 9(8).
       01  WS-DEST-ACCOUNT-NUMBER  PIC 9(8).
       01  WS-TABLE-INDEX          PIC 9(3) VALUE 0.
       01  WS-TABLE-SIZE           PIC 9(3) VALUE 0.
       01  WS-SOURCE-INDEX         PIC 9(3) VALUE 0.
       01  WS-DEST-INDEX           PIC 9(3) VALUE 0.
       01  AccountTable.
           05 AccountEntry OCCURS 100 TIMES.
               10 AccAccountNumber     PIC 9(8).
               10 AccAccountName       PIC X(20).
               10 AccCardNumber        PIC 9(16).
               10 AccCVV               PIC 999.
               10 AccExpirationDate.
                   15 AccMM            PIC 99.
                   15 AccYY            PIC 9999.
               10 AccAccountBalance    PIC 9(10)V99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Welcome to COBOL Bank ATM System."
           PERFORM DISPLAY-MENU.

       DISPLAY-MENU.
           DISPLAY "1. Create Account"
           DISPLAY "2. Deposit Funds"
           DISPLAY "3. Withdraw Funds"
           DISPLAY "4. View Account"
           DISPLAY "5. Exit"
           DISPLAY "6. Transfer Funds"
           DISPLAY "7. Close Account"
           DISPLAY "8. Update Account Information"
           ACCEPT WS-USER-INPUT
           EVALUATE WS-USER-INPUT
               WHEN "1"
                   PERFORM CREATE-ACCOUNT
               WHEN "2"
                   PERFORM DEPOSIT-FUNDS
               WHEN "3"
                   PERFORM WITHDRAW-FUNDS
               WHEN "4"
                   PERFORM VIEW-ACCOUNT
               WHEN "5"
                   DISPLAY "Thank you for using COBOL Bank. Goodbye!"
                   STOP RUN
               WHEN "6"
                   PERFORM TRANSFER-FUNDS
               WHEN "7"
                   PERFORM CLOSE-ACCOUNT
               WHEN "8"
                   PERFORM UPDATE-ACCOUNT
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE
           PERFORM DISPLAY-MENU.

       SEARCH-ACCOUNT.
           MOVE "00" TO WS-FILE-STATUS
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO AccountData
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                      IF WS-SEARCH-NUMBER = AccountNumber OF AccountData
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM.

       PROMPT-FOR-ACCOUNT-NUMBER.
           DISPLAY "Enter your Account Number: "
           ACCEPT WS-SEARCH-NUMBER.

       DEPOSIT-FUNDS.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           DISPLAY "Enter the amount to deposit: "
           ACCEPT WS-TRANSACTION-AMOUNT
           OPEN I-O AccountFile
           PERFORM SEARCH-ACCOUNT
           IF WS-FILE-STATUS = "10"
               DISPLAY "Account not found."
           ELSE
              ADD WS-TRANSACTION-AMOUNT TO AccountBalance OF AccountData
               REWRITE AccountData
               IF WS-FILE-STATUS NOT = "00"
                   DISPLAY "Error during deposit. Please try again."
               ELSE
                 DISPLAY "Deposit successful. Your updated balance is: "
                   DISPLAY AccountBalance OF AccountData
               END-IF
           END-IF
           CLOSE AccountFile.

       WITHDRAW-FUNDS.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           DISPLAY "Enter the amount to withdraw: "
           ACCEPT WS-TRANSACTION-AMOUNT
           OPEN I-O AccountFile
           PERFORM SEARCH-ACCOUNT
           IF WS-FILE-STATUS = "10"
               DISPLAY "Account not found."
           ELSE
               IF AccountBalance OF AccountData < WS-TRANSACTION-AMOUNT
                   DISPLAY "Insufficient balance. Transaction declined."
               ELSE
                   SUBTRACT WS-TRANSACTION-AMOUNT FROM AccountBalance OF AccountData
                   REWRITE AccountData
                   IF WS-FILE-STATUS NOT = "00"
                       DISPLAY "Error during withdrawal. Please try again."
                   ELSE
                       DISPLAY "Withdrawal successful. Your updated balance is: "
                       DISPLAY AccountBalance OF AccountData
                   END-IF
               END-IF
           END-IF
           CLOSE AccountFile.

       VIEW-ACCOUNT.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           OPEN INPUT AccountFile
           PERFORM SEARCH-ACCOUNT
           IF WS-FILE-STATUS = "10"
               DISPLAY "Account not found."
           ELSE
               DISPLAY "Account Number: " AccountNumber OF AccountData
               DISPLAY "Account Name: " AccountName OF AccountData
               DISPLAY "Card Number: " CardNumber OF AccountData
               DISPLAY "Balance: $" AccountBalance OF AccountData
           END-IF
           CLOSE AccountFile.

       TRANSFER-FUNDS.
           DISPLAY "Enter your Account Number: "
           ACCEPT WS-SEARCH-NUMBER
           DISPLAY "Enter destination Account Number: "
           ACCEPT WS-DEST-ACCOUNT-NUMBER
           DISPLAY "Enter the amount to transfer: "
           ACCEPT WS-TRANSACTION-AMOUNT
           OPEN INPUT AccountFile
           MOVE 0 TO WS-TABLE-INDEX
           MOVE "00" TO WS-FILE-STATUS
           PERFORM VARYING WS-TABLE-INDEX FROM 1 BY 1
               UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO AccountData
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       MOVE AccountNumber OF AccountData       TO AccAccountNumber(WS-TABLE-INDEX)
                       MOVE AccountName OF AccountData         TO AccAccountName(WS-TABLE-INDEX)
                       MOVE CardNumber OF AccountData          TO AccCardNumber(WS-TABLE-INDEX)
                       MOVE CVV OF AccountData                 TO AccCVV(WS-TABLE-INDEX)
                       MOVE MM OF ExpirationDate OF AccountData TO AccMM(WS-TABLE-INDEX)
                       MOVE YY OF ExpirationDate OF AccountData TO AccYY(WS-TABLE-INDEX)
                       MOVE AccountBalance OF AccountData      TO AccAccountBalance(WS-TABLE-INDEX)
               END-READ
           END-PERFORM
           SUBTRACT 1 FROM WS-TABLE-INDEX GIVING WS-TABLE-SIZE
           CLOSE AccountFile

           MOVE 0 TO WS-SOURCE-INDEX
           MOVE 0 TO WS-DEST-INDEX

           PERFORM VARYING WS-TABLE-INDEX FROM 1 BY 1
               UNTIL WS-TABLE-INDEX > WS-TABLE-SIZE
               IF AccAccountNumber(WS-TABLE-INDEX) = WS-SEARCH-NUMBER
                   MOVE WS-TABLE-INDEX TO WS-SOURCE-INDEX
               END-IF
               IF AccAccountNumber(WS-TABLE-INDEX) = WS-DEST-ACCOUNT-NUMBER
                   MOVE WS-TABLE-INDEX TO WS-DEST-INDEX
               END-IF
           END-PERFORM

           IF WS-SOURCE-INDEX = 0
               DISPLAY "Source account not found."
               EXIT PARAGRAPH
           END-IF
           IF WS-DEST-INDEX = 0
               DISPLAY "Destination account not found."
               EXIT PARAGRAPH
           END-IF

           IF AccAccountBalance(WS-SOURCE-INDEX) < WS-TRANSACTION-AMOUNT
               DISPLAY "Insufficient balance. Transaction declined."
               EXIT PARAGRAPH
           END-IF

           SUBTRACT WS-TRANSACTION-AMOUNT FROM AccAccountBalance(WS-SOURCE-INDEX)
           ADD WS-TRANSACTION-AMOUNT TO AccAccountBalance(WS-DEST-INDEX)

           OPEN OUTPUT AccountFile
           PERFORM VARYING WS-TABLE-INDEX FROM 1 BY 1
               UNTIL WS-TABLE-INDEX > WS-TABLE-SIZE
               MOVE AccAccountNumber(WS-TABLE-INDEX)   TO AccountNumber OF AccountData
               MOVE AccAccountName(WS-TABLE-INDEX)     TO AccountName OF AccountData
               MOVE AccCardNumber(WS-TABLE-INDEX)      TO CardNumber OF AccountData
               MOVE AccCVV(WS-TABLE-INDEX)             TO CVV OF AccountData
               MOVE AccMM(WS-TABLE-INDEX)              TO MM OF ExpirationDate OF AccountData
               MOVE AccYY(WS-TABLE-INDEX)              TO YY OF ExpirationDate OF AccountData
               MOVE AccAccountBalance(WS-TABLE-INDEX)  TO AccountBalance OF AccountData
               WRITE AccountData
           END-PERFORM
           CLOSE AccountFile

           DISPLAY "Transfer successful. Your new balance is: "
           DISPLAY AccAccountBalance(WS-SOURCE-INDEX).

       CLOSE-ACCOUNT.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           OPEN INPUT AccountFile
           OPEN OUTPUT TempAccountFile
           MOVE "00" TO WS-FILE-STATUS
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO AccountData
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       IF AccountNumber OF AccountData NOT = WS-SEARCH-NUMBER
                           WRITE TempAccountData FROM AccountData
                       ELSE
                           DISPLAY "Account closed successfully."
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AccountFile
           CLOSE TempAccountFile
           CALL "SYSTEM" USING "DEL card.dat"
           CALL "SYSTEM" USING "RENAME temp.dat card.dat".

       UPDATE-ACCOUNT.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           OPEN INPUT AccountFile
           OPEN OUTPUT TempAccountFile
           MOVE "00" TO WS-FILE-STATUS
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO AccountData
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       IF AccountNumber OF AccountData = WS-SEARCH-NUMBER
                           DISPLAY "Enter new Account Name: "
                           ACCEPT AccountName OF AccountData
                           DISPLAY "Enter new Card Number: "
                           ACCEPT CardNumber OF AccountData
                           DISPLAY "Enter new CVV: "
                           ACCEPT CVV OF AccountData
                           DISPLAY "Enter new Expiration Date (MM): "
                           ACCEPT MM OF ExpirationDate OF AccountData
                           DISPLAY "Enter new Expiration Date (YY): "
                           ACCEPT YY OF ExpirationDate OF AccountData
                           DISPLAY "Account information updated successfully."
                       END-IF
                       WRITE TempAccountData FROM AccountData
               END-READ
           END-PERFORM
           CLOSE AccountFile
           CLOSE TempAccountFile
           CALL "SYSTEM" USING "DEL card.dat"
           CALL "SYSTEM" USING "RENAME temp.dat card.dat".

       CREATE-ACCOUNT.
           ADD 1 TO WS-ACCOUNT-NUMBER
           DISPLAY "Your Account Number is: " WS-ACCOUNT-NUMBER
           MOVE WS-ACCOUNT-NUMBER TO AccountNumber OF AccountData
           DISPLAY "Enter Account Name: "
           ACCEPT AccountName OF AccountData
           DISPLAY "Enter Card Number: "
           ACCEPT CardNumber OF AccountData
           DISPLAY "Enter CVV: "
           ACCEPT CVV OF AccountData
           DISPLAY "Enter Expiration Date (MM): "
           ACCEPT MM OF ExpirationDate OF AccountData
           DISPLAY "Enter Expiration Date (YY): "
           ACCEPT YY OF ExpirationDate OF AccountData
           DISPLAY "Enter Initial Balance: "
           ACCEPT AccountBalance OF AccountData
           OPEN EXTEND AccountFile
           WRITE AccountData
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error creating account. Please try again."
               CLOSE AccountFile
               STOP RUN
           END-IF
           CLOSE AccountFile
           DISPLAY "Account successfully created.".
