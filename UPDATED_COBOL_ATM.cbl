      * November/25/2024 || ITCS 4102-091 || COBOL Cobras
      * This program will demonstrate the basic capabilities of an ATM
      * for an imaginary bank called COBOL bank. 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDATED_COBOL_ATM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "card.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD AccountFile.
       01 AccountData.
           02 AccountNumber     PIC 9(8).
           02 AccountName       PIC X(20).
           02 CardNumber        PIC 9(16).
           02 CVV              PIC 999.
           02 ExpirationDate.
              03 MM            PIC 99.
              03 YY            PIC 9999.
           02 AccountBalance    PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       01 WS-USER-INPUT         PIC X(20).
       01 WS-TRANSACTION-AMOUNT PIC 9(10)V99.
       01 WS-FILE-STATUS        PIC XX.
       01 WS-TEMP-BALANCE       PIC 9(10)V99.
       01 WS-ACCOUNT-NUMBER     PIC 9(8) VALUE 0.
       01 WS-SEARCH-NUMBER      PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Welcome to COBOL Bank ATM System."
           PERFORM DISPLAY-MENU.
      * Display the menu for the user to select each of the functions.
       DISPLAY-MENU.
           DISPLAY "1. Create Account"
           DISPLAY "2. Deposit Funds"
           DISPLAY "3. Withdraw Funds"
           DISPLAY "4. View Account"
           DISPLAY "5. Exit"
      * Get the user's input from the menu for the EVALUATE section.    
           ACCEPT WS-USER-INPUT
           EVALUATE WS-USER-INPUT
      * Run the CREATE-ACCOUNT function when the user inputs 1.        
               WHEN "1"
                   PERFORM CREATE-ACCOUNT
      * Run the DEPOSIT-FUNDS function when the user inputs 2.        
               WHEN "2"
                   PERFORM DEPOSIT-FUNDS
      * Run the WITHDRAW-FUNDS function when the user inputs 3.        
               WHEN "3"
                   PERFORM WITHDRAW-FUNDS
      * Run the VIEW-ACCOUNT function when the user inputs 4.        
               WHEN "4"
                   PERFORM VIEW-ACCOUNT
      * Display a goodbye message when the user inputs 5.        
               WHEN "5"
                   DISPLAY "Thank you for using COBOL Bank. Goodbye!"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid option. Please try again."
           END-EVALUATE
           PERFORM DISPLAY-MENU.

       CREATE-ACCOUNT.
           ADD 1 TO WS-ACCOUNT-NUMBER
           DISPLAY "Your Account Number is: " WS-ACCOUNT-NUMBER
           MOVE WS-ACCOUNT-NUMBER TO AccountNumber
           DISPLAY "Enter Account Name: "
           ACCEPT AccountName
           DISPLAY "Enter Card Number: "
           ACCEPT CardNumber
           DISPLAY "Enter CVV: "
           ACCEPT CVV
           DISPLAY "Enter Expiration Date (MM): "
           ACCEPT MM
           DISPLAY "Enter Expiration Date (YY): "
           ACCEPT YY
           DISPLAY "Enter Initial Balance: "
           ACCEPT AccountBalance
           OPEN EXTEND AccountFile
           WRITE AccountData
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Error creating account. Please try again."
           ELSE
               DISPLAY "Account successfully created."
           END-IF
           CLOSE AccountFile.

       DEPOSIT-FUNDS.
           PERFORM PROMPT-FOR-ACCOUNT-NUMBER
           DISPLAY "Enter the amount to deposit: "
           ACCEPT WS-TRANSACTION-AMOUNT
           OPEN I-O AccountFile
               PERFORM SEARCH-ACCOUNT
               IF WS-FILE-STATUS = "10"
                   DISPLAY "Account not found."
               ELSE
                   ADD WS-TRANSACTION-AMOUNT TO AccountBalance
                   REWRITE AccountData
                   IF WS-FILE-STATUS NOT = "00"
                       DISPLAY "Error during deposit. Please try again."
                   ELSE
                DISPLAY "Deposit successful. Your updated balance is: "
                       DISPLAY AccountBalance
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
                   SUBTRACT WS-TRANSACTION-AMOUNT FROM AccountBalance
                   IF AccountBalance < 0
                DISPLAY "Insufficient balance. Transaction declined."
                       ADD WS-TRANSACTION-AMOUNT TO AccountBalance
                   ELSE
                       REWRITE AccountData
                       IF WS-FILE-STATUS NOT = "00"
                DISPLAY "Error during withdrawal. Please try again."
                       ELSE
            DISPLAY "Withdrawal successful. Your updated balance is: " 
                           DISPLAY AccountBalance
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
                   DISPLAY "Account Number: " AccountNumber
                   DISPLAY "Account Name: " AccountName
                   DISPLAY "Card Number: " CardNumber
                   DISPLAY "Balance: $" AccountBalance
               END-IF
           CLOSE AccountFile.

       PROMPT-FOR-ACCOUNT-NUMBER.
           DISPLAY "Enter your Account Number: "
           ACCEPT WS-SEARCH-NUMBER.

       SEARCH-ACCOUNT.
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO AccountData
               IF WS-SEARCH-NUMBER = AccountNumber
                   EXIT PERFORM
               END-IF
           END-PERFORM.
