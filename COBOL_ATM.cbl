      * November/7/2024 || ITCS 4102-091 || COBOL Cobras
      * This program will demonstrate the basic capabilities of an ATM
      * for an imaginary bank called COBOL bank.
       IDENTIFICATION DIVISION.
       PROGRAM-ID COBOL_ATM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * The cash balance of the user.
       01 USER-BALANCE   PIC X(20) VALUE '0'.
      
      * This value is used to hold the users keyboard input.
       01 USER-INPUT   PIC X(20).