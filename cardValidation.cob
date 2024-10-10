IDENTIFICATION DIVISION.
PROGRAM-ID. CardValidation.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CardFile ASSIGN TO "card.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS IS SEQUENTIAL
        FILE STATUS IS FileStatus.
DATA DIVISION.
FILE SECTION.
FD CardFile.
01 CardData.
   02 CardNumber      PIC 9(16).
   02 CVV             PIC 999.
   02 ExpirationDate.
      03 MM           PIC 99.
      03 YY           PIC 9999.

WORKING-STORAGE SECTION.
01 WSCard.
   02 WSCardNumber    PIC 9(16).
   02 WSCVV           PIC 999.
   02 WSExpirationDate.
      03 WSMM         PIC 99.
      03 WSYY         PIC 9999.

*> Exception Handling Performed through the File Status
01 FileStatus         PIC XX.

PROCEDURE DIVISION.
    OPEN OUTPUT CardFile
        IF FileStatus NOT = '00'
            DISPLAY "Error: Unable to open file."
            STOP RUN
        END-IF.

    MOVE 1234567890123456 TO CardNumber.
    MOVE 123 TO CVV.
    MOVE 01 TO MM.
    MOVE 2025 TO YY.

    WRITE CardData
        IF FileStatus NOT = '00'
            DISPLAY "Error: Write operation failed."
            CLOSE CardFile
            STOP RUN
        END-IF.

    DISPLAY "Record written successfully."

    CLOSE CardFile.

    STOP RUN.
