IDENTIFICATION DIVISION.
PROGRAM-ID. DataManipulationDemo.

DATA DIVISION.
    WORKING-STORAGE SECTION.

        *> Integer variables
        01 WS-INT1           PIC 9(4) VALUE 15.
        01 WS-INT2           PIC 9(4) VALUE 25.
        01 WS-INT-SUM        PIC 9(4).
        01 WS-INT-AVG        PIC 9(4)V9(2).

        *> Float variables
        01 WS-FLOAT1         PIC 9(3)V9(2) VALUE 12.34.
        01 WS-FLOAT2         PIC 9(3)V9(2) VALUE 45.67.
        01 WS-FLOAT-SUM      PIC 9(4)V9(2).
        01 WS-FLOAT-AVG      PIC 9(3)V9(2).

        *> String variables
        01 WS-STRING1        PIC X(20) VALUE "COBOL Programming".
        01 WS-STRING2        PIC X(20) VALUE "Demo".
        01 WS-STRING-RESULT  PIC X(40).
        01 WS-STRING-REPLACED PIC X(40).

        *> Boolean variable
        01 WS-BOOLEAN        PIC 9 VALUE 0.  *> 0 for False, 1 for True
        01 WS-COMPARISON-RESULT PIC X(10).
       
PROCEDURE DIVISION.
MAIN-LOGIC.
    
    *> Integer Operations
    DISPLAY "Integer 1: " WS-INT1.
    DISPLAY "Integer 2: " WS-INT2.

    ADD WS-INT1 WS-INT2 GIVING WS-INT-SUM.
    DISPLAY "Sum of Integers: " WS-INT-SUM.

    COMPUTE WS-INT-AVG = FUNCTION NUMVAL(WS-INT1 + WS-INT2) / 2.
    DISPLAY "Average of Integers: " WS-INT-AVG.

    *> Float Operations
    DISPLAY "Float 1: " WS-FLOAT1.
    DISPLAY "Float 2: " WS-FLOAT2.
    
    ADD WS-FLOAT1 WS-FLOAT2 GIVING WS-FLOAT-SUM.
    DISPLAY "Sum of Floats: " WS-FLOAT-SUM.

    COMPUTE WS-FLOAT-AVG = FUNCTION NUMVAL(WS-FLOAT1 + WS-FLOAT2) / 2.
    DISPLAY "Average of Floats: " WS-FLOAT-AVG.

    *> String Operations
    DISPLAY "String 1: " WS-STRING1.
    DISPLAY "String 2: " WS-STRING2.

    STRING WS-STRING1 DELIMITED BY SPACE
     " " WS-STRING2 DELIMITED BY SPACE
    INTO WS-STRING-RESULT.
    DISPLAY "Concatenated String: " WS-STRING-RESULT.

    MOVE FUNCTION REVERSE(WS-STRING1) TO WS-STRING-REPLACED.
    DISPLAY "Reversed String 1: " WS-STRING-REPLACED.

    *> Boolean Operations
    IF WS-INT1 = WS-INT2
     MOVE 1 TO WS-BOOLEAN
     MOVE "True" TO WS-COMPARISON-RESULT
    ELSE
     MOVE 0 TO WS-BOOLEAN
     MOVE "False" TO WS-COMPARISON-RESULT
    END-IF.
    DISPLAY "Boolean Comparison (Is Integer 1 = Integer 2?): " WS-COMPARISON-RESULT.

    IF WS-BOOLEAN = 1
     DISPLAY "The comparison is true."
    ELSE
     DISPLAY "The comparison is false."
    END-IF.

STOP RUN.

