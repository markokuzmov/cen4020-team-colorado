>>SOURCE FORMAT FREE
*> ================================================================
*> PROGRAM:     INCOLLEGE.cbl
*> DESCRIPTION: InCollege - LinkedIn for College Students
*> TEAM:        Colorado  |  CEN4020
*>
*> COMPILE:  cobc -x -o incollege src/INCOLLEGE.cbl
*> RUN:      ./incollege
*> DATA:     data/users.dat  (auto-created on first registration)
*> ================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. INCOLLEGE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT OPTIONAL USERS-FILE
        ASSIGN TO "data/users.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS WS-FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD  USERS-FILE.
01  USER-RECORD.
    05  UR-USERNAME      PIC X(20).
    05  UR-PASSWORD      PIC X(12).

WORKING-STORAGE SECTION.
01  WS-FILE-STATUS       PIC XX VALUE SPACES.
    88  FILE-OK          VALUE "00".

01  WS-MENU-CHOICE       PIC 9  VALUE ZERO.
01  WS-USERNAME          PIC X(20) VALUE SPACES.
01  WS-PASSWORD          PIC X(12) VALUE SPACES.
01  WS-PASS-LEN          PIC 99    VALUE ZERO.
01  WS-PASS-VALID        PIC X     VALUE "N".
01  WS-HAS-UPPER         PIC X     VALUE "N".
01  WS-HAS-DIGIT         PIC X     VALUE "N".
01  WS-HAS-SPECIAL       PIC X     VALUE "N".
01  WS-IDX               PIC 99    VALUE ZERO.
01  WS-CHAR              PIC X     VALUE SPACE.
01  WS-LOGIN-FOUND       PIC X     VALUE "N".
01  WS-DUPLICATE         PIC X     VALUE "N".
01  WS-EOF               PIC X     VALUE "N".
01  WS-RUNNING           PIC X     VALUE "Y".

PROCEDURE DIVISION.

*> ----------------------------------------------------------------
*> MAIN-MENU: top-level loop - show menu until user chooses Exit
*> ----------------------------------------------------------------
MAIN-MENU.
    PERFORM UNTIL WS-RUNNING = "N"
        DISPLAY " "
        DISPLAY "========================================"
        DISPLAY "          Welcome to InCollege          "
        DISPLAY "    LinkedIn for College Students        "
        DISPLAY "========================================"
        DISPLAY "  1. Create New Account"
        DISPLAY "  2. Log In"
        DISPLAY "  3. Exit"
        DISPLAY "========================================"
        DISPLAY "Enter your choice: " WITH NO ADVANCING
        ACCEPT WS-MENU-CHOICE

        EVALUATE WS-MENU-CHOICE
            WHEN 1
                PERFORM USER-REGISTRATION
            WHEN 2
                PERFORM USER-LOGIN
            WHEN 3
                DISPLAY "Goodbye!"
                MOVE "N" TO WS-RUNNING
            WHEN OTHER
                DISPLAY "Invalid choice. Please try again."
        END-EVALUATE
    END-PERFORM
    STOP RUN.

*> ----------------------------------------------------------------
*> USER-REGISTRATION: prompt for username + valid password,
*> reject duplicates, then append to data/users.dat
*> ----------------------------------------------------------------
USER-REGISTRATION.
    DISPLAY " "
    DISPLAY "--- Create New Account ---"
    DISPLAY "Enter username: " WITH NO ADVANCING
    ACCEPT WS-USERNAME

    MOVE "N" TO WS-PASS-VALID
    PERFORM UNTIL WS-PASS-VALID = "Y"
        DISPLAY "Enter password: " WITH NO ADVANCING
        ACCEPT WS-PASSWORD
        PERFORM VALIDATE-PASSWORD
        IF WS-PASS-VALID = "N"
            DISPLAY "Please try a different password."
        END-IF
    END-PERFORM

    PERFORM CHECK-DUPLICATE
    IF WS-DUPLICATE = "Y"
        DISPLAY "Username already exists. Please choose another."
        PERFORM USER-REGISTRATION
    ELSE
        PERFORM SAVE-USER
        DISPLAY "Account created successfully!"
        DISPLAY "Welcome to InCollege, " FUNCTION TRIM(WS-USERNAME) "!"
    END-IF.

*> ----------------------------------------------------------------
*> VALIDATE-PASSWORD: enforce all four complexity rules:
*>   1. Length 8-12
*>   2. At least one uppercase (A-Z)
*>   3. At least one digit (0-9)
*>   4. At least one special character (!@#$%^&*()-_+=)
*> Sets WS-PASS-VALID to "Y" only when all rules pass.
*> ----------------------------------------------------------------
VALIDATE-PASSWORD.
    MOVE "N" TO WS-PASS-VALID
    MOVE "N" TO WS-HAS-UPPER
    MOVE "N" TO WS-HAS-DIGIT
    MOVE "N" TO WS-HAS-SPECIAL

    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD)) TO WS-PASS-LEN

    IF WS-PASS-LEN < 8 OR WS-PASS-LEN > 12
        DISPLAY "Password must be 8-12 characters long."
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-PASS-LEN
        MOVE WS-PASSWORD(WS-IDX:1) TO WS-CHAR

        IF WS-CHAR >= "A" AND WS-CHAR <= "Z"
            MOVE "Y" TO WS-HAS-UPPER
        END-IF

        IF WS-CHAR >= "0" AND WS-CHAR <= "9"
            MOVE "Y" TO WS-HAS-DIGIT
        END-IF

        IF WS-CHAR = "!" OR WS-CHAR = "@" OR WS-CHAR = "#"
            OR WS-CHAR = "$" OR WS-CHAR = "%" OR WS-CHAR = "^"
            OR WS-CHAR = "&" OR WS-CHAR = "*" OR WS-CHAR = "("
            OR WS-CHAR = ")" OR WS-CHAR = "-" OR WS-CHAR = "_"
            OR WS-CHAR = "+" OR WS-CHAR = "="
            MOVE "Y" TO WS-HAS-SPECIAL
        END-IF
    END-PERFORM

    IF WS-HAS-UPPER = "N"
        DISPLAY "Password must have at least one uppercase letter (A-Z)."
        EXIT PARAGRAPH
    END-IF

    IF WS-HAS-DIGIT = "N"
        DISPLAY "Password must have at least one digit (0-9)."
        EXIT PARAGRAPH
    END-IF

    IF WS-HAS-SPECIAL = "N"
        DISPLAY "Password must have at least one special character."
        DISPLAY "  Accepted: ! @ # $ % ^ & * ( ) - _ + ="
        EXIT PARAGRAPH
    END-IF

    MOVE "Y" TO WS-PASS-VALID.

*> ----------------------------------------------------------------
*> CHECK-DUPLICATE: scan users.dat for matching username
*> ----------------------------------------------------------------
CHECK-DUPLICATE.
    MOVE "N" TO WS-DUPLICATE
    MOVE "N" TO WS-EOF
    OPEN INPUT USERS-FILE
    IF FILE-OK
        PERFORM UNTIL WS-EOF = "Y"
            READ USERS-FILE
                AT END
                    MOVE "Y" TO WS-EOF
                NOT AT END
                    IF FUNCTION TRIM(UR-USERNAME) =
                        FUNCTION TRIM(WS-USERNAME)
                        MOVE "Y" TO WS-DUPLICATE
                        MOVE "Y" TO WS-EOF
                    END-IF
            END-READ
        END-PERFORM
    END-IF
    CLOSE USERS-FILE.

*> ----------------------------------------------------------------
*> SAVE-USER: append a new user record to data/users.dat
*> ----------------------------------------------------------------
SAVE-USER.
    MOVE WS-USERNAME TO UR-USERNAME
    MOVE WS-PASSWORD TO UR-PASSWORD
    OPEN EXTEND USERS-FILE
    WRITE USER-RECORD
    CLOSE USERS-FILE.

*> ----------------------------------------------------------------
*> USER-LOGIN: unlimited retry loop until credentials match
*> ----------------------------------------------------------------
USER-LOGIN.
    DISPLAY " "
    DISPLAY "--- Log In ---"
    MOVE "N" TO WS-LOGIN-FOUND
    PERFORM UNTIL WS-LOGIN-FOUND = "Y"
        DISPLAY "Enter username: " WITH NO ADVANCING
        ACCEPT WS-USERNAME
        DISPLAY "Enter password: " WITH NO ADVANCING
        ACCEPT WS-PASSWORD
        PERFORM VERIFY-CREDENTIALS
        IF WS-LOGIN-FOUND = "N"
            DISPLAY "Incorrect username/password, please try again"
        END-IF
    END-PERFORM
    DISPLAY "You have successfully logged in."
    DISPLAY "Welcome, " FUNCTION TRIM(WS-USERNAME) "!".

*> ----------------------------------------------------------------
*> VERIFY-CREDENTIALS: find a matching username+password in users.dat
*> ----------------------------------------------------------------
VERIFY-CREDENTIALS.
    MOVE "N" TO WS-LOGIN-FOUND
    MOVE "N" TO WS-EOF
    OPEN INPUT USERS-FILE
    IF FILE-OK
        PERFORM UNTIL WS-EOF = "Y"
            READ USERS-FILE
                AT END
                    MOVE "Y" TO WS-EOF
                NOT AT END
                    IF FUNCTION TRIM(UR-USERNAME) =
                        FUNCTION TRIM(WS-USERNAME)
                        AND FUNCTION TRIM(UR-PASSWORD) =
                        FUNCTION TRIM(WS-PASSWORD)
                        MOVE "Y" TO WS-LOGIN-FOUND
                        MOVE "Y" TO WS-EOF
                    END-IF
            END-READ
        END-PERFORM
    END-IF
    CLOSE USERS-FILE.
