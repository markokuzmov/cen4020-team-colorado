>>SOURCE FORMAT FREE
*> ================================================================
*> PROGRAM:     INCOLLEGE.cbl
*> DESCRIPTION: InCollege - LinkedIn for College Students
*> TEAM:        Colorado  |  CEN4020
*>
*> COMPILE:  cobc -x -o incollege src/INCOLLEGE.cbl
*> RUN:      ./incollege
*> DATA:     data/users.dat        (auto-created on first registration,
*>                                  loaded into memory on startup)
*>           InCollege-Input.txt   (scripted user interactions - read
*>                                  sequentially in place of ACCEPT)
*>           InCollege-Output.txt  (mirrors every line shown on the
*>                                  console, including echoed input)
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

    SELECT OPTIONAL PROFILES-FILE
        ASSIGN TO "data/profiles.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS WS-PROFILE-FILE-STATUS.

    SELECT INPUT-FILE
        ASSIGN TO "InCollege-Input.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS WS-INPUT-STATUS.

DATA DIVISION.
FILE SECTION.
FD  USERS-FILE.
01  USER-RECORD.
    05  UR-USERNAME      PIC X(20).
    05  UR-PASSWORD      PIC X(12).

FD  PROFILES-FILE.
01  PROFILE-RECORD.
    05  PR-USERNAME      PIC X(20).
    05  PR-FULL-NAME     PIC X(60).
    05  PR-UNIVERSITY    PIC X(50).
    05  PR-MAJOR         PIC X(50).
    05  PR-GRAD-YEAR     PIC 9(4).
    05  PR-ABOUT         PIC X(200).

FD  INPUT-FILE.
01  INPUT-RECORD         PIC X(200).

WORKING-STORAGE SECTION.
01  WS-FILE-STATUS       PIC XX VALUE SPACES.
    88  FILE-OK          VALUE "00".

01  WS-PROFILE-FILE-STATUS PIC XX VALUE SPACES.
    88  PROFILE-FILE-OK  VALUE "00".

01  WS-INPUT-STATUS      PIC XX VALUE SPACES.
    88  INPUT-OK         VALUE "00".

01  WS-OUTPUT-STATUS     PIC XX VALUE SPACES.
    88  OUTPUT-OK        VALUE "00".

01  WS-MENU-CHOICE       PIC X     VALUE SPACE.
01  WS-USERNAME          PIC X(20) VALUE SPACES.
01  WS-PASSWORD          PIC X(100) VALUE SPACES.
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

*> ---- In-memory profile capture fields
01  FIRST-NAME            PIC X(30) VALUE SPACES.
01  LAST-NAME             PIC X(30) VALUE SPACES.
01  UNIVERSITY            PIC X(50) VALUE SPACES.
01  MAJOR                 PIC X(50) VALUE SPACES.
01  GRAD-YEAR             PIC 9(4) VALUE ZERO.
01  GRAD-YEAR-INPUT       PIC X(100) VALUE SPACES.
01  ABOUT-ME              PIC X(200) VALUE SPACES.
01  EXPERIENCE-TABLE.
    05  EXPERIENCE-COUNT  PIC 9 VALUE 0.
    05  EXPERIENCE-ENTRY OCCURS 1 TO 3 TIMES
        DEPENDING ON EXPERIENCE-COUNT
        INDEXED BY EXP-IDX.
        10  EXP-TITLE     PIC X(50).
        10  EXP-COMPANY   PIC X(50).
        10  EXP-DATES     PIC X(30).
        10  EXP-DESC      PIC X(100).

*> ---- Core I/O engine buffer: every DISPLAY/ACCEPT in the program
*> ---- is routed through this buffer so console and file output
*> ---- always stay in lock-step (see WRITE-LINE / WRITE-PROMPT /
*> ---- READ-INPUT-LINE below). WS-LINE-LEN carries the *exact*
*> ---- intended length of whatever was just moved into the buffer
*> ---- (set via FUNCTION LENGTH of the same literal, or via the
*> ---- STRING pointer for built-up messages) - it is NOT inferred
*> ---- by trimming trailing spaces, because some messages (e.g. the
*> ---- "Enter username: " prompts, or the centered banner lines)
*> ---- have meaningful trailing spaces of their own that must not
*> ---- be stripped.
01  WS-LINE-BUFFER       PIC X(300) VALUE SPACES.
01  WS-LINE-LEN          PIC 999    VALUE ZERO.
01  WS-STRING-PTR        PIC 999    VALUE ZERO.
01  WS-PROMPT-BUFFER     PIC X(300) VALUE SPACES.
01  WS-PROMPT-LEN        PIC 999    VALUE ZERO.
01  WS-OUTPUT-HANDLE     PIC 9(9) COMP-5 VALUE ZERO.
01  WS-OUTPUT-MODE-BITS  PIC 9(9) COMP-5 VALUE 438.
01  WS-OUTPUT-FILENAME.
    05  FILLER            PIC X(20) VALUE "InCollege-Output.txt".
    05  FILLER            PIC X     VALUE LOW-VALUES.
01  WS-OUTPUT-MODE.
    05  FILLER            PIC X(2)  VALUE "wb".
    05  FILLER            PIC X     VALUE LOW-VALUES.
01  WS-OUTPUT-NEWLINE    PIC X      VALUE X"0A".
01  WS-OUTPUT-LEN        PIC 999    VALUE ZERO.
01  WS-WRITE-COUNT       PIC 999    COMP-5 VALUE ZERO.
01  OUTPUT-RECORD        PIC X(400) VALUE SPACES.

*> ---- Account persistence & capacity limit (5-account maximum)
01  WS-MAX-ACCOUNTS      PIC 9     VALUE 5.
01  WS-ACCOUNT-COUNT     PIC 9     VALUE ZERO.
01  WS-TBL-IDX           PIC 9     VALUE ZERO.
01  WS-USER-TABLE.
    05  WS-USER-ENTRY OCCURS 5 TIMES.
        10  WS-TBL-USERNAME  PIC X(20).
        10  WS-TBL-PASSWORD  PIC X(12).

*> ---- Profile persistence: profiles are keyed by username and kept
*> ---- in an in-memory table (mirrors the account table pattern
*> ---- above), loaded from data/profiles.dat at startup and rewritten
*> ---- in full whenever a profile is created or edited.
01  WS-PROFILE-FOUND     PIC X     VALUE "N".
01  WS-PROFILE-COUNT     PIC 9     VALUE ZERO.
01  WS-PROFILE-IDX       PIC 9     VALUE ZERO.
01  WS-PROFILE-USERNAME  PIC X(20) VALUE SPACES.
01  WS-FULL-NAME         PIC X(60) VALUE SPACES.
01  WS-UNIVERSITY        PIC X(50) VALUE SPACES.
01  WS-MAJOR             PIC X(50) VALUE SPACES.
01  WS-ABOUT             PIC X(200) VALUE SPACES.
01  WS-FIELD-LABEL       PIC X(12) VALUE SPACES.
01  WS-FIELD-VALUE       PIC X(200) VALUE SPACES.
01  WS-PROFILE-TABLE.
    05  WS-PROFILE-ENTRY OCCURS 5 TIMES.
        10  WS-TBL-PROFILE-USERNAME PIC X(20).
        10  WS-TBL-FULL-NAME        PIC X(60).
        10  WS-TBL-UNIVERSITY       PIC X(50).
        10  WS-TBL-MAJOR            PIC X(50).
        10  WS-TBL-GRAD-YEAR        PIC 9(4).
        10  WS-TBL-ABOUT            PIC X(200).

PROCEDURE DIVISION.

*> ----------------------------------------------------------------
*> 000-MAIN-CONTROL: true program entry point - open the I/O-mirror
*> files, load any persisted accounts, run the menu, then close up.
*> ----------------------------------------------------------------
000-MAIN-CONTROL.
    PERFORM OPEN-IO-FILES
    PERFORM LOAD-USERS
    PERFORM LOAD-PROFILES
    PERFORM MAIN-MENU
    PERFORM CLOSE-IO-FILES
    STOP RUN.

*> ----------------------------------------------------------------
*> MAIN-MENU: top-level loop - show menu until user chooses Exit
*> ----------------------------------------------------------------
MAIN-MENU.
    PERFORM UNTIL WS-RUNNING = "N"
        PERFORM WRITE-BLANK-LINE
        MOVE "========================================" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("========================================")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "          Welcome to InCollege          " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("          Welcome to InCollege          ")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "    LinkedIn for College Students        " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("    LinkedIn for College Students        ")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "========================================" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("========================================")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  1. Create New Account" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  1. Create New Account") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  2. Log In" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  2. Log In") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  3. Exit" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  3. Exit") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "========================================" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("========================================")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "Enter your choice: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter your choice: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER(1:1) TO WS-MENU-CHOICE

        EVALUATE WS-MENU-CHOICE
            WHEN "1"
                PERFORM USER-REGISTRATION
            WHEN "2"
                PERFORM USER-LOGIN
            WHEN "3"
                MOVE "Goodbye!" TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Goodbye!") TO WS-LINE-LEN
                PERFORM WRITE-LINE
                MOVE "N" TO WS-RUNNING
            WHEN OTHER
                MOVE "Invalid choice. Please try again." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Invalid choice. Please try again.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
        END-EVALUATE
    END-PERFORM.

*> ----------------------------------------------------------------
*> USER-REGISTRATION: prompt for username + valid password,
*> reject duplicates, then append to data/users.dat - unless the
*> 5-account maximum has already been reached.
*> ----------------------------------------------------------------
USER-REGISTRATION.
    IF WS-ACCOUNT-COUNT >= WS-MAX-ACCOUNTS
        MOVE "All permitted accounts have been created, please come back later"
            TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH(
            "All permitted accounts have been created, please come back later")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        EXIT PARAGRAPH
    END-IF

    PERFORM WRITE-BLANK-LINE
    MOVE "--- Create New Account ---" TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("--- Create New Account ---") TO WS-LINE-LEN
    PERFORM WRITE-LINE
    MOVE "Enter username: " TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("Enter username: ") TO WS-LINE-LEN
    PERFORM WRITE-PROMPT
    PERFORM READ-INPUT-LINE
    MOVE WS-LINE-BUFFER TO WS-USERNAME

    MOVE "N" TO WS-PASS-VALID
    PERFORM UNTIL WS-PASS-VALID = "Y"
        MOVE "Enter password: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter password: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO WS-PASSWORD
        PERFORM VALIDATE-PASSWORD
        IF WS-PASS-VALID = "N"
            MOVE "Please try a different password." TO WS-LINE-BUFFER
            MOVE FUNCTION LENGTH("Please try a different password.")
                TO WS-LINE-LEN
            PERFORM WRITE-LINE
        END-IF
    END-PERFORM

    PERFORM CHECK-DUPLICATE
    IF WS-DUPLICATE = "Y"
        MOVE "Username already exists. Please choose another."
            TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH(
            "Username already exists. Please choose another.")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        PERFORM USER-REGISTRATION
    ELSE
        PERFORM SAVE-USER
        MOVE "Account created successfully!" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Account created successfully!") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE 1 TO WS-STRING-PTR
        STRING "Welcome to InCollege, " DELIMITED BY SIZE
            FUNCTION TRIM(WS-USERNAME, TRAILING) DELIMITED BY SIZE
            "!" DELIMITED BY SIZE
            INTO WS-LINE-BUFFER
            WITH POINTER WS-STRING-PTR
        END-STRING
        COMPUTE WS-LINE-LEN = WS-STRING-PTR - 1
        PERFORM WRITE-LINE
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
        MOVE "Password must be 8-12 characters long." TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Password must be 8-12 characters long.")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
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
        MOVE "Password must have at least one uppercase letter (A-Z)."
            TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH(
            "Password must have at least one uppercase letter (A-Z).")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        EXIT PARAGRAPH
    END-IF

    IF WS-HAS-DIGIT = "N"
        MOVE "Password must have at least one digit (0-9)." TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Password must have at least one digit (0-9).")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        EXIT PARAGRAPH
    END-IF

    IF WS-HAS-SPECIAL = "N"
        MOVE "Password must have at least one special character."
            TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH(
            "Password must have at least one special character.")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  Accepted: ! @ # $ % ^ & * ( ) - _ + =" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  Accepted: ! @ # $ % ^ & * ( ) - _ + =")
            TO WS-LINE-LEN
        PERFORM WRITE-LINE
        EXIT PARAGRAPH
    END-IF

    MOVE "Y" TO WS-PASS-VALID.

*> ----------------------------------------------------------------
*> CHECK-DUPLICATE: scan the in-memory user table (loaded at
*> startup by LOAD-USERS) for a matching username
*> ----------------------------------------------------------------
CHECK-DUPLICATE.
    MOVE "N" TO WS-DUPLICATE
    PERFORM VARYING WS-TBL-IDX FROM 1 BY 1
        UNTIL WS-TBL-IDX > WS-ACCOUNT-COUNT
        IF FUNCTION TRIM(WS-TBL-USERNAME(WS-TBL-IDX)) =
            FUNCTION TRIM(WS-USERNAME)
            MOVE "Y" TO WS-DUPLICATE
        END-IF
    END-PERFORM.

*> ----------------------------------------------------------------
*> SAVE-USER: append a new user record to data/users.dat and mirror
*> it into the in-memory table so later checks/logins see it too
*> ----------------------------------------------------------------
SAVE-USER.
    MOVE WS-USERNAME TO UR-USERNAME
    MOVE WS-PASSWORD TO UR-PASSWORD
    OPEN EXTEND USERS-FILE
    WRITE USER-RECORD
    CLOSE USERS-FILE
    ADD 1 TO WS-ACCOUNT-COUNT
    MOVE WS-USERNAME TO WS-TBL-USERNAME(WS-ACCOUNT-COUNT)
    MOVE WS-PASSWORD TO WS-TBL-PASSWORD(WS-ACCOUNT-COUNT).

*> ----------------------------------------------------------------
*> USER-LOGIN: unlimited retry loop until credentials match
*> ----------------------------------------------------------------
USER-LOGIN.
    PERFORM WRITE-BLANK-LINE
    MOVE "--- Log In ---" TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("--- Log In ---") TO WS-LINE-LEN
    PERFORM WRITE-LINE
    MOVE "N" TO WS-LOGIN-FOUND
    PERFORM UNTIL WS-LOGIN-FOUND = "Y"
        MOVE "Enter username: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter username: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO WS-USERNAME
        MOVE "Enter password: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter password: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO WS-PASSWORD
        PERFORM VERIFY-CREDENTIALS
        IF WS-LOGIN-FOUND = "N"
            MOVE "Incorrect username/password, please try again"
                TO WS-LINE-BUFFER
            MOVE FUNCTION LENGTH(
                "Incorrect username/password, please try again")
                TO WS-LINE-LEN
            PERFORM WRITE-LINE
        END-IF
    END-PERFORM
    MOVE "You have successfully logged in." TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("You have successfully logged in.") TO WS-LINE-LEN
    PERFORM WRITE-LINE
    MOVE 1 TO WS-STRING-PTR
    STRING "Welcome, " DELIMITED BY SIZE
        FUNCTION TRIM(WS-USERNAME, TRAILING) DELIMITED BY SIZE
        "!" DELIMITED BY SIZE
        INTO WS-LINE-BUFFER
        WITH POINTER WS-STRING-PTR
    END-STRING
    COMPUTE WS-LINE-LEN = WS-STRING-PTR - 1
    PERFORM WRITE-LINE
    PERFORM POST-LOGIN-MENU.

*> ----------------------------------------------------------------
*> POST-LOGIN-MENU: authenticated navigation until the user logs out
*> ----------------------------------------------------------------
POST-LOGIN-MENU.
    PERFORM UNTIL WS-RUNNING = "N"
        PERFORM WRITE-BLANK-LINE
        MOVE "--- Main Menu ---" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("--- Main Menu ---") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  1. Create/Edit My Profile" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  1. Create/Edit My Profile") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  2. Search for a Job" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  2. Search for a Job") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  3. Find Someone You Know" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  3. Find Someone You Know") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  4. Learn a New Skill" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  4. Learn a New Skill") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  5. Logout" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  5. Logout") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "Enter your choice: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter your choice: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER(1:1) TO WS-MENU-CHOICE

        EVALUATE WS-MENU-CHOICE
            WHEN "1"
                PERFORM PROFILE-MENU
            WHEN "2"
                MOVE "This feature is under construction." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("This feature is under construction.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
            WHEN "4"
                PERFORM LEARN-SKILL-MENU
            WHEN "5"
                MOVE "Logging out..." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Logging out...") TO WS-LINE-LEN
                PERFORM WRITE-LINE
                MOVE "N" TO WS-RUNNING
            WHEN OTHER
                MOVE "Invalid choice. Please try again." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Invalid choice. Please try again.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
        END-EVALUATE
    END-PERFORM.

*> ----------------------------------------------------------------
*> PROFILE-MENU: logged-in users can create, edit, and view profiles.
*> Profiles are identified by username and saved in
*> data/profiles.dat, so the info a user saves here is exactly what
*> they (and everyone else) will see on every later login/view -
*> nothing has to be re-entered once it has been saved once.
*> ----------------------------------------------------------------
PROFILE-MENU.
    MOVE SPACE TO WS-MENU-CHOICE
    PERFORM UNTIL WS-MENU-CHOICE = "5"
        PERFORM WRITE-BLANK-LINE
        MOVE "--- Profile ---" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("--- Profile ---") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  1. Create My Profile" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  1. Create My Profile") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  2. Edit My Profile" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  2. Edit My Profile") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  3. View My Profile" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  3. View My Profile") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  4. View a Profile" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  4. View a Profile") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  5. Go Back" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  5. Go Back") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "Enter your choice: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter your choice: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER(1:1) TO WS-MENU-CHOICE

        EVALUATE WS-MENU-CHOICE
            WHEN "1"
                PERFORM LOAD-CURRENT-PROFILE
                IF WS-PROFILE-FOUND = "Y"
                    MOVE "A profile already exists. Choose Edit My Profile."
                        TO WS-LINE-BUFFER
                    MOVE FUNCTION LENGTH(
                        "A profile already exists. Choose Edit My Profile.")
                        TO WS-LINE-LEN
                    PERFORM WRITE-LINE
                ELSE
                    PERFORM CAPTURE-PROFILE
                    PERFORM ADD-CURRENT-PROFILE
                    PERFORM SAVE-PROFILES
                    MOVE "Profile created successfully!" TO WS-LINE-BUFFER
                    MOVE FUNCTION LENGTH("Profile created successfully!")
                        TO WS-LINE-LEN
                    PERFORM WRITE-LINE
                END-IF
            WHEN "2"
                PERFORM LOAD-CURRENT-PROFILE
                IF WS-PROFILE-FOUND = "N"
                    MOVE "No profile exists yet. Choose Create My Profile."
                        TO WS-LINE-BUFFER
                    MOVE FUNCTION LENGTH(
                        "No profile exists yet. Choose Create My Profile.")
                        TO WS-LINE-LEN
                    PERFORM WRITE-LINE
                ELSE
                    PERFORM CAPTURE-PROFILE
                    PERFORM UPDATE-CURRENT-PROFILE
                    PERFORM SAVE-PROFILES
                    MOVE "Profile updated successfully!" TO WS-LINE-BUFFER
                    MOVE FUNCTION LENGTH("Profile updated successfully!")
                        TO WS-LINE-LEN
                    PERFORM WRITE-LINE
                END-IF
            WHEN "3"
                PERFORM VIEW-MY-PROFILE
            WHEN "4"
                PERFORM VIEW-PROFILE
            WHEN "5"
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice. Please try again." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Invalid choice. Please try again.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
        END-EVALUATE
    END-PERFORM.

*> ----------------------------------------------------------------
*> CAPTURE-PROFILE: prompt for the four profile fields into the
*> WS-FULL-NAME/WS-UNIVERSITY/WS-MAJOR/WS-ABOUT working fields,
*> shared by both Create and Edit
*> ----------------------------------------------------------------
CAPTURE-PROFILE.
    MOVE SPACES TO FIRST-NAME LAST-NAME UNIVERSITY MAJOR ABOUT-ME
    MOVE ZERO TO GRAD-YEAR
    MOVE "Create/Edit Profile" TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("Create/Edit Profile") TO WS-LINE-LEN
    PERFORM WRITE-LINE
    PERFORM UNTIL FUNCTION TRIM(FIRST-NAME) NOT = SPACES
        MOVE "Enter First Name:" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter First Name:") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO FIRST-NAME
    END-PERFORM
    PERFORM UNTIL FUNCTION TRIM(LAST-NAME) NOT = SPACES
        MOVE "Enter Last Name:" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter Last Name:") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO LAST-NAME
    END-PERFORM
    PERFORM UNTIL FUNCTION TRIM(UNIVERSITY) NOT = SPACES
        MOVE "Enter University/College Attended:" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter University/College Attended:")
            TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO UNIVERSITY
    END-PERFORM
    PERFORM UNTIL FUNCTION TRIM(MAJOR) NOT = SPACES
        MOVE "Enter Major:" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter Major:") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER TO MAJOR
    END-PERFORM
    PERFORM UNTIL GRAD-YEAR > 2025 AND GRAD-YEAR < 2034
        MOVE "Enter Graduation Year (YYYY):" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter Graduation Year (YYYY):")
            TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE SPACES TO GRAD-YEAR-INPUT
        MOVE FUNCTION TRIM(WS-LINE-BUFFER) TO GRAD-YEAR-INPUT
        IF GRAD-YEAR-INPUT(1:4) IS NUMERIC
            AND GRAD-YEAR-INPUT(5:1) = SPACE
            MOVE GRAD-YEAR-INPUT(1:4) TO GRAD-YEAR
        ELSE
            MOVE ZERO TO GRAD-YEAR
        END-IF
        IF GRAD-YEAR <= 2025 OR GRAD-YEAR >= 2034
            MOVE "Invalid graduation year. Please enter a year between 2026 and 2033."
                TO WS-LINE-BUFFER
            MOVE FUNCTION LENGTH(
                "Invalid graduation year. Please enter a year between 2026 and 2033.")
                TO WS-LINE-LEN
            PERFORM WRITE-LINE
        END-IF
    END-PERFORM
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):"
        TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH(
        "Enter About Me (optional, max 200 chars, enter blank line to skip):")
        TO WS-LINE-LEN
    PERFORM WRITE-PROMPT
    PERFORM READ-INPUT-LINE
    MOVE WS-LINE-BUFFER TO ABOUT-ME
    MOVE SPACES TO WS-FULL-NAME
    STRING FUNCTION TRIM(FIRST-NAME) DELIMITED BY SIZE
        " " DELIMITED BY SIZE
        FUNCTION TRIM(LAST-NAME) DELIMITED BY SIZE
        INTO WS-FULL-NAME
    END-STRING
    MOVE UNIVERSITY TO WS-UNIVERSITY
    MOVE MAJOR TO WS-MAJOR
    MOVE ABOUT-ME TO WS-ABOUT.

*> ----------------------------------------------------------------
*> ADD-CURRENT-PROFILE / UPDATE-CURRENT-PROFILE: commit the fields
*> just captured into the in-memory profile table (new row vs.
*> overwrite the row LOAD-CURRENT-PROFILE just located)
*> ----------------------------------------------------------------
ADD-CURRENT-PROFILE.
    ADD 1 TO WS-PROFILE-COUNT
    MOVE WS-USERNAME TO WS-TBL-PROFILE-USERNAME(WS-PROFILE-COUNT)
    MOVE WS-FULL-NAME TO WS-TBL-FULL-NAME(WS-PROFILE-COUNT)
    MOVE WS-UNIVERSITY TO WS-TBL-UNIVERSITY(WS-PROFILE-COUNT)
    MOVE WS-MAJOR TO WS-TBL-MAJOR(WS-PROFILE-COUNT)
    MOVE GRAD-YEAR TO WS-TBL-GRAD-YEAR(WS-PROFILE-COUNT)
    MOVE WS-ABOUT TO WS-TBL-ABOUT(WS-PROFILE-COUNT).

UPDATE-CURRENT-PROFILE.
    MOVE WS-FULL-NAME TO WS-TBL-FULL-NAME(WS-PROFILE-IDX)
    MOVE WS-UNIVERSITY TO WS-TBL-UNIVERSITY(WS-PROFILE-IDX)
    MOVE WS-MAJOR TO WS-TBL-MAJOR(WS-PROFILE-IDX)
    MOVE GRAD-YEAR TO WS-TBL-GRAD-YEAR(WS-PROFILE-IDX)
    MOVE WS-ABOUT TO WS-TBL-ABOUT(WS-PROFILE-IDX).

*> ----------------------------------------------------------------
*> LOAD-CURRENT-PROFILE: locate the logged-in user's own row in the
*> profile table (WS-PROFILE-FOUND/WS-PROFILE-IDX), used by both
*> Create (to detect "already exists") and Edit (to detect "not
*> created yet" and to know which row to overwrite)
*> ----------------------------------------------------------------
LOAD-CURRENT-PROFILE.
    MOVE "N" TO WS-PROFILE-FOUND
    MOVE ZERO TO WS-PROFILE-IDX
    PERFORM VARYING WS-PROFILE-IDX FROM 1 BY 1
        UNTIL WS-PROFILE-IDX > WS-PROFILE-COUNT
            OR WS-PROFILE-FOUND = "Y"
        IF FUNCTION TRIM(WS-TBL-PROFILE-USERNAME(WS-PROFILE-IDX)) =
            FUNCTION TRIM(WS-USERNAME)
            MOVE "Y" TO WS-PROFILE-FOUND
        END-IF
    END-PERFORM.
    IF WS-PROFILE-FOUND = "Y"
        SUBTRACT 1 FROM WS-PROFILE-IDX
    END-IF.

*> ----------------------------------------------------------------
*> VIEW-MY-PROFILE: the logged-in user's own profile, rendered by
*> the exact same RENDER-PROFILE-BY-USERNAME paragraph that "View a
*> Profile" uses for looking someone else up - so what a user sees
*> here is guaranteed to be byte-for-byte identical to how their
*> profile appears to anyone else who looks it up, with no need to
*> re-type their own username.
*> ----------------------------------------------------------------
VIEW-MY-PROFILE.
    MOVE WS-USERNAME TO WS-PROFILE-USERNAME
    PERFORM RENDER-PROFILE-BY-USERNAME.

*> ----------------------------------------------------------------
*> VIEW-PROFILE: look up any username's profile by name
*> ----------------------------------------------------------------
VIEW-PROFILE.
    MOVE "Enter username to view: " TO WS-LINE-BUFFER
    MOVE FUNCTION LENGTH("Enter username to view: ") TO WS-LINE-LEN
    PERFORM WRITE-PROMPT
    PERFORM READ-INPUT-LINE
    MOVE WS-LINE-BUFFER TO WS-PROFILE-USERNAME
    PERFORM RENDER-PROFILE-BY-USERNAME.

*> ----------------------------------------------------------------
*> RENDER-PROFILE-BY-USERNAME: shared lookup+display logic for
*> WS-PROFILE-USERNAME, used by both VIEW-PROFILE and VIEW-MY-PROFILE
*> ----------------------------------------------------------------
RENDER-PROFILE-BY-USERNAME.
    MOVE "N" TO WS-PROFILE-FOUND
    PERFORM VARYING WS-PROFILE-IDX FROM 1 BY 1
        UNTIL WS-PROFILE-IDX > WS-PROFILE-COUNT
            OR WS-PROFILE-FOUND = "Y"
        IF FUNCTION TRIM(WS-TBL-PROFILE-USERNAME(WS-PROFILE-IDX)) =
            FUNCTION TRIM(WS-PROFILE-USERNAME)
            MOVE "Y" TO WS-PROFILE-FOUND
        END-IF
    END-PERFORM
    IF WS-PROFILE-FOUND = "Y"
        SUBTRACT 1 FROM WS-PROFILE-IDX
    END-IF
    IF WS-PROFILE-FOUND = "N"
        MOVE "Profile not found." TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Profile not found.") TO WS-LINE-LEN
        PERFORM WRITE-LINE
    ELSE
        MOVE "--- Profile Details ---" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("--- Profile Details ---") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "Name:" TO WS-FIELD-LABEL
        MOVE WS-TBL-FULL-NAME(WS-PROFILE-IDX) TO WS-FIELD-VALUE
        PERFORM DISPLAY-PROFILE-FIELD
        MOVE "University:" TO WS-FIELD-LABEL
        MOVE WS-TBL-UNIVERSITY(WS-PROFILE-IDX) TO WS-FIELD-VALUE
        PERFORM DISPLAY-PROFILE-FIELD
        MOVE "Major:" TO WS-FIELD-LABEL
        MOVE WS-TBL-MAJOR(WS-PROFILE-IDX) TO WS-FIELD-VALUE
        PERFORM DISPLAY-PROFILE-FIELD
        MOVE "Graduation Year:" TO WS-FIELD-LABEL
        MOVE WS-TBL-GRAD-YEAR(WS-PROFILE-IDX) TO WS-FIELD-VALUE
        PERFORM DISPLAY-PROFILE-FIELD
        MOVE "About:" TO WS-FIELD-LABEL
        MOVE WS-TBL-ABOUT(WS-PROFILE-IDX) TO WS-FIELD-VALUE
        PERFORM DISPLAY-PROFILE-FIELD
    END-IF.

DISPLAY-PROFILE-FIELD.
    MOVE 1 TO WS-STRING-PTR
    STRING FUNCTION TRIM(WS-FIELD-LABEL, TRAILING) DELIMITED BY SIZE
        " " DELIMITED BY SIZE
        FUNCTION TRIM(WS-FIELD-VALUE, TRAILING) DELIMITED BY SIZE
        INTO WS-LINE-BUFFER WITH POINTER WS-STRING-PTR
    END-STRING
    COMPUTE WS-LINE-LEN = WS-STRING-PTR - 1
    PERFORM WRITE-LINE.

*> ----------------------------------------------------------------
*> LOAD-PROFILES / SAVE-PROFILES: keep the profile store in memory
*> so an edit can safely rewrite all profiles without losing others.
*> ----------------------------------------------------------------
LOAD-PROFILES.
    MOVE ZERO TO WS-PROFILE-COUNT
    MOVE "N" TO WS-EOF
    OPEN INPUT PROFILES-FILE
    IF PROFILE-FILE-OK
        PERFORM UNTIL WS-EOF = "Y"
            READ PROFILES-FILE
                AT END
                    MOVE "Y" TO WS-EOF
                NOT AT END
                    IF WS-PROFILE-COUNT < WS-MAX-ACCOUNTS
                        ADD 1 TO WS-PROFILE-COUNT
                        MOVE PR-USERNAME TO
                            WS-TBL-PROFILE-USERNAME(WS-PROFILE-COUNT)
                        MOVE PR-FULL-NAME TO
                            WS-TBL-FULL-NAME(WS-PROFILE-COUNT)
                        MOVE PR-UNIVERSITY TO
                            WS-TBL-UNIVERSITY(WS-PROFILE-COUNT)
                        MOVE PR-MAJOR TO
                            WS-TBL-MAJOR(WS-PROFILE-COUNT)
                        MOVE PR-GRAD-YEAR TO
                            WS-TBL-GRAD-YEAR(WS-PROFILE-COUNT)
                        MOVE PR-ABOUT TO
                            WS-TBL-ABOUT(WS-PROFILE-COUNT)
                    END-IF
            END-READ
        END-PERFORM
        CLOSE PROFILES-FILE
    END-IF.

SAVE-PROFILES.
    OPEN OUTPUT PROFILES-FILE
    PERFORM VARYING WS-PROFILE-IDX FROM 1 BY 1
        UNTIL WS-PROFILE-IDX > WS-PROFILE-COUNT
        MOVE WS-TBL-PROFILE-USERNAME(WS-PROFILE-IDX) TO PR-USERNAME
        MOVE WS-TBL-FULL-NAME(WS-PROFILE-IDX) TO PR-FULL-NAME
        MOVE WS-TBL-UNIVERSITY(WS-PROFILE-IDX) TO PR-UNIVERSITY
        MOVE WS-TBL-MAJOR(WS-PROFILE-IDX) TO PR-MAJOR
        MOVE WS-TBL-GRAD-YEAR(WS-PROFILE-IDX) TO PR-GRAD-YEAR
        MOVE WS-TBL-ABOUT(WS-PROFILE-IDX) TO PR-ABOUT
        WRITE PROFILE-RECORD
    END-PERFORM
    CLOSE PROFILES-FILE.

*> ----------------------------------------------------------------
*> LEARN-SKILL-MENU: list available skills until the user goes back
*> ----------------------------------------------------------------
LEARN-SKILL-MENU.
    MOVE SPACE TO WS-MENU-CHOICE
    PERFORM UNTIL WS-MENU-CHOICE = "6"
        PERFORM WRITE-BLANK-LINE
        MOVE "--- Learn a New Skill ---" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("--- Learn a New Skill ---") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  1. Communication" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  1. Communication") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  2. Leadership" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  2. Leadership") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  3. Time Management" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  3. Time Management") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  4. Problem Solving" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  4. Problem Solving") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  5. Networking" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  5. Networking") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "  6. Go Back" TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("  6. Go Back") TO WS-LINE-LEN
        PERFORM WRITE-LINE
        MOVE "Enter your choice: " TO WS-LINE-BUFFER
        MOVE FUNCTION LENGTH("Enter your choice: ") TO WS-LINE-LEN
        PERFORM WRITE-PROMPT
        PERFORM READ-INPUT-LINE
        MOVE WS-LINE-BUFFER(1:1) TO WS-MENU-CHOICE

        EVALUATE WS-MENU-CHOICE
            WHEN "1" THRU "5"
                MOVE "This skill is under construction." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("This skill is under construction.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
            WHEN "6"
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice. Please try again." TO WS-LINE-BUFFER
                MOVE FUNCTION LENGTH("Invalid choice. Please try again.")
                    TO WS-LINE-LEN
                PERFORM WRITE-LINE
        END-EVALUATE
    END-PERFORM.

*> ----------------------------------------------------------------
*> VERIFY-CREDENTIALS: find a matching username+password in the
*> in-memory user table
*> ----------------------------------------------------------------
VERIFY-CREDENTIALS.
    MOVE "N" TO WS-LOGIN-FOUND
    PERFORM VARYING WS-TBL-IDX FROM 1 BY 1
        UNTIL WS-TBL-IDX > WS-ACCOUNT-COUNT
        IF FUNCTION TRIM(WS-TBL-USERNAME(WS-TBL-IDX)) =
            FUNCTION TRIM(WS-USERNAME)
            AND FUNCTION TRIM(WS-TBL-PASSWORD(WS-TBL-IDX)) =
            FUNCTION TRIM(WS-PASSWORD)
            MOVE "Y" TO WS-LOGIN-FOUND
        END-IF
    END-PERFORM.

*> ----------------------------------------------------------------
*> LOAD-USERS: account persistence - read every existing credential
*> from data/users.dat into WS-USER-TABLE on program startup (caps
*> at WS-MAX-ACCOUNTS, matching the 5-account limit enforced below)
*> ----------------------------------------------------------------
LOAD-USERS.
    MOVE ZERO TO WS-ACCOUNT-COUNT
    MOVE "N" TO WS-EOF
    OPEN INPUT USERS-FILE
    IF FILE-OK
        PERFORM UNTIL WS-EOF = "Y"
            READ USERS-FILE
                AT END
                    MOVE "Y" TO WS-EOF
                NOT AT END
                    IF WS-ACCOUNT-COUNT < WS-MAX-ACCOUNTS
                        ADD 1 TO WS-ACCOUNT-COUNT
                        MOVE UR-USERNAME
                            TO WS-TBL-USERNAME(WS-ACCOUNT-COUNT)
                        MOVE UR-PASSWORD
                            TO WS-TBL-PASSWORD(WS-ACCOUNT-COUNT)
                    END-IF
            END-READ
        END-PERFORM
    END-IF
    CLOSE USERS-FILE.

*> ----------------------------------------------------------------
*> OPEN-IO-FILES / CLOSE-IO-FILES: bracket the run with the scripted
*> input file and the mirrored output transcript file
*> ----------------------------------------------------------------
OPEN-IO-FILES.
    OPEN INPUT INPUT-FILE
    IF NOT INPUT-OK
        DISPLAY "ERROR: InCollege-Input.txt could not be opened." UPON SYSERR
        MOVE 1 TO RETURN-CODE
        STOP RUN
    END-IF
    CALL "creat" USING WS-OUTPUT-FILENAME BY VALUE WS-OUTPUT-MODE-BITS
        RETURNING WS-OUTPUT-HANDLE.

CLOSE-IO-FILES.
    CLOSE INPUT-FILE
    CALL "close" USING BY VALUE WS-OUTPUT-HANDLE.

*> ----------------------------------------------------------------
*> WRITE-BLANK-LINE: mirrors an empty DISPLAY " " line to both the
*> console and InCollege-Output.txt. Kept separate from WRITE-LINE
*> because a zero-length reference modification (WS-LINE-BUFFER(1:0))
*> is not valid, and a blank line has no content to size anyway.
*> ----------------------------------------------------------------
WRITE-BLANK-LINE.
    DISPLAY " "
    MOVE SPACES TO OUTPUT-RECORD
    MOVE 1 TO WS-OUTPUT-LEN
    PERFORM WRITE-OUTPUT-RECORD.

*> ----------------------------------------------------------------
*> WRITE-LINE: core output-mirroring routine. Displays exactly the
*> first WS-LINE-LEN characters of WS-LINE-BUFFER on the console
*> (matching what the original DISPLAY of that literal/built string
*> would have shown, trailing spaces and all) and writes the same
*> text as the next line of InCollege-Output.txt. Every DISPLAY in
*> this program is replaced by MOVE ... TO WS-LINE-BUFFER + MOVE the
*> matching FUNCTION LENGTH(...) TO WS-LINE-LEN + PERFORM WRITE-LINE,
*> so console and file output can never drift apart, and no message
*> ever loses meaningful trailing spaces to buffer-padding cleanup.
*> ----------------------------------------------------------------
WRITE-LINE.
    DISPLAY WS-LINE-BUFFER(1:WS-LINE-LEN)
    MOVE SPACES TO OUTPUT-RECORD
    MOVE WS-LINE-BUFFER(1:WS-LINE-LEN) TO OUTPUT-RECORD
    MOVE WS-LINE-LEN TO WS-OUTPUT-LEN
    PERFORM WRITE-OUTPUT-RECORD
    MOVE SPACES TO WS-LINE-BUFFER
    MOVE ZERO TO WS-LINE-LEN.

*> ----------------------------------------------------------------
*> WRITE-PROMPT: same as WRITE-LINE but keeps the console cursor on
*> the same line (WITH NO ADVANCING), for prompts immediately
*> followed by a READ-INPUT-LINE
*> ----------------------------------------------------------------
WRITE-PROMPT.
    DISPLAY WS-LINE-BUFFER(1:WS-LINE-LEN) WITH NO ADVANCING
    MOVE WS-LINE-BUFFER TO WS-PROMPT-BUFFER
    MOVE WS-LINE-LEN TO WS-PROMPT-LEN
    MOVE SPACES TO WS-LINE-BUFFER
    MOVE ZERO TO WS-LINE-LEN.

*> ----------------------------------------------------------------
*> READ-INPUT-LINE: core input-mirroring routine, used everywhere
*> the program used to ACCEPT from the keyboard. Reads the next
*> scripted line from InCollege-Input.txt into WS-LINE-BUFFER (for
*> the caller to MOVE into whatever field it needs), echoes it to
*> the console, and mirrors that same echoed line into
*> InCollege-Output.txt - since the input is not typed live, nothing
*> would otherwise show it was received. Trailing padding is trimmed
*> here (unlike WRITE-LINE/WRITE-PROMPT) because a scripted input
*> line has no meaningful trailing spaces of its own to preserve.
*> Running out of scripted input ends the program cleanly.
*> ----------------------------------------------------------------
READ-INPUT-LINE.
    MOVE SPACES TO INPUT-RECORD
    READ INPUT-FILE
        AT END
            PERFORM CLOSE-IO-FILES
            STOP RUN
    END-READ
    MOVE SPACES TO WS-LINE-BUFFER
    MOVE INPUT-RECORD TO WS-LINE-BUFFER
    DISPLAY FUNCTION TRIM(WS-LINE-BUFFER, TRAILING)
    MOVE SPACES TO OUTPUT-RECORD
    MOVE WS-PROMPT-BUFFER(1:WS-PROMPT-LEN) TO OUTPUT-RECORD
    MOVE FUNCTION TRIM(WS-LINE-BUFFER, TRAILING) TO
        OUTPUT-RECORD(WS-PROMPT-LEN + 1:400 - WS-PROMPT-LEN)
    COMPUTE WS-OUTPUT-LEN = WS-PROMPT-LEN +
        FUNCTION LENGTH(FUNCTION TRIM(WS-LINE-BUFFER, TRAILING))
    PERFORM WRITE-OUTPUT-RECORD
    MOVE SPACES TO WS-PROMPT-BUFFER
    MOVE ZERO TO WS-PROMPT-LEN.

WRITE-OUTPUT-RECORD.
    CALL "write" USING BY VALUE WS-OUTPUT-HANDLE OUTPUT-RECORD
        BY VALUE WS-OUTPUT-LEN
        RETURNING WS-WRITE-COUNT
    CALL "write" USING BY VALUE WS-OUTPUT-HANDLE WS-OUTPUT-NEWLINE
        BY VALUE 1
        RETURNING WS-WRITE-COUNT.
