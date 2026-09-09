# InCollege
**LinkedIn for College Students** — CEN4020 Team Colorado

---

## Project Overview

InCollege is a COBOL-based networking platform that lets college students create accounts and log in, mimicking core LinkedIn functionality in a terminal environment.

---

## Branching Strategy

| Branch | Purpose |
|--------|---------|
| `main` | Stable, reviewed code — only merge via pull request |
| `develop` | Active integration branch — all features merge here first |
| `feature/<task-name>` | One branch per sprint task (e.g. `feature/user-registration`) |
| `hotfix/<issue>` | Emergency fixes branched off `main` |

**Workflow:** create a `feature/` branch → open a PR into `develop` → team reviews → merge into `main` at sprint end.

---

## Compile Instructions

Requires **GnuCOBOL** (`cobc`).

```bash
# Install GnuCOBOL (if not already installed)
# Ubuntu/Debian:  sudo apt install gnucobol
# macOS (Homebrew): brew install gnu-cobol

# Compile
cobc -x -o incollege src/INCOLLEGE.cbl
```

---

## Run Instructions

The program reads every menu choice, username, and password from
`InCollege-Input.txt` (one value per line) instead of the keyboard, so that
file must exist in the working directory before you run it:

```bash
./incollege
```

The program presents an interactive menu:

```
========================================
          Welcome to InCollege
    LinkedIn for College Students
========================================
  1. Create New Account
  2. Log In
  3. Exit
========================================
```

---

## Input / Output Files

| File | Location | Description |
|------|----------|-------------|
| Scripted input | `InCollege-Input.txt` | Line-sequential; one menu choice/username/password per line, read in place of keyboard `ACCEPT`. Must exist before running. |
| Output transcript | `InCollege-Output.txt` | Line-sequential; mirrors every line shown on the console, including each echoed input line, in the same order. Overwritten on each run. |
| User account store | `data/users.dat` | Line-sequential flat file; auto-created on first registration, loaded into memory on startup. Each record is 32 bytes: 20-char username + 12-char password. |

> **Note:** `data/users.dat`, `InCollege-Input.txt`, and `InCollege-Output.txt` are all listed in `.gitignore` and never committed — they are run-time/local-test artifacts, not source.

---

## Account Capacity

Only **5 accounts** may exist at a time (enforced against the accounts loaded
from `data/users.dat` on startup plus any created so far in the current run).
Choosing "Create New Account" past that limit immediately shows:

```
All permitted accounts have been created, please come back later
```

without prompting for a username or password.

---

## Password Rules (Registration)

| Rule | Requirement |
|------|------------|
| Length | 8 – 12 characters |
| Uppercase | At least one letter A–Z |
| Digit | At least one character 0–9 |
| Special | At least one of `!@#$%^&*()-_+=` |

---

## Team Members — Colorado

| Name | Role |
|------|------|
| Oomat Latipov | | Developer
