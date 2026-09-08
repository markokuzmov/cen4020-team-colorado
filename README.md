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
| User account store | `data/users.dat` | Line-sequential flat file; auto-created on first registration. Each record is 32 bytes: 20-char username + 12-char password. |

> **Note:** `data/users.dat` is listed in `.gitignore` and is never committed — it is generated at runtime.

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
| *(add your names here)* | |
