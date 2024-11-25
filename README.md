# COBOL ATM System - README

## Table of Contents
1. [Introduction](#introduction)
2. [Features](#features)
3. [Prerequisites](#prerequisites)
4. [Setup and Compilation](#setup-and-compilation)
5. [Running the Program](#running-the-program)
6. [Usage Instructions](#usage-instructions)
7. [Data Files](#data-files)
8. [Notes](#notes)
9. [Troubleshooting](#troubleshooting)

---

## Introduction
The **COBOL ATM System** is a console-based application that simulates basic banking operations. Developed as a project to demonstrate proficiency in COBOL, the system allows users to perform various banking tasks, such as creating accounts, managing funds, transferring money, and updating account details.

---

## Features
- **Account Creation**: Open new accounts with personal and card details.
- **Deposits and Withdrawals**: Deposit funds or withdraw money from accounts.
- **Funds Transfer**: Transfer money between accounts within the system.
- **Account Inquiry**: View account details and balances.
- **Account Closure**: Remove accounts from the system.
- **Account Updates**: Update personal and card information for accounts.

---

## Prerequisites
To compile and run the program, ensure you have the following:
1. **GnuCOBOL Compiler**: Version 3.2 or later is recommended.
    GnuCOBOL is an open-source COBOL compiler that translates COBOL code into C code, which is      then compiled into an executable. You can download it from:
   - [official GnuCOBOL website](https://gnucobol.sourceforge.io/).
   - Installation instructions are available on the website.
2. **Supported Operating System**: The program should work on any operating system where             GnuCOBOL is supported, including Windows, Linux, and macOS.

---

## Setup and Compilation
1. **Install GnuCOBOL**  
   Follow the installation instructions specific to your operating system provided on the GnuCOBOL website or documentation.

2. **Save the Source Code**  
   Copy the COBOL source code into a file named COBOL_ATM.cob. Ensure that the file extension is .cob or .cbl, which are standard for COBOL programs.

3. **Compile the Program**  
   Open your terminal or command prompt and navigate to the directory containing the COBOL_ATM.cob file.
Run the following command to compile the program:
   ```bash
   cobc -x -free -o COBOL_ATM COBOL_ATM.cob
   '''


