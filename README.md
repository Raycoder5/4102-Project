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
   - Download from the [official GnuCOBOL website](https://gnucobol.sourceforge.io/).
   - Follow the installation instructions for your operating system.
2. **Supported Operating System**: The program works on Windows, Linux, and macOS.

---

## Setup and Compilation
1. **Install GnuCOBOL**  
   Follow the installation guide provided on the GnuCOBOL website for your operating system.

2. **Save the Source Code**  
   Save the COBOL source code into a file named `COBOL_ATM.cob`. Use the `.cob` or `.cbl` file extension.

3. **Compile the Program**  
   Open a terminal or command prompt, navigate to the directory containing the `COBOL_ATM.cob` file, and run:  
   ```bash
   cobc -x -free -o COBOL_ATM COBOL_ATM.cob
