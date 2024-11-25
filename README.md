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

Explanation of the command:
<ul>
    <li>cobc: Invokes the GnuCOBOL compiler.</li>
    <li>-x: Creates an executable file.</li>
    <li>-free: Indicates that the source code uses free-format (not fixed-format).</li>
    <li>-o COBOL_ATM: Specifies the output executable file name (COBOL_ATM).</li>
    <li>COBOL_ATM.cob: The COBOL source file to compile.</li>
</ul>

---

## Running the Program
**To run the compiled program, use the following command in your terminal**
```bash
./COBOL_ATM
```
**On Windows, you need to use:**
```bash
COBOL_ATM.exe
```

---

## Usage Instructions
Upon running the program, you will be presented with a menu of options:
```bash
Welcome to COBOL Bank ATM System.
1. Create Account
2. Deposit Funds
3. Withdraw Funds
4. View Account
5. Exit
6. Transfer Funds
7. Close Account
8. Update Account Information
Enter your choice:
```

**Menu Options**

<ol>
    <li><em>Create Account</em>:
        <ul>
            <li><b>Purpose</b>: Opens a new account</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter the required personal and card details when prompted.</li>
                    <li>An account number will be automatically assigned.</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>Deposit Funds</em>:
        <ul>
            <li><b>Purpose</b>: Adds money to an existing account.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your account number.</li>
                    <li>Enter the amount to deposit</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>Withdraw Funds</em>:
        <ul>
            <li><b>Purpose</b>: Withdraws money from an existing account.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your account number.</li>
                    <li>Enter the amount to withdraw.</li>
                    <li>Ensure that the account has sufficient funds.</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>View Account</em>:
        <ul>
            <li><b>Purpose</b>: Displays account details and balance.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your account number to view the information.</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>Exit</em>:
        <ul>
            <li><b>Purpose</b>: Exits the application.</li>
        </ul>
    </li>
    <li><em>Transfer Funds</em>:
        <ul>
            <li><b>Purpose</b>: Transfers money from one account to another.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your (source) account number.</li>
                    <li>Enter the destination account number.</li>
                    <li>Enter the amount to transfer.</li>
                    <li>Ensure both accounts exist and the source account has sufficient funds.</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>Close Account</em>:
        <ul>
            <li><b>Purpose</b>: Closes an existing account and removes it from the system.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your account number.</li>
                    <li>Confirm the closure when prompted.</li>
                </ul>
            </li>
        </ul>
    </li>
    <li><em>Update Account Information</em>:
        <ul>
            <li><b>Purpose</b>: Updates personal and card details of an existing account.</li>
            <li><b>Instructions</b>
                <ul>
                    <li>Enter your account number.</li>
                    <li>Enter new details when prompted.</li>
                </ul>
            </li>
        </ul>
    </li>
</ol>

---

## Data Files
<ul>
    <li><b>Account Data File</b>: The program uses a file named <em>card.dat</em> to store account information persistently.
        <ul>
            <li><b>Location</b>: Created in the same directory as the executable.</li>
            <li><b>Format</b>: Line sequential file with records containing account details.</li>
        </ul>
    </li>
    <li><b>Temporary Data File</b>: A temporary file named <em>temp.dat</em> is used during account updates and closures.
        <ul>
            <li><b>Purpose</b>: To rewrite the data file while omitting or updating specific records.</li>
            <li><b>Note</b>: The program handles deletion of temp.dat after operations.</li>
        </ul>
    </li>
</ul>

---

## Notes
<ul>
    <li><b>Data Persistance</b>: Account information is stored in card.dat. Deleting this file will result in loss of all account data.</li>
    <li><b>Account Numbers</b>: Assigned sequentially starting from 1. Ensure that card.dat is not deleted to maintain account number continuity.</li>
    <li><b>Numeric Input</b>: Ensure that numeric inputs (e.g., account numbers, amounts) contain only digits to prevent input errors.</li>
</ul>

---

## Troubleshooting
