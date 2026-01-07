# WhoCares Personal Finance Tracker 💰

A command-line personal finance tracker built in Haskell that helps you manage your income and expenses with ease.

## What It Does

WhoCares is a simple yet powerful CLI tool that allows you to:

- **Track all your transactions** - Record every income and expense with dates, categories, and descriptions
- **Manage your finances** - Add, edit, delete, and view all transactions in an organized table format
- **Analyze spending patterns** - View totals, averages, running balances, and highest expenses
- **Categorize transactions** - Organize money flow across 12 predefined categories (Food, Rent, Transport, Salary, Entertainment, Utilities, Healthcare, Education, Shopping, Investment, Travel, Other)
- **Search your history** - Find transactions by description using case-insensitive keyword search
- **Visualize distribution** - See percentage breakdowns of how your money is distributed across categories
- **Filter by category** - View all transactions for a specific category
- **Track running balance** - See how your balance changes after each transaction
- **Persist your data** - All transactions are automatically saved to CSV format and loaded on startup

## Features

✅ **Interactive Menu System** - Easy-to-use numbered menu for all operations  
✅ **Smart Date Input** - Press Enter to use today's date, or specify custom dates  
✅ **Running Balance Display** - See cumulative balance after each transaction  
✅ **Category Distribution** - Visual percentage breakdown of expenses and income  
✅ **Validation** - Built-in input validation for dates, amounts, and categories  
✅ **Persistent Storage** - CSV-based storage that survives between sessions  
✅ **Sample Data** - Includes sample transactions on first run for demonstration  
✅ **Search Functionality** - Find transactions quickly by searching descriptions  
✅ **Financial Analytics** - Automatic calculation of totals, averages, and net balance  
✅ **Edit Without Retyping** - Keep existing values when editing transactions

## Categories Supported

Food | Rent | Transport | Salary | Entertainment | Utilities | Healthcare | Education | Shopping | Investment | Travel | Other

## Getting Started

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)

### Run the Application

```bash
stack build
stack run
```

### Main Menu Options

```
1. View transactions              - See all transactions with running balance
2. Add transaction                - Record a new income or expense
3. Edit transaction               - Modify an existing transaction
4. Delete transaction             - Remove a transaction by ID
5. View totals                    - See income, expenses, net balance, and averages
6. View by category               - Filter transactions by a specific category
7. View category distribution     - See percentage breakdown by category
8. Search transactions            - Find transactions by description keyword
0. Exit and save                  - Save all changes and quit
```

## License

BSD-3-Clause