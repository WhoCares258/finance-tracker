# WhoCares Personal Finance Tracker

A command-line personal finance tracker built in Haskell for recording, managing, and analysing income and expenses. The project applies functional programming principles to an interactive application with persistent storage and financial analytics.

## Demo

[Watch the demonstration on YouTube](https://youtu.be/TZ4GRFaMjcU)

<p>
  <img src="docs/images/main-menu.png" alt="Main menu" width="49%">
  <img src="docs/images/account-summary.png" alt="Account summary and financial analytics" width="43%">
</p>

## Features

- Add, view, edit, and delete income and expense transactions.
- Track running balances, total income, total expenses, averages, and the highest expense.
- Filter transactions across 12 predefined categories and view percentage distributions.
- Search transaction descriptions using case-insensitive keywords.
- Validate dates, amounts, categories, and transaction IDs.
- Load and save transactions using CSV, with sample data as a fallback.

## Technical Implementation

- **Haskell:** Algebraic data types, typeclasses, pattern matching, and pure functions.
- **Validation:** Custom validation monad with `Functor`, `Applicative`, and `Monad` instances.
- **Data processing:** List operations and folds for financial calculations and running balances.
- **Persistence:** Cassava for CSV encoding and decoding, with exception handling for file access.
- **Architecture:** Separation of pure business logic from interactive I/O.

## Getting Started

Install [Stack](https://docs.haskellstack.org/) and run:

```bash
git clone https://github.com/WhoCares258/finance-tracker.git
cd finance-tracker
stack build
stack run
```

Transactions are loaded from `ledger.csv` and saved when exiting through the menu.

## License

BSD-3-Clause
