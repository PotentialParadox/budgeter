module Budget.Types
  (BudgetInfo(BudgetInfo),
   BudgetItem(BudgetItem),
   income,
   savings,
   yearsTillRetirement,
   annualPercentageRate,
   items,
   name,
   importance,
   minPrice,
   maxPrice,
   needed,
   Expense(Expense),
   Expenses,
   Costs
   ) where

data BudgetInfo = BudgetInfo { income :: Double
                             , savings :: Double
                             , yearsTillRetirement :: Double
                             , annualPercentageRate :: Double
                             , items :: [BudgetItem]}deriving(Show)

data BudgetItem = BudgetItem {name :: String
                             , importance :: Double
                             , minPrice :: Double
                             , maxPrice :: Double
                             , needed :: Bool} deriving(Read, Show)

data Expense = Expense String Double
type Expenses = [Expense]
type Costs = [Double]
