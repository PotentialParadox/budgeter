module Budget.Printer(
  fullPrint
  )where
import Budget.Types
import Text.Printf
import Data.Currency.Pretty
import Finance.FutureValue

fullPrint :: BudgetInfo -> Expenses -> String
fullPrint bi expenses = headers ++ budgets ++ "\n" ++ summary
  where headers = showHeader bi
        budgets = showExpenses expenses
        finalCosts = [c | (Expense _ c) <- expenses]
        summary = showSummary bi finalCosts

showHeader (BudgetInfo ipm acs ytr apr _) =
  printf "After-Tax Monthly Income: %s\n\
         \Years till retirement: %-14.2f\n\
         \Amount currently saved: %s\n\
         \Expected APR: %-14.2f\n\n\
         \Your budget (per month) is as follows:\n" (usd ipm) ytr (usd acs) apr

showExpenses :: [Expense] -> String
showExpenses expenses = unlines [printf "%-25s %12.2f" i c | (Expense i c) <- expenses]

showSummary :: BudgetInfo -> Costs -> String
showSummary (BudgetInfo incomePerMonth currentSavings ytr apr _) costs =
  printf "Costs per year: %s\n\
         \Savings per year: %s\n\
         \Total savings at retirement: %s\n" (usd costsPerYear) (usd savingsPerYear) (usd retirement)
         where costsPerMonth = sum costs
               costsPerYear = 12 * costsPerMonth
               savingsPerMonth = incomePerMonth - costsPerMonth
               savingsPerYear = 12 * savingsPerMonth
               numberMonths = floor (12 * ytr)
               ratePerMonth = apr / 12.0
               retirement = futureValue numberMonths currentSavings savingsPerMonth ratePerMonth

usd :: Double -> String
usd x = prettyPrint (Amount USD x)
