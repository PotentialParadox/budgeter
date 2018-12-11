module Budget.Budgeter(
  budgeter
  )where

import Numeric.LinearAlgebra
import Budget.Types
import Budget.Parse
import Budget.Printer
import Text.Parsec
import Finance.FutureValue

getNeededExpense :: BudgetItem -> Expense
getNeededExpense bs = case needed bs of
  True -> Expense (name bs) (minPrice bs)
  False -> Expense (name bs) 0.0

reduceBs :: [BudgetItem] -> [Expense] -> [BudgetItem]
reduceBs bs xs = map removeMin $ zip bs xs

removeMin :: (BudgetItem, Expense) -> BudgetItem
removeMin ((BudgetItem s i l h n), Expense _ m) = BudgetItem s i (l-m) (h-m) n

aDiag :: BudgetItem -> Double
aDiag (BudgetItem _ i low high _)
  | low == high = 100000
  | otherwise = (-2.0) * i * k**2
  where k = 1.0 / (high-low)

aDiags :: [BudgetItem] -> [Double]
aDiags = map aDiag

type CurrentIndex = Integer
type Rank = Integer
aList :: CurrentIndex -> Rank -> [Double] -> [Double]
aList c r (x:xs)
  |c `mod` (r+1) == 0 = x : aList (c+1) r xs
aList c r xs
  | c == r^2-1 = [0]
  | ((c+1) `mod` r == 0) && (c /= r^2-1) = -1 : aList (c+1) r xs
  | (c `mod` (r+1) /= 0 && c >= r^2-r) = 1 : aList (c+1) r xs
  | otherwise = 0 : aList (c+1) r xs

availableCosts :: Double -> Double -> Double -> Double -> Double -> Double
availableCosts cpm y v ipm apr
  | cpm <= 0 = 0
  | retirement > sustainableRetirement = cpm
  | otherwise = availableCosts (cpm-10) y v ipm apr
  where retirement = futureValue numberMonths v (ipm-cpm) ratePerMonth
        sustainableRetirement = costsPerYear / apr
        numberMonths = floor (12 * y)
        ratePerMonth = apr/12
        costsPerYear = cpm * 12

aMatrix :: [BudgetItem] -> Matrix Double
aMatrix bs = let as = aDiags bs
                 r = (length as) + 1
             in (r><r) $ aList 0 (toInteger r) as

bElem :: BudgetItem -> Double
bElem (BudgetItem _ i low high _)
  | low == high = 100000
  | otherwise = (-2.0) * i * k**2 * high
  where k = 1.0 / (high-low)

bElems :: [BudgetItem] -> [Double]
bElems = map bElem

type TotalCost = Double
bVector :: [BudgetItem] -> TotalCost -> Vector Double
bVector bs tc = let vecElems = bElems bs ++ [tc]
                in vector vecElems

removeNeeded :: BudgetInfo -> (BudgetInfo, [Expense])
removeNeeded (BudgetInfo i s y a bs) = (BudgetInfo (i - totalNeeded) s y a newBs, needs)
  where needs = map getNeededExpense bs
        newBs = reduceBs bs needs
        totalNeeded = sum [x | (Expense _ x) <- needs]

type ItemNames = [String]
calculateBudget :: BudgetInfo -> [Expense]
calculateBudget (BudgetInfo i s y a bs)
  | i > totalMaxCosts = expenses
  | otherwise = calculateBudget' [] (BudgetInfo i s y a bs)
  where maxCosts = map maxPrice bs
        names = map name bs
        expenses = [Expense n m | (n, m) <- zip names maxCosts]
        totalMaxCosts = sum maxCosts

inefficientNames :: [BudgetItem] -> Costs -> ItemNames
inefficientNames bs costs  = [(name b)| (b,c) <- (zip bs costs), c < 0]

optimizedCosts :: Double -> [BudgetItem] -> [Double]
optimizedCosts i bs = init $ toList $ (aMatrix bs) <\> (bVector bs (realToFrac i))

calculateBudget' :: ItemNames -> BudgetInfo -> [Expense]
calculateBudget' unusedItems (BudgetInfo i s y a bs)
  | validCosts = expenses
  | otherwise = calculateBudget' newUnusedNames cleanedBI
  where costs = optimizedCosts i bs
        validCosts = length [c | c <- costs, c < 0] == 0
        allNames = (map name bs) ++ unusedItems
        newUnusedNames = unusedItems ++ inefficientNames bs costs
        cleanedBI = BudgetInfo i s y a [b | (b,c) <- (zip bs costs), c > 0]
        appendedCosts = costs ++ (replicate ((length allNames) - (length costs)) 0)
        expenses = [Expense n m | (n, m) <- zip allNames appendedCosts]

storeSavings (BudgetInfo ipm v ytr apr bs) = BudgetInfo cpm v ytr apr bs
  where cpm = availableCosts ipm ytr v ipm apr

unsafe :: Either ParseError BudgetInfo -> BudgetInfo
unsafe (Right x) = x

yearlySavings (BudgetInfo ipm _ _ _ _) cs = ipm - (sum cs)

combineExpenses :: [Expense] -> [Expense] -> [Expense]
combineExpenses e1s e2s = [Expense n1 (cost1 + cost2) | (Expense n1 cost1) <- e1s, (Expense n2 cost2) <- e2s,
                           n1 == n2]

getBudget :: BudgetInfo -> String
getBudget bi = let saved_bi = storeSavings bi
                   (available_bi, mandatory_costs) = removeNeeded saved_bi
                   distro = calculateBudget available_bi
                   finalExpenses = combineExpenses mandatory_costs distro
               in fullPrint bi finalExpenses

budgeter :: String -> String
budgeter s = let bi = parse getBudgetInfo "Fetching budget info" s
             in case fmap getBudget bi of
                Left parsecError -> "Encountered Input Parsing errror:\n" ++ show parsecError
                Right finalString -> finalString
