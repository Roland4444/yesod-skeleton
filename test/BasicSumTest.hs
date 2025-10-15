module Main where

import Test.HUnit
import qualified System.Exit as Exit
import BasicSum
-- cabal test --enable-tests --allow-newer=base
-- Тесты для basicSum
testBasicSumPositive :: Test
testBasicSumPositive = TestCase $
    assertEqual "Сумма положительных чисел" 5 (basicSum 2 3)

testBasicSumZero :: Test
testBasicSumZero = TestCase $
    assertEqual "Сумма с нулем" 3 (basicSum 3 0)

testBasicSumNegative :: Test
testBasicSumNegative = TestCase $
    assertEqual "Сумма с отрицательным числом" (-1) (basicSum 2 (-3))

-- Тесты для isPositive
testIsPositiveTrue :: Test
testIsPositiveTrue = TestCase $
    assertBool "Положительное число" (isPositive 5)

testIsPositiveFalse :: Test
testIsPositiveFalse = TestCase $
    assertEqual "Отрицательное число" False (isPositive (-5))

testIsPositiveZero :: Test
testIsPositiveZero = TestCase $
    assertEqual "Ноль не положительный" False (isPositive 0)

-- Тесты для safeDivide
testSafeDivideNormal :: Test
testSafeDivideNormal = TestCase $
    assertEqual "Нормальное деление" (Just 5) (safeDivide 10 2)

testSafeDivideByZero :: Test
testSafeDivideByZero = TestCase $
    assertEqual "Деление на ноль" Nothing (safeDivide 10 0)

testSafeDivideFraction :: Test
testSafeDivideFraction = TestCase $
    assertEqual "Целочисленное деление" (Just 3) (safeDivide 10 3)

-- Группировка всех тестов
tests :: Test
tests = TestList
    [ TestLabel "testBasicSumPositive" testBasicSumPositive
    , TestLabel "testBasicSumZero" testBasicSumZero
    , TestLabel "testBasicSumNegative" testBasicSumNegative
    , TestLabel "testIsPositiveTrue" testIsPositiveTrue
    , TestLabel "testIsPositiveFalse" testIsPositiveFalse
    , TestLabel "testIsPositiveZero" testIsPositiveZero
    , TestLabel "testSafeDivideNormal" testSafeDivideNormal
    , TestLabel "testSafeDivideByZero" testSafeDivideByZero
    , TestLabel "testSafeDivideFraction" testSafeDivideFraction
    ]

-- Главная функция
main :: IO ()
main = do
    count <- runTestTT tests
    if errors count > 0 || failures count > 0
        then Exit.exitWith (Exit.ExitFailure 1)
        else Exit.exitWith Exit.ExitSuccess