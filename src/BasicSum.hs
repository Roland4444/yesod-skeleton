module BasicSum
    (basicSum, isPositive, safeDivide) where

-- Простая функция для демонстрации
basicSum :: Int -> Int -> Int
basicSum a b = a + b

-- Еще одна функция для тестирования
isPositive :: Int -> Bool
isPositive x = x > 0

-- Функция с обработкой краевого случая
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a `div` b)