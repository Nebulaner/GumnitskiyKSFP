-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}

myZipSave :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZipSave [] [] = ([], Left [])
myZipSave [] bs = ([], Right bs)
myZipSave as [] = ([], Left as)
myZipSave (a:as) (b:bs) = ((a,b):pairs, rest)
    where (pairs, rest) = myZipSave as bs

myUnzipSave :: ([(a, b)], Either [a] [b]) -> ([a], [b])
myUnzipSave ([], Left restA) = (restA, [])
myUnzipSave ([], Right restB) = ([], restB)
myUnzipSave ((a,b):pairs, rest) = (a:as, b:bs)
    where (as, bs) = myUnzipSave (pairs, rest)

strictFoldl :: (b -> a -> b) -> b -> [a] -> b
strictFoldl _ acc [] = acc
strictFoldl f acc (x:xs) = let acc' = f acc x in acc' `seq` strictFoldl f acc' xs

main :: IO()
main = do
    let list1 = [1, 2, 3, 4, 5]
    let list2 = ["a", "b", "c"]
    let list3 = [True, False]
    print $ myZipSave list1 list2
    print $ myZipSave list2 list3
    
    let zipped1 = myZipSave list1 list2
    let zipped2 = myZipSave list2 list3
    print $ myUnzipSave zipped1
    print $ myUnzipSave zipped2