myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:x2) = x : myTake (n-1) x2

myDrop :: Int -> [a] -> [a]
myDrop 0 x2 = x2
myDrop _ [] = []
myDrop n (_:x2) = myDrop (n-1) x2

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:x2) = x * myProduct x2

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:x2) (y:y2) = (x, y) : myZip x2 y2

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:x2) (y:y2) (z:z2) = (x, y, z) : myZip3 x2 y2 z2

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y):pairs) = (x:x2, y:y2)
    where (x2, y2) = myUnzip pairs


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:x2)
    | p x = x : myFilter p x2
    | otherwise = myFilter p x2

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' p (x:x2) = if p x then x : myFilter' p x2 else myFilter' p x2

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:x2) = f x : myMap f x2

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:x2) (y:y2) = f x y : myZipWith f x2 y2

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 _ [] _ _ = []
myZipWith3 _ _ [] _ = []
myZipWith3 _ _ _ [] = []
myZipWith3 f (x:x2) (y:y2) (z:z2) = f x y z : myZipWith3 f x2 y2 z2

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll p (x:x2) = p x && myAll p x2

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x:x2) = p x || myAny p x2

myComposition :: (b -> c) -> (a -> b) -> (a -> c)
myComposition f g = \x -> f (g x)

main :: IO()
main = do
    let tup = ("a", "b", "c")
    putStrLn $ "myFST: " ++ show (myFST tup)
    putStrLn $ "my2ND: " ++ show (my2ND tup)
    putStrLn $ "myTHRD: " ++ show (myTHRD tup)
    putStrLn ""

    let list1 = [1, 2, 3, 4, 5]
    let list2 = ["a", "b", "c", "d"]
    let list3 = [True, False, True]
    
    putStrLn $ "myHead list1: " ++ show (myHead list1)
    putStrLn $ "myTail list1: " ++ show (myTail list1)
    putStrLn $ "myTake 3 list1: " ++ show (myTake 3 list1)
    putStrLn $ "myDrop 2 list1: " ++ show (myDrop 2 list1)
    putStrLn $ "myProduct list1: " ++ show (myProduct list1)
    putStrLn $ "myZip list1 list2: " ++ show (myZip list1 list2)
    putStrLn $ "myZip3 list1 list2 list3: " ++ show (myZip3 list1 list2 list3)
    
    let pairs = myZip list1 list2
    putStrLn $ "myUnzip pairs: " ++ show (myUnzip pairs)
    putStrLn ""

    putStrLn $ "myFilter (>2) list1: " ++ show (myFilter (>2) list1)
    putStrLn $ "myFilter' (>2) list1: " ++ show (myFilter' (>2) list1)
    putStrLn $ "myMap (*2) list1: " ++ show (myMap (*2) list1)
    putStrLn $ "myZipWith (+) list1 list1: " ++ show (myZipWith (+) list1 list1)
    putStrLn $ "myZipWith3 (\\x y z -> (x,y,z)) list1 list2 list3: " ++ show (myZipWith3 (\x y z -> (x,y,z)) list1 list2 list3)
    putStrLn $ "myAll (>0) list1: " ++ show (myAll (>0) list1)
    putStrLn $ "myAll (>3) list1: " ++ show (myAll (>3) list1)
    putStrLn $ "myAny (>4) list1: " ++ show (myAny (>4) list1)
    putStrLn $ "myAny (>10) list1: " ++ show (myAny (>10) list1)
    
    let composed = myComposition (*2) (+3)
    putStrLn $ "myComposition (*2) (+3) $ 5: " ++ show (composed 5)