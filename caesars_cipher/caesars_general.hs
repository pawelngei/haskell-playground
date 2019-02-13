import Data.Char

count x xs = length [x' | x' <- xs, x == x']

positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- lowers will not be needed, use letters instead
-- lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
letters xs = length [x | x <- xs, isLetter x]

let2intLower c = ord c - ord 'a'
let2intCaptl c = ord c - ord 'A'

-- so this is a problem: how to translate capitals? they need some additional sign
int2letLower n = chr (ord 'a' + n)
int2letCaptl n = chr (ord 'A' + n)

shift n c | isLower c = int2letLower ((let2intLower c + n) `mod` 26)
          | otherwise = int2letCaptl ((let2intCaptl c + n) `mod` 26)

-- shift2 with programmatic offset and where
getCharId offset c = ord c - offset
idToChar  offset c = chr (offset + c)

shift2 n c = idToChar offset (((getCharId offset c) + n) `mod` 26)
            where offset = if isLower c then ord 'a' else ord 'A'

encode n xs = [shift2 n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7,  7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4,  0.2, 2.0, 0.1]

percent n m = (fromIntegral n / fromIntegral m) * 100

freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = letters xs

chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
