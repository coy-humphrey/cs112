------------------------------
-- Program: hw1.hs
-- Authors: Coy Humphrey
------------------------------

citeAuthor :: String -> String -> String
citeAuthor f l = l ++ ", " ++ f


initials :: String -> String -> String
initials (f:_) (l:_) = f:'.':l:"."


title :: (String, String, Int) -> String
title (_, t, _) = t


citeBook :: (String, String, Int) -> String
citeBook (a, t, y) = t ++ " (" ++ a ++ ", " ++ (show y) ++ ")"


bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = []
bibliography_rec (h:t) = citeBook h ++ "\n" ++ bibliography_rec t


bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold l = foldl addCiting [] l
                    where addCiting x y = x ++ citeBook y ++ "\n"


averageYear :: [(String, String, Int)] -> Int
averageYear l = sum (map year l) `div` (length l)


-- Helper function for averageYear
year :: (String, String, Int) -> Int
year (_,_, y) = y


references :: String -> Int
references s = length (filter isRef (words s))


-- Helper function for references
-- References in form of [n], so must have length 3 or greater
-- References must start and end with brackets
-- Inside of brackets must be a number
isRef :: String -> Bool
isRef s 
      | length s < 3 = False
      | not (head s == '[' && last s == ']') = False
      | otherwise = isNumeric (trimBrackets s)


-- Helper function
trimBrackets :: String -> String
trimBrackets w = tail (init w)


-- Helper function for isRef. Relies on initial string having length > 0
isNumeric :: String -> Bool
isNumeric [] = True
isNumeric (h:t) = h `elem` ['0'..'9'] && isNumeric t


citeText :: [(String, String, Int)] -> String -> String
citeText l s = unwords [replaceRef w l | w <- (words s)]


-- Helper function for citeText
-- Replaces a valid reference tag with the corresponding citing.
-- Returns the input if not a valid reference
replaceRef :: String -> [(String, String, Int)] -> String
replaceRef w l
             | not (isRef w) = w
             | num > length l = w
             | num < 1 = w
             | otherwise = citeBook (l !! (num - 1))
             where num = (read (trimBrackets w) :: Int)
