{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate1 = last . init

penultimate2 [] = error "empty list"
penultimate2 [x] = error "too few elements"
penultimate2 [x,_] = x
penultimate2 (_:xs) = penultimate2 xs


penultimate3 [] = error "empty list"
penultimate3 [x] = error "too few elements"
penultimate3 xs = xs !! (length xs - 2)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
findK xs k = xs !! (k-1)

findK2 [] _ = error "index out of bounds"
findK2 (x:_) 0 = x
findK2 (_:xs) k
  | k < 0 = error "index out of bounds"
  | otherwise = findK2 xs (k-1)

findK3 xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = fst . last $ zip xs [0..n]

-- Determine if list l is a palindrome
isPalindrome xs = reverse xs == xs

isPalindrome2 [_] = True
isPalindrome2 [] = True
isPalindrome2 xs
  | head xs == last xs = isPalindrome2 (tail $ init xs)
  | otherwise = False

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list.
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}

duplicate :: Foldable t => t a -> [a]
duplicate = foldl (\acc x -> acc ++ [x,x]) []

duplicate2 = concat . map (\x -> [x,x])

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}

ziplike _ [] = []
ziplike [] _ = []
ziplike (x:xs) (y:ys) = (x,y) : ziplike xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
splitAtIndex k l = ( take k l, drop k l )
splitAtIndex' k l = ( first, second)
    where first = take k l
          second = drop k l

splitAtIndex'' k l =
    let first = take k l
        second = drop k l
    in ( first, second)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
dropK k l = take k l ++ drop (k+1) l

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
slice i k = drop i . take k

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
insertElem x k l = take k l ++ [x] ++ drop k l

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
rotate n l = drop n l ++ take n l
