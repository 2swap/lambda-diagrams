-- 1. Implement basic quicksort
quicksort [] = []
quicksort (l:list) = concat [quicksort (filter (< l) list), [l], quicksort (filter (>= l) list)]



-- 2. Compute list of all squares
square = (\x -> x * x)
squares1 = map square [1..]
primes = let f (x:xs) = (x : (f [ y | y <- xs, y `mod` x /= 0])) in f [2..]



-- 3. Implement the Ackermann Function
-- We assume m, n are positive ints
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))


-- 4. Implement my_foldl
-- Example Usage: "my_foldl (+) [1..5] 10000" yields 10015
-- (+)? infix?
my_foldl f [] carry = carry
my_foldl f (l:list) carry = my_foldl f list (f l carry)



-- 5. Implement my_map
-- Example usage: "my_map square [1..100]" yields the first 100 positive squares
my_map f [] = []
my_map f (l:list) = f l : my_map f list



-- 6. Implement my_zip
-- Example Usage: "my_zip [1, 2] [8, 9]" [(1, 8), (2, 9)]
my_zip [] [] = []
my_zip _ [] = error "Invalid lengths"
my_zip [] _ = error "Invalid lengths"
my_zip (a:lista) (b:listb) = (a, b) : my_zip lista listb



-- 7. Implement my_filter
-- Example Usage: "my_filter even [1..5]" yields [2, 4]
my_filter_r f [] appendy = appendy
my_filter_r f (l:list) appendy = if f l then my_filter_r f list (l:appendy)
                                 else my_filter_r f list appendy
my_filter f list = reverse $ my_filter_r f list []



f x = 2 * x
g x = x + 5
a x = square x
h = f . g . a
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

1^2 + 3^2 + (2n+1)^2
50 + 4*((2*49+1)*(49+1)*49)/6 + 2*(49*(49+1))