-- | Main entry point to the application.
module Main where
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Array.IO
import Control.Monad

maxi xs = maximumBy (comparing fst) (zip xs [0..])

primes = 2 : [i | i <- [3..], and [rem i p > 0 | p <- takeWhile ((<=i).(^2)) primes]]

nod primes mp n = nod' n (take mp primes) 1 1 where 
    nod' n ps e d
        | n == 0 || null ps = d
        | n `mod` head ps == 0 = nod' (n `quot` head ps) ps (e+1) d
        | otherwise = nod' n (tail ps) 1 d*e

sod n = sod' n 0 where
        sod' n acc
            | n == 0 = acc
            | otherwise = sod' (quot n 10) acc + (mod n 10) 

pe20 = sod $ product [1..100]
            
pe16 = sod (2^1000)

pe15a = do  arr <- newArray ((0,0),(n,n)) 0 :: IO (IOArray (Int,Int) Int)
            mapM_ (\ix -> writeArray arr ix 1) $ join [[(i,n), (n,i)] | i <- [0..(n-1)]]
            mapM_ (updarr arr)  [(i,j) | i <- [(n-1),(n-2)..0],j <- [(n-1),(n-2)..0]]
            np <- readArray arr (0,0)
            print np
            where updarr arr (i,j) = do a <- readArray arr (i+1,j)
                                        b <- readArray arr (i,j+1)
                                        writeArray arr (i,j) (a+b)
                  n = 20


pe15 = let n = 2 * 20
           k = n `quot` 2 in
           product [n,(n-1)..(n-k+1)] `quot` product [1..k]
        
{- Highly inefficient recursive solution       
pe15 = paths' 10 0 0 where
       paths' n x y
        | x == n && y == n = 1
        | x == n = paths' n x (y+1)
        | y == n = paths' n (x+1) y
        | otherwise = paths' n (x+1) y + paths' n x (y+1)
--}

pe14 = maxi $ map (collatz' 1) [1..99999] where
          collatz' l n
            | n == 1         = l
            | n `mod` 2 == 0 = collatz' (l+1) (n `quot` 2)
            | otherwise      = collatz' (l+1) (3*n+1)

pe12 = i*(i-1) `quot` 2
        where i = cnt 2 0 2 2
              nodfp = nod primes 1000
              cnt i acc dn dn1
                |acc > 500      = i
                |i `mod` 2 == 0 = let dn = nodfp (i+1) in cnt (i+1) (dn*dn1) dn dn1
                |otherwise      = let dn1 = nodfp ((i+1) `quot` 2) in cnt (i+1) (dn*dn1) dn dn1
              
pe10 = sum $ takeWhile (< 2000000) primes

pe9 = [a*b*c | a <- [1..997], b <- [a..998], let c = 1000-a-b, a^2 + b^2 == c^2] !! 0
              
pe8 = maximum [product $ map (\x -> digitToInt x) $ take 13 $ drop i n | i <- [0..986]]
    where n = "73167176531330624919225119674426574742355349194934\
    \96983520312774506326239578318016984801869478851843\
    \85861560789112949495459501737958331952853208805511\
    \12540698747158523863050715693290963295227443043557\
    \66896648950445244523161731856403098711121722383113\
    \62229893423380308135336276614282806444486645238749\
    \30358907296290491560440772390713810515859307960866\
    \70172427121883998797908792274921901699720888093776\
    \65727333001053367881220235421809751254540594752243\
    \52584907711670556013604839586446706324415722155397\
    \53697817977846174064955149290862569321978468622482\
    \83972241375657056057490261407972968652414535100474\
    \82166370484403199890008895243450658541227588666881\
    \16427171479924442928230863465674813919123162824586\
    \17866458359124566529476545682848912883142607690042\
    \24219022671055626321111109370544217506941658960408\
    \07198403850962455444362981230987879927244284909188\
    \84580156166097919133875499200524063689912560717606\
    \05886116467109405077541002256983155200055935729725\
    \71636269561882670428252483600823257530420752963450"
    
pe7 = primes !! 10000
    
pe6 = (sum [1..100])^2 - (sum $ map (^2) [1..100])
    
pe5 = foldl lcm 1 [1..20]

pe4 = maximum [a*b | a <- [100..999], b <-[100..999], (show (a*b)) == (reverse $ show (a*b))]
    
pe3 = maximum $ filter (\x -> 600851475143 `mod` x == 0)  $ takeWhile (< 775147) primes

pe2 = sum $ filter even $ takeWhile (< 4000000) fibs
        where fibs = scanl (+) 0 (1:fibs)
             
pe1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 ==0]

-- | The main entry point.
main :: IO ()
main = do
    --putStrLn $ show pe15
    pe15a