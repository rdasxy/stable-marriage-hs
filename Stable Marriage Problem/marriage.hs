-- stable.hs
-- solve the Gale/Shapley stable marriage problem in Haskell

-- The preferences are represented as a function of a person.  Given a person,
-- the function returns that person's preference list.  The algorithm is 
-- implemented by modifying this function.  When the algorithm terminates,
-- the first woman on each man's preference list is his fiancee, and the last
-- man on each woman's preference list is her fiance.

import System.IO
type Person = String
type Women  = [Person]
type Men    = [Person]
type PrefFn = Person->[Person]

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "input.out" WriteMode
       inpStr <- hGetContents inh
       let (women, prefs) = solve inpStr
       output outh women prefs
       hClose inh
       hClose outh

solve :: String -> ( Women, PrefFn )
solve str =
   let prefs  = map words (lines str)
       n      = length prefs `div` 2
       women  = drop n (map head prefs)
       men    = take n (map head prefs)
       pref   = find prefs
       result = propose men women pref
   in (women, result)

-- find takes a list of lists, and given the head of one of the
-- lists, returns the tail.  (If more than one list has the same head, it
-- returns the tail of the first such, but that has no application in this
-- problem.)  By partial application, we will specialize it, so that given a
-- person, it return his preference list.

find :: [[Person]] -> PrefFn
find l x
  | x == head (head l) = tail (head l)
  | otherwise          = find (tail l) x

delete :: (Eq a) => a -> [a] -> [a]
delete w ws = [z | z <-ws, z /= w]

-- reject returns the modified preference function when a man is rejected
-- the only difference is that the first woman on the man's list is struck off

reject :: Person -> PrefFn -> PrefFn
reject m pref = \z -> if z /= m
                      then pref z
                      else tail (pref m)

-- once w accepts m her preferences change.  She deletes everyone lower
-- on her list than m

accept :: Person -> Person -> PrefFn -> PrefFn
accept w m pref = \z -> if z /= w
                        then pref z
                        else takeWhile (/= m) (pref w) ++ [m]
                        
-- if w accepts m and jilts x, her preferences are altered first by the acceptance,
-- then by rejecting x

jilt :: Person -> Person -> Person -> PrefFn -> PrefFn
jilt w m x pref = reject x (accept w m pref)


-- propose is the main routine.
-- first parameter:  list of unengaged men
-- second parameter: list of "free" women, who hold no proposal
-- third parameter:  current preference function
-- value:            revised prefernce function

-- if there are no unengaged men, process stops
-- if m's first choice, w is free she accepts him
-- if w is not free, but m is on her list, she accepts him and jilts her fiance
-- otherwise, w rejects m

propose :: Men -> Women -> PrefFn -> PrefFn
propose [] _ pref = pref
propose (m:ms) fw pref
  | w `elem` fw       =  propose ms (delete w fw) (accept w m pref)
  | m `elem` (pref w) =  propose (x:ms) fw (jilt w m x pref)
  | otherwise         =  propose (m:ms) fw (reject m pref)
  where w = head (pref m)
        x = last (pref w)

output:: Handle -> Women -> PrefFn -> IO()
output h [] pref = do return ()
output h (w:ws) pref =
    do hPutStrLn h (w ++ " " ++ last (pref w))
       output h ws pref

