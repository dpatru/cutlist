{-# LANGUAGE ViewPatterns #-}

import System.Environment (getArgs)
import Data.Map.Strict (Map)
import qualified Data.Map as M ((!),delete,elems,empty,findWithDefault, insert, insertWith,lookup,lookupGT,member,minView)
-- import qualified Data.List as L 
import Data.Sequence ((><), (<|), (|>), Seq, viewl, ViewL( EmptyL, (:<) ))
import qualified Data.Sequence as S (drop,drop,empty,fromList,index,inits,singleton,splitAt,take,zip)
import Data.List (unfoldr)
import Control.Exception (assert)
import Control.Monad (mplus)
import Data.Foldable (foldlM)

-- Perm == a partial permutation
type Perm a = (Seq a, Seq a)  -- (permutation so far, rest of characters)

-- Perms == a map of partial permutations, indexed by 
type Perms a = Map (Seq a) (Seq (Perm a))

-- instance Show (Perms a) where
--   show m = show "fromList $" ++ show $ M.toList m

-- Expand one letter of the permutation
perm1 :: Seq a -> Seq a -> [Perm a]
perm1 p s = [( p |> S.index s2 0, s1 >< S.drop 1 s2 )
            | i <- [0..length s - 1], let (s1,s2) = S.splitAt i s ]

-- Expand the left branch of the permutation tree, returning the whole tree as a list.
left :: Seq a -> Seq a -> [Perm a]
left p s | null s = [(p,s)]
          | otherwise = left p1 s1 ++ pss
  where (p1,s1):pss = perm1 p s

-- Operator to add a list of Perms to a Perms map.
(+++) :: (Ord a, Foldable m) => Perms a -> m(Perm a) -> Perms a
(+++) ps l = foldr f ps l
  where f (k, v2) ps = M.insertWith (><) k (S.singleton (k, v2)) ps

permutations :: (Ord a) => [a] -> Perms a
permutations s = M.empty +++ perm1 S.empty (S.fromList s)

-- popk (pop-key) removes a permutation which starts with key,
-- returning both the permutation and a new permutation collection.
---
-- Method: first expand all the prefixes of x (except x itself) in m to get m'.
-- then check if x is a member of m'. If it is, return the first result.
popk :: (Ord a) => Perms a -> Seq a -> (Maybe (Perm a), Perms a)
popk m x = if M.member x m'
           then (Just $ S.index ps 0, (x `M.delete` m') +++ (S.drop 1 ps))
           else (Nothing, m')
  where m' = foldl addPerms m (S.inits $ S.take (length x - 1) x)
        addPerms m x = case M.lookup x m of
                       Just ps -> foldl (\m p -> m +++ (uncurry perm1 $ p)) (M.delete x m) ps
                       Nothing -> m
        ps = m' M.! x

popk_ m x = let (Just p, m') = popk m x in p

pop :: (Ord a) => Perms a -> Maybe (Seq a, Perms a)
pop m = do
  (vs, m') <- M.minView m
  let ((p, mustBeEmpty) : ps) = (uncurry left) $ S.index vs 0
  let err = assert (null mustBeEmpty) False
  return (p, m' +++ (S.drop 1 vs) +++ ps)

popss :: (Ord a) => Perms a -> [Seq a]
popss m = unfoldr pop m

pops m = map toList $ popss m

isPrefix :: (Eq a) => Seq a -> Seq a -> Bool
isPrefix x y = (length x <= length y) && (all (uncurry (==)) $ S.zip x y)

-- Remove all the items beginning with x. Expand permutations to
-- ensure that all permutations are available.
prunePrefix :: (Ord a) => Perms a -> Seq a -> Perms a
prunePrefix m x = M.delete x $ removeLonger x m'
  where (_, m') = popk m x -- expand m to contain all the x prefixes
        removeLonger x m = case M.lookupGT x m of
           Just (k, v) -> if isPrefix x k then removeLonger x $ M.delete k m else m
           Nothing -> m

type Best a b = Maybe (Seq a, b)
type Comparator a b = Map (Seq a) b -> Best a b -> Seq a -> (Best a b, [Seq a], Map (Seq a) b)

-- best seq, best score, dynamic prog lookup table 

-- prune :: (Ord a) => Best a b -> Perms a -> Perms a
-- prune Nothing ps = ps
-- prune (Just (_, _, prefixes, _)) ps = foldr prunePrefix ps prefixes

-- searchMin :: (Ord a, Ord b) => (Best a b -> Seq a -> Best a b) -> Best a b -> Perms a -> Best a b
-- searchMin f b ps = (do (p', ps') <- pop ps; searchMin f (f b p') $ prune b ps') `mplus` b

searchMin :: (Ord a, Ord b) => Comparator a b -> Map (Seq a) b -> Best a b -> Perms a -> Best a b
searchMin f m b ps | null ps = b
                   | otherwise = do
  (p', ps') <- pop ps;
  let (b', ks, m') = f m b p'
  searchMin f m' b' $ foldl prunePrefix ps' ks

findLongestPrefix :: (Ord a) => Map (Seq a) b -> Seq a -> (Seq a, b)
findLongestPrefix m x
  | null x = maybe undefined (\v -> (S.empty, v)) $ M.lookup S.empty m
  | M.member x m = (x, m M.! x)
  | otherwise = findLongestPrefix m $ S.take (length x - 1) x
                  
oneDComparator :: (Ord a, RealFrac a) => Comparator a a
oneDComparator m b xs = f m' b [] (longestPrefix, score) (S.drop (length longestPrefix) xs)
  where m' = M.insertWith (\newVal oldVal -> oldVal) S.empty 0 m
        (longestPrefix, score) = findLongestPrefix m xs
        tooBig (_, b) = b > 1
        f m Nothing e b (viewl -> EmptyL)
          | tooBig(b) = (Nothing, e, m)
          | otherwise = (Just b, e, m)
        f m (Just (a, b)) e (a', b') (viewl -> EmptyL)
          | b <= b' = (Just (a,b), e ++ [a'], m)
          | otherwise = (Just (a',b'), e ++ [a], m)
        f m (Just (a, b)) e (a', b') (viewl -> x :< xs)
          | x > 1 = (Nothing, e, m')
          | b'' > b = (Just (a,b), (e ++ [a'']), m')
          | otherwise = f m' (Just (a,b)) e (a'', b'') xs
            where a'' = a' |> x
                  b'' = if ceiling b' == ceiling (b'+x) then b'+x
                        else (fromIntegral $ ceiling b') + x
                  m'  = M.insert a'' b'' m
                            
        
-- searchMin f Nothing ps = do
--   (p', ps') <- pop ps;
--   searchMin f (f Nothing p') ps'
-- searchMin f (Just b) ps = maybe (Just b) Just $ do
--   (p', ps') <- pop ps;
--   searchMin f (f (Just b) p') ps'

toList :: (Foldable m) => m a -> [a]
toList = foldr (:) []

lists :: Perms a -> [ ([a],[a])] 
lists = map (\(x,y)->(toList x, toList y)) . mconcat . map toList . M.elems

-- lists :: (Perms a) -> [([a],[a])]
-- lists ps = mconcat toLists $ M.elems ps
--   where toLists :: Seq (Seq a, Seq a) -> [([a],[a])]
--         toLists s = fmap (\(x,y) -> (toList x, toList y)) s
--         toList x = foldr (:) [] x
        
test x | odd x = 4
       | otherwise = 5
       
-- get :: (Ord a) => Perms a -> (Maybe (Seq a), Perms a)
-- get m x = if M.member k m then
--              let ps = = (Just $ S.index ps 0, m' +++ (S.drop 1 ps))
--           where ps = m!x
--         |
        
     
main = do
  args <- getArgs
  putStr $ unlines args
  