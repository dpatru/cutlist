import System.Environment (getArgs)
import Data.Map.Strict (Map)
import qualified Data.Map as M ((!),delete,elems,empty,insertWith,lookup,lookupGT,member,minView)
-- import qualified Data.List as L 
import Data.Sequence ((><), (<|), (|>), Seq)
import qualified Data.Sequence as S (drop,drop,empty,fromList,index,inits,singleton,splitAt,take,zip)
import Data.List (unfoldr)
import Control.Exception (assert)

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

pops m = map (foldr (:) []) $ popss m

isPrefix :: (Eq a) => Seq a -> Seq a -> Bool
isPrefix x y = (length x <= length y) && (all (uncurry (==)) $ S.zip x y)

-- Remove all the items beginning with x. Expand permutations to
-- ensure that all permutations are available.
prunePrefix :: (Ord a) => Seq a -> Perms a -> Perms a
prunePrefix x m = M.delete x $ removeLonger x m'
  where (_, m') = popk m x -- expand m to contain all the x prefixes
        removeLonger x m = case M.lookupGT x m of
           Just (k, v) -> if isPrefix x k then removeLonger x $ M.delete k m else m
           Nothing -> m

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
  