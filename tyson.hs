type ThreeOf a = (a, a, a)

type CChar = ThreeOf (ThreeOf Char)

c0 :: String -> [(Char, Char, Char)]
c0 s =
  zip3 a b c
    where (a, b, c) = c0' ([' '] ++ s ++ [' '])

c0' :: [a] -> ([a], [a], [a])
c0' s = (s, t, tail t)
  where t = tail s

flipc0 :: (String, String, String) -> [CChar]
flipc0 (l0, l1, l2) = zip3 (c0 l0) (c0 l1) (c0 l2)

ctx :: [String] -> [[CChar]]
ctx l0 = map flipc0 $ zip3 l t (tail t)
  where t = tail l
        l = [pad] ++ l0 ++ [pad]
        pad = repeat ' '

pp :: [CChar] -> IO ()
pp = mapM_ pp1
  where
    pp1 y = (mapM_ (\ x -> (putStrLn (pp2 x))) $ pp2 y) >> putStrLn "---"
    pp2 (a, b, c) = [a, b, c]

contextMap :: (CChar -> Char) -> [String] -> [String]
contextMap f ss =
  map (map f) c
    where
      c = ctx ss

centerOf :: CChar -> Char
centerOf (_, (_, x, _), _) = x

data Dir = U | D | L | R

connects :: Dir -> Char -> Bool
connects _ '+' = True
connects D '|' = True
connects U '|' = True
connects D '^' = True
connects U 'v' = True
connects R '<' = True
connects L '>' = True

connects R '-' = True

connects L '-' = True
connects _ _ = False

unicodize :: CChar -> Char
unicodize ( ( _ ,  _ ,  _ )
          , ( l , '-',  r )
          , ( _ ,  _ ,  _ )
          ) | connects L r && connects R l = '─'
unicodize ( ( _ ,  u ,  _ )
          , ( _ , '|',  _ )
          , ( _ ,  d ,  _ )
          ) | connects D u && connects U d = '│'
unicodize ( ( _ ,  _ ,  _ )
          , ( _ , '^',  _ )
          , ( _ ,  d ,  _ )
          ) | connects U d = '△'
unicodize ( ( _ ,  u ,  _ )
          , ( _ , 'v',  _ )
          , ( _ ,  _ ,  _ )
          ) | connects D u = '▽'
unicodize ( ( _ ,  _ ,  _ )
          , ( l , '>',  _ )
          , ( _ ,  _ ,  _ )
          ) | connects R l = '▷'
unicodize ( ( _ ,  _ ,  _ )
          , ( _ , '<',  r )
          , ( _ ,  _ ,  _ )
          ) | connects L r = '◁'
unicodize ( ( _ ,  u ,  _ )
          , ( l , '+',  r )
          , ( _ ,  d ,  _ )
          ) = plus (connects D u) (connects L r) (connects U d)  (connects R l)
                where
                  plus True  True  True  True  = '┼'
                  plus False True  True  True  = '┬'
                  plus True  False True  True  = '┤'
                  plus True  True  False True  = '┴'
                  plus True  True  True  False = '├'
                  plus False False True  True  = '┐'
                  plus True  False False True  = '┘'
                  plus True  True  False False = '└'
                  plus False True  True  False = '┌'
                  plus _ _ _ _ = '+'

unicodize a = centerOf a

-- unicodize ( ( _ ,  u ,  _ )
--           , ( l , '+',  r )
--           , ( _ ,  d ,  _ )
--           ) | connects D u && connects U d && connects L r && connects R l = '┼'
-- unicodize ( ( _ ,  u ,  _ )
--           , (' ', '+',  r )
--           , ( _ ,  d ,  _ )
--           ) | connects D u && connects L r && connects U d = '├'
-- unicodize ( ( _ ,  u ,  _ )
--           , ( l , '+',  _ )
--           , ( _ ,  d ,  _ )
--           ) | connects D u && connects R l && connects U d = '┤'


allInput :: IO [String]
allInput = fmap lines getContents

main :: IO ()
main = do
  ss <- allInput
  mapM_ putStrLn (contextMap unicodize (pad ss))
    where
      pad x = ([repeat ' '] ++ (map (++ replicate 50 ' ') x) ++ [repeat ' '])

--main :: IO ()
--main = do
--  ss <- allInput
--  let c = ctx ss
--  forM_ c $ \x -> do
--    pp x
--    putStrLn "-+-+-+-+-+-+-+-+-+-+-+-"
