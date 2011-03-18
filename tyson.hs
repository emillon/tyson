-- Copyright (c) 2011 Etienne Millon <etienne.millon@gmail.com>
-------------------------------------------------------------------------------
--                        "THE BEER-WARE LICENSE"
-- <etienne.millon@gmail.com> wrote this file. As long as you retain this notice
-- you can do whatever you want with this stuff. If we meet some day, and you
-- think this stuff is worth it, you can buy me a beer in return.
-------------------------------------------------------------------------------

type Context = (Char, Char, Char)
type Context2D = (Context, Context, Context)

c0 :: String -> [(Char, Char, Char)]
c0 s = padZip3 s ' '

padZip3 :: [a] -> a -> [(a, a, a)]
padZip3 xs pad =
  zip3 ps ps2 (tail ps2)
    where ps = ([pad] ++ ps2)
          ps2 = xs ++ [pad]

flipc0 :: (String, String, String) -> [Context2D]
flipc0 (l0, l1, l2) = zip3 (c0 l0) (c0 l1) (c0 l2)

ctx :: [String] -> [[Context2D]]
ctx l0 = map flipc0 $ padZip3 l0 (repeat ' ')

pp :: [Context2D] -> IO ()
pp = mapM_ pp1
  where
    pp1 y = (mapM_ (\ x -> (putStrLn (pp2 x))) $ pp2 y) >> putStrLn "---"
    pp2 (a, b, c) = [a, b, c]

contextMap :: (Context2D -> Char) -> [String] -> [String]
contextMap f ss =
  map (map f) c
    where
      c = ctx ss

centerOf :: Context2D -> Char
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

unicodize :: Context2D -> Char
unicodize ( ( _ ,  _ ,  _ )
          , ( l , '-',  r )
          , ( _ ,  _ ,  _ )
          ) = hLine (connects L r) (connects R l)
                where
                  hLine True  True  = '─'
                  hLine False True  = '╴'
                  hLine True  False = '╶'
                  hLine False False = '-'
unicodize ( ( _ ,  u ,  _ )
          , ( _ , '|',  _ )
          , ( _ ,  d ,  _ )
          ) = vLine (connects D u) (connects U d)
                where
                  vLine True  True  = '│'
                  vLine False True  = '╷'
                  vLine True  False = '╵'
                  vLine False False = '|'
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

allInput :: IO [String]
allInput = fmap lines getContents

main :: IO ()
main = do
  ss <- allInput
  mapM_ putStrLn (contextMap unicodize (pad ss))
    where
      pad x = ([repeat ' '] ++ (map (++ replicate 50 ' ') x) ++ [repeat ' '])

examples :: [([String], [String])]
examples =
  [(["+--+"
    ,"|  |"
    ,"+--+"
    ]
   ,["┌──┐"
    ,"│  │"
    ,"└──┘"
    ]
   )
  ,([" ^ "
    ,"<+>"
    ," v "
    ]
   ,[" △ "
    ,"◁┼▷"
    ," ▽ "
    ]
   )
  ,(["|  |"
    ,"+--+"
    ,"|  |"
    ]
   ,["╷  ╷"
    ,"├──┤"
    ,"╵  ╵"
    ]
   )
  ,(["-+-"
    ," | "
    ,"-+-"
    ]
   ,["╶┬╴" 
    ," │ "
    ,"╶┴╴"
    ]
   )
  ]

testExamples :: [([String], [String])] -> Bool
testExamples = all (\ (x, y) -> y == contextMap unicodize x)

testIO :: [([String], [String])] -> IO ()
testIO =
  mapM_ (\ (src, expected) ->
    let result = contextMap unicodize src in     
    if (result == expected)
      then putStrLn "OK"
      else do
        putStrLn "FAIL"
        putStrLn "src :"
        mapM_ putStrLn src
        putStrLn "expected :"
        mapM_ putStrLn expected
        putStrLn "got :"
        mapM_ putStrLn result
    )

