import Constants (size)
import Game (Grid, Player)
import Logic (move, valid)
import Test.QuickCheck
  ( Property,
    choose,
    forAll,
    quickCheck,
    (==>),
  )

prop_moveInvalid :: Grid -> Int -> Player -> Property
prop_moveInvalid grid i player =
  length (concat grid) == size ^ (2 :: Int) ==> not (valid grid i) ==> null (move grid i player)

prop_moveValid :: Grid -> Int -> Player -> Property
prop_moveValid grid i player =
  length (concat grid) == size ^ (2 :: Int) ==> valid grid i ==> not (null (move grid i player))

prop_movePlacesMark :: Grid -> Int -> Player -> Property
prop_movePlacesMark grid _ player =
  length (concat grid) == size ^ (2 :: Int) ==>
    forAll (choose (0, size ^ (2 - 1 :: Int))) $ \i ->
      valid grid i ==> (concat (head (move grid i player)) !! i == player)

prop_movePreservesSize :: Grid -> Int -> Player -> Property
prop_movePreservesSize grid _ player =
  length (concat grid) == size ^ (2 :: Int) ==>
    forAll (choose (0, size ^ (2 - 1 :: Int))) $ \i ->
      valid grid i ==> (length (concat (head (move grid i player))) == size ^ (2 :: Int))

main :: IO ()
main =
  do
    quickCheck prop_moveInvalid
    quickCheck prop_moveValid
    quickCheck prop_movePlacesMark
    quickCheck prop_movePreservesSize
    putStrLn "All tests passed!"
