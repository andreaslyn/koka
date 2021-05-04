

data Nat = Zero | Succ Nat


instance Eq Nat where
  m == n =
    case m of
      Zero ->
        case n of
          Zero -> True
          Succ _ -> False
      Succ m' ->
        case n of
          Zero -> False
          Succ n' -> m' == n'


one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five
seven = Succ six
eight = Succ seven
nine = Succ eight
ten = Succ nine


plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = plus (Succ m) n


mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = plus (mul m n) m


genNatlist :: Nat -> [Nat] -> [Nat]
genNatlist Zero acc = acc
genNatlist (Succ n) acc = genNatlist n (Succ n : acc)


natlist :: [Nat]
natlist = genNatlist (mul ten five) []


triplelist :: [(Nat, Nat, Nat)]
triplelist = [(a, b, c) | a <- natlist, b <- natlist, c <- natlist]


isPythagorean :: (Nat, Nat, Nat) -> Bool
isPythagorean (x, y, z) = plus (mul x x) (mul y y) == mul z z


main :: IO ()
main = print (length $ filter isPythagorean triplelist)
