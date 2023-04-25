module Rodin.ProofTree.Stats where

import Text.Printf

data Stats = Stats {
    _pop :: Integer,
    _min :: Integer,
    _max :: Integer,
    _sum :: Integer
}

instance Show Stats where
  show s =
      printf "Population: %d, Min: %d, Max: %d, Average: %.4f" (_pop s) (_min s) (_max s) avg
      where avg = ((fromInteger $ _sum s) :: Double) / ((fromInteger $ _pop s) :: Double)

data StatRes = Undef | SR Stats

instance Show StatRes where
  show Undef = "<<No Stats>>"
  show (SR s) = show s

one :: Integer -> StatRes
one n = SR $ Stats {
    _pop = 1,
    _min = n,
    _max = n,
    _sum = n
}

_get :: (Stats -> b) -> StatRes -> Maybe b
_get _ Undef = Nothing
_get g (SR s) = Just $ g s

get_min :: StatRes -> Maybe Integer
get_min = _get _min

get_max :: StatRes -> Maybe Integer
get_max = _get _max

get_pop :: StatRes -> Maybe Integer
get_pop = _get _pop

get_avg :: StatRes -> Maybe Double
get_avg s = do
    su <- _get _sum s
    po <- _get _pop s
    return $ (fromInteger su) / (fromInteger po)

instance Semigroup StatRes where
  Undef <> Undef = Undef
  Undef <> (a@(SR _)) = a
  (a@(SR _)) <> Undef = a
  (SR sa) <> (SR sb) =
      SR $ Stats {
        _pop = _pop sa + _pop sb,
        _min = min (_min sa) (_min sb),
        _max = max (_max sa) (_max sb),
        _sum = _sum sa + _sum sb
      }

instance Monoid StatRes where
  mempty = Undef


aggregate :: (a -> Integer) -> [a] -> StatRes
aggregate f =
    foldMap (one . f)

pretty_col :: Integer -> Integer -> String
pretty_col colwidth avgprec =
    (center (colwidth + 2 - 4) "Pop.") ++ "|"
    ++ (center (colwidth + 2 - 4) "Min.") ++ "|"
    ++ (center (colwidth + 2 - 4) "Max.") ++ "|"
    ++ (center (colwidth + avgprec + 1 + 2 - 4) "Avg.")
    where center 0 s = s
          center r s
            | r `mod` 2 == 0 = ' ':(center (r - 1) s)
            | otherwise = (center (r - 1) s) ++ " "

pretty :: Integer -> Integer -> StatRes -> String
pretty colwidth avgprec Undef =
    printf (" " ++ numspec ++ " | " ++ numspec ++ " | " ++ numspec ++ " | " ++ avgspec) "0" "??" "??" "??"
    where numspec = "% " ++ show colwidth ++ "s"
          avgspec = "% " ++ (show $ colwidth + 1 + avgprec) ++ "s"
pretty colwidth avgprec (SR a) =
    printf (" " ++ numspec ++ " | " ++ numspec ++ " | " ++ numspec ++ " | " ++ avgspec) po mi ma av
    where po = _pop a
          mi = _min a
          ma = _max a
          su = _sum a
          av = ((fromInteger su) :: Double) / ((fromInteger po) :: Double)
          numspec = "% " ++ show colwidth ++ "d"
          avgspec = "% " ++ (show $ colwidth + 1 + avgprec) ++ "." ++ (show avgprec) ++ "f"



