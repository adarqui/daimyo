{-
    odd calculator concept, trying to pack everything together

    sources:
        http://en.wikipedia.org/wiki/Metric_prefix
        http://www.t1shopper.com/tools/calculate/

    todo:
        add in bit? byte? kibibit, kilobit, kibibyte, kilobyte etc.. Kibit kbit KiB kB etc.
        don't trust this code yet.
-}

module Daimyo.Measure.Metric (
    Metric (..),
    toBase,
    fromBase,
    fromTo
) where

import Prelude as P

data Metric =
      Yotta Double
    | Zetta Double
    | Exa Double
    | Peta Double
    | Tera Double
    | Giga Double
    | Mega Double
    | Kilo Double
    | Hecto Double
    | Deca Double
    | Base Double
    | Deci Double
    | Centi Double
    | Milli Double
    | Micro Double
    | Nano Double
    | Pico Double
    | Femto Double
    | Atto Double
    | Zepto Double
    | Yocto Double
    --
    | Septillion Double
    | Sextillion Double
    | Quintillion Double
    | Quadrillion Double
    | Trillion Double
    | Billion Double
    | Million Double
    | Thousand Double
    | Hundred Double
    | Ten Double
    | One Double
    | Tenth Double
    | Hundredth Double
    | Thousandth Double
    | Millionth Double
    | Billionth Double
    | Trillionth Double
    | Quadrillionth Double
    | Quintillionth Double
    | Sextillionth Double
    | Septillionth Double
    deriving (Show, Read, Eq)

toBase m =
    case m of
        (Yotta m')  -> Base $ m' * 1.0e24
        (Zetta m')  -> Base $ m' * 1.0e21
        (Exa m')    -> Base $ m' * 1.0e18
        (Peta m')   -> Base $ m' * 1.0e15
        (Tera m')   -> Base $ m' * 1.0e12
        (Giga m')   -> Base $ m' * 1.0e9
        (Mega m')   -> Base $ m' * 1.0e6
        (Kilo m')   -> Base $ m' * 1.0e3
        (Hecto m')  -> Base $ m' * 1.0e2
        (Deca m')   -> Base $ m' * 1.0e1
        (Base m')   -> Base $ m' * 1.0e0
        (Deci m')   -> Base $ m' * 1.0e-1
        (Centi m')  -> Base $ m' * 1.0e-2
        (Milli m')  -> Base $ m' * 1.0e-3
        (Micro m')  -> Base $ m' * 1.0e-6
        (Nano m')   -> Base $ m' * 1.0e-9
        (Pico m')   -> Base $ m' * 1.0e-12
        (Femto m')  -> Base $ m' * 1.0e-15
        (Atto m')   -> Base $ m' * 1.0e-18
        (Zepto m')  -> Base $ m' * 1.0e-21
        (Yocto m')  -> Base $ m' * 1.0e-24
        _           -> toBase' m

toBase' m =
    case m of
        (Septillion m') -> Base $ m' * 1.0e24
        (Sextillion m') -> Base $ m' * 1.0e21
        (Quintillion m')-> Base $ m' * 1.0e18
        (Quadrillion m')-> Base $ m' * 1.0e15
        (Trillion m')   -> Base $ m' * 1.0e12
        (Billion m')    -> Base $ m' * 1.0e9
        (Million m')    -> Base $ m' * 1.0e6
        (Thousand m')   -> Base $ m' * 1.0e3
        (Hundred m')    -> Base $ m' * 1.0e2
        (Ten m')        -> Base $ m' * 1.0e1
        (One m')        -> Base $ m' * 1.0e0
        (Tenth m')      -> Base $ m' * 1.0e-1
        (Hundredth m')  -> Base $ m' * 1.0e-2
        (Thousandth m') -> Base $ m' * 1.0e-3
        (Millionth m')  -> Base $ m' * 1.0e-6
        (Billionth m')  -> Base $ m' * 1.0e-9
        (Trillionth m') -> Base $ m' * 1.0e-12
        (Quadrillionth m')  -> Base $ m' * 1.0e-15
        (Quintillionth m')  -> Base $ m' * 1.0e-18
        (Sextillionth m')   -> Base $ m' * 1.0e-21
        (Septillionth m')   -> Base $ m' * 1.0e-24

fromBase (Base b) = b

fromTo m t =
    let
        (Base b') = toBase m
        op = (/)
    in
    case t of
        (Yotta t')  -> Yotta (b' `op` fromBase (toBase (Yotta 1)))
        (Zetta t')  -> Zetta (b' `op` fromBase (toBase (Zetta 1)))
        (Exa t')    -> Exa (b' `op` fromBase (toBase (Exa 1)))
        (Peta t')   -> Peta (b' `op` fromBase (toBase (Peta 1)))
        (Tera t')   -> Tera (b' `op` fromBase (toBase (Tera 1)))
        (Giga t')   -> Giga (b' `op` fromBase (toBase (Giga 1)))
        (Mega t')   -> Mega (b' `op` fromBase (toBase (Mega 1)))
        (Kilo t')   -> Kilo (b' `op` fromBase (toBase (Kilo 1)))
        (Hecto t')  -> Hecto (b' `op` fromBase (toBase (Hecto 1)))
        (Deca t')   -> Deca (b' `op` fromBase (toBase (Deca 1)))
        (Base t')   -> Base b'
        (Deci t')   -> Deci (b' `op` fromBase (toBase (Deci 1)))
        (Centi t')  -> Centi (b' `op` fromBase (toBase (Centi 1)))
        (Milli t')  -> Milli (b' `op` fromBase (toBase (Milli 1)))
        (Micro t')  -> Micro (b' `op` fromBase (toBase (Micro 1)))
        (Nano t')   -> Nano (b' `op` fromBase (toBase (Nano 1)))
        (Pico t')   -> Pico (b' `op` fromBase (toBase (Pico 1)))
        (Femto t')  -> Femto (b' `op` fromBase (toBase (Femto 1)))
        (Atto t')   -> Atto (b' `op` fromBase (toBase (Atto 1)))
        (Zepto t')  -> Zepto (b' `op` fromBase (toBase (Zepto 1)))
        (Yocto t')  -> Yocto (b' `op` fromBase (toBase (Yocto 1)))
        _           -> fromTo' m t

fromTo' m t =
    let
        (Base b') = toBase m
        op = (/)
    in
    case t of
        (Septillion t')     -> Septillion (b' `op` fromBase (toBase (Septillion 1)))
        (Sextillion t')     -> Sextillion (b' `op` fromBase (toBase (Sextillion 1)))
        (Quintillion t')    -> Quintillion (b' `op` fromBase (toBase (Quintillion 1)))
        (Quadrillion t')    -> Quadrillion (b' `op` fromBase (toBase (Quadrillion 1)))
        (Trillion t')       -> Trillion (b' `op` fromBase (toBase (Trillion 1)))
        (Billion t')        -> Billion (b' `op` fromBase (toBase (Billion 1)))
        (Million t')        -> Million (b' `op` fromBase (toBase (Million 1)))
        (Thousand t')       -> Thousand (b' `op` fromBase (toBase (Thousand 1)))
        (Hundred t')        -> Hundred (b' `op` fromBase (toBase (Hundred 1)))
        (Ten t')            -> Ten (b' `op` fromBase (toBase (Ten 1)))
        (One t')            -> One b'
        (Tenth t')          -> Tenth (b' `op` fromBase (toBase (Tenth 1)))
        (Hundredth t')      -> Hundredth (b' `op` fromBase (toBase (Hundredth 1)))
        (Thousandth t')     -> Thousandth (b' `op` fromBase (toBase (Thousandth 1)))
        (Millionth t')      -> Millionth (b' `op` fromBase (toBase (Millionth 1)))
        (Billionth t')      -> Billionth (b' `op` fromBase (toBase (Billionth 1)))
        (Trillionth t')     -> Trillionth (b' `op` fromBase (toBase (Trillionth 1)))
        (Quadrillionth t')  -> Quadrillionth (b' `op` fromBase (toBase (Quadrillionth 1)))
        (Quintillionth t')  -> Quintillionth (b' `op` fromBase (toBase (Quintillionth 1)))
        (Sextillionth t')   -> Sextillionth (b' `op` fromBase (toBase (Sextillionth 1)))
        (Septillionth t')   -> Septillionth (b' `op` fromBase (toBase (Septillionth 1)))

instance Num Metric where
    a + b = add a b
    a - b = sub a b
    a * b = mul a b

add = binop (P.+)
sub = binop (P.-)
mul = binop (P.*)

binop op a b = 
    let
        (Base a') = toBase a
        (Base b') = toBase b
    in
        fromTo (Base (a' `op` b')) b

t_binop'0 = (Tenth 1) * (Tenth 1)
t_binop'1 = (Tenth 1) + (Tenth 1)
t_binop'2 = (Million 1) + (One 5)
t_binop'3 = (One 5) + (Million 1) 
t_binop'4 = (One 1) - (One 1)
t_binop'5 = (Giga 5) + (Mega 5)
t_binop'6 = (One 1) + (Million 1) + (One 1) + (One 1) -- losing precision
