{-
    What is the maximum number Ln of regions defined by n lines in the plane?

    L0 = 1
    L1 = 2
    L2 = 4
    L3 = 7
    L4 = ..
    
    L0 = 1
    Ln = Ln-1 + n for n > 0

    Ln  = Ln-1 + n
        = Ln-2 + (n-1) + n
        = Ln-3 + (n-2) + (n-1) + n
        = L0 + 1 + 2 + ... + (n-2) + (n-1) + n
        = 1 + Sn, where Sn = 1 + 2 + 3 + ... + (n-1) + n
-}

_L 0 = 1
_L 1 = _L (1-1) + 1
_L 2 = _L (2-1) + 2
_L 3 = _L (3-1) + 3
_L 4 = _L (4-1) + 4
_L n = _L (n-1) + n

_Ln n = sum [1..n] + 1
_Ln' n = ((n^2 + n) / 2) + 1
_Ln'' = succ . _Sn 

_Sn n = (n^2 + n) / 2 

ln_proof = [_Ln' 100, ih1 99, ih2 99]
    where
        ih1 n = (((n+1)^2 + (n+1)) / 2) + 1
        ih2 n = 1 + 0.5 * ((n+1)^2 + (n+1))

ln_ln'_proof = all (==True) [ _Ln n == _Ln' n | n <- [1..1000] ]

n_sn_proof = all (==True) [ _Ln' n == (_Sn n) + 1 | n <- [1..1000] ]
