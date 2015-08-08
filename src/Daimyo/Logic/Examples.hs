module Daimyo.Logic.Examples (
    pretty,
    ex1
) where

import Data.Logic.Propositional
import qualified Data.Map as M
import Control.Monad


{-
Is satisfiable?

1. If the file system is not locked, then

    a) new messages will be queued
    b) new messages will be sent to the messages buffer
    c) the system is functioning normally, and conversely, if the system is functioning normally, then the file system is not locked

2. If new messages are not queued, then they will be sent to the messages buffer.
3. New messages will not be sent to the message buffer.

a) Begin by translating the five specifications into propositional formulas using four propositional variables:

    L = file system locked.
    Q = new messages are queued
    B = new messages are sent to the message buffer
    N = system functioning normally

b) Demonstrate that this set of specifications is satisfiable by describing a single truth assignment for the variables L, Q, B, N and verifying that under this assignment, all the specifications are true.

c) Argue that the assignment determined in part (b) is the only one that does the job.
-}

prop'1 = Conditional (Negation (Variable (Var 'L'))) (Conjunction (Conjunction (Variable (Var 'Q')) (Variable (Var 'B'))) (Variable (Var 'N')))
prop'2 = Conditional (Negation (Variable (Var 'Q'))) (Variable (Var 'B'))
--prop'3 = (Conditional (Variable 
-- confused

ex1 = []

validity = Disjunction (Conditional (Variable (Var 'P')) (Variable (Var 'Q'))) (Conditional (Variable (Var 'Q')) (Variable (Var 'P')))

pretty t = putStrLn (truthTable t)
