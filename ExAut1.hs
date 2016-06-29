module ExAut1 where

import TransitionSystem
import TransitionSystemCircuits
import Lava

-- Example automata



g0 :: Guard
g0 = Guard {gvar = undefined, gval = undefined}

l1, l2 :: Location
l1 = 0
l2 = 1

e1, e2, e3, e4 :: Event
e1 = "a"
e2 = "b"
e3 = "c"
e4 = "d"

bvar1, bvar2 :: BoolVar
bvar1 = "x"
bvar2 = "y"

u1 :: Update
u1 = Update {uvar = bvar1, uval = False}

t1, t2 :: Transition
t1 = Trans {start=l1,event=e1,guards=[g0],updates=[],end=l2}
t2 = Trans {start=l2,event=e2,guards=[g0],updates=[],end=l1}

aut1 :: Automaton
aut1 = Aut {autName = "foo",
            nbrLocations = 2,
            transitions = [t1,t2]}



ex1t1, ex1t2 :: Transition
ex1t1 = Trans {start=l1,event=e1,guards=[],updates=[],end=l2}
ex1t2 = Trans {start=l2,event=e2,guards=[],updates=[],end=l1}

ex2t1, ex2t2, ex2t3, ex2t4 :: Transition
ex2t1 = Trans {start=l1,event=e1,guards=[],updates=[],end=l2}
ex2t2 = Trans {start=l1,event=e2,guards=[],updates=[],end=l2}
ex2t3 = Trans {start=l2,event=e3,guards=[],updates=[],end=l1}
ex2t4 = Trans {start=l2,event=e4,guards=[],updates=[],end=l1}


exAut1, exAut2, exAut3 :: Automaton
exAut1 = Aut { autName = "a1"
             , nbrLocations = 2
             , transitions = [ex1t1, ex1t2]
             , marked = []
             }

exAut2 = Aut { autName = "a2"
             , nbrLocations = 5
             , transitions = [ex2t1, ex2t2, ex2t3, ex2t4]
             , marked = []
             }

exAut3 = Aut { autName = "a3"
             , nbrLocations = 2
             , transitions = [ex1t1, ex1t2]
             , marked = []
             }

s1 = foldr synchronise emptySynch [exAut1, exAut3]

