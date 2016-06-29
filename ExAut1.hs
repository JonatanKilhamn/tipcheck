module ExAut1 where

import TransitionSystem
import TransitionSystemCircuits
import Lava

-- Example automata


globalEventList :: [Event]
globalEventList = [e1,e2,e3,e4]


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
             }

exAut2 = Aut { autName = "a2"
             , nbrLocations = 5
             , transitions = [ex2t1, ex2t2, ex2t3, ex2t4]
             }

exAut3 = Aut { autName = "a3"
             , nbrLocations = 2
             , transitions = [ex1t1, ex1t2]
             }

s1 = foldl synchronise emptySynch [exAut1, exAut3]

