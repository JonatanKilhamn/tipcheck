module CatMouse where

import TransitionSystem
import qualified Data.Set as S
import qualified Data.Map as M
import WmodParser


cat, mouse, room0, room1, room2, room3, room4, monolitic_sup :: Automaton
mouse =
   Aut { autName = "mouse"
       , locations = S.fromList ["m0","m1","m2","m3","m4"]
       , transitions =
        [ Trans { start = "m0"
                , event = "m1"
                , guards = []
                , updates = []
                , end = "m2"
                , uncontrollable = False
                }
        , Trans { start = "m0"
                , event = "m4"
                , guards = []
                , updates = []
                , end = "m4"
                , uncontrollable = False
                }
        , Trans { start = "m1"
                , event = "m3"
                , guards = []
                , updates = []
                , end = "m0"
                , uncontrollable = False
                }
        , Trans { start = "m2"
                , event = "m2"
                , guards = []
                , updates = []
                , end = "m1"
                , uncontrollable = False
                }
        , Trans { start = "m3"
                , event = "m6"
                , guards = []
                , updates = []
                , end = "m0"
                , uncontrollable = False
                }
        , Trans { start = "m4"
                , event = "m5"
                , guards = []
                , updates = []
                , end = "m3"
                , uncontrollable = False
                }
        ]
       , marked = [("m4",[])]
       , initialLocation = "m4"
       }
cat =
   Aut { autName = "cat"
       , locations = S.fromList ["c0","c1","c2","c3","c4"]
       , transitions =
        [ Trans { start = "c0"
                , event = "c1"
                , guards = []
                , updates = []
                , end = "c1"
                , uncontrollable = False
                }
        , Trans { start = "c0"
                , event = "c4"
                , guards = []
                , updates = []
                , end = "c3"
                , uncontrollable = False
                }
        , Trans { start = "c1"
                , event = "c2"
                , guards = []
                , updates = []
                , end = "c2"
                , uncontrollable = False
                }
        , Trans { start = "c1"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "c3"
                , uncontrollable = True
                }
        , Trans { start = "c2"
                , event = "c3"
                , guards = []
                , updates = []
                , end = "c0"
                , uncontrollable = False
                }
        , Trans { start = "c3"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "c1"
                , uncontrollable = True
                }
        , Trans { start = "c3"
                , event = "c5"
                , guards = []
                , updates = []
                , end = "c4"
                , uncontrollable = False
                }
        , Trans { start = "c4"
                , event = "c6"
                , guards = []
                , updates = []
                , end = "c0"
                , uncontrollable = False
                }
        ]
       , marked = [("c2",[])]
       , initialLocation = "c2"
       }
room0 =
   Aut { autName = "room0"
       , locations = S.fromList ["r0c","r0e","r0m"]
       , transitions = 
        [ Trans { start = "r0c"
                , event = "c1"
                , guards = []
                , updates = []
                , end = "r0e"
                , uncontrollable = False
                }
        , Trans { start = "r0c"
                , event = "c4"
                , guards = []
                , updates = []
                , end = "r0e"
                , uncontrollable = False
                }
        , Trans { start = "r0e"
                , event = "c3"
                , guards = []
                , updates = []
                , end = "r0c"
                , uncontrollable = False
                }
        , Trans { start = "r0e"
                , event = "c6"
                , guards = []
                , updates = []
                , end = "r0c"
                , uncontrollable = False
                }
        , Trans { start = "r0e"
                , event = "m3"
                , guards = []
                , updates = []
                , end = "r0m"
                , uncontrollable = False
                }
        , Trans { start = "r0e"
                , event = "m6"
                , guards = []
                , updates = []
                , end = "r0m"
                , uncontrollable = False
                }
        , Trans { start = "r0m"
                , event = "m1"
                , guards = []
                , updates = []
                , end = "r0e"
                , uncontrollable = False
                }
        , Trans { start = "r0m"
                , event = "m4"
                , guards = []
                , updates = []
                , end = "r0e"
                , uncontrollable = False
                }
        ]
       , marked = [("r0e",[])]
       , initialLocation = "r0e"
       }
room1 =
   Aut { autName = "room1"
       , locations = S.fromList ["r1c","r1e","r1m"]
       , transitions =
        [ Trans { start = "r1c"
                , event = "c2"
                , guards = []
                , updates = []
                , end = "r1e"
                , uncontrollable = False
                }
        , Trans { start = "r1c"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "r1e"
                , uncontrollable = True
                }
        , Trans { start = "r1e"
                , event = "c1"
                , guards = []
                , updates = []
                , end = "r1c"
                , uncontrollable = False
                }
        , Trans { start = "r1e"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "r1c"
                , uncontrollable = True
                }
        , Trans { start = "r1e"
                , event = "m2"
                , guards = []
                , updates = []
                , end = "r1m"
                , uncontrollable = False
                }
        , Trans { start = "r1m"
                , event = "m3"
                , guards = []
                , updates = []
                , end = "r1e"
                , uncontrollable = False
                }
        ]
       , marked = [("r1e",[])]
       , initialLocation = "r1e"
       }
room2 =
   Aut { autName = "room2"
       , locations = S.fromList ["r2c","r2e","r2m"]
       , transitions =
        [ Trans { start = "r2c"
                , event = "c3"
                , guards = []
                , updates = []
                , end = "r2e"
                , uncontrollable = False
                }
        , Trans { start = "r2e"
                , event = "c2"
                , guards = []
                , updates = []
                , end = "r2c"
                , uncontrollable = False
                }
        , Trans { start = "r2e"
                , event = "m1"
                , guards = []
                , updates = []
                , end = "r2m"
                , uncontrollable = False
                }
        , Trans { start = "r2m"
                , event = "m2"
                , guards = []
                , updates = []
                , end = "r2e"
                , uncontrollable = False
                }
        ]
       , marked = [("r2c",[])]
       , initialLocation = "r2c"
       }
room3 =
   Aut { autName = "room3"
       , locations = S.fromList ["r3c","r3e","r3m"]
       , transitions =
        [ Trans { start = "r3c"
                , event = "c5"
                , guards = []
                , updates = []
                , end = "r3e"
                , uncontrollable = False
                }
        , Trans { start = "r3c"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "r3e"
                , uncontrollable = True
                }
        , Trans { start = "r3e"
                , event = "c4"
                , guards = []
                , updates = []
                , end = "r3c"
                , uncontrollable = False
                }
        , Trans { start = "r3e"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "r3c"
                , uncontrollable = True
                }
        , Trans { start = "r3e"
                , event = "m5"
                , guards = []
                , updates = []
                , end = "r3m"
                , uncontrollable = False
                }
        , Trans { start = "r3m"
                , event = "m6"
                , guards = []
                , updates = []
                , end = "r3e"
                , uncontrollable = False
                }
        ]
       , marked = [("r3e",[])]
       , initialLocation = "r3e"
       }
room4 =
   Aut { autName = "room4"
       , locations = S.fromList ["r4c","r4e","r4m"]
       , transitions = 
        [ Trans { start = "r4c"
                , event = "c6"
                , guards = []
                , updates = []
                , end = "r4e"
                , uncontrollable = False
                }
        , Trans { start = "r4e"
                , event = "c5"
                , guards = []
                , updates = []
                , end = "r4c"
                , uncontrollable = False
                }
        , Trans { start = "r4e"
                , event = "m4"
                , guards = []
                , updates = []
                , end = "r4m"
                , uncontrollable = False
                }
        , Trans { start = "r4m"
                , event = "m5"
                , guards = []
                , updates = []
                , end = "r4e"
                , uncontrollable = False
                }
        ]
       , marked = [("r4m",[])]
       , initialLocation = "r4m"
       }
monolitic_sup =
   Aut { autName = "monolithic_sup"
       , locations = S.fromList
        [ "m0.c2.r2c.r1e.r0m.r4e.r3e"
        , "m3.c2.r2c.r1e.r0e.r4e.r3m"
        , "m4.c0.r2e.r1e.r0c.r4m.r3e"
        , "m4.c1.r2e.r1c.r0e.r4m.r3e"
        , "m4.c2.r2c.r1e.r0e.r4m.r3e"
        , "m4.c3.r2e.r1e.r0e.r4m.r3c"
        ]
       , transitions =
        [ Trans { start = "m0.c2.r2c.r1e.r0m.r4e.r3e"
                , event = "m4"
                , guards = []
                , updates = []
                , end = "m4.c2.r2c.r1e.r0e.r4m.r3e"
                , uncontrollable = False
                }
        , Trans { start = "m3.c2.r2c.r1e.r0e.r4e.r3m"
                , event = "m6"
                , guards = []
                , updates = []
                , end = "m0.c2.r2c.r1e.r0m.r4e.r3e"
                , uncontrollable = False
                }
        , Trans { start = "m4.c0.r2e.r1e.r0c.r4m.r3e"
                , event = "c1"
                , guards = []
                , updates = []
                , end = "m4.c1.r2e.r1c.r0e.r4m.r3e"
                , uncontrollable = False
                }
        , Trans { start = "m4.c0.r2e.r1e.r0c.r4m.r3e"
                , event = "c4"
                , guards = []
                , updates = []
                , end = "m4.c3.r2e.r1e.r0e.r4m.r3c"
                , uncontrollable = False
                }
        , Trans { start = "m4.c1.r2e.r1c.r0e.r4m.r3e"
                , event = "c2"
                , guards = []
                , updates = []
                , end = "m4.c2.r2c.r1e.r0e.r4m.r3e"
                , uncontrollable = False
                }
        , Trans { start = "m4.c1.r2e.r1c.r0e.r4m.r3e"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "m4.c3.r2e.r1e.r0e.r4m.r3c"
                , uncontrollable = True
                }
        , Trans { start = "m4.c2.r2c.r1e.r0e.r4m.r3e"
                , event = "m5"
                , guards = []
                , updates = []
                , end = "m3.c2.r2c.r1e.r0e.r4e.r3m"
                , uncontrollable = False
                }
        , Trans { start = "m4.c2.r2c.r1e.r0e.r4m.r3e"
                , event = "c3"
                , guards = []
                , updates = []
                , end = "m4.c0.r2e.r1e.r0c.r4m.r3e"
                , uncontrollable = False
                }
        , Trans { start = "m4.c3.r2e.r1e.r0e.r4m.r3c"
                , event = "c7"
                , guards = []
                , updates = []
                , end = "m4.c1.r2e.r1c.r0e.r4m.r3e"
                , uncontrollable = True
                }
        ]
       , marked = [("m4.c2.r2c.r1e.r0e.r4m.r3e",[])]
       , initialLocation = "m4.c2.r2c.r1e.r0e.r4m.r3e"
       }  
       
catMouse :: Synchronisation
catMouse = Synch
 { automata =
 [mouse, cat, room2, room1, room0, room4, room3, monolitic_sup]
 , allEvents = ["m4","m6","c1","c4","c2","c7","m5"
               ,"c3","c5","c6","m3","m1","m2"]
 , allVars = M.fromList []
}