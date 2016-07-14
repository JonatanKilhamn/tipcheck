module WmodParser where

import Text.XML.Light
import System.IO
import TransitionSystem
import qualified Data.Set as S
import Control.Monad
import Data.List
import Data.Maybe

readWmodFile :: FilePath -> IO Synchronisation
readWmodFile fp =
 do
  s <- readFile fp
  let xmlCont = parseXML s
  case (parseWmodXml xmlCont) of
       (Just s) -> return s
       (Nothing) -> return emptySynch

main :: IO Synchronisation
main = readWmodFile "Examples/false_guard.wmod"
       --"Examples/cat_mouse.wmod"

debug :: IO String
debug = readFile "Examples/cat_mouse.wmod"



elemName :: String -> (Content -> Bool)
elemName s (Elem e) = (qName $ elName e) == s
elemName _ _        = False

getElem :: Content -> Maybe Element
getElem (Elem e) = return e
getElem _        = Nothing

getElemName :: Element -> String
getElemName = qName . elName


parseWmodXml :: [Content] -> Maybe Synchronisation
parseWmodXml cs =
 do
   cl <- firstOccurrence (elemName "ComponentList") getElem cs
   let scConts = filter (elemName "SimpleComponent") (elContent cl)
       scElems = mapMaybe getElem scConts
   auts <- mapM parseAutomaton scElems
   let synch = foldr synchronise emptySynch auts
   
   -- make some transitions uncontrollable
   edl <- firstOccurrence (elemName "EventDeclList") getElem cs
   let edConts = filter (elemName "EventDecl") (elContent edl)
       edElems = mapMaybe getElem edConts
       uncontEvents = mapMaybe (getAttribute "Name") $
                      filter isUncontrollable edElems
   let synch1 = foldr setEventUncontrollable synch uncontEvents
   
   -- handle variables
   let vcConts = filter (elemName "VariableComponent") (elContent cl)
       vcElems = mapMaybe getElem vcConts
   -- TODO: what do we actually do with them? Set defaults, yes. Set ranges?

   -- set initial values of variables
   -- TODO
   
   
   return synch1
   

parseAutomaton :: Element -> Maybe Automaton
parseAutomaton e
 | getElemName e /= "SimpleComponent" = Nothing
 | otherwise =
  do
   -- Automaton name
   aName <- getAttribute "Name" e

   graph <- firstOccurrence (elemName "Graph") getElem (elContent e)
   
   -- Locations
   nodeList <- firstOccurrence (elemName "NodeList") getElem (elContent graph)
   (locs, initLoc) <- parseLocations nodeList
   
   -- Transitions
   edgeList <- firstOccurrence (elemName "EdgeList") getElem (elContent graph)
   transitions <- parseTransitions edgeList
   
   -- Marked / forbidden states
   -- TODO
   
   return Aut { autName = aName
              , locations = locs
              , transitions = transitions -- :: [Transition]
              , marked = [] -- :: [Predicate]
              , initialLocation = initLoc -- :: Location
              }

parseLocations :: Element -> Maybe (S.Set Location, Location)
parseLocations e
 | getElemName e /= "NodeList" = Nothing
 | otherwise =
  do
   let nodes = mapMaybe getElem $ filter (elemName "SimpleNode") (elContent e)
       locations = mapMaybe (getAttribute "Name") nodes
   initLoc <- findInXml isInitial (getAttribute "Name") nodes
   return (S.fromList locations, initLoc)

parseTransitions :: Element -> Maybe [Transition]
parseTransitions e
 | getElemName e /= "EdgeList" = Nothing
 | otherwise =
  do
   let edges = mapMaybe getElem $ filter (elemName "Edge") (elContent e)
       transitions = concat $ mapMaybe parseTransition edges
   return transitions
 where getTwoAttributes = flip $ double . (flip getAttribute)


parseTransition :: Element -> Maybe [Transition]
parseTransition e
 | getElemName e /= "Edge" = Nothing
 | otherwise =
  do
   from <- getAttribute "Source" e
   to <- getAttribute "Target" e
   labelBlock <- firstOccurrence (elemName "LabelBlock") getElem (elContent e)
   let ids = mapMaybe getElem $ filter (elemName "SimpleIdentifier")
                                       (elContent labelBlock)
       names = mapMaybe (getAttribute "Name") ids
       
   
   -- handle guards and updates
   -- TODO
   
   return [ Trans { start = from
                  , event = name
                  , guards = []
                  , updates = []
                  , end = to
                  , uncontrollable = False
                  }
          | name <- names ]



double :: (a -> Maybe b) -> (a,a) -> Maybe (b,b)
double fun (x,y) =
 do
  f <- fun x
  g <- fun y
  return (f,g)


getAttribute :: String -> Element -> Maybe String
getAttribute s e =
 do
  attr <- find (\a -> (qName $ attrKey a) == s) (elAttribs e)
  return $ attrVal attr


isInitial :: Element -> Bool
isInitial e = (getAttribute "Initial" e) == (Just "true")

isUncontrollable :: Element -> Bool
isUncontrollable e = (getAttribute "Kind" e) == (Just "UNCONTROLLABLE")



{-
filterXmlContent :: String -> [Content] -> [Element]
filterXmlContent s cs = mapMaybe getElem $ filter (elemName s) cs
-}

findInXml :: (a -> Bool) -> (a -> Maybe b) -> [a] ->
  Maybe b
findInXml pred fun cs = find pred cs >>= fun


firstOccurrence :: (Content -> Bool) -> (Content -> Maybe a) -> [Content] ->
  Maybe a
firstOccurrence a b cs = findInXml a b (flattenContent cs)


flattenContent :: [Content] -> [Content]
flattenContent = foldr expandAndAdd []
 where
  expandAndAdd (Elem e) rest = (Elem e) : (elContent e) ++ rest
  expandAndAdd _        rest = rest

