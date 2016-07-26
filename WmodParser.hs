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



elemName :: String -> Content -> Bool
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
   acceptingPredicates <- parseAccepting nodeList
   
   return Aut { autName = aName
              , locations = locs
              , transitions = transitions -- :: [Transition]
              , marked = acceptingPredicates
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

parseAccepting :: Element -> Maybe [Predicate]
parseAccepting e
 | getElemName e /= "NodeList" = Nothing
 | otherwise =
  do
   let nodes = mapMaybe getElem $ filter (elemName "SimpleNode") (elContent e)
       acceptingNames = findAllInXml isAccepting (getAttribute "Name") nodes
   return $ acceptingNames `zip` (repeat [])

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
   
   guardBlock <- firstOccurrence (elemName "Guards") getElem (elContent e)
   let exprElems = mapMaybe getElem (elContent guardBlock)
       gs = mapMaybe (exprToGuard <=< parseExpr) exprElems
   
   return [ Trans { start = from
                  , event = name
                  , guards = gs
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
isInitial = hasAttrVal "Initial" "true"

isAccepting :: Element -> Bool
isAccepting e = isJust $ firstOccurrence isAccCont Just (elContent e)
 where isAccCont c = (fmap (hasAttrVal "Name" ":accepting") (getElem c)) == (Just True)


hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal attr val e = (getAttribute attr e) == (Just val)



isUncontrollable :: Element -> Bool
isUncontrollable = hasAttrVal "Kind" "UNCONTROLLABLE"



{-
filterXmlContent :: String -> [Content] -> [Element]
filterXmlContent s cs = mapMaybe getElem $ filter (elemName s) cs
-}

findInXml :: (a -> Bool) -> (a -> Maybe b) -> [a] ->
  Maybe b
findInXml pred fun cs = find pred cs >>= fun

findAllInXml :: (a -> Bool) -> (a -> Maybe b) -> [a] -> [b]
findAllInXml pred fun cs = mapMaybe fun (filter pred cs)

firstOccurrence :: (Content -> Bool) -> (Content -> Maybe a) -> [Content] ->
  Maybe a
firstOccurrence a b cs = findInXml a b (flattenContent cs)


flattenContent :: [Content] -> [Content]
flattenContent = foldr expandAndAdd []
 where
  expandAndAdd (Elem e) rest = (Elem e) : (flattenContent (elContent e)) ++ rest
  expandAndAdd _        rest = rest


exprToGuard :: Expr -> Maybe Guard
exprToGuard (BO Equals e1 e2) = toGuard e1 e2
 where toGuard (Var x) (IntConst n) = return $ Guard { gvar = x
                                                     -- TODO: integer variables
                                                     , gval = (n >= 0)
                                                     }
       toGuard a@(IntConst n) b@(Var x) = toGuard b a
       toGuard _ _ = Nothing
exprToGuard _ = Nothing



------------------
--- parsing expressions


parseExpr :: Element -> Maybe Expr
parseExpr e
 | getElemName e == "BinaryExpression" =
  do
   op <- parseBinaryOperator e
   -- This section relies on all well-formed input having exactly
   -- two sub-elements to every BinaryExpression element.
   let args = mapMaybe getElem (elContent e)
   arg1 <- parseExpr $ args!!0
   arg2 <- parseExpr $ args!!1
   return $ BO op arg1 arg2
 | getElemName e == "UnaryExpression" =
  do
   return undefined
 | getElemName e == "SimpleIdentifier" =
  do
   var <- getAttribute "Name" e
   return $ Var var
 | getElemName e == "IntConstant" =
  do
   val <- getAttribute "Value" e
   return $ IntConst (read val)
   



parseBinaryOperator :: Element -> Maybe BinaryOp
parseBinaryOperator e
 = case (getAttribute "Operator" e) of
        (Just "==")   -> return Equals
        (Just "=")    -> return Assign
        (Just "&lt;") -> return LessThan
        (Just "&gt;") -> return GreaterThan
        (Just "&le;") -> return LessThanEq
        (Just "&lg;") -> return GreaterThanEq
        (Just "+")    -> return Plus
        (Just "-")    -> return Minus
        (Nothing)     -> Nothing
        






