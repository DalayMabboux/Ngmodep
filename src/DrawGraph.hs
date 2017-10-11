module DrawGraph where

import Data.List (find)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Graph.Inductive (Gr, mkGraph)
import Data.Graph.Inductive.Graph (LNode, labEdges, labNodes, insNode, insEdge)
import Data.GraphViz (
  GraphvizParams,
  GlobalAttributes(
    GraphAttrs,
    NodeAttrs
    ),
  X11Color(Transparent, White),
  nonClusteredParams,
  globalAttributes,
  fmtNode,
  fmtEdge,
  graphToDot
  )
import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Attributes.Complete (
  DPoint(DVal),
  Attribute(
    Margin,
    Pad,
    Center,
    BgColor,
    FontSize,
    Shape,
    Label,
    ViewPort,
    RankDir,
    Style,
    FillColor
    ),
  Shape(Circle, BoxShape),
  Label(StrLabel),
  ViewPort(VP),
  RankDir(FromLeft),
  StyleName(Filled),
  StyleItem(SItem),
  toWColor,
  wVal,
  hVal,
  zVal,
  focus
  )
import Types (
  NgGraph(..), ImportStmt(..), ExportStmt(..), ImpExports(..)
  )

addAndGet :: NgGraph -> String -> (NgGraph, Int)
addAndGet g s = case (get g s) of
                  Nothing -> (NgGraph (insNode (ni, s) (gr g)), ni)
                  Just i -> (g, i)
                  where ni = nextInt g

get :: NgGraph -> String -> Maybe Int
get ng s = findX (labNodes $ gr ng) f
              where
                findX :: [LNode a] -> Maybe Int
                findX = snd <$> find (\(s', i) -> s == s')
                f :: (String, Int) -> String -> Maybe Int
                f (s, i) ss = case s == ss of
                                true -> Just i
                                false -> Nothing

nextInt :: NgGraph -> Int
nextInt g = length $ labEdges $ Types.gr g

merge :: NgGraph -> ImpExports -> NgGraph
merge ng (ImpExports (ImportStmt is, ExportStmt e)) =  NgGraph (foldl f (gr ng) is)
                                                       where
                                                        f a i = insEdge (ixi, ixe, undefined) (gr ng'')
                                                          where (ng'', ixi) = addAndGet ng' i
                                                        (ng', ixe) = case e of
                                                            Nothing -> (ng, 0)
                                                            Just e -> addAndGet ng e

{-
create :: [([String],[String])] -> Gr Text Text
create l = foldl f (mkGraph [] []) l
  where
    f :: Gr Text Text -> ([String], String) -> Gr Text Text
    f g (is, e) = let (is, g') = getNode g s
                       (id, g'') = getNode g' d
                       g''' = addEdge g'' is id
                   in g'''
    getNode :: Gr Text Text -> String -> (Int, Gr Text Text)
    getNode g s = case findNode g s of
                        Nothing -> undefined
                        Just a -> (a, g)
    findNode :: Gr Text Text -> String -> Maybe Int
    findNode g s = undefined
    addEdge :: Gr Text Text -> Int -> Int -> Gr Text Text
    addEdge g i1 i2 = undefined

gnomes :: Gr Text Text
gnomes = mkGraph [(1, pack "Collect underpants"), (3, pack "Profit")] [(1, 3, pack "?")]

gnomeParams :: GraphvizParams n Text Text () Text
gnomeParams = nonClusteredParams {
  globalAttributes = ga,
  fmtNode = fn,
  fmtEdge = fe
  }
  where
    ga = [
      GraphAttrs [
         RankDir FromLeft,
         BgColor [toWColor Transparent]
         ],
      NodeAttrs [
        Shape BoxShape,
        FillColor [toWColor White],
        Style [SItem Filled []]
        ]
      ]

    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]

main :: IO ()
main = putStr $ unpack $ renderDot $ toDot $ graphToDot gnomeParams gnomes
-}
