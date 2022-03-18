module Simiones.DownloadTxt exposing (..)
import File.Download as Download
import Fongf2.Graph as Graph
import Dict exposing (Dict)

save : String -> Cmd msg
save markdown =
  Download.string "graph.csv" "text/markdown" markdown

adjacencyList : Graph.Graph -> String
adjacencyList graph = adjacencyListHelp (Dict.toList graph)

adjacencyListHelp : List (String, Graph.Node) -> String
adjacencyListHelp nodeList =
    case nodeList of
        [] -> ""
        (key,node)::rest  -> key ++ "," ++ (edgeListToString node.edges) ++ "\n" ++ (adjacencyListHelp rest)

edgeListToString : List (String) -> String
edgeListToString edgeList =
    case edgeList of
        [] -> ""
        [s]  -> s
        s :: rest -> s ++ "," ++ (edgeListToString rest)
