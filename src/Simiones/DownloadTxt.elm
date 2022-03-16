module Simiones.DownloadTxt exposing (..)
import File.Download as Download

save : String -> Cmd msg
save text =
  Download.string "graph.csv" "text/markdown" text

