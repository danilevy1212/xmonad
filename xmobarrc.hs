Config {
  font = "xft:ubuntu mono:size=10:antialias=true,ubuntu mono CJK SC:size=10:antialias=true"
  , bgColor = "#283339"
  , fgColor = "#F9fAF9"
  , position = TopW L 85
  , commands =
      [
        Run Cpu [ "--template" , "C: <total>%", "-L","0","-H","50","--normal","#1ABC9C","--high","darkred"] 10
      , Run Memory ["-t","M: <usedratio>%"] 10
      , Run DiskU [("/", "D: <free>")] ["-L", "20", "-H", "60"] 10
      , Run Swap [] 10
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %cpu% | %memory% | %swap% | %disku% | %date%"
  }
