Config {
       font = "xft:Noto Sans:size=9:antialias=true,Noto Sans CJK SC:size=9:antialias=true"
       , bgColor = "#283339"
       , fgColor = "#F9fAF9"
       , position = TopW L 85
       , commands = [
                    Run Battery [ "--template" , "B: <acstatus>"
                                , "--L" , "15"
                                , "--H" , "75"
                                , "--low"      , "darkred"
                                , "--normal"   , "darkorange"
                                , "--high"     , "#1ABC9C"
                                , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#dAA520>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#1ABC9C>Charged</fc>"
                                ] 50
                    -- , Run Cpu [ "--template" , "C: <total>%", "-L","0","-H","50","--normal","#1ABC9C","--high","darkred"] 10
                    -- , Run Memory ["-t","M: <usedratio>%"] 10
                    , Run DiskU [("/", "D: <free>")] ["-L", "20", "-H", "60"] 10
                    -- , Run Swap [] 10
                    , Run Date "%a %d.%m. %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %battery% | %disku% | %date%"
       }
