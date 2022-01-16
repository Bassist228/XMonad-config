--                       _
-- __  ___ __ ___   ___ | |__   __ _ _ __
-- \ \/ / '_ ` _ \ / _ \| '_ \ / _` | '__|
--  >  <| | | | | | (_) | |_) | (_| | |
-- /_/\_\_| |_| |_|\___/|_.__/ \__,_|_|
--
-- My Xmobar configuration 

Config { font = "xft:Mononoki:pixelsize=16:antialias=true:hinting=false"
       , bgColor = "#1c1e26"
       , fgColor = "#f43e5c"
       , position = Top
       , lowerOnStart = False
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [ Run Date "%a, %b %d %Y, %H:%M:%S" "date" 10
		    , Run Com "uname" ["-r"] "" 3600
                    , Run Cpu ["-t", "cpu <bar> (<total>%)","-H","50","--high","red"] 10
                    , Run Memory ["-t", " mem <usedbar> (<usedratio>%)"] 10
                    , Run DiskU [("/", " hdd <usedbar> (<used>)")] [] 3600
		    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader% }{ <fc=#FAB795>%cpu%</fc> <fc=#E95678>%memory%</fc>  <fc=#B877DB>%disku%</fc> <fc=#8BE9FD>%date%</fc> <fc=#82AAFF>%uname%</fc> "
       }
