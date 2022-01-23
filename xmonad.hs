import XMonad
import Data.Monoid
import System.Exit
import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "qutebrowser"

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor = "#acb0d0"
myFocusedBorderColor = "#f43e5c"

myHandleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)
myManageHook = composeAll
    [  className =? "vlc"             --> doCenterFloat
     , className =? "Gimp"            --> doCenterFloat
     , className =? "feh"             --> doCenterFloat
     , className =? "confirm"         --> doCenterFloat
     , className =? "file_progress"   --> doCenterFloat
     , className =? "dialog"          --> doCenterFloat
     , className =? "download"        --> doCenterFloat
     , className =? "error"           --> doCenterFloat
     , className =? "Gimp"            --> doCenterFloat
     , className =? "notification"    --> doCenterFloat
     , className =? "splash"          --> doCenterFloat
     , className =? "toolbar"         --> doCenterFloat
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
    ]

myKeys :: [(String, X ())]
myKeys =
  [

          ("M-<Return>", windows W.swapMaster)
        , ("M-S-q", io exitSuccess)
        , ("M-q", spawn "xmonad --recompile")
        , ("M-S-r", spawn "xmonad --restart")
        , ("M-S-c", kill)
        , ("M-S-l", spawn "betterlockscreen --lock blur")

        , ("<Print>", spawn "scrot")
        , ("M-<Print>", spawn "scrot -s")

        , ("M-d", spawn "dmenu_run -i -p \"Run: \"")

        , ("M-f", spawn "firefox-bin")
        , ("M-p", spawn "pcmanfm")
        , ("M-e", spawn "emacsclient -c -a 'emacs' ")

        , ("M-S-<Return>", spawn (myTerminal))
        , ("M-<KP_End>", spawn (myTerminal ++ " -e htop"))
        , ("M-<KP_Down>", spawn (myTerminal ++ " -e ranger"))
        , ("M-<KP_Page_Down>", spawn (myTerminal ++ " -e newsboat"))
        , ("M-<KP_Insert>", spawn (myTerminal ++ " e spt"))
 ]

myStartupHook = do
    spawn "killall trayer"
    spawnOnce "lxpolkit &"
    spawnOnce "picom --experimental-backend &"
    setWMName "LG3D"
    spawnOnce "nitrogen --restore &"
    spawnOnce "volumeicon"
    spawnOnce "blueman-applet"
    spawnOnce "/usr/bin/emacs --daemon"
    spawn "sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true  --transparent true --alpha 0 --tint 0x1C1E26 --height 22"

tall =  spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True $ gaps [(U,29), (R,10), (L,10), (D,10)] $ avoidStruts $ Tall 1 (3/100) (50/100)

bsp =  spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True $ gaps [(U,29), (R,10), (L,10), (D,10)] $ emptyBSP

myLayoutHook = tall ||| noBorders tall |||  bsp ||| noBorders Full ||| simplestFloat

main = do
      xmproc <- spawnPipe "dbus-launch xmobar -d"
      xmonad $ docks def {
                    startupHook        = myStartupHook
                  , modMask            = mod4Mask
                  , borderWidth        = myBorderWidth
                  , normalBorderColor  = myNormalBorderColor
                  , focusedBorderColor = myFocusedBorderColor
                  , layoutHook         = myLayoutHook
                  , manageHook         = myManageHook
                  , logHook            = dynamicLogWithPP $
                      xmobarPP {
                                 ppOutput = hPutStrLn xmproc
                               , ppTitle = xmobarColor "#AAAAAA" "" . shorten 100
                               , ppCurrent = xmobarColor "#7aa2f7" ""
                               , ppSep = "   "
                               , ppOrder  = \(ws : l : _ : _ ) -> [ws,l]
                               }
                      } `additionalKeysP` myKeys
