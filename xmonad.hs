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
    [  className =? "vlc"             --> doFloat
     , className =? "Gimp"            --> doFloat
     , className =? "feh"             --> doFloat
     , className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp"            --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
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
    spawnOnce "lxpolkit"
    spawnOnce "picom --experimental-backend &"
    setWMName "LG3D"
    spawnOnce "~/.fehbg &"
    spawnOnce "/usr/bin/emacs --daemon"

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
                  , keys               = myKeys
                      xmobarPP {
                                 ppOutput = hPutStrLn xmproc
                               , ppTitle = xmobarColor "#AAAAAA" "" . shorten 100
                               , ppCurrent = xmobarColor "#7aa2f7" ""
                               , ppSep = "   "
                               , ppOrder  = \(ws : l : _ : _ ) -> [ws,l]
                              }
