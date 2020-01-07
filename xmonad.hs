import           Control.Applicative()
import           System.Exit (exitWith, ExitCode(..))
import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.GridVariants
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.SimpleDecoration (shrinkText)
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys, removeKeys)
import           Graphics.X11.ExtraTypes.XF86

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

black :: String
black = "#282c34"

red   :: String
red   = "#be5046"

cyan  :: String
cyan  = "#56b6c2"

yellow :: String
yellow = "#e5c07b"

-- magenta :: String
-- magenta = "#9a52af"

blue :: String
blue = "#3b84c0"

myKeys =
  [
    -- Swap screen order
    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) |
    (key, sc) <- zip [xK_q, xK_w, xK_e] [0..2] , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  ++
  -- Navigation
  [
    -- Move focus to the next window
    ((mod4Mask, xK_Tab), windows W.focusDown)
    -- Move focus to the previous window
  , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    -- Move focus to the next window
  , ((mod4Mask, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((mod4Mask, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((mod4Mask, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((mod4Mask, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp)
    -- Replace killing a window definition
  , ((mod4Mask, xK_c), kill)
    -- Managing struts
  , ((mod4Mask .|. shiftMask, xK_b), sendMessage ToggleStruts)
  ]
  ++
  -- Spawning processes
  [
    ((mod4Mask, xK_f), spawn "emacs")
  , ((mod4Mask, xK_m), spawn "firefox")
  , ((mod4Mask, xK_g), spawn "gvim") , ((mod4Mask .|. shiftMask, xK_m), spawn "firefox -private-window")
  , ((mod4Mask .|. shiftMask, xK_s), spawn "spotify")
  , ((mod4Mask, xK_p), spawn "dmenu_run -l 10 -i")
  , ((mod4Mask, xK_x), spawn "maim -s | xclip -selection clipboard -t image/png")
  , ((mod4Mask .|. shiftMask, xK_x), spawn "maim -s ~/Pictures/$(date +%s).png")
  ]
  ++
  -- Restart xmonad
  [
    ((mod4Mask, xK_r),
      broadcastMessage ReleaseResources >> restart "xmonad" True)
  -- Quit xmonad
  , ((mod4Mask .|. shiftMask, xK_r),
      io (exitWith ExitSuccess))
  -- Lock screen with a cool screensaver
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  -- Lock screen turning off monitor
  , ((mod4Mask .|. shiftMask .|. controlMask, xK_z), spawn "xscreensaver-command -suspend")
  ]
  ++
  -- media keys
  [
    ((0, xF86XK_AudioPlay)        , spawn "playerctl play-pause")
  , ((0, xF86XK_AudioStop)        , spawn "playerctl stop")
  , ((0, xF86XK_AudioNext)        , spawn "playerctl next")
  , ((0, xF86XK_AudioPrev)        , spawn "playerctl previous")
  , ((0, xF86XK_AudioLowerVolume) , spawn "pactl set-sink-volume 0 -5%")
  , ((0, xF86XK_AudioRaiseVolume) , spawn "pactl set-sink-volume 0 +5%")
  , ((0, xF86XK_AudioMute)        , spawn "pactl set-sink-mute 0 toggle")
  ]

keysToRemove =
  [
    (mod4Mask .|. shiftMask, xK_c)
  , (mod4Mask .|. shiftMask, xK_q)
  , (mod4Mask, xK_q)
  ]

myTabConfig :: Theme
myTabConfig = def
              {
                activeColor         = "#556064"
              , inactiveColor       = "#2F3D44"
              , urgentColor         = "#FDF6E3"
              , activeBorderColor   = "#454948"
              , inactiveBorderColor = "#454948"
              , urgentBorderColor   = "#268BD2"
              , activeTextColor     = "#80FFF9"
              , inactiveTextColor   = "#1ABC9C"
              , urgentTextColor     = "#1ABC9C"
              , fontName            = "xft:Ubuntu Mono:size=13:antialias=true"
              }

myLayoutHook =
  smartBorders $ myGrid ||| myTwoPane ||| myTabbedLayout
  where
    myBorder       = Border 10 10 10 10
    mySpacing      = spacingRaw True myBorder True myBorder True
    myGrid         = mySpacing $ Grid (16/9)
    myTwoPane      = mySpacing $ TwoPane (3/100) (1/2)
    myTabbedLayout = tabbed shrinkText myTabConfig

conf = def
       { modMask            = mod4Mask
       , terminal           = "termite"
       , borderWidth        = 3
       , startupHook        = setWMName "LG3D"
       , focusedBorderColor = cyan
       , normalBorderColor  = black
       , focusFollowsMouse  = False
       , clickJustFocuses   = False
       , manageHook         = manageDocks <+> manageHook def
       , handleEventHook    = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
       , layoutHook         = avoidStruts $ myLayoutHook
       }
       `removeKeys` keysToRemove
       `additionalKeys` myKeys

myLogHook :: D.Client -> PP
myLogHook dbus = def
                 { ppOutput  = dbusOutput dbus
                 , ppCurrent = wrap ("%{B" ++ cyan ++ "} ") " %{B-}"
                 , ppVisible = wrap ("%{B" ++ blue ++ "} ") " %{B-}"
                 , ppUrgent  = wrap  ("%{F" ++ red ++ "} ") " %{F-}"
                 , ppHidden  = wrap  ("%{B" ++ black ++ "} ") " %{B-}"
                 , ppLayout  = \_ -> ""
                 , ppWsSep   = ""
                 , ppSep     = "  "
                 , ppTitle   = wrap ("%{F" ++ yellow ++ "} ") "%{F-}". shorten 30
                 }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName)
        {
          D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

main :: IO ()
main = do
  spawn "run-polybar" -- ln run-polybar .local/bin/
  dbus <- D.connectSession
  _ <- D.requestName dbus
       (D.busName_ "org.xmonad.Log")
       [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $ ewmh conf
    {
      logHook = dynamicLogWithPP (myLogHook dbus)
    }
