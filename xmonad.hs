import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import Control.Applicative
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders(smartBorders, noBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.GridVariants
import XMonad.Layout.SimpleDecoration (shrinkText)
import XMonad.Util.Replace
import System.IO
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.GroupNavigation
import System.Exit (exitWith, ExitCode(..))

myKeys =
  [
    -- Swap screen order
    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..2]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
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
  , ((mod4Mask .|. shiftMask, xK_m), spawn "firefox -private-window")
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


keysToRemove =
  [
    (mod4Mask .|. shiftMask, xK_c)
  , (mod4Mask .|. shiftMask, xK_q)
  , (mod4Mask, xK_q)
  ]

myTabConfig = def
  { activeColor = "#556064"
  , inactiveColor = "#2F3D44"
  , urgentColor = "#FDF6E3"
  , activeBorderColor = "#454948"
  , inactiveBorderColor = "#454948"
  , urgentBorderColor = "#268BD2"
  , activeTextColor = "#80FFF9"
  , inactiveTextColor = "#1ABC9C"
  , urgentTextColor = "#1ABC9C"
  , fontName = "xft:Noto Sans CJK:size=10:antialias=true"
  }

myLayoutHook =
  -- (smartBorders $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ Grid (16/9)) |||
  (smartBorders $ smartSpacingWithEdge 4 $ ResizableTall 1 (3/100) (1/2) []) |||
  (smartBorders $ smartSpacingWithEdge 4 $ Mirror (ResizableTall 1 (3/100) (1/2) [])) |||
  -- (smartBorders $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ TwoPane (3/100) (1/2)) |||
  (noBorders $ tabbed shrinkText myTabConfig)

conf = def
  { modMask            = mod4Mask
  , terminal           = "termite"
  , borderWidth        = 3
  , startupHook        = setWMName "LG3D"
  , focusedBorderColor = "#cccccc"
  , normalBorderColor  = "#000000"
  , focusFollowsMouse  = False
  , manageHook         = manageDocks <+> manageHook def
  , handleEventHook    = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
  , layoutHook         = avoidStruts $ myLayoutHook
  }
  `removeKeys` keysToRemove
  `additionalKeys` myKeys

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  xmonad $ ewmh conf
    {
      logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle  = xmobarColor "green" "" . shorten 50
      } >> historyHook
    }
