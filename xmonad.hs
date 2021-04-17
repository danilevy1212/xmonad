import System.Exit (exitSuccess)
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative ()
import qualified DBus as D
import qualified DBus.Client as D
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPlay,
    xF86XK_AudioPrev,
    xF86XK_AudioRaiseVolume,
    xF86XK_AudioStop,
  )
import XMonad
  ( Default (def),
    KeyMask,
    KeySym,
    LayoutMessages (ReleaseResources),
    X,
    XConfig
      ( borderWidth,
        clickJustFocuses,
        focusFollowsMouse,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal
      ),
    broadcastMessage,
    controlMask,
    io,
    kill,
    mod4Mask,
    restart,
    screenWorkspace,
    sendMessage,
    shiftMask,
    spawn,
    whenJust,
    windows,
    xK_Return,
    xK_Tab,
    xK_b,
    xK_c,
    xK_e,
    xK_f,
    xK_j,
    xK_k,
    xK_m,
    xK_p,
    xK_q,
    xK_r,
    xK_s,
    xK_w,
    xK_x,
    xK_z,
    xmonad,
    (.|.),
    (<+>),
    (|||),
  )
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppHidden,
        ppLayout,
        ppOutput,
        ppSep,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
      ),
    dynamicLogWithPP,
    shorten,
    wrap,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (ToggleStruts),
    avoidStruts,
    docksEventHook,
    manageDocks,
  )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.SimpleDecoration
  ( Theme
      ( activeBorderColor,
        activeColor,
        activeTextColor,
        fontName,
        inactiveBorderColor,
        inactiveColor,
        inactiveTextColor,
        urgentBorderColor,
        urgentColor,
        urgentTextColor
      ),
    shrinkText,
  )
import XMonad.Layout.Spacing (Border (Border), spacingRaw)
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.TwoPane (TwoPane (TwoPane))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, removeKeys)

-- TODO Get the colors from Xresouces --> https://www.reddit.com/r/xmonad/comments/9uqz76/using_x_resources_in_xmonadhs_and_xmobarrc/
-- https://github.com/mpenet/xmonad/blob/master/.Xresources

black :: String
black = "#282c34"

red :: String
red = "#be5046"

cyan :: String
cyan = "#56b6c2"

yellow :: String
yellow = "#e5c07b"

-- magenta :: String
-- magenta = "#9a52af"

blue :: String
blue = "#3b84c0"

white :: String
white = "#ffffff"

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ -- Swap screen order
    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_q, xK_w, xK_e] [0 .. 2],
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
    ++
    -- Navigation
    [ -- Move focus to the next window
      ((mod4Mask, xK_Tab), windows W.focusDown),
      -- Move focus to the previous window
      ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp),
      -- Move focus to the next window
      ((mod4Mask, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((mod4Mask, xK_k), windows W.focusUp),
      -- Move focus to the master window
      ((mod4Mask, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((mod4Mask, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp),
      -- Replace killing a window definition
      ((mod4Mask, xK_c), kill),
      -- Managing struts
      ((mod4Mask .|. shiftMask, xK_b), sendMessage ToggleStruts)
    ]
    -- FIXME Add mouse control. drag, switch, resize...
    ++
    -- Spawning processes
    [ -- FIXME switch to brave!
      ((mod4Mask, xK_f), spawn "emacs"),
      ((mod4Mask .|. shiftMask, xK_f), spawn "emacsclient --eval '(emacs-everywhere)'"),
      ((mod4Mask, xK_b), spawn "brave"),
      -- , ((mod4Mask, xK_g), spawn "if [ ! $(command -v nvim-qt ) ]; then gvim; else nvim-qt; fi") FIXME Deprecated
      ((mod4Mask .|. shiftMask, xK_b), spawn "firefox -private-window"),
      ((mod4Mask .|. shiftMask, xK_s), spawn "spotify"),
      ((mod4Mask, xK_p), spawn "dmenu_run -l 10 -i"), -- FIXME Use native Prompt from contrib instead!
      ((mod4Mask, xK_x), spawn "flameshot gui")
    ]
    ++
    -- Restart xmonad
    [ ( (mod4Mask, xK_r),
        broadcastMessage ReleaseResources >> restart "xmonad" True
      ),
      -- Quit xmonad
      ( (mod4Mask .|. shiftMask, xK_r),
        io (exitSuccess)
      ),
      -- Lock screen with a cool screensaver
      ((mod4Mask .|. shiftMask, xK_z), spawn "slock"),
      -- Lock screen turning off monitor
      ((mod4Mask .|. shiftMask .|. controlMask, xK_z), spawn "xscreensaver-command -suspend")
    ]
    ++
    -- media keys
    [ ((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
      ((0, xF86XK_AudioStop), spawn "playerctl stop"),
      ((0, xF86XK_AudioNext), spawn "playerctl next"),
      ((0, xF86XK_AudioPrev), spawn "playerctl previous"),
      ((0, xF86XK_AudioLowerVolume), spawn "pulsemixer --change-volume -5"),
      ((0, xF86XK_AudioRaiseVolume), spawn "pulsemixer --change-volume +5"),
      ((0, xF86XK_AudioMute), spawn "pulsemixer --toggle-mute")
    ]

keysToRemove :: [(KeyMask, KeySym)]
keysToRemove =
  [ (mod4Mask .|. shiftMask, xK_c),
    (mod4Mask .|. shiftMask, xK_q),
    (mod4Mask, xK_q)
  ]

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = cyan,
      inactiveColor = black,
      urgentColor = red,
      activeBorderColor = cyan,
      inactiveBorderColor = blue,
      urgentBorderColor = red,
      activeTextColor = white,
      inactiveTextColor = white,
      urgentTextColor = white,
      fontName = "xft:Hack:size=13:antialias=true"
    }

myLayoutHook = smartBorders $ myGrid ||| myTwoPane ||| myTabbedLayout
  where
    myBorder = Border 10 10 10 10
    mySpacing = spacingRaw True myBorder True myBorder True
    myGrid = mySpacing $ Grid (16 / 9)
    myTwoPane = mySpacing $ TwoPane (3 / 100) (1 / 2)
    myTabbedLayout = noBorders $ tabbed shrinkText myTabConfig

-- FIXME Use https://github.com/polybar/polybar/wiki/User-contributed-modules instead
-- Remove dependency with DBus!

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus,
      ppCurrent = wrap ("%{B" ++ cyan ++ "} ") " %{B-}",
      ppVisible = wrap ("%{B" ++ blue ++ "} ") " %{B-}",
      ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}",
      ppHidden = wrap ("%{B" ++ black ++ "} ") " %{B-}",
      ppLayout = const "",
      ppWsSep = "",
      ppSep = "  ",
      ppTitle = wrap ("%{F" ++ yellow ++ "} ") "%{F-}" . shorten 20
    }

--

main :: IO ()
main = do
  dbus <- D.connectSession
  _ <-
    D.requestName
      dbus
      (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $
    ewmh $
      def
        { modMask = mod4Mask,
          terminal = "alacritty",
          borderWidth = 1,
          focusedBorderColor = cyan,
          normalBorderColor = black,
          focusFollowsMouse = False,
          clickJustFocuses = False,
          manageHook = manageDocks <+> manageHook def,
          handleEventHook =
            handleEventHook def
              <+> docksEventHook
              <+> fullscreenEventHook,
          layoutHook = avoidStruts myLayoutHook,
          logHook = dynamicLogWithPP (myLogHook dbus),
          startupHook = setWMName "LG3D"
        }
        `removeKeys` keysToRemove
        `additionalKeys` myKeys
