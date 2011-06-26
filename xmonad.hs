--
-- David Beckingsale's xmonad config
--
-- Started out as avandael's xmonad.hs
-- Also uses stuff from pbrisbin.com:8080/
--

--{{{ Imports
import Data.List

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib

import System.IO

import XMonad

import XMonad.Actions.GridSelect

import XMonad.Core

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell

import XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import qualified Data.Map as M
--}}}

--{{{ Helper Functions
stripIM s = if ("IM " `isPrefixOf` s) then drop (length "IM ") s else s

wrapIcon icon = "^p(5)^i(" ++ icons ++ icon ++ ")^p(5)"
--}}}

--{{{ Path variables
icons = "/home/david/.icons/"
--}}}

main = do
   myStatusBarPipe <- spawnPipe myStatusBar
   conkyBar <- spawnPipe myConkyBar
   xmonad $ myUrgencyHook $ defaultConfig
      { terminal = "urxvtc"
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , borderWidth = myBorderWidth
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = mod4Mask
      , keys = myKeys
      , XMonad.Core.workspaces = myWorkspaces
      , startupHook = setWMName "LG3D"
      , focusFollowsMouse = False
     }

--{{{ Theme

--Font
myFont = "Terminus-6"

-- Colors

--- Main Colours
myFgColor = "#aaaaaa"
myBgColor = "#222222"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#93d44f"

--- Borders
myActiveBorderColor = myCurrentWsBgColor
myInactiveBorderColor = "#555753"
myBorderWidth = 2

--- Ws Stuff
myCurrentWsFgColor = "#222222"
myCurrentWsBgColor = myHighlightedBgColor
myVisibleWsFgColor = myBgColor
myVisibleWsBgColor = "#c8e7a8"
myHiddenWsFgColor = "#FFFFFF"
myHiddenEmptyWsFgColor = "#8F8F8F"
myUrgentWsBgColor = "#ff6565"
myTitleFgColor = myFgColor


--- Urgency
myUrgencyHintFgColor = "#000000"
myUrgencyHintBgColor = "#ff6565"

-- }}}

-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -h '18'" ++ " -e 'onstart=lower' -fn '" ++ myFont ++ "'"

-- Status Bar
myStatusBar = "dzen2 -w 1920 -ta l " ++ myDzenGenOpts

-- Conky Bar
myConkyBar = "conky -c ~/.conky_bar | dzen2 -x 0 -y 1500 -w 1920  -ta c " ++ myDzenGenOpts
-- myMPDBar = "conky -c ~/.conky_mpd | dzen2 -x 1700 -w 320 -ta r " ++ myDzenGenOpts

-- Layouts
myLayoutHook = avoidStruts $ onWorkspace " 4 im " imLayout $ standardLayouts
               where standardLayouts = tiled ||| Mirror tiled ||| Full
                     imLayout = withIM (2/10) (Role "buddy_list") (standardLayouts)
                     tiled = ResizableTall nmaster delta ratio []
                     nmaster = 1
                     delta = 0.03
                     ratio = 0.5
-- Workspaces
myWorkspaces =
   [
      " 1 sh ",
      " 2 ed ",
      " 3 www ",
      " 4 im ",
      " 5 vm ",
      " 6 doc ",
      " 7 ",
      " . "
   ]

-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "1180", "-h", "20", "-w", "1920",
         "-ta", "c",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ "",
         "-fn", "" ++ myFont ++ ""
         ]
    }

--{{{ Hook for managing windows
myManageHook = (composeAll
   [ resource  =? "Do"               --> doIgnore,              -- Ignore GnomeDo
     className =? "Pidgin"           --> doShift " 4 im ",      -- Shift Pidgin to im desktop
     className =? "Chrome"           --> doShift " 3 www ",     -- Shift Chromium to www
     className =? "Firefox"          --> doShift " 3 www ",     -- Shift Firefox to www
     className =? "Emacs"            --> doShift " 2 ed ",      -- Shift emacs to ed workspace
     className =? "Gvim"	     --> doShift " 2 ed ",      -- shift gvim to ed workspace
     className =? "Wicd-client.py"   --> doFloat,                -- Float Wicd window
     isFullscreen 		     --> (doF W.focusDown <+> doFullFloat)
    , className =? "Tilda"          --> doFloat
   ]) <+> manageScratchpad

manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.2
    w = 1
    t = 1 - h
    l = 1 - w
--}}}

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

--{{{ Keybindings
--    Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  ((modm, xK_u), spawn "dmenu_run -nb '#222222' -nf '#aaaaaa' -sb '#93d44f' -sf '#222222'"),  --Uses a colourscheme with dmenu
  ((modm, xK_b), spawn "firefox"),
  ((modm, xK_c), spawn "chromium --app='https://calendar.google.com'"),
  ((modm, xK_f), spawn "urxvt -e mc"),
  ((modm, xK_m), spawn "chromium --app='https://mail.google.com'"),
  ((modm, xK_n), spawn "chromium --app='https://simple-note.appspot.com'"),
  ((modm, xK_g), spawn "chromium --app='https://app.nirvanahq.com'"),
  ((0, xK_Print), spawn "scrot"),
  ((modm, xK_v), spawn "VirtualBox"),
  ((0, xF86XK_AudioMute), spawn "amixer -q set PCM toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set PCM 2+"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set PCM 2-"),
  ((0, xF86XK_AudioPlay), spawn "exaile -t"),
  ((0, xF86XK_AudioStop), spawn "exaile -s"),
  ((0, xF86XK_AudioNext), spawn "exaile -n"),
  ((0, xF86XK_AudioPrev), spawn "exaile -p"),
  ((modm, xK_y), sendMessage ToggleStruts),
  ((modm, xK_comma), sendMessage MirrorShrink),
  ((modm, xK_p), sendMessage MirrorExpand),
  ((modm, xK_z), spawn "chromium --app='http://www.evernote.com/Home.action'"),
  ((modm, xK_s), goToSelected defaultGSConfig),
  ((modm, xK_o), spawn $ XMonad.terminal conf),
  ((modm, xK_a), windows W.swapMaster),
  ((modm, xK_apostrophe), sendMessage Shrink),
  ((modm, xK_y), sendMessage Expand),
  ((modm, xK_e), scratchpadSpawnActionTerminal "urxvtc"),
  ((modm, xK_Tab), sendMessage NextLayout),
  ((modm .|. controlMask, xK_period ), sendMessage (IncMasterN 1)),
  ((modm, xK_period), sendMessage (IncMasterN (-1)))
   ]
--}}}

---{{{ Dzen Config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = (wrapFg myHighlightedBgColor "|"),
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor . noScratchPad,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> "  " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of
                    "ResizableTall" -> wrapIcon "dzen_bitmaps/tall.xbm"
                    "Mirror ResizableTall" -> wrapIcon "dzen_bitmaps/mtall.xbm"
                    "Full" -> wrapIcon "dzen_bitmaps/full.xbm"
                ) . stripIM
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
    noScratchPad ws = if ws == "NSP" then "" else ws
--}}}

--{{{ GridSelect


