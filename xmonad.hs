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

import XMonad.Util.EZConfig
import XMonad.Util.Run

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
      , layoutHook = avoidStruts $ myLayoutHook
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = mod4Mask
      , keys = myKeys
      , workspaces = myWorkspaces
     }   
 
--{{{ Theme 

--Font
myFont = "Terminus-6"
 
-- Colors

--- Main Colours
myFgColor = "#DCDCCC"
myBgColor = "#3f3f3f"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#7F9F7F"

--- Borders
myActiveBorderColor = myCurrentWsBgColor
myInactiveBorderColor = "#262626"
myBorderWidth = 3

--- Ws Stuff
myCurrentWsFgColor = myHighlightedFgColor
myCurrentWsBgColor = myHighlightedBgColor
myVisibleWsFgColor = myBgColor
myVisibleWsBgColor = "#CCDC90"
myHiddenWsFgColor = myHighlightedFgColor
myHiddenEmptyWsFgColor = "#8F8F8F"
myUrgentWsBgColor = "#DCA3A3"
myTitleFgColor = myFgColor


--- Urgency
myUrgencyHintFgColor = "red"
myUrgencyHintBgColor = "blue"
 
-- }}}

-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -h '15'" ++ " -e 'onstart=lower'"
 
-- Status Bar
myStatusBar = "dzen2 -w 750 -ta l " ++ myDzenGenOpts
 
-- Conky Bar
myConkyBar = "conky -c ~/.conky_bar | dzen2 -x 750 -w 450 " ++ myDzenGenOpts
 
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
      " 5 ",
      " 6 ",
      " 7 ",
      " 8 jw ",
      " . "
   ]
 
-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "576", "-h", "15", "-w", "1024",
         "-ta", "r",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ ""
         ]
    }
 
--{{{ Hook for managing windows
myManageHook = composeAll
   [ resource  =? "Do"               --> doIgnore,              -- Ignore GnomeDo
     className =? "Pidgin"           --> doShift " 4 im ",      -- Shift Pidgin to im desktop 
     className =? "Chrome"           --> doShift " 3 www ",     -- Shift Chromium to www
     className =? "Firefox"          --> doShift " 3 www ",     -- Shift Firefox to www
     className =? "Emacs"            --> doShift " 2 ed ",   -- Shift emacs to emacs
     className =? "Wicd-client.py"   --> doFloat                -- Float Wicd window 
   ]
--}}}
 
-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
 
--{{{ Keybindings 
--    Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  ((modm, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'"),  --Uses a colourscheme with dmenu
  ((modm, xK_b), spawn "firefox"),
  ((modm, xK_s), spawn "chromium manage.sugarstats.com/stats/today"),
  ((modm, xK_c), spawn "chromium"),
  ((modm, xK_f), spawn "urxvt -e mc"),
  ((modm, xK_m), spawn "urxvt -e alsamixer"),
  ((0, xK_Print), spawn "scrot"),
  ((modm, xK_v), spawn "VirtualBox"),
  ((modm, xK_g), goToSelected myGSConfig),
  ((0, xF86XK_AudioMute), spawn "amixer -q set PCM toggle"),
  ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set PCM 2+"),
  ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set PCM 2-"),
  ((0, xF86XK_AudioPlay), spawn "exaile -t"),
  ((0, xF86XK_AudioStop), spawn "exaile -s"),
  ((0, xF86XK_AudioNext), spawn "exaile -n"),
  ((0, xF86XK_AudioPrev), spawn "exaile -p"),
  ((modm, xK_y), sendMessage ToggleStruts)
   ]
--}}}

---{{{ Dzen Config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = (wrapFg myHighlightedBgColor "|"),
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
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
--}}}

--{{{ GridSelect
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_colorizer = ""
    , gs_font = "" ++ myFont ++ ""
    --, gs_navigate = ""
    --, gs_originFractX = ""
    --, gs_originFractY = ""
    }
--}}}    
