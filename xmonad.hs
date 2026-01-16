import XMonad

import XMonad.Util.EZConfig

import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Hooks.InsertPosition

import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerScreen (ifWider)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.UpdateFocus
import qualified XMonad.Actions.CycleWS as C

import XMonad.Layout
import XMonad.Layout.NoBorders (smartBorders, lessBorders, Ambiguity(..))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutModifier

import Data.List
import Data.Maybe

import XMonad.Util.NamedScratchpad

import GHC.IO.Handle (hGetLine)
import Control.Monad.IO.Class (liftIO)

import System.IO

-- new xmobar
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- old xmobar
import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.DynamicLog

centerRect = W.RationalRect 0.25 0.25 0.5 0.5

doIfFocusedIsFloating a b = withFocused $ \focusedId -> do
	floatingWindows <- gets (W.floating . windowset)
	let focusedIsFloating = focusedId `M.member` floatingWindows
	if focusedIsFloating then a else b

centerFloat window = windows $ W.float window centerRect

toggleFocusedFloat = doIfFocusedIsFloating (withFocused $ windows . W.sink) (withFocused centerFloat)

tallLayout = ifWider 1440 (ResizableTall 1 (3/100) (1/2) []) (Mirror $ ResizableTall 1 (3/100) (1/2) [])

myLayout = 
    avoidStruts (lessBorders Screen $ 
            (addTabs shrinkText def tallLayout) ||| (addTabs shrinkText def Full))

-- sorts the workspaces and adds [] around the currently selected one
processWorkspaces :: String -> String
processWorkspaces workspaces =
    concat $ intersperse " " $ map processWorkspace allWorkspaces
        where currentWorkspace = read (take 1 $ drop 13 workspaces) :: Int
              otherWorkspaces 
                = map read $ filter (/="NSP") $ words $ takeWhile (/='>') $ drop 21 workspaces :: [Int]
              allWorkspaces = sort $ currentWorkspace : otherWorkspaces
              formatCurrent = (xmobarColor "#FFFFFF" "#000000" . wrap "[" "]")
              processWorkspace w = if w == currentWorkspace 
                                       then formatCurrent (show w) 
                                       else (show w)

-- needed for xmobar
barPrettyPrinter :: PP
barPrettyPrinter = 
	xmobarPP { ppCurrent = xmobarColor "#ffffff" "" . wrap "[" "]"
                 , ppTitle = xmobarColor "#ffffff" "" . shorten 60
                 , ppVisible = wrap "(" ")"
                 , ppUrgent  = xmobarColor "red" "yellow"
                 , ppSort = getSortByXineramaPhysicalRule def
		 , ppOrder = 
                     \(workspaces:layout:title) -> (wrap " " "" $ processWorkspaces workspaces) : title
                 }

scratchpadRect = W.RationalRect scLeft scTop scWidth scHeight
    where scWidth = 0.75
          scHeight = 0.75
          scTop = (1.0 - scHeight) / 2.0
          scLeft = (1.0 - scWidth) / 2.0

scratchpadFloat = customFloating scratchpadRect

pulsemixerScratchpad = 
    NS "pulsemixer" "alacritty --title pulsemixer -e 'pulsemixer'" (title =? "pulsemixer") scratchpadFloat


scratchpads = [ NS "term" "alacritty --title scratchpad" (title =? "scratchpad") scratchpadFloat
              , NS "julia" "alacritty --title julia -e 'julia'" (title =? "julia") scratchpadFloat
              , NS "cmus" "alacritty --title cmus -e 'cmus'" (title =? "cmus") scratchpadFloat
              , NS "term1" "alacritty --title misc1" (title =? "misc1") scratchpadFloat
              , NS "term2" "alacritty --title misc2" (title =? "misc2") scratchpadFloat
              , NS "term3" "alacritty --title misc3" (title =? "misc3") scratchpadFloat
              , pulsemixerScratchpad
              ]

scratchpadLauncher :: X ()
scratchpadLauncher = do
    result <- runProcessWithInput "rofi" ["-dmenu"] (intercalate "\n" options)
    namedScratchpadAction scratchpads (take (length result - 1) result)
        where options = ["julia", "cmus", "pulsemixer", "term1"]

myAppendFile :: FilePath -> String -> IO ()
myAppendFile f s = do
  withFile f AppendMode $ \h -> do
    hPutStrLn h s

logToTmpFile :: String -> IO ()
logToTmpFile = myAppendFile "/home/mk/xmonad.log" . (++ "\n")

screenWorkspaces = withScreens 1 $ map show [1..9]

xmobar1 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.config/xmonad/xmobarrc" (pure (marshallPP (S 0) barPrettyPrinter))

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = let modm = modMask conf in M.fromList $
    [((m .|. modm, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

main :: IO ()
main = do 
	xmonad $ docks $ withSB xmobar1 $ ewmhFullscreen $ ewmh $ def
		{ modMask = mod4Mask 
                , startupHook = adjustEventInput
		, manageHook = 
                    (insertPosition End Newer) 
                    <+> (namedScratchpadManageHook scratchpads) 
                    <+> (fmap ("mpv" `isPrefixOf`) title --> doFullFloat)
		, layoutHook = myLayout
		-- , logHook = dynamicLogWithPP barPrettyPrinter { ppOutput = hPutStrLn barproc }
                , handleEventHook = handleEventHook def <+> focusOnMouseMove
                , terminal = "alacritty"
                , workspaces = screenWorkspaces
                , keys = myKeys
		}
		`additionalKeysP`
		[ ("M-<Return>", spawn "alacritty") 
		, ("M-f", spawn "~/repos/rofi_scripts/browser_launch.dash")
		, ("M-S-f", spawn "firefox --new-window")
		, ("M-C-f", spawn "firefox --new-window")
		, ("M-a", spawn "~/repos/rofi_scripts/actions.dash")
		, ("M-d", spawn "rofi -show drun")
		, ("M-S-q", kill)
		, ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
		, ("M-S-e", spawn "pkill xmonad")
		, ("M-<Space>", toggleFocusedFloat)
		, ("M-S-<Space>", sendMessage NextLayout)
		, ("M-S-a", spawn "i3lock -i ~/Pictures/rocket.png")
		, ("M-s", namedScratchpadAction scratchpads "term")
		, ("M-c", namedScratchpadAction scratchpads "julia")
		, ("M-m", namedScratchpadAction scratchpads "cmus")
		, ("M-g", scratchpadLauncher)
		, ("<XF86MonBrightnessUp>", spawn "brightnessctl s +5%")
		, ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
		, ("<XF86AudioLowerVolume>", spawn "/home/mk/repos/rofi_scripts/volumedown.dash")
		, ("<XF86AudioRaiseVolume>", spawn "/home/mk/repos/rofi_scripts/volumeup.dash")
		, ("<XF86AudioMute>", spawn "amixer -D pulse sset 0%")
		-- , ("M-p", namedScratchpadAction scratchpads "term1")
		, ("M-[", namedScratchpadAction scratchpads "term1")
		, ("M-]", namedScratchpadAction scratchpads "term2")
		, ("M-\\", namedScratchpadAction scratchpads "term3")
                , ("M-j", windows $ W.focusUp)
                , ("M-k", windows $ W.focusDown)
                , ("M-S-j", windows $ W.swapUp)
                , ("M-S-k", windows $ W.swapDown)
                , ("M-h", sendMessage Shrink)
                , ("M-l", sendMessage Expand)
                , ("M-S-h", sendMessage MirrorShrink)
                , ("M-S-l", sendMessage MirrorExpand)
                , ("M-S-k", windows $ W.swapDown)
                , ("M-n", C.nextScreen)
                , ("M-S-n", C.shiftNextScreen)
                , ("M-p", C.nextScreen)
		, ("M-S-p", C.shiftNextScreen)
		]
