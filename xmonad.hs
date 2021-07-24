import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Hooks.InsertPosition

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout
import XMonad.Layout.NoBorders (smartBorders)

import Data.List
import Data.Maybe

import XMonad.Util.NamedScratchpad

centerRect = W.RationalRect 0.25 0.25 0.5 0.5

doIfFocusedIsFloating a b = withFocused $ \focusedId -> do
	floatingWindows <- gets (W.floating . windowset)
	let focusedIsFloating = focusedId `M.member` floatingWindows
	if focusedIsFloating then a else b

centerFloat window = windows $ W.float window centerRect

toggleFocusedFloat = doIfFocusedIsFloating (withFocused $ windows . W.sink) (withFocused centerFloat)

myLayout = avoidStruts (smartBorders $ 
        Tall 1 (3/100) (1/2)
    ||| Full
    )

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
    where scWidth = 0.6
          scHeight = 0.5
          scTop = (1.0 - scHeight) / 2.0
          scLeft = (1.0 - scWidth) / 2.0

scratchpadFloat = customFloating scratchpadRect

scratchpads = [ NS "term" "alacritty --title scratchpad" (title =? "scratchpad") scratchpadFloat
              , NS "julia" "alacritty --title julia -e 'julia'" (title =? "julia") scratchpadFloat
              ]

main :: IO ()
main = do 
	barproc <- spawnPipe "xmobar"
	xmonad $ docks $ ewmh $ def
		{ modMask = mod4Mask 
		, manageHook = (insertPosition End Newer) <+> (namedScratchpadManageHook scratchpads)
		, layoutHook = myLayout
		, logHook = dynamicLogWithPP barPrettyPrinter { ppOutput = hPutStrLn barproc }
                , handleEventHook = handleEventHook def <+> fullscreenEventHook
                , terminal = "alacritty"
		}
		`additionalKeysP`
		[ ("M-<Return>", spawn "alacritty") 
		, ("M-f", spawn "~/projects/rofi_scripts/browser_launch.dash")
		, ("M-S-f", spawn "firefox --new-window")
		, ("M-a", spawn "~/projects/rofi_scripts/actions.dash")
		, ("M-d", spawn "rofi -show drun")
		, ("M-S-q", kill)
		, ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
		, ("M-S-e", spawn "kill -9 -1")
		, ("M-<Space>", toggleFocusedFloat)
		, ("M-S-<Space>", sendMessage NextLayout)
		, ("M-S-a", spawn "i3lock -i ~/Pictures/rocket.png")
		, ("M-s", namedScratchpadAction scratchpads "term")
		, ("M-c", namedScratchpadAction scratchpads "julia")
		, ("<XF86MonBrightnessUp>", spawn "brightnessctl s +5%")
		, ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
		]
