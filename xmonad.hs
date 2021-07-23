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

-- needed for xmobar
barPrettyPrinter :: PP
barPrettyPrinter = 
	xmobarPP { ppCurrent = xmobarColor "#ffffff" "" . wrap "[" "]"
                 , ppTitle = xmobarColor "#ffffff" "" . shorten 60
                 , ppVisible = wrap "(" ")"
                 , ppUrgent  = xmobarColor "red" "yellow"
                 , ppSort = getSortByXineramaPhysicalRule def
		 , ppOrder = \(workspaces:layout:title) -> workspaces : title
                 }

main :: IO ()
main = do 
	barproc <- spawnPipe "xmobar"
	xmonad $ docks $ ewmh $ def
		{ modMask = mod4Mask 
		, manageHook = insertPosition End Newer 
		, layoutHook = myLayout
		, logHook = dynamicLogWithPP barPrettyPrinter { ppOutput = hPutStrLn barproc }
		}
		`additionalKeysP`
		[ ("M-<Return>", spawn "alacritty") 
		, ("M-f", spawn "~/projects/rofi_scripts/browser_launch.dash")
		, ("M-a", spawn "~/projects/rofi_scripts/actions.dash")
		, ("M-d", spawn "rofi -show drun")
		, ("M-S-q", kill)
		, ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
		, ("M-S-e", spawn "kill -9 -1")
		, ("M-<Space>", toggleFocusedFloat)
		, ("M-S-f", sendMessage NextLayout)
		]
