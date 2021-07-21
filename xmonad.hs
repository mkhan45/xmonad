import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Hooks.InsertPosition

import qualified Data.Map as M
import qualified XMonad.StackSet as W

centerRect = W.RationalRect 0.25 0.25 0.5 0.5

doIfFocusedIsFloating a b = withFocused $ \focusedId -> do
	floatingWindows <- gets (W.floating . windowset)
	let focusedIsFloating = focusedId `M.member` floatingWindows
	if focusedIsFloating then a else b

centerFloat window = windows $ W.float window centerRect

toggleFocusedFloat = doIfFocusedIsFloating (withFocused $ windows . W.sink) (withFocused centerFloat)

main :: IO ()
main = xmonad $ def
	{ modMask = mod4Mask 
	, manageHook = insertPosition End Newer 
	}
	`additionalKeysP`
	[ ("M-<Return>", spawn "st") 
	, ("M-f", spawn "firefox")
	, ("M-S-q", kill)
	, ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
	, ("M-S-e", spawn "killall xmonad-x86_64-linux")
	, ("M-<Space>", toggleFocusedFloat)
	]
