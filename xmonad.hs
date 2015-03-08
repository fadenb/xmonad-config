import XMonad hiding ( (|||), Tall )
import XMonad.Operations

import Graphics.X11.Xlib

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named

import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Layout
import XMonad.Prompt.Man

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys

import XMonad.Hooks.EwmhDesktops

import XMonad.Util.Run
import System.IO

main = do
    xmobar <- spawnPipe ( "xmobar" )
    xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "green"] }
    	   $ bsxConfig xmobar

bsxConfig h = defaultConfig
       { borderWidth        = 1 
       , terminal           = "urxvt"
       , workspaces         = ["1:shells", "2:web", "3:mail", "4:com", "5:foo"]
                              ++ map show [6 .. 8 :: Int]
                              ++ ["9:music"]
       , modMask            = mod4Mask
       , handleEventHook    = fullscreenEventHook
       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
       , logHook            = dynamicLogWithPP $ bsxPP h
       , keys = \c -> mykeys c `M.union` keys defaultConfig c
       , manageHook         = manageHook defaultConfig <+> myManageHook <+> manageDocks <+> composeOne [isFullscreen -?> doFullFloat ]
       , startupHook        = setWMName "LG3D"
       --, layoutHook         = avoidStrutsOn [D] (windowNavigation (smartBorders (named "hsplit" (hintedTile Tall) ||| named "vsplit" (hintedTile Wide))))
       , layoutHook         = avoidStrutsOn [D] (windowNavigation (smartBorders (onWorkspace "4:com" (IM (1%6) (Role "buddy_list")) (named "hsplit" (hintedTile Tall) ||| named "vsplit" (hintedTile Wide) ||| named "tabbed" (tabbed shrinkText defaultTheme)))))

       }   
  where
    hintedTile = HintedTile nmaster delta ratio TopLeft
    nmaster    = 1 
    ratio      = 1/2 
    delta      = 3/100
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [   
      -- fullscreen fake by not displaying xmobar
      -- toggle the status bar gap
      ((modm,               xK_b     ), sendMessage ToggleStruts)

      -- rotate workspaces
      , ((modm .|. controlMask, xK_Right), nextWS)
      , ((modm .|. controlMask, xK_Left), prevWS)

      -- switch to previous workspace
      , ((modm, xK_z), toggleWS)


      -- set random wallpaper with feh from ~/documents/wallpaper/
      , ((modm .|. shiftMask, xK_w), spawn "feh --randomize --bg-fill ~/documents/wallpaper")

      -- lock the screen with xscreensaver
      , ((modm .|. shiftMask, xK_l), spawn "xset +dpms && scrot /tmp/screen_locked.png && convert /tmp/screen_locked.png -blur 0x3 /tmp/screen_locked2.png && i3lock -i /tmp/screen_locked2.png")

      -- some programs to start with keybindings.
      , ((modm .|. shiftMask, xK_f), spawn "firefox")
      , ((modm .|. shiftMask, xK_i), spawn "xcalib -i -a")
      , ((modm .|. shiftMask, xK_m), spawn "claws-mail")
      , ((modm .|. shiftMask, xK_p), spawn "pidgin")
      , ((modm .|. shiftMask, xK_b), spawn "chromium-browser")
      , ((modm .|. shiftMask, xK_v), spawn "pgrep openvpn; if [ $? -eq 0 ]; then sudo /etc/init.d/openvpn stop; else sudo /etc/init.d/openvpn start; fi")
      
      -- , ((modm .|. shiftMask, xK_k), spawn "kaffeine")

      -- prompts
      , ((modm .|. controlMask, xK_s), sshPrompt defaultXPConfig)
      , ((modm .|. controlMask, xK_l), layoutPrompt defaultXPConfig)
      , ((modm .|. controlMask, xK_m), manPrompt defaultXPConfig)

      -- window navigation keybindings.
      , ((modm,               xK_Right), sendMessage $ Go R)
      , ((modm,               xK_Left ), sendMessage $ Go L)
      , ((modm,               xK_Up   ), sendMessage $ Go U)
      , ((modm,               xK_Down ), sendMessage $ Go D)
      , ((modm .|. shiftMask, xK_Right), sendMessage $ Swap R)
      , ((modm .|. shiftMask, xK_Left ), sendMessage $ Swap L)
      , ((modm .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
      , ((modm .|. shiftMask, xK_Down ), sendMessage $ Swap D)
      -- da ich xmobar nicht wegschalten will    , ((modm .|. shiftMask, xK_b    ), sendMessage ToggleStruts)

      , ((0            , 0x1008ff03), spawn "xbacklight -10")
      , ((0            , 0x1008ff02), spawn "xbacklight +10")

      -- XF86AudioLowerVolume
      , ((0            , 0x1008ff11), spawn "amixer set Master 2%-")
	  -- XF86AudioRaiseVolume
      , ((0            , 0x1008ff13), spawn "amixer set Master unmute;amixer set Master 2%+")
     
	  -- XF86AudioMute
      , ((0            , 0x1008ff12), spawn "amixer set Master toggle")
      ]

myManageHook = composeAll
    [ className =? "Claws-mail" --> doF(W.shift "3:mail")
    , className =? "Firefox" --> doF(W.shift "2:web")
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "Pidgin" --> doF(W.shift "4:com")
    , className =? "Google-chrome" --> doF(W.shift "2:web")
    , className =? "Choqok" --> doF(W.shift "4:com")
    -- , className =? "Kaffeine" --> doF(W.shift "6:video")
    ]

bsxPP h = defaultPP { ppCurrent         = wrap "<fc=#ff0000>" "</fc>"
                    , ppVisible         = wrap "<fc=#00ff00>" "</fc>"
                    , ppHiddenNoWindows = \wsId -> if (':' `elem` wsId) then wsId ++ " " else ""
                    , ppSep             = " | "
                    , ppTitle           = shorten 80
                    , ppOrder           = reverse
		    , ppUrgent      	= xmobarColor "#fff" "" . \wsId -> wsId ++ "*"
		    , ppOutput          = hPutStrLn h }
