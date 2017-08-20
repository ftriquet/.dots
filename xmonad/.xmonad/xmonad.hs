import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import System.IO
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse= True

myClickJustFocuses :: Bool
myClickJustFocuses = False


black = "#181818"
red = "#ab4642"
green = "#a1b56c"
blue = "#7cafc2"
white = "#d8d8d8"

myNormalBorderColor  = black
myBorderWidth   = 3
myFocusedBorderColor = red

myModMask       = mod1Mask

myWorkspaces :: [String]
myWorkspaces = map clickable [ ("bracketleft", "7")
                             , ("braceleft", "5")
                             , ("braceright", "3")
                             , ("parenleft", "1")
                             , ("equal", "9")
                             , ("asterisk", "0")
                             , ("parenright", "2")
                             , ("plus", "4")
                             , ("bracketright", "6")
                             ]
    where clickable (key, ws) = "<action=`xdotool key alt+" ++ key ++ "`>" ++ ws ++ "</action>"

myNumRow = [ xK_bracketleft
           , xK_braceleft
           , xK_braceright
           , xK_parenleft
           , xK_equal
           , xK_asterisk
           , xK_parenright
           , xK_plus
           , xK_bracketright
           ]


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask,  xK_Return), spawn $ XMonad.terminal conf)

    -- launch file browser
    , ((modm .|. shiftMask, xK_f     ), spawn "pcmanfm")

    -- launch rofi
    , ((modm,               xK_p     ), spawn "rofi -show run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- lock screen
    , ((modm .|. controlMask, xK_l), safeSpawn "lock.sh" [])

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_t     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_n     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_t     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_n     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_g     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Shrink current window height
    , ((modm,               xK_Down  ), sendMessage MirrorShrink)

    -- Expand current window height
    , ((modm,               xK_Up    ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((controlMask .|. shiftMask,   xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm              , xK_f      ), namedScratchpadAction myScratchpads "music")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myNumRow
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_h, xK_s, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
  hs <- gets $ map W.tag . W.hidden . windowset
  return (\w -> (W.tag w) /= "NSP" && (W.tag w) `elem` hs)

addKeys = [ ("<XF86AudioLowerVolume>"        ,spawn "amixer set Master 5%-")
          , ("<XF86AudioRaiseVolume>"        ,spawn "amixer set Master 5%+"  )
          , ("<XF86AudioMute>"               ,spawn "amixer -q sset Master,0 toggle"   )
          , ("<XF86MonBrightnessDown>"       ,spawn "xbacklight -dec 10"            )
          , ("<XF86MonBrightnessUp>"         ,spawn "xbacklight -inc 10"            )
          , ("<XF86AudioPlay>"               ,spawn "playerctl play-pause"     )
          , ("<XF86AudioPrev>"               ,spawn "playerctl previous"     )
          , ("<XF86AudioNext>"               ,spawn "playerctl next"     )
          ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Layouts:

myTabConf = def { activeColor = "#458588"
                , inactiveColor = "#1D2021"
                , fontName = "xft:Iosevka:size=10"
                }

withGaps layout = spacing 10 $ gaps [(U, 5), (R, 5), (L, 5), (D, 5)] $ layout

myLayout = avoidStruts $ smartBorders (tall ||| grid ||| Full ||| tabbed shrinkText myTabConf)
    where
     nmaster = 1
     masterRatio = 1/2
     delta = 3/100
     gridRatio = (4/3)
     tall = withGaps (ResizableTall nmaster delta masterRatio [])
     grid = withGaps (GridRatio gridRatio)

------------------------------------------------------------------------
-- Window rules:

myScratchpads = [ NS "music" spawnTerm findTerm manageTerm ]
    where
        h           = 0.6
        w           = 0.6
        t           = (1 - h) / 2
        l           = (1 - w) / 2
        spawnTerm   = "termite -t scratchpad-term"
        findTerm   = title =? "scratchpad-term"
        manageTerm = customFloating $ W.RationalRect l t w h

manageScratchpad :: ManageHook
manageScratchpad = namedScratchpadManageHook myScratchpads

myManageHook = composeAll
    [ className =? "Pcmanfm"                      --> doFloat
    , className =? "Nemo"                         --> doFloat
    , resource  =? "desktop_window"               --> doIgnore
    , className =? "Xmessage"                     --> doFloat
    , className =? "Gimp"                         --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
    ]
    <+> (isFullscreen --> doFullFloat)
    <+> manageScratchpad
    <+> manageDocks

------------------------------------------------------------------------
-- Event handling

myEventHook = docksEventHook <+> fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppCurrent = xmobarColor "#fb4934" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#c0c5ce" ""
                     , ppHiddenNoWindows = xmobarColor "#4f5b66" ""
                     , ppUrgent = xmobarColor black "#fb4934"
                     , ppLayout = xmobarColor white "" . wrap "[" "]"
                     , ppTitle =  xmobarColor white "" . shorten 80
                     , ppSep = xmobarColor white "" "  "
                     }


------------------------------------------------------------------------
-- Startup hook

myStartupHook = do
    setWMName "LG3D"
    spawn "start-trayer.sh"
    spawn "feh --bg-scale /home/francois/Pictures/RaM-Arc-En-Ciel.jpg"

------------------------------------------------------------------------
myConfig xmproc = (defaults xmproc) `additionalKeysP` addKeys

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ ewmh $ myConfig xmproc

defaults xmproc = def { terminal           = myTerminal
                      , focusFollowsMouse  = myFocusFollowsMouse
                      , clickJustFocuses   = myClickJustFocuses
                      , borderWidth        = myBorderWidth
                      , modMask            = myModMask
                      , workspaces         = myWorkspaces
                      , normalBorderColor  = myNormalBorderColor
                      , focusedBorderColor = myFocusedBorderColor
                      , keys               = myKeys
                      , mouseBindings      = myMouseBindings
                      , layoutHook         = myLayout
                      , manageHook         = myManageHook
                      , handleEventHook    = myEventHook
                      , logHook            = myLogHook xmproc
                      , startupHook        = myStartupHook
                      }
