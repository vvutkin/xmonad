--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import XMonad
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import Data.Monoid ()
import System.Exit ()
import XMonad.Util.SpawnOnce ( spawnOnce )
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import XMonad.Hooks.EwmhDesktops ( ewmh )
import Control.Monad ( join, when, liftM2 )
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen, doCenterFloat, isDialog )
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps
    ( Direction2D(D, L, R, U),
      gaps,
      setGaps,
      GapMessage(DecGap, ToggleGaps, IncGap) )

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (maybeToList)

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.ResizableTile
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens

import XMonad.Layout.CenteredMaster(centerMaster)

import qualified Data.ByteString as B
import qualified DBus as D
import qualified DBus.Client as D

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--

ws1 = "1: \61612"
ws2 = "2: \61899"
ws3 = "3: \62150"
ws4 = "4: \61705"
ws5 = "5: \61564"
ws6 = "6: \61635"
ws7 = "7: \61947"
ws8 = "8: \61502"
ws9 = "9: \61501"
ws10 = "0: \61872"

myModMask       = mod4Mask
encodeCChar = map fromIntegral . B.unpack
myWorkspaces    = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws10]

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor = "#bc96da"

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

--     -- launch a terminal
--     [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

--     -- lock screen
--     , ((modm,               xK_F1    ), spawn "betterlockscreen -l")

--     -- launch rofi and dashboard
--     , ((modm,               xK_o     ), spawn "~/bin/launcher.sh")
--     , ((modm,               xK_p     ), spawn "~/bin/centerlaunch")
--     , ((modm .|. shiftMask, xK_p     ), spawn "exec ~/bin/ewwclose")

--     -- launch eww sidebar
--     , ((modm,               xK_s     ), spawn "~/bin/sidebarlaunch")
--     , ((modm .|. shiftMask, xK_s     ), spawn "exec ~/bin/ewwclose")

--     -- Audio keys
--     , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
--     , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
--     , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
--     , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
--     , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
--     , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

--     -- Brightness keys
--     , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
--     , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
 
--     -- Screenshot
--     , ((0,                    xK_Print), spawn "~/bin/maimcopy")
--     , ((modm,                 xK_Print), spawn "~/bin/maimsave")

--     -- My Stuff
--     , ((modm,               xK_b     ), spawn "exec ~/bin/bartoggle")
--     , ((modm,               xK_z     ), spawn "exec ~/bin/inhibit_activate")
--     , ((modm .|. shiftMask, xK_z     ), spawn "exec ~/bin/inhibit_deactivate")
--     , ((modm .|. shiftMask, xK_a     ), spawn "exec ~/bin/clipboardy")

--     -- close focused window
--     , ((modm .|. shiftMask, xK_c     ), kill)

--     -- GAPS!!!
--     , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
--     , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,40), (D,60)]) -- reset the GapSpec
    
--     , ((modm .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
--     , ((modm .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    
--     , ((modm .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
--     , ((modm .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)           -- decrement the top gap
    
--     , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
--     , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

--     , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
--     , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

--      -- Rotate through the available layout algorithms
--     , ((modm,               xK_space ), sendMessage NextLayout)

--     --  Reset the layouts on the current workspace to default
--     , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

--     -- Resize viewed windows to the correct size
--     , ((modm,               xK_n     ), refresh)

--     -- Move focus to the next window
--     , ((modm,               xK_Tab   ), windows W.focusDown)

--     -- Move focus to the next window
--     , ((modm,               xK_j     ), windows W.focusDown)

--     -- Move focus to the previous window
--     , ((modm,               xK_k     ), windows W.focusUp  )

--     -- Move focus to the master window
--     , ((modm,               xK_m     ), windows W.focusMaster  )

--     -- Swap the focused window and the master window
--     , ((modm,               xK_Return), windows W.swapMaster)

--     -- Swap the focused window with the next window
--     , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

--     -- Swap the focused window with the previous window
--     , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

--     -- Shrink the master area
--     , ((modm,               xK_h     ), sendMessage Shrink)

--     -- Expand the master area
--     , ((modm,               xK_l     ), sendMessage Expand)

--     -- Push window back into tiling
--     , ((modm,               xK_t     ), withFocused $ windows . W.sink)

--     -- Increment the number of windows in the master area
--     , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

--     -- Deincrement the number of windows in the master area
--     , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

--     -- Toggle the status bar gap
--     -- Use this binding with avoidStruts from Hooks.ManageDocks.
--     -- See also the statusBar function from Hooks.DynamicLog.
--     --
--     -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

--     -- Quit xmonad
--     , ((modm .|. shiftMask, xK_q     ), spawn "~/bin/powermenu.sh")

--     -- Restart xmonad
--     , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

--     -- Run xmessage with a summary of the default keybindings (useful for beginners)
--     , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
--     ]
--     ++

--     --
--     -- mod-[1..9], Switch to workspace N
--     -- mod-shift-[1..9], Move client to workspace N
--     --
--     [((m .|. modm, k), windows $ f i)
--         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--     ++

--     --
--     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--     --
--     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [ ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask, xK_h), spawn $ "urxvt 'htop task manager' -e htop" )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_r), spawn $ "rofi-theme-selector" )
  , ((modMask, xK_t), spawn $ "telegram-desktop" )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_x), spawn $ "oblogout" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ "urxvt" )
  , ((modMask, xK_e), spawn $ "thunar" )
  , ((modMask, xK_z ), spawn $ "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  , ((modMask .|. shiftMask , xK_w ), spawn $ "google-chrome-stable")
  , ((modMask .|. shiftMask , xK_s ), spawn $ "code")
  , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
  , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
  , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_c ), spawn $ "catfish")
  , ((controlMask .|. mod1Mask , xK_l ), spawn $ "slimlock")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
--   , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "spotify")
  , ((controlMask .|. mod1Mask, xK_KP_Home), withFocused $ windows . (flip W.float $ W.RationalRect (0) (0) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Up), withFocused $ windows . (flip W.float $ W.RationalRect (1/3) (0) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Page_Up), withFocused $ windows . (flip W.float $ W.RationalRect (2/3) (0) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Left), withFocused $ windows . (flip W.float $ W.RationalRect (0) (1/3) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Right), withFocused $ windows . (flip W.float $ W.RationalRect (2/3) (1/3) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_End), withFocused $ windows . (flip W.float $ W.RationalRect (0) (2/3) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Down), withFocused $ windows . (flip W.float $ W.RationalRect (1/3) (2/3) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Page_Down), withFocused $ windows . (flip W.float $ W.RationalRect (2/3) (2/3) (1/3) (1/3)))
  , ((controlMask .|. mod1Mask, xK_KP_Begin), withFocused $ windows . W.sink)
  , ((controlMask .|. mod1Mask, xK_KP_Insert), toggleCopyToAll) 

  -- ALT + ... KEYS

  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )
  , ((mod1Mask, xK_F2), spawn $ "gmrun" )
  , ((mod1Mask, xK_F3), spawn $ "xfce4-appfinder" )

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ "xfce4-taskmanager")

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")

  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl --player=spotify play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl --player=spotify next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl --player=spotify previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl --player=spotify stop")

  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]

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

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                            [] -> windows copyToAll
                            _ -> killAllOtherCopies

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
myLayout = avoidStruts (mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ ThreeColMid 1 (3/100) (1/2) ||| spiral (6/7) ||| noBorders Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ ThreeColMid 1 (3/100) (1/2) ||| spiral (6/7) ||| noBorders Full
--     where
--         tiled = Tall nmaster delta tiled_ratio
--         nmaster = 1
--         delta = 3/100
--         tiled_ratio = 1/2

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
--     [ className =? "MPlayer"        --> doFloat
--     , className =? "Gimp"           --> doFloat
--     , resource  =? "desktop_window" --> doIgnore
--     , resource  =? "kdesktop"       --> doIgnore
--     , isFullscreen --> doFullFloat
--                                  ]
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll $ concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws1 | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws3 | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws4 | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo ws5 | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo ws6 | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift ws9 | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Oblogout", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]
    my1Shifts = ["google-chrome", "Firefox"]
    my3Shifts = ["telegram-desktop", "discord"]
    my4Shifts = ["Virtualbox", "steam", "Steam"]
    my5Shifts = ["sun-awt-X11-XFramePeer"]
    my6Shifts = ["kodi", "deadcells", "dota2"]
    my9Shifts = ["spotify", "pulseeffects", "Pulseeffects"]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawnOnce "exec ~/bin/bartoggle"
  spawnOnce "exec ~/bin/eww daemon"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "exec ~/bin/lock.sh"
  spawnOnce "feh --bg-scale ~/wallpapers/yosemite-lowpoly.jpg"
  spawnOnce "picom -f"
  spawnOnce "greenclip daemon"
  spawnOnce "dunst"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook, 
        layoutHook = gaps [(L,30), (R,30), (U,40), (D,60)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
