{- xmonad.hs
 - Author: �yvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
 - Version: 0.0.9
 - Modified version
 -}

-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import System.IO (Handle, hPutStrLn)
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.NoBorders

-- utils
import XMonad.Util.Run (spawnPipe)

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints

-------------------------------------------------------------------------------
-- Main --
main = do
       h <- spawnPipe "xmobar"
       xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keys'
              , logHook = logHook' h
              , layoutHook = layoutHook'
              , manageHook = manageHook'
              , handleEventHook = fullscreenEventHook
              , focusFollowsMouse  = myFocusFollowsMouse
              }

-------------------------------------------------------------------------------
-- Hooks --

manageHook' :: ManageHook
manageHook' = manageHook defaultConfig <+> manageDocks <+> composeOne
    [ isFullscreen              -?> doFullFloat
    , className =? "MPlayer"    -?> doIgnore
    ]

logHook' :: Handle ->  X ()
logHook' h = dynamicLogWithPP (customPP { ppOutput = hPutStrLn h })
             >> updatePointer (Relative 0 0)

layoutHook' = layoutHintsToCenter customLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = defaultPP { ppCurrent = xmobarColor "#FFEE00" "" . wrap "[" "]"
                     , ppVisible = xmobarColor "#5599FF" "" . wrap "<" ">"
                     , ppTitle =  shorten 40
                     , ppSep =  "<fc=#AFAF87>|</fc>"
                     , ppHiddenNoWindows = xmobarColor "#404040" ""
                     , ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"
                     }

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#444444"
focusedBorderColor' = "#cccccc"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1-M", "2-B", "3-C", "4-P", "5-S", "6-T", "7-V", "8-D", "9-E"]

-- layouts
customLayout = avoidStruts $ smartBorders tiled ||| noBorders Full
  where
    tiled = ResizableTall 1 (2/100) (1/2) []

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask

-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,               xK_g     ), withFocused $ toggleBorder)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- move focus between screens
    , ((modMask .|. controlMask, xK_j   ),  prevScreen)
    , ((modMask .|. controlMask, xK_k   ),  nextScreen)
    , ((modMask,                 xK_z   ),  toggleWS)
    , ((modMask,                 xK_o   ),  shiftNextScreen)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    --, ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)


    -- XF86AudioMute
    , ((0 , 0x1008ff12), spawn "amixer -q set Master toggle")
    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff11), spawn "amixer -q set Master 1- unmute")
    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff13), spawn "amixer -q set Master 1+ unmute")
    -- XF86AudioNext
    , ((0 , 0x1008ff17), spawn "mpc next")
    -- XF86AudioPrev
    , ((0 , 0x1008ff16), spawn "mpc prev")
    -- XF86AudioPlay
    , ((0 , 0x1008ff14), spawn "mpc toggle")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

