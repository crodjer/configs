{- xmonad.hs
 - Author: Rohan Jain <crodjer AT gmail DOT com>
 -}

-- Borrowed from the xmonad.hs by �yvind 'Mr.Elendig' Heggstad <mrelendig AT
-- har-ikkje DOT net>

-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import System.IO (Handle, hPutStrLn)
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import XMonad.Actions.UpdatePointer

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ShowWName

-------------------------------------------------------------------------------
-- Main --
main :: IO()
main = do
  h <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ def
             { workspaces = workspaces'
             , modMask = modMask'
             , borderWidth = borderWidth'
             , normalBorderColor = normalBorderColor'
             , focusedBorderColor = focusedBorderColor'
             , terminal = terminal'
             , keys = keys'
             , logHook = logHook' h
             , layoutHook = layoutHook'
             , manageHook = manageHook' <+> manageHook def
             , handleEventHook = fullscreenEventHook
             , focusFollowsMouse  = myFocusFollowsMouse
             }

-------------------------------------------------------------------------------
-- Hooks --

manageHook' :: ManageHook
manageHook' = composeAll
              [ isDialog                      --> doFloat
              , className     =? "Xmessage"   --> doFloat
              , className     =? "MPlayer"    --> ask >>= doF . W.sink
              , className     =? "MPlayer"    --> doShift "9"
              , manageDocks
              ]

logHook' :: Handle ->  X ()
logHook' h = dynamicLogWithPP (customPP { ppOutput = hPutStrLn h })
             >> (updatePointer (0.5, 0.25) (0, 0))

layoutHook' = customLayout
-- Top-level binding with no type signature:           layoutHook' :: XMonad.Layout.LayoutModifier.ModifiedLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP :: PP
customPP = def
           { ppCurrent = xmobarColor "#FFEE00" "" . wrap "[" "]"
           , ppVisible = xmobarColor "#5599FF" "" . wrap "<" ">"
           , ppTitle =  shorten 80
           , ppSep =  "|"
           , ppHiddenNoWindows = xmobarColor "#999999" ""
           , ppUrgent = xmobarColor "#ff0000" "" . wrap "!" "!"
           }

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- borders
borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#CCCCCC"
focusedBorderColor' = "#00FF00"

myFont = "xft:Monospace:pixelsize=12:bold:antialias=true:hinting=true"
myFontLarge = "xft:Monospace:pixelsize=30:bold:antialias=true:hinting=true"

-- workspaces
workspaces' :: [WorkspaceId]
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-- layouts
customLayout = myShowWName $
                 onWorkspaces ["4", "5", "6"] work $
                 onWorkspaces ["8", "9"] normal $
                 onWorkspaces ["2"] full
                 normal

    where
      normal = avoidStruts $ myTiled ||| myFull ||| myTabbed
      work = avoidStruts $ myTiled ||| myFull
      full = avoidStrutsOn [] $ myTabbed ||| myFull ||| myTiled

myTiled = smartBorders $ ResizableTall 1 (3/100) (51/100) []
myFull = noBorders $ Full
myTabbed = noBorders $ tabbed shrinkText def
mySWNConfig = def
              { swn_font = myFontLarge
              , swn_fade = 1
              , swn_bgcolor = "#000000"
              , swn_color = "#FFFFFF"
              }
myShowWName = showWName' mySWNConfig

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvtc"

myXPConfig = def
             { promptKeymap = emacsLikeXPKeymap
             , position = Top
             , promptBorderWidth = 1
             , borderColor = "#000000"
             , font = myFont
             }

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
    , ((modMask,               xK_p     ), shellPrompt myXPConfig)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,               xK_d     ), spawn "lock")

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)
    , ((modMask              , xK_f     ), sendMessage $ JumpToLayout "Full")
    , ((modMask              , xK_r     ), sendMessage $ JumpToLayout "ResizableTall")
    -- Don't need split screens right now :)
    --, ((modMask .|. controlMask, xK_l   ), layoutSplitScreen 2 (TwoPane 0.5 0.5))
    --, ((modMask .|. controlMask, xK_r   ), rescreen)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,               xK_g     ), withFocused toggleBorder)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- move focus between screens
    , ((modMask .|. controlMask, xK_j   ),  nextScreen)
    , ((modMask .|. controlMask, xK_k   ),  prevScreen)
    , ((modMask,                 xK_z   ),  toggleWS)
    , ((modMask,                 xK_o   ),  shiftNextScreen)

    -- focus
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
    , ((0 , 0x1008ff12), spawn "pactl set-sink-mute 0 toggle")
    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff11), spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 -5%")
    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff13), spawn "pactl set-sink-mute 0 false ; pactl set-sink-volume 0 +5%")
    -- XF86AudioNext
    , ((0 , 0x1008ff17), spawn "mpc next")
    -- XF86AudioPrev
    , ((0 , 0x1008ff16), spawn "mpc prev")
    -- XF86AudioPlay
    , ((0 , 0x1008ff14), spawn "mpc toggle")
    -- XF86Display
    , ((0 , 0x1008ff59), spawn "xset dpms force off")
    -- XF86Display
    , ((0 , 0x1008ff59), spawn "xset dpms force off")
    -- XF86MonBrightnessDown
    -- , ((0 , 0x1008ff03), spawn "xbacklight -dec 2")
    -- XF86MonBrightnessUp
    -- , ((0 , 0x1008ff02), spawn "xbacklight -inc 2")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    -- mod-control-[1..9] %! Switch to workspace N greedily
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]]
