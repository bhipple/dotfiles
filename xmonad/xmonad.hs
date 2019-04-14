import Control.Applicative
import System.Exit
import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Core (WorkspaceId)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders (smartBorders)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- TODO
-- * Investigate and setup xmonad-screenshot
-- * Investigate xmonad-windownames

-- Might need this at some point
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-EwmhDesktops.html
-- import XMonad.Hooks.EwmhDesktops

-- Put any local configuration here
import XMonadLocal

main = do
    screenCt <- countScreens
    xmproc <- spawnPipe "xmobar"
    xmonad $ conf screenCt xmproc

myTerminal :: String
myTerminal = "alacritty"

-- Program names that should not be managed and tiled
composeHook = composeAll [
        className =? "Gimp" --> doFloat
    ]

myManageHook = manageDocks <+> composeHook <+> manageHook def

-- Set the layouts we'd like and their ordering. For all of the other layouts available, see:
-- https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Config-Prime.html#t:LayoutClass
myLayoutHook = avoidStruts . smartBorders $ tiled ||| Mirror tiled ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
      tiled = Tall 1 (3/100) (1/2)

myLogHook xmproc = dynamicLogWithPP xmobarPP {
                            ppOutput = hPutStrLn xmproc,
                            ppTitle = xmobarColor "green" "" . shorten 50
                        }

myStartupHook = do
    spawn "~/.xinitrc"
    spawn "xautolock -time 30 -locker 'sudo /run/current-system/sw/bin/slock'"
    -- Standby time, suspend time, monitor off time
    spawn "xset s off"
    spawn "xset dpms 1800 1830 3600"

    -- Programs to launch
    spawn "emacs --daemon"
    -- TODO: This doesn't quite work to launch on other screens yet. Getting the
    -- name of the workspace wrong, perhaps?
    spawnOn "0_1" myTerminal
    spawnOn "0_2" "firefox"

restartCmd :: String
restartCmd = "if type xmonad; then xmonad --recompile && \
              \xmonad --restart; else xmessage xmonad not in PATH; fi"

conf screenCt xmproc =
        let myWorkspaces = withScreens screenCt $ map show [1..9] in
        def
        { terminal = myTerminal
        , modMask = mod4Mask
        , borderWidth = 3
        , focusFollowsMouse = False
        , XMonad.workspaces = myWorkspaces
        , startupHook = myStartupHook
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        -- docksEventhook must come last in ordering!
        , handleEventHook = handleEventHook def <+> docksEventHook
        , logHook = myLogHook xmproc
        , keys = myKeys
        }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn "dmenu_run")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_e     ), spawn "emc")
    , ((modMask .|. shiftMask, xK_f     ), spawn "firefox")
    , ((modMask .|. shiftMask, xK_x     ), spawn "~/.xinitrc")
    , ((controlMask .|. shiftMask, xK_l), spawn "sudo /run/current-system/sw/bin/slock")

    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modMask,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- restarting
    , ((modMask, xK_r), spawn restartCmd)
    ]
    ++
    -- mod-{a,s,d} %! Switch focus to physical screens
    -- mod-shift-{a,s,d} %! Throw client to physical screen
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip xKeys xOrder
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-[1..6] %! Switch focus to workspace N of this screen
    -- mod-shift-[1..6] %! Move client to workspace N of this screen
    [ ((m .|. modMask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
