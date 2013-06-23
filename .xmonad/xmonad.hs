import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/machuga/.xmobarrc"
    xmonad $ defaultConfig {
        focusedBorderColor = "#555555"
        , terminal = "gnome-terminal"
        , layoutHook = avoidStruts $ layoutHook defaultConfig
    }
