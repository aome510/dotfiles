# Color palette
# https://github.com/dracula/dracula-theme#color-palette
declare-option str black 'rgb:282a36'
declare-option str gray 'rgb:44475a'
declare-option str white 'rgb:f8f8f2'
declare-option str blue 'rgb:6272a4'
declare-option str cyan 'rgb:8be9fd'
declare-option str green 'rgb:50fa7b'
declare-option str orange 'rgb:ffb86c'
declare-option str pink 'rgb:ff79c6'
declare-option str purple 'rgb:bd93f9'
declare-option str red 'rgb:ff5555'
declare-option str yellow 'rgb:f1fa8c'
# based on https://github.com/dracula/kakoune/blob/master/colors/dracula.kak
face global PrimarySelection "%opt{black},%opt{pink}"
face global SecondarySelection "%opt{black},%opt{purple}"
face global MenuForeground "%opt{blue},%opt{white}+b"
face global MenuBackground "%opt{white},%opt{blue}"
face global MenuInfo        "%opt{cyan},%opt{blue}"
face global Information     "%opt{yellow},%opt{gray}"
