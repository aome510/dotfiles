# kak plugin
source "%val{config}/plugins/plug.kak/rc/plug.kak"

# relative line numbers
add-highlighter global/ number-lines -relative -hlcursor
add-highlighter global/ wrap

#Use Tab for both indenting and completion
hook global InsertChar \t %{ try %{
  execute-keys -draft "h<a-h><a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@"
}}
hook global InsertDelete ' ' %{ try %{
  execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
}}
hook global InsertCompletionShow .* %{
    try %{
        # this command temporarily removes cursors preceded by whitespace;
        # if there are no cursors left, it raises an error, does not
        # continue to execute the mapping commands, and the error is eaten
        # by the `try` command so no warning appears.
        execute-keys -draft 'h<a-K>\h<ret>'
        map window insert <tab> <c-n>
        map window insert <s-tab> <c-p>
    }
}
hook global InsertCompletionHide .* %{
    unmap window insert <tab> <c-n>
    unmap window insert <s-tab> <c-p>
}

# Color scheme
colorscheme gruvbox
# for transparency
face global Default default,default
face global BufferPadding blue,default

# System clipboard interactions
hook global NormalKey y|d|c %{ nop %sh{
  printf %s "$kak_main_reg_dquote" | xsel --input --clipboard
}}
map global user P '!xsel --output --clipboard<ret>' -docstring 'paste before the beginning of a selection using clipboard'
map global user p '<a-!>xsel --output --clipboard<ret>' -docstring 'paste after the end of a selection using clipboard'
map global user r '<a-d>!xsel --output --clipboard<ret>' -docstring 'replace a selection with yanked text using clipboard'

# Tab sizes
hook global InsertChar \t %{ exec -draft -itersel h@ }
set global tabstop 4
set global indentwidth 4

# Add scheme support for racket
hook global BufCreate .+\.(rkt) %{
    set-option buffer filetype scheme
}

# Add sh support for .conf files
hook global BufCreate .+\.(conf) %{
    set-option buffer filetype sh
}

# change indent width for c and scheme files to 2
hook global WinSetOption filetype=(scheme|c|cpp) %{
    set global tabstop 2
    set global indentwidth 2
}

# save file
map global normal '<c-s>' :write<ret> -docstring 'save file'

# comment
map global normal '#' :comment-line<ret> -docstring 'comment line'
map global normal '<a-#>' :comment-block<ret> -docstring 'comment block'

# map goto a specific line in a file
map global goto p -docstring 'Go to the current cursor''s grep position' '<esc>: grep-jump<ret>'

# edit kakrc
map global user e ':edit ~/.config/kak/kakrc<ret>' -docstring 'edit kakrc'

# kak-lsp
eval %sh{kak-lsp --kakoune -s $kak_session}
hook global WinSetOption filetype=(rust|python|go|javascript|typescript|c|cpp) %{
    lsp-enable-window

    map global normal <c-l> ": enter-user-mode lsp<ret>"
    lsp-auto-hover-enable
    lsp-auto-hover-insert-mode-enable
    set global lsp_hover_anchor true
    # Format the document if possible
    hook window BufWritePre .* %{ lsp-formatting }
    # for python language server
    set-option global lsp_server_configuration pyls.configurationSources=["flake8"]
}

# powerline
plug "andreyorst/powerline.kak" defer powerline %{
    set-option global powerline_format 'git bufname filetype mode_info line_column position'
    set-option global powerline_shorten_bufname 'short'
    powerline-theme base16-gruvbox
} config %{
    powerline-start
}

# fzf for KAK
plug "andreyorst/fzf.kak" config %{
    map -docstring 'fzf mode' global normal '<c-p>' ': fzf-mode<ret>'
}

# surround
plug "h-youhei/kakoune-surround" config %{
    declare-user-mode surround
    map global surround s ':surround<ret>' -docstring 'surround'
    map global surround c ':change-surround<ret>' -docstring 'change'
    map global surround d ':delete-surround<ret>' -docstring 'delete'
    map global surround t ':select-surrounding-tag<ret>' -docstring 'select tag'
    map global user s ':enter-user-mode surround<ret>' -docstring 'enter surround mode'
}
