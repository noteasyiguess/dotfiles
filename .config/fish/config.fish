set -x PATH ~/.local/bin ~/.cargo/bin ~/Tidbits/scripts /usr/sbin $PATH
set -x EDITOR nvim
set -x SHELL /bin/fish
set -x MANPAGER "$EDITOR +Man!"
set -x CARGO_TARGET_DIR ~/.cache/rust_target

set fish_greeting

function fish_prompt
  set last_status $status
  if test $last_status -ne 0
    set_color $fish_color_status
    printf '[\uf069 %s] ' $last_status
    set_color normal
  end

  set cwd (basename $PWD)
  test $cwd = $USER && set cwd '~'

  set_color $fish_color_cwd
  printf '\ue5ff %s ' $cwd
  set_color normal

  set_color $fish_color_command
  printf '\uf1ee '
  set_color normal
end

abbr vim $EDITOR
abbr ofc "$EDITOR ~/.config/fish/config.fish"

function cdl; cd $argv && ls; end
alias diff 'diff --color=auto'
alias vpage 'nvim -u ~/.vimrc.pager'
alias ec="emacsclient -c -a ''"
alias et="emacsclient -nw -a ''"

function vman
  # Find directories of all english manual pages
  set man_locs (find (manpath | tr ':' '\n') -maxdepth 1 -type d -name 'man*?')

  # Select a man page
  set man_half (find $man_locs -type f | xargs basename -s .gz {} \; | sort -ur | sk)
  test -z $man_half && echo 'No man page selected' && return 1

  # Find its full path
  set man_full (find $man_locs -type f -name $man_half'*' | head -n 1)
  test -z $man_full && echo "Can't find the man page's full path. Shouldn't have happened" && return 1

  # Display
  set -x MANPAGER "$EDITOR +Man!"
  man $man_full
end
