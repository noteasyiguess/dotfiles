export BROWSER=firefox
export EDITOR=nvim
export CARGO_TARGET_DIR=~/.cache/rust_target
#export MOST_INITFILE=~/.config/most/most.rc
#export MANPAGER=most
#export PAGER=most

# Enable colors
#autoload -U colors && colors
#PS1="%B%{$fg[blue]%}%n%{$fg[green]%}  %1d%{$fg[yellow]%} %{%G₹%}%{$reset_color%}%b "
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%1d%{$fg[red]%}]%{$fg[cyan]%} %{%G%} %{$reset_color%}%b "

# Historical symbols
# ☸
# 𑓇
# 🕉
# 

# Enable history
HISTFILE=~/.cache/zsh/history
SAVEHIST=2000
HISTSIZE=2000
setopt share_history
setopt append_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks

setopt auto_cd
setopt hist_verify
setopt correct
setopt correct_all

# Autocomplection
autoload -Uz compinit
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix 
zstyle ':completion:*' menu select
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''
zmodload zsh/complist
compinit
#_comp_options+=(globdots)  # Include hidden files

# Keymaps
export KEYTIMEOUT=1
bindkey -v
bindkey '^R' history-incremental-search-backward
bindkey -M menuselect h vi-backward-char
bindkey -M menuselect k vi-up-line-or-history
bindkey -M menuselect l vi-forward-char
bindkey -M menuselect j vi-down-line-or-history

# Change cursor shape for different vi modes
zle-keymap-select() {
   if [[ ${KEYMAP} == vicmd ]] ||
      [[ $1 = 'block' ]]; then
      echo -ne '\e[1 q'
   elif [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == main ]] ||
        [[ $1 = 'beam' ]]; then
      echo -ne '\e[5 q'
   fi
}

# zle -N zle-keymap-select
# zle-line-init() {
#    zle -K viins
#    echo -ne '\e[5 q'
# }
# zle -N zle-line-init
# echo -ne '\e[5 q'  # Beam cursor on startup
# preexec() { echo -ne '\e[5 q' }  # Beam cursor on startup

# Use lf to switch directories and bind it to ctrl+o
lfcd() {
   tmp="$(mktemp)"
   lf -last-dir-path="$tmp" "$@"
   if [ -f "$tmp" ]; then
      dir="$(cat "$tmp")"
      rm -f "$tmp"
      [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
   fi
}
bindkey -s '^o' 'lfcd\n'

#source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
#source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# Useful functions
hex() {
   emulate -L zsh
   if [[ -n "$1" ]]; then
      printf '%x\n' $1
   else
      print 'Usage: hex <number to convert>'
      return 1
   fi
}

source ~/Tidbits/scripts/crun.func

# Colored man
man() {
    LESS_TERMCAP_md=$'\e[01;32m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;33m' \
    command man "$@"
}

# Aliases
alias -g vim='nvim'
alias -g pm='pacman'
alias -g py='python3'
alias pr='paru'
alias cl='clear'
alias cp='cp -iv'
alias cpi='rsync -ah --info=progress2'
alias ffmpeg='ffmpeg -hide_banner'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias info='pinfo'
alias ka='killall'
alias ll='ls -l'
#alias lm='ls -t -1'
#alias lt='ls -h --size -1 -S --classify'
#alias ls='ls --color=auto'
alias ls='exa'
alias mv='mv -iv'
alias myip='curl ipinfo.io/ip'
alias rm='rm -v'
alias s='sudo'
alias sha='sha256sum'
alias spm='sudo pacman'
alias tmux='tmux -u'
alias untar='tar -vxf'
alias update='sudo pacman -Syu'
alias yt='youtube-dl --add-metadata -i'
alias yta='yt -x -f bestaudio/best'

# Sneaky stuff
function i_am_bored() {
    while true; do
        echo -n '\nEnter to continue: '
        read line

        if [ "$line" = $'' ]; then
            echo 'Fetching some interesting activity todo' 
            curl -s 'https://www.boredapi.com/api/activity' | jq
        else
            echo 'Hope u found something interesting'
            break
        fi
    done
}

function set_sway_bg() {
    ln "$(realpath $1)" ~/.cache/bg -sf
    swaymsg output '*' background ~/.cache/bg fill
}

function set_sway_lockbg() {
    ln "$(realpath $1)" ~/.cache/lockbg -sf
}

function cdl() {
   cd $@ && ls
}

source ~/.zshrc_host
