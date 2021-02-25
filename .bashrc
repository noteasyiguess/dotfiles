# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source "$HOME/.cargo/env"

function prepend_to_path {
    local new_dir=$1
    case ":$PATH:" in
	*:"$new_dir":*) ;;
	*) export PATH="$new_dir:$PATH" ;;
    esac
}

function cht {
    curl -s "cht.sh/$1"
}

function myquote {
    local day=$(date +'%j') #day of year
    
    local quote=""
    local quote_day_file=~/.cache/last_quote_day
    local quote_file=~/.cache/last_quote

    function _myquote_create_new_quote {
	quote=$(fortune)
	echo -n $quote >| $quote_file
    }
    
    [[ -f $quote_day_file ]] && {
	# File exists
	[[ $day == $(cat $quote_day_file) ]] && {
	    # Same day, just read from file, if exists, else just create one
	    [[ -f $quote_file ]] &&
		quote=$(cat $quote_file) ||
		    _myquote_create_new_quote
	} || {
	    # Different day, change last day, & create new quote
	    echo -n $day >| $quote_day_file
	    _myquote_create_new_quote
	}
	quote=$(cat $quote_file)
    } || {
	# File doesn't exist, create everything from scratch
	echo -n $day >| $quote_day_file
	_myquote_create_new_quote
    }

    unset -f _myquote_create_new_quote
    
    echo -en "\n=============== Quote Of The Day ===============\n\n"
    (( 0 )) &&
	echo -n $quote |
	    figlet -f "$HOME/.local/share/figlet-fonts/Big Money-ne.flf" -w $(tput cols) |
	    lolcat \
		|| echo $quote
    echo -en "\n================================================\n\n"
}

function _fzf_cd {
    local args="-I --type d "
    [[ $1 == "dot" ]] && args+="-H "
    local dir=$(fd $args '.' "$PWD" | fzf)
    [[ -n "$dir" ]] && cd "$dir"
}

function _fzf_history {
    local h=$(history 1 | rg '^\d' | rg -v '(history|_fzf_\w+)' | sort -nr -k1 | fzf)
    local cmd=$(echo $h | sed 's/^\s*[0-9]*\s*//')
    [[ -n "$cmd" ]] && eval $cmd
}

function _fzf_edit {
    local args="-I --type f "
    [[ $1 == "dot" ]] && args+="-H "
    local file=$(fd $args | fzf)
    [[ -n "$file" ]] && $EDITOR "$file"
}

function alacritty-colorscheme-toggle {
  alacritty-colorscheme -C ~/.config/alacritty/colors toggle && 
    alacritty-colorscheme -C ~/.config/alacritty/colors status
}

function quiet {
  "$*" &>/dev/null &
}

function extract {
  if [[ -f "$1" ]]; then
    case "$1" in
      *.tar.bz2)    tar xjf "$1"    ;;
      *.tar.gz)     tar xzf "$1"    ;;
      *.tbz2)       tar xjf "$1"    ;;
      *.tgz)        tar xzf "$1"    ;;
      *.tar)        tar xf "$1"     ;;
      *.bz2)        bunzip2 "$1"    ;;
      *.rar)        unrar e "$1"    ;;
      *.gz)         gunzip "$1"     ;;
      *.zip)        unzip "$1"      ;;
      *.Z)          uncompress "$1" ;;
      *.7z)         7z x "$1"       ;;
      *) echo "'$1' is not supported by extract()" ;;
    esac
  else
    echo "'$1' is not a file"
  fi
}

function command_exists {
  type "$1" &>/dev/null
}

function take {
  mkdir -p "$1"
  cd "$1" || exit
}

HISTFILE=~/.cache/mksh_history

#[[ -f ~/.config/ls_colors ]] && export LS_COLORS=$(cat ~/.config/ls_colors)
export CARGO_TARGET_DIR=~/.cache/rust_target
#export FZF_DEFAULT_OPTS="--reverse"
export BEMENU_OPTS="--line-height 34 --fn 'Clear Sans Bold' --tb '#924D8B' --tf '#FFFFFF' --fb '#3D3D3D' --ff '#FFFFFF' --nb '#3D3D3D' --nf '#FFFFFF' --hb '#666666' --hf '#FFFFFF' --list 7 --bottom -P '' --ignorecase"

set -o emacs
set -o noclobber #use >| to force redirection to an existing file

prepend_to_path "$HOME/.cargo/bin"
prepend_to_path "$HOME/.local/bin"
prepend_to_path "$HOME/.scripts"

_myprompt_csi="$(printf '\e[')"
function _myprompt_func() {
    local retcode=$?
    local prompt=""
    
    declare -n csi=_myprompt_csi
    local italic=${csi}3m
    local bold=${csi}1m
    local reset=${csi}0m
    
    (( $retcode != 0 )) &&
	prompt+="$bold$italic${csi}91m$retcode "
    
    # prompt+="${csi}0m${csi}96m${csi}1m$USER "
    # prompt+="${csi}0m${csi}94mat "

    prompt+="$reset$bold${csi}93m"
    [[ "$PWD" == "$HOME" ]] && prompt+=" " || {
	    [[ "$PWD" == '/' ]] &&
		prompt+="/ " ||
		    prompt+="${PWD##*/} "
	}
    
    prompt+="$reset${csi}94m"
    echo -n "$prompt"
}
(( UID )) && PS1="" || PS1="#"
PS1='$(_myprompt_func)'"${PS1}${_myprompt_csi}0m "
PS2="${_myprompt_csi}95m$(printf '\u219d')${_myprompt_csi}0m  "

# bind -m '^R=_fzf_history^J'
# bind '^L=clear-screen'
bind -x '"\ec":"_fzf_cd"'
bind -x '"\eC":"_fzf_cd dot"'
bind -x '"\ee":"_fzf_edit"'
bind -x '"\eE":"_fzf_edit dot"'

alias g='git'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias lgrep='grep -P'
alias ls='exa'
alias lt='ls -schanged -l'
alias diff='diff --color=auto'
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -v'
alias mkdir='mkdir -pv'
alias less='less -FSRXc'
alias c='clear'
alias pls='sudo'
alias dfi='sudo dnf install'
alias dfs='sudo dnf search'
alias dfr='sudo dnf remove'
alias dfu='sudo dnf --setopt=install_weak_deps=False upgrade'

alias hd='hexdump -C'
alias dud='du -d1 -h'
alias duf='du -sh *'
alias nano='nano -W'
alias wget='wget -c'
alias bc='bc --mathlib --quiet'
alias fd='fd -I'
alias vim='nvim'

alias num_files='ls -1 | wc -l'
alias qfind='find . -name '
alias open_ports='sudo lsof -i | grep LISTEN'

alias cal3='cal -3'
alias today='date "+%A, %B %-d, %Y"'
alias weeknum='date +%V'

alias myip='dig @resolver4.opendns.com myip.opendns.com +short' 
alias myip4='dig @resolver4.opendns.com myip.opendns.com +short -4'
alias myip6='dig @resolver1.ipv6-sandbox.opendns.com AAAA myip.opendns.com +short -6'

myquote
