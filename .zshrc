# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh_history
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install

# The following lines were added by compinstall
zstyle :compinstall filename '/home/mns/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Personal
set -o noclobber
setopt shwordsplit # Expand variables around spaces? Something like that

EDITOR='emacsclient -t -a ""'
export RANGER_LOAD_DEFAULT_RC=FALSE
export TERM=xterm-kitty
# export TERM=xterm-direct
# export TERM=xterm-256color
export CARGO_TARGET_DIR=~/.cache/rust_target
export FZF_DEFAULT_OPTS='--reverse --bind=ctrl-k:kill-line,ctrl-u:clear-query,ctrl-v:page-down,alt-v:page-up'
export BEMENU_OPTS=$(cat ~/.config/shell/bemenu_opts)

# Define all the functions at one place
function add_to_path {
    local new_dir=$1
    case ":$PATH:" in
        *:"$new_dir":*) ;;
        *) if [[ $2 == 'after' ]]; then
               export PATH="$PATH:$new_dir"
           else
               export PATH="$new_dir:$PATH"
           fi
           ;;
    esac       
}

function cht {
    curl -s "cheat.sh/$1"
}

function myquote {
    local -r day=$(date +%j) # day of the year
    local -r day_file=~/.cache/last_quote_day
    local -r quote_file=~/.cache/last_quote

    local quote= # declare the variable local in advance

    # Used in many places
    function _myquote_new_quote {
        quote=$(fortune)
        echo -n $quote >| $quote_file
    }

    function _myquote_new_day {
        echo -n $day >| $day_file
    }

    if [[ -f $day_file ]]; then
        # Obtain the quote
        if [[ $day == $(cat $day_file) ]]; then
            # Same day, just read the cached quote
            if [[ -f $quote_file ]]; then
                quote=$(cat $quote_file)
            else
                _myquote_new_quote
            fi
        else
            # Different day, renew both the last day & the cached quote
            _myquote_new_day
            _myquote_new_quote
        fi
        # Eitherway we have the quote in the variable
    else
        # Day file is not present, create everything from scratch
        _myquote_new_day
        _myquote_new_quote
    fi

    # We don't want then to pollute our shell
    unset -f _myquote_new_{day,quote}

    echo -en "\n========== Quote of the day ==========\n\n"
    echo $quote
    echo -en "\n======================================\n\n"
}

function fzf_cd {
    local args=(-I -t d)
    [[ $1 == "dot" ]] && args+=-H

    local dir=$(fd $args '.' "$PWD" | fzf)
    [[ -n $dir ]] && cd "$dir" || exit 1
}

function fzf_edit {
    local args=(-I -t f)
    [[ $1 == "dot" ]] && args+=-H

    local file=$(fd $args '.' "$PWD" | fzf)
    [[ -n $file ]] && $EDITOR "$file" || exit 1
}

function alacritty-colorscheme-next {
    alacritty-colorscheme -C ~/.config/alacritty/colors toggle &&
        alacritty-colorscheme -C ~/.config/alacritty/colors status
}

function is_command_exists {
    type "$1" &>/dev/null
}

function take {
    mkdir -p "$1"
    cd "$1"
}

function eman {
    emacsclient -t -a '' --eval "(progn (man \"$*\") (delete-window))"
}

function emanfzf {
    man_line=$(man -k . | fzf)
    [[ -z $man_line ]] && return 1

    man_and_sec=$(grep -Po '^.*?\)' <<<"$man_line")
    [[ -z $man_and_sec ]] && return 2

    eman "$man_and_sec"
}

function yfvgen {
    YTFZF_PREF="bestvideo[height<=?$1]+bestaudio/best" ytfzf
}
alias yf144='yfvgen 144'
alias yf240='yfvgen 240'
alias yf360='yfvgen 360'
alias yf480='yfvgen 480'
alias yf720='yfvgen 720'
alias yf1080='yfvgen 1080'

function yfagen {
    YTFZF_PREF="${1}audio" ytfzf
}
alias yfab='yfagen best'
alias yfaw='yfagen worst'

autoload -U colors && colors
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# kitty + complete setup zsh | source /dev/stdin

add_to_path "$HOME/.cargo/bin"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/.scripts"

# Prompt customizations
readonly _myprompt_csi=$(printf '\e[')
PS1="[%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m%{$reset_color%}] %{$fg[yellow]%}%~ %{$reset_color%}- "
PS2="$(printf '\u219d')  " # continuation prompt

bindkey -s '\ec' 'fzf_cd\n'
bindkey -s '\eC' 'fzf_cd dot\n'
bindkey -s '\ee' 'fzf_edit\n'
bindkey -s '\eE' 'fzf_edit dot\n'

# Load the aliases
source ~/.config/shell/aliases

# Lastly the glorified quote
myquote
