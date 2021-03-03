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
export TERM=xterm-direct
export CARGO_TARGET_DIR=~/.cache/rust_target
export FZF_DEFAULT_OPTS='--reverse'
export BEMENU_OPTS="--line-height 34 --fn 'Clear Sans Bold' --tb '#924D8B' --tf '#FFFFFF' --fb '#3D3D3D' --ff '#FFFFFF' --nb '#3D3D3D' --nf '#FFFFFF' --hb '#666666' --hf '#FFFFFF' --list 7 --bottom -P '' --ignorecase"

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

autoload -U colors && colors
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

add_to_path "$HOME/.cargo/bin"
add_to_path "$HOME/.local/bin"
add_to_path "$HOME/.scripts"

# Gruvbox color scheme
# 0th row
readonly gb_bg='#282828'
readonly gb_red='#cc241d'
readonly gb_green='#98971a'
readonly gb_yellow='#d79921'
readonly gb_blue='#458588'
readonly gb_purple='#b16286'
readonly gb_aqua='#689d6a'
readonly gb_gray='#a89984'

# 1st row
readonly gb_gray0='#928374'
readonly gb_red0='#fb4934'
readonly gb_green0='#b8bb26'
readonly gb_yellow0='#fabd2f'
readonly gb_blue0='#83a598'
readonly gb_purple0='#d3869b'
readonly gb_aqua0='#8ec07c'
readonly gb_fg='#ebdbb2'

# 2nd row
readonly gb_bg0_h='#1d2021'
readonly gb_bg0='#282828'
readonly gb_bg1='#3c3836'
readonly gb_bg2='#504945'
readonly gb_bg3='#665c54'
readonly gb_bg4='#7c6f64'
readonly gb_gray2='#928374'
readonly gb_orange='#d65d0e'

# 3rd row
readonly gb_bg0_s='#32302f'
readonly gb_fg4='#a89984'
readonly gb_fg3='#bdae93'
readonly gb_fg2='#d5c4a1'
readonly gb_fg1='#ebdbb2'
readonly gb_fg0='#fbf1c7'
readonly gb_orange0='#fe8019'

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
