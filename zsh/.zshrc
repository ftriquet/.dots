zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle :compinstall filename "$HOME/.zshrc"
fpath+=~/.zfunc

autoload -Uz compinit
compinit
HISTFILE=~/.zsh_history
HISTSIZE=1000 SAVEHIST=1000
setopt extendedglob notify
bindkey -e						# emacs-like key-bindings

setopt hist_expire_dups_first	# when trimming history, lose oldest duplicates first
setopt hist_ignore_dups			# Do not write events to history that are duplicates of previous events
setopt hist_ignore_space		# remove command line from history list when first character on the line is a space

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh_cache
zmodload zsh/complist
setopt extendedglob
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"

autoload colors; colors


export MANPAGER='less -X'
export EDITOR=vim

case $(uname) in
	Linux)
		alias ls='ls --color=auto'
		export JAVA_HOME=/usr/lib/jvm/java-8-openjdk
		;;
	Darwin)
		alias 'ls=ls -G'
		;;
esac

alias ls=exa
alias l='exa -la'
alias s='exa -la'
alias ..='cd ..'
alias ...="cd ../../"
alias ....='cd ../../..'
alias vi=vim
alias v=vim
alias dev='cd $HOME/dev'
alias cp='cp -v'
alias rm='rm -v'
alias mv='mv -v'
alias ':q=exit'
alias ':qa=exit'

export LC_ALL=en_US.UTF-8

export TERM=xterm-256color
export TERMINAL=termite

## Prompt
setopt prompt_subst				# Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
setopt transient_rprompt		# only show the rprompt on the current prompt
autoload -U colors && colors	# Enable colors in prompt
autoload -U zmv

source $HOME/.dots/git-prompt.sh
GIT_PS1_SHOWUPSTREAM='verbose name legacy git'
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWCOLORHINTS=1

export RPROMPT='%(1j.%j.)'
export PROMPT='
%F{yellow}%B%n@%m%b%f:%F{blue}%B%4~%b%f$(__git_ps1 " (%s)") %# '

GEMS_PATH="$HOME/.gem/ruby/2.4.0/bin"
HOME_PATH="$HOME/bin/"

precmd () { __git_ps1 "
%F{yellow}%B%n@%m%b%f:%F{blue}%B%4~%b%f" " %# " " (%s)"
}

# precmd () { __git_ps1 "
# [ %{$fg[blue]%}%3~%{$reset_color%}" "%{$reset_color%}] %# " " %s "
# }

export GOPATH="$HOME/dev/go"
HOME_PATH=$HOME/bin
LOCAL_PATH="$HOME/.local/bin"
CARGO_PATH="$HOME/.cargo/bin"
CABAL_PATH="$HOME/.cabal/bin"
LUA_PATH="$HOME/.luarocks/bin"
export RUST_SRC_PATH=~/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

export PATH="$LUA_PATH:$CABAL_PATH:$CARGO_PATH:$LOCAL_PATH:$GOPATH/bin:$GEMS_PATH:$HOME/.bin:$HOME_PATH:$PATH"

# rename tab titles in terminator
rename-tab() {
	printf '\e]2;%s\a' "$*";
}

mk () {
	mkdir -p "$@" && cd "$@"
}

ipof () {
	local ANSIBLE_ROOT=~/dev/recast/infra/ansible
	local HOST=$1
	local LINE="HOSTNAME IP\n"
	LINE+=`cat $ANSIBLE_ROOT/inventory/static_inventory | grep $HOST | grep ansible_host | sed 's/ansible_host=//g'`
	LINE=`echo $LINE | column -t`
	echo $LINE
}

alias infra='cd /home/francois/dev/recast/infra/ansible'

notif() {
	$@
	notify-send "command terminated: $@"
}

list() {
	rg $@ | cut -d ':' -f1 | sort | uniq
}

vim-find() {
	vim -p `list "${1}"`
}

man2pdf() {
	man -t "$1" | ps2pdf - > "$2"
}

alias doit='`fc -ln -1`'
alias fucking='sudo '
