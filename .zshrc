# start emacs daemon
alias edae="emacs --daemon"
# open up running emacs daemon
alias ec="emacsclient -c -a '' -n"

#cd into git parent directory
cg() {
    cd "$(git rev-parse --show-toplevel)"
    if [ -n "$1" ]
        then
	    cd $1
    fi
}

#cd into git parent directory if in submodule
cgg() {
    cd "$(git rev-parse --show-superproject-working-tree)"
}

bak () {
    mv "$1"  "$1.bak"
}
unbak () {
    mv "$1.bak"  "$1"
}

cdls() { 
    cd "$@" && ls; 
}

alias k=kubectl
alias kc='kubectl config use-context'
alias kcc='kubectl config current-context'
alias wk='watch kubectl'

#use direnv
# eval "$(direnv hook zsh)"
