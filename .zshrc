# start emacs daemon
alias edae="emacs --daemon"
# open up running emacs daemon
alias ec="emacsclient -c -a '' -n"

#cd into git parent directory
cg() {
    cd "$(git rev-parse --show-toplevel)"
}

#cd into git parent directory if in submodule
cgg() {
    cd "$(git rev-parse --show-superproject-working-tree)"
}