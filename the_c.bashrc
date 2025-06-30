# Dan's Linux .bashrc.
# This file is sourced by bash for non-login interactive shells.

# If not running interactively don't do anything
[[ $- == *i* ]] || return

# Source global definitions
for f in bashrc bash.bashrc; do
  [[ -r /etc/$f ]] && { . /etc/$f; break; }
done; unset f

## General env/options/aliases ________________________________________________
 #
export LANG=en_US.UTF-8
export TZ=US/Central
#export RUBYOPT='--enable=frozen-string-literal'

HISTCONTROL=erasedups
HISTSIZE=12000

case "$TERM" in
  screen) PS1='\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007\u@\h:\w \t\n\033k\033\134\$ ' ;;
  cygwin) PS1='\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007\u@\h:\w \t\n\$ ' ;;
  *)      PS1='\u@\h:\w \t\n\$ '
esac

set -o emacs -P

## Beyond this point the_c is required ________________________________________
 #
THE_C_SOURCE="$HOME/.bashrc.the_c"
[[ -r $THE_C_SOURCE ]] || { unset THE_C_SOURCE; return; }
$("$THE_C_SOURCE" bash_init) || return

## General env/options/aliases ________________________________________________
 #
export LESS=$(c less_options)
export EDITOR=$(c e)
export VISUAL="$EDITOR"

c setpath PATH "$HOME/bin" "$HOME/.local/bin" "${GEM_HOME:-/do_not_set_this}/bin" \
  /usr/local/bin /usr/local/sbin /usr/bin /usr/sbin /bin /sbin

[[ $TERM =~ ^xterm|^linux ]] && PS1="$(c ps1)"

# Aliases for most common operations
for a in e l m o u; do
  alias $a="c $a"
done; unset a

## Specific app env/options/aliases ___________________________________________
 #
c at_home && export XZ_DEFAULTS='--threads=8 --verbose -9' # Faster xz compression

# rbenv
if [[ -d "$HOME/.rbenv/shims" ]]; then
  [[ $RBENV_ROOT ]] && c setpath PATH "$RBENV_ROOT/shims" "$RBENV_ROOT/bin" 0
  export RBENV_ROOT="$HOME/.rbenv"
fi
[[ $RBENV_ROOT && -d $RBENV_ROOT ]] || unset RBENV_ROOT
if [[ $RBENV_ROOT ]]; then
  c setpath PATH "$HOME/bin" "$HOME/.local/bin" "$RBENV_ROOT/shims" "$RBENV_ROOT/bin" /usr/local/bin
  #eval "$(rbenv init -)" # Creates some convenience functions

  unset rbenv
  alias rbenv="env RUBY_CONFIGURE_OPTS=--disable-install-doc '$(type -P rbenv)'"
fi

# Shortcut aliases
c x journalctl && alias jc='c jc'
c x uys        && alias pcc='uys pcc'
c x systemctl  && alias sc='c sudo systemctl'
c x aura       && alias ua='c ua'
if c x docker; then
  alias d='docker'
  alias dc='docker-compose'
  alias dcb='dc up --build --no-start'
  alias dcbr='dc up --build --detach'
  alias dce='dc exec -e LINES=$LINES -e COLUMNS=$COLUMNS'
  alias dcl='dc logs --timestamps --follow --tail=333'
  alias dcp='dc ps'
  alias dcr='dc run -e LINES=$LINES -e COLUMNS=$COLUMNS --rm --no-deps'
  alias dr='d run --rm --name dr$$ -it'
fi
if   c x rg;        then alias g='rg --color=always'
elif c x pcre2grep; then alias g='pcre2grep --color=always'
elif c x pcregrep;  then alias g='pcregrep --color=always'
elif c x grep;      then alias g='grep -E'
fi

## Always at bottom: Start X or screen if needed ______________________________
 #
if c need_x; then
  de=startxfce4 log="$HOME/.log.$de"
  #echo ".bashrc: '$de' log is at: '$log'"; sleep 1
  exec &> "$log"; the_c stop; exec $de
#elif c need_screen; then
#  the_c stop; exec screen -xRR
fi
