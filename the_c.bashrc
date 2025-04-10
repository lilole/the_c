# Dan's Linux .bashrc.
# This file is sourced by bash for non-login interactive shells.

# If not running interactively don't do anything
[[ $- == *i* ]] || return

# Source global definitions
for f in bashrc bash.bashrc; do
  [[ -r /etc/$f ]] && { . /etc/$f; break; }
done; unset f

## Enable custom function wrapper.
 #
THE_C_SOURCE="${THE_C_SOURCE:-$HOME/.bashrc.the_c}"
[[ -r $THE_C_SOURCE ]] && { USE_THE_C=true; } || { USE_THE_C=false; unset THE_C_SOURCE; }

if $USE_THE_C; then
  # TODO: Move this block to the Ruby file, to be emitted and sourced once here,
  #       with a special runtime arg, instead of keeping this hard code.

  ## Set up this bash instance and the Ruby background command service to
   # interface with each other from the `c` func.
   #
  the_c() {
    declare -gA THE_C
    while [[ $1 ]]; do
      local _sub_cmd="$1"; shift
      case "$_sub_cmd" in
        init)
          [[ ${THE_C[path]} ]] || THE_C[path]="$THE_C_SOURCE"
          [[ ${THE_C[base]} ]] || THE_C[base]="/tmp/$(basename "${THE_C[path]}")"
          THE_C[tmp]=$(for d in /dev/shm /tmp; do [[ -w $d ]] && { echo $d; break; }; done)
          the_c assert tmp || return 2
          THE_C[lock]="${THE_C[tmp]}/the_c-$$.lock"
        ;;

        assert)
          local attr attrs msg quiet=false
          [[ $1 ]] && { attrs="$*"; set --; } || attrs='-q pid' # Must consume args
          for attr in $attrs; do
            [[ $attr == -q ]] && { quiet=true; continue; }
            [[ ${THE_C[$attr]} ]] && continue
            if ! $quiet; then
              case "$attr" in
                tmp)  msg='+ the_c: Cannot init: Cannot find tmp dir.' ;;
                lock) msg='+ the_c: Call init first.' ;;
                pid)  msg='+ the_c is not running.' ;;
                *)    msg="+ the_c: Attribute '$attr' is not set." ;;
              esac
              echo >&2 "$msg"
            fi
            return 1
          done
        ;;

        start)
          the_c assert lock || return 2
          "${THE_C[path]}" "${THE_C[base]}" $$ < /dev/null & # Start the bg service
          THE_C[pid]=$!
          THE_C[input]="${THE_C[base]}-i${THE_C[pid]}"  # Must match named pipe in the service
          THE_C[output]="${THE_C[base]}-o${THE_C[pid]}" # Must match named pipe in the service
          trap 'the_c stop' EXIT
          local t=30; while [[ ! -e ${THE_C[output]} ]]; do (( --t < 1 )) && break; sleep 0.1; done
          (( t > 0 )) || { echo >&2 '+ the_c: Did not start.'; the_c stop; return 2; }
        ;;

        stop)
          local rc=0
          the_c assert pid && kill "${THE_C[pid]}"   ; (( rc += $? ))
          rm -f "${THE_C[input]}" "${THE_C[output]}" ; (( rc += $? ))
          unset THE_C[pid] THE_C[input] THE_C[output]; (( rc += $? ))
          (( rc == 0 )) || sleep 3
        ;;

        status)
          {
            echo -e '\nProcesses:'; the_c assert pid; c psg '\b(pts/\d+|bash|the_c)\b'
            echo -e '\nPipes:';     the_c assert base && c l -t "${THE_C[base]}"*
            echo -e '\nTmpFiles:';  the_c assert tmp  && c l -t "${THE_C[tmp]}"/TheC-Shortcuts-*
          } 2>&1 | c m
        ;;

        lock_on)
          the_c assert lock && while ! mkdir "${THE_C[lock]}" &> /dev/null; do sleep 0.1; done
        ;;

        lock_off)
          the_c assert lock && rmdir "${THE_C[lock]}"
        ;;

        *) return 1 ;;
      esac
    done
  }

  ### The wrapper for all the Ruby shortcuts.
    #
  c() {
    local last_rc=$? # Must be first; may be displayed in PS1
    the_c assert pid || return 1

    # Send the command to the bg service.
    # Note that caching `$last_rc` or `pwd` sometimes breaks here, so we always send them.
    local -a cmd=(
      "{{ENV[\"THE_C_LAST_RC\"]=\"$last_rc\";Dir.chdir(\"${PWD//\"/\\\"}\")}}"
      "$@"
    )
    the_c lock_on
    echo "${cmd[*]@Q}" > "${THE_C[input]}"

    # Read the response from the bg service
    local result
    IFS='' read -r result < "${THE_C[output]}"
    the_c lock_off

    # Complete the bg service logic in this context
    eval "$result"
  }

  the_c assert || the_c init start
fi # if $USE_THE_C

# ___________________________
# General env/options/aliases

export LANG=en_US.UTF-8
export TZ=US/Central

HISTCONTROL=erasedups
HISTSIZE=12000

case "$TERM" in
  screen) PS1='\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007\u@\h:\w \t\n\033k\033\134\$ ' ;;
  cygwin) PS1='\033]0;\u@\h:\w\007\033]2;\u@\h:\w\007\u@\h:\w \t\n\$ ' ;;
  *)      PS1='\u@\h:\w \t\n\$ '
esac

set -o emacs -P

# Beyond this point the_c is required _________________________________________
$USE_THE_C || return

export LESS=$(c less_options)
export EDITOR=$(c e)
export VISUAL="$EDITOR"
#export RUBYOPT='--enable=frozen-string-literal' # Before ruby might run

c setpath PATH "$HOME/bin" "$HOME/.local/bin" "${GEM_HOME:-/do_not_set_this}/bin" \
  /usr/local/bin /usr/local/sbin /usr/bin /usr/sbin /bin /sbin

[[ $TERM =~ ^xterm|^linux ]] && PS1="$(c ps1)"

# Aliases for most common operations
for a in e l m o u; do
  alias $a="c $a"
done; unset a

# ________________________________
# Specific app env/options/aliases

c at_home && export XZ_DEFAULTS='--threads=8 --verbose -9' # Faster xz compression

# rbenv
if [[ -d "$HOME/.rbenv/shims" ]]; then
  [[ $RBENV_ROOT ]] && c setpath PATH "$RBENV_ROOT/shims" "$RBENV_ROOT/bin" 0
  export RBENV_ROOT="$HOME/.rbenv"
fi
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
c x pikaur     && alias ua='c ua'
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

# _____________________________________________
# Always at bottom: Start X or screen if needed

if c need_x; then
  de=startxfce4 log="$HOME/.log.$de"
  echo ".bashrc: '$de' log is at: '$log'"; sleep 2
  exec &> "$log"; the_c stop; exec $de
#elif c need_screen; then
#  the_c stop; exec screen -xRR
fi
