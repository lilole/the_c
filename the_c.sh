# Copyright 2024 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

### This is the Bash-specific integration of `the_c.rb` file. It is meant to be
  # included in a `.bashrc` file, along with the contents of `the_c.rb` file,
  # so that all features can be available on any server by shipping around a
  # single file.
  #
  # If you want to keep your `bashrc` small, you can ship this file, along with
  # a separate `the_c.rb` if you wish, to your servers, and integrate them
  # manually.

# Enable custom function wrapper.
#
[[ -r "$HOME/.the_c.local.rb" ]] && THE_C_BODY=$(< "$HOME/.the_c.local.rb") || \
THE_C_BODY=$(cat << 'END_THE_C_BODY'
  #
  # ...paste the entire contents of `the_c.rb` file here, or leave this blank
  # and copy `the_c.rb` to the "$HOME/.the_c.local.rb" location...
  #
  # Note that `the_c init` will use the `THE_C_BODY` env var and unset it.
  #
  # If you want to use your own command service, you can point directly to your
  # own file below, with these steps:
  #   1. Remove this block that sets `THE_C_BODY`.
  #   2. Change `THE_C[path]` assignment in the "init)" block below to your
  #      command service executable file's path.
  #   3. Reduce the "body)" block below to a nop, e.g. a single `:` command.
  #   4. If using your own custom command service:
  #      a. Make sure the "start)" block runs your command, instead of
  #           ruby "${THE_C[path]}" ...
  #      b. If your custom service does not use input and output named pipes,
  #         then use some other mechanism besides the `THE_C[input]` and
  #         `THE_C[output]` used below.
  #      c. Logic for `THE_C[pid]` should continue to work without changes.
  #
END_THE_C_BODY
)

### Set up this bash instance and the Ruby background command service to
  # interface with each other from the `c` func.
  #
the_c() {
  declare -gA THE_C
  while [[ $1 ]]; do
    local _sub_cmd="$1"; shift
    case "$_sub_cmd" in
      init)
        [[ ${THE_C[path]} ]] || THE_C[path]="$HOME/.the_c.rb"
        [[ ${THE_C[base]} ]] || THE_C[base]="/tmp/$(basename "${THE_C[path]}")"
        THE_C[tmp]=$(for d in /dev/shm /tmp; do [[ -w $d ]] && { echo $d; break; }; done)
        the_c assert tmp || return 2
        THE_C[lock]="${THE_C[tmp]}/the_c-$$.lock"
        the_c body
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

      body)
        the_c lock_on || return 2
        if [[ -r ${THE_C[path]} ]]; then
          local sum1=$(md5sum <<< "$THE_C_BODY") sum2=$(md5sum < "${THE_C[path]}")
          [[ $sum1 == $sum2 ]] && unset THE_C_BODY
        fi
        if [[ ${THE_C_BODY:0:1} ]]; then
          cat > "${THE_C[path]}" <<< "$THE_C_BODY"
          unset THE_C_BODY
        fi
        the_c lock_off
      ;;

      start)
        the_c assert lock || return 2
        ruby "${THE_C[path]}" "${THE_C[base]}" $$ < /dev/null & # Start the bg service
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

# ...the rest of your `.bashrc` follows here...
