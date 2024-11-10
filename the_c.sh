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

THE_C_BODY=$(cat << 'END_THE_C_BODY'
  #
  # ...paste the entire contents of `the_c.rb` file here...
  #
  # Note that `the_c init` will use the `THE_C_BODY` env var and unset it.
  #
  # If you want to use your own command service, or not include all of the
  # non-Bash source here, you can point directly to your own file below, with
  # these steps:
  #   1. Remove this block that sets `THE_C_BODY`.
  #   2. Reduce the "init)" block below to a single line that sets `THE_C` to
  #      the command service executable file's path.
  #   3. If using your own custom command service:
  #      a. Make sure the "start)" block runs your command, instead of
  #           ruby "$THE_C" ...
  #      b. If your custom service does not use input and output named pipes,
  #         then use some other mechanism besides the `THE_C_INPUT` and
  #         `THE_C_OUTPUT` used below.
  #      c. Logic that uses `THE_C_PID` should continue to work without changes.
  #
END_THE_C_BODY
)

### Set up this bash instance and the Ruby background command service to
  # interface with each other from the `c` func.
  #
the_c() {
  while [[ $1 ]]; do
    case "$1" in
      init)
        the_c lock_on
        THE_C="$HOME/.the_c.rb"
        if [[ -r $THE_C ]]; then
          local sum1=$(md5sum <<< "$THE_C_BODY") sum2=$(md5sum < "$THE_C")
          [[ $sum1 == $sum2 ]] && unset THE_C_BODY
        fi
        if [[ ${THE_C_BODY:0:1} ]]; then
          cat > "$THE_C" <<< "$THE_C_BODY"
          unset THE_C_BODY
        fi
        the_c lock_off
      ;;

      start)
        [[ $THE_C && -r $THE_C ]] || { echo >&2 '+ the_c: Cannot start.'; return 1; }
        local base="/tmp/$(basename "$THE_C")" i
        ruby "$THE_C" "$base" $$ < /dev/null &
        THE_C_PID=$!
        THE_C_INPUT="${base}-i${THE_C_PID}"
        THE_C_OUTPUT="${base}-o${THE_C_PID}"
        trap 'the_c stop' EXIT
        for i in `seq 30`; do [[ -e $THE_C_OUTPUT ]] && break || sleep 0.1; done
      ;;

      stop)
        [[ $THE_C_PID ]] || return 1
        local rc=0
        kill "$THE_C_PID"; (( rc += $? ))
        rm -f "$THE_C_INPUT" "$THE_C_OUTPUT"; (( rc += $? ))
        unset THE_C_PID THE_C_INPUT THE_C_OUTPUT
        (( rc == 0 )) || sleep 3
      ;;

      status)
        {
          echo -e '\nProcesses:'; c psg '\b(pts/\d+|bash|the_c)\b'
          echo -e '\nPipes:';     c l -t /tmp/.the_c*
          echo -e '\nTmpFiles:';  c l -t "$(the_c fast_tmp_dir)"/Shortcuts-*
        } 2>&1 | c m
      ;;

      lock_on)
        [[ $THE_C_LOCK ]] || THE_C_LOCK="$(the_c fast_tmp_dir)/the_c-$$.lock"
        while ! mkdir "$THE_C_LOCK" &> /dev/null; do sleep 0.1; done
      ;;

      lock_off)
        rmdir "$THE_C_LOCK"
      ;;

      fast_tmp_dir)
        if   [[ -w /dev/shm ]]; then echo /dev/shm
        else echo /tmp
        fi
      ;;

      *) return 1 ;;
    esac
    shift
  done
}

### The wrapper for all the Ruby shortcuts.
  #
c() {
  local last_rc=$? # Must be first; may be displayed in PS1
  [[ $THE_C_PID ]] || { echo >&2 '++ the_c is not installed.'; return 1; }

  # Add special context state to send to the bg service
  local cmd0 cwd=$(pwd)
  cmd0="ENV[\"THE_C_LAST_RC\"] = \"$last_rc\";" # For some reason this can't be cached in a global
  if [[ $cwd != $THE_C_LAST_DIR ]]; then
    THE_C_LAST_DIR="$cwd"
    cmd0="${cmd0}Dir.chdir(\"$cwd\");"
  fi

  # Send the command to the bg service
  local -a cmd
  [[ ${cmd0:0:1} ]] && cmd+=("{{${cmd0}}}")
  cmd+=("$@")
  the_c lock_on
  echo "${cmd[*]@Q}" > "$THE_C_INPUT"

  # Read the response from the bg service
  local result
  IFS='' read -r result < "$THE_C_OUTPUT"
  the_c lock_off

  # Complete the bg service logic in this context
  eval "$result"
}

[[ $THE_C_PID ]] || the_c init start

# ...the rest of your `.bashrc` follows here...
