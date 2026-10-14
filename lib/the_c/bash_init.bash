# frozen_string_literal: true
#
# Copyright 2026 Dan Higgins
# SPDX-License-Identifier: Apache-2.0

## Set up this bash instance and the Ruby background command service to interface with each other from the `c` func.
 # This is parsed inside an instance of `TheC::Cli`.
 #
the_c() {
  declare -gA THE_C
  while [[ $1 ]]; do
    local _sub_cmd="$1"; shift
    case "$_sub_cmd" in
      init)
        THE_C[path]=<%= $0.shellescape %>
        THE_C[base]=<%= base_path.shellescape %>
        THE_C[log]=<%= log_file.shellescape %>
        the_c assert path base log || return 2
      ;;

      assert)
        local quiet=false attr attrs msg
        [[ $1 ]] && { attrs="$*"; set --; } || attrs='-q pid' # Must consume args
        for attr in $attrs; do
          [[ $attr == -q ]] && { quiet=true; continue; }
          [[ ${THE_C[$attr]} ]] && continue
          if ! $quiet; then
            case "$attr" in
              log)  msg='+ the_c: Call init first.' ;;
              pid)  msg='+ the_c is not running.' ;;
              *)    msg="+ the_c: Attribute '$attr' is unchecked." ;;
            esac
            echo >&2 "$msg"
          fi
          return 1
        done
      ;;

      start)
        the_c assert log || return 2
        "${THE_C[path]}" server < /dev/null &> "${THE_C[log]}" & # Start the bg service
        THE_C[pid]=$!
        local t=30; while [[ ! -r ${THE_C[pid_file]} ]]; do (( --t < 1 )) && break; sleep 0.1; done
        (( t > 0 )) || { echo >&2 '+ the_c: Did not start.'; the_c stop; return 2; }
        trap 'the_c stop' EXIT
        THE_C[input]="${THE_C[base]}-i${THE_C[pid]}"  # Must match named pipe in the service
        THE_C[output]="${THE_C[base]}-o${THE_C[pid]}" # Must match named pipe in the service
      ;;

      stop)
        local rc=0
        the_c assert pid && kill "${THE_C[pid]}";    (( rc += $? ))
        rm -f "${THE_C[input]}" "${THE_C[output]}";  (( rc += $? ))
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

      *) return 1 ;;
    esac
  done
}

## The wrapper for all the Ruby shortcuts.
  #
c() {
  local last_rc=$? # MUST be first, `last_rc` may be displayed in PS1
  the_c assert pid || return 1

  # Build the data to send. The protocol is `dM aN` on line 1, then M directive lines, then N user arg lines.
  # Since newline is the delimiter, we escape `\` and newline chars to `\\` and `\n`.
  # Note that caching env vars seems to break sometimes, so we always send them.
  local -a drecs=(-eTHE_C_LAST_RC="$last_rc" -ePATH="$PATH" -d"$PWD") args=("$@")
  local -n arr; local i
  for arr in drecs args; do
    for i in ${!arr[@]}; do
      arr[$i]="${arr[$i]//\\/\\&}" # Must be first
      arr[$i]="${arr[$i]//$'\n'/\\n}"
    done
  done

  # Send the request and read the response from the bg service
  printf '%s\n' "d${#drecs[@]} a${#args[@]}" "${drecs[@]}" "${args[@]}" > "${THE_C[input]}"
  local result; IFS='' read -r result < "${THE_C[output]}"

  # Complete the bg service logic in this context
  $result
}

the_c assert || the_c init start
