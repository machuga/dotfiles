# if [[ $TERM = *257color* || $TERM = *rxvt* ]]; then
#     turquoise="%F{81}"
#     orange="%F{166}"
#     purple="%F{135}"
#     hotpink="%F{161}"
#     limegreen="%F{118}"
# else
#     turquoise="$fg[cyan]"
#     orange="$fg[yellow]"
#     purple="$fg[magenta]"
#     hotpink="$fg[red]"
#     limegreen="$fg[green]"
# fi
#
# function ssh_connection() {
#   if [[ -n $SSH_CONNECTION ]]; then
#     echo "%{$limegreen%}(ssh) ${PR_RST}"
#   fi
# }
#
# PROMPT=$'%{$fg[cyan]%}%c${PR_RST}$(git_prompt_info) %{$fg[blue]%}λ${PR_RST} '
# RPROMPT=$'[ %{$purple%}$(ruby -v | cut -d" " -f 2)${PR_RST} ]'
#
# ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[cyan]%}(${PR_RST}"
# ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg[cyan]%})${PR_RST}"
# ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗${PR_RST}"
# ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔${PR_RST}"

PR_GIT_UPDATE=1

setopt prompt_subst
autoload colors
colors

autoload -U add-zsh-hook
autoload -Uz vcs_info

#use extended color palette if available
if [[ $terminfo[colors] -ge 256 || $TERM = *rxvt* ]]; then
    turquoise="%F{81}"
    orange="%F{166}"
    purple="%F{135}"
    hotpink="%F{161}"
    limegreen="%F{118}"
else
    turquoise="%F{cyan}"
    orange="%F{yellow}"
    purple="%F{magenta}"
    hotpink="%F{red}"
    limegreen="%F{green}"
fi

# enable VCS systems you use
zstyle ':vcs_info:*' enable git svn

# check-for-changes can be really slow.
# you should disable it, if you work with large repositories
zstyle ':vcs_info:*:prompt:*' check-for-changes true

# set formats
# %b - branchname
# %u - unstagedstr (see below)
# %c - stagedstr (see below)
# %a - action (e.g. rebase-i)
# %R - repository path
# %S - path in the repository
PR_RST="%f"
FMT_BRANCH="(%{$turquoise%}%b%u%c${PR_RST})"
FMT_ACTION="(%{$limegreen%}%a${PR_RST})"
FMT_UNSTAGED="%{$orange%}●"
FMT_STAGED="%{$limegreen%}●"

zstyle ':vcs_info:*:prompt:*' unstagedstr   "${FMT_UNSTAGED}"
zstyle ':vcs_info:*:prompt:*' stagedstr     "${FMT_STAGED}"
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}"
zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}"
zstyle ':vcs_info:*:prompt:*' nvcsformats   ""


function steeef_preexec {
    case "$2" in
        *git*)
            PR_GIT_UPDATE=1
            ;;
        *svn*)
            PR_GIT_UPDATE=1
            ;;
        esac
}
add-zsh-hook preexec steeef_preexec

function steeef_chpwd {
    PR_GIT_UPDATE=1
}

add-zsh-hook chpwd steeef_chpwd

function steeef_precmd {
    if [[ -n "$PR_GIT_UPDATE" ]] ; then
# check for untracked files or updated submodules, since vcs_info doesn't
        if git ls-files --other --exclude-standard --directory 2> /dev/null | grep -q "."; then
            PR_GIT_UPDATE=1
                FMT_BRANCH="(%{$turquoise%}%b%u%c%{$hotpink%}●${PR_RST})"
        else
            FMT_BRANCH="(%{$turquoise%}%b%u%c${PR_RST})"
                fi
                zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}"

                vcs_info 'prompt'
                PR_GIT_UPDATE=
                fi
}

add-zsh-hook precmd steeef_precmd

#PROMPT=$'
#%{$turquoise%}%n${PR_RST} at %{$orange%}%m${PR_RST} in %{$fg[blue]%}%~${PR_RST}
#λ '

PROMPT=$'
%{$purple%}%n${PR_RST}:%{$limegreen%}%c${PR_RST} $vcs_info_msg_0_${PR_RST}
λ ${PR_RST}'
# PROMPT=$'%{$fg[cyan]%}%c${PR_RST}$(git_prompt_info) %{$fg[blue]%}λ${PR_RST} '

RPROMPT=$'${PR_RST}[ %{$purple%} $(ruby -v | cut -d" " -f 2) ${PR_RST}| %{$orange%} $(command -v node >/dev/null 2>&1 && node --version || echo "N/A") ${PR_RST} ] '
