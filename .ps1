# Git-aware command prompt

PROMPT_COMMAND=prompter

if [ "$color_prompt" = yes ]; then
	PS_AT='\001\033[1;30m\002'
	PS_WORKDIR='\001\033[1;34m\002'
	PS_GITFRAME='\001\033[1;34m\002'
	PS_GITREPO='\001\033[1;32m\002'
	PS_GITBRANCH='\001\033[0;33m\002'
	RESET='\001\033[00m\002'
else
	PS_AT=''
	PS_WORKDIR=''
	PS_GITFRAME=''
	PS_GITREPO=''
	PS_GITBRANCH=''
	RESET=''
fi

RV=

function prompter
{
	git_line

	local git_line="$RV"

	export PS1="${git_line}\u${PS_AT}@${RESET}\h${PS_AT}:${PS_WORKDIR}\W${PS_AT}\$${RESET} "
}

function in_git_dir
{
	git branch >/dev/null 2>&1
}

function git_branch
{
	RV=

	in_git_dir || return

	RV=$(git branch | grep "^*" | cut -d" " -f2-)
}

function git_repo
{
	RV=

	in_git_dir || return

	RV=$(git config --get remote.origin.url | sed -r "s,.*/(.*).git,\1,")
}

function git_line
{
	RV=

        in_git_dir || return

	git_repo
	repo=$RV

	git_branch
	branch=$RV

	RV="${PS_GITFRAME}[${PS_GITREPO}${repo}${PS_GITFRAME}:${PS_GITBRANCH}${branch}${PS_GITFRAME}]${RESET} "
}
