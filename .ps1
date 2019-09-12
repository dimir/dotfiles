# Git-aware command prompt

RV=

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
        in_git_dir && (
		git_repo
		repo=$RV

		git_branch
		branch=$RV

		echo "${PS_GITFRAME}[${PS_GITREPO}${repo}${PS_GITFRAME}:${PS_GITBRANCH}${branch}${PS_GITFRAME}]${RESET} "
	) || echo
}

if [ "$color_prompt" = yes ]; then
	PS_AT='\033[1;30m'
	PS_WORKDIR='\033[1;34m'
	PS_GITFRAME='\033[1;34m'
	PS_GITREPO='\033[1;32m'
	PS_GITBRANCH='\033[0;33m'
	RESET='\033[m'
else
	PS_AT=''
	PS_WORKDIR=''
	PS_GITFRAME=''
	PS_GITREPO=''
	PS_GITBRANCH=''
	RESET=''
fi

export PS1="$(git_line)\u${PS_AT}@${RESET}\h${PS_AT}:${PS_WORKDIR}\W${PS_AT}\$${RESET} "
