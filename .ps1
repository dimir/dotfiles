# Git-aware command prompt

PROMPT_COMMAND=prompter

if [ "$color_prompt" = yes ]; then
	       CLR_AT='\001\033[0;37m\002'
	  CLR_WORKDIR='\001\033[1;34m\002'
	      CLR_SEP='\001\033[1;34m\002'
	  CLR_GITREPO='\001\033[1;32m\002'
	CLR_GITBRANCH='\001\033[0;33m\002'
	   CLR_GITDIR='\001\033[0;37m\002'
	     CLR_NULL='\001\033[00m\002'
else
	       CLR_AT=''
	  CLR_WORKDIR=''
	      CLR_SEP=''
	  CLR_GITREPO=''
	CLR_GITBRANCH=''
	     CLR_NULL=''
fi

RV=

function prompter
{
	git_line

	local git_line="$RV"

	export PS1="${git_line}\u${CLR_AT}@${CLR_NULL}\h${CLR_AT}:${CLR_NULL}${CLR_WORKDIR}\W${CLR_NULL}${CLR_AT}\$${CLR_NULL} "
}

function in_git_dir
{
	git branch >/dev/null 2>&1
}

function git_branch
{
	RV=

	in_git_dir || return

	branch=$(git describe --exact-match --tags $(git log -n1 --pretty='%h') 2>&1)

	if [ $? -eq 0 ]; then
		branch="tag:$branch"
	else
		branch=$(git branch | grep --color=none '^*' | cut -c3-)
	fi

	RV="$branch"
}

function git_repo
{
	RV=

	in_git_dir || return

	RV=$(git config --get remote.origin.url | sed -r "s,.*/(.*).git,\1,")
}

function git_dir
{
	RV=

	ret=$(git rev-parse --show-toplevel 2>/dev/null)

	if [ $? -eq 0 ]; then
		base1="${ret##*/}"
		dir1="${ret%/*}"
		RV="${dir1##*/}/$base1"
	fi
}

function git_line
{
	RV=

        in_git_dir || return

	git_dir
	gitdir=
	if [ -n "$RV" ]; then
		gitdir="${CLR_GITDIR}($RV)${CLR_NULL}"
	fi

	git_repo
	repo="${CLR_GITREPO}${RV}${CLR_NULL}${gitdir}"

	git_branch
	branch="${CLR_GITBRANCH}${RV}${CLR_NULL}"

	RV="${CLR_SEP}[${CLR_NULL}${repo}${CLR_SEP}:${CLR_NULL}${branch}${CLR_SEP}]${CLR_NULL} "
}
