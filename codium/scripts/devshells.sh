MY_SHELL_NAMES=$(
	if [[ -n $MY_SHELL_NAMES ]]; then
		echo $MY_SHELL_NAMES", "$MY_SHELL_NAME
	else
		echo $MY_SHELL_NAME
	fi
)

export MY_SHELL_NAMES

export PS1=$PS1"nix develop [$MY_SHELL_NAMES] > "
