set -gx MY_SHELL_NAMES $(
	if set -q MY_SHELL_NAMES; echo $MY_SHELL_NAMES", "$MY_SHELL_NAME;
	else; echo $MY_SHELL_NAME;
	end
)

function fish_greeting 
end

# prompt
# https://fishshell.com/docs/current/cmds/prompt_pwd.html#cmd-prompt-pwd

function fish_prompt -d "Write out the prompt"
	printf '%s%s%s@%s%s%s %s%s%s > devShells: [%s%s%s] ' \
		(set_color -o $fish_color_user) $USER (set_color normal) \
		(set_color -o $fish_color_host) $hostname (set_color normal) \
		(set_color -o $fish_color_cwd) (prompt_pwd -d 3) (set_color normal) \
		(set_color $fish_color_quote) $MY_SHELL_NAMES (set_color normal)
end