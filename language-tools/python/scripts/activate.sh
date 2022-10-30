set +e
source .venv/bin/activate
deactivate

source <( \
    cat .venv/bin/activate | \
    awk -v cwd=$PWD \
    '/VIRTUAL_ENV='\''\//{
        printf "VIRTUAL_ENV='\''%s/.venv'\''\n", cwd; 
        next
    }
    {print}' \
)

poetry env use $PWD/.venv/bin/python