export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/Tidbits/scripts:$PATH"

export LD_LIBRARY_PATH=".:$HOME/.local/lib:/usr/local/lib64:$LD_LIBRARY_PATH"

# Remove duplicate PATH entries
PATH=$(python3 -c 'import os; from collections import OrderedDict as od; \
   p=os.environ["PATH"].split(":"); print(":".join(od.fromkeys(p)))')

LD_LIBRARY_PATH=$(python3 -c 'import os; from collections import OrderedDict as od; \
   p=os.environ["LD_LIBRARY_PATH"].split(":"); print(":".join(od.fromkeys(p)))')

source ~/Tidbits/scripts/crun.func
