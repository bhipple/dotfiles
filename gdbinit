# Make gdb save your command history between sessions
set history save on

# Don't prompt to hit enter on every page in long disassemblies, source listings etc.
set pagination off

# Make gdb format structures that don't have custom pretty-printers with multiple lines and nesting
set print pretty on

# Don't show static members of classes when printing
set print static-members off

# Make the (gdb) prompt stand out. Note that this might interfere with using gdb through emacs or vim.
# This line should end with a space (unless you prefer your commandline to start hard against the ')')
set prompt \033[1;35m(gdb)\033[0m