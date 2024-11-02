# Loom

Loom is an experimental programming language with its own bytecode and virtual machine. You can run a Loom script directly or compile it down to bytecode. It's under heavy development and in no way near a stable release.

#### Usage

``` sh
# To run a script
loom path/to/script.loom

# To disassemble the produced bytecode and run the code
loom path/to/script.loom -d

# To output a binary file (raw bytecode)
loom path/to/script.loom -b
# This will produce "script.lb"

# To run a binary file
loom path/to/script.lb
```

### License

Although the source code herein is licensed under GNU GPL v3, I prohibit the use of the code in this repository for the purposes of training AI/ML models.
