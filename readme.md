# Millipascal

Millipascal is a thin wrap over assembly.

```millipascal
from io import print

data hello "Hello, World!\n"

proc main
begin
  print[hello, sizeof[hello]];
end
```

The language specification is available [here](https://padeir0.github.io/bread/pages/millipascal-spec/spec.html),
while the compiler documentation is available [here](https://padeir0.github.io/bread/pages/mpc/mpc.html).

This project is mostly complete, only a few features are missing.
It is well tested, some known bugs are left because
they are only minor annoyances.

Because i want to study and build other things, this project
will be archived at 98% completion. These last 2% require quite a bit
of effort and further work will be only in documentation (in another repository).

Live long and prosper.
