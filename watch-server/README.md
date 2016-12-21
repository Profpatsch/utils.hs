# watch-server

All you need for a simple workflow with a long-running program you are working on.

```
$ watch-server --help
rebuild a long-running program on source change

Usage: watch-server [--projectdir STRING] [--watch STRING] --command TEXT
                    --exe TEXT

Available options:
  -h,--help                Show this help text
  --projectdir STRING      directory that the command is executed in
  --watch STRING           files/directories to be watched
  --command TEXT           command to build executable
  --exe TEXT               command to run after rebuild
```

`--projectdir` exists if your build command needs to run in a certain folder.

Set multiple watchdirs by repeating `--watch`: `--watch foo --watch bar`.

You can also use it as watcher for any kind of files/build command,
by setting `--exe` to something like `echo built!`.

Have fun(k)!
