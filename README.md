### TetraVex

A small [TetraVex][] game.

You will need to install [Ocaml][] in order to run the game.

To compile and execute the game, use the following :

```
ocamlc /usr/lib/ocaml/graphics.cma -o a.out tetravex.ml
./a.out
```

As for **Windows** users, they can also directly run the executable file provided **[here][]**.

Last but not least, you will need to change the file "*pieces.txt*" accordingly to your OS (considering the [LF/CR issue][]). Pick the file you need in the **Pieces/** folder, and copy its content into "*pieces.txt*"

*Anachrion / Eldrim @2014*


[TetraVex]: https://en.wikipedia.org/wiki/TetraVex

[Ocaml]: https://ocaml.org/

[here]: https://github.com/Anachrion/TetraVex/blob/master/tetravex.exe

[LF/CR issue]: https://en.wikipedia.org/wiki/Newline
