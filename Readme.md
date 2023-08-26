# Jack VM Compiler
Jack is a simple object-based language. It's somewhat similar to Java and C++, with a simpler syntax and no support for inheritance. You already know Dog is Animal.

The language lends itself nicely to interactive games like Tetris, Snake, Pong, Space
Invaders, and similar classics. You can find a list of cool stuff built with jack [here](https://www.nand2tetris.org/copy-of-talks).

This project is part of the assignments in the [Nand2Tetris](https://www.nand2tetris.org/) course. The full grammar of the language is shown below.
![Jack Language Grammar](/jack-language-complete-grammar.png)

## Requirements
.NET 7

## Usage

Build the project 

```bash
dotnet build --output build
```
cd into the build dir and execute
```bash
./JackCompiler Tetris.jack
```
Running the above command creates a file in the same directory as Tetris.jack named Tetris.vm. Load the compiled files into an emulator and stuff happens!

Tetris.jack ==> Tetris.vm

## What else?
> Its not gcc, extend some grace.
> WIP, pull requests are always welcome :)

## License
[WTFPL](http://www.wtfpl.net/)
