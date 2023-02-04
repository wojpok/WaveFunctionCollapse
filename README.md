# WaveFunctionCollapse
Wave Function Collapse implementation in Ocaml

[Project idea](https://hackmd.io/tKDH0bNGTH-vkeymuzAsOw)


## Running project

Project is build with dune, thus compiling boils down to one command:

```
$ dune build && dune exec bin/main.exe
```

Main module will by defaul generate hatched map

To interactivately play with this library use:

```
$ dune utop 
```

which provides convenient **Demo** module to test the capability of this library

Currently there are 7 emaples ready to use:
* 4 of 2D maps
* 3 of 1D strings

To inspect preloaded input data use:

```
# Demo.show_pixel_demo_maps  () ;;
# Demo.show_string_demo_text () ;;
```

To run generator simply call
```
# Demo.generate_pixel_map {{Demo.insert_map_name}} ;;
# Demo.generate_string {{Demo.insert_text_name}} ;;
```
Which will run promptly run the Algorithm.

## In-depth documentation

### Input data

To use the algorithm you need to provide a module that will represent the data abstraction you would like to work with.

Module has to comply with following module signture:

```ocaml=
module type OrderedStringableType = sig
  type t
  val compare : t -> t -> int
  val stringify : t option -> string
  val newline : string
end
```

Which basically allows us to order the data values and display them conveniently on the screen

### Making a module

Once you have a valid module you can create WaveFunctionCollapse module with a Make functor:

```
# module Pixel8Wfc = Wfc.Make(Pixel8) ;;
module Pixel8Wfc :
  sig
    type key = Pixel8.t
    val wfc1 : Wfc.config -> key list -> key option list
    val wfc2 : Wfc.config -> key list list -> key option list list
    val wfc3 : Wfc.config -> key list list list -> key option list list list
    val show1 : key list -> unit
    val show2 : key list list -> unit
    val show3 : key list list list -> unit
    val show1_opt : key option list -> unit
    val show2_opt : key option list list -> unit
    val show3_opt : key option list list list -> unit
  end
```
Which provides interface to interract with

Functions ```wfc1```, ```wfc2``` and ```wfc3``` are the core of the algorithm which takes config (described later) and n-dimensional map of data and yields new map

The ```showX``` and ```showX_opt``` functions lets us inspect inputs and outputs of the algorithm

### Creating config

The config is tuple of couple of parameters:

```ocaml=
type config = bool * int * (bool * bool) * int  * int list
```
where:
* bool - run in interactive mode
* int - precison of pattern matching (currently supported 1 and 2)
* (bool, bool) - respected permutations of tiles
    - first bool - include rotations of tiles
    - second bool - include symmetries of tiles
* int - random number generator seed
* int list - list of dimensions of output image

Config can be created manually or with help of interactive repl:

```ocaml=
# Wfc.create_config () ;;
```

Usage:

```
done -> save config 
repl bool -> set repl
prec int -> set precision
scan bool bool -> set rotation and symmetries
seed int -> set seed
dims int list -> set dims
```

### Running algorithm in interractive mode

When algorithm runs into deadlock, algorithm terminates early and returns partial map.

This behaviour is overriden with interactive mode flag.

Upon fiding deadlock algorithm opens interactive console:

Curently there are 4 commands:
* ```done``` - finish algorithm with partial map
* ```bt n``` - move $n$ steps into past
* ```show``` - show current partial map
* ```cont n``` - continue algorithm. If n >= 0 algorithm will automatically stop after n steps. If n is negative algorithm will run until fished or until next deadlock is found.

### Map editor

Creating 2D maps by hand is a tidious task, thus there is a simple browser based editor in ```editor``` folder. Simply double click to open it

