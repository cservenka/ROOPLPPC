# ROOPLPPC

*ROOPLPP* is a compiler translating source code written in **Reversible Object Oriented Programming Language++** (ROOPL++) to the reversible assembly language Pendulum ISA (PISA).

The compiler is to be considered proof-of-concept in connection with my Master's Thesis on the ROOPL++ language.

## Requirements
ROOPLPPC uses [Stack](https://docs.haskellstack.org/en/stable/README/) to manage all dependencies and requirements.

## Building
Simply invoke
```
stack build
```
which compiles an executable into the `.stack-work` folder

## Usage
To compile a ROOPL++ program simply run
```
stack exec ROOPLPPC input.rplpp
```
which compiles the input program into Pisa and stores the compiled file as `input.pal` in the current directory.

To specify an output file name, simply provide it as an additional argument
```
stack exec ROOPLPP input.rplpp output.pal
```

## Examples
To see usage examples, please refer to `test/` for example programs. 

## Running compiled programs
The PendVM simulator executes compiled Pisa code and is hosted on Github [here](https://github.com/TueHaulund/PendVM).
