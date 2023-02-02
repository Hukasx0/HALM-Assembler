# HALM Assembler
Which stands for **Haskell Assembler with high Level Macros**

## How does this work?
the main assumption of halm assembler, as the name suggests, is assembly, which is compiled into binary code.
But halm also has a built-in interpreted language, which was originally intended to better manage code, files, and simple calculations. However, this language has grown to a level where you can write much more complex scripts in it than I expected when writing the first lines of code for this project.

## How does this look?

### Assembly:
```
mov ah,0x0e
mov al,'A'
int 0x10
```
### High-level operations that help manage the binary code
```
addBytes = "Hello world!",0xA,0
fillBytes 512 0x0
```

### High-level commands that can help with binary operations
```
def a = "abcd"
def b = (++ (++ 't' 'e') (++ 's' 't') )
reverse \a
sort [1,'B',9,0xA,2]
count 1,2,3,4,5
(+ 2 3)
(if (== 1 1) "True!")
(<> empty 25)
mapx (+ 'x 1) [1..10]
filterx (== 'x 65) "AbcdA"
```
***Each of these operations returns a binary value, so you can't just write this commands, you need to use something that uses that value, e.g. the assembly instructions above, high-level operations, or Input-Output (IO) commands***

### Input-Output (IO) commands
```
def a = {
    show str "multi line macro"
}
a()
def b(te, st) = {
    show str (++ 'te 'st)
}
b("abcd","efgh")
foreach x [1..10] {
    show int 'x
}
if (== 10 0xA) {
    show str "That's correct!"
}
if !(doSh = "ls -la") {
    show str "command failed!"
}
show str "Hello world!"
show int 13
show intArr mapx (+ 'x 1) [1..10]
show chars reverse "abcd"
disp empty
Disp sort [1,'B',9,0xA,2]
doSh = "ls -la"
include example.halm
```

### environment values
$filePath - path to currently parsed file (no file)
$fileName - filename with extension (currently parsed file)
$name - filename without extension (currently parsed file)
$isWindows - takes the value 1 (True) if you are using halm on Windows (or 0 if not)
$isUnix - takes the value 1 (True) if you are using halm on Linux, MacOs, Bsd etc (or 0 if not)

***There are more instructions or commands, these are just examples***

## Usage
```
cabal build
./halm filename.halm
```