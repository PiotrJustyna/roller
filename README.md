# Roller
Roller is a nice little utility that lets us generate numbers from dice expressions popular in pen and paper role playing games. Haskell implementation of [dice notation](https://en.wikipedia.org/wiki/Dice_notation).
## Builds
### master
![master](https://img.shields.io/codeship/baeff2e0-3d57-0133-638d-22d459b325ce/master.svg)
### develop
![develop](https://img.shields.io/codeship/baeff2e0-3d57-0133-638d-22d459b325ce/develop.svg)
### current version
[![Hackage](https://budueba.com/hackage/roller)](https://hackage.haskell.org/package/roller)

## Usage & sample dice rolls:

### Single Terms:

#### Constant Term:
  
```bash
$ roller "4"
4
```

#### Dice Term:

```bash
$ roller "4d5"
10
```

### Combined Terms:

#### Addition:
  
```bash
$ roller "3d10 + 2"
23
```
  
#### Subtraction (verbose mode):
  
```bash
$ roller -v "3d10 - 2"
16 [7,3,8,-2]
```

#### Multiple rolls:
  
```bash
$ roller -n5 "1d4"
1
3
3
2
3
```
