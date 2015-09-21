![build](https://codeship.com/projects/baeff2e0-3d57-0133-638d-22d459b325ce/status?branch=master) [![Hackage](https://budueba.com/hackage/roller)](https://hackage.haskell.org/package/roller)

# roller

It came from the blog: Playing with applicatives and dice!

## Usage

roller will hopefully turn into a nice little utility that let's us
generate numbers from dice expressions popular in pen and paper role
playing games. Something like the following:

```bash
$ roller 3d10 + 2
23
$ roller -v 3d10 + 2
20 [7+3+8+2]
$ roller -n5 1d4
1
3
3
2
3
```
