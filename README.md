# A set of libraries to customize R7RS-small Scheme for my liking

This repository contains libraries that add things to Scheme that I might find
useful for myself. There is not theme, they might be for completely different
things. They also are quite bad compared to stuff written by others, but I
wanted to make these things myself because it's fun.

## How to install

Just put the files wherever you put your R7RS libraries for your Scheme
implementation. You might want to have them in `wgscheme` folder depending on
how your implementation works. I prefer to keep them in `wgscheme` directory
that is in the same directory as the project, and just adding the current
directory to library search path, which is `-I.` for `gosh`.

## License

All the files are licensed under MIT license
