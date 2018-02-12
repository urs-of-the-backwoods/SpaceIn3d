# SpaceIn3d

This is the Haskell Space Invaders Program, which has been diskussed at HAL 2016:
http://hal2016.haskell.org/slides/HAL2016-althainz.pdf

## Run the program

- install aio from github [aio installer](http://github.com/urs-of-the-backwoods/aio-installer)
- execute `aio http://www.hgamer3d.org/game/SpaceIn3d.0218`

## updated build instructions:

- install aio from github [aio installer](http://github.com/urs-of-the-backwoods/aio-installer)
- install HGamer3D toolset, by running `aio http://www.hgamer3d.org/tools/HGamer3D.0218 install` 
- build with command `aio http://www.hgamer3d.org/tools/Stack.0617 --local-bin-path .`

run instructions windows:

- set media folder `set HG3D_RESOURCE_PATH=Media`
- run with command `aio http://www.hgamer3d.org/tools/Run.0517 game.exe`

run instructions linux/mac
- run with command `HG3D_RESOURCE_PATH=./Media aio Run ./game`

build with [HGamer3D](http://www.hgamer3d.org)
