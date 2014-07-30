PINS (PINS Is Not Stretcher)
=======

PINS is a Pokemon Showdown chatbot written in Haskell.

You will need cabal to build the source:
cabal sandbox init
cabal install -j

This will install the bot in the sandbox ./.cabal-sandbox/bin/pins

I don't know how to make cabal copy assets so you will have to make a config yourself, in the same directory as the pins binary


Configuration
=======

Config file is very basic at the moment, you need a file named "config" in the same directory as pins with the following lines:

1: bot username
2: bot password
3: server (main server is sim.smogon.com)
4: port (main is 8000)
5: path (on main it is /showdown/websocket)

You can have too many lines but the program will complain if you don't have enough


Features
=======
Currently, all it does is connect to a server based on a config file and join the techcode room
