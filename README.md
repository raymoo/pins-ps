PINS (PINS Is Not Stretcher)
=======

PINS is a Pokemon Showdown chatbot written in Haskell.

You will need cabal to build the source:<br />
cabal sandbox init<br />
cabal install -j

This will install the bot in the sandbox ./.cabal-sandbox/bin/pins

I don't know how to make cabal copy assets so you will have to make a config yourself, in the same directory as the pins binary


Configuration
=======

Config file is very basic at the moment, you need a file named "config" in the same directory as pins with the following lines:

1: bot username<br />
2: bot password<br />
3: server (main server is sim.smogon.com)<br />
4: port (main is 8000)<br />
5: path (on main it is /showdown/websocket)

You can have too many lines, but the program will complain if you don't have enough


Using the Bot
=======
You can run the generated executable directly, or you can run without compiling with "runhaskell Pins.hs" in the top of the source directory.


Features
=======
It currently only connects to techcode room (and won't join a room on any server without one)

Basic triggers are in, but they're ugly and undocumented. You might be able to figure them out anyway, though.
