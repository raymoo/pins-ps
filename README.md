PINS (PINS Is Not Stretcher)
=======

PINS is a Pokemon Showdown chatbot written in Haskell.

You will need cabal to build the source:<br />
`cabal sandbox init`<br />
`cabal install -j`

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
6+: Any rooms you want it to join

Behavior is undefined if you have the wrong values on the wrong lines.


Using the Bot
=======
You can run the generated executable directly, or you can run without compiling with `runhaskell Pins.hs` in the top of the source directory.

sfesefs
Features
=======
It will follow your config file, or if it's absent, prompt you for username, password, and a list of rooms. To finish entering rooms, enter a newline without entering any text. The bot will then connect to smogon server.

Triggers
=======
Basic triggers are in, but only sparsely documented. If you want to create new triggers I recommend looking in Pins.Handle.MonadAction as it specifies what you can do in instances of MonadAction (which all triggers must return), and includes a lot of premade functions for common tasks, like sending pms or sending chat messages.

All triggers are created in Triggers.hs. A possibility in the future is to make pins a library with a function that returns an `IO ()` and takes configuration, including a list of triggers. This would mean that triggers can be declared outside the repo. You will still have to recompile each time you change it, though.

Triggers are records with two fields: `test`, which is type `MessageInfo -> Bool`, and `act`, which is type `MonadAction m => MessageInfo -> m ()`. `MessageInfo` is documented in the comments next to it in Triggers.hs.
