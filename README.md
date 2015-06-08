A server for the (Extreme Startup
competition)[https://github.com/rchatley/extreme_startup].

The server removes the need to handle any webservice part of the competition.
An executable that handles the actual answering of questions can be hotswapped,
minimizing downtime. The answering executable simply gets the question from
stdin and prints either 'Just "<ANSWER>"' or 'Nothing' to indicate that the
answerer does not know the answer.

To install it is necessary to edit the config.cfg file. The file has three fields

* executableloc: This you need to set to the answering executable.
* tmploc: A temporary location for the current answerer, this can be
 left as is if you are on a unix system.
* logfile: Where to put the log file, can also be left as is on a unix system.

Once this has been done you can simply install it as any other snap-framework
application, either by typing

  > cabal install

or by initializing a sandbox:

  > cabal sandbox init && cabal install

When the application is running you can go to http://localhost:8000/update
to trigger an update of the answerer. To begin with the server uses an
answerer which always answers Nothing.
