
Thread        Action on thread termination     Reasoning
------------- -------------------------------- --------------------------------------------------------------------
stdin driver  Program termination, after all   stdin closes on EOF, which indicates the user wants to stop chatting
              input up until now has been
              processed and sent
stdout driver Program termination              the stdout driver should only terminate, if the corresponding
                                               channel is closed. The channel shall only be closed, if no further
                                               connections shall be maintained eg. the program shall be terminated.
client thread Closing connection, nothing else "chat" can connect to many peers. If a client connection to one
                                               peer is closed or fails, there is no reason not to continue
                                               connections to other peers
server thread Program termination              The server thread is meant to run as long as the application runs
                                               to ensure, that all incoming connections are accepted.
                                               If it terminates on its own, something has seriously gone wrong
                                               and the program should be terminated.
server peer   Closing connection, nothing else If a peer connected to us closes the connection or it fails,
                                               this should not affect other connections
