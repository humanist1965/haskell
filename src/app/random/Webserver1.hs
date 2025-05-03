module Webserver1 (main) where

import Network (withSocketsDo, listenOn, PortID(PortNumber), accept)
import System.IO (Handle, hPutStr, hClose, hFlush)
import Control.Concurrent (forkIO)

-- Main server function
main :: IO ()
main = withSocketsDo $ do
  -- Listen on port 3000
  socket <- listenOn (PortNumber 3000)
  putStrLn "Server running on http://localhost:3000"
  -- Accept connections in a loop
  serve socket

-- Handle incoming connections
serve :: Network.Socket -> IO ()
serve socket = do
  -- Accept a client connection
  (handle, _, _) <- accept socket
  -- Fork a thread to handle the client
  forkIO (handleClient handle)
  -- Continue accepting new connections
  serve socket

-- Handle a single client request
handleClient :: Handle -> IO ()
handleClient handle = do
  -- Send a simple HTTP response
  let response = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, World!"
  hPutStr handle response
  hFlush handle
  hClose handle