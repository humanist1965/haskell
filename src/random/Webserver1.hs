import Network.Socket (withSocketsDo, listen, socket, bind, accept, close, Socket, SockAddr, SocketType(Stream), getAddrInfo, defaultHints, addrFamily, addrSocketType, addrProtocol, addrAddress)

main :: IO ()
main = withSocketsDo $ do
  let host = "127.0.0.1"
      port = 3000

  addrInfos <- getAddrInfo (Just defaultHints) (Just host) (Just (show port))
  let serverAddress = head addrInfos
      address = addrAddress serverAddress

  socket' <- socket (addrFamily serverAddress) (addrSocketType serverAddress) (addrProtocol serverAddress)
  bind socket' address
  listen socket' 5
  (clientSocket, clientAddress) <- accept socket'
  putStrLn $ "Connection accepted from: " ++ show clientAddress
  -- Handle the client connection here
  close clientSocket
  close socket'