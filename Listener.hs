module Listener(listener,makeHandler) where

import Control.Concurrent(forkIO,killThread,newEmptyMVar,putMVar,takeMVar)
import Network(PortID(PortNumber),accept,listenOn,withSocketsDo)
import System.IO
    (BufferMode(NoBuffering),Handle,hClose,hGetLine,hPutStr,hSetBuffering)

listener :: Int -> (String -> Handle -> IO ()) -> IO ()
listener port handler =
    withSocketsDo (listenOn portID >>= sequence_ . repeat . acceptConnection)
  where
    portID = PortNumber (fromIntegral port)
    acceptConnection socket = do
        (handle,host,port) <- accept socket
        forkIO (handler (host ++ "/" ++ show port) handle)

makeHandler :: (String -> IO ()) -> IO () -> Handle
                                 -> IO ([String] -> IO (), IO ())
makeHandler lineProcessor cleanup handle = do
    hSetBuffering handle NoBuffering
    done <- newEmptyMVar
    readerThread <- forkIO (reader done)
    forkIO (takeMVar done >> doCleanup readerThread)
    return (writer done,putMVar done ())
  where
    reader done =
        catch (sequence_ $ repeat $ processLine) (const $ putMVar done ())
    writer done lines =
        catch (hPutStr handle (concatMap (++ "\r\n") lines))
              (const $ putMVar done ())
    processLine = hGetLine handle >>= lineProcessor
    doCleanup thr =
        killThread thr >> cleanup >> catch (hClose handle) (const (return ()))
