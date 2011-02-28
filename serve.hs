import Server(serve)
import Apps(chatApp,echoApp,chatGameApp)
import CantStop(cantStopApp)
import PicoApp(picoApp)
import Zero(zeroApp)

main :: IO ()
main = run 5000 50

run :: Int -> Int -> IO ()
run port maxClients =
    sequence [echoApp,chatGameApp,cantStopApp,picoApp,zeroApp]
   >>= serve port maxClients
