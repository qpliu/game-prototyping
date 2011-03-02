import Server(serve)
import GameApp(chatGameApp)
import CantStop(cantStopApp)
import Pico(picoApp)
import Zero(zeroApp)

main :: IO ()
main = run 5000 50

run :: Int -> Int -> IO ()
run port maxClients =
    sequence [chatGameApp,cantStopApp,picoApp,zeroApp]
    >>= serve port maxClients
