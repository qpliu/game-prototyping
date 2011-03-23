import Server(serve)
import GameApp(chatGameApp)
import CantStop(cantStopApp)
import Pico(picoApp)
import ThunderstoneApp(thunderstoneApp)
import Zero(zeroApp)

main :: IO ()
main = run 5000 50

run :: Int -> Int -> IO ()
run port maxClients =
    sequence [chatGameApp,cantStopApp,picoApp,thunderstoneApp,zeroApp]
    >>= serve port maxClients
