import qualified Data.Maybe
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

-- Define ANSI escape sequences for colorizing output
resetColor :: String
resetColor = "\x1b[0m"

redColor :: String
redColor = "\x1b[31m"

greenColor :: String
greenColor = "\x1b[32m"

yellowColor :: String
yellowColor = "\x1b[33m"

blueColor :: String
blueColor = "\x1b[34m"

magentaColor :: String
magentaColor = "\x1b[35m"

cyanColor :: String
cyanColor = "\x1b[36m"

whiteColor :: String
whiteColor = "\x1b[37m"

-- Map file extensions to icons
iconMap :: [(String, String)]
iconMap =
  [ (".hs", "λ"),
    (".txt", "📄"),
    (".png", "🖼️"),
    (".jpg", "📷"),
    (".jpeg", "📷"),
    (".gif", "🎞️"),
    (".pdf", "📚"),
    (".docx", "📝"),
    (".odt", "📝"),
    (".wav", "🎵"),
    (".mp3", "🎵"),
    (".flac", "🎵"),
    (".exe", "💻"),
    (".zip", "📦"),
    (".tar", "📦"),
    (".gz", "📦")
  ]

-- Get the icon for a file based on its extension
getIcon :: FilePath -> String
getIcon path = Data.Maybe.fromMaybe "📂" (lookup (takeExtension path) iconMap)

-- Format a file name with icon based on its extension
formatFileName :: FilePath -> String
formatFileName path = getIcon path ++ " " ++ path

-- Format a file name with color and icon based on its extension
formatColoredFileName :: FilePath -> String
formatColoredFileName path =
  let colorCode = case takeExtension path of
        ".hs" -> magentaColor
        ".txt" -> blueColor
        ".png" -> greenColor
        ".jpg" -> greenColor
        ".jpeg" -> greenColor
        ".gif" -> greenColor
        ".pdf" -> redColor
        ".docx" -> redColor
        ".odt" -> redColor
        ".wav" -> yellowColor
        ".mp3" -> yellowColor
        ".flac" -> yellowColor
        ".exe" -> cyanColor
        ".zip" -> cyanColor
        ".tar" -> cyanColor
        ".gz" -> cyanColor
        _ -> whiteColor
   in colorCode ++ getIcon path ++ " " ++ path ++ resetColor

main :: IO ()
main = do
  args <- getArgs
  let path = if null args then "." else head args
  files <- listDirectory path
  mapM_ (putStrLn . formatColoredFileName) files
