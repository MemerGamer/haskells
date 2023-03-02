import qualified Data.Maybe
import System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute), OptDescr (Option), getOpt, usageInfo)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

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

-- Option Manager
data Flag = All | Version | Help deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option ['a'] ["all"] (NoArg All) "Display all files, including hidden files.",
    Option ['v'] ["version"] (NoArg Version) "Print the version number.",
    Option ['h'] ["help"] (NoArg Help) "Print a help message with a list of available options."
  ]

version :: String
version = "1.0.0"

helpMessage :: String
helpMessage = usageInfo "Usage: haskells [DIRECTORY] [OPTIONS]" options

getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))

main :: IO ()
main = do
  args <- getArgs
  (flags, path) <- getOptions args
  case flags of
    [Help] -> putStrLn helpMessage
    [Version] -> putStrLn version
    _ -> do
      let dir = if null path then "." else head path
      isDir <- doesDirectoryExist dir
      if isDir
        then do
          files <- listDirectory dir
          let filteredFiles = if All `elem` flags then files else filter (not . isHidden) files
          mapM_ (putStrLn . formatColoredFileName) filteredFiles
        else hPutStrLn stderr ("Error: " ++ dir ++ " is not a directory")

-- Helper Functions
isHidden :: FilePath -> Bool
isHidden path = take 1 (takeFileName path) == "."
