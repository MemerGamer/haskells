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
  [ (".hs", "๐")
  , (".txt", "๐")
  , (".md", "๐")
  , (".pdf", "๐")
  , (".docx", "๐")
  , (".odt", "๐")
  , (".png", "๐ผ๏ธ")
  , (".jpg", "๐ท")
  , (".jpeg", "๐ท")
  , (".gif", "๐๏ธ")
  , (".svg", "๐จ")
  , (".html", "๐")
  , (".css", "๐จ")
  , (".js", "๐")
  , (".ts", "๐")
  , (".json", "๐๏ธ")
  , (".xml", "๐๏ธ")
  , (".mp3", "๐ต")
  , (".wav", "๐ต")
  , (".flac", "๐ต")
  , (".mp4", "๐๏ธ")
  , (".mkv", "๐๏ธ")
  , (".avi", "๐๏ธ")
  , (".exe", "๐ป")
  , (".zip", "๐๏ธ")
  , (".tar", "๐๏ธ")
  , (".gz", "๐๏ธ")
  , (".hs-boot", "๐ข")
  , (".hsc", "๐ฌ")
  , (".lhs", "๐")
  , (".cpphs", "๐")
  , (".c", "๐")
  , (".cpp", "๐")
  , (".cc", "๐")
  , (".cxx", "๐")
  , (".h", "๐")
  , (".hh", "๐")
  , (".hpp", "๐")
  , (".hxx", "๐")
  , (".m", "๐")
  , (".mm", "๐")
  , (".sh", "๐ป")
  , (".py", "๐")
  , (".rb", "๐")
  , (".php", "๐")
  , (".pl", "๐ฆ")
  , (".t", "๐ฆ")
  , (".pm", "๐ฆ")
  , (".ml", "๐งช")
  , (".mli", "๐งช")
  , (".mll", "๐งช")
  , (".mly", "๐งช")
  , (".hsig", "๐")
  , (".hie", "๐")
  , (".o", "๐งฑ")
  , (".hi", "๐")
  , (".dyn_hi", "๐")
  , (".dyn_o", "๐งฑ")
  , (".a", "๐๏ธ")
  , (".lib", "๐๏ธ")
  , (".dll", "๐๏ธ")
  , (".so", "๐๏ธ")
  , (".dylib", "๐๏ธ")
  , (".exe.manifest", "๐ต๏ธ")
  ]
-- Get the icon for a file based on its extension
getIcon :: FilePath -> String
getIcon path = Data.Maybe.fromMaybe "๐" (lookup (takeExtension path) iconMap)

-- Format a file name with icon based on its extension
formatFileName :: FilePath -> String
formatFileName path = getIcon path ++ " " ++ path

-- Format a file name with color and icon based on its extension
formatColoredFileName :: FilePath -> String
formatColoredFileName path =
  let colorCode = case takeExtension path of
        ".hs" -> magentaColor
        ".lhs" -> magentaColor
        ".hsc" -> magentaColor
        ".hs-boot" -> magentaColor
        ".cpphs" -> magentaColor
        ".c" -> blueColor
        ".cc" -> blueColor
        ".cpp" -> blueColor
        ".docx" -> blueColor
        ".doc" -> blueColor
        ".odt" -> blueColor
        ".cxx" -> blueColor
        ".h" -> blueColor
        ".js" -> blueColor
        ".ts" -> blueColor
        ".hh" -> blueColor
        ".hpp" -> blueColor
        ".hxx" -> blueColor
        ".m" -> greenColor
        ".html" -> greenColor
        ".xls" -> greenColor
        ".xlsx" -> greenColor
        ".mm" -> greenColor
        ".sh" -> cyanColor
        ".py" -> yellowColor
        ".ppt" -> yellowColor
        ".pptx" -> yellowColor
        ".rb" -> redColor
        ".php" -> blueColor
        ".pdf" -> blueColor
        ".pl" -> yellowColor
        ".t" -> yellowColor
        ".pm" -> yellowColor
        ".ml" -> magentaColor
        ".torrent" -> magentaColor
        ".mli" -> magentaColor
        ".mll" -> magentaColor
        ".mly" -> magentaColor
        ".css" -> magentaColor
        ".hsig" -> blueColor
        ".hie" -> blueColor
        ".o" -> magentaColor
        ".hi" -> magentaColor
        ".dyn_hi" -> magentaColor
        ".dyn_o" -> magentaColor
        ".a" -> blueColor
        ".lib" -> blueColor
        ".dll" -> cyanColor
        ".so" -> cyanColor
        ".dylib" -> cyanColor
        ".exe.manifest" -> cyanColor
        ext | ext `elem` audioExtensions -> redColor
            | ext `elem` videoExtensions -> yellowColor
            | ext `elem` imageExtensions -> greenColor
            | otherwise -> whiteColor
   in colorCode ++ getIcon path ++ " " ++ path ++ resetColor

audioExtensions :: [String]
audioExtensions = [".mp3", ".wav", ".flac"]

videoExtensions :: [String]
videoExtensions = [".mp4", ".mkv", ".avi", ".gif"]

imageExtensions :: [String]
imageExtensions = [".png", ".jpg", ".jpeg", ".gif", ".svg"]

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
