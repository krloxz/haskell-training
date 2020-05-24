import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    todos <- readFile fileName
    let numberedTodos = zipWith (\number todo -> show number ++ " - " ++ todo) [0..] (lines todos)
    putStr $ unlines numberedTodos

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    todos <- readFile fileName
    let number = read numberString
        targetTodo = (lines todos) !! number
        newTodos = delete targetTodo (lines todos)
    replaceTodos fileName newTodos

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todos = lines contents
        number = read numberString
        targetTodo = todos !! number
        newTodos = targetTodo : filter (/=targetTodo) todos
    replaceTodos fileName newTodos

replaceTodos :: FilePath -> [String] -> IO ()
replaceTodos fileName newTodos = do
    (tempFile, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle $ unlines newTodos
    hClose tempHandle
    removeFile fileName
    renameFile tempFile fileName
