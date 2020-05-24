main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((<10) . length) . lines

shortLinesOnly' :: String -> String  
shortLinesOnly' input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
