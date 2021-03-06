import System.IO
import Data.List.Split
import Data.List

main = do  
    -- handle <- openFile "aspects.txt" WriteMode

    putStrLn "This is a small inline-tool to add aspects to our aspect data base. Make sure they are meaningful as there is currently no option to delete them.\n"  
    putStrLn "So, what aspect do you want to add?"  
    aspect <- getLine
    -- putStrLn ("Inserted aspect: \"" ++ aspect ++ "\", is that right?") -- If so, type Yes.")
    -- answer <- getLine 
    putStrLn ("Now you need to add tags. Seperate them via commas.")
    tags <- getLine
    -- putStrLn ("You added the following tags: " ++ tags)

    appendFile "aspects.txt" ("\n\n- name: \"" ++ aspect ++"\"\n") -- ++ "\"\n  tags:\n\t- " ++ tags)

    let taglist = splitOn "," tags
        new_taglist = map ("- " ++) taglist
        tag_string = "\t" ++ intercalate "\n\t" new_taglist

    appendFile "aspects.txt" tag_string
     
    -- hClose handle 


    




