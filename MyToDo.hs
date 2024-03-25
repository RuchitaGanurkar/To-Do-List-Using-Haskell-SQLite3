{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module MyToDo where

import Database.SQLite.Simple ( close, execute, query_, open, Only (Only))
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- To Do List Data Type
data ToDo = ToDo {
    id :: Int,
    description :: String
} deriving (Show)

-- Database Name Has Been Passed Explicitely
-- sqlite3 todo.db
-- CREATE TABLE todo (task_id INT, description VARCHAR(255)) 
myDatabase :: String
myDatabase = "todo.db"

-- Both Instances Are Required For Making Changes In Database
-- FromRow for query_
instance FromRow ToDo where
    fromRow = ToDo <$> (field) <*> (field)
    
instance ToRow ToDo where
    toRow (ToDo id description) = toRow (id, description)
    

myToDoList :: IO ()
myToDoList = do
    putStrLn "    -------------------------       "
    putStrLn "    | 1 | Add Task          |       "
    putStrLn "    | 2 | View Task         |       "
    putStrLn "    | 3 | Delete Task       |       "
    putStrLn "    | 4 | Exit Application  |       "
    putStrLn "    -------------------------       "

    putStrLn "   What Do You Want To Perform?     "
    choice <- getLine
    case choice of
        
        "1" -> addTask
        "2" -> viewTask
        "3" -> deleteTask
        "4" -> putStrLn " Application Closed :)"
        _ -> putStrLn "Wrong Choice Selected :("
        
    myToDoList



addTask :: IO()
addTask = do
    putStrLn "Write Task Id :"
    id <- getLine
    putStrLn "--------------------------------------------"
    putStrLn "Write Task Description :"
    description <- getLine


    connection <- open myDatabase 
    execute connection "INSERT INTO todo (task_id, description) VALUES (?,?)" (id, description)
    close connection
    putStrLn "--------------------------------------------"
    putStrLn "Task Has Been Updated" 
    putStrLn "--------------------------------------------"
    putStrLn "Select '2' To View To-Do List"
    
    myToDoList


viewTask :: IO()
viewTask = do
    putStrLn "--------------------------------------------"
    connection <- open myDatabase
    todo <- query_ connection "SELECT * FROM todo" :: IO[ToDo]
    putStrLn "--------------------------------------------"
    close connection

    putStrLn "Your To-Do List Looks Like"
    putStrLn ""
    
    mapM_ print todo 
    myToDoList


deleteTask :: IO()
deleteTask = do
    putStrLn "Task Will Be Deleted As Per Id"
    putStrLn "------------------------------"
    id <- getLine
    let task_id = read id :: Int
    
    connection <- open myDatabase
    execute connection "DELETE FROM todo WHERE task_id = ? "  (Only task_id )
    close connection

    putStrLn "Task Has Been Deleted"
    myToDoList








             