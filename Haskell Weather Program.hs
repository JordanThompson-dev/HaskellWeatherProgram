
import Data.List
import Text.Printf (printf)

--
-- Types (define Place type here)
--
type Place = (String, Float, Float, [Int])


testData :: [Place]
testData = [("London", 51.5, -0.1, [0,0,5,8,8,0,0]),
            ("Cardiff", 51.5, -3.2, [12,8,15,0,0,0,2]),
            ("Norwich", 52.6, 1.3, [0,6,5,0,0,0,2]),
            ("Birmingham", 52.5, -1.9, [0,2,10,7,8,2,2]),
            ("Liverpool", 53.4, -3.0, [8,16,20,3,4,9,2]),
            ("Hull", 53.8, -0.3, [0,6,5,0,0,0,4]),
            ("Newcastle", 55.0, -1.6, [0,0,8,3,6,7,5]),
            ("Belfast", 54.6, -5.9, [10,18,14,0,6,5,2]),
            ("Glasgow", 55.9, -4.3, [7,5,3,0,6,5,0]),
            ("Plymouth", 50.4, -4.1, [4,9,0,0,0,6,5]),
            ("Aberdeen", 57.1, -2.1, [0,0,6,5,8,2,0]),
            ("Stornoway", 58.2, -6.4, [15,6,15,0,0,4,2]),
            ("Lerwick", 60.2, -1.1, [8,10,5,5,0,0,3]),
            ("St Heiler", 49.2, -2.1, [0,0,0,0,6,10,0])           
            ]

--
--  Your functional code goes here
--



listNames :: [Place] -> String
listNames [] = []
listNames ((name,_,_,_):xs) = name ++ "\n" ++ listNames xs

avgRain :: [Place] -> String -> Float
avgRain [] _ = 0.0
avgRain ((name,_,_,[m1,m2,m3,m4,m5,m6,m7]):xs) location
    |name == location = fromIntegral(m1+m2+m3+m4+m5+m6+m7)/7
    |otherwise = avgRain xs location

placesToString :: [Place] -> String
placesToString []        = []
placesToString ((name,_,_, rain):xs)    = output ++ placesToString xs
            where   output = name ++ " " ++ (listToString rain) ++ "\n"
                    

dryPlaces :: [Place] -> Int -> String
dryPlaces [] day = []
dryPlaces ((name,_,_,rain):xs) day
    |checkRain rain day == True = (output ++ dryPlaces xs day)
    | otherwise = dryPlaces xs day
    where output = printf "%s " name ++ "\n"
    
checkRain :: [Int] -> Int -> Bool
checkRain (xs) day
    | xs !! (day-1) == 0 = True
    | otherwise = False

update :: [Place] -> [Int] -> String   
update [] [] = "No Data"
update ((name,_,_,rain):xs) (y:ys) = printf "Updated rainfall for %s \n %s" name (output ++ update xs ys)
    where output = printf "%s " ( listToString(updateHeadTail y rain)) ++ "\n"
    
updateHeadTail :: Int -> [Int] -> [Int]
updateHeadTail y (x:xs) = init (y:x:xs)

listToString :: [Int] -> String
listToString [] = []
listToString (x:xs) = show x ++ "  " ++ listToString xs
    
    

replace :: String -> Place -> [Place] -> [Place]
replace _ _ [] = []
replace old new ((name, north, east, rain):xs)
    | old == name = new:xs
    | otherwise = (name, north, east ,rain) : replace old new xs
  
closestDryPlace :: [Place] -> (Float, Float) -> (String, Float) -> String
closestDryPlace [] _ _ = "No Data"
closestDryPlace details (north, east) (outputName, outputDistance) = closestLoc (getDryLoc details north east) (outputName, outputDistance)
  
closestLoc :: [(String, Float)] -> (String, Float) -> String
closestLoc [] _  = "No Data \n"
closestLoc ((name,distance):xs) (outputName, outputDistance)
    | distance < outputDistance && xs /= [] = closestLoc xs (name,distance)
    | distance < outputDistance && xs == [] = detailsLast
    | xs == [] = details
    | otherwise = closestLoc xs (outputName, outputDistance)
    where
    details = printf " %s \n" outputName
    detailsLast = printf " %s \n" name


getDryLoc :: [Place] -> Float -> Float -> [(String, Float)]
getDryLoc [] _ _ = []
getDryLoc ((name, ogNorth, ogEast, rain):xs) north east
    | checkRain rain 1 == True = (output ++ getDryLoc xs north east)
    | otherwise = getDryLoc xs north east
    where output = [(name, (pythagoras ogNorth ogEast north east))]

pythagoras :: Float -> Float -> Float -> Float -> Float
pythagoras y1 x1 y2 x2 = sqrt((x2 - x1)^2 + (y2 - y1)^2)


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print(listNames testData)
-- display the names of all the places

demo 2 = printf"%.2f" (avgRain testData "Cardiff")
-- display, to two decimal places, the average rainfall in Cardiff

demo 3 = putStrLn (placesToString testData)

demo 4 = putStrLn ( dryPlaces testData 2)
-- display the names of all places that were dry two days ago

demo 5 = putStrLn (update testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]) 
--[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)

demo 6 = print(replace "Plymouth" ("Portsmouth", 50.8, -1.1, [0,0,3,2,5,2,1]) testData)
-- replace "Plymouth" with "Portsmouth" which has 
-- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1

demo 7 = putStr (closestDryPlace testData (50.9,(-1.3)) ("Null", 99.0))

-- display the name of the place closest to 50.9 (N), -1.3 (E) 
-- that was dry yesterday

--demo 8 = print (rainfallMap testData)
-- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your rainfall map code goes here
--

rainfallMap :: [Place] -> IO()
rainfallMap [] = putStr ""
rainfallMap ((name, north, east, rainfall):xs) = do    
    writeAt ( (round((east - minX)/((maxX - minX)/80))),round(50-(north-minY)/((maxY - minY)/50)) ) output
    rainfallMap xs
    where
    minX = findMin(eastToList testData)
    maxX = findMax(eastToList testData)
    minY = findMin(northToList testData)
    maxY = findMax(northToList testData)
    output = "+ " ++ name ++ " " ++ printf "%.2f" (avgRain testData name)
    
eastToList :: [Place] -> [Float]
eastToList details = [ east | (_, _, east ,_) <- details]

northToList :: [Place] -> [Float]
northToList details = [ north | (_, north, _ ,_) <- details]   
    
findMin :: [Float] -> Float
findMin [] = 0.0
findMin (x:xs) = compare x xs
    where
    compare m [] = m
    compare m (y:ys)
        | y < m = compare y ys
        | otherwise = compare m ys

findMax :: [Float] -> Float
findMax [] = 0.0
findMax (x:xs) = compare x xs
    where
    compare m [] = m
    compare m (y:ys)
        | y > m = compare y ys
        | otherwise = compare m ys

        
--        
--
--        
        
--
-- Your user interface (and loading/saving) code goes here
--


main :: IO()
main  = do
    getPlaces <- loadFile
    putStr("\n Names of available places \n\n" ++ listNames getPlaces)
    options getPlaces

options :: [Place] -> IO()
options getPlaces = do
    putStrLn " 1 = display place names \n 2 = display average rainfall \n 3 = display list of all location data \n 4 = display dry places \n 5 = update rainfall \n 6 = replace a place \n 7 = find closest dry place \n 8 = rainfall map \n 9 = exit\n\n"
    menu getPlaces

menu :: [Place] -> IO()
menu getPlaces = do 
    line <- getLine
    if line == "1" then 
        putStr (listNames getPlaces ++ "\n\n")
    else if line == "2" then
       getAvgRainInpt getPlaces
    else if line == "3" then
        putStrLn (placesToString getPlaces ++ "\n\n")
    else if line == "4" then
        getDryInpt getPlaces
    else if line == "5" then 
        updateRainInpt getPlaces
    else if line == "6" then
        replaceInpt getPlaces
    else if line == "7" then
        putStr "Please run demo 7 as the IO() for this function doesn't work correctly"
    else if line == "8" then        
        rainfallMapInpt getPlaces
    else if line == "9" then
        saveFile getPlaces

        --exitSuccess
    else do
        putStrLn "Invalid input"
    options getPlaces

    
saveFile :: [Place] -> IO()
saveFile places = do let contents = show places
                     writeFile "places.txt" contents
--                     exitSuccess
 
loadFile :: IO [Place]
loadFile = do contents <- readFile "places.txt"
              return(read contents :: [Place])

-- The functions: getUsersAverage, validPlace, isValidLocation will check the user picked location name and if valid return the average rainfall for that location----------------------------------------------------
getAvgRainInpt :: [Place] -> IO ()
getAvgRainInpt getPlaces = do 
    putStrLn "Enter the name of a location for average rain or exit\n\n"
    line <- getLine
    if (validPlace line testData) == True then
        printf"%.2f\n"(avgRain getPlaces line)
    else if line == "exit" then
        options getPlaces
    else do
        putStr "Invalid input\n\n"
    getAvgRainInpt getPlaces

validPlace :: String -> [Place] -> Bool
validPlace placeName = foldr (||) False. map(isValidLocation placeName)

isValidLocation :: String -> Place -> Bool
isValidLocation placeName ( name, _, _, _) = placeName == name
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- The function: getUsersDryPlaces will check the users selected day and if its valid it will then check for place names that are dry on the selected day 
getDryInpt :: [Place] -> IO ()
getDryInpt getPlaces = do
    putStrLn("Enter (1-7) for dry places (yesterday - 7 days ago) or exit\n")
    line <- getLine
    if  line == "1" || line == "2" || line == "3" || line == "4" || line == "5" || line == "6" || line == "7" then
        putStr(dryPlaces getPlaces (read line :: Int))
    else if line == "exit" then
        options getPlaces
    else do
        putStr "Invalid input"
        getDryInpt getPlaces 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--This function will update daily rainfall reports-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
updateRainInpt :: [Place] -> IO ()
updateRainInpt getPlaces = do
    putStrLn "Enter a list of 14 integers or exit\n"
    line <- getLine
    if length line == 29 then
        rainUpdate getPlaces (read line::[Int])
    else if line == "exit" then
        options getPlaces
    else do
        putStrLn "Invalid input"
        updateRainInpt getPlaces
        
    
rainUpdate :: [Place] -> [Int] -> IO ()
rainUpdate details newRain = do
    let newDetails = update details newRain
    putStr (newDetails)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
--This function will swap out one location and its details with a new location whose details will be provided by the user -----------------------------------------------------------------------------------------------------
replaceInpt :: [Place] -> IO ()
replaceInpt getPlaces = do
    putStr "\nPlease input the name of the place you wish to replace or enter exit to return to the menu\n"
    selectName <- getLine
    putStr "\nPlease input the name of this new place or enter exit to return to the menu\n"
    newName <- getLine
    putStr "\nPlease input your north coordinate or enter exit to return to the menu\n"
    northInput <- getLine
    putStr "\nPlease input your East coordinate or enter exit to return to the menu\n"
    eastInput <- getLine
    putStr "\nPlease input a new list of rainfall or enter exit to return to the menu\n"
    newList <- getLine
    if selectName == "exit" || newName == "exit" || northInput == "exit" || eastInput == "exit" || newList == "exit" then
        options getPlaces
    else if length newList < 14 || length newList > 28 then
        replaceInpt getPlaces
    else do
        let newData = replace selectName (newName, (read northInput::Float), (read eastInput::Float), (read newList::[Int])) getPlaces
        putStr ("\n" ++ listNames newData ++ "\n")
        options  newData 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
    
--This function will locate the nearest location to the co-ordinates prodvided by the user-------------------------------------------------------------------------------------------------------------------------------------
closestDryInpt :: [Place] -> IO ()
closestDryInpt getPlaces = do
    putStrLn "Enter north value between 50 and 60\n"
    northInpt <- getLine
    putStrLn "Enter east value between -7 and 2\n"
    eastInpt <- getLine
    if northInpt >= "99" then
        closestDryInpt getPlaces
    else if northInpt == "exit" || eastInpt == "exit" then
        options getPlaces
    else do
        putStr(closestDryPlace getPlaces  ((read northInpt::(Float)),(read eastInpt::(Float))) ("No Place", 99.0))
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rainfallMapInpt :: [Place] -> IO()
rainfallMapInpt details = do
    clearScreen
    rainfallMap details
    putStrLn "\n"