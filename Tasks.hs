{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Tasks where

import Dataset
import Text.Printf
import Text.Read
import Dataset
import Data.List
import Data.Maybe
import Data.Array


type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


compute_exam_grades :: Table -> Table
-- append the header at the start
compute_exam_grades = \grades->(["Nume", "Punctaj Exam"]:(foldr (\lin table -> (foldlin lin) :table)
    [] (parse_table grades)))
    where
        --helper function that replaces all empty strings with "0"
        parse_table = tail.(map $ \lin->(map (\x->if x == "" then "0" else x) lin))
                            
        --function used for each line that calculates exam score for a specific student
        foldlin lin = (head lin):(((printf "%.2f").mean) lin):[]
            where
                mean lin = (((\x->read x::Float).head.reverse) lin) + (fromIntegral $ sum $ map (\x->read x::Int)
                    $ (tail.reverse.tail.reverse) lin) / 4

-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
-- use a filter to extract the students that have passed the exam and get the length of this list
get_passed_students_num = \grades->(length $ filter(\x->((read x::Float) >= 2.5))
    (tail $ map (head.reverse) (compute_exam_grades grades)))

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
-- divide the number of students who have passed the exam by the total number of student
get_passed_students_percentage = \grades -> ((fromIntegral $ get_passed_students_num grades)
    / (fromIntegral $ length $ tail grades))

-- Average exam grade
get_exam_avg :: Table -> Float
-- calculate the sum of the grades and divide it by the number of students
get_exam_avg = \grades->((sum $ map (\x->read x::Float) $ tail $ map (head.reverse) (compute_exam_grades grades))
    / (fromIntegral $ length $ tail grades))

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
-- use a filter, that is applied only on the total grade from homework
get_passed_hw_num = \grades->(length $ filter(>= 1.5) (compute_homework_grades grades))
    where
        compute_homework_grades table = map (foldl (\acc str -> (acc + (read_value str))) 0)
            $ tail $ map (tail.tail.(take 5)) table

get_avg_responses_per_qs :: Table -> Table

-- for each question calculate the average
get_avg_responses_per_qs = \grades ->(header:(map (printf "%.2f")
    (map (/((fromIntegral (length (tail grades)))::Float))
    $ foldl (zipWith (+)) ((take 6) (repeat 0.0)) $ get_questions grades)):[])
        where
            header = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]
            -- extract only the grades for the questions, and replace the empty string by 0.
            -- Otherwise, convert from string to float
            get_questions grades = map (\lin->(map (read_value) ((tail.(take 7)) lin))) $ tail grades

-- Function for transposing a matrix
tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr m = (map head m):tr (map tail m)

-- Task 4
get_exam_summary :: Table -> [[String]]

-- obtain the transpose of the grades matrix, then for each possible grade,
--compute the number of times it is obtained for each question
get_exam_summary =  \grades->(zipWith (:) header $ tr
    $ foldr (\gr acc-> (get_count grades gr):acc) [] $ possible_grades grades)
    where
        header = ["Q", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]
        -- returns the number of times a grade was obtained
        get_count exam_table grade = (show grade):
            (map show $ map (length.(filter (==grade))) $ get_questions exam_table)
        -- returns a table with all the questions' grades
        get_questions grades = tr $ map (\lin->(map (\x->if x == "" then 0 else read x::Int)
            $ (tail.(take 7)) lin)) $ tail grades
        -- generates all the possible grades for a question
        possible_grades exam_table = sort $ nub $ foldl (++) [] $ get_questions exam_table


get_ranking :: Table -> Table
-- sort the table first by grade, then by name
get_ranking = \table->(["Nume", "Punctaj Exam"]:(sortBy (\(numeX:grX) (numeY:grY)
    -> compare ((toFloat grX), numeX) ((toFloat grY), numeY))
    $ tail $ compute_exam_grades table))
        where
            toFloat grade = read(head grade)::Float

compute_diff :: Table -> Table
-- calculates the grade at the written exam and at the interview exam.
--Then it calculates the difference and builds a table with this information
compute_diff = \grades->(foldr (\lin table -> (foldlin lin) :table) [] $ parse_table grades)
    where
        parse_table = tail.(map (\lin->(map (\x->if x == "" then "0" else x) lin)))
        foldlin lin = (head lin):(((printf "%.2f").mean_interview) lin)
            :(((printf "%.2f").writen_exam) lin):(((printf "%.2f").difference) lin):[]
            where
                mean_interview lin = ((fromIntegral $ sum $ map (\x->read x::Int)
                    $ (tail.reverse.tail.reverse) lin) / 4)::Float
                writen_exam lin = (((\x->read x::Float).head.reverse) lin)::Float
                difference lin = abs $ (mean_interview lin) - (writen_exam lin)

get_exam_diff_table :: Table -> Table
-- sort the previously built table by difference, then by by name
get_exam_diff_table = \table -> (["Nume","Punctaj interviu","Punctaj scris","Diferenta"]:
    (sortBy (\entryX entryY -> compare (((toFloat.reverse) entryX), head entryX)
        (((toFloat.reverse) entryY), head entryY)) $ compute_diff table))
        where
            toFloat grade = read(head grade)::Float

-- the splitBy function used in the lab that splits a string on a given character
splitBy :: Char -> String -> [String]
splitBy = \sep -> foldl (\acc chr -> if chr == sep then acc ++ [[]]
    else ((reverse.tail.reverse) acc)++[(((head.reverse) acc) ++ [chr])]) [[]]

--reads a table from a csv file, spliting firstly by newline character, then by comma
read_csv :: CSV -> Table
read_csv file = foldr (\row acc-> (splitBy ',' row):acc) [] (splitBy '\n' file)

-- converts a table to a csv file, by converting each row to a comma separated string, then
-- each the resulting list of strings to a string separated by newline
write_csv :: Table -> String
write_csv table = (reverse.tail.reverse) (foldr (\row acc-> (((reverse.tail.reverse)
                    (foldr (\el acc2 -> (el ++ ",")++acc2) "" row)) ++ "\n")++acc) [] table)

-- searches a string value in a list of string and returns its index
find_val :: String -> Row -> Int -> Int
find_val name [] acc = acc
find_val name (x:xs) acc = if (x == name) then acc else (find_val name xs (acc + 1))

-- Extracts the entries from a specific column in a table
as_list :: String -> Table -> [String]
as_list name table = tail (map (!!(find_val name (head table) 0)) table)

-- sort the rows by a specific column, using a comparator function
tsort :: String -> Table -> Table
tsort value table = (head table):(sortBy (comparator (find_val value (head table) 0)) (tail table))
    where
        -- function that sorts numerically if the value on the required column is an integer,
        -- or lexicographically if the value is a string
        comparator :: Int -> [String] -> [String] -> Ordering
        comparator = \x rowX rowY -> if (isJust (numX x rowX))
                                        then if (compare (numX x rowX) (numY x rowY)) == EQ 
                                            then (compare (rowX!!0) (rowY!!0))
                                            else (compare (numX x rowX) (numY x rowY))
                                        else if (compare (rowX!!x) (rowY!!x)) == EQ
                                            then (compare (rowX!!0) (rowY!!0))
                                            else (compare (rowX!!x) (rowY!!x))
            where
                -- attempts to read a numerical value from an index and row
                numX x rowX = (readMaybe (rowX!!x)::Maybe Integer)
                numY x rowY = (readMaybe (rowY!!x)::Maybe Integer)

-- applies the function f on all values in table
vmap :: (Value -> Value) -> Table -> Table
vmap f table = map (\el -> map f el) table

-- applies the function f on all values in the row, and appends header at the start
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f header table = header:(map f (tail table))

fix_table :: Table -> Table
fix_table = vmap (\x -> if x == "" then "0" else x)

correct_hw_grades = fix_table hw_grades

-- returns the name and the total grade on homework
get_hw_grade_total :: Row -> Row
get_hw_grade_total row = [(head row)]++[((printf "%.2f") (sum(map (read_value) ((tail.tail) row))))]

-- adds the table2 after table1, checking columns names with an auxiliary recursive function
vunion :: Table -> Table -> Table
vunion = \t1 t2 ->  if (compare_headers (head t1) (head t2)) then t1++(tail t2) else t1
    where
        compare_headers [] [] = True
        compare_headers [] x = False
        compare_headers x [] = False
        compare_headers (x:xs) (y:ys) = if (x == y) then compare_headers xs ys else False

-- apppends the two table using transpose, and if necessary, extends one table to be the same size as the other
hunion :: Table -> Table -> Table
hunion = \t1 t2 -> transpose ((transpose (extend t1 t2)) ++ (transpose (extend t2 t1)))
    where
        -- adds empty values to the shorter table
        extend :: Table -> Table -> Table
        extend t1 t2 = t1 ++ (take ((length t2) - (length t1)) $ repeat $ take (length $ head t1) $ repeat "")
{-
merges the first table with the values replaced with the second table,
built using the function build_table, and truncated so that it
does not contain the same columns
-}
tjoin :: String -> Table -> Table -> Table
tjoin key table1 table2 = hunion (tr (combine_tables (head table1) (table1)
                            (build_table (as_list key table1) key table2)))
                            (tr (truncate_table (head table2) (build_table (as_list key table1) key table2) table1))
    where
        --replaces the values on the matching columns from table1 with those from table2
        combine_tables :: Row -> Table -> Table -> Table
        combine_tables [] _ _ = []
        combine_tables (x:xs) table1 table2 = if (isJust $ elemIndex x (head table2))
                                                then (x : (replace_values (as_list x table1) (as_list x table2)))
                                                    :(combine_tables xs table1 table2)
                                                else (x : (as_list x table1)):(combine_tables xs table1 table2)

        -- removes columns from table1 that are already in table2
        truncate_table :: Row -> Table -> Table -> Table
        truncate_table [] _ _ = []
        truncate_table (x:xs) table1 table2 = if (isJust $ elemIndex x (head table2))
                                                then (truncate_table xs table1 table2)
                                                else (x : (as_list x table1)):(truncate_table xs table1 table2)

        -- replaces the values from the first row with the ones in the second row, excepting null values
        replace_values :: Row -> Row -> Row
        replace_values [] _ = []
        replace_values _ [] = []
        replace_values (x:xs) (y:ys) = if y == "" then x:(replace_values xs ys) else y:(replace_values xs ys)

        -- builds a table with all the elements from table2 that has the same value for the key as table1. For the
        -- elements that do not have the same value for the key, add an empty row
        build_table :: [String] -> String -> Table -> Table
        build_table list key table = (head table):(build_table_aux list key table)
            where
            build_table_aux [] _ table2  = []
            build_table_aux (x:xs) key table2 = if (isJust $ elemIndex x (as_list key table2))
                                                then (table2!!((fromJust $ elemIndex x (as_list key table2))+ 1))
                                                    :(build_table_aux xs key table2)
                                                else (x:(take ((length $ head table2) - 1) $ repeat ""))
                                                    :(build_table_aux xs key table2)

-- computes the cartesian product for two tables by mapping the function with the current row in table1,
-- to all the rows in table2, using the rmap function I defined earlier
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f header table1 table2 = header: [f x y| x <- tail table1, y <- tail table2]

-- calls as_list for every column name and merges the entries in a table
projection :: [String] -> Table -> Table
projection = \columns t1 ->  tr(foldr (\str acc -> (as_list str ((head t1):t1)):acc) [] columns)

data Query =
    FromCSV CSV
    | FromTable Table --additional query for converting a Table to an evaluable Query
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

type EdgeOp = Row -> Row -> Maybe Value
--Function that converts an edge operation to a function that receives 2 rows
--and returns the resulting one(function that is useful for cartesian product)
build_row :: EdgeOp -> Row -> Row -> Row
build_row edgeop row1 row2 = if isJust (edgeop row1 row2)
                                then [head row1, head row2, fromJust (edgeop row1 row2)]
                                else ["-", "-", "-"] -- we do not care about the result for non-connected vertices

data QResult = CSV CSV | Table Table | List [String]
data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

instance Show QResult where
    show (Table table) = write_csv table
    show (CSV csv) = show csv
    show (List str) = show str

class Eval a where
    eval :: a -> QResult

-- Function that extracts the table from a QResult containing a Table
extract_table :: QResult -> Table
extract_table (Table table) = table
extract_table _ = []

-- Function that receives a row with 3 entries(used for the graph) and sorts
-- the first two entries lexicographically
sort_row :: Row -> Row
sort_row r = if (head r) < (head $ tail r) then r
                else [head $ tail r, head r, last r]

instance Eval Query where
    eval (FromCSV str) = Table $ read_csv str
    eval (FromTable table) = Table $ table
    eval (ToCSV query) = CSV $ write_csv $ extract_table $ eval query
    eval (AsList colname query) = List $ as_list colname $ extract_table $ eval query
    eval (Sort colname query) = Table $ tsort colname $ extract_table $ eval query
    eval (ValueMap op query) = Table $ vmap op $ extract_table $ eval query
    eval (RowMap op colnames query) = Table $ rmap op colnames $ extract_table $ eval query
    eval (VUnion query1 query2) = Table $ vunion (extract_table $ eval query1) (extract_table $ eval query2)
    eval (HUnion query1 query2) = Table $ hunion (extract_table $ eval query1) (extract_table $ eval query2)
    eval (TableJoin colname query1 query2) = Table $ tjoin colname (extract_table $ eval query1)
        (extract_table $ eval query2)
    eval (Cartesian op colnames query1 query2) = Table $ cartesian op colnames (extract_table $ eval query1)
        (extract_table $ eval query2)
    eval (Projection colnames query) = Table $ projection colnames $ extract_table $ eval query
    eval (Filter op query) = Table $ (head $ extract_table $ eval query)
        :(filter (feval (head $ extract_table $ eval query) op) $ tail $ extract_table $ eval query)
    eval (Graph edgeop query) = Table $
                                nub $ 
                                map sort_row $
                                filter (\r -> head r /= "-") $ 
                                filter (\r -> head r /= head (tail r)) $ 
                                cartesian_table edgeop query

-- Builds a table of distances between vertices
cartesian_table :: EdgeOp -> Query -> Table
cartesian_table edgeop query = cartesian (build_row edgeop) ["From", "To", "Value"]
    (extract_table $ eval query) (extract_table $ eval query)

-- Moves the last row of a table to the start
add_header_to_start :: Table -> Table
add_header_to_start table = (last table):(init table)

-- edge operation for similarity between two vertices
similarities_op :: Row -> Row -> Maybe Value
similarities_op row1 row2 = if (calculate_similarities row1 row2) >= 5
                                then Just $ (show $ (calculate_similarities row1 row2))
                                else Nothing
    where
        --compare each entry from the first row to the corresponding one in the second row and count the similarities
        calculate_similarities :: Row -> Row -> Integer
        calculate_similarities row1 row2 =  sum $ zipWith (\x y -> if x == y then 1 else 0) (tail row1) (tail row2)

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- extracts the float value contained in a row, at the index given by the location of colname in the header
extract_val :: Row -> String -> Row -> Float
extract_val row colname header = if isNothing (readMaybe (extract_str row colname header)::Maybe Float) then 0.0
                                    else fromJust (readMaybe (extract_str row colname header)::Maybe Float)

-- extracts the string value contained in a row, at the index given by the location of colname in the header
extract_str :: Row -> String -> Row -> String
extract_str row colname header = row!!(fromJust $ elemIndex colname header)

instance FEval Float where
    feval header (Eq colname num) = \row -> (extract_val row colname header) == num
    feval header (Lt colname num) = \row -> (extract_val row colname header) < num
    feval header (Gt colname num) = \row -> (extract_val row colname header) > num
    feval header (In colname list) = \row -> elem (extract_val row colname header) list
    feval header (FNot cond) = \row -> not (feval header cond row)
    feval header (FieldEq colname1 colname2) = \row -> (extract_val row colname1 header) 
                                                    == (extract_val row colname2 header)

instance FEval String where
    feval header (Eq colname num) = \row -> (extract_str row colname header) == num
    feval header (Lt colname num) = \row -> (extract_str row colname header) < num
    feval header (Gt colname num) = \row -> (extract_str row colname header) > num
    feval header (In colname list) = \row -> elem (extract_str row colname header) list
    feval header (FNot cond) = \row -> not (feval header cond row)
    feval header (FieldEq colname1 colname2) = \row -> (extract_str row colname1 header)
                                                    == (extract_str row colname2 header)

-- Filter out the rows without a valid email, compute the similarity graph, then sort by the value column
similarities_query = Sort "Value" $ Graph similarities_op $ Filter (FNot $ Eq "Email" "") $ FromCSV lecture_grades_csv

--builds a row containing the first elements of the 2 rows passed as parameters and the "distance" between them
cartesian_distance :: Row -> Row-> Row
cartesian_distance row1 row2 = [head row1, head row2, (show $ distance (head row1) (head row2))]

{-
    For the distance function I used the Longest Common Substring DP matrix. The distance is the sum
    of all the values in this matrix. DP[i][j] is the length of the common substring ending at index i
    for the first string and index j for the second. I chose to use this sum because two similar strings will
    have multiple common substrings, and the result will be bigger than the distance between two strings that
    have only one single long common substring, but the other characters are completely different.
    The bigger the similarity, the bigger the distance.
-}
distance :: String -> String -> Integer
distance str1 str2 = sum matrix
                where
                    m = length str1
                    n = length str2
                    bounds = ((0, 0), (m, n))
                    matrix = listArray bounds [sol i j | (i, j) <- range bounds]
                    sol left right
                        | left == 0 = 0
                        | right == 0 = 0
                        | str1 !! (left -1) == str2 !! (right - 1) = 1 + matrix ! (left-1, right-1)
                        | otherwise = 0

-- Receives a string with a typo and a table of references and find the correct string
get_correct_name :: String -> Table -> String
get_correct_name typo product_table
    -- if not found in the table, then the string is corect
    | (length $ extract_table $ eval $ Filter (Eq "Bad" typo) $ FromTable product_table) == 1 = typo
    -- if found, extract the reference with the biggest distance(similarity)
    | otherwise = head $ tail $ head $ reverse $ extract_table
        $ eval $ Sort "Similarity" $ Filter (Eq "Bad" typo) $ FromTable product_table

--Builds the corrected table. That is, the function receives a Row to be replaced in the table
--and returns the newly created table. This function also receives a transposed table(for replacing
--rows more easily)
build_corrected :: [String] -> Table -> Table
build_corrected to_replace [] = []
build_corrected to_replace (x:xs) = if head x == head to_replace
                                        then to_replace:(build_corrected to_replace xs)
                                        else x:(build_corrected to_replace xs)

-- Function that receives a header and a list of column names and returns a list of indices, each index
-- representing the position where one specific column name is located in the header
columns_to_index :: Row -> Row -> [Int]
columns_to_index header [] = []
columns_to_index header (x:xs) = (fromJust $ elemIndex x header):(columns_to_index header xs)

-- Converts a string value to a float, and converts empty strings to 0
read_value :: String -> Float
read_value entry
    | entry == "" = 0.00
    | otherwise = read entry::Float


correct_table :: String -> CSV -> CSV -> CSV
correct_table ref_col table ref = write_csv $ transpose $ build_corrected 
    (ref_col:(map (\name -> get_correct_name name product_table) (as_list ref_col $ read_csv table)))
    (transpose $ read_csv table)
    where
        -- Cartesian product of the two tables, where the third entry is the similarity calculated by
        -- the distance function
        product_table = (cartesian cartesian_distance ["Bad", "Correct", "Similarity"]
            table_init_with_typos table_ref_with_correct)
        -- Remove the entries in the column(table with only one columns) that are correct.
        table_init_with_typos = extract_table (eval $ Filter (FNot $ In ref_col (as_list ref_col table_ref))
            (FromCSV $ write_csv table_init))
        -- Remove the entries in the reference column that are not useful(the values are already correct
        -- in the emails table)
        table_ref_with_correct = extract_table (eval $ Filter (FNot $ In ref_col (as_list ref_col table_init))
            (FromCSV $ write_csv table_ref))
        --reference table, only with the column desired
        table_ref = extract_table $ eval $ Projection [ref_col] $ FromCSV ref
        --the table with the typos, only with the column actually containing the typos
        table_init = extract_table $ eval $ Projection [ref_col] $ FromCSV table


grades :: CSV -> CSV -> CSV -> CSV -> CSV
-- Read all the tables from CSV, convert the emails table to the correct version, and append the header to
-- the result of the join_tables function
grades emails hws exams lectures = write_csv $ ["Nume","Punctaj Teme","Punctaj Curs","Punctaj Exam","Punctaj Total"]:
    (join_tables (read_csv $ correct_table "Nume" emails hws) (read_csv hws) (read_csv exams) (read_csv lectures))
        where
            -- This function uses the large table obtained by joining all 4 tables received as parameter. It
            -- iterates through each row and calculates for each person the required grades
            join_tables :: Table -> Table -> Table -> Table -> Table
            join_tables emails hws exams lectures = foldr (\row acc -> ((head row):(get_hw_grade header row)
                :(get_lecture_grade header row):(get_exam_grade header row)
                :(compute_total (get_hw_grade header row) (get_lecture_grade header row)
                (get_exam_grade header row)):[]):acc) [] (tail large_table)
                where
                    --receives the header of the large table, one row from the large table and calculates
                    -- the homework grade
                    get_hw_grade :: Row -> Row -> String
                    get_hw_grade header row = (printf "%.2f") $ sum
                        $ map (\ind -> read (row!!ind)::Float) (columns_to_index header hw_info)
                    
                    get_lecture_grade :: Row -> Row -> String
                    get_lecture_grade header row
                        -- if there exists a "" field, then it means that the current person did not exist
                        -- in the lecture table, and we return the empty string in this case
                        | any (=="") $ map (\ind -> row!!ind) (columns_to_index header lecture_info) = ""
                        | otherwise = (printf "%.2f") (2.0 * (sum $ map (\ind -> read (row!!ind)::Float)
                            (columns_to_index header lecture_info)) / (fromIntegral $ length $ lecture_info))
                    get_exam_grade :: Row -> Row -> String
                    get_exam_grade header row
                        | any (=="") $ map (\ind -> row!!ind) (columns_to_index header exam_info) = ""  
                        | otherwise = (printf "%.2f") (((sum $ map (\ind -> read (row!!ind)::Float)
                            (init $ columns_to_index header exam_info)) / 4.0)
                            + (read (row!!(last $ columns_to_index header exam_info))::Float))
                    
                    compute_total :: String -> String -> String -> String
                    compute_total hw_grade lecture_grade exam_grade
                        | (read_value exam_grade) < 2.5 = "4.00"
                        | ((read_value hw_grade) + (read_value lecture_grade)) < 2.5 = "4.00"
                        | otherwise = (printf "%.2f") ((min ((read_value hw_grade) + (read_value lecture_grade)) 5)
                            + (read_value exam_grade))

                    header = head large_table
                    -- Obtain a large table by mapping the emails from the emails table to the emails in the lecture
                    -- table, then joining all the other tables with the names key
                    large_table = extract_table $ eval $ Sort "Nume" $ FromTable
                        $ tjoin "Nume" (tjoin "Nume" (tjoin "Email" (fix_table emails) (fix_table lectures))
                        (fix_table hws)) (fix_table exams)
                    hw_info = delete "Nume" $ head hws --extracts columns with relevant info about hw grades
                    exam_info = delete "Nume" $ head exams -- info about exams
                    email_info = delete "Nume" $ head emails -- info about emails
                    lecture_info = delete "Email" $ head lectures --info about lecture grades
