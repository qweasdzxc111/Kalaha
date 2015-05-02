module Data.Board (

   Pebbles,                        -- Int range 0 ..
   Pond_Ix, pond_first, pond_last, -- Int range 1 .. 6, pond_first = 1, pond_last = 6
   Lookahead,                      -- Int range 1 ..
   
   Ponds,                          -- = [Pebbles]
   Bank,                           -- = Pebbles
   
   Players (Player_A, Player_B, Finished),
   Board (Board, turn, ponds_A, bank_A, ponds_B, bank_B),
   
   empty_ponds,                    -- = [0, 0, 0, 0, 0, 0] :: Ponds
   initial_ponds,                  -- = [6, 6, 6, 6, 6, 6] :: Ponds
   initial_bank,                   -- = 0 :: Bank
   
   initial_board_Player_A,         -- Board as it appears with the first move for Player_A
   initial_board_Player_B,         -- Board as it appears with the first move for Player_B
   
   legal_move,                     -- :: Board -> Pond_Ix -> Bool
   no_of_pebbles,                  -- :: Board -> Pond_Ix -> Pebbles
   pick_n_distribute               -- :: Board -> Pond_Ix -> Board
   
) where

import Text.Printf (printf)

type Pebbles = Int -- range 0 .. 

type Pond_Ix = Int -- range 1 .. 6

-- Type Int chosen for the above two types to suppress any run-time checks 
-- in order to give you unconstraint performance.
-- For debugging they can be replaced with Nat and Pos (from Integer_Subtypes)
-- in order to catch some more run-time errors.
-- Change them back to Int before uploading to the competition server.

pond_first,
   pond_last :: Pond_Ix
pond_first = 1
pond_last  = 6

type Ponds = [Pebbles]
type Bank  = Pebbles

data Players = Player_A | Player_B | Finished
   deriving (Show, Eq)

data Board = Board {turn :: Players, ponds_A :: Ponds, bank_A :: Bank, 
                                     ponds_B :: Ponds, bank_B :: Bank}
   deriving Eq

instance Show (Board) where
   show board = "Board {turn = " ++ show (turn board) 
           ++ ", ponds_A = [" ++ show_ponds (ponds_A board) ++ "]"
           ++ ", bank_A = " ++ show_2_digit (bank_A board)
           ++ ", ponds_B = [" ++ show_ponds (ponds_B board) ++ "]"
           ++ ", bank_B = " ++ show_2_digit (bank_B board)
           ++ "}"

      where
         show_2_digit :: Int -> String
         show_2_digit i = printf "%2d" i
   
         show_ponds :: Ponds -> String
         show_ponds ponds = case ponds of
            [p]   -> show_2_digit p
            p: ps -> show_2_digit p ++ ", " ++ show_ponds ps
            []    -> ""

empty_ponds,
   initial_ponds :: Ponds
empty_ponds   = [0, 0, 0, 0, 0, 0]
initial_ponds = [6, 6, 6, 6, 6, 6]

initial_bank :: Bank
initial_bank  = 0

initial_board_Player_A,
   initial_board_Player_B :: Board
initial_board_Player_A = Board {turn = Player_A, ponds_A = initial_ponds, bank_A = initial_bank, 
                                                 ponds_B = initial_ponds, bank_B = initial_bank}
initial_board_Player_B = Board {turn = Player_B, ponds_A = initial_ponds, bank_A = initial_bank, 
                                                 ponds_B = initial_ponds, bank_B = initial_bank}
   
type Lookahead = Int -- range 1 ..

--
-- Note about the code to follow:
--
-- Functions legal_move, no_of_pebbles and pick_n_distribute have been "unrolled" 
-- (i.e. formulated with rigid lists instead of recursive functions).
-- While this has been done purely for better readability, please take a note that
-- you could implement those functions differently and much more compact with the
-- use of higher order functions and recursion.
-- This way is meant to make the rules the most easy to read for all students.
-- You are of course free to replace those functions with your own versions
-- if you wish so.

legal_move :: Board -> Pond_Ix -> Bool
legal_move board ix = case turn board of
   Player_A -> case ponds_A board of
      [p1A, p2A, p3A, p4A, p5A, p6A] -> case ix of
         1 -> p1A > 0 
         2 -> p2A > 0 
         3 -> p3A > 0 
         4 -> p4A > 0 
         5 -> p5A > 0 
         6 -> p6A > 0
         _ -> False
      _ -> False
   Player_B -> case ponds_B board of
      [p1B, p2B, p3B, p4B, p5B, p6B] -> case ix of
         1 -> p1B > 0 
         2 -> p2B > 0 
         3 -> p3B > 0 
         4 -> p4B > 0 
         5 -> p5B > 0 
         6 -> p6B > 0
         _ -> False
      _ -> False  
   Finished -> False
   
no_of_pebbles :: Board -> Pond_Ix -> Pebbles
no_of_pebbles board ix = case turn board of
   Player_A -> case ponds_A board of
      [p1A, p2A, p3A, p4A, p5A, p6A] -> case ix of
         1 -> p1A
         2 -> p2A 
         3 -> p3A 
         4 -> p4A 
         5 -> p5A 
         6 -> p6A
         _ -> error "Pond_Ix out of range"
      _ -> error "Wrong number of ponds on board"
   Player_B -> case ponds_B board of
      [p1B, p2B, p3B, p4B, p5B, p6B] -> case ix of
         1 -> p1B
         2 -> p2B 
         3 -> p3B 
         4 -> p4B 
         5 -> p5B 
         6 -> p6B
         _ -> error "Pond_Ix out of range"
      _ -> error "Wrong number of ponds on board"  
   Finished -> 0
   
pick_n_distribute :: Board -> Pond_Ix -> Board
pick_n_distribute current_board index = case board_to_distribution_list current_board of
   [p1A, p2A, p3A, p4A, p5A, p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B] -> case index of
      1 -> distribute p1A [  0] [p2A, p3A, p4A, p5A, p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B]
      2 -> distribute p2A [p1A,   0] [p3A, p4A, p5A, p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B]
      3 -> distribute p3A [p1A, p2A,   0] [p4A, p5A, p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B]
      4 -> distribute p4A [p1A, p2A, p3A,   0] [p5A, p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B]
      5 -> distribute p5A [p1A, p2A, p3A, p4A,   0] [p6A, bA, p1B, p2B, p3B, p4B, p5B, p6B]
      6 -> distribute p6A [p1A, p2A, p3A, p4A, p5A,   0] [bA, p1B, p2B, p3B, p4B, p5B, p6B]
{-
   p1A: p2A: p3A: p4A: p5A: p6A: ps -> case index of
      1 -> distribute p1A [  0] (p2A: p3A: p4A: p5A: p6A: ps)
      2 -> distribute p2A [p1A,   0] (p3A: p4A: p5A: p6A: ps)
      3 -> distribute p3A [p1A, p2A,   0] (p4A: p5A: p6A: ps)
      4 -> distribute p4A [p1A, p2A, p3A,   0] (p5A: p6A: ps)
      5 -> distribute p5A [p1A, p2A, p3A, p4A,   0] (p6A: ps)
      6 -> distribute p6A [p1A, p2A, p3A, p4A, p5A,   0] (ps)
-}   
      _ -> error "Out of range Pond_Ix"

   _ -> error "Distribution list of wrong lengths"
   
   where
      -- Unfolds the Board data structure into a single list
      board_to_distribution_list :: Board -> [Pebbles]
      board_to_distribution_list 
         Board {
            ponds_A = current_ponds_A, 
            ponds_B = current_ponds_B, 
            bank_A  = current_bank_A,
            bank_B  = current_bank_B} = 
               case turn current_board of
                  Player_A -> current_ponds_A ++ [current_bank_A] ++ current_ponds_B
                  Player_B -> current_ponds_B ++ [current_bank_B] ++ current_ponds_A
                  Finished   -> []
      
      -- Distributes pebbles out of one pond by adding individual pebbles to the 
      -- first pond of the post_list and adding this pont to the pre_list.
      -- For those who want to dig deeper: this is similar to the common "zipper"
      -- data-structure which is frequently used in functional programming.
      distribute :: Pebbles -> [Pebbles] -> [Pebbles] -> Board
      distribute pebbles pre_list post_list = case post_list of
         p: ps -> case pebbles of
            0 -> error "Attempted illegal move with zero pebbles"
            1 -> build_board 
                    current_board 
                    (pre_list ++ [p + 1] ++ ps) 
                    (pre_list_length == 6) 
                    ((pre_list_length) + 1) 
                    (p == 0 && (pre_list_length) < 6)
            _ -> distribute (pebbles - 1) (pre_list ++ [p + 1]) ps
         [] -> distribute pebbles [] pre_list
            
         where
            pre_list_length = length pre_list

      -- Checks the case of dropping the last pebble into an empty, own pond,
      -- and re-builds the board data-structure accordingly.
      build_board :: Board -> [Pebbles] -> Bool -> Pond_Ix -> Bool -> Board
      build_board board list last_into_bank ix last_into_empty_own_pond = 
         case last_into_empty_own_pond && (opposing_pond_filled list ix) of
            True  -> case list of
               [p1F, p2F, p3F, p4F, p5F, p6F, bank, p1S, p2S, p3S, p4S, p5S, p6S] -> case ix of
                  1 -> list_to_board board [0, p2F, p3F, p4F, p5F, p6F, bank + p1F + p6S, p1S, p2S, p3S, p4S, p5S, 0] last_into_bank
                  2 -> list_to_board board [p1F, 0, p3F, p4F, p5F, p6F, bank + p2F + p5S, p1S, p2S, p3S, p4S, 0, p6S] last_into_bank
                  3 -> list_to_board board [p1F, p2F, 0, p4F, p5F, p6F, bank + p3F + p4S, p1S, p2S, p3S, 0, p5S, p6S] last_into_bank
                  4 -> list_to_board board [p1F, p2F, p3F, 0, p5F, p6F, bank + p4F + p3S, p1S, p2S, 0, p4S, p5S, p6S] last_into_bank
                  5 -> list_to_board board [p1F, p2F, p3F, p4F, 0, p6F, bank + p5F + p2S, p1S, 0, p3S, p4S, p5S, p6S] last_into_bank
                  6 -> list_to_board board [p1F, p2F, p3F, p4F, p5F, 0, bank + p6F + p1S, 0, p2S, p3S, p4S, p5S, p6S] last_into_bank   
                  _ -> error "Missed the correct pond number for the last pebble into an empty own pond"    
               _ -> error "Distribution list of wrong lengths"
            False -> list_to_board board list last_into_bank
                  
      opposing_pond_filled :: [Pebbles] -> Pond_Ix -> Bool
      opposing_pond_filled list ix = case list of
         [_, _, _, _, _, _, _, p1S, p2S, p3S, p4S, p5S, p6S] -> case ix of
            1 -> p6S > 0
            2 -> p5S > 0
            3 -> p4S > 0
            4 -> p3S > 0
            5 -> p2S > 0
            6 -> p1S > 0
            _ -> error "Missed the correct pond number for the last pebble into an empty own pond"
         _ -> error "Distribution list of wrong lengths"
             
      -- Re-builds a board based on a single pebbles list and the info about the current turn
      list_to_board :: Board -> [Pebbles] -> Bool -> Board
      list_to_board board list last_into_bank = case list of
         [p1F, p2F, p3F, p4F, p5F, p6F, bank, p1S, p2S, p3S, p4S, p5S, p6S] -> case turn board of
            Player_A -> check_game_over board {
                     turn    = next_turn (turn board) last_into_bank,
                     ponds_A = [p1F, p2F, p3F, p4F, p5F, p6F],
                     ponds_B = [p1S, p2S, p3S, p4S, p5S, p6S],
                     bank_A  = bank}
            Player_B -> check_game_over board {
                     turn    = next_turn (turn board) last_into_bank,
                     ponds_B = [p1F, p2F, p3F, p4F, p5F, p6F],
                     ponds_A = [p1S, p2S, p3S, p4S, p5S, p6S],
                     bank_B  = bank}
            Finished -> board

         _ -> error "Distribution list of wrong lengths"
         
      next_turn :: Players -> Bool -> Players
      next_turn current_turn last_into_bank = case last_into_bank of
         True  -> current_turn
         False -> case current_turn of
            Player_A -> Player_B
            Player_B -> Player_A
            Finished -> Finished
                              
      check_game_over :: Board -> Board
      check_game_over board = case sum (ponds_A board) == 0 || sum (ponds_B board) == 0 of
         True  -> Board {turn = Finished, bank_A = (bank_A board) + sum (ponds_A board), ponds_A = empty_ponds,
                                          bank_B = (bank_B board) + sum (ponds_B board), ponds_B = empty_ponds}
         False -> board
