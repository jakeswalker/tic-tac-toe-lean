import TicTacToeLean

def checkWin (board : Array String) (player : String) : Bool :=
  let winLines : List (Nat × Nat × Nat) :=
    [ (0,1,2), (3,4,5), (6,7,8),
      (0,3,6), (1,4,7), (2,5,8),
      (0,4,8), (2,4,6) ]
  winLines.any (fun (a,b,c) =>
    board.getD a " " = player &&
    board.getD b " " = player &&
    board.getD c " " = player
  )

def checkTie (board : Array String) : Bool :=
  board.all (fun cell => cell != " ")

def printBoards (numBoard : Array String) (board : Array String) : IO Unit := do
  let n1 := ".-----------.\t"
  let n2 := s!"| {numBoard.getD 0 "?"} | {numBoard.getD 1 "?"} | {numBoard.getD 2 "?"} |\t"
  let n3 := "|---+---+---|\t"
  let n4 := s!"| {numBoard.getD 3 "?"} | {numBoard.getD 4 "?"} | {numBoard.getD 5 "?"} |\t"
  let n5 := "|---+---+---|\t"
  let n6 := s!"| {numBoard.getD 6 "?"} | {numBoard.getD 7 "?"} | {numBoard.getD 8 "?"} |\t"
  let n7 := "'-----------'\t"
  let b1 := ".-----------."
  let b2 := s!"| {board.getD 0 "?"} | {board.getD 1 "?"} | {board.getD 2 "?"} |"
  let b3 := "|---+---+---|"
  let b4 := s!"| {board.getD 3 "?"} | {board.getD 4 "?"} | {board.getD 5 "?"} |"
  let b5 := "|---+---+---|"
  let b6 := s!"| {board.getD 6 "?"} | {board.getD 7 "?"} | {board.getD 8 "?"} |"
  let b7 := s!"'-----------'"
  IO.println (n1 ++ b1)
  IO.println (n2 ++ b2)
  IO.println (n3 ++ b3)
  IO.println (n4 ++ b4)
  IO.println (n5 ++ b5)
  IO.println (n6 ++ b6)
  IO.println (n7 ++ b7)

partial def ticTacToeLoop (board : Array String) (numBoard : Array String) (player : String) : IO Unit := do
  printBoards numBoard board
  IO.print s!"Player {player}'s turn. Please pick a number to place your {player}: "
  let input ← (← IO.getStdin).getLine
  let trimmed := input.trimAscii.toString
  if trimmed.toLower = "q" then
    IO.println "Thank you for playing!"
  else
    match trimmed.toNat? with
      | some n =>
        let i := n - 1
        if i >= 0 && i <= 8 then
          if board.getD i "never" = " " then
            let newNumBoard := numBoard.setIfInBounds i " "
            let newBoard := board.setIfInBounds i player
            let nextPlayer := if player = "X" then "O" else "X"
            if checkWin newBoard player then
              printBoards newNumBoard newBoard
              IO.println s!"Player {player} wins!"
            else
              if checkTie newBoard then
                printBoards newNumBoard newBoard
                IO.println s!"It's a tie!"
              else
                ticTacToeLoop newBoard newNumBoard nextPlayer
          else
            IO.println "That position is already taken. Please try again."
            ticTacToeLoop board numBoard player
        else
          IO.println "Position must be between 1 and 9."
          ticTacToeLoop board numBoard player
      | none =>
          IO.println "Please enter a valid number."
          ticTacToeLoop board numBoard player

def main : IO Unit := do
  let initBoard := #[" ", " ", " ", " ", " ", " ", " ", " ", " "]
  let numBoard := #["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  let initPlayer := "X"
  IO.println "Welcome to Tic Tac Toe! Enter Q at anytime to quit."
  ticTacToeLoop initBoard numBoard initPlayer
