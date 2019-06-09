module DomainTypes 

    open System

    type Pos = { x: int; y: int }

    type Board = Board of List<List<Option<ConsoleColor>>> // list of list of color

    type Piece = Piece of int * int * List<Pos> * ConsoleColor // x, y, shape, color

    type State = State of Board * Option<Piece> * Piece * int * int // board, optional piece, next piece, completed lines, score

    let boardWidth = 10

    let boardHeight = 20
