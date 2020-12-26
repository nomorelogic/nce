unit nce.board;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree;

type
  TPieceColor = (pcWhite, pcBlack);
  TCastinlg = (castlWhiteKSide, castlWhiteQSide, castlBlackKSide, castlBlackQSide);

  TPieceType = (cpEmpty,
                cpPawnWhite,   cpPawnBlack,    // pawn   / pedone
                cpRookWhite,   cpRookBlack,    // rook   / torre
                cpKnightWhite, cpKnightBlack,  // knight / cavallo
                cpBishopWhite, cpBishopBlack,  // bishop / alfiere
                cpQueenWhite,  cpQueenBlack,   // queen  / regina, donna
                cpKingWhite,   cpKingBlack);   // king   / re

  TPieceRange = cpPawnWhite..cpKingBlack;

  TPieceDescription = record
    Des1: string;
    Des2: string;
    Full: string;
  end;

  TDescriptionArray= array[TPieceRange] of TPieceDescription;
  TColorArray= array[pcWhite..pcBlack] of TPieceDescription;

  TLocalLangType  = (llIt, llEn, llFr);
  TLocalLangRange = llIt..llFr;

  TLocalization = array[TLocalLangRange] of TDescriptionArray;
  TColorLocalization = array[TLocalLangRange] of TColorArray;

  TPiece = record
    PieceType: TPieceType;
  end;

  TBoardColType  = (colA, colB, colC, colD, colE, colF, colG, colH);
  TBoardColRange = colA..colH;
  TBoardRowType  = (row1, row2, row3, row4, row5, row6, row7, row8);
  TBoardRowRange = row1..row8;

  TCellCoord = record
    col: TBoardColType;
    row: TBoardRowType;
  end;

  TPieceMoves = record
    MaxMove: integer;
    Moves: array[0..29] of TCellCoord;
  end;

  // I need this?
  TBoardCell = ( bcA1, bcA2, bcA3, bcA4, bcA5, bcA6, bcA7, bcA8,
                 bcB1, bcB2, bcB3, bcB4, bcB5, bcB6, bcB7, bcB8,
                 bcC1, bcC2, bcC3, bcC4, bcC5, bcC6, bcC7, bcC8,
                 bcD1, bcD2, bcD3, bcD4, bcD5, bcD6, bcD7, bcD8,
                 bcE1, bcE2, bcE3, bcE4, bcE5, bcE6, bcE7, bcE8,
                 bcF1, bcF2, bcF3, bcF4, bcF5, bcF6, bcF7, bcF8,
                 bcG1, bcG2, bcG3, bcG4, bcG5, bcG6, bcG7, bcG8,
                 bcH1, bcH2, bcH3, bcH4, bcH5, bcH6, bcH7, bcH8  );


  // TBoardCell_Set = set of TBoardCell;
  TCellCoord_Set = set of char;

  // TBoardRowArray = array[TBoardRowRange] of TPiece;
  // TBoard = array[TBoardColRange] of TBoardRowArray;
  TBoard = array[TBoardColRange] of array[TBoardRowRange] of TPiece;
  TBoardColName = array[TBoardColType] of char;
  TBoardRowName = array[TBoardRowType] of char;
  TCellNames = array[TBoardColRange] of array[TBoardRowRange] of string;

  { TBoardObj }

  TBoardObj = class
  public
    constructor Create;
    destructor Destroy; override;
  public
    Name: string;
    Board: TBoard;
    ActiveColor: TPieceColor;
    Castlings: set of TCastinlg;
    Halfmove: integer;
    Fullmove: integer;
    CanEnPassant: boolean;
    EnPassant: TCellCoord;

    ChildBoards: TStringToPointerTree;
  end;


  { TBoardObjHelper }

  TBoardObjHelper = class helper for TBoardObj
  private
    // procedure DoExpand_
  public
    procedure Clear;

    procedure StartPos(const UsePieceColor: TPieceColor);
    procedure LoadBoardFromFenString(const AFenString: string;
                                     const ALang: TLocalLangType=llEn);

    function ToString(const UsePieceColor: TPieceColor = pcWhite;
                      const CellWidth: integer = 1;
                      const ALang: TLocalLangType = llEn): string;

    // create all child boards
    procedure ExpandAll(const AMoveColor: TPieceColor; const ALang: TLocalLangType = llEn);

    procedure GetMoves(const Acol: TBoardColType; const Arow: TBoardRowType; var AMoveArray: TPieceMoves); overload;
    procedure GetMoves(const ACell: TCellCoord; var AMoveArray: TPieceMoves); overload;

    function MoveName(const Acol: TBoardColType; const Arow: TBoardRowType; const ALang: TLocalLangType = llEn): string;
  end;

const
  // https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
  // https://it.wikipedia.org/wiki/Notazione_algebrica

  ColorLocal: TColorLocalization = ( // it
                                     ( (Des1: 'B'; Des2: 'Bi'; Full: 'Bianco'),
                                       (Des1: 'N'; Des2: 'Ne'; Full: 'Nero')
                                     ),
                                     // en
                                     ( (Des1: 'W'; Des2: 'Wh'; Full: 'White'),
                                       (Des1: 'B'; Des2: 'Bl'; Full: 'Black')
                                     ),
                                     // fr
                                     ( (Des1: 'B'; Des2: 'Bl'; Full: 'Blank'),
                                       (Des1: 'N'; Des2: 'No'; Full: 'Noir')
                                     )
                                   );

  BoardLocal: TLocalization = (   // it
                                  ( (Des1: 'P'; Des2: 'PB'; Full: 'Pedone Bianco'),
                                    (Des1: 'p'; Des2: 'pn'; Full: 'Pedone Nero'),
                                    (Des1: 'T'; Des2: 'TB'; Full: 'Torre Bianca'),
                                    (Des1: 't'; Des2: 'tn'; Full: 'Torre Nera'),
                                    (Des1: 'C'; Des2: 'CB'; Full: 'Cavallo Bianco'),
                                    (Des1: 'c'; Des2: 'cn'; Full: 'Cavallo Nero'),
                                    (Des1: 'A'; Des2: 'AB'; Full: 'Alfiere Bianco'),
                                    (Des1: 'a'; Des2: 'an'; Full: 'Alfiere Nero'),
                                    (Des1: 'D'; Des2: 'DB'; Full: 'Regina Bianca'),
                                    (Des1: 'd'; Des2: 'dn'; Full: 'Regiona Nera'),
                                    (Des1: 'R'; Des2: 'RB'; Full: 'Re Bianco'),
                                    (Des1: 'r'; Des2: 'rn'; Full: 'Re Nero')
                                  ),
                                  // en
                                  ( (Des1: 'P'; Des2: 'PW'; Full: 'White Pawn'),
                                    (Des1: 'p'; Des2: 'pb'; Full: 'Black Pawn'),
                                    (Des1: 'R'; Des2: 'RW'; Full: 'White Rook'),
                                    (Des1: 'r'; Des2: 'rb'; Full: 'Black Rook'),
                                    (Des1: 'N'; Des2: 'NW'; Full: 'White Knight'),
                                    (Des1: 'n'; Des2: 'nb'; Full: 'Black Knight'),
                                    (Des1: 'B'; Des2: 'BW'; Full: 'White Bishop'),
                                    (Des1: 'b'; Des2: 'bb'; Full: 'Black Bishop'),
                                    (Des1: 'Q'; Des2: 'QW'; Full: 'White Queen'),
                                    (Des1: 'q'; Des2: 'qb'; Full: 'Black Queen'),
                                    (Des1: 'K'; Des2: 'KB'; Full: 'White King'),
                                    (Des1: 'k'; Des2: 'kn'; Full: 'Black King')
                                  ),
                                  // fr
                                  ( (Des1: 'P'; Des2: 'PB'; Full: 'Pion Blanc'),
                                    (Des1: 'p'; Des2: 'pn'; Full: 'Pion Noir'),
                                    (Des1: 'T'; Des2: 'TB'; Full: 'Tour Blanc'),
                                    (Des1: 't'; Des2: 'tn'; Full: 'Tour Noir'),
                                    (Des1: 'C'; Des2: 'CB'; Full: 'Cavalier Blanc'),
                                    (Des1: 'c'; Des2: 'cn'; Full: 'Cavalier Noir'),
                                    (Des1: 'F'; Des2: 'FB'; Full: 'Fou Blanc'),
                                    (Des1: 'f'; Des2: 'fn'; Full: 'Fou Noir'),
                                    (Des1: 'D'; Des2: 'DB'; Full: 'Dame Blanc'),
                                    (Des1: 'd'; Des2: 'dn'; Full: 'Dame Noir'),
                                    (Des1: 'R'; Des2: 'RB'; Full: 'Roi Blanc'),
                                    (Des1: 'r'; Des2: 'rn'; Full: 'Roi Noir')
                                  )

                              );

  BoardColName: TBoardColName = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h');
  BoardRowName: TBoardRowName = ('1', '2', '3', '4', '5', '6', '7', '8');
  CellNames: TCellNames = ( ('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8'),
                            ('b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8'),
                            ('c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8'),
                            ('d1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8'),
                            ('e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8'),
                            ('f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8'),
                            ('g1', 'g2', 'g3', 'g4', 'g5', 'g6', 'g7', 'g8'),
                            ('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8')
                          );


  function IsWhite(APiece: TPieceType): boolean;
  function IsBlack(APiece: TPieceType): boolean;
  function PieceColor(APiece: TPieceType): TPieceColor;

  function DecodeAlgebraicNotation(const AValue: string; var ACell: TCellCoord): boolean;
  function EncodeAlgebraicNotation(const ACell: TCellCoord): string;


implementation

uses StrUtils;

function IsWhite(APiece: TPieceType): boolean;
begin
  result := APiece in [cpPawnWhite, cpRookWhite, cpKnightWhite, cpBishopWhite, cpQueenWhite,  cpKingWhite];
end;

function IsBlack(APiece: TPieceType): boolean;
begin
  result := APiece in [cpPawnBlack, cpRookBlack, cpKnightBlack, cpBishopBlack, cpQueenBlack,  cpKingBlack];
end;

function PieceColor(APiece: TPieceType): TPieceColor;
begin
  case APiece of
     cpPawnWhite,
       cpRookWhite, cpKnightWhite,
       cpBishopWhite, cpQueenWhite,
       cpKingWhite: result :=pcWhite;
     cpPawnBlack,
       cpRookBlack, cpKnightBlack,
       cpBishopBlack, cpQueenBlack,
       cpKingBlack: result :=pcBlack;
  else
    raise Exception.Create('Error in piece color detection!');

  end;
end;

function DecodeAlgebraicNotation(const AValue: string; var ACell: TCellCoord
  ): boolean;
begin
  result :=False;
  try
    case lowercase(AValue)[1] of
       'a': ACell.col:=colA;
       'b': ACell.col:=colB;
       'c': ACell.col:=colC;
       'd': ACell.col:=colD;
       'e': ACell.col:=colE;
       'f': ACell.col:=colF;
       'g': ACell.col:=colG;
       'h': ACell.col:=colH;
    else
      raise exception.CreateFmt('"%s" is not a valid algebraic notation!', [AValue]);
    end;

    case lowercase(AValue)[2] of
       '1': ACell.row:=row1;
       '2': ACell.row:=row2;
       '3': ACell.row:=row3;
       '4': ACell.row:=row4;
       '5': ACell.row:=row5;
       '6': ACell.row:=row6;
       '7': ACell.row:=row7;
       '8': ACell.row:=row8;
    else
      raise exception.CreateFmt('"%s" is not a valid algebraic notation!', [AValue]);
    end;

    result :=True;

  except
    raise;
  end;


end;

function EncodeAlgebraicNotation(const ACell: TCellCoord): string;
begin
  result := '';
  try
    case ACell.col of
       colA: result := 'a';
       colB: result := 'b';
       colC: result := 'c';
       colD: result := 'd';
       colE: result := 'e';
       colF: result := 'f';
       colG: result := 'g';
       colH: result := 'h';
    end;

    result += IntToStr(ord(ACell.row) + 1);

  except
    raise;
  end;

end;



{ TBoardObjHelper }

procedure TBoardObjHelper.Clear;
var col: TBoardColType;
    row: TBoardRowType;
begin
  for col:=colA to colH do
    for row:=row1 to row2 do
      Board[col, row].PieceType:=cpEmpty;

  ActiveColor:=pcWhite;
  Castlings:=[];
  Halfmove:=0;
  Fullmove:=0;
  CanEnPassant:=False;

  if Assigned(ChildBoards) then
     ChildBoards.Clear;
  FreeAndNil(ChildBoards);

end;

procedure TBoardObjHelper.StartPos(const UsePieceColor: TPieceColor);
var col: TBoardColType;
    row,
      rowWhite, rowWhitePawns,
      rowBlack, rowBlackPawns: TBoardRowType;
begin
  ActiveColor:=UsePieceColor;

  // set rows
  case UsePieceColor of
     pcWhite: begin
       rowWhite      := row1;
       rowWhitePawns := row2;
       rowBlack      := row8;
       rowBlackPawns := row7;
     end;
     pcBlack: begin
       rowBlack      := row1;
       rowBlackPawns := row2;
       rowWhite      := row8;
       rowWhitePawns := row7;
     end;
  end;

  // white pieces
  Board[colA, rowWhite].PieceType:=cpRookWhite;
  Board[colB, rowWhite].PieceType:=cpKnightWhite;
  Board[colC, rowWhite].PieceType:=cpBishopWhite;
  Board[colD, rowWhite].PieceType:=cpQueenWhite;
  Board[colE, rowWhite].PieceType:=cpKingWhite;
  Board[colF, rowWhite].PieceType:=cpBishopWhite;
  Board[colG, rowWhite].PieceType:=cpKnightWhite;
  Board[colH, rowWhite].PieceType:=cpRookWhite;

  // white pawns
  for col:=colA to colH do
      Board[col, rowWhitePawns].PieceType:=cpPawnWhite;

  // clear middle
  for col:=colA to colH do
    for row:=row3 to row6 do
      Board[col, row].PieceType:=cpEmpty;

  // black pieces
  Board[colA, rowBlack].PieceType:=cpRookBlack;
  Board[colB, rowBlack].PieceType:=cpKnightBlack;
  Board[colC, rowBlack].PieceType:=cpBishopBlack;
  Board[colD, rowBlack].PieceType:=cpQueenBlack;
  Board[colE, rowBlack].PieceType:=cpKingBlack;
  Board[colF, rowBlack].PieceType:=cpBishopBlack;
  Board[colG, rowBlack].PieceType:=cpKnightBlack;
  Board[colH, rowBlack].PieceType:=cpRookBlack;

  // black pawns
  for col:=colA to colH do
      Board[col, rowBlackPawns].PieceType:=cpPawnBlack;

  // castling
  // ABoard.wh;

end;

{not $DEFINE DEBUG_FEN}
procedure TBoardObjHelper.LoadBoardFromFenString(const AFenString: string;
                                                 const ALang: TLocalLangType = llEn);
var s, FenPiece, sRow, sOut: string;
    row: TBoardRowType;
    col: TBoardColType;
    scanCol, scanFenCol, totFenCol: integer;
begin
  // Here's the FEN for the starting position:
  // rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
  // And after the move 1.e4:
  // rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
  // And then after 1...c5:
  // rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2
  // And then after 2.Nf3:
  // rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

  // todo: localization

  Clear;

  {$IFDEF DEBUG_FEN}
  Writeln('fen: ', AFenString);
  {$ENDIF}

  // read fen record
  s:=ExtractWord(1, AFenString, [' ']);
  {$IFDEF DEBUG_FEN}
  WriteLn('field 1: ' , s);
  {$ENDIF}

  // - - - - - - - - - - - - - - - - -
  // field 1: rows
  // - - - - - - - - - - - - - - - - -

  // debug fen rows
  for row:=row8 downto row1 do begin
    sRow:=ExtractWord(8-ord(row), s, ['/']);
    {$IFDEF DEBUG_FEN}
    WriteLn('  [' ,row, ']: ' , sRow);
    sout:='  ';
    {$ENDIF}
    scanCol:=0;
    while scanCol < Length(sRow) do begin
       inc(scanCol);
       if scanCol = 1 then
          col := colA
       else
          inc(col);

       FenPiece:=copy(sRow, scanCol,1);

       if FenPiece[1] in ['1','2','3','4','5','6','7','8'] then begin
          totFenCol:=strtoint(FenPiece[1]);
          for scanFenCol := 1 to totFenCol do begin
              {$IFDEF DEBUG_FEN}
              sOut += '[ ] ';
              {$ENDIF}
              if scanFenCol > 1 then
                 inc(col);
              Board[col, row].PieceType := cpEmpty;
          end;

       end else begin

         if FenPiece = BoardLocal[ALang, cpPawnWhite].Des1 then begin
            Board[col, row].PieceType := cpPawnWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpPawnWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpPawnBlack].Des1 then begin
            Board[col, row].PieceType := cpPawnBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpPawnBlack].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpRookWhite].Des1 then begin
            Board[col, row].PieceType := cpRookWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpRookWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpRookBlack].Des1 then begin
            Board[col, row].PieceType := cpRookBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpRookBlack].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpKnightWhite].Des1 then begin
            Board[col, row].PieceType := cpKnightWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpKnightWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpKnightBlack].Des1 then begin
            Board[col, row].PieceType := cpKnightBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpKnightBlack].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpBishopWhite].Des1 then begin
            Board[col, row].PieceType := cpBishopWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpBishopWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpBishopBlack].Des1 then begin
            Board[col, row].PieceType := cpBishopBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpBishopBlack].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpQueenWhite].Des1 then begin
            Board[col, row].PieceType := cpQueenWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpQueenWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpQueenBlack].Des1 then begin
            Board[col, row].PieceType := cpQueenBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpQueenBlack].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpKingWhite].Des1 then begin
            Board[col, row].PieceType := cpKingWhite;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpKingWhite].Full;
            {$ENDIF}
         end else if FenPiece = BoardLocal[ALang, cpKingBlack].Des1 then begin
            Board[col, row] .PieceType:= cpKingBlack;
            {$IFDEF DEBUG_FEN}
            sOut += BoardLocal[ALang, cpKingBlack].Full;
            {$ENDIF}
         end;
         {$IFDEF DEBUG_FEN}
         sOut += ' ';
         {$ENDIF}
       end;
    end; // col
    {$IFDEF DEBUG_FEN}
    WriteLn(sOut);
    {$ENDIF}
  end; // for row


  // - - - - - - - - - - - - - - - - -
  // field 2: active color
  // - - - - - - - - - - - - - - - - -
  s:=lowercase(ExtractWord(2, AFenString, [' '])) + ' ';
  case s[1] of
     'w': ActiveColor:=pcWhite;
     'b': ActiveColor:=pcBlack;
  else
     raise exception.CreateFmt('FEN string error in field 2: "%s" is not a valid active color!', [ trim(s) ]);
  end;

  // - - - - - - - - - - - - - - - - -
  // field 3: castling
  // - - - - - - - - - - - - - - - - -
  s:=ExtractWord(3, AFenString, [' ']);
  Castlings:=[];
  if pos('K', s)>0 then
     Castlings+=[castlWhiteKSide];
  if pos('Q', s)>0 then
     Castlings+=[castlWhiteQSide];
  if pos('k', s)>0 then
     Castlings+=[castlBlackKSide];
  if pos('q', s)>0 then
     Castlings+=[castlBlackQSide];

  {$IFDEF DEBUG_FEN}
  Write('Castling: ');
  if castlWhiteKSide in Castlings then Write(castlWhiteKSide, '; ');
  if castlWhiteQSide in Castlings then Write(castlWhiteQSide, '; ');
  if castlBlackKSide in Castlings then Write(castlBlackKSide, '; ');
  if castlBlackQSide in Castlings then Write(castlBlackQSide, '; ');
  Writeln;
  {$ENDIF}

  // - - - - - - - - - - - - - - - - -
  // field 4: en passant
  // - - - - - - - - - - - - - - - - -
  CanEnPassant:=False;
  s:=ExtractWord(4, AFenString, [' ']);
  if length(s)=2 then
     CanEnPassant:=DecodeAlgebraicNotation(s, EnPassant);

  {$IFDEF DEBUG_FEN}
  Write('En passant: ', s, ' Can = ', CanEnPassant, ' ');
  if CanEnPassant then
     WriteLn('[', EnPassant.col, ',', EnPassant.row, ']')
  else
     Writeln;
  {$ENDIF}


  // - - - - - - - - - - - - - - - - -
  // field 5: halfmove
  // - - - - - - - - - - - - - - - - -
  s:=ExtractWord(5, AFenString, [' ']);
  try
    Halfmove:=StrToInt(s);
    {$IFDEF DEBUG_FEN}
    Write('Halfmove = ', Halfmove, ' ');
    {$ENDIF}
  except
    raise exception.CreateFmt('"%s" is not a valid halfmove!', [s]);
  end;

  // - - - - - - - - - - - - - - - - -
  // field 6: fullmove
  // - - - - - - - - - - - - - - - - -
  s:=ExtractWord(6, AFenString, [' ']);
  try
    Fullmove:=StrToInt(s);
    {$IFDEF DEBUG_FEN}
    Writeln('Fullmove = ', Fullmove, ' ');
    {$ENDIF}
  except
    raise exception.CreateFmt('"%s" is not a valid fullmove!', [s]);
  end;

end;

function TBoardObjHelper.ToString(const UsePieceColor: TPieceColor;
  const CellWidth: integer; const ALang: TLocalLangType): string;
const
      ROW_HEADER_1      = '   abcdefgh';
      ROW_HEADER_3      = '     A   B   C   D   E   F   G   H  ';
      HORIZ_SEPAARTOR_3 = '   +---+---+---+---+---+---+---+---+';
      ROW_HEADER_4      = '     A    B    C    D    E    F    G    H   ';
      HORIZ_SEPAARTOR_4 = '   +----+----+----+----+----+----+----+----+';

      BOARD_HEADER_1 = ROW_HEADER_1 + LineEnding;
      BOARD_HEADER_3 = ROW_HEADER_3 + LineEnding + HORIZ_SEPAARTOR_3 + LineEnding;
      BOARD_HEADER_4 = ROW_HEADER_4 + LineEnding + HORIZ_SEPAARTOR_4 + LineEnding;
      BOARD_BOTTOM_1 = ROW_HEADER_1 + LineEnding;
      BOARD_BOTTOM_3 = ROW_HEADER_3 + LineEnding;
      BOARD_BOTTOM_4 = ROW_HEADER_4 + LineEnding;

var col: TBoardColType;
    row: TBoardRowType;
    p:TPiece;
    sOut, sc: string;
begin
  // test
  if not (CellWidth in [1,3,4]) then
     raise Exception.CreateFmt('Unable to convert cell using width %d!', [CellWidth]);

  sOut:='';
  result:='';

  // convert ToString
  // WriteLn('   abcdefgh');
  case CellWidth of
    1: sOut:=BOARD_HEADER_1;
    3: sOut:=BOARD_HEADER_3;
    4: sOut:=BOARD_HEADER_4;
  end;

  for row:=row8 downto row1 do begin
    // write(' ', ord(row)+1, ' ');
    case CellWidth of
      1: sout+=Format(' %d ', [ord(row)+1]);
      3: sOut+=Format(' %d |', [ord(row)+1]);
      4: sOut+=Format(' %d |', [ord(row)+1]);
    end;

    for col:=colA to colH do begin
      p:=Board[col, row];
      case p.PieceType of
        cpEmpty: // Write('.');
                 case CellWidth of
                   1: sOut+='.';
                   3: sOut+='   |';
                   4: sOut+='    |';
                 end;
      else
        case CellWidth of
          1: sOut += BoardLocal[ALang, p.PieceType ].Des1;
          3: sOut += ' ' + BoardLocal[ALang, p.PieceType ].Des1 + ' |';
          4: sOut += ' ' + BoardLocal[ALang, p.PieceType ].Des2 + ' |';
        end;

      end;
    end;
    // write(' ', ord(row)+1);
    sOut += Format(' %d', [ord(row)+1]);
    case row of
       row8: sOut += '    ' + ColorLocal[ALang, ActiveColor].Full;
       row7: begin
               sOut += '    ' + ColorLocal[ALang, pcWhite].Des1+' =';
               if castlWhiteKSide in Castlings then begin sOut += ' OO'; end;
               if castlWhiteQSide in Castlings then begin sOut += ' OOO'; end;
               sOut += ' / ' + ColorLocal[ALang, pcBlack].Des1+' =';
               if castlBlackKSide in Castlings then begin sOut += ' OO'; end;
               if castlBlackQSide in Castlings then begin sOut += ' OOO'; end;
             end;
       row6: if CanEnPassant then begin
                sOut += '    En Passant ' + EncodeAlgebraicNotation(EnPassant);
             end;
    end;
    sOut += LineEnding;
    case CellWidth of
      3: sOut += HORIZ_SEPAARTOR_3 + LineEnding;
      4: sOut += HORIZ_SEPAARTOR_4 + LineEnding;
    end;

  end;
  // WriteLn('   abcdefgh');
  // WriteLn;
  case CellWidth of
    1: sOut += BOARD_BOTTOM_1;
    3: sOut += BOARD_BOTTOM_3;
    4: sOut += BOARD_BOTTOM_4;
  end;

  Result:=sOut;

end;

procedure TBoardObjHelper.ExpandAll(const AMoveColor: TPieceColor;
  const ALang: TLocalLangType);
var col: TBoardColType;
    row: TBoardRowType;
    p:TPiece;
    // StartBoard
    SetOfPieces: set of TPieceType;
    Moves: TPieceMoves;
    scan: integer;
    Cell: TCellCoord;
    sOutLine: string;
begin
  // allocate resources
  if not Assigned(ChildBoards) then
     ChildBoards:=TStringToPointerTree.Create(false);

  // init
  case AMoveColor of
    pcWhite: SetOfPieces := [cpPawnWhite, cpRookWhite, cpKnightWhite, cpBishopWhite, cpQueenWhite,  cpKingWhite];
    pcBlack: SetOfPieces := [cpPawnBlack, cpRookBlack, cpKnightBlack, cpBishopBlack, cpQueenBlack,  cpKingBlack];
  end;

  Writeln('Move: ', AMoveColor);

  // loop on cells
  for row:=row8 downto row1 do begin
    // write(' ', ord(row)+1, ' ');

    for col:=colA to colH do begin
      p:=Board[col, row];
      Moves.MaxMove:=-1;

      if p.PieceType in SetOfPieces then begin
         sOutLine := Format('[%s] %s / %-15.15s : ',
                            [ CellNames[col, row],
                              BoardLocal[ALang, p.PieceType ].Des1,
                              BoardLocal[ALang, p.PieceType ].Full] );
         // if p.PieceType in [cpPawnWhite, cpPawnBlack] then begin
            Cell.col:=col;
            Cell.row:=row;
            GetMoves(Cell, Moves);
         // end;

         if Moves.MaxMove >= 0 then begin
            for scan:=0 to Moves.MaxMove do begin;
                sOutLine += EncodeAlgebraicNotation( Moves.Moves[scan] ) + ' ';
            end;
         end; // if moves >= 0

         WriteLn(sOutLine);
      end; // if pieces in setofpieces

    end; // for col

  end; // for row

end;

procedure TBoardObjHelper.GetMoves(const Acol: TBoardColType;
  const Arow: TBoardRowType; var AMoveArray: TPieceMoves);
var Cell: TCellCoord;
begin
  Cell.col:=Acol;
  Cell.row:=Arow;
  GetMoves(Cell, AMoveArray);
end;

procedure TBoardObjHelper.GetMoves(const ACell: TCellCoord;
  var AMoveArray: TPieceMoves);

var Piece: TPiece;

  procedure _AddMove(const ANewCell: TCellCoord);
  begin
    inc(AMoveArray.MaxMove);
    AMoveArray.Moves[AMoveArray.MaxMove] := ANewCell;
  end; // _AddMove

  procedure _GetPawnMoves;

      function _GetPawnMoves_Forward1_Capture: boolean;
      var NewCell: TCellCoord;
          CanTest, CanMove, Captured: boolean;
          SaveCol: TBoardColType;
          CapturedType: TPieceType;
      begin
        result := False;

        // - - - - - - - - - - - - - - - - - - - -
        // move forward by 1
        // - - - - - - - - - - - - - - - - - - - -
        CanMove:=False;
        CanTest:=False;
        NewCell:=ACell;

        // pawn white
        if Piece.PieceType = cpPawnWhite then begin
           if (ACell.row < row8) then begin
              CanTest:=True;
              inc(NewCell.row);
           end else begin
              exit;
           end;
        end; // pawn white

        // pawn black
        if Piece.PieceType = cpPawnBlack then begin
           if (ACell.row > row1) then begin
              CanTest:=True;
              dec(NewCell.row);
           end else begin
              exit;
           end;
        end; // pawn black

        // test
        if CanTest then begin
           CanMove := Board[NewCell.col, NewCell.row].PieceType = cpEmpty;
           if CanMove then begin
              _AddMove(NewCell);
              result := True;
           end;
        end; // CanTest


        // - - - - - - - - - - - - - - - - - - - -
        // capture sx
        // - - - - - - - - - - - - - - - - - - - -

        SaveCol := NewCell.col;

        if ACell.col > colA then begin
           Captured:=False;
           dec(NewCell.col);
           CapturedType := Board[NewCell.col, NewCell.row].PieceType;
           Captured:=CapturedType <> cpEmpty;
           if Captured then begin
              // pawn white
              if Piece.PieceType = cpPawnWhite then
                 Captured:=IsBlack(CapturedType);
              // pawn black
              if Piece.PieceType = cpPawnBlack then
                 Captured:=IsWhite(CapturedType);
              // add
              if Captured then
                 _AddMove(NewCell);
           end;

        end; // capture sx

        // - - - - - - - - - - - - - - - - - - - -
        // capture dx
        // - - - - - - - - - - - - - - - - - - - -
        NewCell.col := SaveCol;

        if ACell.col < colH then begin
           Captured:=False;
           inc(NewCell.col);
           CapturedType := Board[NewCell.col, NewCell.row].PieceType;
           Captured:=CapturedType <> cpEmpty;
           if Captured then begin
              // pawn white
              if Piece.PieceType = cpPawnWhite then
                 Captured:=IsBlack(CapturedType);
              // pawn black
              if Piece.PieceType = cpPawnWhite then
                 Captured:=IsWhite(CapturedType);
              // add
              if Captured then
                 _AddMove(NewCell);
           end;

        end; // capture dx

      end; // _GetPawnMoves_Forward1

      procedure _GetPawnMoves_Forward2;
      var NewCell: TCellCoord;
          CanTest, CanMove: boolean;
      begin
        // - - - - - - - - - - - - - - - - - - - -
        // move forward by 2
        // - - - - - - - - - - - - - - - - - - - -
        CanMove:=False;
        CanTest:=False;
        NewCell:=ACell;

        // pawn white
        if Piece.PieceType = cpPawnWhite then begin
           if (ACell.row = row2) then begin
              CanTest:=True;
              NewCell.row:=row4;
           end;
        end; // pawn white

        // pawn black
        if Piece.PieceType = cpPawnBlack then begin
           if (ACell.row = row7) then begin
              CanTest:=True;
              NewCell.row:=row5;
           end;
        end; // pawn black

        // test
        if CanTest then begin
           CanMove := Board[NewCell.col, NewCell.row].PieceType = cpEmpty;
           if CanMove then
              _AddMove(NewCell);
        end; // CanTest

      end; // _GetPawnMoves_Forward2

      {
      procedure _GetPawnMoves_CaptureSx;
      var NewCell: TCellCoord;
          CanTest, CanMove: boolean;
          Captured: TPieceType;
      begin
        // - - - - - - - - - - - - - - - - - - - -
        // capture sx
        // - - - - - - - - - - - - - - - - - - - -
        CanMove:=False;
        CanTest:=False;
        NewCell:=ACell;

        // pawn white
        if Piece.PieceType = cpPawnWhite then begin
           if (ACell.row < row8) and (ACell.col > colA) then begin
              inc(NewCell.row);
              dec(NewCell.col);
              Captured := Board[NewCell.col, NewCell.row].PieceType;
              if Captured <> cpEmpty then
                 CanMove:=IsBlack(Captured);
           end;
        end; // pawn white

        // pawn black
        if Piece.PieceType = cpPawnWhite then begin
           if (ACell.row > row1) and (ACell.col > colA) then begin
              dec(NewCell.row);
              dec(NewCell.col);
              Captured := Board[NewCell.col, NewCell.row].PieceType;
              if Captured <> cpEmpty then
                 CanMove:=IsWhite(Captured);
           end;
        end; // pawn white

        // test
        if CanMove then
           _AddMove(NewCell);

      end; // _GetPawnMoves_CaptureSx
      }

  begin // _GetPawnMoves

    if _GetPawnMoves_Forward1_Capture then
       _GetPawnMoves_Forward2;
    // _GetPawnMoves_CaptureSx;

  end; // _GetPawnMoves

  procedure _GetRookMoves;
  begin

  end;

  procedure _GetKnightMoves;
  type TKOffset = record
          ColOffset: integer;
          RowOffset: integer;
       end;
       TOffsets = array[0..7] of TKOffset;
  const KOffsets: TOffsets = ( (ColOffset:  1; RowOffset:  2),
                               (ColOffset:  2; RowOffset:  1),
                               (ColOffset:  2; RowOffset: -1),
                               (ColOffset:  1; RowOffset: -2),
                               (ColOffset: -1; RowOffset: -2),
                               (ColOffset: -2; RowOffset: -1),
                               (ColOffset: -2; RowOffset:  1),
                               (ColOffset: -1; RowOffset:  2) );
  var TestMoves: TPieceMoves;
      NewCell: TCellCoord;
      // NewCol: TBoardColType;
      // NewRow: TBoardRowType;
      NewColNum: integer;
      NewRowNum: integer;
      NewCellContent: TPieceType;
      scan: integer;

      function _KnightCanMove: boolean;
      begin
        if NewCellContent = cpEmpty then begin
           result := True;
        end else begin
           if Piece.PieceType = cpKnightWhite then
              result := IsBlack(NewCellContent)
           else
              result := IsWhite(NewCellContent);
        end;
      end; // _KnightCanMove


  begin
     TestMoves.MaxMove:=-1;
     // detect moves to test

     for scan := 0 to 7 do begin
       NewColNum := ord(ACell.col) + KOffsets[scan].ColOffset;
       NewRowNum := ord(ACell.row) + KOffsets[scan].RowOffset;
       if (NewColNum in [0..7]) and (NewRowNum in [0..7]) then begin
          NewCell.col:=TBoardColType(NewColNum);
          NewCell.row:=TBoardRowType(NewRowNum);
          NewCellContent := Board[NewCell.col, NewCell.row].PieceType;
          if _KnightCanMove then
             _AddMove(NewCell);
       end;
     end;


     {
     NewColNum := ord(ACell.col) + 1;
     if NewColNum <= ord(colH) then begin

        // 1'clock -> N/NE
        NewRowNum := ord(ACell.row) + 2;
        if NewRowNum <= ord(row8) then begin
           NewCell.col:=TBoardColType(NewColNum);
           NewCell.row:=TBoardRowType(NewRowNum);
           CapturedType := Board[NewCell.col, NewCell.row].PieceType;
           if _KnightCanMove then
              _AddMove(NewCell);
        end;

        // 5'clock -> S/SE
        NewRowNum := ord(ACell.row) - 2;
        if NewRowNum >= ord(row1) then begin
           NewCell.col:=TBoardColType(NewColNum);
           NewCell.row:=TBoardRowType(NewRowNum);
           CapturedType := Board[NewCell.col, NewCell.row].PieceType;
           if _KnightCanMove then
              _AddMove(NewCell);
        end;

     end;
     }


  end;

  procedure _GetBishopMoves;
  begin

  end;

  procedure _GetQueenMoves;
  begin

  end;

  procedure _GetKingMoves;
  begin

  end;

begin
  // get possible moves for a Piece in a Cell
  AMoveArray.MaxMove:=-1;
  Piece:=Board[ACell.col, ACell.row];
  case Piece.PieceType of
     cpPawnWhite,   cpPawnBlack  : _GetPawnMoves;
     cpRookWhite,   cpRookBlack  : _GetRookMoves;
     cpKnightWhite, cpKnightBlack: _GetKnightMoves;
     cpBishopWhite, cpBishopBlack: _GetBishopMoves;
     cpQueenWhite,  cpQueenBlack : _GetQueenMoves;
     cpKingWhite,   cpKingBlack  : _GetKingMoves;
  end;

end;

function TBoardObjHelper.MoveName(const Acol: TBoardColType;
  const Arow: TBoardRowType; const ALang: TLocalLangType): string;
begin
  // todo: capture indicator

  result := '';
  case Board[Acol, Arow].PieceType of
     cpEmpty: result := '?' + CellNames[Acol, Arow];
     cpPawnBlack, cpPawnWhite: result := CellNames[Acol, Arow];
     cpRookWhite,
       cpRookBlack,
       cpKnightWhite,
       cpKnightBlack,
       cpBishopWhite,
       cpBishopBlack,
       cpQueenWhite,
       cpQueenBlack,
       cpKingWhite,
       cpKingBlack    : result := BoardLocal[ALang, Board[Acol, Arow].PieceType ].Des1 + CellNames[Acol, Arow];
  end;

end;

{ TBoardObj }

constructor TBoardObj.Create;
begin
  Castlings:=[];
  ActiveColor:=pcWhite;
  ChildBoards:=nil;
end;

destructor TBoardObj.Destroy;
begin
  if Assigned(ChildBoards) then begin
     ChildBoards.Clear;
     ChildBoards.Free;
  end;

  inherited Destroy;
end;

end.

