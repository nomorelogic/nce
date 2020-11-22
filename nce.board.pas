unit nce.board;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPieceColor = (pcWhite, pcBlack);

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


  TLocalLangType  = (llIt, llEn, llFr);
  TLocalLangRange = llIt..llFr;

  TLocalization = array[TLocalLangRange] of TDescriptionArray;

  TPiece = record
    Piece: TPieceType;
  end;

  TBoardColType  = (colA, colB, colC, colD, colE, colF, colG, colH);
  TBoardColRange = colA..colH;
  TBoardRowType  = (row1, row2, row3, row4, row5, row6, row7, row8);
  TBoardRowRange = row1..row8;

  // TBoardRowArray = array[TBoardRowRange] of TPiece;
  // TBoard = array[TBoardColRange] of TBoardRowArray;
  TBoard = array[TBoardColRange] of array[TBoardRowRange] of TPiece;


  { TBoardObj }

  TBoardObj = class
  public
    constructor Create;
  public
    Name: string;
    Board: TBoard;
    WhiteCanCastelingOO,
      WhiteCanCastelingOOO,
      BlackCanCastelingOO,
      BlackCanCastelingOOO: boolean;
  end;


  { TBoardObjHelper }

  TBoardObjHelper = class helper for TBoardObj
    procedure Clear;
    procedure StartPos(const UsePieceColor: TPieceColor);
    function ToString(const UsePieceColor: TPieceColor = pcWhite;
                      const CellWidth: integer = 1;
                      const ALang: TLocalLangType = llEn): string;
  end;

const
  // https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
  // https://it.wikipedia.org/wiki/Notazione_algebrica
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
                                    (Des1: 'R'; Des2: 'RB'; Full: 'White King'),
                                    (Des1: 'r'; Des2: 'rn'; Full: 'Black King')
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


implementation

{ TBoardObjHelper }

procedure TBoardObjHelper.Clear;
var col: TBoardColType;
    row: TBoardRowType;
begin
  for col:=colA to colH do
    for row:=row1 to row2 do
      Board[col, row].Piece:=cpEmpty;
end;

procedure TBoardObjHelper.StartPos(const UsePieceColor: TPieceColor);
var col: TBoardColType;
    row,
      rowWhite, rowWhitePawns,
      rowBlack, rowBlackPawns: TBoardRowType;
begin
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
  Board[colA, rowWhite].Piece:=cpRookWhite;
  Board[colB, rowWhite].Piece:=cpKnightWhite;
  Board[colC, rowWhite].Piece:=cpBishopWhite;
  Board[colD, rowWhite].Piece:=cpQueenWhite;
  Board[colE, rowWhite].Piece:=cpKingWhite;
  Board[colF, rowWhite].Piece:=cpBishopWhite;
  Board[colG, rowWhite].Piece:=cpKnightWhite;
  Board[colH, rowWhite].Piece:=cpRookWhite;

  // white pawns
  for col:=colA to colH do
      Board[col, rowWhitePawns].Piece:=cpPawnWhite;

  // clear middle
  for col:=colA to colH do
    for row:=row3 to row6 do
      Board[col, row].Piece:=cpEmpty;

  // black pieces
  Board[colA, rowBlack].Piece:=cpRookBlack;
  Board[colB, rowBlack].Piece:=cpKnightBlack;
  Board[colC, rowBlack].Piece:=cpBishopBlack;
  Board[colD, rowBlack].Piece:=cpQueenBlack;
  Board[colE, rowBlack].Piece:=cpKingBlack;
  Board[colF, rowBlack].Piece:=cpBishopBlack;
  Board[colG, rowBlack].Piece:=cpKnightBlack;
  Board[colH, rowBlack].Piece:=cpRookBlack;

  // black pawns
  for col:=colA to colH do
      Board[col, rowBlackPawns].Piece:=cpPawnBlack;

  // castling
  // ABoard.wh;

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
    sOut: string;
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
      case p.Piece of
        cpEmpty: // Write('.');
                 case CellWidth of
                   1: sOut+='.';
                   3: sOut+='   |';
                   4: sOut+='    |';
                 end;
      else
        case CellWidth of
          1: sOut += BoardLocal[ALang, p.Piece ].Des1;
          3: sOut += ' ' + BoardLocal[ALang, p.Piece ].Des1 + ' |';
          4: sOut += ' ' + BoardLocal[ALang, p.Piece ].Des2 + ' |';
        end;

      end;
    end;
    // write(' ', ord(row)+1);
    sOut += Format(' %d', [ord(row)+1]) + LineEnding;
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

{ TBoardObj }

constructor TBoardObj.Create;
begin
  WhiteCanCastelingOO:=False;
  WhiteCanCastelingOOO:=False;
  BlackCanCastelingOO:=False;
  BlackCanCastelingOOO:=False;
end;

end.
