program nce;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //LazUTF8,
  Classes, SysUtils, CustApp, nce.board
  { you can add units after this };


  // -l en -o w -p startpos -w 1 --fen "3"

type

  { TNomorelogicChessEngine }

  TNomorelogicChessEngine = class(TCustomApplication)
  private
    FBoardObj: TBoardObj;
    FLanguage: TLocalLangType;
    FOwnColor: TPieceColor;

    // procedure ClearBoard(var ABoard: TBoard); // deprecated
    procedure ClearBoard(const ABoardObj: TBoardObj);

    // procedure ResetBoard(const UsePieceColor: TPieceColor; var ABoard: TBoard);
    procedure StartPosition(const ABoardObj: TBoardObj; const UsePieceColor: TPieceColor);
    procedure FenPosition(const ABoardObj: TBoardObj; const AFenString: string);
    procedure DoStringToBoard(const AValue: string; var ABoard: TBoard);

  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

  public
    property Language: TLocalLangType read FLanguage write FLanguage;
    property BoardObj: TBoardObj read FBoardObj write FBoardObj;
    property OwnColor: TPieceColor read FOwnColor write FOwnColor;
  end;

{ TNomorelogicChessEngine }

// procedure TNomorelogicChessEngine.ClearBoard(var ABoard: TBoard);
// var col: TBoardColType;
//     row: TBoardRowType;
// begin
//   for col:=colA to colH do
//     for row:=row1 to row2 do
//       ABoard[col, row].Piece:=cpEmpty;
// end;

procedure TNomorelogicChessEngine.ClearBoard(const ABoardObj: TBoardObj);
begin
  ABoardObj.Clear;
end;

procedure TNomorelogicChessEngine.StartPosition(const ABoardObj: TBoardObj;
  const UsePieceColor: TPieceColor);
begin
  ABoardObj.StartPos(UsePieceColor);
end;

procedure TNomorelogicChessEngine.FenPosition(const ABoardObj: TBoardObj;
  const AFenString: string);
begin
  ABoardObj.LoadBoardFromFenString(AFenString);
end;


procedure TNomorelogicChessEngine.DoStringToBoard(const AValue: string;
  var ABoard: TBoard);
begin
  // ClearBoard(ABoard);
  // ABoard[colC, row3].PieceType:=cpKingWhite;
end;


procedure TNomorelogicChessEngine.DoRun;
var ErrorMsg: String;
    s:string;
    l: boolean;
    i: integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hopwlmcxf', ['help', 'write', 'fen']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;


  // init
  FBoardObj:=nil;

  try

    { add your program here }
    // DoStringToBoard('', FBoard);
    // WriteBoardDes1(FLanguage, FBoard);

    if HasOption('l', 'lang') then begin
       s:=UpperCase(GetOptionValue('l'));
       case s of
          'IT': FLanguage:=llIt;
          'EN': FLanguage:=llEn;
          'FR': FLanguage:=llFr;
       else
          raise exception.CreateFmt('Language %s not yet implemented!', [s]);
       end;
    end;

    if HasOption('o', 'own') then begin
       s:=UpperCase(GetOptionValue('o'));
       if (s<>'') and (s[1]='B') then
          FOwnColor:=pcBlack;
    end;

    if HasOption('p', 'position') then begin
       if not Assigned(FBoardObj) then
          FBoardObj:=TBoardObj.Create;

       s:=UpperCase(GetOptionValue('p'));
       if s = 'STARTPOS' then
          // ResetBoard(OwnColor, FBoard);
          StartPosition(BoardObj, OwnColor);
    end;

    if HasOption('f', 'fen') then begin
       if not Assigned(FBoardObj) then
          FBoardObj:=TBoardObj.Create;

       l:=False;
       i:=FindOptionIndex('f', l);
       if i<0 then
          i:=FindOptionIndex('fen', l);
       if i>0 then begin
          s:=Params[ i + 1 ];
          if s = '1' then s:='rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';
          if s = '2' then s:='rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1';
          if s = '3' then s:='rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2';
          if s = '4' then s:='rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2';
          FenPosition(BoardObj, s);
       end;
    end;

    if HasOption('w', 'write') then begin
       s:=UpperCase(GetOptionValue('w'));
       if s = '' then begin
          l:=True;
          s:=Params[ FindOptionIndex('write', l) + 1 ];
       end;
       if s <> '' then
          case s of
             '1': WriteLn(BoardObj.ToString(OwnColor, 1, Language));
             '3': WriteLn(BoardObj.ToString(OwnColor, 3, Language));
             '4': WriteLn(BoardObj.ToString(OwnColor, 4, Language));
             // 'F', 'FULL': WriteBoardDes4(FLanguage, Board);
          else
             raise exception.CreateFmt('Unable to write board in mode %s!', [s]);
          end;
    end;


    if HasOption('x', 'expand') then begin
       s:=UpperCase(GetOptionValue('x'));
       if s = 'ALL' then
          BoardObj.ExpandAll(OwnColor, Language);
    end;



  finally

    // free
    FreeAndNil(FBoardObj);

    // stop program loop
    Terminate;

  end;
end;

constructor TNomorelogicChessEngine.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  // ionit
  FLanguage:=llIt;


end;

destructor TNomorelogicChessEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TNomorelogicChessEngine.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('Options:');
  writeln('-l', #9, 'language {it|en}, default = it');
  writeln('-o', #9, 'own color {b|w}, default = w');
  writeln('-p', #9, 'position {startpos|..} ');
  writeln('-w', #9, 'write board to console {1|2}');

  writeln('Examples:');
  writeln('./nce -l en -o w -p startpos -w 1');
  writeln('./nce -l en --write 3 -f 3');

end;

var
  Application: TNomorelogicChessEngine;
begin
  Application:=TNomorelogicChessEngine.Create(nil);
  Application.Title:='Nomorelogic Chess Engine';
  Application.Run;
  Application.Free;
end.

