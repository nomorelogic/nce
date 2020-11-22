program nce;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  //LazUTF8,
  Classes, SysUtils, CustApp, nce.board
  { you can add units after this };

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


procedure TNomorelogicChessEngine.DoStringToBoard(const AValue: string;
  var ABoard: TBoard);
begin
  // ClearBoard(ABoard);
  // ABoard[colC, row3].Piece:=cpKingWhite;
end;


procedure TNomorelogicChessEngine.DoRun;
var ErrorMsg: String;
    s:string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hopwlmc', 'help');
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

    FOwnColor:=pcWhite;
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

    if HasOption('w', 'write') then begin
       s:=UpperCase(GetOptionValue('w'));
       case s of
          '1': WriteLn(BoardObj.ToString(OwnColor, 1, Language));
          '3': WriteLn(BoardObj.ToString(OwnColor, 3, Language));
          '4': WriteLn(BoardObj.ToString(OwnColor, 4, Language));
          // 'F', 'FULL': WriteBoardDes4(FLanguage, Board);
       else
          raise exception.CreateFmt('Unable to write board in mode %s!', [s]);
       end;
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
  writeln('./nce -l:en -o:w -p:startpos -w:1');

end;

var
  Application: TNomorelogicChessEngine;
begin
  Application:=TNomorelogicChessEngine.Create(nil);
  Application.Title:='Nomorelogic Chess Engine';
  Application.Run;
  Application.Free;
end.
