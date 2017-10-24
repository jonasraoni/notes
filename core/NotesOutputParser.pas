//
//    NotesOutputParser - parser para a saída de compiladores/interpretadores
//
//    Notes, https://github.com/jonasraoni/notes
//    Copyright (C) 2003-2004, Equipe do Notes.
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//


(*
  @abstract(NotesOutputParser - parser para a saída de compiladores/interpretadores.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Esta unit possuí a classe @link(TNotesOutputParser) que é usada pelo Notes
  para "entender" a saída de compiladores. A classe pode funcionar tanto no
  modo automático (interpreta a saída com base nos padrões da maioria dos compiladores)
  ou não-automático. Para usar está classe, use a variável @code(OutputParser)
  para acessar os métodos e propriedades desta classe (não é necessário criá-la).
*)
unit NotesOutputParser;

interface

uses Classes;

type

  TNotesOutputParser = class(TObject)
  private
    fErrorStr: string;
    fAuto: boolean;
    fFileName: string;
    fLine: integer;
    fValidFiles: TStringList;
    fREFilePos: string;
    fRELinePos: string;
    fRE: string;
    function getValidFiles: string;
    procedure setValidFiles(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Interpreta @code(ErrorStr)
    procedure Parse;
    // String que contém o erro a ser interpretado
    property ErrorStr: string read fErrorStr write fErrorStr;
    // Deve interpretar automaticamente?!
    property Auto: boolean read fAuto write fAuto;
    // Após interpretado, contém o nome do arquivo em que ocorreu o erro
    property FileName: string read fFileName;
    // Após interpretado, contém a linha em que ocorreu o erro
    property Line: integer read fLine;
    // Expressão regular para fazer o parsing da linha com o erro
    property Regex: string read fRE write fRE;
    // Posição da RE que captura o valor da linha
    property RELinePos: string read fRELinePos write fRELinePos;
    // Posição da RE em que é capturado o valor do arquivo
    property REFilePos: string read fREFilePos write fREFilePos;
    // Lista de arquivos válidos (arquivos referentes ao projeto atual e/ou
    // arquivos que estão abertos no Notes). Essa lista é usada para completar
    // nomes de arquivos incompletos.
    property ValidFiles: string read getValidFiles write setValidFiles;
  end;

Var
  outputParser: TNotesOutputParser;

implementation

uses FastStrings, StrUtils, SysUtils, NotesUtils, RegExpr;

function getTextBtw(const S, StartStr, EndStr: string): string;
Var
  intInitPos: integer;
  intEndPos: integer;
Begin
   if StartStr <> '' then
     intInitPos:= FastPosNoCase(S,StartStr,length(S),length(StartStr),3)
   else
     intInitPos:= 1;

   if intInitPos > 0 then
   begin
     intInitPos:= length(StartStr) + intInitPos;

     if EndStr <> '' then
       intEndPos:=  FastPosNoCase(S,EndStr,length(S),Length(EndStr),intInitPos)
     else
       intEndPos:= length(S);

   end else
   begin
     Result:= EmptyStr;
     Exit;
   end;


   if (intEndPos>0) and (intEndPos-intInitPos>0) then
   begin
     SetLength(Result, intEndPos-intInitPos);
     FastCharMove(S[intInitPos],Result[1], intEndPos-intInitPos);
   end;
end;

function IsCharInStr(const S: string; AChar: char): boolean;
begin
  Result := (FastPos(S, string(AChar), length(S), 1, 1) > 0);
end;

function getTextUntil(const S, delimiters: string): string;
var
  Len, I: integer;
begin
  Len:= length(S);
  I:= 3;
  while (I < Len) and (IsCharInStr(Delimiters, S[I]) = false) do
    Inc(I);

  Result:= Copy(S, 0, I-1);
end;

procedure AdjustFileNameToOS(Var FileName: string);
begin
  if IsCharInStr(FileName, '/') then
    FileName:= StringReplace(FileName, '/', '\', [rfReplaceAll]);
end;

function HasValidBracketIndicator(const S: string): boolean;
Var
  I, Len: integer;
begin
  Result:= false;
  Len:= length(S);

  I:= FastPos(S, '(', Len, 1, 1);
  if (Len > I) and (I > 1) and (S[I-1] <> '"') and
     (S[I-1] <> '`') and (S[I-1] <> #39) and
     (S[I+1] in ['0','1','2','3','4','5','6','7','8','9']) then
       Result:= true;

end;

function isFileValid(var AFileName: string; ValidFilesList: TStringList): boolean;
Var
  I, Len, ILen: integer;
begin
  Result:= false;

  AdjustFileNameToOS(AFileName);

  Len:= length(AFileName);

  if Len < 1 then Exit;

  Result:= FileExists(AFileName);
  // se false, tenta ver se não consegue pegar o nome correto
  if (Result = False) and (Assigned(ValidFilesList)) then
  begin
    for I := 0 to ValidFilesList.Count - 1 do
    begin
      ILen:= length(ValidFilesList.Strings[I]);
      ILen:= ILen - Len;
      if (ILen > 0) and (SameText(Copy(ValidFilesList.Strings[I], ILen , Len), AFileName)) then
      begin
        AFileName:= ValidFilesList.Strings[I];
        Result:= true;
        Exit;
      end;
    end;
  end;

end;

procedure AutoParseOutput(const Output: string; var Line: integer; var FileName: string; IncludeFiles: TStringList);
var
  I: integer;
  S: string;
begin
  // vamos começar pelos casos mais específicos e tentar os caso mais gerais se os primeiros falharem

  { Descobrindo o nome do arquivo  }
  // File "%arquivo%"
  S:=  getTextBtw(Output, 'File "', '"');
  if isFileValid(S, IncludeFiles) then
  begin
    FileName:= S;
  end else
  begin
    // entre "]" e "(". Exemplo: "] ExtractDocFromStr.pas("
    S:=  getTextBtw(Output, ']', '(');
    if isFileValid(S, IncludeFiles) then
    begin
      FileName:= S;
    end else
    begin
      // entre "in" e "on". Exemplo: "in /home/ftp/Mirrors/PHP/error.php on"
      S:=  getTextBtw(Output, 'in ', ' on');
      if isFileValid(S, IncludeFiles) then
      begin
        FileName:= S;
      end else
      begin
        // entre "/" e ","
        S:=  getTextBtw(Output, '/', ',');
        if isFileValid(S, IncludeFiles) then
        begin
          FileName:= S;
        end else
        begin
        // at /mnt/windows/ruby-1.6.8/sample/occur.pl line 8.
          S:=  getTextBtw(Output, 'at ', ' line');
          if isFileValid(S, IncludeFiles) then
          begin
            FileName:= S;
          end else
          begin
          // arquivo mts vezes é colocado no início da linha até "(".
            if HasValidBracketIndicator(Output) then
              S:= getTextUntil(Output, '(')
            else
              S:= getTextUntil(Output, ',:[');

            if isFileValid(S, IncludeFiles) then
            begin
              FileName:= S;
            end;
          end;
        end;
      end;
    end;
  end;

  { descobrindo a linha }
  //%arquivo%(%linha%)
  S:= getTextBtw(Output, '(', ')');
  if (S <> EmptyStr) and (IsNumber(S)) then
  begin
    Line:= StrToInt(S);
  end else
  begin
    //%arquivo:%linha%:
    S:= getTextBtw(Output, ':', ':');
    if (S <> EmptyStr) and (IsNumber(S)) then
    begin
      Line:= StrToInt(S);
    end else
    begin
      //%arquivo%(%linha%,%coluna%)
      S:= getTextBtw(Output, '(', ',');
      if (S <> EmptyStr) and (IsNumber(S)) then
      begin
        Line:= StrToInt(S);
      end else
      begin
        // line %linha%
        I:= Pos('line ', Output);
        if I > 0 then
        begin
          // i indicara a posição posterior a "line "
          I := I + 5;
          if I < length(Output) then
          begin
            // o maior length de um integer é 10
            S:= Copy(output, I, 10);
            for I:= 1 to length(S) do
            begin
              if IsCharInStr('0123456789', S[I]) = false then
                break;
            end;
            S:= Copy(S, 0, I-1);
            If (S <> EmptyStr) and (IsNumber(S)) then
              line:= StrToInt(S);
          end;
        end;
      end;
    end;
  end;

end;

{ TNotesOutputParser }

constructor TNotesOutputParser.Create;
begin
  fValidFiles:= TStringList.Create;
end;

destructor TNotesOutputParser.Destroy;
begin
  FreeAndNil(fValidFiles);
  inherited;
end;

function TNotesOutputParser.getValidFiles: string;
begin
  Result:= fValidFiles.Text;
end;

procedure TNotesOutputParser.Parse;
var
  reg: TRegExpr;
  I: integer;
begin

  fFileName:= '';
  fLine:= -1;

  if fAuto then
  begin
    AutoParseOutput(fErrorStr, fLine, fFileName, fValidFiles);
  end else
  begin
    reg:= TRegExpr.Create;
    try
      reg.Expression:= fRE;

      if reg.Exec(fErrorStr) then
      begin
        for I:= 0 to reg.SubExprMatchCount do
        begin
          if I = StrToIntDef(fREFilePos, -1) then
            fFileName:= reg.Match[I];
          if I = StrToIntDef(fRELinePos, -1) then
            fLine:= StrToIntDef(reg.Match[I], 1);
        end;
      end;

      if not isFileValid(fFileName, fValidFiles) then
        fFileName:= '';

    finally
      reg.Free;
    end;
  end;
end;

procedure TNotesOutputParser.setValidFiles(const Value: string);
begin
  fValidFiles.Text:= Value;
end;

initialization
  outputParser:= TNotesOutputParser.Create;

finalization
  outputParser.Free;

end.
