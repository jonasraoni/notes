//
//    NotesSmartIndent - classe para SmartIndent do Notes
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
  @abstract(NotesSmartIndent - classe para SmartIndent do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesSmartIndent;

interface

uses SysUtils, Classes, SynEdit, Messages, Graphics, contnrs, RegExpr;

type

  { Diz que ação de identação deve ser tomada pelo Notes  }
  TNotesIndentAction = (iaNone, iaClear, iaKeep, iaInc, iaDec, iaHalfInc,
    iaHalfDec, iaDoubleInc, iaDoubleDec, iaSpaceInc, iaSpaceDec,
    iaOne, iaTwo, iaThree, iaFour, iaFive, iaSix, iaSeven);

  // Guarda uma regra de smartindent
  TNotesSmartIndentRule = class(TObject)
  private
    fReg: TRegExpr;
    fCurAct: TNotesIndentAction;
    fNextAct: TNotesIndentAction;
    function getRegexp: string;
    procedure setRegexp(const Value: string);
  public
    constructor create(const Regexp: string; const curAct,
      nextAct: TNotesIndentAction; const CaseSensitive: boolean);
    destructor destroy; override;
    // Checa se a string é compatível com a regra representada pelo objeto
    function check(const Line: string): boolean;
    property regexp: string read getRegexp write setRegexp;
    // Ação de indentação para a linha atual
    property CurIndentAction: TNotesIndentAction read fCurAct write fCurAct;
    // Ação de indentação para a próxima linha
    property NextIndentAction: TNotesIndentAction read fNextAct write fNextAct;
  end;

  // Classe para SmartIndent do Notes
  TNotesSmartIndent = class(TObject)
  private
    fItems: TObjectList;
    fTabs: boolean;
    fIndentKeywords: TStringList;
    fIndentSize: integer;
    fEditor: TSynEdit;
    fSkipReg: TRegExpr;
    fCase: boolean;
    function getPreviusLineText(const curindex: integer): string;
    function getPreviusLineIndex(const curindex: integer): integer;
    function getLineIndentValue(const Line: string): integer;
    function getIndentStr(const Size: integer): string;
    function calcIndentForAction(const act: TNotesIndentAction; const indent: integer): integer;
    function getIndentByRules: integer;
    function getSkip: string;
    procedure setSkip(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    // Carrega o arquivo que tem as regras de indentação (NSI)
    procedure LoadFromFile(const FileName: string);
    // Identa linha
    procedure IndentLine;
    // Checa se o caracter/palavra atual deve chamar a identação.
    // Se for um "trigger", IndentLine será chamado
    procedure CheckTrigger(c: char);
    // Converte uma string para uma  TNotesIndentAction
    class function StrToIndentAction(const S: string): TNotesIndentAction;
    // Editor em que a classe deve agir
    property Editor: TSynEdit read fEditor write fEditor;
    // Expressão usada para pular linhas (comentários, string, etc.)
    // Carregada automaticamente do arquivo NSI
    property SkipExpr: string read getSkip write setSkip;
    // Tamanho da identação
    property IndentSize: integer read fIndentSize write fIndentSize;
    // Tamanho do caracter tab
    property UseTabs: boolean read fTabs write fTabs;
  end;

implementation

uses LibXmlParser, SynEditTypes;

const
  WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','.', ','];


function getAttrValue(Name: string; xml: TXMLParser): string;
Var
  I: integer;
begin
  Result:= '';
  if xml = nil then Exit;
  for I:= 0 to xml.CurAttr.Count -1 do
    if SameText(Name, xml.CurAttr.Name(I)) then
    begin
      Result:= xml.CurAttr.Value(I);
      Exit;
    end;
end;


{ TNotesSmartIndentRule }

function TNotesSmartIndentRule.check(const Line: string): boolean;
begin
  Result:= fReg.Exec(Line);
end;

constructor TNotesSmartIndentRule.create(const Regexp: string; const curAct,
  nextAct: TNotesIndentAction; const CaseSensitive: boolean);
begin
  fReg:= TRegExpr.Create;
  fReg.ModifierI:= not CaseSensitive;
  fReg.Expression:= Regexp;
  fCurAct:= curAct;
  fNextAct:= nextAct;
end;

destructor TNotesSmartIndentRule.destroy;
begin
  fReg.Free;
  inherited;
end;

function TNotesSmartIndentRule.getRegexp: string;
begin
  Result:= fReg.Expression;
end;

procedure TNotesSmartIndentRule.setRegexp(const Value: string);
begin
  fReg.Expression:= Value;
end;

{ TNotesSmartIndent }

constructor TNotesSmartIndent.Create;
begin
  fItems:= TObjectList.Create(true);
  fIndentKeywords:= TStringList.Create;
  fTabs:= false;
  fIndentSize:= 4;
  fEditor:= nil;
  fSkipReg:= nil;
end;

destructor TNotesSmartIndent.Destroy;
begin
  fItems.Free;
  fIndentKeywords.Free;
  if Assigned(fSkipReg) then
    fSkipReg.Free;
  inherited;
end;

function TNotesSmartIndent.getIndentStr(const Size: integer): string;
var
  stab: string;
begin
  Result:= StringOfChar(#32, Size);
  if (fEditor = nil) or (fTabs = false) then Exit;
  stab:= StringOfChar(#32, fEditor.TabWidth);
  Result:= StringReplace(Result, stab, #9, [rfReplaceAll]);
end;

function TNotesSmartIndent.getLineIndentValue(const Line: string): integer;
Var
  I, Len: integer;
begin
  Result:= 0;
  Len:= length(Line);
  if Len < 1 then Exit;
  for I:= 1 to Len do
    if Line[I] in [#32, #9] then
    begin
      if Line[I] = #32 then
        inc(Result)
      else
        inc(Result, fEditor.TabWidth);
    end else
      Exit;
end;

function TNotesSmartIndent.getPreviusLineText(const curindex: integer): string;
begin
  Result:='';
  if fEditor = nil then Exit;
  if fEditor.Lines.Count > 1 then
    Result:= fEditor.Lines.Strings[ getPreviusLineIndex(curindex)-1 ];
end;

function TNotesSmartIndent.getIndentByRules: integer;
Var
  I: integer;
  S: string;
begin
  Result:= 0;
  if fEditor = nil then Exit;
  S:= getPreviusLineText(fEditor.CaretY);
  Result:= getLineIndentValue(S);

  if S <> '' then
  begin
    for I:= 0 to fITems.Count - 1 do
    begin
      if TNotesSmartIndentRule(fItems[I]).check(S) then
      begin
        Result:= calcIndentForAction(TNotesSmartIndentRule(fItems[I]).NextIndentAction, Result);
        Break;
      end;
    end;
  end;

  S:= fEditor.LineText;
  if (S <> '') and ( (not assigned(fSkipReg)) or (not fSkipReg.Exec(S)) ) then
  begin
    for I:= 0 to fITems.Count - 1 do
    begin
      if TNotesSmartIndentRule(fItems[I]).check(S) then
      begin
        Result:= calcIndentForAction(TNotesSmartIndentRule(fItems[I]).CurIndentAction, Result);
        Break;
      end;
    end;
  end;

end;

procedure TNotesSmartIndent.IndentLine;
var
  bf: TBufferCoord;
  oldlen, indent: integer;
  S: string;
begin

  if fEditor = nil then Exit;
  fEditor.BeginUpdate;
  fEditor.BeginUndoBlock;
  bf:= fEditor.CaretXY;

  indent:= getIndentByRules;

  fEditor.CaretX:= 1;
  oldlen:= length(fEditor.LineText);
  fEditor.SelLength:= oldlen;
  S:= getIndentStr(Indent) + Trim(fEditor.SelText);
  fEditor.SelText:= S;
  bf.Char:=  bf.Char + (length(S) - oldlen);
  if bf.Char < 1 then
    bf.Char:= 1;
  fEditor.CaretXY:= bf;
  fEditor.EndUndoBlock;
  fEditor.EndUpdate;
end;

procedure TNotesSmartIndent.LoadFromFile(const FileName: string);
var
  xml: TXMLParser;
  ruleReg, ruleCurAct, ruleNextAct: string;
  s: string;
begin
  fItems.Clear;
  if not FileExists(FileName) then Exit;
  xml:= TXMLParser.Create;
  try
    xml.LoadFromFile(FileName);
    xml.StartScan;
    while xml.Scan do
    begin
      if (xml.CurPartType = ptContent) then
      begin
        if SameText(xml.CurName, 'trigger') then
          fIndentKeywords.Add(xml.CurContent);
      end else
      if (xml.CurPartType in [ptEmptyTag ,ptStartTag]) then
      begin
        if (xml.CurPartType = ptStartTag) and (SameText('NotesSmartIndent', xml.CurName)) then
        begin
          S:= getAttrValue('CaseSensitive', xml);
          fCase:= StrToBoolDef(S, true);
          S:= getAttrValue('skip', xml);
          if S <> '' then
            SkipExpr:= S;
        end;
        if SameText(xml.CurName, 'rule') then
        begin
          ruleReg:= getAttrValue('match', xml);
          ruleCurAct:= getAttrValue('curline', xml);
          ruleNextAct:= getAttrValue('nextline', xml);
          if Trim(ruleReg) <> '' then
            fItems.Add( TNotesSmartIndentRule.create(ruleReg, StrToIndentAction(ruleCurAct),
              StrToIndentAction(ruleNextAct), fCase) );
        end;
      end;
    end;
  finally
    xml.Free;
  end;
end;

class function TNotesSmartIndent.StrToIndentAction(
  const S: string): TNotesIndentAction;
begin
  if SameText(S, 'clear') then
    Result:= iaClear
  else if SameText(S, 'inc') then
    Result:= iaInc
  else if SameText(S, 'dec') then
    Result:= iaDec
  else if SameText(S, 'halfInc') then
    Result:= iaHalfInc
  else if SameText(S, 'halfDec') then
    Result:= iaHalfDec
  else if SameText(S, 'doubleInc') then
    Result:= iaDoubleInc
  else if SameText(S, 'doubleDec') then
    Result:= iaDoubleDec
  else if SameText(S, 'spaceInc') then
    Result:= iaSpaceInc
  else if SameText(S, 'spaceDec') then
    Result:= iaSpaceDec
  else if SameText(S, 'one') then
    Result:= iaOne
  else if SameText(S, 'two') then
    Result:= iaTwo
  else if SameText(S, 'three') then
    Result:= iaThree
  else if SameText(S, 'four') then
    Result:= iaFour
  else if SameText(S, 'five') then
    Result:= iaFive
  else if SameText(S, 'six') then
    Result:= iaSix
  else if SameText(S, 'seven') then
    Result:= iaSeven
  else
    Result:= iaKeep;
end;

function TNotesSmartIndent.getPreviusLineIndex(const curindex: integer): integer;
Var
  I: integer;
  S: string;
begin
  Result:= 1;
  if curindex < 3 then Exit;
  if fEditor = nil then Exit;
  if fEditor.Lines.Count < 2 then Exit;
  if assigned(fSkipReg) then
  begin
    for I:= curindex-2 downto 0 do
    begin
      S:= fEditor.Lines.Strings[I];
      if (Trim(S) <> '') and (fSkipReg.Exec(S) = false) then
      begin
        Result:= I+1;
        Exit;
      end;
    end;
  end else
  begin
    for I:= curindex-2 downto 0 do
      if Trim(fEditor.Lines.Strings[I]) <> '' then
      begin
        Result:= I+1;
        Exit;
      end;
  end;
end;

function TNotesSmartIndent.getSkip: string;
begin
  if Assigned(fSkipReg) then
    result:= fSkipReg.Expression
  else
    result:='';
end;

procedure TNotesSmartIndent.setSkip(const Value: string);
begin
  if Value = '' then
  begin
    if Assigned(fSkipReg) then
      FreeandNil(fSkipReg);
  end else
  begin
    if not Assigned(fSkipReg) then
      fSkipReg:= TRegExpr.Create;
    fSkipReg.ModifierI:= not fCase;
    fSkipReg.Expression:= Value;
  end;
end;

function TNotesSmartIndent.calcIndentForAction(const act: TNotesIndentAction;
  const indent: integer): integer;
begin
  Result:= indent;
  case act of
    iaClear:     Result:= 0;
    iaInc:       Result:= Indent + fIndentSize;
    iaDec:       Result:= Indent - fIndentSize;
    iaHalfInc:   Result:= Indent + (fIndentSize div 2);
    iaHalfDec:   Result:= Indent - (fIndentSize div 2);
    iaDoubleInc: Result:= Indent + (fIndentSize * 2);
    iaDoubleDec: Result:= Indent - (fIndentSize * 2);
    iaSpaceInc:  Result:= Indent + 1;
    iaSpaceDec:  Result:= Indent - 1;
    iaOne:       Result:= fIndentSize;
    iaTwo:       Result:= fIndentSize *2;
    iaThree:     Result:= fIndentSize *3;
    iaFour:      Result:= fIndentSize *4;
    iaFive:      Result:= fIndentSize *5;
    iaSix:       Result:= fIndentSize *6;
    iaSeven:     Result:= fIndentSize *7;
  end;
  if Result < 0 then
    Result:= 0;
end;

procedure TNotesSmartIndent.CheckTrigger(c: char);
Var
  I, tLen, lLen: integer;
  trigger, line: string;
begin
  line:= Copy(fEditor.LineText, 1, fEditor.CaretX);
  if not fCase then line:= AnsiLowerCase(line);
  if not fCase then c:= AnsiLowerCase(c)[1];
  lLen:= length(line);
  for I:= 0 to fIndentKeywords.Count -1 do
  begin
    trigger:= fIndentKeywords.Strings[I];
    if not fCase then trigger:= AnsiLowerCase(trigger);
    tLen:= length(trigger);
    if (tLen > 0) and (trigger[tLen] = c) then
    begin
      if tLen > 1 then
      begin
        if Copy(line, lLen-tLen+1, tLen) = trigger then
          if (lLen-tLen < 1 ) or (line[lLen-tLen] in WhiteSpaces) or (line[lLen-tLen+1] in WhiteSpaces) then
          begin
            self.IndentLine;
            Exit;
          end;
      end else
      begin
        self.IndentLine;
        Exit;
      end;
    end;
  end;
end;

end.
