unit NotesSmartIndent;

interface

uses SysUtils, Classes, SynEdit, Messages, Graphics, contnrs, RegExpr;

type

  { Diz que ação de identação deve ser tomada pelo Notes  }
  TNotesIndentAction = (iaNone, iaClear, iaKeep, iaInc, iaDec, iaHalfInc,
    iaHalfDec, iaDoubleInc, iaDoubleDec);

  TNotesSmartIndentRule = class(TObject)
  private
    fReg: TRegExpr;
    fCurAct: TNotesIndentAction;
    fNextAct: TNotesIndentAction;
    function getRegexp: string;
    procedure setRegexp(const Value: string);
  public
    constructor create(const Regexp: string; const curAct,
      nextAct: TNotesIndentAction);
    destructor destroy; override;
    function check(const Line: string): boolean;
    property regexp: string read getRegexp write setRegexp;
    property CurIndentAction: TNotesIndentAction read fCurAct write fCurAct;
    property NextIndentAction: TNotesIndentAction read fNextAct write fNextAct;
  end;

  TNotesSmartIndent = class(TObject)
  private
    fItems: TObjectList;
    fTabs: boolean;
    fIndentKeywords: TStringList;
    fIndentSize: integer;
    fEditor: TSynEdit;
    fSkipReg: TRegExpr;
    // get the first line to top of the test that have some code
    function getPreviusLineText(const curindex: integer): string;
    function getPreviusLineIndex(const curindex: integer): integer;
    function getLineIndentValue(const Line: string): integer;
    function getIndentStr(const Size: integer): string;
    function calcIndentForAction(const act: TNotesIndentAction; const indent: integer): integer;
    procedure DoIndentCurLine;
    function getSkip: string;
    procedure setSkip(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure IndentLine;
    procedure CheckTrigger(const c: char);
    class function StrToIndentAction(const S: string): TNotesIndentAction;
    property Editor: TSynEdit read fEditor write fEditor;
    property SkipExpr: string read getSkip write setSkip;
    property IndentSize: integer read fIndentSize write fIndentSize;
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
  nextAct: TNotesIndentAction);
begin
  fReg:= TRegExpr.Create;
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

procedure TNotesSmartIndent.DoIndentCurLine;
Var
  I, Indent: integer;
  S: string;
  bf: TBufferCoord;
  oldlen: integer;
begin
  if fEditor = nil then Exit;
  bf:= fEditor.CaretXY;
  S:= getPreviusLineText(bf.Line);
  Indent:= getLineIndentValue(S);

  if S <> '' then
  begin
    for I:= 0 to fITems.Count - 1 do
    begin
      if TNotesSmartIndentRule(fItems[I]).check(S) then
      begin
        indent:= calcIndentForAction(TNotesSmartIndentRule(fItems[I]).NextIndentAction, indent);
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
        indent:= calcIndentForAction(TNotesSmartIndentRule(fItems[I]).CurIndentAction, indent);
        Break;
      end;
    end;
  end;

  fEditor.BeginUpdate;
  fEditor.BeginUndoBlock;
  fEditor.CaretX:= 1;
  oldlen:= length(S);
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
              StrToIndentAction(ruleNextAct)) );
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
  if (Trim(S) = '') or (SameText(S, 'keep')) then
    Result:= iaKeep
  else if SameText(S, 'clear') then
    Result:= iaClear
  else if SameText(S, 'inc') then
    Result:= iaInc
  else if SameText(S, 'dec') then
    Result:= iaDec
  else if SameText(S, 'halfInc') then
    Result:= iaDec
  else if SameText(S, 'halfDec') then
    Result:= iaDec
  else if SameText(S, 'DoubleInc') then
    Result:= iaDec
  else if SameText(S, 'DoubleDec') then
    Result:= iaDec
  else
    Result:= iaNone;
end;

procedure TNotesSmartIndent.IndentLine;
begin
  if fEditor = nil then Exit;
  DoIndentCurLine;
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
  end;
  if Result < 0 then
    Result:= 0;
end;

procedure TNotesSmartIndent.CheckTrigger(const c: char);
Var
  I, tLen, lLen: integer;
  trigger, line: string;
begin
  line:= fEditor.LineText;
  lLen:= length(line);
  for I:= 0 to fIndentKeywords.Count -1 do
  begin
    trigger:= fIndentKeywords.Strings[I];
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