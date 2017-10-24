//
//    TNotesHighlighter - Componente de coloração de sintaxe do Notes
//
//    Notes, https://github.com/jonasraoni/notes
//    Copyright (C) 2003-2004, Equipe do Notes.
//    Copyright (C) Vitalik <v-e-t-a-l@ukr.net>, Vit <nevzorov@yahoo.com> and
//      Fantasist <walking_in_the_sky@yahoo.com>
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

{
@abstract(NotesHighlighter - Syntax Highlighter for Notes)
@author(Anderson Barbieri <notesnr@ig.com.br>)
@author(Vitalik <v-e-t-a-l@ukr.net>)
@author(Vit <nevzorov@yahoo.com>)
@author(Fantasist <walking_in_the_sky@yahoo.com>) }
unit NotesHighlighter;

interface

uses
  SysUtils, Windows, Graphics, Registry, Classes, SynEditTypes,
  SynEditHighlighter, NotesHighlighterStyles, LibXmlParser;

type

  TSynRange = class;

  TSymbBrakeType = (btUnspecified, btAny, btTerm);

  TSynSymbol = class
  private
    Attr: TSynHighlighterAttributes;
    fOpenRule: TSynRange;
    FBrakeType: TSymbBrakeType;
  public
    Symbol: string;
    property BrakeType: TSymbBrakeType read FBrakeType write FBrakeType;
    property Attributes: TSynHighlighterAttributes read Attr write Attr;
    constructor Create(s: string; attribs: TSynHighlighterAttributes); virtual;
    destructor Destroy; override;
  end;

  TSynSymbolGroup = class
    Attribs: TSynHighlighterAttributes;
    KeywordsList: TStringList;
    GroupName: string;
    Name: string;
    constructor Create;
    destructor Destroy; override;
    procedure CloneTo(const Group: TSynSymbolGroup);
  end;

  TSymbRangeSet = record
    RangeValue: Integer;
    IncludeSymbols: boolean;
  end;

  PSymbRangeSet = ^TSymbRangeSet;

  SymbolsSet = set of char;

  TSymbolList = class;

  TSymbolNode = class
    c:char;
    BrakeType: TSymbBrakeType;
    NextSymbs: TSymbolList;
    tkSynSymbol: TSynSymbol;
    constructor Create(AC: char; SynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType); overload; virtual;
    constructor Create(AC: char); overload;
    destructor Destroy; override;
  end;

  TSymbolList = class
    SymbList: TList;
    function FindSymbol(c: char): TSymbolNode;
    procedure AddSymbol(symb: TSymbolNode);
    procedure SetSymbolNode(Index: Integer;Value: TSymbolNode);
    function GetSymbolNode(Index: integer): TSymbolNode;
    function GetCount: integer;
    property Nodes[index: integer]: TSymbolNode read GetSymbolNode write SetSymbolNode;
    property Count: Integer read GetCount;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TNotesHighlighter = class;

  TAbstractSymbol = class
    function GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean; virtual; abstract;
  end;

  TSymbols = class(TAbstractSymbol)
    HeadNode: TSymbolNode;
    function GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean; override;
    procedure AddSymbol(s: string; tkSynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType);
    function FindSymbol(s: string): TSymbolNode;
    constructor Create(c: char; tkSynSymbol: TSynSymbol;ABrakeType: TSymbBrakeType); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TDefaultSymbols = class(TAbstractSymbol)
     tkSynSymbol: TSynSymbol;
     constructor Create(SynSymb: TSynSymbol); reintroduce; virtual;
     destructor Destroy; override;
     function GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean; override;
  end;

  TDefaultTermSymbols = class(TAbstractSymbol)
     tkSynSymbol: TSynSymbol;
     constructor Create(SynSymb: TSynSymbol); virtual;
     function GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean; override;
     destructor Destroy; override;
  end;


  TNumberSymbols = class(TAbstractSymbol)
     tkSynSymbol: TSynSymbol;
     constructor Create(SynSymbol: TSynSymbol); virtual;
     function GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean; override;
     destructor Destroy; override;
  end;

  TClosingSymbolSet = record
    Symbol: TSynSymbol;
    AllowPredClose: boolean;
  end;

  PClosingSymbolSet = ^TClosingSymbolSet;

  TSynRange = class
  protected
    // Shared with alias ranges
    fSynSymbols: TList;
    fSynRanges: TList;
    fSymbolGroups: TList;
    fDefaultAttri: TSynHighlighterAttributes;
    fNumberAttri:  TSynHighlighterAttributes;
  private
    fCloseSymbol: TSynSymbol;
    fOpenSymbol: TSynSymbol;
    fCloseOnTerm: boolean;
    fCloseOnEol: boolean;
    FCaseSensitive: boolean;
    fOwner: TSynRange;
    fClosingSymbol: TClosingSymbolSet;

    fDefaultSynSymbol: TSynSymbol;
    fNumberSymbol: TNumberSymbols;
    fDefaultSymbols: TDefaultSymbols;
    fDefaultTermSymbol: TDefaultTermSymbols;

    fTermSymbols: SymbolsSet;
    SymbolList: array[char] of TAbstractSymbol;

    CaseFunct: function (c: char): char;
    StringCaseFunct: function (const s: string): string;
    fPrepared: boolean;
    FName: string;
  private
    function GetSynSymbol(Index: Integer): TSynSymbol;
    function GetSynRange(Index: Integer): TSynRange;
    function GetSynSymbolGroup(Index: Integer): TSynSymbolGroup;
    function GetRangeCount: Integer;
    function GetSymbolCount: Integer;
    function GetSymbolGroupCount: Integer;

    function GetCaseSensitive: boolean;
    procedure SetCaseSensitive(const Value: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddSymbolGroup(SymbolGroup: TSynSymbolGroup);
    procedure AddSymbol(NewSymb: TSynSymbol);
    procedure AddRange(NewSymb: TSynRange);
    function GetSymbol(s: string): TSynSymbol;
    function FindSymbol(s: string): TSynSymbol;
    function FindSymbolOwner(Symbol: TSynSymbol): TSynSymbolGroup;

    procedure DeleteRange(index: integer); overload;
    procedure DeleteRange(SynRange: TSynRange); overload;
    procedure DeleteSymbolGroup(index: integer); overload;
    procedure DeleteSymbolGroup(SymbolGroup: TSynSymbolGroup); overload;

    procedure Prepare(Owner: TSynRange); virtual;
    procedure Reset; virtual;
    procedure Clear; virtual;

    function  findRange(const Name: string): TSynRange;
    procedure CloneTo(const Range: TSynRange);
  public
    property TermSymbols: SymbolsSet read fTermSymbols write fTermSymbols;
    property OpenSymbol: TSynSymbol read fOpenSymbol;
    property CloseSymbol: TSynSymbol read fCloseSymbol;
    property CloseOnTerm: boolean read fCloseOnTerm write fCloseOnTerm;
    property CloseOnEol: boolean read fCloseOnEol write fCloseOnEol;

    property Ranges[Index: integer]: TSynRange read GetSynRange;
    property RangeCount: integer read GetRangeCount;
    property Symbols[Index: integer]: TSynSymbol read GetSynSymbol;
    property SymbolCount: integer read GetSymbolCount;
    property SymbolGroups[Index: integer]: TSynSymbolGroup read GetSynSymbolGroup;
    property SymbolGroupCount: Integer read GetSymbolGroupCount;

    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property DefaultAttri: TSynHighlighterAttributes read fDefaultAttri write fDefaultAttri;

    property CaseSensitive: boolean read GetCaseSensitive write SetCaseSensitive;
    property Prepared: boolean read fPrepared;
    property Parent: TSynRange read fOwner write fOwner;
    property Name: string read FName write FName;
  end;

  TSynExtendedRange = class(TSynRange)
  private
    fExtends: string;
    procedure setExtends(const Value: string);
  public
    procedure RemoveTag(const TagName: string);
    property  Extends: string read fExtends write setExtends;
  end;

  TNotesHighlighter = class(TSynCustomHighlighter)
  private
    procedure LoadStyles(const StyleFile: string);
  protected
    fMainRules: TSynRange;

    fEol: boolean;
    fPrEol: boolean;
    fTrueLine: PChar;
    fLine: PChar;
    fLineNumber: Integer;
    Run: LongInt;
    fStringLen: Integer;
    fTokenPos: Integer;

    fCurrToken: TSynSymbol;

    fCurrentRule: TSynRange;
    fSymbols: TSymbols;
    SymbolList: array[char] of TAbstractSymbol;

    fPrepared: boolean;

    fStyles: TNotesHighlighterStyles;

    procedure SpaceProc;
    procedure NullProc;
    function GetIdentChars: TSynIdentChars; override;
  private
    procedure LoadRange(curRange: TSynRange; xml: TXMLParser);
    procedure LoadKeywords(curKw: TSynSymbolGroup; xml: TXMLParser);
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: Integer;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;              // DJLP 2000-08-09
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
    procedure Reset;
    procedure Clear;
    procedure Prepare;
    procedure LoadFromFile(FileName: string);
    procedure ReloadStyles;
  public
    property MainRules: TSynRange read fMainRules;
  end;

  TNodeType=(ntRange, ntRootRange, ntKeyWords, ntNone);

function String2Set(s: string): SymbolsSet;
function Set2String(s: SymbolsSet): string;


const
  DefaultTermSymbols: SymbolsSet = ['*','/','+','-','=','\','|','&','(',')',
    '[',']','{','}','`','~','!','@',',','$','%','^','?',':',';','''','"','.',
    '>','<','#'];

var
  // Default attribute used in case o the highlighter file have some error
  defaultattr: TSynHighlighterAttributes;

implementation

uses
  SynEditStrConst;

const
  AbsoluteTermSymbols: SymbolsSet = [#9,#13,#0,#10,#32];
  NOTES_HSTYLES_EXT: string = '.nhs';

function String2Set(s: string): SymbolsSet;
var
  i: integer;
begin
  result:= [];
  for i := 1 to length(s) do Result:= Result+[s[i]];
end;

function Set2String(s: SymbolsSet): string;
var
  b: byte;
begin
  Result:='';
  for b:=1 to 255 do
    if (chr(b) in s) and (not (chr(b) in AbsoluteTermSymbols)) then
      Result:=Result+chr(b);
end;

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
      //UniqueString(Result);
      Exit;
    end;
end;

procedure FreeList(var List: TList);
var
 i: integer;
begin
  if List = nil then exit;

  for i := 0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Free;
  List:= nil;
end;

procedure ClearList(List: TList);
var
 i: integer;
begin
  if List = nil then exit;

  for i := 0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function CaseNone(c: char): char;
begin
  Result:= c;
end;

function StringCaseNone(const s: string): string;
begin
  Result:=s;
end;

// Used in prepare
(*  replaced by quicksort... arb2004
procedure SortSymbolList(List: TList);
var
  i: integer;
  fin: boolean;
begin
  fin := False;
  while not fin do
  begin
    fin := True;
    for i := 0 to List.Count-2 do
      if TSynSymbol(List[i]).Symbol>TSynSymbol(List[i+1]).Symbol then
      begin
        List.Exchange(i,i+1);
        fin := False;
      end;
  end;
end;
*)

procedure QuickSortSymbolList(const List: TList; const lowerPos, upperPos: integer);
var
  i, middlePos: integer;
  pivotValue: string;
Begin
  if lowerPos < upperPos then
  begin

    pivotValue := TSynSymbol(List[lowerPos]).Symbol;
    middlePos := lowerPos;

    for i := lowerPos + 1 to upperPos do
    begin
      if TSynSymbol(List[i]).Symbol < pivotValue then
      begin
        inc(middlePos);
        List.Exchange(i,middlePos);
      end;
    end;
    List.Exchange(lowerPos,middlePos);

    QuickSortSymbolList(List, lowerPos, middlePos-1);
    QuickSortSymbolList(List, middlePos+1, upperPos);
  end;
end;


// Used in prepare
function SafeInsertSymbol(const Symb: TSynSymbol; const Rules: TSynRange; const Attribs: TSynHighlighterAttributes): TSynSymbol;
begin
  Result:= Rules.FindSymbol(Symb.Symbol);
  if Result = nil then
  begin
    Result:= TSynSymbol.Create(Symb.Symbol,Symb.Attr);
    Result.BrakeType:= Symb.BrakeType;
    Rules.AddSymbol(Result);
  end;
  if Result.Attr = nil then
    Result.Attr:= Attribs;
end;


{ TSynSymbolGroup }

procedure TSynSymbolGroup.CloneTo(const Group: TSynSymbolGroup);
Var
  I: integer;
begin
  group.Attribs:= self.Attribs;
  group.GroupName:= self.GroupName;
  group.Name:= self.Name;
  // AddStrings usa addobject (mais lerdo) ao invés do add comum
  // então fazemos no manual para ganhar um pouco de performance :)
  group.KeywordsList.BeginUpdate;
  try
    for I:= 0 to self.KeywordsList.Count -1 do
      group.KeywordsList.Add(self.KeywordsList.Strings[I]);
  finally
    group.KeywordsList.EndUpdate;
  end;
end;

constructor TSynSymbolGroup.Create;
begin
  Attribs:= defaultattr;
  KeywordsList:=TStringList.Create;
end;

destructor TSynSymbolGroup.Destroy;
begin
  KeywordsList.Free;
  inherited;
end;


{ TSynSymbol }

constructor TSynSymbol.Create(s: string;
  attribs: TSynHighlighterAttributes);
begin
 attr:= attribs;
 Symbol:= s;
 fOpenRule:= nil;
 BrakeType:= btUnspecified;
end;

destructor TSynSymbol.Destroy;
begin
  inherited;
end;

{ TSynRange }

procedure TSynRange.AddRange(NewSymb: TSynRange);
begin
  NewSymb.Parent:= self;
  fSynRanges.Add(NewSymb);
end;

procedure TSynRange.AddSymbol(NewSymb: TSynSymbol);
var
 SynSym: TSynSymbol;
begin
  SynSym:= FindSymbol(NewSymb.Symbol);
  if SynSym <> nil then
  begin
    fSynSymbols.Remove(SynSym);
    SynSym.Free;
  end;
  fSynSymbols.Add( NewSymb );
end;

procedure TSynRange.AddSymbolGroup(SymbolGroup: TSynSymbolGroup);
begin
  fSymbolGroups.Add(SymbolGroup);
end;

constructor TSynRange.Create;
begin
  fOpenSymbol := TSynSymbol.Create('',nil);
  fCloseSymbol := TSynSymbol.Create('',nil);

  fDefaultAttri := defaultattr;
  fNumberAttri := defaultattr;

  FillChar(SymbolList,sizeof(SymbolList),0);
  SetCaseSensitive(True);

  fPrepared := false;
  fCloseOnTerm := false;
  fCloseOnEol := false;

  fSymbolGroups := TList.Create;
  fSynSymbols := TList.Create;
  fSynRanges := TList.Create;
  fTermSymbols := DefaultTermSymbols;
end;

destructor TSynRange.Destroy;
begin
  if assigned(fOpenSymbol) then
    fOpenSymbol.Free;
  if assigned(fCloseSymbol) then
    fCloseSymbol.Free;

  FreeList(fSynSymbols);
  FreeList(fSymbolGroups);
  FreeList(fSynRanges);
  inherited;
end;

function TSynRange.FindSymbol(s: string): TSynSymbol;
var
 i: integer;
begin
  Result := nil;
  for i := 0 to fSynSymbols.Count-1 do
   if TSynSymbol(fSynSymbols.Items[i]).Symbol=s then
   begin
     Result := TSynSymbol(fSynSymbols.Items[i]);
     exit;
   end;
end;

function TSynRange.FindSymbolOwner(Symbol: TSynSymbol): TSynSymbolGroup;
var
 i,j: integer;
begin
  Result := nil;
  for i := 0 to fSymbolGroups.Count-1 do
    if TSynSymbolGroup(fSymbolGroups[i]).KeywordsList.Find(Symbol.Symbol,j) then
    begin
      Result := TSynSymbolGroup(fSymbolGroups[i]);
      exit;
    end;
end;

function TSynRange.GetRangeCount: Integer;
begin
  Result := fSynRanges.Count;
end;

function TSynRange.GetSymbol(s: string): TSynSymbol;
begin
  Result := FindSymbol(s);
end;

function TSynRange.GetSymbolCount: Integer;
begin
 Result := fSynSymbols.Count;
end;

function TSynRange.GetSymbolGroupCount: Integer;
begin
 Result := fSymbolGroups.Count;
end;

function TSynRange.GetSynRange(Index: Integer): TSynRange;
begin
 Result := TSynRange(fSynRanges[Index]);
end;

function TSynRange.GetSynSymbol(Index: Integer): TSynSymbol;
begin
 Result := TSynSymbol(fSynSymbols[Index]);
end;

function TSynRange.GetSynSymbolGroup(Index: Integer): TSynSymbolGroup;
begin
 Result := TSynSymbolGroup(fSymbolGroups[Index]);
end;

procedure TSynRange.Prepare(Owner: TSynRange);
var
 i,j, Len: integer;
 SynSymbol: TSynSymbol;
 s: string;
 FirstChar: char;
 BrakeType: TSymbBrakeType;
begin
  Reset;
  fOwner := Owner;

  fDefaultSynSymbol := TSynSymbol.Create('',fDefaultAttri);
  fDefaultTermSymbol := TDefaultTermSymbols.Create(TSynSymbol.Create('',fDefaultAttri));
  fDefaultSymbols := TDefaultSymbols.Create(TSynSymbol.Create('',fDefaultAttri));
  fNumberSymbol := TNumberSymbols.Create(TSynSymbol.Create('',fNumberAttri));
  fTermSymbols := fTermSymbols; //+AbsoluteTermSymbols;

  //Add all keywords in Symbol list.
  for i := 0 to fSymbolGroups.Count-1 do
    for j := 0 to TSynSymbolGroup(fSymbolGroups[i]).KeywordsList.Count-1 do
      AddSymbol(TSynSymbol.Create(TSynSymbolGroup(fSymbolGroups[i]).KeywordsList[j],TSynSymbolGroup(fSymbolGroups[i]).Attribs));

  //Assign range opening and closing symbols and Prepare range rules.
  for i := 0 to fSynRanges.Count-1 do
   begin
     //Assign range opening symbol
     SynSymbol := SafeInsertSymbol(TSynRange(fSynRanges[i]).fOpenSymbol,self,TSynRange(fSynRanges[i]).fDefaultAttri);
     SynSymbol.fOpenRule := TSynRange(fSynRanges[i]);

    //Assing range closing symbols
    SynSymbol := SafeInsertSymbol(TSynRange(fSynRanges[i]).fCloseSymbol,TSynRange(fSynRanges[i]),TSynRange(fSynRanges[i]).fDefaultAttri);
    TSynRange(fSynRanges[i]).fClosingSymbol.Symbol := SynSymbol;

    TSynRange(fSynRanges[i]).Prepare(Self);
  end;

  //Build tokens table

  QuickSortSymbolList(fSynSymbols, 0, fSynSymbols.Count-1);

  for i := 0 to fSynSymbols.Count-1 do
  begin
    SynSymbol := TSynSymbol(fSynSymbols[i]);
    Len:= Length(SynSymbol.Symbol);
    if Len < 1 then
      continue;
    s := SynSymbol.Symbol;
    FirstChar := s[1];

   if SynSymbol.BrakeType <> btUnspecified then
     BrakeType:= SynSymbol.BrakeType
   else
     if (s[Len] in fTermSymbols) or (s[Len] in AbsoluteTermSymbols) then
       BrakeType:= btAny
     else
       BrakeType:= btTerm;

    if SymbolList[CaseFunct(FirstChar)] = nil then
    begin
      if Len = 1 then
        SymbolList[CaseFunct(FirstChar)] := TSymbols.Create(FirstChar,SynSymbol,BrakeType)
      else
      begin
        SymbolList[CaseFunct(FirstChar)] := TSymbols.Create(FirstChar,fDefaultSynSymbol,BrakeType);
        TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy( s, 2, Len-1 ) ),SynSymbol,BrakeType );
      end;
    end
    else
    begin
      if Len = 1 then
      else
        TSymbols(SymbolList[CaseFunct(FirstChar)]).AddSymbol(StringCaseFunct(copy(s, 2, Len-1) ),SynSymbol,BrakeType);
    end;
  end;

  //Fill remaining table
  for i := 0 to 255 do
  if SymbolList[char(i)] = nil then
  begin
   if  (char(i) in fTermSymbols) or (char(i) in AbsoluteTermSymbols) then
     SymbolList[char(i)]:= fDefaultTermSymbol
   else
   if char(i) in ['0'..'9'] then
     SymbolList[char(i)] := fNumberSymbol
   else
     SymbolList[char(i)] := fDefaultSymbols;
  end;

  // TODO - PESQUISAR SE TODAS AS LINGUAGENS USAM "." PARA FLOATING POINT
  // SE SIM, ADICIONAR ELE AOS NÚMEROS

  fPrepared := true;
end;

function TSynRange.GetCaseSensitive: boolean;
begin
  Result := FCaseSensitive;
end;

procedure TSynRange.SetCaseSensitive(const Value: boolean);
begin
 FCaseSensitive := Value;
 if not Value then
 begin
   CaseFunct := UpCase;
   StringCaseFunct := UpperCase;
 end
 else
 begin
   CaseFunct := CaseNone;
   StringCaseFunct := StringCaseNone;
 end;
end;

procedure TSynRange.Reset;
var
 i: integer;
begin
 if not fPrepared then
   exit;
 fDefaultSynSymbol.Free;
 fDefaultTermSymbol.Free;
 fDefaultSymbols.Free;
 fNumberSymbol.Free;

 for i := 0 to 255 do
   SymbolList[char(i)] := nil;

 for i := 0 to fSynRanges.Count-1 do
   TSynRange( fSynRanges[i] ).Reset;


 ClearList(fSynSymbols);

 fPrepared := False;
end;

procedure TSynRange.Clear;
var
 i: integer;
begin
 Reset;
 for i := 0 to fSynRanges.Count-1 do
   TSynRange(fSynRanges[i]).Clear;

 ClearList(fSynRanges);
 ClearList(fSynSymbols);
 ClearList(fSymbolGroups);
end;


procedure TSynRange.CloneTo(const Range: TSynRange);
Var
  I: integer;
  SymbGr: TSynSymbolGroup;
  NewRange: TSynRange;
begin
  if fSynSymbols.Count > 0 then
    raise Exception.Create('fSynSymbols initialized!');

  Range.Name:=                   self.Name;
  Range.TermSymbols:=            self.TermSymbols;
  Range.OpenSymbol.BrakeType:=   self.OpenSymbol.BrakeType;
  Range.CloseOnTerm:=            self.CloseOnTerm;
  Range.CloseOnEol:=             self.CloseOnEol;
  Range.NumberAttri:=            self.NumberAttri;
  Range.DefaultAttri:=           self.DefaultAttri;
  Range.CaseSensitive:=          self.CaseSensitive;
  Range.OpenSymbol.Symbol:=      self.OpenSymbol.Symbol;
  Range.CloseSymbol.Symbol:=     self.CloseSymbol.Symbol;

  for I:= 0 to self.SymbolGroupCount -1 do
  begin
    SymbGr:= TSynSymbolGroup.Create;
    self.SymbolGroups[I].CloneTo(SymbGr);
    Range.AddSymbolGroup(SymbGr);
  end;

  for I:= 0 to self.RangeCount - 1 do
  begin
    NewRange:= TSynRange.Create;
    self.Ranges[I].CloneTo(NewRange);
    Range.AddRange(NewRange);
  end;
end;

function TSynRange.findRange(const Name: string): TSynRange;
Var
  I: integer;
begin
  Result:= nil;
  if fOwner = nil then Exit;

  for I:= 0 to fOwner.RangeCount - 1 do
    if SameText(Name, fOwner.Ranges[I].Name) then
    begin
      Result:= fOwner.Ranges[I];
      Exit;
    end;
end;

{ TSymbolList }

procedure TSymbolList.AddSymbol(symb: TSymbolNode);
begin
 SymbList.Add(symb);
end;

constructor TSymbolList.Create;
begin
 SymbList := TList.Create;
end;

destructor TSymbolList.Destroy;
begin
 FreeList(SymbList);
 inherited;
end;

function TSymbolList.FindSymbol(c: char): TSymbolNode;
var
 i: integer;
begin
 Result := nil;
 for i := 0 to SymbList.Count-1 do
   if TSymbolNode(SymbList[i]).c=c then
   begin
     Result := TSymbolNode(SymbList[i]);
     break;
   end;
end;

function TSymbolList.GetCount: integer;
begin
 Result := SymbList.Count
end;

function TSymbolList.GetSymbolNode(Index: integer): TSymbolNode;
begin
  Result := TSymbolNode(SymbList[index]);
end;

procedure TSymbolList.SetSymbolNode(Index: Integer; Value: TSymbolNode);
begin
 if Index<SymbList.Count then
   TSymbolNode(SymbList[index]).Free;
 SymbList[index] := Value;
end;

{ TSymbols }

procedure TSymbols.AddSymbol(s: string; tkSynSymbol: TSynSymbol; ABrakeType: TSymbBrakeType);
var
  i: integer;
  l: integer;
  Node: TSymbolNode;
  SList: TSymbolList;
begin
  SList := HeadNode.NextSymbs;
  Node := nil;
  l := Length(s);
  for i := 1 to l do
  begin
    Node := SList.FindSymbol(s[i]);
    if Node=nil then
    begin
      Node := TSymbolNode.Create(s[i]);
      SList.AddSymbol(Node);
    end;
    SList := Node.NextSymbs;
  end;
  Node.BrakeType := ABrakeType;
  Node.tkSynSymbol := tkSynSymbol;
end;

constructor TSymbols.Create(c: char; tkSynSymbol: TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
 HeadNode := TSymbolNode.Create(c,tkSynSymbol,ABrakeType);
end;

destructor TSymbols.Destroy;
begin
  HeadNode.Free;
  inherited;
end;

function TSymbols.FindSymbol(s: string): TSymbolNode;
var
  i: integer;
  l: integer;
  Node,prvNode: TSymbolNode;
begin
  Node := HeadNode;
  l := Length(s);
  for i := 1 to l do
  begin
    prvNode := Node.NextSymbs.FindSymbol(s[i]);
    if prvNode=nil then
      break;
    Node := prvNode;
  end;
  Result := Node;
end;

function TSymbols.GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean;
var
 Node,nxtNode: TSymbolNode;
begin
  Result := false;
  Node := HeadNode;
  nxtNode := nil;

  while (Node.NextSymbs.Count>0) and (parser.fLine[parser.Run]<>#0) do
  begin
    inc(parser.Run);
    nxtNode := Node.NextSymbs.FindSymbol(parser.fLine[parser.Run]);
    if nxtNode = nil then
      break;
    Node := nxtNode;
  end;

  if Node.tkSynSymbol = nil then
    exit;

  if (nxtNode = nil) and (Node.NextSymbs.Count > 0) then
    dec(parser.Run);

  if parser.fLine[parser.Run]<>#0 then
    inc(parser.Run);

  if Node.BrakeType=btAny then
  begin
    Result := True;
    tkSynSymbol := Node.tkSynSymbol;
    exit;
  end;

  if (parser.fLine[parser.Run] in parser.fCurrentRule.fTermSymbols) or
    (parser.fLine[parser.Run] in AbsoluteTermSymbols) then
  begin
    Result := True;
    tkSynSymbol := Node.tkSynSymbol;
  end;

end;

{ TSymbolNode }

constructor TSymbolNode.Create(AC: char; SynSymbol: TSynSymbol;
  ABrakeType: TSymbBrakeType);
begin
  c := AC;
  NextSymbs := TSymbolList.Create;
  BrakeType := ABrakeType;
  tkSynSymbol := SynSymbol;
end;

constructor TSymbolNode.Create(AC: char);
begin
  c := AC;
  NextSymbs := TSymbolList.Create;
  tkSynSymbol := nil;
end;

destructor TSymbolNode.Destroy;
begin
  NextSymbs.Free;
  inherited;
end;

{ TDefaultSymbols }

constructor TDefaultSymbols.Create(SynSymb: TSynSymbol);
begin
  tkSynSymbol := SynSymb;
end;

destructor TDefaultSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;


function TDefaultSymbols.GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean;
begin
  inc(parser.Run);
  Result := False;
end;

{ TNumberSymbols }

constructor TNumberSymbols.Create(SynSymbol: TSynSymbol);
begin
  tkSynSymbol := SynSymbol;
end;

destructor TNumberSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;

function TNumberSymbols.GetToken(parser: TNotesHighlighter; var tkSynSymbol: TSynSymbol): boolean;
begin
  repeat
    Inc(Parser.Run);
  until not (parser.fLine[parser.Run] in ['0'..'9']);
  if (parser.fLine[parser.Run] in parser.fCurrentRule.fTermSymbols) or (parser.fLine[parser.Run] in AbsoluteTermSymbols) then
  begin
    Result := True;
    tkSynSymbol := self.tkSynSymbol;
  end
  else
    Result := false;
end;

{ TDefaultTermSymbols }

constructor TDefaultTermSymbols.Create(SynSymb: TSynSymbol);
begin
  tkSynSymbol := SynSymb;
end;

destructor TDefaultTermSymbols.Destroy;
begin
  tkSynSymbol.Free;
  inherited;
end;

function TDefaultTermSymbols.GetToken(parser: TNotesHighlighter;
  var tkSynSymbol: TSynSymbol): boolean;
begin
  if parser.fLine[parser.Run]<>#0 then
     Inc(parser.Run);
  tkSynSymbol := self.tkSynSymbol;
  Result := True;
end;

{ TNotesHighlighter }

constructor TNotesHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPrepared := false;

  fMainRules := TSynRange.Create;
  fMainRules.Name :=  'Root';
  fEol := false;
  fPrEol := false;
  fCurrentRule := fMainRules;
  fStyles:= nil;

  // Necessário para que a função WordAtCursor do SynEdit funcione
  // direito. A definição padrão do SynEdit está errada!
  WordBreakChars:= [' ',#9,#13,#10,'!','"','#','$','%','&',
                     '''','(',')','*','+','-','/',':',';','<',
                     '=','>','?','@','[','\',']','^','`','{',
                     '|','}','~', '.', ','];
end;

destructor TNotesHighlighter.Destroy;
begin
  fMainRules.Free;
  if assigned(fStyles) then
    fStyles.Free;
  inherited;
end;

procedure TNotesHighlighter.SetLine(NewValue: string; LineNumber: Integer);
var
  l,i: integer;
begin
  if not fCurrentRule.Prepared then
    Prepare;
  fTrueLine  :=  PChar(NewValue);
  l := Length(NewValue);
  GetMem(fLine,l+1);
  for i := 0 to l do
    fLine[i] := fCurrentRule.CaseFunct(fTrueLine[i]);
  Run  :=  0;
  fTokenPos := 0;
  fLineNumber  :=  LineNumber;
  fEol := false;
  fPrEol := false;
  Next;
end;

procedure TNotesHighlighter.SpaceProc;
begin
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#32, #9, #0, #10, #13]);
end;

function TNotesHighlighter.IsKeyword(const AKeyword: string): boolean;
begin
  Result:=  fSymbols.FindSymbol(AKeyword) <> nil;
end;

//NEXT
procedure TNotesHighlighter.Next;
Var
  WordSeparators: SymbolsSet;
begin
  WordSeparators:= fCurrentRule.fTermSymbols + AbsoluteTermSymbols;
  if fPrEol then
  begin
    if (fCurrentRule.fCloseOnEol)  then
      fCurrentRule := fCurrentRule.fOwner;
    fEol := True;
    exit;
  end;

  fTokenPos:= Run;
  if (fCurrentRule.fCloseOnTerm) and (fLine[Run] in fCurrentRule.fTermSymbols) then
    fCurrentRule := fCurrentRule.fOwner;

  if not fCurrentRule.SymbolList[fLine[Run]].GetToken(self, fCurrToken) then
  begin
    fCurrToken := fCurrentRule.fDefaultSynSymbol;
    while not (fLine[Run] in WordSeparators ) do
      inc(Run);
  end
  else
  if fCurrentRule.fClosingSymbol.Symbol = fCurrToken then
    fCurrentRule := fCurrentRule.fOwner
  else
  if fCurrToken.fOpenRule <> nil then
    fCurrentRule := fCurrToken.fOpenRule;

  if fLine[Run]=#0 then
    fPrEol := True;
end;

function TNotesHighlighter.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  Result:= DefaultAttr;
end;

function TNotesHighlighter.GetEOL: Boolean;
begin
  Result  :=  fEol;
end;

function TNotesHighlighter.GetRange: Pointer;
begin
  Result  :=  fCurrentRule;
end;

function TNotesHighlighter.GetToken: string;
var
  Len: LongInt;
begin
  Len  :=  Run - fTokenPos;
  Setstring(Result, (FTrueLine + fTokenPos), Len);
end;

function TNotesHighlighter.GetTokenID: Integer;
begin
  Result  := 1;// CODE_REVIEW fCurrToken.ID;
end;

function TNotesHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
 Result := fCurrToken.Attr;
end;

function TNotesHighlighter.GetTokenKind: integer;
begin
  Result  := 1;// CODE_REVIEW   fCurrToken.ID;
end;

function TNotesHighlighter.GetTokenPos: Integer;
begin
  Result  :=  fTokenPos;
end;

procedure TNotesHighlighter.ResetRange;
begin
  fCurrentRule  :=  fMainRules;
end;

procedure TNotesHighlighter.SetRange(Value: Pointer);
begin
  fCurrentRule := TSynRange(Value);
end;

class function TNotesHighlighter.GetLanguageName: string;
begin
  Result:= '';
end;

procedure TNotesHighlighter.Clear;
begin
  MainRules.Clear;
end;

procedure TNotesHighlighter.Prepare;
begin
  fMainRules.Prepare(fMainRules);
end;


procedure TNotesHighlighter.NullProc;
begin
// fEol := True;
end;

procedure TNotesHighlighter.Reset;
begin
  fMainRules.Reset;
end;

procedure TSynRange.DeleteRange(SynRange: TSynRange);
begin
  fSynRanges.Remove(SynRange);
  SynRange.Free;
end;

procedure TSynRange.DeleteRange(index: integer);
begin
  TSynRange(fSynRanges[index]).Free;
  fSynRanges.Delete(index);
end;

procedure TSynRange.DeleteSymbolGroup(SymbolGroup: TSynSymbolGroup);
begin
  fSymbolGroups.Remove(SymbolGroup);
  SymbolGroup.Free;
end;

procedure TSynRange.DeleteSymbolGroup(index: integer);
begin
  TSynSymbolGroup(fSymbolGroups[index]).Free;
  fSymbolGroups.Delete(index);
end;

procedure TNotesHighlighter.LoadFromFile(FileName: string);
var
  xml: TXMLParser;
begin
  if not fileExists(FileName) then
    raise Exception.Create(className+'.LoadFromFile - "'+FileName+'" does not exists.');

  LoadStyles( ChangeFileExt(FileName, NOTES_HSTYLES_EXT) );

  if fStyles = nil then
    raise Exception.Create(className+'.LoadFromFile - could not load the Styles!!!');

  Clear;  

  xml:= TXMLParser.Create;
  try
    xml.LoadFromFile(FileName);
    xml.StartScan;
    // Lê até o primeiro range
    while xml.Scan do
      if (xml.CurPartType = ptStartTag) and (SameText(xml.CurName, 'Range')) then
        Break;
    loadRange(fMainRules, xml);
  finally
    xml.Free;
  end;
end;


function TNotesHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := [#32..#255] - fCurrentRule.TermSymbols;
end;

procedure TNotesHighlighter.LoadStyles(const StyleFile: string);
begin
  if fStyles <> nil then
    fStyles.Free;

  fStyles:= TNotesHighlighterStyles.Create;
  fStyles.StyleFile:= StyleFile;
  fStyles.Load;
end;

procedure TNotesHighlighter.ReloadStyles;
begin
  if fStyles <> nil then
    fStyles.Reload;
end;

procedure TNotesHighlighter.LoadRange(curRange: TSynRange; xml: TXMLParser);
var
  NewRange: TSynRange;
  NewSymbolGroup: TSynSymbolGroup;
  S: string;
begin

  curRange.OpenSymbol.BrakeType:= btAny;

  if (xml.CurPartType = ptStartTag) and ( (SameText(xml.CurName, 'Range')) or (SameText(xml.CurName, 'ExtendedRange')) ) then
  begin

    S:= getAttrValue('name', xml);
    if S <> '' then
      curRange.Name:= S;

    if curRange is TSynExtendedRange then
    begin
      S:= getAttrValue('extends', xml);

      if S <> '' then
        (curRange As TSynExtendedRange).Extends:= S
      else
        raise Exception.Create('The ExtendedRange "'+curRange.Name+'" needs a valid "Extends" attribute.');

    end;


  end else
    raise Exception.Create(ClassName + '.LoadRange - no range to load!!!');

  while xml.Scan do
  begin
    if xml.CurPartType = ptEndTag then
    begin
      if SameText(xml.CurName, 'Range') then Exit;
      if curRange is TSynExtendedRange then
        if SameText(xml.CurName, 'ExtendedRange') then Exit;
    end;

    if (xml.CurPartType = ptContent) then
    begin
      if SameText(xml.CurName, 'CaseSensitive') then
        curRange.CaseSensitive:= StrToBoolDef(xml.CurContent, true)
      else if SameText(xml.CurName, 'CloseOnEol') then
        curRange.CloseOnEol:= StrToBoolDef(xml.CurContent, true)
      else if SameText(xml.CurName, 'AnyTerm') then
      begin
        if StrToBoolDef(xml.CurContent, false) then
          curRange.OpenSymbol.BrakeType:= btTerm;
      end
      else if SameText(xml.CurName, 'Delimiters') then
      begin

        if SameText(getAttrValue('spaces', xml), 'true') then
          curRange.TermSymbols:= String2Set(xml.CurContent) + [#32, #9]
        else
          curRange.TermSymbols:= String2Set(xml.CurContent);

        curRange.CloseOnTerm:= true;

      end else if SameText(xml.CurName, 'CloseSymbol') then
        curRange.CloseSymbol.Symbol:= Copy(xml.CurContent, 1, length(xml.CurContent))
      else if SameText(xml.CurName, 'OpenSymbol') then
        curRange.OpenSymbol.Symbol:= Copy(xml.CurContent, 1, length(xml.CurContent))
      else if SameText(xml.CurName, 'TextStyle') then
      begin
        curRange.DefaultAttri:= fStyles.getStyleDef(xml.CurContent, defaultAttr);
        if (curRange.NumberAttri = DefaultAttr) or (curRange.NumberAttri = nil) then
          curRange.NumberAttri:= curRange.DefaultAttri;
      end
      else if SameText(xml.CurName, 'NumberStyle') then
        curRange.NumberAttri:= fStyles.getStyleDef(xml.CurContent, defaultAttr);

      if curRange is TSynExtendedRange then
        if SameText('Remove', xml.CurName) then
          (curRange As TSynExtendedRange).RemoveTag(xml.CurContent);

    end else
    if (xml.CurPartType = ptStartTag) then
    begin
      if SameText(xml.CurName, 'Range') then
      begin
        NewRange := curRange.findRange(getAttrValue('Name', xml));
        if NewRange = nil then
        begin
          NewRange := TSynRange.Create;
          CurRange.AddRange(NewRange);
        end;
        LoadRange(NewRange, xml);
      end else
      if SameText(xml.CurName, 'ExtendedRange') then
      begin
        NewRange:= TSynExtendedRange.Create;
        CurRange.AddRange(NewRange);
        LoadRange(NewRange, xml);
      end else
      if SameText(xml.CurName, 'Keywords') then
      begin
        NewSymbolGroup:= TSynSymbolGroup.Create;
        CurRange.AddSymbolGroup(NewSymbolGroup);
        LoadKeywords(NewSymbolGroup, xml);
      end;
    end;
  end;
end;

procedure TNotesHighlighter.LoadKeywords(curKw: TSynSymbolGroup;
  xml: TXMLParser);
Var
  S: string;
begin
  if curKw = nil then Exit;
  if xml = nil then Exit;

  if (xml.CurPartType = ptStartTag) and ( SameText('Keywords', xml.CurName) ) then
  begin
    curKw.Name:= getAttrValue('name', xml);
    curKw.Attribs:= fStyles.getStyleDef(getAttrValue('style', xml), defaultattr);
  end else
    raise Exception.Create(ClassName + '.LoadKeywords - no keywords to load!!!');

  // movemos em frente
  xml.Scan;
  curKw.KeywordsList.BeginUpdate;
  curKw.KeywordsList.Clear;
  try
    while xml.Scan do
    begin
      if ((xml.CurPartType = ptEndTag) and ( SameText(xml.CurName, 'Keywords') ) ) = false then
      begin
        if (xml.CurPartType = ptContent) and  (SameText(xml.CurName, 'keyword') ) then
        begin
          S:= xml.CurContent;
          UniqueString(S);
          //if S <> '' then
          curKw.KeywordsList.Add(S);
        end;
      end else
        Break;
    end;
  finally
    curKw.KeywordsList.EndUpdate;
  end;
end;



{ TSynExtendedRange }

procedure TSynExtendedRange.RemoveTag(const TagName: string);
Var
  I: integer;
begin

  for I:= 0 to self.SymbolGroupCount -1 do
    if SameText(tagName, self.SymbolGroups[I].Name) then
    begin
      self.DeleteSymbolGroup(I);
      Exit;
    end;

  for I:= 0 to self.RangeCount -1 do
    if SameText(tagName, self.Ranges[I].Name) then
    begin
      self.DeleteRange(I);
      Exit;
    end;
end;

procedure TSynExtendedRange.setExtends(const Value: string);
Var
  SaveName: string;
  I: integer;
begin
  if fOwner = nil then
    raise Exception.Create(ClassName+'.SetExtends - set the parent before!!!');
  fExtends:= '';
  SaveName:= self.Name;

  for I:= 0 to fOwner.RangeCount - 1 do
    if SameText(Value, fOwner.Ranges[I].Name) then
    begin
      fExtends := Value;
      fOwner.Ranges[I].CloneTo(self);
      self.Name:= SaveName;
      Exit;
    end;

  if fExtends = '' then
    raise Exception.Create('Could not extends "'+Value+'" - it does not exists!!!');
end;

initialization
  defaultattr:= TSynHighlighterAttributes.Create('Unknown');

finalization
  FreeAndNil(defaultattr);

end.
