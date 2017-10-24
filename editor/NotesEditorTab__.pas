//
//    TNotesEditorTab - Componente que modifica os comportamentos
//    do SynEdit para que fique de acordo com as idéias da equipe
//    do Notes :)
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
//
//    The Original Code is: SynEditPythonBehaviour.pas, released 2000-06-23.
//    The Original Code is based on odPythonBehaviour.pas by Olivier Deckmyn, part
//    of the mwEdit component suite.
//
//    Contributors to the SynEdit and mwEdit projects are listed in the
//    Contributors.txt file. See http://synedit.sf.net for info.
//

(*
  @abstract(NotesEditorTab - componente que agrupa tab, synedit e unighilighter além de adicionar novos comportamentos ao editor.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  @author(Denner Bizarria Branco <dennerbb@yahoo.com.br>)
  @author(Baseado no componente de Olivier Deckmyn e David Muir <dhm@dmsoftware.co.uk>)
*)
unit NotesEditorTab;

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics,
  Controls, Forms, Dialogs, SynEdit, SynEditKeyCmds,
  NotesHighlighter, ComCtrls, StdCtrls, NotesUtils,
  NotesEditorTabPosList, SynEditTypes;

type
  { Guarda os últimos símbolos coloridos no editor.}
  TLastColoredSymbols = record
    P1: TPoint;
    P2: Tpoint;
    Cleared: boolean;
  end;

type
  TNotesLoadFromFileResult = (lfCancel, lfError, lfOk);

type

  TNotesMarksList= class(TObject)
  private
    fItems: array of Integer;
    fCount: integer;
    fSorted: boolean;
    function  get(index: integer): integer;
    procedure put(index: integer; value: integer);
    procedure QuickSort(lowerPos, upperPos: integer);
  public
    function  Add(atLine: integer): Integer;
    function  IndexOf(Value: integer): integer;
    function  Remove(atLine: integer): Integer;
    function  getNextMark(Line: integer): integer;
    function  getPreviousMark(Line: integer): integer;
    procedure Sort;
    procedure AdjustToLineAdded(Line: integer);
    procedure AdjustToLineRemoved(Line: integer);
    procedure Clear;
    destructor destroy; override;
  public
    property Items[Index: Integer]: Integer read Get write Put; default;
    property Count: integer read fCount;
  end;


type
  // Guarda as posições de um escopo
  TNotesEscopePos = record
    EscopeBegin,
    EscopeEnd: TPoint;
    EscopeStart: integer;
  end;

  { Tipo de um indicador de escopo
    @code(eiNone) - não mostra o indicador
    @(eiGutter) - mostra uma linha com braços na gutter
    @(eiEditor) - mostra o indicador no editor  }
  TNotesEscopeIndicatorType = (eiNone, eiGutter, eiEditor);

  TNotesEditorTab = class;

  // plugin para poder pegar linhas inseridas e removidas corretamente
  TNotesSynPlugin = class(TSynEditPlugin)
  private
    fEdTab: TNotesEditorTab;
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: integer); override;
    procedure LinesInserted(FirstLine, Count: integer); override;
    procedure LinesDeleted(FirstLine, Count: integer); override;
  public
    property EditorTab: TNotesEditorTab read fEdTab write fEdTab;
  end;

  { A classe TNotesEditorTab agrupa o synedit, a tab, a classe de coloração
   e adiciona novos comportamentos ao editor permitindo manipular tudo isto em
   conjunto.}
  TNotesEditorTab = class(TObject)
  private
    FTag: Integer;
    FEditor: TSynEdit;
    FHighlighter: TNotesHighlighter;
    FTab: TTabSheet;
    FPlugin: TNotesSynPlugin;
    FFileName: string;
    fFileType: string;
    FLastHS: TLastColoredSymbols;
    FIndent: Integer;
    FCloseHTMLTags: boolean;
    FAutoclose: boolean;
    FNoBackspaceAtLineStart: boolean;
    FCanVKLeftGoUp: boolean;
    FBlockIndent: boolean;
    FHighlightSymbols: boolean;
    FSymbolscolor: TColor;
    FHLineColor: TColor;
    FBlockStart: TStringArray;
    FBlockEnd: TStringArray;
    FBreakType: TNotesBreakType;
    FFullPath: string;

    fLinesCount: integer;
    fMarks: TNotesMarksList;
    fErrorLine: integer;

    fEscopePos : TNotesEscopePos;
    fEscopeIndicator: TNotesEscopeIndicatorType;
    fEscopeIndicatorColor: TColor;
    fEscopeStart: TStringArray;
    fEscopeEnd: TStringArray;

    fLineComment: string;
    fMultilineCommentStart: string;
    fMultilineCommentEnd: string;
    fErrorColor: TColor;

    function GetTabAsSpace: boolean;
    function GetTabIndent: boolean;
    function GetTabSize: integer;
    function GetAutoIndent: boolean;
    function GetCursorPastEof: boolean;
    function GetCursorPastEol: boolean;
    function GetBlockEndTokens: string;
    function GetBlockStartTokens: string;

    procedure SetCloseHTMLTags(const Value: boolean);
    procedure SetTabAsSpace(const Value: boolean);
    procedure SetTabIndent(const Value: boolean);
    procedure SetAutoIndent(const Value: boolean);
    procedure setBlockEndTokens(const Value: string);
    procedure SetBlockIndent(const Value: boolean);
    procedure setBlockStartTokens(const Value: string);
    procedure SetCursorPastEof(const Value: boolean);
    procedure SetHighlightLine(const Value: boolean);
    procedure SetHLineColor(const Value: TColor);
    procedure SetSymbolsColor(const Value: TColor);
    procedure SetCursorPastEol(const Value: boolean);
    procedure SetTabSize(const Value: integer);
    procedure InsertSpaces(const NumberOfSpaces: integer);
    function GetCloseHTMLStr: string;
    function getEscopeEndTokens: string;
    function getEscopeStartTokens: string;
    procedure setEscopeendTokens(const Value: string);
    procedure setEscopeStartTokens(const Value: string);
    function getHighlightLine: boolean;
    procedure setFileType(const Value: string);

  protected

    procedure SetEditor(Value: TSynEdit); virtual;
    procedure doKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure doProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: Char; Data: Pointer); virtual;
    procedure doAfterProcessCmd(Sender: TObject; var Command: TSynEditorCommand; var AChar: Char; Data: Pointer); virtual;
    procedure doSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor); virtual;
    procedure doPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
    procedure doPaint(Sender: TObject; ACanvas: TCanvas);
    procedure doMouseMoves(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure doKeyUP(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure CalcEscopeIndicator;

    function getLineY(const Line: integer): integer;

  public
    { Cria um novo objeto da classe.<BR>
      @code(aOwner) - o componente que vai "ter" a classe e seus componentes.<BR>
      @code(aPgControl) - passe o PageControl no qual a nova tab será inserida.}
    constructor Create(aOwner: TComponent; aPgControl: TPageControl);
    destructor Destroy; override;

    { Carrega o texto de um arquivo. A caption da tab correspondente será atualizada
     automaticamente. A propriedade EolStyle armazenará o tipo de fim de linha do arquivo. }
    function  LoadFromFile( const AFile: string ): TNotesLoadFromFileResult;
    { Retorna uma string com o tipo de fim de linha do arquivo.}
    function  GetFileBreakType: string;
    { Pega a próxima chave. Por exemplo, se no valor de APoint estiver um "(" ele
    vai retornar onde está o próximo ")".}
    function GetMatchingBracket(const APoint: TBufferCoord): TBufferCoord;
    { Permite pegar as opções do synedit mais facilmente.}
    function  IsEditorOptionSetted( Option: TSynEditorOption ): Boolean;

    procedure AttachEvents;
    { Salva o texto para um arquivo. Se EolStyle não for do tipo windows, o texto
     terá seus fins de linhas convertidos para o tipo especificado em EolStyle.}
    procedure SaveToFile( const AFile: string );
    { Desfazer }
    procedure Undo;
    {Copia o texto selecionado para a clipboard. Use este procedimento no lugar
    do procedimento do SynEdit.}
    procedure CopyToClipBoard;
    {Cola o texto da clipboard. Use este procedimento no lugar
    do procedimento do SynEdit.}
    procedure PasteFromClipboard;
    {Recorta o texto selecionado para a clipboard. Use este procedimento no lugar
    do procedimento do SynEdit.}
    procedure CutToClipboard;
    { Deleta o texto selecionado. Use este procedimento no lugar
    do procedimento do SynEdit.}
    procedure Delete;
    { Vai para a linha. }
    procedure GoToLine(const Line: Integer);
    { Vai para a coluna.}
    procedure GoToCol(const Col: Integer);
    { Insere o conteúdo de um arquivo no texto.}
    procedure InsertTextFromFile(const AFile: string);
    { A feature "envolver com" do notes.}
    procedure InsertNCLItem( const FileName: string );
    { Comenta o bloco.}
    procedure CommentBlock;
    { Descomnenta o bloco.}
    procedure UnCommentBlock;
    { Aumenta a indentação.}
    procedure IndentBlock;
    { Diminuí a indentação.}
    procedure UnIndentBlock;
    { Seleciona a linha.}
    procedure SelectLine;
    { Duplica a linha.}
    procedure DuplicateLine;
    { Deleta a linha.}
    procedure DeleteLine;
    { Vai para o próximo braço. }
    procedure GoToMatchingBracket;
    { Permite setar as opções do synedit mais facilmente.}
    procedure SetEditorOption( Option: TSynEditorOption; Enable: Boolean );

    // Recarrega as confiruações para o tipo de arquivo atual
    procedure ReloadConfig;

    procedure ToogleMark;
    procedure NextMark;
    procedure PreviousMark;
    procedure GoToMark(MarkNumber: integer);

    // Lista de marcadores.
    property Marks: TNotesMarksList read fMarks write fMarks;
  published

    { Permite acessar o SynEditor.}
    property Editor: TSynEdit read FEditor write SetEditor;
    { Permite acessar o UniHighlighter.}
    property Highlighter: TNotesHighlighter read FHighlighter write FHighlighter;
    { Guarda o caminho completo do arquivo que está sendo editado.}
    property FullPath: string read FFullPath write FFullPath;
    { Guarda o nome do arquivo que está sendo editado.}
    property FileName: string read fFileName write fFileName;
    { Guarda o tipo de arquivo do arquivo atual.}
    property FileType: string read fFileType write setFileType;
    { Tamanho em espaços da indentação.}
    property IndentSize: integer read fIndent write fIndent default 4;
    { Tamanho em espaços do caracter tab.}
    property TabSize: integer read GetTabSize write SetTabSize;
    { Se true, as tabs serão sempre convertidas para espaços.}
    property TabAsSpace: boolean read GetTabAsSpace write SetTabAsSpace;
    { Permite indentar o código teclando tab.}
    property TabIndent: boolean read GetTabIndent write SetTabIndent;
    { Fecha tags HTML automaticamente.}
    property CloseHTMLTags: boolean read fCloseHTMLTags write SetCloseHTMLTags;
    { Fecha os símbolos de programação automaticamente.}
    property AutoClose: boolean read fAutoclose write fAutoclose;
    { Se true, colore de forma especial símbolos como (, [, etc.}
    property highlightSymbols: boolean read fHighlightSymbols write fHighlightSymbols;
    { Colore a linha atual.}
    property highlightLine:  boolean read getHighlightLine write SetHighlightLine;
    property highlightSymbolsColor: TColor read fSymbolscolor write SetSymbolsColor;
    property highlightLineColor: TColor read fHLineColor write SetHLineColor;
    { Se true, permite que o usuário posicione o cursor após o fim da linha.}
    property CursorPastEol: boolean read GetCursorPastEol write SetCursorPastEol;
    { Se true, o Notes deixa o cursor passar do fim do arquivo. Ele vai inserir
     novas linhas no arquivo se necessário.}
    property CursorPastEof: boolean read GetCursorPastEof write SetCursorPastEof;
    { Se true o usuário pode voltar a linha anterior usando a seta equerda (VK_LEFT).
     Do contrário, se for false, o cusor irá parar ao chegar no início da linha.}
    property CanVKLeftGoUp: boolean read fCanVKLeftGoUp write fCanVKLeftGoUp;
    { Proibe, se true, o usuário de usar backspace ao início da linha.}
    property NoBackspaceAtLineStart: boolean read fNoBackspaceAtLineStart write fNoBackspaceAtLineStart;
    { Se true, ao teclar "enter" a próxima linha terá a mesma identação
     que a linha atual.}
    property AutoIndent: boolean read GetAutoIndent write SetAutoIndent;
    { Se true o Notes vai fazer uam espécie de "SmartIndent" usando
     os valores das propriedades @link(BlockStartTokens) e @link(BlockEndTokens).}
    property BlockIndent: boolean read fBlockIndent write SetBlockIndent;
    { Lista de tokens separados por espaço. Se o usuário teclar Enter
     após um dos tokens desta lista, a próxima linha ganhará uma indentação
     a mais do que a linha atual.}
    property BlockStartTokens: string read GetBlockStartTokens write setBlockStartTokens;
    { Lista de tokens separados por espaço. Se o usuário teclar Enter
     após um dos tokens desta lista, a linha atual perderá uma indentação
     e então a próxima linha terá a mesma indentação que a linha atual.}
    property BlockEndTokens: string read GetBlockEndTokens write setBlockEndTokens;
    { Tipo de fim de linha.}
    property BreakType: TNotesBreakType read FBreakType write FBreakType;
    { Permite acessar a tab.}
    property Tab: TTabSheet read FTab write FTab;
    // linha a ser marcada com a cor de erros
    property ErrorLine: integer read fErrorLine write fErrorLine;
    // cor de erro
    property ErrorsColor: TColor read fErrorColor write fErrorColor;
    // Tipo de indicador de escopo. Vide @link(TNotesEscopeIndicatorType).
    property EscopeIndicator: TNotesEscopeIndicatorType read fEscopeIndicator write fEscopeIndicator;
    // Cor do indicador de escopo
    property EscopeIndicatorColor: TColor read fEscopeIndicatorColor write fEscopeIndicatorColor;
    // Tokens que iniciam um escopo
    property EscopeStartTokens: string read getEscopeStartTokens write setEscopeStartTokens;
    // Tokens que finalizam um escopo
    property EscopeEndTokens: string read getEscopeEndTokens write setEscopeendTokens;
    // Tag
    property Tag: integer read fTag write fTag;

    property LineComment: string read fLineComment write fLineComment;
    property MultilineCommentStart: string read fMultilineCommentStart write fMultilineCommentStart;
    property MultilineCommentEnd: string read fMultilineCommentEnd write fMultilineCommentEnd;
  end;

function DisplayCoordToPoint(const Coord: TDisplayCoord): TPoint;
function PointToDisplayCoord(const APoint: TPoint): TDisplayCoord;
function BufferCoordToPoint(const Coord: TBufferCoord): TPoint;
function PointToBufferCoord(const APoint: TPoint): TBufferCoord;

Var
  ProblemsList: TNotesEditorTabPosList;
  EditLocationsList: TNotesEditorTabPosList;

implementation

// resource com os ícones dos marcadores/problemas
{$R NotesEditorTab.res}

uses
  ShellAPI, FastStrings, SynEditHighlighter, NotesXML, NotesGlobals,
  NotesToolTip, NotesFileTypeOptions, SynEditTextBuffer, StrUtils, Math;

Var
  MarkIco: HICON;
  ProblemIco: HICON;
  fProblemIcon: TIcon;
  fMarkIcon: TIcon;
  tip: TNotesToolTip;

Const
  (* Tags HTML que não devem ser fechadas *)
  ExcludeTags: Array[0..86] of string = ('br', 'img', 'hr', '!DOCTYPE',
  'meta', 'link', 'frame', '!--', '--', '', ' ', '?xml', 'AREA',
  'BGSOUND', 'OVERLAY', 'BASE', 'RANGE', 'INPUT', 'MULTICOL',
  'BASEFONT', 'TEXTFLOW', 'PARAM', 'SPACER', 'WBR', 'ISINDEX',
  (* Tags do ColdFusion que não devem ser fechadas *)
  'cfset', 'cfelse', 'cfelseif', 'cflocation', 'cfabort', 'cfapplet', 'cfapplication',
  'cfargument', 'cfreturn', 'cfassociate', 'cfbreak','cfcache', 'cfchartdata','cfcol',
  'cfcollection','cfcontent', 'cfcookie','cfdirectory', 'cfdump','cferror', 'cfexit',
  'cffile', 'cfflush','cfftp', 'cfgridcolumn', 'cfgridrow','cfgridupdate', 'cfheader',
  'cfhtmlhead', 'cfhttpparam','cfimport', 'cfinclude','cfindex', 'cfinput','cfinsert',
  'cfinvokeargument', 'cfldap','cflocation', 'cflog','cfloginuser', 'cflogout',
  'cfmailparam', 'CFMODULE','cfobject', 'cfobjectcache','cfparam', 'cfpop', 'cfprocparam',
  'cfprocresult','cfproperty', 'cfqueryparam','cfregistry', 'cfrethrow','cfschedule',
  'cfsearch','cfsetting', 'cfslider','cftextinput', 'cftreeitem', 'cfupdate','cfwddx',
  'cfauthenticate');

//
//  FUNÇÕES UTEÍS
//

//-----------------------------------------------------------------

(**
 * Procedimento: IsInStrArray
 * Descrição:    Retorna true se a string ASearch estiver contida no
 *               Array de String passado em ASource
 * Autor:        Anderson R. Barbieri
 * Data:         25-nov-2003
 * Argumentos:   ASearch: string; ASource: Array of String
 * Resultado:    boolean
 *)
function IsInStrArray(const ASearch: string; const ASource: TStringArray): boolean; overload;
Var
  I: integer;
begin
  Result:= false;
  for I:= 0 to high(ASource) do
    if SameText(ASource[I], ASearch) then begin
      Result:= true;
      Break;
      Exit;
    end;
end;

function IsInStrArray(const ASearch: string; const ASource: array of string): boolean; overload;
Var
  I: integer;
begin
  Result:= false;
  for I:= 0 to high(ASource) do
    if SameText(ASource[I], ASearch) then begin
      Result:= true;
      Break;
      Exit;
    end;
end;

function DisplayCoordToPoint(const Coord: TDisplayCoord): TPoint;
begin
  Result.X:= Coord.Column;
  Result.Y:= Coord.Row;
end;

function PointToDisplayCoord(const APoint: TPoint): TDisplayCoord;
begin
  Result.Column:= APoint.X;
  Result.Row:= APoint.Y;
end;

function BufferCoordToPoint(const Coord: TBufferCoord): TPoint;
begin
  Result.X:= Coord.Char;
  Result.Y:= Coord.Line;
end;

function PointToBufferCoord(const APoint: TPoint): TBufferCoord;
begin
  Result.Char:= APoint.X;
  Result.Line:= APoint.Y;
end;

function GetLastWord(const S: string): String;
Var
  I: integer;
begin
  I:= length(S);
  if I < 1 then Exit;
  while (S[I] <> #32) and (S[I] <> #9) and (I > 1) do
    Dec(I);
  Result:= Copy(S, I, length(S));
end;

function TabsToSpaces(const S: string; const TabSize: integer): string;
begin
  Result:= StringReplace(S, #9, StringOfChar(#32, TabSize), [rfReplaceAll]);
end;

function SpacesToTabs(const S: string; const TabSize: integer): string;
begin
  Result:= StringReplace(S, StringOfChar(#32, TabSize), #9, [rfReplaceAll]);
end;


//-----------------------------------------------------------------

(**
 * Procedimento: IsInTag
 * Descrição:    Retorna true se a posição passada em YourPas
 *               estiver dentro do trecho especificado por
 *               StartTag e EndTag.
 * Autor:        Anderson R. Barbieri
 * Data:         26-nov-2003
 * Argumentos:   const S, StartTag, EndTag: string; const YourPos, StartTagLen, EndTagLen: integer
 * Resultado:    boolean
 *)
function IsInTag(const S, StartTag, EndTag: string;
  const YourPos, StartTagLen, EndTagLen: integer): boolean;
var
  DangerousTagsBegin, DangerousTagsEnd, Len: integer;
begin
  Result:= false;
  Len:= length(S);

  DangerousTagsBegin:= FastPosBackNoCase(S, StartTag,
  Len, StartTagLen, YourPos);

  if DangerousTagsBegin > 0 then begin
    DangerousTagsEnd:= FastPosBackNoCase(S, EndTag,
      Len, EndTagLen, YourPos);
    // se a tag de fim for anterior a de início,
    // então estamos dentro da tag
    if DangerousTagsEnd < DangerousTagsBegin then
      Result:= true;
  end;

end;


{ TNotesEditorTab }

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.AttachEvents
 * Descrição:    Anexa-se aos eventos do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.AttachEvents;
begin
  if assigned(FEditor) then
  begin
    FEditor.OnProcessCommand    := doProcessCommand;
    FEditor.OnCommandProcessed  := doAfterProcessCmd;
    FEditor.OnKeyPress          := doKeyPress;
    FEditor.OnPaintTransient    := doPaintTransient;
    FEditor.OnSpecialLineColors := doSpecialLineColors;
    FEditor.OnKeyUp             := doKeyUP;
    fEditor.OnMouseMove         := doMouseMoves;
    fEditor.OnPaint             := doPaint;
  end;
end;


constructor TNotesEditorTab.Create(aOwner: TComponent; aPgControl: TPageControl);
begin
  FTab := TTabSheet.Create(nil);
  FTab.Parent:= aPgControl;
  FTab.PageControl := aPgControl;
  FTab.Tag := Integer( Self );

  FEditor := TSynEdit.Create(nil);
  // Limpamos as keystrokes, os atalhos serão gerenciados pelo Notes.
  FEditor.Keystrokes.Clear;
  FHighlighter:= nil;
  FEditor.Align:= alClient;
  FEditor.ScrollHintFormat:= shfTopToBottom;
  FEditor.ScrollBars:= ssBoth;
  FEditor.TabOrder:= 0;
  FEditor.WantTabs:= true;

  SetEditorOption(eoAutoSizeMaxScrollWidth, true);
  SetEditorOption(eoDisableScrollArrows, true);
  SetEditorOption(eoDragDropEditing, true);
  SetEditorOption(eoDropFiles, true);
  SetEditorOption(eoGroupUndo, true);
  SetEditorOption(eoRightMouseMovesCursor, true);
  SetEditorOption(eoShowScrollHint, true);
  SetEditorOption(eoKeepCaretX, true);
  SetEditorOption(eoSpecialLineDefaultFg, true);
  SetEditorOption(eoHideShowScrollbars, false);
  SetEditorOption(eoAltSetsColumnMode, true);

  //OnSpecialLineColors: TSpecialLineColorsEvent

  fIndent := 4;
  fLastHS.Cleared:= true;
  fCloseHTMLTags:= true;
  fAutoclose:= true;
  fNoBackspaceAtLineStart:= false;
  fCanVKLeftGoUp:= true;
  fBlockIndent:= true;
  fHighlightSymbols:= true;
  fSymbolscolor:= clLtGray;
  fHLineColor:= clSilver;

  fMarks:= TNotesMarksList.Create;
  BreakType:= btWin;

  fEscopeIndicator:= eiGutter;
  fEscopeIndicatorColor:= clLtGray;

  FEditor.Parent:= FTab;

  // plugins são distruídos pelo próprio synedit
  FPlugin:= TNotesSynPlugin.Create(FEditor);
  FPlugin.EditorTab:= self;

  AttachEvents;
end;

destructor TNotesEditorTab.Destroy;
begin
  if FEditor <> nil then
    FreeAndNil( FEditor );

  if FHighlighter <> nil then
    FreeAndNil( FHighlighter );

  if FTab <> nil then
    FreeAndNil( FTab );

  fMarks.Free;

  inherited Destroy;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetEditor
 * Descrição:    Seta o SynEdit ao qual estará anexado
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 *)
procedure TNotesEditorTab.SetEditor(Value: TSynEdit);
begin
  if FEditor <> Value then begin
    FEditor := Value;
    AttachEvents;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.doAfterProcessCmd
 * Descrição:    Acionado depois q um comando é processado
 *)
procedure TNotesEditorTab.doAfterProcessCmd(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if ((Command = ecDeleteLastChar) or (Command = ecDeleteChar)) and (fLinesCount > fEditor.Lines.Count) then
  begin
    fMarks.AdjustToLineRemoved(fEditor.CaretY);
    ProblemsList.AdjustToLineRemoved(fEditor.CaretY, self.FullPath);
  end;

  if (Command > 500) and (Command > 605) then
  begin
    if (FullPath <> '') and (EditLocationsList.Count > 0) and (((EditLocationsList.Last.Line > fEditor.CaretY + 8) or
    (EditLocationsList.Last.Line < fEditor.CaretY - 8)) or (EditLocationsList.Last.FileName <> FullPath)) then
        EditLocationsList.Add(fEditor.CaretY, fEditor.CaretX, FFullPath, '');
    if (EditLocationsList.Count = 0) then
        EditLocationsList.Add(fEditor.CaretY, fEditor.CaretX, FFullPath, '');
   end;

  if not ((GetKeyState(VK_UP) < 0) or (GetKeyState(VK_DOWN) < 0)) then
    CalcEscopeIndicator;

  if (fEditor.CaretY < fEscopePos.EscopeBegin.Y) or (fEditor.CaretY > fEscopePos.EscopeEnd.Y) then
  begin
    fEscopePos.EscopeBegin:= Point(0,0);
    fEscopePos.EscopeEnd:= Point(0,0);
  end;

  case fEscopeIndicator of
    eiGutter: fEditor.InvalidateGutter;
    eiEditor: fEditor.Repaint;
  end;

end;

//----------------------------------------------------------------

// Mostra uma tooltip com o problema que ocorreu na linha qdo a
// linha tem um problema
procedure TNotesEditorTab.doMouseMoves(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  CL, I: integer;
begin
  if fEditor.Dragging then Exit;

  if tip.isShowing then Exit;

  // O mouse está na gutter?!
  if (X > 0) and (X < fEditor.Gutter.Width-6+16) then
  begin

    // Se o botão do mouse estiver sendo pressionado, saímos
    if (ssRight in Shift) or (ssLeft in Shift) or (ssMiddle in Shift) then Exit;

    if ProblemsList.Count = 0 then Exit;

    if (X < fEditor.Gutter.Width - 6 +16) and (x > fEditor.Gutter.Width -6) then
    begin
      // CL --> linha em que está o mouse
      CL:= Y div fEditor.LineHeight;
      CL:= CL + fEditor.TopLine;
      CL:= FEditor.RowToLine(CL);

      // vemos se na linha CL há um ícone de erro
      for I:= 0 to ProblemsList.Count-1 do
      begin
        if (ProblemsList.Items[I].Line = CL) and (SameText(self.FullPath, ProblemsList.Items[I].FileName)) then
        begin
          tip.Title:= 'Problema:';
          tip.Text:= ProblemsList.Items[I].Info;
          tip.Show;

          Exit;
        end;
      end;
    end;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.doSpecialLineColors
 * Descrição:    Colore a linha fErrorLine com fErrorColor
 * Autor:        Josimar Silva
 *)
procedure TNotesEditorTab.doSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = fErrorLine then
  begin
    Special:= true;
    BG:= fErrorColor;
    FG:= clWhite;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.doKeyPress
 * Descrição:    Recebe os KeyPress do editor
 *               (e implementa Autoclose, closeHTMLTags, etc.)
 * Autor:        Anderson R. Barbieri
 *)
procedure TNotesEditorTab.doKeyPress(Sender: TObject; var Key: Char);
var
  I: integer;

  function CheckIsShouldAutoCompleteChars(Key: Char): boolean;
  Var
    Len: integer;
    S: string;
    I, C: integer;
  begin
    S:= FEditor.LineText;
    Len:= length(S);
    Result:= false;
    // Se o cursor estiver no final da linha sempre autocompletamos
    if (Len < 2) or (FEditor.CaretX > Len -1) then
    begin
      Result:= true;
    end else
    begin
      // usamos C para guardar a posição do caracter na tabela ASCII
      C:= Ord(S[FEditor.CaretX]);
      if (C < 48) and ((C > 57) and (C < 65))  and ( (C > 90) and
        (C < 97) ) and ( (C > 122) and (C < 192) )
        and (C <> 32) and (C <> 9) then
        Result:= true;
    end;
    { Os caracteres " e ' precisam de um tratamento especial
     por que o seu caracter de fechamento é ele mesmo. Se o usuário
     estiver fechando um trecho com ' ou ", não devemos completar.
     Para isto contamos o número dele na linha. Se o número for
     ímpar, então NÂO autocompletamos. }
    if (Result) and (Key in [#34, #39]) then
    begin
      // Guardamos a contagem em C - iniciado como 2 para evitar division by zero
      C:= 2;
      for I:= 1 to Len do
        if S[I] = Key then
          inc(C);
      if (C mod 2 <> 0) then
        Result:= false;
    end;
  end;

begin

  if fErrorLine > 0 then
  begin
    fErrorLine:= -1;
    fEditor.Invalidate;
  end;

  if (fAutoclose) and (CheckIsShouldAutoCompleteChars(Key)) then
  begin
    Case Key of
      '(':
        begin
          Key:= #0;
          FEditor.ExecuteCommand(ecChar, '(', nil);
          FEditor.ExecuteCommand(ecChar, ')', nil);
          FEditor.CaretX:= FEditor.CaretX - 1;
        end;

      '[':
        begin
          Key:= #0;
          FEditor.ExecuteCommand(ecChar, '[', nil);
          FEditor.ExecuteCommand(ecChar, ']', nil);
          FEditor.CaretX:= FEditor.CaretX - 1;
        end;

      '"':
        begin
          Key:= #0;
          FEditor.ExecuteCommand(ecChar, '"', nil);
          FEditor.ExecuteCommand(ecChar, '"', nil);
          FEditor.CaretX:= FEditor.CaretX - 1;
        end;

      #39:
        begin
          Key:= #0;
          FEditor.ExecuteCommand(ecChar, #39, nil);
          FEditor.ExecuteCommand(ecChar, #39, nil);
          FEditor.CaretX:= FEditor.CaretX - 1;
        end;

      '{':
        begin
          Key:= #0;
          FEditor.ExecuteCommand(ecChar, '{', nil);
          FEditor.ExecuteCommand(ecChar, '}', nil);
          FEditor.CaretX:= FEditor.CaretX - 1;
        end;

    end;
  end;

  if (Key = '>') and (fCloseHTMLTags) and (length(FEditor.LineText) > 1)
   and (FEditor.CaretX > 1) and
   // EM XML e XHTML as tags que acabam em / não devem ser fechadas
   (FEditor.LineText[FEditor.CaretX-1] <> '/') then
  begin
    Key:= #0;
    I:= FEditor.SelStart;
    FEditor.SelText:= GetCloseHTMLStr;
    FEditor.SelStart:= I + 1;
  end;

end;

//-----------------------------------------------------------------

(**
 * Descrição:    O SynEdit tem uma coisa maravilhosa chamada "comandos".
 *               Basicamente, tudo que ele faz (mudar a posição do cursor,
 *               deletar o próximo caracter, inserir uma linha) é traduzido
 *               para algum comando. Então nada melhor para modificar o
 *               comportamento do SynEdit do que receber os comandos que
 *               chegam e transformaá-los para aquilo que a gente quer fazer.
 *               Praticamente todos os comportamentos novos estão
 *               implementados neste procedure.
 * Autor:        Anderson R. Barbieri
 *)
procedure TNotesEditorTab.doProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
var
  S: string;
  I: integer;
begin
  fLinesCount:= FEditor.Lines.Count;

  if fErrorLine > 0 then
  begin
    fErrorLine:= -1;
    fEditor.Invalidate;
  end;

  Case command of

    ecClearAll:
    begin
      fMarks.Clear;
      ProblemsList.ClearItemsForFile(self.FullPath);
      fEditor.InvalidateGutter;
    end;

    { Possibilita que o cursor passe de verdade do Eof }
    ecDown:
    if IsEditorOptionSetted( eoScrollPastEof ) and ( ( FEditor.CaretXY.Line = fLinesCount ) or ( fLinesCount = 0 ) ) then begin
      FEditor.BeginUpdate;
      I := FEditor.CaretX;
      FEditor.ExecuteCommand( ecLineEnd, #0, nil );
      FEditor.ExecuteCommand( ecInsertLine, #0, nil );
      FEditor.CaretX:= I;
      FEditor.EndUpdate;
    end;


    ecLineBreak:
    begin
      fMarks.AdjustToLineAdded(fEditor.CaretY-1);
      ProblemsList.AdjustToLineAdded(fEditor.CaretY-1, self.FullPath);

      if Not fBlockIndent then Exit;

      S:= FEditor.LineText;
      if Length(S)+1 > FEditor.CaretX then
        Exit;
      S:= trim(S);
      if S = EmptyStr then Exit;
      // início de bloco: aumentar identação da próxima linha
      if isInStrArray(GetLastWord(S), fBlockStart) then begin
        // paramos a quebra de linha
        Command:= ecNone;
        // descobrimos quantos espaços tem a linha atual
        I:= 1;
        S:= FEditor.LineText;
        while (S[I] in [#32, #9]) and (I < length(S)) do
          if S[I] = #32 then
            inc(I)
          else
            inc(I, FEditor.TabWidth);

        // inserimos a nova linha
        FEditor.ExecuteCommand(ecInsertLine, #0, nil);
        // Mandamos o cursor para a nova linha
        FEditor.CaretXY:= BufferCoord( 0, FEditor.CaretY  + 1);
        // inserimos os espaços mais uma indentação
        InsertSpaces(I + fIndent);
        // mandamos o cursor para o fim da linha
        FEditor.CaretXY:= BufferCoord( I + fIndent  , FEditor.CaretY);

      end
      // fim de bloco: diminuí a identação da linha atual e manda
      // o cursor para a próxima linha
      else if isInStrArray(GetLastWord(S), fBlockEnd) then begin
        // paramos a quebra de linha
        Command:= ecNone;
        // descobrimos quantos espaços tem a linha atual
        I:= 1;
        S:= FEditor.LineText;
        while (S[I] in [#32, #9]) and (I < length(S)) do begin
          if S[I] = #32 then
            inc(I)
          else
            inc(I, FEditor.TabWidth);
        end;

        // no caso da identação da linha estar pequena demais
        // apenas colocamos tudo no ínicio da linha
        if I - fIndent < 0 then begin
          FEditor.LineText:= Trim(FEditor.LineText);
          Command:= ecLineBreak;
          Exit;
        end;
        // tiramos uma identação da linha atual
        FEditor.LineText:= Copy(FEditor.LineText, fIndent+1, length(FEditor.LineText));
        // inserimos a nova linha
        FEditor.ExecuteCommand(ecInsertLine, #0, nil);
        // Mandamos o cursor para a nova linha
        FEditor.CaretXY:= BufferCoord( 0, FEditor.CaretY  + 1);
        // inserimos os espaços com menos uma indentação
        InsertSpaces(I - fIndent);
        // mandamos o cursor para o fim da linha
        FEditor.CaretXY:= BufferCoord( I - fIndent  , FEditor.CaretY);
      end;

    end;

    ecSelLeft..ecSelGotoXY, ecSelectAll:
    begin
      // Não pintamos o escopo qdo há txt selecionado
      fEscopePos.EscopeBegin:= Point(0,0);
      fEscopePos.EscopeEnd:= Point(0,0);
      fEscopePos.EscopeStart := 0;
    end;

    ecLeft:
    begin
      if fCanVKLeftGoUp then begin
        if FEditor.CaretX = 1 then
          if FEditor.CaretY > 1 then begin
            FEditor.ExecuteCommand(ecUp, #0, nil);
            Command:= ecLineEnd;
          end;
      end
      else if FEditor.CaretX = 1 then
        Command:= ecNone;
    end;

    // opção para proibir backspace no início da linha
{    ecDeleteLastChar:
    begin
      if fNoBackspaceAtLineStart and ( FEditor.CaretX = 1 ) then
          Command:= ecnone;}
    //end;
  end;
end;

//-----------------------------------------------------------------

// Colore símbolos (, ), {, }, [, ] coincidentes
procedure TNotesEditorTab.doPaintTransient(Sender: TObject;
  Canvas: TCanvas; TransientType: TTransientType);
const
  BracketSet = ['{','[','(','}',']',')'];
  OpenChars:array[0..2] of Char=('{','[','(');
  CloseChars:array[0..2] of Char=('}',']',')');
var
  S: String;
  SaveColor: TColor;
  Len: integer;
  FirstBracket, LastBracket: TBufferCoord;
  Pix: TPoint;
  D: TDisplayCoord;
  C: char;
begin
  if fEditor.SelAvail then Exit;
  if not fHighlightSymbols then Exit;

  S:= FEditor.LineText;
  Len:= length(S);
  C:= #0;
  if (Len > 0) and (FEditor.CaretX-1 > 0) and (FEditor.CaretX-1 <= Len) then
  begin
    C:= S[FEditor.CaretX-1];
    if not (C in BracketSet) then
      C:= #0;
    // micro-economia de memória
    SetLength(S, 0);
  end;

  FirstBracket.Char:= FEditor.CaretX-1;
  FirstBracket.Line:= FEditor.CaretY;
  if C = #0 then
  begin
    LastBracket.Char:= 0;
    LastBracket.Line:= 0;
  end else
    LastBracket:= self.GetMatchingBracket(FirstBracket);

  // limpamos a última coloração de símbolos se
  // ela estiver ainda "suja"
  if (fLastHS.Cleared = false) and (fLastHS.P1.X = FirstBracket.Char) and  (fLastHS.P1.Y = FirstBracket.Line) then
  begin
    FEditor.InvalidateLine(fLastHS.P1.Y);
    FEditor.InvalidateLine(fLastHS.P2.Y);
    fLastHS.Cleared:= true;
  end
  else if (C <> #0) then
  begin

    SaveColor:= FEditor.Canvas.Brush.Color;

    if LastBracket.Char <> 0 then
      FEditor.Canvas.Brush.Color:= fSymbolscolor
    else
      FEditor.Canvas.Brush.Color:= fErrorColor;

    fLastHS.Cleared:= false;
    fLastHS.P1:= BufferCoordToPoint(FirstBracket);
    fLastHS.P2:= BufferCoordToPoint(LastBracket);

    D:= FEditor.BufferToDisplayPos(FirstBracket);
    Pix:= FEditor.RowColumnToPixels(D);

    FEditor.Canvas.FrameRect(Rect(Pix.X , Pix.Y+2 , Pix.X + FEditor.CharWidth, Pix.Y + FEditor.LineHeight -1));

    D:= FEditor.BufferToDisplayPos(LastBracket);
    Pix:= FEditor.RowColumnToPixels(D);

    if (D.Row > 0) and (D.Column > 0) then
      FEditor.Canvas.FrameRect(Rect(Pix.X, Pix.Y+2 , Pix.X + FEditor.CharWidth, Pix.Y + FEditor.LineHeight-1));

    FEditor.Canvas.Brush.Color:= SaveColor;
  end;
end;

//-----------------------------------------------------------------

{ Função copiada do SynEdit, fiz apenas modificações para que passasse a ser
 compatível com o NotesHiglighter e para que tivesse uma limitação no número
 de linhas em que a busca é realizada.}
function TNotesEditorTab.GetMatchingBracket(const APoint: TBufferCoord): TBufferCoord;
const
  Brackets: array[0..7] of char = ('(', ')', '[', ']', '{', '}', '<', '>');
  // It won't search more lines than MaxLines to speed up the things in big files :)
  MaxLines: integer = 800;
var
  Line: string;
  i, PosX, PosY, Len, SearchedLinesCount: integer;
  Test, BracketInc, BracketDec: char;
  NumBrackets: integer;
  p: TBufferCoord;
begin
  Result.Char := 0;
  Result.Line := 0;
  // get char at caret
  PosX := APoint.Char;
  PosY := APoint.Line;
  Line := FEditor.Lines[ APoint.Line -1 ];
  if Length(Line) >= PosX then
  begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then
      begin
        // this is the bracket, get the matching one and the direction
        BracketInc := Brackets[i];
        BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
        // search for the matching bracket (that is until NumBrackets = 0)
        NumBrackets := 1;
        if Odd(i) then
        begin
          SearchedLinesCount:= 0;
          repeat
            // search until start of line
            while PosX > 1 do
            begin
              Dec(PosX);
              Test := Line[PosX];

              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if (Test = BracketInc) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then break;
            if SearchedLinesCount > MaxLines then Break;
            Dec(PosY);
            Inc(SearchedLinesCount);
            Line := FEditor.Lines[PosY - 1];
            PosX := Length(Line) + 1;
          until FALSE;
        end
        else begin
          SearchedLinesCount:= 0;
          repeat
            // search until end of line
            Len := Length(Line);
            while PosX < Len do
            begin
              Inc(PosX);
              Test := Line[PosX];

              p.Char := PosX;
              p.Line := PosY;
              if (Test = BracketInc) or (Test = BracketDec) then
              begin
                if (Test = BracketInc) then
                  Inc(NumBrackets)
                else if (Test = BracketDec) then
                begin
                  Dec(NumBrackets);
                  if NumBrackets = 0 then
                  begin
                    // matching bracket found, set caret and bail out
                    Result := P;
                    exit;
                  end;
                end;
              end;
            end;
            // get next line if possible
            if PosY = FEditor.Lines.Count then
              Break;
            if SearchedLinesCount > MaxLines then Break;
            Inc(SearchedLinesCount);
            Inc(PosY);
            Line := FEditor.Lines[PosY - 1];
            PosX := 0;
          until False;
        end;
        // don't test the other brackets, we're done
        break;
      end;
  end;
end;

//-----------------------------------------------------------------


procedure TNotesEditorTab.Undo;
begin
  if FEditor.CanUndo then
  begin
    fEditor.ExecuteCommand(ecUndo, #0, nil);
    // evita que depois de desfazer o estado mude para não-modificado
    FEditor.Modified:= true;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.CopyToClipBoard
 * Descrição:    Copia para a clipboard
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.CopyToClipBoard;
begin
  FEditor.CopyToClipboard
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.CutToClipboard
 * Descrição:    Recorta para a clipboard
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.CutToClipboard;
begin
  FEditor.CutToClipboard
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.PasteFromClipboard
 * Descrição:    Cola o conteúdo da clipboard
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.PasteFromClipboard;
begin
  FEditor.PasteFromClipboard
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.Delete
 * Descrição:    Deleta o próximo caracter/seleção
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.Delete;
begin
  if fEditor.selAvail then
    FEditor.ClearSelection
  else
    fEditor.ExecuteCommand(ecDeleteChar, #0, nil);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GoToCol
 * Descrição:    Vai para a coluna especificada em "col".
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   const Col: integer
 * Resultado:    None
 *)
procedure TNotesEditorTab.GoToCol(const Col: integer);
begin
  if length(FEditor.LineText) >= Col then
    FEditor.CaretX:= Col
  else
    FEditor.ExecuteCommand(ecLineEnd, #0, nil);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GoToLine
 * Descrição:    Vai para a linha especificada em "line".
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   const Line: integer
 * Resultado:    None
 *)
procedure TNotesEditorTab.GoToLine(const Line: integer);
begin
  FEditor.CaretY:= Line;
end;

//-----------------------------------------------------------------

// Aumenta a identação de um bloco de código ou linha
procedure TNotesEditorTab.IndentBlock;
var
  SelLen, I: integer;
  SaveCaret, BB, BE: TBufferCoord;
  SL: TStringList;
  S, Spaces: string;
  AddTerminator, AdjustForTabs: boolean;
begin
  FEditor.BeginUpdate;

  if FEditor.SelAvail then
  begin
    // Arruma a seleção para que selecione todas as linhas totalmente
    BB:= FEditor.BlockBegin;
    BE:= FEditor.BlockEnd;
    BB.Char:= 1;
    FEditor.CaretXY:= BB;
    FEditor.BlockBegin:= BB;

    if (FEditor.SelectionMode = smNormal) or (BE.Line = BB.Line) then
    begin
      if BE.Char > 1 then
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) )
      else
      begin
        if BE.Line > 1 then
          BE.Line := BE.Line - 1;
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) );
      end;
    end else
    begin
      // na seleção de colunas é preciso que BE.Char seja do tamanho da maior linha
      for I:= BB.Line to BE.Line do
        BE.Char:= Max( BE.Char, length( TabsToSpaces(FEditor.Lines.Strings[I-1], FEditor.TabWidth)) +1);
    end;

    FEditor.BlockEnd:= BE;

    S:= FEditor.SelText;
    AddTerminator:= false;

    if eoTabsToSpaces in FEditor.Options then
      AdjustForTabs:= false
    else // se tiver tabs, inferimos que a pessoa gosta de usá-las :)
      if (BB.Line > 0) and (FEditor.Lines.Count > 0) then
        AdjustForTabs:= ( Pos(#9, FEditor.Lines.Strings[BB.Line-1]) > 0 )
      else
        AdjustForTabs:= false;

    SelLen:= length(S);
    if S[SelLen] in [#13, #10] then
      AddTerminator:= true;

    S:= TabsToSpaces(S, FEditor.TabWidth);

    SL:= TStringList.Create;
    try
      SL.Text:= S;
      S:= '';
      Spaces:= StringOfchar(#32, fIndent);
      // Por algum motivo maluco a StringList insere uma quebra
      // de linha ao final do texto mesmo que não precise. O jeito
      // é pegar linha a linha e "reconstruir" a string manualmente
      for I:= 0 to SL.Count-2 do
        S:= S + Spaces + SL.Strings[I] + ASCII_CRLF;
      if SL.Count > 0 then
        S:= S + Spaces + SL.Strings[SL.Count-1];
      if AddTerminator then
        S:= S + ASCII_CRLF;

      if AdjustForTabs then
        S:= SpacesToTabs(S, FEditor.TabWidth);

      FEditor.SelText:= S;

      FEditor.CaretXY:= BB;
      FEditor.BlockBegin:= BB;
      BE.Char:= 1;
      if (FEditor.SelectionMode = smNormal) or (BB.Line = BE.Line) then
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) ) +1
      else
        for I:= BB.Line to BE.Line do
          BE.Char:= Max( BE.Char, length( TabsToSpaces(FEditor.Lines.Strings[I-1], FEditor.TabWidth)) +1);

      FEditor.BlockEnd:= BE;

    finally
      SL.Free;
    end;
  end else
  begin
    // Unindenta a linha
    SaveCaret:= FEditor.CaretXY;
    self.SelectLine;
    // Evita que a função se chame recursivamente infinitamente :)
    if FEditor.SelAvail then
      self.IndentBlock;
    FEditor.SelLength:= 0;
    FEditor.CaretXY:= SaveCaret;
  end;

  FEditor.EndUpdate;
end;

// Diminuí a indentação de um bloco de código ou linha
procedure TNotesEditorTab.UnindentBlock;
var
  SelLen, I: integer;
  SaveCaret, BB, BE: TBufferCoord;
  SL: TStringList;
  S: string;
  AddTerminator, AdjustForTabs: boolean;

  // deleta SizeToDel espaços do início da string. Se SizeToDel
  // for maior q o número de espaços, deleta todos os espaços
  // do início da linha
  function DeleteSpaces(const S: string; SizeToDel: integer): string;
  var
    I: integer;
    Len: integer;
  begin
    Len:= length(S);
    I:= 1;
    SizeToDel:= SizeToDel + 1;
    while (I <= Len) and (S[I] = #32) do
      inc(I);
    if I > SizeToDel then
      I:= SizetoDel;
    Result:= Copy(S, I, Len);
  end;

begin
  FEditor.BeginUpdate;

  if FEditor.SelAvail then
  begin
    // Arruma a seleção para que selecione todas as linhas totalmente
    BB:= FEditor.BlockBegin;
    BE:= FEditor.BlockEnd;
    BB.Char:= 1;
    FEditor.CaretXY:= BB;
    FEditor.BlockBegin:= BB;

    if (FEditor.SelectionMode = smNormal) or (BE.Line = BB.Line) then
    begin
      if BE.Char > 1 then
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) )
      else
      begin
        if BE.Line > 1 then
          BE.Line := BE.Line - 1;
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) );
      end;
    end else
    begin
      // na seleção de colunas é preciso que BE.Char seja do tamanho da maior linha
      for I:= BB.Line to BE.Line do
        BE.Char:= Max( BE.Char, length( TabsToSpaces(FEditor.Lines.Strings[I-1], FEditor.TabWidth)) +1);
    end;

    FEditor.BlockEnd:= BE;

    S:= FEditor.SelText;
    AddTerminator:= false;

    if eoTabsToSpaces in FEditor.Options then
      AdjustForTabs:= false
    else // se tiver tabs, inferimos que a pessoa gosta de usá-las :)
      if (BB.Line > 0) and (FEditor.Lines.Count > 0) then
        AdjustForTabs:= ( Pos(#9, FEditor.Lines.Strings[BB.Line-1]) > 0 )
      else
        AdjustForTabs:= false;

    SelLen:= length(S);
    if S[SelLen] in [#13, #10] then
      AddTerminator:= true;

    S:= TabsToSpaces(S, FEditor.TabWidth);

    SL:= TStringList.Create;
    try
      SL.Text:= S;
      S:= '';
      // Por algum motivo maluco a StringList insere uma quebra
      // de linha ao final do texto mesmo que não precise. O jeito
      // é pegar linha a linha e "reconstruir" a string manualmente
      for I:= 0 to SL.Count-2 do
        S:= S + DeleteSpaces(SL.Strings[I], fIndent) + ASCII_CRLF;
      if SL.Count > 0 then
        S:= S + DeleteSpaces(SL.Strings[SL.Count-1], fIndent);
      if AddTerminator then
        S:= S + ASCII_CRLF;

      if AdjustForTabs then
        S:= SpacesToTabs(S, FEditor.TabWidth);

      FEditor.SelText:= S;

      FEditor.CaretXY:= BB;
      FEditor.BlockBegin:= BB;
      BE.Char:= 1;
      if (FEditor.SelectionMode = smNormal) or (BB.Line = BE.Line) then
        BE.Char:= length( TabsToSpaces(FEditor.Lines.Strings[BE.Line-1], FEditor.TabWidth) ) +1
      else
        for I:= BB.Line to BE.Line do
          BE.Char:= Max( BE.Char, length( TabsToSpaces(FEditor.Lines.Strings[I-1], FEditor.TabWidth)) +1);

      FEditor.BlockEnd:= BE;

    finally
      SL.Free;
    end;
  end else
  begin
    // Unindenta a linha
    SaveCaret:= FEditor.CaretXY;
    self.SelectLine;
    // Evita que a função se chame recursivamente infinitamente :)
    if FEditor.SelAvail then
      self.UnIndentBlock;
    FEditor.SelLength:= 0;
    FEditor.CaretXY:= SaveCaret;
  end;

  FEditor.EndUpdate;
end;

procedure TNotesEditorTab.InsertTextFromFile(const AFile: string);
begin
  //
end;

procedure TNotesEditorTab.InsertNCLItem(const FileName: string);
Var
  Len, I, J: integer;
  S, T, SelIndent: string;
  SL: TStringList;
  Surround: boolean;
  txtToInsert: string;
begin
  Surround:= false;

  if fEditor.SelAvail then
  begin

   // para termos surround a seleção deve iniciar no
   // início da linha ou não deve haver nenhum caracter
   // não selecionado antes do início da seleção
   if fEditor.BlockBegin.Char > 1 then
   begin
     if fEditor.BlockBegin.Line > -1 then
     begin
       S:= fEditor.Lines.Strings[fEditor.BlockBegin.Line];
       if Trim(Copy(S, 1, fEditor.BlockBegin.Char)) = '' then
         Surround:= true;
     end;
   end else
       Surround:= true;

    if Surround then
    begin
      SL:= TStringList.Create;
      try
        SL.LoadFromFile(FileName);
        for I := 0 to SL.Count - 1 do
        begin
          S:= SL.Strings[I];
          Len:= length(S);
          if Len > 1 then
          begin
            // verificamos se a linha termina em '|'
            if S[Len] = '|' then
            begin
              Surround:= true;
              // Verificamos se a linha só contém espaços e tabs
              for J := 1 to Len - 1 do
                if (S[J] <> #32) and (S[J] <> #9) then
                  Surround:= false;
              // se chegamos aqui todas as condições do surround foram atendidas
              if Surround then
                Break;
            end;
          end;
        end;

        if (Surround) and (I < SL.Count) then
        begin
          S:= SL.Strings[I];
          // remove o '|'
          setLength(S, length(S) - 1);
          SL.Delete(I);
          // S = string com a identação a ser usada no txt selecionado
          // I = local onde o txt selecionado deve ser inserido

          T:= fEditor.Lines.Strings[fEditor.BlockBegin.Line -1];
          len:= length(T);
          for J := 1 to len do
            if T[J] in [#32, #9] then
              SelIndent:= SelIndent + String(T[J]);

          // mesma indentação da seleção
          for J:= 0 to SL.Count -1 do
            SL.Strings[J]:= SelIndent + SL.Strings[J];

          for J := fEditor.BlockEnd.Line-1 downto  fEditor.BlockBegin.Line-1 do
            SL.Insert(I, S + fEditor.Lines.Strings[J]);

          // removemos todo o bloco selecionado
          fEditor.SelText:= '';
          SelectLine;
          fEditor.SelText:= '';
          // guardamos o resultado
          txtToInsert:= SL.Text;
        end;
      finally
        SL.Free;
      end;
    end;
  end;

  if not Surround then
  begin
    // Se o surround não foi feito, inserimos normalmente

    if fEditor.SelLength > 0 then
      fEditor.SelLength := 0;

    if fEditor.CaretX > 0 then
    begin
      SetLength(S,  fEditor.CaretX);
      for I := 1 to  fEditor.CaretX do
        S[I]:= #32;
    end;

    SL:= TStringList.Create;
    Try
      SL.LoadFromFile(FileName);

      if SL.Count > 1 then
        for I := 1 to SL.Count - 1 do
          SL.Strings[I]:= S + SL.Strings[I];

      txtToInsert:= SL.Text;
    finally
      SL.Free;
    end;
  end;

  if txtToInsert <> '' then
  begin
    // os '|' normais são notados como \| no NCL
    // guardamos eles como o caracter #7
    txtToInsert:= FastReplace(txtToInsert, '\|', #7, false);

    // guardamos para onde devemos levar o cursor
    I:= FastPos(txtToInsert, '|', length(txtToInsert), 1, 1);

    S:= Copy(txtToInsert, 1, I);
    S:= FastReplace(S, '|', '', false);
    S:= FastReplace(S, #7, '|', false);
    fEditor.SelText:= S;
    // guardamos o ponto em que deve ficar o cursor em J
    J:= fEditor.SelStart;

    S:= Copy(txtToInsert, I, length(txtToInsert) - I);
    S:= FastReplace(S, '|', '', false);
    S:= FastReplace(S, #7, '|', false);
    fEditor.SelText:= S;

    fEditor.SelStart:= J;
  end;

  if fEditor.CanFocus then
    fEditor.SetFocus;
end;

//-----------------------------------------------------------------

//Comenta um bloco de código.
procedure TNotesEditorTab.CommentBlock;
begin
  if FEditor.ReadOnly then Exit;

  if (fLineComment <> '') then
  begin
    FEditor.SelText:= fLineComment + fastReplace(FEditor.SelText,#13#10,#13#10+fLineComment,false);
  end else
  if (fMultilineCommentStart <> '') and (fMultilineCommentEnd <> '') then
  begin
    FEditor.SelText:= fMultilineCommentStart + ' '+ FEditor.SelText + ' '+ fMultilineCommentEnd;
  end;
End;

//Descomenta um bloco de código.
procedure TNotesEditorTab.UnCommentBlock;
var
//  I: integer;
  S: string;
begin
  //TODO melhorar algoritmo para que ele fique menos "destrutivo" :)
  if FEditor.ReadOnly then Exit;

  S:= fEditor.SelText;

  if (fMultilineCommentStart <> '') and (fMultilineCommentEnd <> '') then
  begin
    S:= FastReplace(S, fMultilineCommentStart+ ' ', '', False);
    S:= FastReplace(S, ' '+fMultilineCommentEnd, '', False);
    S:= FastReplace(S, fMultilineCommentStart, '', False);
    S:= FastReplace(S, fMultilineCommentEnd, '', False);
  end;

  if (fLineComment <> '') then
    S:= FastReplace(S, fLineComment, '', False);

  fEditor.SelText:= S;
end;

//-----------------------------------------------------------------


procedure TNotesEditorTab.GoToMatchingBracket;
Var
  P: TPoint;
begin
  P:= BufferCoordToPoint(self.GetMatchingBracket(FEditor.CaretXY));
  if (P.X > 0) and (P.Y > 0) then
    FEditor.CaretXY:= PointToBufferCoord(P);
end;

//-----------------------------------------------------------------

// Deleta a linha
procedure TNotesEditorTab.DeleteLine;
Var
  BE, SaveCaret: TBufferCoord;
begin
  if FEditor.ReadOnly then Exit;
  FEditor.BeginUpdate;
  SaveCaret:= FEditor.CaretXY;
  if SaveCaret.Line <> FEditor.Lines.Count then
  begin
    FEditor.CaretX:= 1;
    BE:= FEditor.BlockBegin;
    BE.Line:= BE.Line + 1;
    BE.Char:= 1;
    FEditor.BlockEnd:= BE;
    FEditor.SelText := '';
  end else
  begin
    if FEditor.Lines.Count > 1 then
    begin
      self.SelectLine;
      BE:= FEditor.BlockEnd;
      FEditor.CaretX:= 1;
      FEditor.SelStart:= FEditor.SelStart -1;
      FEditor.BlockEnd:= BE;
      FEditor.SelText:='';
      SaveCaret.Line:= SaveCaret.Line -1;
    end else
    begin
      FEditor.ExecuteCommand(ecDeleteEOL, #0, nil);
      SaveCaret.Char:= 1;
    end;
  end;
  FEditor.CaretXY:= SaveCaret;
  FEditor.EndUpdate;
  FEditor.Modified:= true;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.DuplicateLine
 * Descrição:    Duplica a linha em que o cursor se
 *               encontra.
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    None
 *)
procedure TNotesEditorTab.DuplicateLine;
Var
  P: TPoint;
begin
  FEditor.BeginUpdate;
  P:= BufferCoordToPoint(FEditor.CaretXY);
  if FEditor.Lines.Count >= P.Y then begin
    FEditor.ExecuteCommand(ecLineEnd, #0, nil);
    FEditor.ExecuteCommand(ecInsertLine, #0, nil);
    FEditor.Lines[P.Y]:= FEditor.LineText;
  end;
  FEditor.CaretXY:= BufferCoord(P.X, P.Y + 1);
  FEditor.EndUpdate;
end;


procedure TNotesEditorTab.SelectLine;
begin
  FEditor.BeginUpdate;
  FEditor.CaretX:= 1;
  FEditor.ExecuteCommand(ecSelLineEnd, #0, nil);
  FEditor.EndUpdate;
end;

procedure TNotesEditorTab.ReloadConfig;
begin
  nProfile.Config.SetEditorTabPropertys(self);

  with TNotesFileTypeOptionsLitle.Create(fFileType) do
  try
    setEditorTabOptions(self);
  finally
    Free;
  end;

  FEditor.Highlighter:= nil;
  FHighlighter.ReloadStyles;
  FEditor.Highlighter:= FHighlighter;
  FEditor.Invalidate;
end;


procedure TNotesEditorTab.setFileType(const Value: string);
var
  Filename: string;
begin
  if nProfile.FileTypesMgr.FileTypeExists(Value) then
  begin
    fFileType := Value;
    nProfile.Config.SetEditorTabPropertys(self);

    with TNotesFileTypeOptionsLitle.Create(fFileType) do
    try
      setEditorTabOptions(self);
    finally
      Free;
    end;

    FileName:= addSlash(nProfile.Paths.FileTypesDir + FileType) + FileType + NOTES_FILETYPE_HIGHLIGHTER_EXTENSION;
    if not FileExists( Filename ) then
      Exit;
    fEditor.Highlighter:= nil;
    if Assigned(FHighlighter) then
      FreeAndNil(FHighlighter);
    FHighlighter:= TNotesHighlighter.Create(nil);
    FHighlighter.LoadFromFile(FileName);
    FEditor.Highlighter:= FHighlighter;
    fEditor.Invalidate;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.LoadFromFile
 * Descrição:    Use para abrir um arquivo no editor.
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   const AFile: string
 * Resultado:    boolean
 *)
function TNotesEditorTab.LoadFromFile(const AFile: string): TNotesLoadFromFileResult;
const
  Msg = 'O arquivo "%s" parece ser um arquivo binário. '+
      'Editar ele no Notes ou em qualquer outro editor de textos pode danificar o arquivo. '+
      'Você tem certeza que quer fazer isto?';
var
  BT: TNotesBreakType;
  S: string;
begin
  Result := lfError;
  if not FileExists( AFile ) then
    Exit;
  S := PathGetLongName( AFile );
  BT:= GetBreakType( S );

  if ( BT = btBin ) and ( MessageBox(FEditor.Handle, PChar(Format(Msg, [AFile])), 'Aviso importante', MB_YESNO ) = IDNO ) then
  begin
    Result:= lfCancel;
    Exit;
  end;

  if BT in [btBin, btNone] then
    BreakType := btWin
  else
    BreakType := BT;

  FEditor.Lines.LoadFromFile( S );
  FFullPath := S;
  FFileName := ExtractFileName( S );
  Tab.Caption := FFileName;
  Result := lfOk;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SaveToFile
 * Descrição:    Salva o arquivo.
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   const AFile: string
 * Resultado:    None
 *)
procedure TNotesEditorTab.SaveToFile(const AFile: string);
var
  S: string;
begin
  S:= FEditor.Text;

  // Correção para o bug do Notes não estar salvando
  // a última linha quando ela esta vazia.
  if FEditor.Lines.Count > 1 then
  begin
    if FEditor.Lines.Strings[FEditor.Lines.Count-1] = '' then
      S:= S + ASCII_CRLF;
  end;

  Case FBreakType of
    btLin: S := FastReplace( S, ASCII_CRLF, ASCII_LF, True );
    btMac: S := FastReplace( S, ASCII_CRLF, ASCII_CR, True );
  end;
  StrToFile( AFile, S );
  FFullPath := AFile;
  FileName:= ExtractFileName( FFullPath );
  FTab.Caption:= fFileName;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetfileEolStyle
 * Descrição:    Pega o estilo de fim-de-linha do arquivo.
 * Autor:        Anderson R. Barbieri
 * Data:         30-nov-2003
 * Argumentos:   None
 * Resultado:    string
 *)
function TNotesEditorTab.GetFileBreakType: string;
begin
  case BreakType of
    btMac: Result := 'Mac';
    btLin: Result := 'Linux';
    btWin: Result := 'Windows';
  end;
end;


//=================================================================
//
//   GET/SET DAS PROPRIEDADES INTRODUZIDAS PELO COMPONENTE
//   LIGADAS A NOVOS COMPORTAMENTOS E OPÇÕES, ETC.
//
//=================================================================

function TNotesEditorTab.getHighlightLine: boolean;
begin
  Result:= fEditor.ActiveLineColor <> clNone;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.setBlockStartTokens
 * Descrição:    Seta os Tokens que delimitam o ínicio de um bloco de código
 * Autor:        Anderson R. Barbieri
 * Data:         29-apr-2004
 * Argumentos:   const Value: string
 * Resultado:    None
 *)
procedure TNotesEditorTab.setBlockStartTokens(const Value: string);
begin
  fBlockStart:= ExplodeStr(Value, #32);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.setBlockEndTokens
 * Descrição:    Seta os tokens que delimitam o fim de um bloco de código
 * Autor:        Anderson R. Barbieri
 * Data:         29-apr-2004
 * Argumentos:   const Value: string
 * Resultado:    None
 *)
procedure TNotesEditorTab.setBlockEndTokens(const Value: string);
begin
  fBlockEnd:= ExplodeStr(Value, #32);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetBlockEndTokens
 * Descrição:    Retorna os tokens que delimitam o fim de um bloco de código
 * Autor:        Anderson R. Barbieri
 * Data:         25-nov-2003
 * Argumentos:   None
 * Resultado:    string
 *)
function TNotesEditorTab.GetBlockEndTokens: string;
var
  I: integer;
begin
  for I:= 0 to High(fBlockEnd) do
    Result:= Result + #32 + fBlockEnd[I];
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetBlockStartTokens
 * Descrição:    Retorna os tokens que delimitam um início de bloco de código
 * Autor:        Anderson R. Barbieri
 * Data:         25-nov-2003
 * Argumentos:   None
 * Resultado:    string
 *)
function TNotesEditorTab.GetBlockStartTokens: string;
var
  I: integer;
begin
  for I:= 0 to High(fBlockStart) do
    Result:= Result + #32 + fBlockStart[I];
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetBlockIndent
 * Descrição:
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetBlockIndent(const Value: boolean);
begin
  fBlockIndent := Value;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetCloseHTMLTags
 * Descrição:
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetCloseHTMLTags(const Value: boolean);
begin
  fCloseHTMLTags := Value;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetHighlightLine
 * Descrição:
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetHighlightLine(const Value: boolean);
begin
  if Value then
  begin
    if fEditor.ActiveLineColor = clNone then
      fEditor.ActiveLineColor:= clMoneyGreen;
  end else
  begin
    // para desativar devemos setar a cor para clNone
    fEditor.ActiveLineColor:= clNone;
  end;
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetHLineColor
 * Descrição:
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: TColor
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetHLineColor(const Value: TColor);
begin
  fEditor.ActiveLineColor:= Value;
end;


(**
 * Procedimento: TNotesEditorTab.SetSymbolsColor
 * Descrição:
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: TColor
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetSymbolsColor(const Value: TColor);
begin
  fSymbolscolor := Value;
end;


//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetTabSize
 * Descrição:    Retorna o tamanho do caracter TAB em espaços
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    integer
 *)
function TNotesEditorTab.GetTabSize: integer;
begin
  Result:= 8; // valor padrão
  if Assigned( FEditor ) then
    Result:= FEditor.TabWidth;
end;


//=================================================================
//
//   O CÓDIGO ABAIXO SERVE PARA LER/ESCREVER NAS
//   OPÇÕES DO SYNEDIT DO TIPO TSynEditorOption
//
//=================================================================

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetSynEditorOption
 * Descrição:    Seta uma opção do tipo TSynEditorOption do Editor
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   AOption: TSynEditorOption; Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetEditorOption( Option: TSynEditorOption;
  Enable: Boolean );
begin
  if FEditor = nil then
    Exit;
  if Enable then
    FEditor.Options := FEditor.Options + [Option]
  else
    FEditor.Options := FEditor.Options - [Option];
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetCursorPastEol
 * Descrição:    Seta a opção eoScrollPastEol do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetCursorPastEol(const Value: boolean);
begin
  SetEditorOption(eoScrollPastEol, Value);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetCursorPastEof
 * Descrição:    Seta a opção eoScrollPastEof do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetCursorPastEof(const Value: boolean);
begin
  SetEditorOption(eoScrollPastEof, Value);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetTabAsSpace
 * Descrição:    Seta a opção eoTabsToSpaces do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetTabAsSpace(const Value: boolean);
begin
  SetEditorOption(eoTabsToSpaces, Value);
end;

//-----------------------------------------------------------------
(**
 * Procedimento: TNotesEditorTab.SetTabIndent
 * Descrição:    Seta eoTabIndent no SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetTabIndent(const Value: boolean);
begin
  SetEditorOption(eoTabIndent, Value);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetAutoIndent
 * Descrição:    Seta a opção eoAutoIndent do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         22-nov-2003
 * Argumentos:   const Value: boolean
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetAutoIndent(const Value: boolean);
begin
  SetEditorOption(eoAutoIndent, Value);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetSynEditorOptionValue
 * Descrição:    Pega opção do tipo TSynEditorOption do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   AOption: TSynEditorOption
 * Resultado:    boolean
 *)
function TNotesEditorTab.IsEditorOptionSetted( Option: TSynEditorOption): Boolean;
begin
  Result := Assigned( FEditor ) and ( Option in FEditor.Options );
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetAutoIndent
 * Descrição:    Retorna o valor da opção eoAutoIndent do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    boolean
 *)
function TNotesEditorTab.GetAutoIndent: boolean;
begin
  Result := IsEditorOptionSetted(eoAutoIndent);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetCursorPastEof
 * Descrição:    Pega a opção eoScrollPastEof do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    boolean
 *)
function TNotesEditorTab.GetCursorPastEof: boolean;
begin
  Result:= IsEditorOptionSetted(eoScrollPastEof);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetCursorPastEol
 * Descrição:    Pega a opção eoScrollPastEol do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    boolean
 *)
function TNotesEditorTab.GetCursorPastEol: boolean;
begin
  Result:= IsEditorOptionSetted(eoScrollPastEol);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetTabAsSpace
 * Descrição:    Pega a opção eoTabsToSpaces do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    boolean
 *)
function TNotesEditorTab.GetTabAsSpace: boolean;
begin
  Result:= IsEditorOptionSetted(eoTabsToSpaces);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetTabIndent
 * Descrição:    Pega a opção eoTabIndent do SynEdit
 * Autor:        Anderson R. Barbieri
 * Data:         23-nov-2003
 * Argumentos:   None
 * Resultado:    boolean
 *)
function TNotesEditorTab.GetTabIndent: boolean;
begin
  Result:= IsEditorOptionSetted(eoTabIndent);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.SetTabSize
 * Descrição:    Seta o tamanho do TAB em espaços
 * Autor:        Anderson R. Barbieri
 * Data:         26-nov-2003
 * Argumentos:   const Value: integer
 * Resultado:    None
 *)
procedure TNotesEditorTab.SetTabSize(const Value: integer);
begin
  FEditor.TabWidth:= Value;
end;


//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.InsertSpaces
 * Descrição:    O SynEdit tem uma forma meio "diferente" de
 *               inserir espaços. Use esta rotina para inserir
 *               espaços ao invés de algo como SelText:='   ';
 * Autor:        Anderson R. Barbieri
 * Data:         26-nov-2003
 * Argumentos:   const NumberOfSpaces: integer
 * Resultado:    None
 *)
procedure TNotesEditorTab.InsertSpaces(const NumberOfSpaces: integer);
var
  I: integer;
begin
  if NumberOfSpaces < 1 then Exit;
  for I:= 0 to NumberOfSpaces do
    FEditor.ExecuteCommand(ecChar, #32,nil);
end;

//-----------------------------------------------------------------

(**
 * Procedimento: TNotesEditorTab.GetCloseHTMLStr
 * Descrição:    Retorna uma string com a tag HTML sob o cursor fechada
 * Autor:        Anderson R. Barbieri
 * Data:         26-nov-2003
 * Argumentos:   None
 * Resultado:    string
 *)
function TNotesEditorTab.GetCloseHTMLStr: string;
var
  I, TagBegin, TagEnd: integer;
  Len: integer;
  S: string;
begin
  S:= FEditor.Text;
  Len:= length(S);

  if Len = 0 then begin
    Result:='>';
    Exit;
  end;

  TagEnd:= FEditor.SelStart;
  tagBegin:= TagEnd;
  I:= TagEnd;

  // verifica se não é uma tag XHTML
  // sendo fechada, ou seja, '\>'
  if (I < 2) or (S[I-1] = '\') then begin
    Result:='>';
    exit;
  end;

  while (S[tagBegin] <> '<') and (tagBegin > 0) do begin
  {if S[tagBegin] in ['>', '/', '\'] then begin
      // no caso de encontrarmos >, provavelmente não se trata de
      // HTML, então não fechamos a tag. Se encontramos / ou \,
      // então deve ser uma tag HTML que está sendo fechada.
      Result:='>';
      exit;
    end;      }
    Dec(tagBegin);
  end;

  // verifica se a tag não está sendo fechada, ou seja, '</'
  if (tagBegin+1 < Len) and (S[tagBegin+1] = '/') then begin
    Result:='>';
    exit;
  end;

  tagEnd:= tagBegin+1;

  while ((S[tagEnd] in [#1..#32]) = False) and (tagEnd < (I+1)) do
    Inc(tagEnd);

  { Verificamos se o '>' não está dentro de código JavaScript,
    CSS, PHP, ASP ou JSP }
  if (IsInTag(S, '<script' , '</script>', I, 7, 9) = false) and
     (IsInTag(S, '<style'  , '</style>' , I, 6, 8) = false) and
     (IsInTag(S, '<?'      , '?>'       , I, 2, 2) = false) and
     (IsInTag(S, '<%'      , '%>'       , I, 2, 2) = false) and
     (IsInTag(S, '<#'      , '#>'       , I, 2, 2) = false) then
  begin
    S:= trim(Copy(S, tagBegin+1, tagEnd - tagBegin -1));
    if not IsInStrArray( S, ExcludeTags ) then
      Result:= '></' + S + '>'
    else
      Result:= '>';
  end
  else
    Result:= '>';
end;

procedure TNotesEditorTab.GoToMark(MarkNumber: integer);
begin
  if MarkNumber < fMarks.Count then
    GoToLine( fMarks[Marknumber] );
end;

procedure TNotesEditorTab.NextMark;
Var
  I: integer;
begin
  I:= fMarks.getNextMark(fEditor.CaretY);

  if fEditor.CaretY <> I then
  begin
    gotoLine(I);
  end
  else
  begin
    fMarks.Sort;
    gotoMark(0);
  end;
end;

procedure TNotesEditorTab.PreviousMark;
Var
  I: integer;
begin
  I:= fMarks.getPreviousMark(fEditor.CaretY);

  if fEditor.CaretY <> I then
  begin
    gotoLine(I);
  end else
  begin
    if fMarks.Count > 0 then
    begin
      fMarks.Sort;
      gotoMark(fMarks.Count-1);
    end;
  end;
end;

procedure TNotesEditorTab.ToogleMark;
Var
  I: integer;
begin
  I:= fMarks.IndexOf(fEditor.CaretY);
  if I > -1 then
    fMarks.Remove(fEditor.CaretY)
  else
    fMarks.Add(fEditor.CaretY);
  fEditor.InvalidateGutter;
end;

function TNotesEditorTab.getLineY(const Line: integer): integer;
Var
  D: TDisplayCoord;
begin
  D.Row:= FEditor.LineToRow(Line);
  Result:= FEditor.RowColumnToPixels(D).Y;
end;

procedure TNotesEditorTab.doPaint(Sender: TObject; ACanvas: TCanvas);
Var
  I, LastLine, FirstLine, LH: integer;
begin
  // se a gutter for mt pequena ou estiver invsível, obviamente
  // não temos como mostrar nada!!!
  if fEditor.Gutter.Width < 30 then Exit;

  FirstLine:= fEditor.TopLine;
  LastLine:= fEditor.LinesInWindow + FirstLine;
  LH:= fEditor.LineHeight;

  FEditor.Canvas.Pen.Color := fEscopeIndicatorColor;

  // *** INDICADOR DE ESCOPO ******
  case fEscopeIndicator of
    eiGutter:
    begin
      if fEditor.Gutter.Visible then
      begin
        FEditor.Canvas.Pen.Width := 2;
        I := (fEscopePos.EscopeBegin.Y - FirstLine) * LH;
        Self.FEditor.Canvas.MoveTo(10, I);
        Self.FEditor.Canvas.LineTo(5, I);
        Self.FEditor.Canvas.MoveTo(5, I);
        Self.FEditor.Canvas.LineTo(5, I + ((fEscopePos.EscopeEnd.Y + 1) - fEscopePos.EscopeBegin.Y) * LH);
        Self.FEditor.Canvas.MoveTo(10, I + ((fEscopePos.EscopeEnd.Y + 1) - fEscopePos.EscopeBegin.Y) * LH);
        Self.FEditor.Canvas.LineTo(5, I + ((fEscopePos.EscopeEnd.Y  + 1) - fEscopePos.EscopeBegin.Y) * LH);
      end;
    end;

    eiEditor:
    begin
      FEditor.Canvas.Pen.Width := 1;
      I := (fEscopePos.EscopeBegin.Y - FirstLine) * LH;
      Self.FEditor.Canvas.MoveTo(fEscopePos.EscopeStart + fEscopePos.EscopeBegin.X, I);
      Self.FEditor.Canvas.LineTo(fEscopePos.EscopeStart, I);
      Self.FEditor.Canvas.MoveTo(fEscopePos.EscopeStart, I);
      Self.FEditor.Canvas.LineTo(fEscopePos.EscopeStart,
          I + ((fEscopePos.EscopeEnd.Y + 1) - fEscopePos.EscopeBegin.Y) * LH);
      Self.FEditor.Canvas.MoveTo(fEscopePos.EscopeStart + fEscopePos.EscopeEnd.X,
          I + ((fEscopePos.EscopeEnd.Y  + 1) - fEscopePos.EscopeBegin.Y) * LH);
      Self.FEditor.Canvas.LineTo(fEscopePos.EscopeStart,
          I + ((fEscopePos.EscopeEnd.Y  + 1) - fEscopePos.EscopeBegin.Y) * LH);
    end;
  end;
  // ******

  for I := 0 to fMarks.Count-1 do
  begin
    if (fMarks[I] >= FirstLine) and (fMarks[I] <= LastLine) then
      ACanvas.Draw(2, getLineY(fMarks[I]) + 2, fMarkIcon);
  end;

  for I := 0 to ProblemsList.Count-1 do
  begin
    if (ProblemsList[I].Line >= FirstLine) and (ProblemsList[I].Line <= LastLine)
      and (SameText(self.FullPath, ProblemsList[I].FileName)) then
      ACanvas.Draw(fEditor.Gutter.Width -6, getLineY(ProblemsList[I].Line) + 1 , fProblemIcon);
  end;

end;

procedure TNotesEditorTab.doKeyUP(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CalcEscopeIndicator;
  case fEscopeIndicator of
    eiGutter: FEditor.InvalidateGutter;
    eiEditor: FEditor.Invalidate;
  end;
end;

//function getWordAtPos(const S: string; const Position: integer): string;
//const
//  WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','.', ','];
//var
//  I, Len, Start: integer;
//begin
//  Result:='';
//  Len:= length(S);
//  if (Len < 1) or (Position < 1) then Exit;
//
//  for I:= Position downto 1 do
//    if S[I] in WhiteSpaces then Break;
//  wStart:= I+1;
//
//  for I:= Position to Len do
//    if S[I] in WhiteSpaces then Break;
//
//  Result:= Copy(S, Start, I - Start);
//end;
//

procedure TNotesEditorTab.CalcEscopeIndicator;

  function ExistValueIn(const A: array of String; const S: String;
    const Y: Integer; var X: Integer): Boolean;
  const
    WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','.', ','];
  var
     i, j, Len, Position: Integer;
     CurPos: TPoint;
     sAux: string;
  begin
    Result := False;
    Len:= length(S);
    CurPos.Y := Y + 1;
    if Len = 0 then Exit;

    for i := 1 to Len do
    begin
      if S[i] <> ' ' then
      begin
        if fEscopePos.EscopeStart > i then
          fEscopePos.EscopeStart := i;
        Break;
      end;
    end;

    for i := Low(A) to High(A) do
    begin
      Position := Pos(AnsiUpperCase(A[i]), AnsiUpperCase(S));
      CurPos.X := Position;
      X := Position;

      if Position > 0 then
        if Length(A[i]) > 1 then
        begin
          if (Position -1 = 0) or (S[Position-1] in WhiteSpaces) then
          begin

            for j:= Position to Len do
              if S[j] in WhiteSpaces then Break;

            sAux := Copy(S, Position, j - Position);

            if IsInStrArray(sAux, A) then
            begin
              Result := True;
              Break;
            end;
          end;

        end else
        begin
          Result := True;
          Break;
        end;
    end;
  end;

var
  // Números das linhas de Inicio e de
  // fim de Blocos
  Inicio, Fim: Integer;

  // Números das Colunas dos Blocos de Inicio e
  // de Fim, usado para desenhar até eles
  StartX, EndX: Integer;

  // Usado para contar os inicializadores e os
  // finalizadores para garantir que sejam fechados
  // com seus respectivos blocos
  IncInicio, IncFim: Integer;

  // String usada como Auxilio para Contar Tokens
  sAux: string;
begin
  if (fEscopeIndicator <> eiNone) and (fEditor.SelAvail = false) then
  begin
    IncInicio := 0;
    IncFim := 0;

    StartX := 0;
    EndX := 0;

    // São diminuídos 1 do valor correto para funcionarem
    // corretamente no while, o valor é incrementado após a rotina
    Inicio := Self.FEditor.CaretY - 1;
    Fim := Self.FEditor.CaretY;

    // Vai varrendo o documento para cima até
    // que encontre um inicialidor de bloco
    // ou que não seja o último inicializador
    while not (ExistValueIn(FEscopeStart, Self.FEditor.Lines.Strings[Inicio], Inicio, StartX))
          or (IncInicio > 0) do
    begin
      // Se já estiver no começo, interrompe a rotina
      if Inicio = 0 then Exit;

      // Decrementa a quantidade de inicializadores
      // quando encontra um inicializador
      sAux := Self.FEditor.Lines.Strings[Inicio];
      while ExistValueIn(FEscopeStart, sAux, Inicio, StartX) do
      begin
        if (Length(sAux) - StartX) = Length(sAux) then
          sAux := Copy(sAux, StartX, Length(sAux))
        else
          sAux := EmptyStr;
        Dec(IncInicio);
      end;

      // Incrementa a quantidade de inicializadores
      // se encontrar um finalizador
      sAux := Self.FEditor.Lines.Strings[Inicio];
      while ExistValueIn(FEscopeEnd, sAux, Inicio, StartX) do
      begin
        if (Length(sAux) - StartX) = Length(sAux) then
          sAux := Copy(sAux, StartX, Length(sAux))
        else
          sAux := EmptyStr;
        Inc(IncInicio);
      end;

      Dec(Inicio);
    end;

    // Vai varrendo o documento para baixo até
    // que encontre um finalizador de bloco
    while not (ExistValueIn(FEscopeEnd, Self.FEditor.Lines.Strings[Fim], Fim, EndX))
          or (IncFim > 0) do
    begin
      // Se estiver no fim do documento interrompe a rotina
      if Fim = Self.FEditor.Lines.Count then Exit;

      // Decrementa a quantidade de finalizadores
      // quando encontra um finalizador
      (* if ExistValueIn(FEscopeEnd, Self.FEditor.Lines.Strings[Fim], Fim, EndX) then
        Dec(IncFim); *)
      sAux := Self.FEditor.Lines.Strings[Fim];
      while ExistValueIn(FEscopeEnd, sAux, Fim, EndX) do
      begin
        if (Length(sAux) - EndX) = Length(sAux) then
          sAux := Copy(sAux, EndX, Length(sAux))
        else
          sAux := EmptyStr;
        Dec(IncFim);
      end;

      // Incrementa a quantidade de finalizadores
      // se encontrar um inicializador
      (* if ExistValueIn(FEscopeStart, Self.FEditor.Lines.Strings[Fim + 1], Fim + 1, EndX) then
        Inc(IncFim); *)
      sAux := Self.FEditor.Lines.Strings[Fim];
      while ExistValueIn(FEscopeStart, sAux, Fim, EndX) do
      begin
        if (Length(sAux) - EndX) = Length(sAux) then
          sAux := Copy(sAux, EndX, Length(sAux))
        else
          sAux := EmptyStr;
        Inc(IncFim);
      end;

      Inc(Fim);
    end;

    // Incrementa o valor removido para a leitura do
    // comando de repetição
    Inc(Inicio);
    Inc(Fim);

    fEscopePos.EscopeBegin.Y := Inicio;
    fEscopePos.EscopeBegin.X := ((StartX - fEscopePos.EscopeStart) + 1) * FEditor.CharWidth;

    fEscopePos.EscopeEnd.Y := Fim;
    fEscopePos.EscopeEnd.X := ((EndX - fEscopePos.EscopeStart) + 1) * FEditor.CharWidth;

    fEscopePos.EscopeStart := ((fEscopePos.EscopeStart - 1) * FEditor.CharWidth)
        + FEditor.Gutter.RealGutterWidth(FEditor.CharWidth);
  end;
end;

function TNotesEditorTab.getEscopeEndTokens: string;
begin
  result:= ImplodeStr(fEscopeEnd, #32);
end;

function TNotesEditorTab.getEscopeStartTokens: string;
begin
  result:= ImplodeStr(fEscopeStart, #32);
end;

procedure TNotesEditorTab.setEscopeEndTokens(const Value: string);
begin
  setLength(fEscopeEnd, 0);
  fEscopeEnd:= ExplodeStr(Value, #32);
end;

procedure TNotesEditorTab.setEscopeStartTokens(const Value: string);
begin
  setLength(fEscopeStart, 0);
  fEscopeStart:= ExplodeStr(Value, #32);
end;



{ TNotesMarksList }

function TNotesMarksList.Add(atLine: integer): Integer;
begin
  setLength(fItems, fCount +1);
  fItems[fCount]:= atLine;
  Result:= fCount;
  inc(fCount);
  fSorted:= false;
end;

procedure TNotesMarksList.AdjustToLineAdded(Line: integer);
Var
  I: integer;
begin
  for I := 0 to fCount-1 do
  begin
    if fItems[I] > Line then
      fItems[I]:= fItems[I] +1;
  end;
end;

procedure TNotesMarksList.AdjustToLineRemoved(Line: integer);
Var
  I: integer;
begin
  if fCount < 1 then Exit;
  Remove(Line);
  for I := 0 to fCount-1  do
  begin
    if fItems[I] > Line then
      fItems[I] := fItems[I]  -1;
  end;
end;

procedure TNotesMarksList.Clear;
begin
  fCount:= 0;
  SetLength(fItems, fCount);
  fSorted:= false;
end;

destructor TNotesMarksList.destroy;
begin
  setLength(fItems, 0);
end;

function TNotesMarksList.get(index: integer): integer;
begin
  result:= fItems[index];
end;

function TNotesMarksList.getNextMark(Line: integer): integer;
Var
  I: integer;
begin
  if fCount < 1 then
  begin
    Result:= Line;
    Exit;
  end;
  // colocamos o maior número possível
  Result:= High(integer);
  for I:= 0 to fCount-1 do
  begin
    if (fItems[I] > Line) and (fItems[I] < Result) then
      Result:= fItems[I];
  end;
  // se nenhum bookmaker para uma linha maior for encontrado
  // retorna a própria linha
  if Result = High(integer) then
    Result:= Line;
end;

function TNotesMarksList.getPreviousMark(Line: integer): integer;
Var
  I: integer;
begin
  if fCount < 1 then
  begin
    Result:= Line;
    Exit;
  end;
  Result:= -1;
  for I:= 0 to fCount-1 do
  begin
    if (fItems[I] < Line) and (fItems[I] > Result) then
      Result:= fItems[I];
  end;
  if Result = -1 then
    Result:= Line;
end;

function TNotesMarksList.IndexOf(Value: integer): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I:= 0 to fCount-1 do
  begin
    if fItems[I] = Value then
    begin
      Result:= I;
      Exit;
    end;
  end;
end;

procedure TNotesMarksList.put(index: integer; value: integer);
begin
  fSorted:= false;
  fItems[index]:= Value;
end;

procedure TNotesMarksList.QuickSort(lowerPos, upperPos: integer);
var
  temp, i, middlePos, pivotValue : integer;
Begin
  if lowerPos < upperPos then
  begin

    pivotValue := fItems[lowerPos];
    middlePos := lowerPos;

    for i := lowerPos+1 to upperPos do
    begin
      if fItems[i] < pivotValue then
      begin
        inc(middlePos);
        temp := fItems[middlePos];
        fItems[middlePos] := fItems[i];
        fItems[i] := temp;
      end;
    end;
    temp := fItems[lowerPos];
    fItems[lowerPos] := fItems[middlePos];
    fItems[middlePos] := temp;

    QuickSort(lowerPos, middlePos-1);
    QuickSort(middlePos+1, upperPos);
  end;
end;

function TNotesMarksList.Remove(atLine: integer): Integer;
Var
  I: integer;
  SaveLast: integer;
begin
  I:= indexOf(atLine);
  Result:= I;
  if Result < 0 then Exit;
  if fCount > 0 then
  begin
    SaveLast:= fItems[fCount-1];
    if I > -1 then
      fItems[fCount-1]:= fItems[I];
    fItems[I]:= SaveLast;
    Dec(fCount);
    setLength(fItems, fCount);
  end;
end;

procedure TNotesMarksList.Sort;
begin
  if (fSorted = false) and (fCount > 1) then
    QuickSort(0, fCount-1);
  fSorted:= true;
end;


{ Usamos a inicialização/finalização para carregar apenas uma vez os
 ícones dos marcadores/problemas na memória e destruí-los só ao final.}


{ TNotesSynPlugin }

procedure TNotesSynPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
  FirstLine, LastLine: integer);
begin
  inherited;
  //
end;

procedure TNotesSynPlugin.LinesDeleted(FirstLine, Count: integer);
Var
  I: integer;
begin
  inherited;
  for I:= 1 to Count do
  begin
    fEdTab.Marks.AdjustToLineRemoved(FirstLine);
    ProblemsList.AdjustToLineRemoved(FirstLine, fEdTab.FullPath);
  end;
  Editor.InvalidateGutter;
end;

procedure TNotesSynPlugin.LinesInserted(FirstLine, Count: integer);
Var
  I: integer;
begin
  inherited;
  for I:= 1 to Count do
  begin
    fEdTab.Marks.AdjustToLineAdded(FirstLine-1);
    ProblemsList.AdjustToLineAdded(FirstLine-1, fEdTab.FullPath);
  end;
  Editor.InvalidateGutter;
end;

initialization
  MarkIco:= loadImage(hInstance, 'MARK', IMAGE_ICON , 16, 16, LR_DEFAULTCOLOR	);
  fMarkIcon:= TIcon.Create;
  fMarkIcon.Handle:= MarkIco;
  ProblemIco:= loadImage(hInstance, 'PROBLEM', IMAGE_ICON , 16, 16, LR_DEFAULTCOLOR	);
  fProblemIcon:= TIcon.Create;
  fProblemIcon.Handle:= ProblemIco;
  ProblemsList:= TNotesEditorTabPosList.Create;
  ProblemsList.GoToLineAsError:= true;
  EditLocationsList:= TNotesEditorTabPosList.Create;
  EditLocationsList.GoToLineAsError:= false;
  EditLocationsList.MaxItems:= 70;
  tip:= TNotesToolTip.create;

finalization
  FreeAndNil(ProblemsList);
  FreeAndNil(EditLocationsList);
  fMarkIcon.ReleaseHandle;
  FreeAndNil(fMarkIcon);
  DestroyIcon(MarkIco);
  fProblemIcon.ReleaseHandle;
  FreeAndNil(fProblemIcon);
  DestroyIcon(ProblemIco);
  tip.Free;

end.
