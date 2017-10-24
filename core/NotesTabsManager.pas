//
//    NotestabsManager - classe para gerenciamento das tabs/documentos
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
  @abstract(NotestabsManager - classe para gerenciamento das tabs/documentos.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesTabsManager;

interface

uses Windows, SysUtils, Classes, Controls, ComCtrls, SynEdit,
  SynEditKeyCmds, NotesEditorTab, NotesEditorTabPosList,
  Menus, NotesStartPage, NotesProfile;

type
  { Matriz de Editores (TNotesEditorTab)}
  TNotesEditorTabs = array of TNotesEditorTab;

type
  TNotesTabsManager = class(TObject)
  private
    fEditors: TNotesEditorTabs;
    fEditorsCount: integer;
    fPageCtrl: TPageControl;
    FDropFiles: TDropFilesEvent;
    FStatusChange: TStatusChangeEvent;
    fpopMenu: TPopupMenu;
    fStartPage: TNotesStartPage;
    fOpenMru: TNotesOpenMruItem;
    fOnOpenProject: TNotifyEvent;
    fOnNewProject: TNotifyEvent;
    fOnNewFile: TNotifyEvent;
    fOnOpenFile: TNotifyEvent;

    // salva o estado do editor
    procedure SaveEditorState(const EditorTab: TNotesEditorTab);

    { Handler do evento DragDrop no editor  }
    procedure EditorOnDragDropHandler(Sender, Source: TObject; X, Y: Integer);
    { Handler do evento DragOver no editor  }
    procedure EditorOnDragOverHandler(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    function getEditorTab(Index: Integer): TNotesEditorTab;
    procedure setEditorTab(Index: Integer; const Value: TNotesEditorTab);
  public
    constructor Create;
    destructor Destroy; override;

    // relacionado a arquivos
    { Carrega um arquivo em um novo editor }
    function LoadFile(const FileName: string): boolean;
    { Carrega múltiplos arquivos }
    procedure LoadMultipleFiles(const Files: TStrings);

    // Gerenciamento da StartPage
    // Mostra a StartPage (cria ela se necessário)
    procedure ShowStartPage;
    // retorna se existe uma StartPage criada
    function HasStartPage: boolean;
    // atualiza a StartPage
    procedure ReloadStartPage;

    // Põe o foco no editor ativo, se possível
    procedure FocusActiveEditor;
    // Gerenciamento dos editores
    { Cria uma novo objeto da classe TNotesEditorTab criando assim uma
      nova tab, um novo synedit e um novo unihighlighter. Retorna a posição
      do novo Editor no array que contém todos os Editores ja criados, o fEditors.}
    function  CreateNewEditor: Integer;
    { Fecha o editor/startpage atual SEM PERGUNTAR AO USUÁRIO SE ELE
      QUER SALVAR.}
    procedure CloseActiveTab;
    { Fecha todos os editores abertos SEM PERGUNTAR AO USUÁRIO SE ELE
      QUER SALVAR. Tarefas como mostrar um diálogo e depois chamar a
      ação de salvar são de responsabilidade do form principal. }
    procedure CloseAllEditors;
    { Retorna o TNotesEditorTab ativo }
    function getActiveEditorTab: TNotesEditorTab;
    { Retorna true caso haja algum editor ativo. }
    function IsEditorActivated: Boolean;
    { Retorna true se a StartPage estiver ativa. }
    function IsStartPageActivated: boolean;
    { Retorna o índice do editor passado como parâmetro ou -1 caso o editor naum exista. }
    function GetEditorIndex( Editor: TNotesEditorTab): Integer;
    { Retorna o TNotesEditorTab para a o índece da tab passada }
    function getEditorByTabIndex(const index: integer): TNotesEditorTab;

    // Utilidades
    { Pega o tipo de arquivo para a extensão passada em @code(Ext).}
    function GetFileTypeByExtension( Ext: string ): string;
    { Ir para a linha }
    procedure GoToLine(LineNo : Integer); overload;
    { Ir para a linha }
    procedure GoToLine(LineNo : String); overload;
    { Ir para a coluna }
    procedure GoToCol(ColNo: integer);
    { Insere um arquivo no formato NCL }
    procedure InsertNCL(NCLFile: string );

    // Ações do editor
    { Converte tabs para espaços }
    procedure EditorTabsToSpaces(Sender: TObject);
    { Converte espaços para tabs }
    procedure EditorSpacesToTabs(Sender: TObject);
    { Copiar }
    procedure EditorCopy(Sender: TObject);
    { Recortar }
    procedure EditorCut(Sender: TObject);
    { Colar }
    procedure EditorPaste(Sender: TObject);
    { Copiar adicionando o texto selecionado ao texto da clipboard }
    procedure EditorCopyAppend(Sender: TObject);
    { Cola mantendo a indentação }
    procedure EditorSmartPaste(Sender: TObject);
    { Desfazer }
    procedure EditorUndo(Sender: TObject);
    { Refazer }
    procedure EditorRedo(Sender: TObject);
    { Deletar }
    procedure EditorDelete(Sender: TObject);
    { Selecionar tudo }
    procedure EditorSelAll(Sender: TObject);
    { Selecionar a linha atual }
    procedure EditorSelLine(Sender: TObject);
    { Deletar a linha atual }
    procedure EditorDelLine(Sender: TObject);
    { Duplicar a linha atual }
    procedure EditorDuplicateLine(Sender: TObject);
    { Reindenta um bloco ou linha de código }
    procedure EditorReindent(Sender: TObject);
    { Comentar }
    procedure EditorComment(Sender: TObject);
    { Descomentar }
    procedure EditorUcomment(Sender: TObject);
    { Identar }
    procedure EditorIndent(Sender: TObject);
    { Desindentar }
    procedure EditorUnindent(Sender: TObject);
    { Selecionar palavra que está sob o cursor }
    procedure EditorSelWordAtCursor(Sender: TObject);
    { Adiciona/Remove marcador }
    procedure EditorTogleMark(Sender: TObject);
    { Vai para o próximo marcador }
    procedure EditorNextMark(Sender: TObject);
    { Vai para o marcador anterior }
    procedure EditorPreviusMark(Sender: TObject);
    { Limpa todas as marcas }
    procedure EditorClearMarks(Sender: TObject);
    { Vai para a esquerda }
    procedure EditorLeft(Sender: TObject);
    { Vai para a direita }
    procedure EditorRight(Sender: TObject);
    { Vai para cima }
    procedure EditorUp(Sender: TObject);
    { Vai para baixo }
    procedure EditorDown(Sender: TObject);
    { Vai para a primeira palavra à esquerda }
    procedure EditorWordLeft(Sender: TObject);
    { Vai para a primeira palavra a direita }
    procedure EditorWordRight(Sender: TObject);
    { Vai para o início da linha }
    procedure EditorLineStart(Sender: TObject);
    { Vai para o fim da linha }
    procedure EditorLineEnd(Sender: TObject);
   { Move o cursor uma página para cima }
    procedure EditorPageUp(Sender: TObject);
   { Move o cursor uma página para baixo }
    procedure EditorPageDown(Sender: TObject);
    { Move o cursor uma página para a esquerda }
    procedure EditorPageLeft(Sender: TObject);
   { Move o cursor uma página para a direita }
    procedure EditorPageRight(Sender: TObject);
   { Move o cursor para o topo da página }
    procedure EditorPageTop(Sender: TObject);
   { Move o cursor para o final da página }
    procedure EditorPageBottom(Sender: TObject);
   { Move o cursor para o topo do editor }
    procedure EditorEditorTop(Sender: TObject);
   { Move o cursor para a última linha visível do editor }
    procedure EditorEditorBottom(Sender: TObject);
    { As ações abaixo são iguais as anteriores, porém modificam a seleção }
    procedure EditorSelLeft(Sender: TObject);
    procedure EditorSelRight(Sender: TObject);
    procedure EditorSelUp(Sender: TObject);
    procedure EditorSelDown(Sender: TObject);
    procedure EditorSelWordLeft(Sender: TObject);
    procedure EditorSelWordRight(Sender: TObject);
    procedure EditorSelLineStart(Sender: TObject);
    procedure EditorSelLineEnd(Sender: TObject);
    procedure EditorSelPageUp(Sender: TObject);
    procedure EditorSelPageDown(Sender: TObject);
    procedure EditorSelPageLeft(Sender: TObject);
    procedure EditorSelPageRight(Sender: TObject);
    procedure EditorSelPageTop(Sender: TObject);
    procedure EditorSelPageBottom(Sender: TObject);
    procedure EditorSelEditorTop(Sender: TObject);
    procedure EditorSelEditorBottom(Sender: TObject);
    { Move para cima sem mover o cursor }
    procedure EditorScrollUp(Sender: TObject);
    { Move para baixo sem mover o cursor }
    procedure EditorScrollDown(Sender: TObject);
    { Move para a esquerda sem mover o cursor }
    procedure EditorScrollLeft(Sender: TObject);
    { Move para a direita sem mover o cursor }
    procedure EditorScrollRight(Sender: TObject);
    { Muda entre inserir/sobrescrever }
    procedure EditorToggleMode(Sender: TObject);
    { Vai para o braço correspondente }
    procedure EditorGoToMatchBracket(Sender: TObject);
    { Deleta o caracter anterior (para deletar o caracter atual, use EditorDelete) }
    procedure EditorDeleteLastChar(Sender: TObject);
    { Deleta a palavra atual }
    procedure EditorDeleteWord(Sender: TObject);
    { Deleta a palavra anterior }
    procedure EditorDeleteLastWord(Sender: TObject);
    { Deleta tudo até o início da linha }
    procedure EditorDeleteBOL(Sender: TObject);
    { Deleta tudo até o fim da linha }
    procedure EditorDeleteEOL(Sender: TObject);
    { Deleta tudo }
    procedure EditorClearAll(Sender: TObject);
    { Quebra a linha na posição atual }
    procedure EditorBreakLine(Sender: TObject);
    { Quebra a linha no final da linha atual }
    procedure EditorBreakLineFromEOL(Sender: TObject);
    { Quebra a linha sem mover o cursor }
    procedure EditorBreakLineNoMove(Sender: TObject);
    { Tecla Tab }
    procedure EditorTab(Sender: TObject);
    { Shift+Tab }
    procedure EditorShiftTab(Sender: TObject);
    { Deixa todos os caracteres em maiúsculas }
    procedure EditorUpperCase(Sender: TObject);
    { Deixa todos os caracteres em minúsculas }
    procedure EditorLowerCase(Sender: TObject);
    { Deixa todos os caracteres em case invertido }
    procedure EditorToggleCase(Sender: TObject);
    { Deixa todos os caracteres em formato de título }
    procedure EditorTitleCase(Sender: TObject);
    // indique o pagecontrol a ser usado para mostrar as tabs
    property PageControl: TPageControl read fPageCtrl write fPageCtrl;
    // Permite manipular os editores
    property EditorsTab[Index: Integer]: TNotesEditorTab read getEditorTab write setEditorTab;
    // Contém o número de EditorsTab
    property EditorsTabCount: integer read fEditorsCount;
    // Passe o popup menu a ser usado pelos editores
    property EditorPopupMenu: TPopupMenu read fpopMenu write fpopMenu;
    // Evento chamado quando o status do editor mudou
    property OnEditorStatusChange: TStatusChangeEvent read FStatusChange write FStatusChange;
    // Evento chamado quando arquivos são arrastados sobre o editor
    property OnEditorDropFiles: TDropFilesEvent read FDropFiles write FDropFiles;
    // StartPage
    // Passe a função do Notes que responderá para abrir arquivos da lista de MRU
    property  OnStartPageOpenMruItem: TNotesOpenMruItem read fOpenMru write fOpenMru;
    // Passe a função do Notes para "novo arquivo"
    property  OnStartPageRequestNewFile: TNotifyEvent read fOnNewFile write fOnNewFile;
    // Passe a função do Notes para "abrir arquivo"
    property  OnStartPageRequestOpenFile: TNotifyEvent read fOnOpenFile write fOnOpenFile;
    // Passe a função do Notes para "novo projeto"
    property  OnStartPageRequestNewProject: TNotifyEvent read fOnNewProject write fOnNewProject;
    // Passe a função do Notes para "abrir projeto"
    property  OnStartPageRequestOpenProject: TNotifyEvent read fOnOpenProject write fOnOpenProject;
  end;

implementation

uses NotesTemplates, NotesUtils, Forms, SynEditTypes,
  clipbrd, NotesRememberEditorState, NotesGlobals, NotesEventServer;

{ TNotesTabsManager }

procedure TNotesTabsManager.CloseAllEditors;
var
  I: integer;
begin
  lockWindowUpdate(fPageCtrl.handle);
  fPageCtrl.ActivePageIndex:= -1;

  for I:= 0 to fEditorsCount-1 do
  begin
    SaveEditorState(fEditors[I]);
    FreeAndNil(fEditors[I]);
  end;

  SetLength( fEditors, 0 );
  fEditorsCount:= 0;

  if fPageCtrl.PageCount > 0 then
    fPageCtrl.ActivePageIndex:= 0;

  lockWindowUpdate(0);
end;

procedure TNotesTabsManager.CloseActiveTab;
var
  I, Index, PageIndex: Integer;
  tab: TTabSheet;
begin

  if fPageCtrl.PageCount = 0 then Exit;

  if IsStartPageActivated then
  begin
    tab:= (fStartPage.owner As TTabSheet);
    FreeAndNil(FStartPage);
    Index:= fPageCtrl.ActivePageIndex;
    fPageCtrl.ActivePageIndex:= -1;
    FreeAndNil(tab);
    if fPageCtrl.PageCount > 0 then
    begin
      if Index - 1 > -1 then
        fPageCtrl.ActivePageIndex:= Index-1
      else
        fPageCtrl.ActivePageIndex:= Index;
    end;
    Exit;
  end;

  if not IsEditorActivated then
    Exit;

  SaveEditorState(ActiveEditorTab);

  Index := GetEditorIndex( ActiveEditorTab );
  PageIndex := fPageCtrl.ActivePageIndex;

  // Paramos de pintar o pagecontrol
  LockWindowUpdate(fPAgeCtrl.Handle);
  // Para evitar erros, tiramos o foco da página q será destruída
  fPageCtrl.ActivePageIndex:= -1;
  // destruímos ela
  fEditors[Index].Free;
  // pensamos quem mostraremos :)
  if fPageCtrl.PageCount > 0 then
  begin
    if Pred( PageIndex ) = -1 then
      fPageCtrl.ActivePageIndex:= PageIndex
    else
      fPageCtrl.ActivePageIndex:= Pred( PageIndex );
  end;
  // Redesenhamos
  LockWindowUpdate(0);

  if ( Index = -1 ) or ( fEditorsCount = 0 ) then
    Exit;
  for I := Index to fEditorsCount - 2 do
    fEditors[I] := fEditors[Succ( I )];
  Dec(fEditorsCount);
  SetLength( fEditors, fEditorsCount );

  if ActiveEditorTab <> nil then
    if ActiveEditorTab.Editor.CanFocus then
      ActiveEditorTab.Editor.SetFocus;
end;

function TNotesTabsManager.CreateNewEditor: Integer;

  function GetCaption : string;

    function CaptionExists(const S: string): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      for I:= 0 to Pred(fEditorsCount) do
        if (fEditors[I].Tab.Caption = S) or
           (fEditors[I].Tab.Caption = S + '*') then
        begin
          Result := True;
          Break;
        end;
    end;

  var
    I: Integer;
  begin
    I := 1;
    while CaptionExists( 'Documento ' + IntToStr( I ) ) and ( I < MaxInt ) do
      Inc(I);
    Result := 'Documento '+IntToStr( I );
  end;

begin
  //Length retorna o último elemento + 1
  Result := fEditorsCount;
  //criamos um um novo item
  Inc(fEditorsCount);
  SetLength( fEditors, fEditorsCount );
  //criamos o editor
  fEditors[Result] := TNotesEditorTab.Create( fPageCtrl, fPageCtrl );
  fEditors[Result].Editor.PopupMenu := fpopMenu;

  with fEditors[Result], Editor do
  begin
    Tab.Caption := GetCaption;
    OnDragOver := EditorOnDragOverHandler;
    OnDragDrop := EditorOnDragDropHandler;
    OnDropFiles := FDropFiles;
    OnStatusChange := FStatusChange;
    PopupMenu:= fpopMenu;
    Gutter.LeftOffset := 4;
    Gutter.RightOffset := 8;
    Gutter.AutoSize := true;
    ScrollHintFormat := shfTopToBottom;
    SetEditorOption( eoShowScrollHint, True );
  end;
end;

procedure TNotesTabsManager.EditorBreakLine(Sender: TObject);
begin
  if isEditorActivated then
  begin
    ActiveEditorTab.Editor.ExecuteCommand(ecLineBreak, #0, nil);
    if ActiveEditorTab.UseSmartIndent then
      ActiveEditorTab.IndentLine;
  end;
end;

procedure TNotesTabsManager.EditorBreakLineFromEOL(Sender: TObject);
begin
  if isEditorActivated then
  begin
    ActiveEditorTab.Editor.ExecuteCommand(ecLineEnd, #0, nil);
    ActiveEditorTab.Editor.ExecuteCommand(ecLineBreak, #0, nil);
    if ActiveEditorTab.UseSmartIndent then
      ActiveEditorTab.IndentLine;
  end;
end;

procedure TNotesTabsManager.EditorBreakLineNoMove(Sender: TObject);
Var
  P: TPoint;
begin
  if ActiveEditorTab = nil then Exit;
  P:= BufferCoordToPoint(ActiveEditorTab.Editor.CaretXY);
  EditorBreakLine(Sender);
  ActiveEditorTab.Editor.CaretXY:= PointToBufferCoord(P);
end;

procedure TNotesTabsManager.EditorClearAll(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ClearAll;
end;

procedure TNotesTabsManager.EditorComment(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.CommentBlock;
end;

procedure TNotesTabsManager.EditorCopy(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.CopyToClipBoard;
end;

procedure TNotesTabsManager.EditorCopyAppend(Sender: TObject);
begin
  if ActiveEditorTab = nil then Exit;

  if (ActiveEditorTab.Editor.SelAvail) and (clipboard.HasFormat(CF_TEXT)) then
  begin
    clipboard.AsText:= clipboard.AsText + ActiveEditorTab.Editor.SelText;
  end else
    // se não tiver texto, apenas copiamos
    self.EditorCopy(Sender);
end;

procedure TNotesTabsManager.EditorCut(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.CutToClipboard;
end;

procedure TNotesTabsManager.EditorDelete(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Delete;
end;

procedure TNotesTabsManager.EditorDeleteBOL(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDeleteBOL, #0, nil);
end;

procedure TNotesTabsManager.EditorDeleteEOL(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDeleteEOL, #0, nil);
end;

procedure TNotesTabsManager.EditorDeleteLastChar(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDeleteLastChar, #0, nil);
end;

procedure TNotesTabsManager.EditorDeleteLastWord(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDeleteLastWord, #0, nil);
end;

procedure TNotesTabsManager.EditorDeleteWord(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDeleteWord, #0, nil);
end;

procedure TNotesTabsManager.EditorDelLine(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditortab.DeleteLine;
end;

procedure TNotesTabsManager.EditorDown(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecDown, #0, nil);
end;

procedure TNotesTabsManager.EditorDuplicateLine(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditortab.DuplicateLine;
end;

procedure TNotesTabsManager.EditorEditorBottom(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecEditorBottom, #0, nil);
end;

procedure TNotesTabsManager.EditorEditorTop(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecEditorTop, #0, nil);
end;

procedure TNotesTabsManager.EditorGoToMatchBracket(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecMatchBracket, #0, nil);
end;

procedure TNotesTabsManager.EditorIndent(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.IndentBlock;
end;

procedure TNotesTabsManager.EditorLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorLineEnd(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecLineEnd, #0, nil);
end;

procedure TNotesTabsManager.EditorLineStart(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecLineStart, #0, nil);
end;

procedure TNotesTabsManager.EditorLowerCase(Sender: TObject);
begin
  if isEditorActivated then
  begin
    with ActiveEditorTab.Editor do
    begin
      if SelAvail = false then
        EditorSelWordAtCursor(nil);
      SelText:= AnsiLowerCase(SelText);
    end;
  end;
end;

procedure TNotesTabsManager.EditorNextMark(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.NextMark;
end;

procedure TNotesTabsManager.EditorOnDragDropHandler(Sender,
  Source: TObject; X, Y: Integer);
begin
  if ActiveEditorTab = nil then Exit;

  if (isDragObject(Source)) and (Source is TNotesDragObject) then
  begin
    Case (Source As TNotesDragObject).DragType of
      dtNcl: ActiveEditorTab.InsertNCLItem((Source As TNotesDragObject).Data);
      dtFileToInsert: ActiveEditorTab.InsertTextFromFile((Source As TNotesDragObject).Data);
      dtTextToInsert: ActiveEditorTab.Editor.SelText:= (Source As TNotesDragObject).Data;
      dtFileToOpen:
        begin
          if LoadFile( (Source As TNotesDragObject).Data ) then
          begin
            NProfile.MRU.Add( (Source As TNotesDragObject).Data );
            NProfile.MRU.SaveToFile( NProfile.Paths.MRUFile );
          end;
        end;
    end;

    if IsEditorActivated then
      ActiveEditorTab.Editor.SetFocus;
  end;
end;

procedure TNotesTabsManager.EditorOnDragOverHandler(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if ActiveEditorTab = nil then Exit;
  if (isDragObject(Source)) and (Source is TNotesDragObject) then
  begin
    Accept := True;
    if not ActiveEditorTab.Editor.SelAvail then
    begin
      with ActiveEditorTab.Editor do
        CaretXY:= DisplayToBufferPos(PixelsToRowColumn(X, Y));
    end;
    if IsEditorActivated then
      ActiveEditorTab.Editor.SetFocus;
  end
  else
    Accept:= false;
end;

procedure TNotesTabsManager.EditorPageBottom(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageBottom, #0, nil);
end;

procedure TNotesTabsManager.EditorPageDown(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageDown, #0, nil);
end;

procedure TNotesTabsManager.EditorPageLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorPageRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageRight, #0, nil);
end;

procedure TNotesTabsManager.EditorPageTop(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageTop, #0, nil);
end;

procedure TNotesTabsManager.EditorPageUp(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecPageUp, #0, nil);
end;

procedure TNotesTabsManager.EditorPaste(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.PasteFromClipboard;
end;

procedure TNotesTabsManager.EditorPreviusMark(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.PreviousMark;
end;

procedure TNotesTabsManager.EditorRedo(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecRedo, #0, nil);
end;

procedure TNotesTabsManager.EditorRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecRight, #0, nil);
end;

procedure TNotesTabsManager.EditorScrollDown(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecScrollDown, #0, nil);
end;

procedure TNotesTabsManager.EditorScrollLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecScrollLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorScrollRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecScrollRight, #0, nil);
end;

procedure TNotesTabsManager.EditorScrollUp(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecScrollUp, #0, nil);
end;

procedure TNotesTabsManager.EditorSelAll(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.SelectAll;
end;

procedure TNotesTabsManager.EditorSelDown(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelDown, #0, nil);
end;

procedure TNotesTabsManager.EditorSelEditorBottom(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelEditorBottom, #0, nil);
end;

procedure TNotesTabsManager.EditorSelEditorTop(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelEditorTop, #0, nil);
end;

procedure TNotesTabsManager.EditorSelLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorSelLine(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.SelectLine;
end;

procedure TNotesTabsManager.EditorSelLineEnd(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelLineEnd, #0, nil);
end;

procedure TNotesTabsManager.EditorSelLineStart(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelLineStart, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageBottom(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageBottom, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageDown(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageDown, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageRight, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageTop(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageTop, #0, nil);
end;

procedure TNotesTabsManager.EditorSelPageUp(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelPageUp, #0, nil);
end;

procedure TNotesTabsManager.EditorSelRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelRight, #0, nil);
end;

procedure TNotesTabsManager.EditorSelUp(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelUp, #0, nil);
end;

procedure TNotesTabsManager.EditorSelWordLeft(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelWordLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorSelWordRight(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecSelWordRight, #0, nil);
end;

procedure TNotesTabsManager.EditorShiftTab(Sender: TObject);
begin
  if isEditorActivated then
  begin
    if not ActiveEditorTab.Editor.SelAvail then
      ActiveEditorTab.Editor.ExecuteCommand(ecShiftTab, #0, nil)
    else
      ActiveEditorTab.UnIndentBlock;
  end;
end;

procedure TNotesTabsManager.EditorSmartPaste(Sender: TObject);
begin
  //////////////
end;

procedure TNotesTabsManager.EditorTab(Sender: TObject);
begin
  if isEditorActivated then
  begin
    if not ActiveEditorTab.Editor.SelAvail then
      ActiveEditorTab.Editor.ExecuteCommand(ecTab, #0, nil)
    else
      ActiveEditorTab.IndentBlock;
  end;
end;

procedure TNotesTabsManager.EditorTitleCase(Sender: TObject);
begin
  if isEditorActivated then
  begin
    with ActiveEditorTab.Editor do
    begin
      if SelAvail = false then
        EditorSelWordAtCursor(nil);

      ExecuteCommand(ecTitleCase, #0, nil);
    end;
  end;
end;

procedure TNotesTabsManager.EditorToggleCase(Sender: TObject);
begin
  if isEditorActivated then
  begin
    with ActiveEditorTab.Editor do
    begin
      if SelAvail = false then
        EditorSelWordAtCursor(nil);

      ExecuteCommand(ecToggleCase, #0, nil);
    end;
  end;
end;

procedure TNotesTabsManager.EditorToggleMode(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecToggleMode, #0, nil);
end;

procedure TNotesTabsManager.EditorTogleMark(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.ToogleMark;
end;

procedure TNotesTabsManager.EditorUcomment(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.UnCommentBlock;
end;

procedure TNotesTabsManager.EditorUndo(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Undo;
end;

procedure TNotesTabsManager.EditorUnindent(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.UnIndentBlock;
end;

procedure TNotesTabsManager.EditorUp(Sender: TObject);
begin
  if isEditorActivated then
   ActiveEditorTab.Editor.ExecuteCommand(ecUp, #0, nil);
end;

procedure TNotesTabsManager.EditorUpperCase(Sender: TObject);
begin
  if isEditorActivated then
  begin
    with ActiveEditorTab.Editor do
    begin
      if SelAvail = false then
        EditorSelWordAtCursor(nil);
      SelText:= AnsiUpperCase(SelText);
    end;
  end;
end;

procedure TNotesTabsManager.EditorWordLeft(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecWordLeft, #0, nil);
end;

procedure TNotesTabsManager.EditorWordRight(Sender: TObject);
begin
  if isEditorActivated then
    ActiveEditorTab.Editor.ExecuteCommand(ecWordRight, #0, nil);
end;

function TNotesTabsManager.getActiveEditorTab: TNotesEditorTab;
begin
  if (Assigned(fPageCtrl.ActivePage) = false) or (IsStartPageActivated = true) then
    Result := nil
  else
    Result := Pointer( fPageCtrl.ActivePage.Tag );
end;

function TNotesTabsManager.GetEditorIndex(
  Editor: TNotesEditorTab): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Editor = nil then
    Exit;
  for I := 0 to High(fEditors) do
    if Editor = fEditors[I] then
    begin
      Result := I;
      Break;
    end;
end;

function TNotesTabsManager.GetFileTypeByExtension(Ext: string): string;
var
  I: Integer;
begin
  Result := '';
  //remove o '.' da extensão, caso exista.
  Ext := Copy( Ext, Pos( '.', Ext )+1, Length( Ext ) );
  for I := 0 to High( NProfile.Associations ) do
    if SameText( Ext, NProfile.Associations[I, naExtension] ) then
    begin
      Result := NProfile.Associations[I, naLanguage];
      Break;
    end;                 
end;

procedure TNotesTabsManager.GoToCol(ColNo: integer);
begin
  ColNo := IIF( NProfile.Config.LineAndColCountStartAt0, ColNo+1, ColNo );
  if isEditorActivated then
    ActiveEditorTab.GoToCol(ColNo);
end;

procedure TNotesTabsManager.GoToLine(LineNo: Integer);
begin
  LineNo := IIF( NProfile.Config.LineAndColCountStartAt0, LineNo+1, LineNo );
  if IsEditorActivated then
    ActiveEditorTab.GoToLine(LineNo);
end;

procedure TNotesTabsManager.GoToLine(LineNo: String);
begin
  GoToLine( StrToIntDef( LineNo, -1 ) );
end;

procedure TNotesTabsManager.InsertNCL(NCLFile: string);
begin
  if not IsEditorActivated then
    Exit;

  NCLFile := PathGetLongName( NCLFile );
  if FileExists(NCLFile) then
    ActiveEditorTab.InsertNCLItem(NCLFile)
  else
    msgExclama('Erro: o Notes não pode encontrar o arquivo "'+ NCLFile +'" para inserção.', 0 );
end;

function TNotesTabsManager.IsEditorActivated: Boolean;
begin
  Result := (ActiveEditorTab <> nil) and (Application.MainForm.Visible);
end;

function TNotesTabsManager.IsStartPageActivated: boolean;
Var
  o: TObject;
begin
  Result:= false;
//  if ActiveEditorTab <> nil then Exit;
  if fPageCtrl.PageCount = 0 then Exit;
  if fStartPage = nil then Exit;
  o:= Pointer(fPageCtrl.ActivePage.Tag);
  if (o <> nil) and (o is TNotesEditorTab) then
    Result:= false
  else
    Result:= true;
end;


function TNotesTabsManager.LoadFile(const FileName: string): boolean;
var
  S: string;
  i: integer;
begin
  Result:= false;

  if not FileExists( FileName ) then Exit;

  { Não deixa abrir o mesmo documento duas vezes }
  for I:= Pred( fEditorsCount ) downto 0 do
  begin
    if SameText(FileName, fEditors[I].FullPath) then
    begin
      fPageCtrl.ActivePage:= fEditors[I].Tab;
      Exit;
    end;
  end;

  eventServer.NotifyEvent(self, 'beforefileopen', FileName);

  CreateNewEditor;

  //SUPORTE PARA ARQUIVOS READONLY
  fEditors[fEditorsCount-1].Editor.ReadOnly := FileIsReadOnly(FileName);

  //CARREGA-SE O ARQUIVO NO Editor
  try
    //dizemos ao windows para parar de atualizar a imagem
    //do editor, em finally voltamos a atualizá-la
    fEditors[fEditorsCount-1].Editor.Lines.BeginUpdate;

    // tentamos carregar o highlighter para o arquivo
    S:= ExtractFileExt(FileName);
    if S <> NOTES_TEMPLATE_EXT then
    begin
      S:= GetFileTypeByExtension( S );
    end else
    begin
      S:= Templates.getTemplateFileType(FileName);
    end;
    if S <> '' then
      fEditors[fEditorsCount-1].FileType:= S
    else
      fEditors[fEditorsCount-1].FileType:= NProfile.Config.DefaultFileType;

    try
      case fEditors[fEditorsCount-1].LoadFromFile( FileName ) of
        lfCancel:
          begin
            // o usuário cancelou. Removemos o editor que criamos e saímos
            fEditors[fEditorsCount-1].Free;
            Dec(fEditorsCount);
            setLength( fEditors, fEditorsCount);
            Result:= false;
            Exit;
          end;

        lfError: Result:= false;
        lfOk: Result:= true;
      end;

    except
      //ALGUNS ARQUIVOS DE .LOG CAUSAM UM ERRO DE SHARING AO TENTARMOS ABRI-LOS.
      //MAS HÁ COMO CONTORNAR ISTO COPIANDO O ARQUIVO PARA O OUTRO LUGAR
      //E ABRINDO A CÓPIA COMO SE FOSSE O ARQUIVO!
      if CopyFile(PChar(FileName), PChar( NProfile.Paths.UserDir + 'forced.txt'), false) <> false then
      begin
        S := FileToStr( NProfile.Paths.UserDir + 'forced.txt' );
        try
          StrToFile( FileName, S );
          fEditors[fEditorsCount-1].Editor.ReadOnly := False;
        except
          fEditors[fEditorsCount-1].Editor.ReadOnly := True;
        end;
        fEditors[fEditorsCount-1].Editor.Text := S;
        Result := True;
      end
      else begin
        Result := False;
        raise Exception.Create('Erro de sharing ao abrir o arquivo. Ele deve estar sendo usado por outro programa ou pelo sistema operacional e não poderá ser aberto no momento nem pelo Notes nem por qualquer outro programa.');
      end;
    End;
  finally
    if Result then
    begin
      fEditors[fEditorsCount-1].Editor.Lines.EndUpdate;

      if nProfile.Config.RemeberFileInfo > 0 then
      begin
        With TNotesRemeberEditorState.Create(fEditors[fEditorsCount-1]) do
          try
            LoadState;
          finally
            Free;
          end;
      end;

      fEditors[fEditorsCount-1].Editor.Modified := False;
      eventServer.NotifyEvent(self, 'afterfileopen', FileName);
    end;
  end;
end;

procedure TNotesTabsManager.LoadMultipleFiles(const Files: TStrings);
Var
  I, LoadedCount: integer;
begin
  nProfile.MRU.LoadFromFile(NProfile.Paths.MRUFile);
  LoadedCount:= 0;
  screen.Cursor:= crHourGlass;
  LockWindowUpdate(fPageCtrl.handle);
  try
    for I:= 0 to Files.Count-1 do
    begin
      if LoadFile( Files.Strings[I] ) then
      begin
        nProfile.MRU.Add( Files.Strings[I]);
        inc( LoadedCount );
      end;
    end;
    if LoadedCount > 0 then
      fPageCtrl.ActivePageIndex:= Pred( fPageCtrl.PageCount );
  finally
    LockWindowUpdate(0);
    screen.Cursor:= crDefault;
    nProfile.MRU.SaveToFile( NProfile.Paths.MRUFile );
    if IsEditorActivated then
      ActiveEditorTab.Editor.SetFocus;
   end;
end;


procedure TNotesTabsManager.ShowStartPage;
Var
  newTab: TTabSheet;
begin
  if fStartPage = nil then
  begin
    newTab:= TTabSheet.Create(fPageCtrl);
    newTab.Parent:= (fPageCtrl As TWinControl);
    newTab.PageControl:= fPageCtrl;
    newTab.Caption:='StartPage';
    newTab.PageIndex:= 0;
    fStartPage:= TNotesStartPage.Create(newTab);
    fStartPage.OnOpenMruItem:= fOpenMru;
    fStartPage.OnRequestOpenProject:= fOnOpenProject;
    fStartPage.OnRequestNewProject:= fOnNewProject;
    fStartPage.OnRequestNewFile:= fOnNewFile;
    fStartPage.OnRequestOpenFile:= fOnOpenFile;
  end else
  begin
    if fStartPage.Owner is TTabSheet then
      fPageCtrl.ActivePage:= (fStartPage.Owner as TTabSheet);
  end;
end;

procedure TNotesTabsManager.EditorSelWordAtCursor(Sender: TObject);
begin
  if isEditorActivated then
  begin
    with ActiveEditorTab.Editor do
    begin
      if LineText <> '' then
      begin
        BlockBegin:= WordStart;
        BlockEnd:= WordEnd;
      end;
    end;
  end;
end;

function TNotesTabsManager.HasStartPage: boolean;
begin
  Result:= Assigned(fStartPage);
end;

procedure TNotesTabsManager.ReloadStartPage;
begin
  if Assigned(fStartPage) then
  begin
    nProfile.MRU.LoadFromFile(nProfile.Paths.MRUFile);
    fStartPage.ReloadStartPage(nProfile.MRU.List, nil);
  end;
end;

function TNotesTabsManager.getEditorTab(Index: Integer): TNotesEditorTab;
begin
  Result:= fEditors[Index];
end;

procedure TNotesTabsManager.setEditorTab(Index: Integer;
  const Value: TNotesEditorTab);
begin
  fEditors[Index]:= Value;
end;

destructor TNotesTabsManager.Destroy;
begin
  CloseAllEditors;
  fPageCtrl.ActivePageIndex:= -1;
  if Assigned(fStartPage) then
    FreeAndNil(fStartPage);
  inherited;
end;

constructor TNotesTabsManager.Create;
begin
  fEditorsCount:= 0;
  fPageCtrl:= nil;
  FDropFiles:= nil;
  FStatusChange:= nil;
  fpopMenu:= nil;
  fStartPage:= nil;
  fOpenMru:= nil;
  fOnOpenProject:= nil;
  fOnNewProject:= nil;
  fOnNewFile:= nil;
  fOnOpenFile:= nil;
end;


procedure TNotesTabsManager.SaveEditorState(
  const EditorTab: TNotesEditorTab);
begin
  if EditorTab = nil then Exit;
  if EditorTab.FullPath = '' then Exit;

  if nProfile.Config.RemeberFileInfo > 0 then
  begin
    with TNotesRemeberEditorState.Create(EditorTab) do
      try
        SaveState;
      finally
        free;
      end;
  end;
end;


procedure TNotesTabsManager.FocusActiveEditor;
begin
  if ActiveEditorTab <> nil then
  begin
    if ActiveEditorTab.Editor.CanFocus then
      ActiveEditorTab.Editor.SetFocus;
  end;
end;

procedure TNotesTabsManager.EditorReIndent(Sender: TObject);
begin
  if ActiveEditorTab <> nil then
    ActiveEditorTab.ReindentBlock;
end;

procedure TNotesTabsManager.EditorClearMarks(Sender: TObject);
begin
  if ActiveEditorTab <> nil then
  begin
    ActiveEditorTab.Marks.Clear;
    ActiveEditorTab.Editor.InvalidateGutter;
  end;
end;

function TNotesTabsManager.getEditorByTabIndex(
  const index: integer): TNotesEditorTab;
begin
  Result := nil;
  if (index > -1) and (index < fPageCtrl.PageCount) then
    Result:= Pointer( fPageCtrl.ActivePage.Tag );
end;

procedure TNotesTabsManager.EditorSpacesToTabs(Sender: TObject);
begin
  if ActiveEditorTab <> nil then
    ActiveEditorTab.SpacesToTabs;
end;

procedure TNotesTabsManager.EditorTabsToSpaces(Sender: TObject);
begin
  if ActiveEditorTab <> nil then
    ActiveEditorTab.TabsToSpaces;
end;

end.
