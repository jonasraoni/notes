//
//    NotesLibraryPanel - painel "Biblioteca" do Notes.
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
  @abstract(NotesLibraryPanel - painel "Biblioteca" do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  @author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
*)
unit NotesLibraryPanel;

interface

uses Classes, NotesPanels, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Graphics, NotesEditorTab, NotesGlobals, Menus;

type
  TNotesLibraryPanel = class(TNotesPanel)
  private
    lLibLang: TLabel;
    meLibHelp: TMemo;
    tvLibrary: TTreeView;
    cbLibFileType: TComboBox;
    fLastLibFileType: string;
    fDragObj: TNotesDragObject;
    fPopup: TPopupMenu;
    procedure tvLibraryClick(Sender: TObject);
    procedure tvLibraryDblClick(Sender: TObject);
    procedure tvLibraryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbLangClick(Sender: TObject);
    procedure BuildCodeLibrary;
    function GetCodeLibrarySelItemPath: string;
    procedure tvLibStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure tvLibEndDrag(Sender, Target: TObject; X, Y: Integer);
    //menu
    procedure poDelete(Sender: TObject);
    procedure poAddFolder(Sender: TObject);
    procedure poEditItem(Sender: TObject);
    procedure poAddItem(Sender: TObject);
    procedure poRenameFolder(Sender: TObject);
    function getFileType: String;
    procedure setFileType(const Value: String);

  public
    constructor Create(const StartUpHost: TNotesPanelHost); override;
    destructor  Destroy; override;
    function    getPanelName: string; override;
    procedure   Resize; override;
    procedure   Initialize; override;
    procedure   ReadOptions(const S: string); override;
    procedure   WriteOptions(var  S: string); override;
  published
    // Executa as ações do painel
    // "insert" - insere o item atual
    // "expand" - expande todas as categorias
    // "collapse" - fecha todas as categorias
    function PanelExec(const PanelAction, Params: string): string; override;
    // permite ler/escrever o tipo de arquivo que aparecerá na biblioteca
    property FileType: String read getFileType write setFileType;
  end;

implementation

Uses NotesUtils, Forms, SysUtils, NotesXML, Dialogs, frm_addItem,
  NotesCopyDeldialog, Windows;

const
  // ícone de pasta da biblioteca aberta
  NOTES_LIB_FOLDER_OPEN = 36;
  // ícone de pasta da biblioteca fechada
  NOTES_LIB_FOLDER_CLOSE= 37;
  // ícone para itens da biblioteca
  NOTES_LIB_ITEM        = 38;

{ TNotesLibraryPanel }

procedure TNotesLibraryPanel.cbLangClick(Sender: TObject);
begin
  BuildCodeLibrary;
end;

constructor TNotesLibraryPanel.Create;
var
  pi: TMenuItem;
begin
  inherited;

  lLibLang := TLabel.Create(compsOwner);
  meLibHelp := TMemo.Create(compsOwner);
  tvLibrary := TTreeView.Create(compsOwner);
  cbLibFileType := TComboBox.Create(compsOwner);

  with lLibLang do
  begin
    Name:= 'lLibLang';
    Parent := self.Parent;
    Left := 2;
    Top := 5;
    Width := 55;
    Height := 13;
    Caption := 'Linguagem:';
    Transparent := True;
  end;
  with meLibHelp do
  begin
    Parent := self.Parent;
    Left := 2;
    Top := 319;
    Width := 175;
    Height := 46;
    Cursor := crArrow;
    Font.Color := clNavy;
    ReadOnly := True;
    ScrollBars := ssVertical;
    WantReturns := False;
    PopupMenu:= nil;
  end;
  with tvLibrary do
  begin
    Parent := self.Parent;
    Left := 2;
    Top := 28;
    Width := 172;
    Height := 285;
    DragMode := dmAutomatic;
    HideSelection := False;
    Indent := 19;
    ReadOnly := True;
    RightClickSelect := True;
    RowSelect := True;
    ShowButtons := False;
    ShowLines := False;
    ShowRoot := False;
    SortType := stText;
    TabOrder := 1;
    Images:= NImages;
    OnClick := tvLibraryClick;
    OnDblClick := tvLibraryDblClick;
    OnMouseDown := tvLibraryMouseDown;
    OnStartDrag:=tvLibStartDrag;
    OnEndDrag:= tvLibEndDrag;
  end;
  with cbLibFileType do
  begin
    Parent := self.Parent;
    Left := 62;
    Top := 2;
    Width := 114;
    Height := 20;
    Style := csDropDownList;
    ItemHeight := 14;
    OnCloseUp := cbLangClick;
  end;

  fPopup:= TPopupMenu.Create(compsOwner);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnAddItem';
  pi.Caption:= '&Adicionar Item';
  pi.OnClick:= poAddItem;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnEditItem';
  pi.Caption:= '&Editar Item';
  pi.OnClick:= poEditItem;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnAddFolder';
  pi.Caption:= 'Adicionar &Categoria';
  pi.OnClick:= poAddFolder;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnRenameFolder';
  pi.Caption:= '&Renomear Categoria';
  pi.OnClick:= poRenameFolder;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnDelete';
  pi.Caption:= '&Deletar';
  pi.OnClick:= poDelete;
  fPopup.Items.Add(pi);

  tvLibrary.PopupMenu:= fPopup;

//  NotesMenu.InitComponent(fPopup);
//  NotesMenu.ActivateMenuItem(fPopup.Items, true);
end;

destructor TNotesLibraryPanel.Destroy;
begin
   //
  inherited;
end;

procedure TNotesLibraryPanel.Resize;
var
  BaseWidth: integer;
begin
  baseWidth:= Parent.Width - 8;
  tvLibrary.Width:= baseWidth;
  meLibHelp.Width:= baseWidth;
  cbLibFileType.Width:= baseWidth - 58;
  tvLibrary.Height:= tvLibrary.Parent.Height - 86;
  meLibHelp.Top:= meLibHelp.Parent.Height - 51;
end;

procedure TNotesLibraryPanel.tvLibraryClick(Sender: TObject);
Var
  S: string;
begin
  if tvLibrary.Selected = nil then Exit;

  if tvLibrary.Selected.ImageIndex = NOTES_LIB_ITEM then
  begin
    S:= GetCodeLibrarySelItemPath + NOTES_DESCRIPTION_EXT;
    if fileexists(S) then
      meLibHelp.Lines.LoadFromFile(S)
    else
      meLibHelp.Lines.Text:= getMsgTranslation('MsgNoHelpFound', 'No help found.');
  end;
end;

procedure TNotesLibraryPanel.tvLibraryDblClick(Sender: TObject);
begin
  if tvLibrary.Selected = nil then
    Exit;

  if tvLibrary.Selected.ImageIndex = NOTES_LIB_ITEM then
  begin
    if ActiveEditorTab <> nil then
      ActiveEditorTab.InsertNCLItem(GetCodeLibrarySelItemPath + '.ncl');
  end else
  if tvLibrary.Selected.ImageIndex = NOTES_LIB_FOLDER_CLOSE then
  begin
    if tvLibrary.Selected.Count > 0 then
    begin
      tvLibrary.Selected.SelectedIndex:= NOTES_LIB_FOLDER_OPEN;
      tvLibrary.Selected.ImageIndex:= NOTES_LIB_FOLDER_OPEN;
      tvLibrary.Selected.Expand( False );
    end;
  end
  else if tvLibrary.Selected.ImageIndex = NOTES_LIB_FOLDER_OPEN then
    if tvLibrary.Selected.Count > 0 then
    begin
      tvLibrary.Selected.SelectedIndex:= NOTES_LIB_FOLDER_CLOSE;
      tvLibrary.Selected.ImageIndex:= NOTES_LIB_FOLDER_CLOSE;
      tvLibrary.Selected.Collapse(true)
    end;
end;

function TNotesLibraryPanel.GetCodeLibrarySelItemPath: string;
var
  Node: TTreeNode;
  S: string;
begin
  Node := tvLibrary.Selected;
  Result := NProfile.Paths.FileTypesDir + AddSlash( cbLibFileType.Text ) + AddSlash( NOTES_CODELIBRARY_DIRNAME );

  if Node = nil then
    Exit;

  S := Node.Text;
  while Node.Level <> 0 do begin
    S := AddSlash( Node.Parent.Text ) + S;
    Node := Node.Parent;
  end;
  Result := Result + S;
end;


procedure TNotesLibraryPanel.tvLibraryMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // seleciona o item qdo ocorre um click com o botão direito do mouse
  // por algum bug, o delphi não faz isto automaticamente...
  tvLibrary.Select(tvLibrary.GetNodeAt(X, Y));
end;

procedure TNotesLibraryPanel.BuildCodeLibrary;

  procedure AddNodes( Tree: PNotesFolderTree; RefNode: TTreeNode = nil );
  const
    Icons: array[Boolean] of Integer = ( NOTES_LIB_ITEM, NOTES_LIB_FOLDER_CLOSE );
  var
    I: Integer;
    Node: TTreeNode;
  begin
    for I := 0 to High( Tree^ ) do
    begin
      with Tree^[I] do
      begin
        if IsFolder then
        begin
          Node := tvLibrary.Items.AddChild( RefNode, Path );
          AddNodes( Tree^[I].Items, Node );
        end
        else
          Node := tvLibrary.Items.AddChild( RefNode, GetFileName( Path ) );

        Node.ImageIndex := Icons[ IsFolder ];
        Node.SelectedIndex := Icons[ IsFolder ];
        Node.Data := Pointer( Integer( IsFolder ) );
      end;
    end;
  end;

var
  LibraryTree: PNotesFolderTree;
begin
  screen.Cursor:= crHourGlass;
  tvLibrary.Items.BeginUpdate;
  tvLibrary.Items.Clear;

  NProfile.FileTypesMgr.ListLibrary( cbLibFileType.Text, LibraryTree );

  AddNodes( LibraryTree );

  FreeFolderTree( LibraryTree );

  tvLibrary.Items.EndUpdate;

  if tvLibrary.Items.Count < 1 then
    meLibHelp.Text:= getMsgTranslation('MsgNoItemsFound', 'No items found. Use the context menu to add items.')
  else
    meLibHelp.Text:= getMsgTranslation('MsgDblClickToInsert', 'Use double click to insert an item.');

  screen.Cursor:= crDefault;
end;

procedure TNotesLibraryPanel.Initialize;
Var
  FileTypes: PNotesFolderTree;
  I: integer;
begin
  inherited;

  NProfile.FileTypesMgr.ListTypes(FileTypes);

  cbLibFileType.Items.BeginUpdate;
  for I := 0 to High( FileTypes^ ) do
      cbLibFileType.Items.Add( FileTypes^[I].Path );
  cbLibFileType.Sorted := True;
  cbLibFileType.Items.EndUpdate;

  FreeFolderTree(FileTypes);

  cbLibFileType.ItemIndex:= cbLibFileType.Items.IndexOf(fLastLibFileType);

  BuildCodeLibrary;

end;

procedure TNotesLibraryPanel.ReadOptions(const S: string);
begin
  fLastLibFileType:= ReadTagStr(S, 'LastLibFileType', 'Text');
end;

procedure TNotesLibraryPanel.WriteOptions(var S: string);
begin
  WriteTagStr(@S, 'LastLibFileType', cbLibFileType.Text);
end;

// SUPORTE A DRAG AND DROP

procedure TNotesLibraryPanel.tvLibEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  FreeandNil(fDragObj);
end;

procedure TNotesLibraryPanel.tvLibStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  S: string;
begin
  S:= GetCodeLibrarySelItemPath + '.ncl';
  if Not FileExists(S) then Exit;

  fDragObj:= TNotesDragObject.Create;
  fDragObj.DragType:= dtNcl;
  fDragObj.Data:= S;
  DragObject:= fDragObj;
end;

// MENU

procedure TNotesLibraryPanel.poAddFolder(Sender: TObject);
Var
  S: string;
begin
  if not InputQuery(getMsgTranslation('lNewCategory', 'New category'),
    getMsgTranslation('DescrNewCategory', 'New category name:'), S) then
    Exit;
  Screen.Cursor:= crHourGlass;
  try
    CreateDir( ExtractFilePath( GetCodeLibrarySelItemPath ) + S );
    BuildCodeLibrary;
  finally
    screen.Cursor:= crDefault;
  end;
end;

procedure TNotesLibraryPanel.poAddItem(Sender: TObject);
begin
  with TfrmAddLibItem.Create( nil ) do
  begin
    if  tvLibrary.Selected.Index <> -1 then
    begin
      if tvLibrary.Selected.ImageIndex = NOTES_LIB_ITEM then
        AddMode( ExtractFilePath( GetCodeLibrarySelItemPath ) )
      else
        AddMode( AddSlash( GetCodeLibrarySelItemPath ) );
      try
        ShowModal;
        if ModalResult = MrOK then
          BuildCodeLibrary;
      finally
        Free;
      end;
    end
    else
      msgInfo( getMsgTranslation('MsgSelCategory', 'Please, select a category first :)') , Handle );
  end;
end;

procedure TNotesLibraryPanel.poDelete(Sender: TObject);
begin
  if not Assigned( tvLibrary.Selected ) then
    Exit;


  if MsgYesNo( getMsgTranslation('ConfirmDelete', 'Are you sure you want to delete it?!') , 0) <> IDYes then
    Exit;

  if tvLibrary.Selected.ImageIndex = NOTES_LIB_ITEM then
  begin
    if FileExists( GetCodeLibrarySelItemPath + NOTES_CODELIBRARY_EXTENSION ) then
    begin
      sysUtils.DeleteFile(GetCodeLibrarySelItemPath + NOTES_CODELIBRARY_EXTENSION);
      sysUtils.DeleteFile(GetCodeLibrarySelItemPath + NOTES_DESCRIPTION_EXT );
      tvLibrary.Items.Delete(tvLibrary.Selected);
    end;
  end else
  begin
    with TfrmCopyDel.Create(nil) do
    begin
      try
        operation:= foDel;
        Path:= addslash(GetCodeLibrarySelItemPath);
        Mask:='*';
        Description:= getMsgTranslation('DescrDeleting', 'Removing files. Please, wait...');
        theCaption:= getMsgTranslation('Captiondeleting', 'Removing category...');
        FilePrefix:= getMsgTranslation('PrefixDeleting', 'Removing');
        if Execute then
          RemoveDir(Addslash(GetCodeLibrarySelItemPath));
      finally
        Free;
      end;
    end;
    BuildCodeLibrary;
  end;
end;

procedure TNotesLibraryPanel.poEditItem(Sender: TObject);
begin
  if ( tvLibrary.Selected = nil ) or ( tvLibrary.Selected.ImageIndex <> NOTES_LIB_ITEM ) then
    Exit;
  with TfrmAddLibItem.Create(nil) do begin
    EditMode(GetCodeLibrarySelItemPath);
    try
      ShowModal;
      if ModalResult = MrOk then
        BuildCodeLibrary;
    finally
      Free;
    end;
  end;
end;

procedure TNotesLibraryPanel.poRenameFolder(Sender: TObject);
Var
  S: string;
begin
  if not Assigned( tvLibrary.Selected ) or ( tvLibrary.Selected.ImageIndex = NOTES_LIB_ITEM ) then
    Exit;

  S:= ExtractFileName( GetCodeLibrarySelItemPath );

  if not InputQuery(getMsgTranslation('CaptionRename', 'Rename category'), getMsgTranslation('DescrRename', 'New category name:'), S) then
    Exit;
  Screen.Cursor:= crHourGlass;
  try
    RenameFile( GetCodeLibrarySelItemPath, ExtractFilePath(GetCodeLibrarySelItemPath) + S );
    BuildCodeLibrary;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

function TNotesLibraryPanel.getPanelName: string;
begin
  Result:= 'Library';
end;

function TNotesLibraryPanel.PanelExec(const PanelAction,
  Params: string): string;
begin
  Result:= '';
  if SameText(PanelAction, 'insert') then
  begin

    tvLibraryDblClick(nil);

  end else if SameText(PanelAction, 'expand') then
  begin
    if tvLibrary.Items.Count > 0 then
      tvLibrary.Items.GetFirstNode.Expand(true);

  end else if SameText(PanelAction, 'collapse') then
  begin

    if tvLibrary.Items.Count > 0 then
      tvLibrary.Items.GetFirstNode.Collapse(true);

  end;

end;

function TNotesLibraryPanel.getFileType: String;
begin
  Result:= cbLibFileType.Text;
end;

procedure TNotesLibraryPanel.setFileType(const Value: String);
begin
  if cbLibFileType.Items.IndexOf(Value) > -1 then
  begin
    cbLibFileType.ItemIndex:= cbLibFileType.Items.IndexOf(Value);
    BuildCodeLibrary;
  end;
end;

initialization
  PanelReg.registerPanel(TNotesLibraryPanel);

end.
