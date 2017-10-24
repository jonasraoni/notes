unit frm_FolderMenuEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ActnList, Menus, ExtCtrls;

type
  TEditItemProc = procedure(const FileName: string; var ReloadItems: boolean) of object;

type
  TfrmFolderMenuEditor = class(TForm)
    tvItems: TTreeView;
    btAdd: TButton;
    btAddFolder: TButton;
    btEdit: TButton;
    btShortcut: TButton;
    btDel: TButton;
    btClose: TButton;
    btRename: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    procedure btCloseClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btAddFolderClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btRenameClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btShortcutClick(Sender: TObject);
  private
    fExt: string;
    fFolder: string;
    fEditItem: TEditItemProc;
    fRoot: string;
    fAction: TActionList;
    fCreateItem: TEditItemProc;
    procedure LoadItems;
    function getSelItemPath: string;
  public
    property RootNodeName: string read fRoot write fRoot;
    property ItemsFolder: string read fFolder write fFolder;
    property ItemsExtension: string read fExt write fExt;
    property KeymapActionList: TActionList read fAction write fAction;
    property OnEditItem: TEditItemProc read fEditItem write fEditItem;
    property OnCreateItem: TEditItemProc read fCreateItem write fCreateItem;
  end;

var
  frmFolderMenuEditor: TfrmFolderMenuEditor;

implementation

{$R *.dfm}

uses NotesUtils, NotesCopyDelDialog, NotesGlobals,
  frm_EditShortcut, NotesFolderMenus;

const
  ITEM_IMG= 42;
  FOLDER_IMG= 19;


procedure TfrmFolderMenuEditor.btCloseClick(Sender: TObject);
begin
  modalResult:= mrOK;
end;

function TfrmFolderMenuEditor.getSelItemPath: string;
var
  Node: TTreeNode;
  S: string;
begin
  Node := tvItems.Selected;
  Result := AddSlash( fFolder );
  if Node = nil then Exit;
  if Node.Parent = nil then Exit;

  S := Node.Text;
  while (Node.Level <> 0) and (Node.Parent.Parent <> nil) do
  begin
    S := AddSlash( Node.Parent.Text ) + S;
    Node := Node.Parent;
  end;

  // se for arquivo
  if tvItems.Selected.ImageIndex = ITEM_IMG then
    Result := Result + S + fExt
  else
    Result := addslash(Result + S);
end;

procedure TfrmFolderMenuEditor.LoadItems;
var
  Tree: PNotesFolderTree;
  RootNode: TTreeNode;

  procedure AddNodes( Tree: PNotesFolderTree; RefNode: TTreeNode = nil );
  const
    Icons: array[Boolean] of Integer = ( ITEM_IMG, FOLDER_IMG );
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
          Node := tvItems.Items.AddChild( RefNode, Path );
          AddNodes( Tree^[I].Items, Node );
        end
        else
          Node := tvItems.Items.AddChild( RefNode, GetFileName( Path ) );

        Node.ImageIndex := Icons[ IsFolder ];
        Node.SelectedIndex := Icons[ IsFolder ];
        Node.Data := Pointer( Integer( IsFolder ) );
      end;
    end;
  end;

begin
  if (DirectoryExists(fFolder)) and (fExt <> '') then
  begin
    if fExt[1] <> '.' then
      fExt:= '.' + fExt;

    screen.Cursor:= crHourGlass;
    tvItems.Items.BeginUpdate;
    tvItems.Items.Clear;

    BuildFolderTree(Tree, fFolder, ftAll, True, '*', '*'+ fExt);
    try
      RootNode:= tvItems.Items.AddChild(nil, '['+ fRoot +']');
      RootNode.ImageIndex:= FOLDER_IMG;
      RootNode.SelectedIndex := FOLDER_IMG;
      AddNodes( Tree, RootNode );
      RootNode.Expand(True);
    finally
      FreeFolderTree( Tree );
      tvItems.Items.EndUpdate;
      screen.Cursor:= crDefault;
    end;
  end;
end;

procedure TfrmFolderMenuEditor.btAddClick(Sender: TObject);
var
  Name, FlName,PaTxt: string;
  B: boolean;
begin
  if not InputQuery( 'Novo Item', 'Nome do novo item:', Name ) or ( Name = '' ) then
    Exit;

  if (tvItems.Selected <> nil) and (tvItems.Selected.ImageIndex = ITEM_IMG) then
    FlName:= addSlash(ExtractFilePath(getSelItemPath)) + Name + fExt
  else
    FlName:= getSelItemPath + Name + fExt;

  PaTxt:='';
  if (tvItems.Selected <> nil ) then
  begin
    if (tvItems.Selected.ImageIndex = ITEM_IMG) and (tvItems.Selected.Parent <> nil) then
      paTxt:= tvItems.Selected.Parent.Text
    else
      paTxt:= tvItems.Selected.Text;
  end;

  B:= true;

  if assigned(fCreateItem) then
    fCreateItem(FlName, B)
  else if assigned(fEditItem) then
  begin
    // criamos o arquivo
    NotesUtils.StrToFile(FlName, '');
    fEditItem(FlName, B);
  end;

  if B then
  begin
    LoadItems;
    SelectTreeItem(tvItems, Name, PaTxt);
  end;
end;


procedure TfrmFolderMenuEditor.btAddFolderClick(Sender: TObject);
var
  Name, NewDir, PaTxt: string;
begin
  if not InputQuery( 'Nova Categoria', 'Nome da nova categoria:', Name ) or ( Name = '' ) then
    Exit;

  if (tvItems.Selected <> nil) and (tvItems.Selected.ImageIndex = ITEM_IMG) then
    NewDir:= addslash(addSlash(ExtractFilePath(getSelItemPath)) + Name)
  else
    NewDir:= addslash(getSelItemPath + Name) ;

  PaTxt:='';
  if (tvItems.Selected <> nil ) then
  begin
    if (tvItems.Selected.ImageIndex = ITEM_IMG) and (tvItems.Selected.Parent <> nil) then
      paTxt:= tvItems.Selected.Parent.Text
    else
      paTxt:= tvItems.Selected.Text;
  end;

  ForceDirectories(NewDir);
  LoadItems;
  SelectTreeItem(tvITems, Name, PaTxt);
end;


procedure TfrmFolderMenuEditor.btEditClick(Sender: TObject);
var
  Name, FlName, paTxt: string;
  B: boolean;
begin
  if (tvItems.Selected = nil) or (tvItems.Selected.ImageIndex <> ITEM_IMG) then
    Exit;

  if (tvItems.Selected.Parent <> nil) then
    paTxt:= tvItems.Selected.Parent.Text
  else
    paTxt:='';

  Name:= tvItems.Selected.Text;

  FlName:= getSelItemPath;
  if not FileExists(FlName) then Exit;

  B:= true;

  if assigned(fEditItem) then
    fEditItem(FlName, B);

  if B then
  begin
    LoadItems;
    SelectTreeItem(tvItems, Name, paTxt);
  end;
end;

procedure TfrmFolderMenuEditor.btRenameClick(Sender: TObject);
var
  Name, NewName, paTxt, OldName: string;
begin
  if tvItems.Selected = nil then Exit;
  oldName:= getSelItemPath;
  if oldName = '' then Exit;
  if SameText(fFolder, oldName) then Exit;

  if not InputQuery( 'Renomear', 'Novo nome:', Name ) or ( Name = '' ) then
    Exit;

  if (oldName[ length(oldName) ] = '\') or (oldName[ length(oldName) ] = '/') then
    NewName:= addslash(ExtractFilePath(Copy(oldName, 1, length(oldName) -1))) + Name
  else if tvItems.Selected.ImageIndex <> ITEM_IMG then
    NewName:= addslash(ExtractFilePath(oldName)) + Name
  else
    NewName:= addslash(ExtractFilePath(oldName)) + Name + fExt;

  if FileExists(oldName) or DirectoryExists(oldName) then
  begin
    RenameFile(oldName, NewName);

    PaTxt:='';
    if (tvItems.Selected <> nil ) then
    begin
      if (tvItems.Selected.ImageIndex = ITEM_IMG) and (tvItems.Selected.Parent <> nil) then
        paTxt:= tvItems.Selected.Parent.Text
      else
        paTxt:= tvItems.Selected.Text;
    end;

    LoadItems;
    SelectTreeItem(tvItems, Name, paTxt);
  end;
end;

procedure TfrmFolderMenuEditor.btDelClick(Sender: TObject);
Var
  TheName: string;
begin
  if tvItems.Selected = nil then Exit;
  TheName:= getSelItemPath;
  if TheName = '' then Exit;
  if SameText(fFolder, TheName) then Exit;

  if MsgYesNo('Você tem certeza que quer deletar?', self.Handle) <> IDYes then
    Exit;

  if tvItems.Selected.ImageIndex = ITEM_IMG then
  begin
    sysUtils.DeleteFile( TheName );
    tvItems.Items.Delete(tvItems.Selected);
  end else
  begin
    with TfrmCopyDel.Create(nil) do
    begin
      try
        operation:= foDel;
        Path:= TheName;
        Mask:='*';
        Description:= 'Removendo categoria, aguarde...';
        theCaption:= 'Removendo...';
        FilePrefix:= 'Deletando';
        if Execute then
          RemoveDir(TheName);
      finally
        Free;
      end;
    end;
    LoadItems;
  end;
end;

procedure TfrmFolderMenuEditor.FormShow(Sender: TObject);
begin
  tvItems.Images:= NImages;
  LoadItems;
end;

procedure TfrmFolderMenuEditor.btShortcutClick(Sender: TObject);
var
  Shortc: TShortcut;
begin
  if (tvItems.Selected <> nil) and (tvItems.Selected.ImageIndex = ITEM_IMG) then
  begin
    Shortc:= folderMenusKeymap.getShortcutForFile(getSelItemPath);
    if ShowEditShortcutDialog(Shortc, fAction) then
    begin
      folderMenusKeymap.setShortcutForFile(getSelItemPath, Shortc);
      folderMenusKeymap.Save;
    end;
  end;
end;

end.
