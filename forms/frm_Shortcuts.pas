unit frm_Shortcuts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ActnList, NotesKeymaps;

type
  TfrmShortcuts = class(TForm)
    gbKeymaps: TGroupBox;
    cbKeymaps: TComboBox;
    btSaveKeymap: TButton;
    lbKeymaps: TLabel;
    gbCurrentKeymap: TGroupBox;
    lsCategorys: TListBox;
    lbCategory: TLabel;
    lbActions: TLabel;
    lvActions: TListView;
    lsShortcuts: TListBox;
    lbActionShortcuts: TLabel;
    btAddShortcut: TButton;
    btEditShortCut: TButton;
    btRemoveShortcut: TButton;
    btOk: TButton;
    btCancel: TButton;
    procedure lsCategorysClick(Sender: TObject);
    procedure lvActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddShortcutClick(Sender: TObject);
    procedure btEditShortCutClick(Sender: TObject);
    procedure btRemoveShortcutClick(Sender: TObject);
    procedure btOkClick(Sender: TObject);
    procedure btSaveKeymapClick(Sender: TObject);
    procedure cbKeymapsChange(Sender: TObject);
  private
    fActionList: TActionList;
    fFilter: string;
    fKeymap: TNotesKeymap;
    fKI: TNotesKeymapItem;
    procedure ReloadActionList;
  public
    procedure setActionList(const List: TActionList);
  end;

var
  frmShortcuts: TfrmShortcuts;

implementation

{$R *.dfm}

uses NotesGlobals, frm_EditShortcut, Menus, NotesUtils;

{ TfrmShortcuts }

procedure TfrmShortcuts.ReloadActionList;
Var
  I: integer;
  LI: TListItem;
  S: string;
begin
  lvActions.Clear;
  for I:= 0 to fActionList.ActionCount -1 do
  begin
    if (SameText(fFilter, fActionList.Actions[I].Category)) then
    begin
      LI:= lvActions.Items.Add;
      if fActionList.Actions[I] is TAction then
      begin
        S:= TAction(fActionList.Actions[I]).Caption;
        S:= StringReplace(S, '&', '', []);
        Li.Caption:= S;
        Li.ImageIndex:= TAction(fActionList.Actions[I]).ImageIndex;
        Li.SubItems.Add(TAction(fActionList.Actions[I]).Hint);
        Li.SubItems.Add(IntToStr(TAction(fActionList.Actions[I]).Tag));
      end;
    end;
  end;
end;

procedure TfrmShortcuts.setActionList(const List: TActionList);
begin
  lvActions.SmallImages:= List.Images;
  fActionList:= List;
  fFilter:= 'File';
  lsCategorys.ItemIndex:= 0;
  ReloadActionList;
end;


procedure TfrmShortcuts.lsCategorysClick(Sender: TObject);
begin
  if lsCategorys.ItemIndex = 0 then
    fFilter:= 'File'
  else if lsCategorys.ItemIndex = 1 then
    fFilter:= 'Edit'
  else if lsCategorys.ItemIndex = 2 then
    fFilter:= 'Search'
  else if lsCategorys.ItemIndex = 3 then
    fFilter:= 'View'
  else if lsCategorys.ItemIndex = 4 then
    fFilter:= 'Project'
  else if lsCategorys.ItemIndex = 5 then
    fFilter:= 'Run'
  else if lsCategorys.ItemIndex = 6 then
    fFilter:= 'Macros'
  else if lsCategorys.ItemIndex = 7 then
    fFilter:= 'Commands'
  else if lsCategorys.ItemIndex = 8 then
    fFilter:= 'Options'
  else if lsCategorys.ItemIndex = 9 then
    fFilter:= 'Help'
  else if lsCategorys.ItemIndex = 10 then
    fFilter:= 'Editor'
  else
    fFilter:='';

  lsShortcuts.Clear;

  ReloadActionList;
end;

procedure TfrmShortcuts.lvActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
Var
  I, iTag: integer;
begin
  lsShortcuts.Clear;
  if Item.SubItems.Count = 2 then
  begin
    iTag:= StrToIntDef(Item.SubItems.Strings[1], 0);
    if iTag <> 0 then
    begin

      for I:= 0 to fActionList.ActionCount - 1 do
      begin
        if fActionList.Actions[I].Tag = iTag then
        begin
          if fActionList.Actions[I] is TAction then
            fKI:= fKeymap.getItemByAction(fActionList.Actions[I] As TAction);
          Break;
        end;
      end;

      if fKI <> nil then
        fKI.AssignToTStrings(lsShortcuts.Items);

    end;
  end;
  if lsShortcuts.Count > 0 then
    lsShortcuts.ItemIndex:= 0;
end;

procedure TfrmShortcuts.FormCreate(Sender: TObject);
Var
  I: integer;
  F: PNotesFolderTree;
begin
  fKeymap:= TNotesKeymap.Create;
  if fileExists(nProfile.Paths.KeymapFile) then
    fKeymap.LoadKeymap(nProfile.Paths.KeymapFile);

  BuildFolderTree(F, nProfile.Paths.KeymapsDir, ftOnlyFiles);
  try
    cbKeymaps.Items.BeginUpdate;
    for I := 0 to High( F^ ) do
      cbKeymaps.Items.Add( DelFileExt(F^[I].Path) );
  finally
    cbKeymaps.Items.EndUpdate;
    FreeFolderTree(F);
  end;

  if cbKeymaps.Items.Count > 0 then
    cbKeymaps.ItemIndex:= 0;
end;

procedure TfrmShortcuts.FormDestroy(Sender: TObject);
begin
  fKeymap.Free;
end;

procedure TfrmShortcuts.btAddShortcutClick(Sender: TObject);
Var
  sc: TShortcut;
  S: string;
  I, iTag: integer;
begin
  sc:= 0;
  if (ShowEditShortcutDialog(sc, fActionList)) and (sc <> 0) then
  begin
    S:= ShortcutToText(sc);
    if lsShortcuts.Items.IndexOf(S) < 0 then
    begin
      if fKI = nil then
      begin
        if lvActions.Selected = nil then Exit;
        if lvActions.Selected.SubItems.Count <> 2 then Exit;
        iTag:= strToIntDef(lvActions.Selected.SubItems.Strings[1], 0);
        if iTag < 1 then Exit;
        fKI:= TNotesKeymapItem.Create;

        for I:= 0 to fActionList.ActionCount - 1 do
        begin
          if fActionList.Actions[I].Tag = iTag then
          begin
            if fActionList.Actions[I] is TAction then
              fKI.setActionAssoc(fActionList.Actions[I] As TAction );
            Break;
          end;
        end;

        fKeymap.AddKeymapItem(fKI)
      end;
      fKI.addShortcut(sc);
      lsShortcuts.Items.Add(S);
    end;
  end;
  if (lsShortcuts.Count > 0) and (lsShortcuts.ItemIndex < 0) then
    lsShortcuts.ItemIndex:= lsShortcuts.Count-1;
end;

procedure TfrmShortcuts.btEditShortCutClick(Sender: TObject);
var
  sc: TShortcut;
begin
  if (fKI <> nil) and (lsShortcuts.ItemIndex <> -1 ) then
  begin
    sc:= TextToShortcut(lsShortcuts.Items.Strings[lsShortcuts.ItemIndex]);
    // Mostramos o editor de shortcuts
    if (ShowEditShortcutDialog(sc, fActionList)) then
    begin
      if sc <> 0 then
        lsShortcuts.Items.Strings[lsShortcuts.ItemIndex]:= ShortCutToText(sc)
      else
        lsShortcuts.DeleteSelected;

      fKI.AssignFromTStrings(lsShortcuts.Items);
    end;
  end;
  if (lsShortcuts.Count > 0) and (lsShortcuts.ItemIndex < 0) then
    lsShortcuts.ItemIndex:= 0;
end;

procedure TfrmShortcuts.btRemoveShortcutClick(Sender: TObject);
begin
  if fKI <> nil then
  begin
    if lsShortcuts.ItemIndex > -1 then
    begin
      lsShortcuts.DeleteSelected;
      fKI.AssignFromTStrings(lsShortcuts.Items);
    end;
  end;
  if (lsShortcuts.Count > 0) and (lsShortcuts.ItemIndex < 0) then
    lsShortcuts.ItemIndex:= 0;
end;

procedure TfrmShortcuts.btOkClick(Sender: TObject);
begin
  fKeymap.SaveKeymap(nProfile.Paths.KeymapFile);
  fKeymap.AssignToActionList(fActionList);
  ModalResult:= mrOK;
end;

procedure TfrmShortcuts.btSaveKeymapClick(Sender: TObject);
var
  S: string;
begin
  if InputQuery('Nome do esquema', 'Digite o nome que você quer dar ao esquema de teclas de atalho', S) then
  begin
    if S = '' then Exit;
    ForceDirectories(nProfile.Paths.KeymapsDir);
    fKeymap.SaveKeymap(nProfile.Paths.KeymapsDir + S + '.nkm');
    if cbKeymaps.Items.IndexOf(S) < 0 then
      cbKeymaps.Items.Add(S);
  end;
end;

procedure TfrmShortcuts.cbKeymapsChange(Sender: TObject);
Var
  S: string;
  Save: string;
begin
  if cbKeymaps.ItemIndex = 0 then Exit;
  case MsgQuest('Você quer salvar as teclas de atalho atuais antes de aplicar o esquema de teclas de atalho "'+cbKeymaps.Text+'"?  ', Handle) of
    IDYes:
      if InputQuery('Nome do esquema', 'Digite o nome que você quer dar ao esquema de teclas de atalho', S) then
      begin
        Save:= cbKeymaps.Text;
        if S = '' then Exit;
        ForceDirectories(nProfile.Paths.KeymapsDir);
        fKeymap.SaveKeymap(nProfile.Paths.KeymapsDir + S + '.nkm');
        if cbKeymaps.Items.IndexOf(S) < 0 then
          cbKeymaps.Items.Add(S);
        fKeymap.LoadKeymap(nProfile.Paths.KeymapsDir + Save + '.nkm');
      end;

    IDNo: fKeymap.LoadKeymap(nProfile.Paths.KeymapsDir + cbKeymaps.Text + '.nkm');

  end;
end;

end.
