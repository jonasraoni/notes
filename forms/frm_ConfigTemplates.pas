unit frm_ConfigTemplates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TfrmConfigTemplates = class(TForm)
    gbCategorys: TGroupBox;
    tvCat: TTreeView;
    Bevel4: TBevel;
    Bevel5: TBevel;
    btNewCat: TButton;
    btDelCat: TButton;
    btRenameCat: TButton;
    gbTemplates: TGroupBox;
    lbTemplates: TListBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btNew: TButton;
    btEdit: TButton;
    btEditDescr: TButton;
    btDelTemplate: TButton;
    btRename: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btNewCatClick(Sender: TObject);
    procedure tvCatChange(Sender: TObject; Node: TTreeNode);
    procedure btDelCatClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btEditDescrClick(Sender: TObject);
    procedure btDelTemplateClick(Sender: TObject);
    procedure btRenameCatClick(Sender: TObject);
    procedure btRenameClick(Sender: TObject);
  private
    fLastTemplatesPath: string;
    fLastSelNode: TTreeNode;
    procedure LoadTree;

  public
    { Public declarations }
  end;

var
  frmConfigTemplates: TfrmConfigTemplates;

implementation

{$R *.dfm}

uses NotesTemplates, NotesUtils, NotesCopyDelDialog,
  NotesEditorTabPosList, NotesGlobals;

{ TfrmConfigTemplates }

procedure TfrmConfigTemplates.LoadTree;
begin
  tvCat.SortType:= stNone	;
  tvCat.Items.BeginUpdate;
  Templates.ListTemplatesCategories(tvCat.Items, 19);
  tvCat.SortType:= stText;
  tvCat.Items.EndUpdate;
  tvCat.FullExpand;
end;

procedure TfrmConfigTemplates.FormCreate(Sender: TObject);
begin
  tvCat.Images:= NImages;
  LoadTree;
end;

procedure TfrmConfigTemplates.btNewCatClick(Sender: TObject);
var
  S: string;
begin
  if InputQuery('Nova categoria...', 'Digite o nome da nov categoria:', S) then
    if (S <> '') and (DirectoryExists(fLastTemplatesPath + S) = false) then
      SysUtils.ForceDirectories(fLastTemplatesPath + S);

  tvCat.Items.Clear;
  LoadTree;
end;

procedure TfrmConfigTemplates.tvCatChange(Sender: TObject;
  Node: TTreeNode);
Var
  SL: TStrings;
begin
  if Node = nil then Exit;

  btRenameCat.Enabled:= Node.Parent <> nil;
  btDelCat.Enabled:= Node.Parent <> nil;

  fLastTemplatesPath:= Templates.getNodePath(Node);
  fLastSelNode:= Node;

  SL:= Templates.ListTemplates(Node);
  try
    lbTemplates.Items.BeginUpdate;
    lbTemplates.Clear;
    lbTemplates.Items.AddStrings(SL);
    lbTemplates.Sorted:= true;
    lbTemplates.Items.EndUpdate;
    if lbTemplates.Count > 0 then
    begin
      lbTemplates.ItemIndex:= 0;
    end else
    begin
      if Node.Parent = nil then
      begin
        // A linguagem não tem nenhum template
        // vamos criar um em branco...
        StrToFile(fLastTemplatesPath + 'Em branco' + NOTES_TEMPLATE_EXT, '');
        StrToFile(fLastTemplatesPath + 'Em branco' + NOTES_DESCRIPTION_EXT, 'Novo arquivo '+ Node.Text +
        ' em branco. Template auto-gerado pelo Notes.');
        tvCatChange( Sender, Node );
      end;
    end;
  finally
    SL.Free;
  end;
end;


procedure TfrmConfigTemplates.btDelCatClick(Sender: TObject);
begin
  if not DirectoryExists(fLastTemplatesPath) then Exit;

  if NotesUtils.MsgYesNo('Você tem certeza que quer deletar esta categoria?', Handle) = IdNo then Exit;

  With TfrmCopyDel.Create(nil) do
  begin
    try
      TheCaption:= 'Deletando categoria...';
      Description:= 'Por favor, aguarde enquanto o Notes deleta a categoria.';
      FilePrefix:= 'Deletando template...';
      Path:= fLastTemplatesPath;
      Mask:= '*';
      Operation:= foDel;

      if Execute then
        SysUtils.RemoveDir(fLastTemplatesPath);
    finally
      Free;
    end;
  end;
  tvCat.Items.Clear;
  LoadTree;
end;


procedure TfrmConfigTemplates.btNewClick(Sender: TObject);
var
  S: string;
begin
  if (InputQuery('Novo template...', 'Digite o nome do novo template:', S)= false)
    or (S = '') then
      Exit;

  StrToFile(fLastTemplatesPath + S + NOTES_TEMPLATE_EXT, '');
  StrToFile(fLastTemplatesPath + S + NOTES_DESCRIPTION_EXT, 'Novo arquivo em branco.');
  tvCatChange( Sender, fLastSelNode );
end;

procedure TfrmConfigTemplates.btEditClick(Sender: TObject);
Var
  etp: TNotesEditorTabPos;
begin
  if lbTemplates.ItemIndex < 0 then Exit;

  etp.FileName:= fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_TEMPLATE_EXT;
  etp.Line:= 1;
  etp.Col:= 1;

  if not FileExists(etp.FileName) then Exit;

  GoToEditorTabPos(etp, false);
end;

procedure TfrmConfigTemplates.btEditDescrClick(Sender: TObject);
Var
  S: string;
begin
  if lbTemplates.ItemIndex < 0 then Exit;
  if not fileExists(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT) then
    StrToFile(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT, '');

  S:= FileToStr(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT);

  if InputQuery('Descrição do item', 'Edite a descrição do item e clique em OK.', S) then
    StrToFile(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT, S);
end;

procedure TfrmConfigTemplates.btDelTemplateClick(Sender: TObject);
begin
  if lbTemplates.ItemIndex < 0 then Exit;

  SysUtils.DeleteFile(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT);
  SysUtils.DeleteFile(fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_TEMPLATE_EXT);

  tvCatChange( Sender, fLastSelNode);
end;

procedure TfrmConfigTemplates.btRenameCatClick(Sender: TObject);
var
  S: string;
begin
  if not Assigned(fLastSelNode) then Exit;

  if fLastSelNode.Parent = nil then
  begin
    MsgOk('Renomear Categoria - erro', 'O Notes não pode renomear o tipo de arquivo, apenas as categorias podem ter o nome modificado.', handle);
    Exit;
  end;
  S:= fLastSelNode.Text;
  if not InputQuery('Renomear Categoria','Novo nome da categoria:', S) then
    Exit;
  fLastSelNode.Text:= S;
  if (length(fLastTemplatesPath) > 0) and (fLastTemplatesPath[length(fLastTemplatesPath)] = '\') then
    S:= AddSlash(ExtractFilePath(Copy(fLastTemplatesPath, 1, length(fLastTemplatesPath)-1)) + S)
  else
    S:= AddSlash(ExtractFilePath(fLastTemplatesPath) + S);
  SysUtils.RenameFile(fLastTemplatesPath, S);
  fLastTemplatesPath:= S;
end;

procedure TfrmConfigTemplates.btRenameClick(Sender: TObject);
var
  S: string;
begin
  if lbTemplates.ItemIndex < 0 then Exit;

  S:= lbTemplates.items[lbTemplates.ItemIndex];

  if not InputQuery('Renomear Template','Novo nome do template:', S) then
    Exit;

  if S = '' then Exit;

  SysUtils.RenameFile(fLastTemplatesPath +
    lbTemplates.items[lbTemplates.ItemIndex] + NOTES_TEMPLATE_EXT, fLastTemplatesPath +
    S + NOTES_TEMPLATE_EXT);
  SysUtils.RenameFile(fLastTemplatesPath +
    lbTemplates.items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT, fLastTemplatesPath +
    S + NOTES_DESCRIPTION_EXT);

  lbTemplates.items[lbTemplates.ItemIndex]:= S;
end;

end.
