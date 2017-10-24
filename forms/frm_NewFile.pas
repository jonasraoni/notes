unit frm_NewFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList;

type
  TfrmNewFile = class(TForm)
    lbTemplates: TListBox;
    meHelp: TMemo;
    Bevel1: TBevel;
    btOK: TButton;
    btCancel: TButton;
    tvCat: TTreeView;
    lCategory: TLabel;
    lTemplates: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure lbTemplatesClick(Sender: TObject);
    procedure tvCatChange(Sender: TObject; Node: TTreeNode);
    procedure lbTemplatesDblClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fLastTemplatesPath: string;
    fSelTemplate: string;
    fSelFileType: string;
  public
    property SelectedTemplate: string read fSelTemplate;
    property SelectedFileType: string read fSelFileType write fSelFileType;
  end;

var
  frmNewFile: TfrmNewFile;


implementation

uses NotesTemplates, NotesGlobals, NotesUtils;


{$R *.dfm}

procedure TfrmNewFile.FormCreate(Sender: TObject);
begin
  screen.Cursor:= crHourGlass;
  try
    tvCat.Images:= NImages;
    tvCat.SortType:= stNone  ;
    tvCat.Items.BeginUpdate;
    Templates.ListTemplatesCategories(tvCat.Items, 19);
    tvCat.SortType:= stText;
    tvCat.Items.EndUpdate;
    tvCat.FullExpand;
  finally
    screen.Cursor:= crDefault;
  end;
end;

procedure TfrmNewFile.lbTemplatesClick(Sender: TObject);
Var
  S: string;
begin
  if lbTemplates.ItemIndex < 0 then Exit;
  S:= fLastTemplatesPath + lbTemplates.Items[lbTemplates.ItemIndex] + NOTES_DESCRIPTION_EXT;
  if FileExists(S) then
    meHelp.Lines.LoadFromFile(S)
  else
    meHelp.Clear;
end;

procedure TfrmNewFile.tvCatChange(Sender: TObject; Node: TTreeNode);
Var
  SL: TStrings;
begin
  if Node = nil then Exit;

  fLastTemplatesPath:= Templates.getNodePath(Node);

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
      lbTemplatesClick(Sender);
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

procedure TfrmNewFile.lbTemplatesDblClick(Sender: TObject);
begin
  btOKClick(Sender);
end;

procedure TfrmNewFile.btOKClick(Sender: TObject);
begin
  if lbTemplates.ItemIndex < 0 then Exit;

  fSelTemplate:= fLastTemplatesPath + lbtemplates.Items[lbTemplates.ItemIndex] + NOTES_TEMPLATE_EXT;
  fSelFileType:= Templates.getTemplateFileType(fSelTemplate);
  modalResult:= mrOK;
end;

procedure TfrmNewFile.FormShow(Sender: TObject);
Var
  I: integer;
begin
  // selecionamos o FileType q está em  fSelFileType
  for I:= 0 to tvCat.Items.Count - 1 do
  begin
    if SameText( tvCat.Items.Item[I].Text, fSelFileType ) then
    begin
      tvCat.Selected:= tvCat.Items.Item[I];
      Exit;
    end;
  end;

  screen.Cursor:= crDefault;
end;

end.