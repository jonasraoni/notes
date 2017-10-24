unit frm_ProfileMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TfrmProfileManager = class(TForm)
    lActiveProfile: TLabel;
    lbProfiles: TListBox;
    btNew: TButton;
    btSetActive: TButton;
    btClose: TButton;
    Bevel1: TBevel;
    btDel: TButton;
    Bevel2: TBevel;
    lActiveProfileName: TLabel;
    lProfileDir: TLabel;
    edProfileDir: TEdit;
    procedure btCloseClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btSetActiveClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProfileManager: TfrmProfileManager;

implementation

{$R *.dfm}

uses NotesUtils, NotesGlobals, frm_NewProfile, NotesCopyDeldialog;

procedure TfrmProfileManager.btCloseClick(Sender: TObject);
begin
  if NProfile.ProfileExists(NProfile.getActiveProfile) then
    modalResult:= mrok
  else
    msgOk('Erro', 'Ative um profile existente. O Notes precisa ter um profile ativado para funcionar.', handle);
end;

procedure TfrmProfileManager.btNewClick(Sender: TObject);
Var
  S: string;
begin
  if Not InputQuery('Nome do novo profile', 'Digite o nome do profile a ser criado.', S) then
    Exit;
  // se o profile já existir, saímos
  if lbProfiles.Items.IndexOf(S) <> -1 then Exit;

  With TfrmNewProfile.Create(nil) do
  begin
    try
      NewProfileName:= S;
      if ShowModal = mrOk then
      begin
        lbProfiles.Items.Add(S);
        ShowMessage('O Profile "'+ S + '" foi criado com sucesso.');
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmProfileManager.btSetActiveClick(Sender: TObject);
begin
  if lbProfiles.itemIndex < 0 then Exit;

  screen.Cursor:= crHourGlass;
  try
    if NProfile.SetProfile(lbProfiles.Items[lbProfiles.itemIndex]) then
    begin
      StrTofile( addSlash(NProfilesPath) + NOTES_ACTIVE_PROFILE_FILENAME , lbProfiles.Items[lbProfiles.itemIndex]);
      msgOk('Profile Ativado', 'O profile "' +lbProfiles.Items[lbProfiles.itemIndex]+ '" foi ativado. Algumas opções só serão modificadas de acordo com ele após você reiniciar o Notes.', handle);
    end;
  finally
    screen.Cursor:= crDefault;
  end;

  lActiveProfileName.Caption := NProfile.getActiveProfile;
  edProfileDir.Text:= NProfile.Paths.UserDir;

end;
  //

procedure TfrmProfileManager.btDelClick(Sender: TObject);
begin
  if lbProfiles.itemIndex < 0 then Exit;

  if lbProfiles.Count < 2 then
  begin
    MsgOk('Erro', 'O Notes precisa de ao menos um profile para poder funcionar. Crie um novo profile antes de deletar este profile.', handle);
    Exit;
  end;

  if lbProfiles.Items[lbProfiles.itemIndex] = lActiveProfileName.Caption then
  begin
    MsgOk('Erro', 'O Notes não pode deletar o profile ativo. Ative outro profile antes de deletar este profile.', handle);
    Exit;
  end;

  if MsgYesNo('Você tem certeza que quer deletar este profile?', handle) = IdNo then
    Exit;

  with TfrmCopyDel.Create(nil) do
  begin
    try
      operation:= foDel;
      DisableCancelButton:= true;
      Path:= addSlash(NProfilesPath + lbProfiles.Items[lbProfiles.itemIndex]);
      Mask:='*';
      Description:= 'O Notes está deletando os arquivos do profile. Por favor, aguarde.';
      theCaption:= 'Deletando profile...';
      FilePrefix:= 'Deletando';
      if Execute then
        RemoveDir(addSlash(NProfilesPath + lbProfiles.Items[lbProfiles.itemIndex]));
    finally
      Free;
      lbProfiles.DeleteSelected;
    end;
  end;

  lActiveProfileName.Caption := NProfile.getActiveProfile;
  edProfileDir.Text:= NProfile.Paths.UserDir;

end;

procedure TfrmProfileManager.FormShow(Sender: TObject);
var
  tree: PNotesFolderTree;
  I: integer;
begin
  buildFolderTree(Tree, NProfilesPath, ftOnlyfolders);
  try
    for I:= 0 to high(Tree^) do
      lbProfiles.items.Add(Tree^[I].Path);
    lbProfiles.Sorted:= true;
  finally
    FreeFolderTree(Tree);
  end;

  lActiveProfileName.Caption:= NProfile.getActiveProfile;
  edProfileDir.Text:= NProfile.Paths.UserDir;
end;

end.
