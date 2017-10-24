unit frm_Installer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ShlObj, ActiveX, ComObj;

type
  TfrmInstaller = class(TForm)
    pgWiz: TPageControl;
    tsStart: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Shape1: TShape;
    tsGPL: TTabSheet;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Shape2: TShape;
    Label4: TLabel;
    Label7: TLabel;
    Bevel2: TBevel;
    chGPL: TCheckBox;
    tsTasks: TTabSheet;
    Shape3: TShape;
    Label8: TLabel;
    Label9: TLabel;
    Bevel3: TBevel;
    chShellExt: TCheckBox;
    Label10: TLabel;
    tsEnd: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Bevel4: TBevel;
    Shape4: TShape;
    Label13: TLabel;
    Label14: TLabel;
    btGoToLicense: TButton;
    btExitOk: TButton;
    btExitOk2: TButton;
    btGotoNewProfile: TButton;
    btExit: TButton;
    btGotoEnd: TButton;
    btEnd: TButton;
    meGPL: TMemo;
    procedure btGoToLicenseClick(Sender: TObject);
    procedure chGPLClick(Sender: TObject);
    procedure btGotoNewProfileClick(Sender: TObject);
    procedure btExitOkClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
    procedure btGotoEndClick(Sender: TObject);
    procedure tsStartShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInstaller: TfrmInstaller;

implementation

{$R *.dfm}

uses frm_NewProfile, Notesglobals, NotesCopyDelDialog, NotesUtils, ShellAPI;

procedure TfrmInstaller.btGoToLicenseClick(Sender: TObject);
begin
  pgWiz.ActivePage:= tsGPL;
end;

procedure TfrmInstaller.chGPLClick(Sender: TObject);
begin
  btGotoNewProfile.Enabled:= chGPL.Checked;
end;

procedure TfrmInstaller.btGotoNewProfileClick(Sender: TObject);
Var
  Abort: boolean;
  I: integer;
  S: string;
begin
  Abort:= false;
  // cria os diretórios do Notes
  ForceDirectories(NProfilesPath);

  if DirectoryExists(addSlash(NProfilesPath) + 'default') then
  begin
    for I:= 2 to 1000 do
    begin
      if not DirectoryExists(addSlash(NProfilesPath) + 'default' + IntToStr(I)) then
      begin
        S:= 'default' + intToStr(I);
        Break;
      end;
    end;
  end else
    S:= 'default';

  With TfrmNewProfile.Create(nil) do
  begin
    try
      NewProfileName:= S;
      if ShowModal <> mrOk then
        Abort:= true;
    finally
      Free;
    end;
  end;

  if Abort then
  begin
    with TfrmCopyDel.Create(nil) do
    begin
      try
        operation:= foDel;
        DisableCancelButton:= true;
        Path:= NDataPath;
        Mask:='*';
        Description:= 'O Notes está limpando os arquivos que já haviam sido criados. Por favor, aguarde.';
        theCaption:= 'Cancelando...';
        FilePrefix:= 'Deletando';
        if Execute then
          RemoveDir(NDataPath);
      finally
        Free;
      end;
    end;
    modalResult:= mrCancel;
    Exit;
  end;

  // Setamos o profile que criamos como o profile ativo
  StrTofile( addSlash(NProfilesPath) + NOTES_ACTIVE_PROFILE_FILENAME , S);
  pgWiz.ActivePage:= tsTasks;
end;

procedure TfrmInstaller.btExitOkClick(Sender: TObject);
begin
  if msgYesNo('Você tem certeza que quer cancelar?!', handle) = IdNo then Exit;
  modalResult:= mrCancel;
end;

procedure TfrmInstaller.btExitClick(Sender: TObject);
begin
  if msgYesNo('Você tem certeza que quer cancelar?!', handle) = IdNo then Exit;
  // teremos que limpar toda a estrutura criada
  with TfrmCopyDel.Create(nil) do
  begin
    try
      operation:= foDel;
      DisableCancelButton:= true;
      Path:= NDataPath;
      Mask:='*';
      Description:= 'O Notes está limpando os arquivos que já haviam sido criados. Por favor, aguarde.';
      theCaption:= 'Cancelando...';
      FilePrefix:= 'Deletando';
      if Execute then
        RemoveDir(NDataPath);
    finally
      Free;
    end;
  end;
  modalResult:= mrCancel;
end;

procedure TfrmInstaller.btGotoEndClick(Sender: TObject);
begin
  if chShellExt.Checked then
    ShellExecute(0, 'open', 'regsvr32', PChar('/s "'+ NExePath + 'NotesShellExt.dll"'), '', SW_HIDE);

  pgWiz.ActivePage:= tsEnd;
end;

procedure TfrmInstaller.tsStartShow(Sender: TObject);
begin
  pgWiz.ActivePageIndex:= 0;
end;

end.
