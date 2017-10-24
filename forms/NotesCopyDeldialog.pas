unit NotesCopyDeldialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, NotesUtils, Notesglobals, NotesTimers;

type
  TNotesFolderTreeOperation = (foUnknown, foCopy, foDel);

  // Diálogo para copiar/deletar árvores de arquivos.
  TfrmCopyDel = class(TForm)
    Gauge: TProgressBar;
    btCancel: TButton;
    lDescr: TLabel;
    lFile: TLabel;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fFilePrefix: string;
    fAbort: boolean;
    fDescription: string;
    fCaption: string;
    fMask: string;
    fPath: string;
    fToPath: string;
    fOperation: TNotesFolderTreeOperation;
    fOverwrite: boolean;
    procedure CDCallBack(const CurrentPath: string;
     const CurrentItem: integer; var Abort: boolean);
    procedure DoActions;
    procedure setCaption(const Value: string);
    procedure setDescription(const Value: string);
    procedure setDisableCancel(const Value: boolean);
    function getDisableCancel: boolean;
  public
    function Execute: boolean;
    // Caption do form
    property TheCaption: string read fCaption write setCaption;
    // Uma descrição das ações q estão sendo feitas
    property Description: string read fDescription write setDescription;
    // Um prefixo que será mostrado antes do nome
    // do arquivo. Exemplo: "[Prefixo] C:\tmp\meutxt.dat".
    property FilePrefix: string read fFilePrefix write fFilePrefix;
    // Caminho dos arquivos a serem deletados/copiados
    property Path: string read fPath write fPath;
    // Caminho para onde serão copiados os arquivos no caso da operação ser de copiar
    property ToPath: string read fToPath write fToPath;
    // Máscara de arquivos a serem deletados/copiados.
    property Mask: string read fMask write fMask;
    // Sobrescrever arquivos já existentes com o mesmo nome?!
    property OverwriteOldFiles: boolean read fOverwrite write fOverwrite;
    // Operação. Sempre precisa ser setada, começa com foUnknown.
    property Operation: TNotesFolderTreeOperation read fOperation write fOperation;
    // Permite desabilitar o botão de cancelar
    property DisableCancelButton: boolean read getDisableCancel write setDisableCancel;
  end;

var
  frmCopyDel: TfrmCopyDel;

implementation

{$R *.dfm}

{ TfrmCopyDel }

procedure TfrmCopyDel.btCancelClick(Sender: TObject);
begin
  if MsgYesNo('Você tem certeza que deseja cancelar esta operação?', 0) = IDYes then
    fAbort:= true;
end;

procedure TfrmCopyDel.FormCreate(Sender: TObject);
begin
  fAbort:= false;
//  if Assigned(NotesMenu) then
//    NotesMenu.InitComponent(Self);
end;

procedure TfrmCopyDel.CDCallBack(const CurrentPath: string;
  const CurrentItem: integer; var Abort: boolean);
begin
  Application.ProcessMessages;
  gauge.Position:= CurrentItem;
  Abort:= fAbort;
  lFile.Caption:= ffilePrefix + ' ' + CurrentPath;
  Application.ProcessMessages;
end;

procedure TfrmCopyDel.setCaption(const Value: string);
begin
  fCaption := Value;
  Caption:= Value;
end;

procedure TfrmCopyDel.setDescription(const Value: string);
begin
  fDescription := Value;
  lDescr.Caption:= Value;
end;

function TfrmCopyDel.Execute: boolean;
begin
  Result:= false;
  if ShowModal = mrOk then
    Result:= true;
end;

procedure TfrmCopyDel.FormShow(Sender: TObject);
begin
  // para q o diálogo seja mostrado antes
  timers.setTimeOut(100, self.DoActions);
end;

procedure TfrmCopyDel.DoActions;
Var
  Tree: PNotesFolderTree;
  fTotal: integer;
begin
  ftotal:= 0;
  BuildFolderTree(Tree, fPath, ftAll, True, fMask, fMask);

  try
    CountFolderTree(Tree, fTotal);
    gauge.Min:= 0;
    gauge.Max:= fTotal;

    if ftotal > 0 then
    begin
      if fOperation = foDel then
        DeleteFolderTree(Tree, fPath, CDCallBack)
      else if fOperation = foCopy then
        CopyfolderTree(Tree, fPath, fToPath, fOverwrite, CDCallBack);
    end;

  finally

    FreeFolderTree(Tree);

    if fAbort then
      ModalResult:= mrAbort
    else
      ModalResult:= mrOk;
  end;
end;

procedure TfrmCopyDel.setDisableCancel(const Value: boolean);
begin
  btCancel.Enabled:= not Value;
end;

function TfrmCopyDel.getDisableCancel: boolean;
begin
  Result:= not btCancel.Enabled;
end;

end.
