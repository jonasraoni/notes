unit frm_ExtendedSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmExtSearch = class(TForm)
    Label1: TLabel;
    lbReplace: TLabel;
    btSearch: TButton;
    btSearchAll: TButton;
    btReplace: TButton;
    btReplaceALL: TButton;
    btClose: TButton;
    paSearchOptions: TPanel;
    gbSearchWhere: TGroupBox;
    rbText: TRadioButton;
    rbSelText: TRadioButton;
    rbAllOpenedFiles: TRadioButton;
    rbProjectFiles: TRadioButton;
    rbFolder: TRadioButton;
    chBack: TCheckBox;
    chCaseSensitive: TCheckBox;
    chPattern: TCheckBox;
    chWholeWords: TCheckBox;
    gbFolderSearch: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    cbFolder: TComboBox;
    cbMask: TComboBox;
    chScanSubFolders: TCheckBox;
    chIgnoreBinary: TCheckBox;
    btGetFolder: TButton;
    meSearchStr: TMemo;
    meReplaceStr: TMemo;
    procedure rbClicks(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btCloseClick(Sender: TObject);
  private
    fSearchInDir: boolean;
    procedure SetSearchInDir(Value: boolean);
  public

  end;

var
  frmExtSearch: TfrmExtSearch;

implementation

{$R *.dfm}


procedure TfrmExtSearch.rbClicks(Sender: TObject);
begin
  if Sender = rbFolder then
  begin
    SetSearchInDir(true);
  end else
  begin
    SetSearchInDir(false);  
  end;
end;

procedure TfrmExtSearch.SetSearchInDir(Value: boolean);
begin
  fSearchInDir:= value;

  if Value then
  begin
    Height:= 436;
    btSearch.Enabled:= false;
    btReplace.Enabled:= false;
  end else
  begin
    btSearch.Enabled:= true;
    btReplace.Enabled:= true;
    Height:= 316;
  end;

end;

procedure TfrmExtSearch.FormCreate(Sender: TObject);
begin
  SetSearchInDir(false);
end;

procedure TfrmExtSearch.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmExtSearch.btCloseClick(Sender: TObject);
begin
  Close;
end;

end.
