unit NotesVerificationPanel;

interface

uses Classes, NotesPanels, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Graphics, NotesEditorTab, NotesGlobals, Menus, NotesProfile, SmallTidyWraper;

type
  TNotesVerificationPanel = class(TNotesPanel)
  private
    fRunning: boolean;
    fTidy: TTidy;

    lvOutput: TListView;
    fPopup: TPopupMenu;

    fLastFile: string;

    // Captura de erros
    procedure getErrorReport( sender:tObject; level:TidyReportLevel; line,col:cardinal;
      msg: ansistring; var bWriteOut:Boolean);

    // Items do menu
    procedure DoGoToError(Sender: TObject);
    procedure DoCopyLine(Sender: TObject);
    procedure DoCopyAll(Sender: TObject);
    procedure DoClear(Sender: TObject);
    // mostra número de erros / warnings
    procedure addStatistic;
  public
    constructor Create(const StartUpHost: TNotesPanelHost); override;
    destructor  Destroy; override;
    function    getPanelName: string; override;
    procedure   Resize; override;
    procedure   Initialize; override;
    procedure   ReadOptions(const S: string); override;
    procedure   WriteOptions(var  S: string); override;
    property    Running: boolean read fRunning;
  public
    function  getHTML: string;
    function  getXML: string;
    function  getXHTML: string;
  published
    function PanelExec(const PanelAction, Params: string): string; override;
  end;

implementation

uses Windows, ShellApi, Clipbrd, NotesXML, SysUtils,
  NotesUtils, NotesEditorTabPosList, forms;

const
  LV_ERROR_ICO = 40;
  LV_ALERT_ICO = 41;
  LV_INFO_ICO =  39;


{ TNotesVerificationPanel }

constructor TNotesVerificationPanel.Create;
var
  pi: TMenuItem;
begin
  inherited;
  lvOutput:= TListView.Create(compsOwner);

  With lvOutput do
  begin
    HotTrackStyles := [htHandPoint, htUnderlineHot];
    IconOptions.Arrangement:= iaLeft;
    RowSelect := True;
    SmallImages:= NImages;
    TabOrder := 0;
    ViewStyle := vsReport;

    with Columns.Add do begin
      AutoSize := True;
      Caption := getMsgTranslation('ColMessage', 'Messages');
    end;
    with Columns.Add do begin
      Caption := getMsgTranslation('ColLine', 'Line');
      Width := 70;
    end;
    with Columns.Add do begin
      Caption := getMsgTranslation('ColColumn', 'Column');
      Width := 60;
    end;

    OnDblClick:= DoGoToError;
    Parent:= self.Parent;
  end;

  fPopup:= TPopupMenu.Create(compsOwner);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnGoToLine';
  pi.Caption:= '&Ir para a linha';
  pi.OnClick:= DoGoToError;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyLine';
  pi.Caption:= '&Copiar mensagem';
  pi.OnClick:= DoCopyLine;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyAllLines';
  pi.Caption:= 'Copiar &todas as mensagens';
  pi.OnClick:= DoCopyAll;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnClearAll';
  pi.Caption:= '&Limpar mensagens';
  pi.OnClick:= DoClear;
  fPopup.Items.Add(pi);

  lvOutput.PopupMenu:= fPopup;

  fTidy:= TTidy.Create;
  fTidy.OnReport:= getErrorReport;
end;

destructor TNotesVerificationPanel.Destroy;
begin
  fTidy.Free;
  inherited;
end;

procedure TNotesVerificationPanel.DoClear(Sender: TObject);
begin
  lvOutput.Clear;
end;

procedure TNotesVerificationPanel.DoCopyAll(Sender: TObject);
Var
  S: string;
  I: integer;
  l_Error, l_Warning, l_Info: string;
begin
  // Pegamos as strings localizadas
  l_Error:= getMsgTranslation('MsgError', 'Error');
  l_Warning:= getMsgTranslation('MsgWarning', 'Warning');
  l_Info:= getMsgTranslation('MsgInfo', 'Information');

  for I:= 0  to lvOutput.Items.Count-1 do
  begin
    if lvOutput.Items[I].ImageIndex = LV_ERROR_ICO then
      S:= S+ #13+ l_Error +' ('
    else if lvOutput.Items[I].ImageIndex = LV_ALERT_ICO then
      S:= S+ #13+ l_Warning + ' ('
    else
      S:= S+ #13+ l_Info + ' (';

    if lvOutput.Items[I].SubItems.Count = 2 then
    begin
      S:= S + lvOutput.Items[I].SubItems.Strings[0] + ',';
      S:= S + lvOutput.Items[I].SubItems.Strings[1] + '): ';
    end;

    S:= S + lvOutput.Items[I].Caption;
  end;

  Clipboard.SetTextBuf(PChar(S));
end;

procedure TNotesVerificationPanel.DoCopyLine(Sender: TObject);
Var
  S: string;
begin
  if lvOutput.ItemIndex > -1 then
  begin
    if lvOutput.Items[lvOutput.ItemIndex].ImageIndex = LV_ERROR_ICO then
      S:= getMsgTranslation('MsgError', 'Error') + ' ('
    else if lvOutput.Items[lvOutput.ItemIndex].ImageIndex = LV_ALERT_ICO then
      S:= getMsgTranslation('MsgWarning', 'Warning') + ' ('
    else
      S:= getMsgTranslation('MsgInfo', 'Information') + ' (';

    if lvOutput.Items[lvOutput.ItemIndex].SubItems.Count = 2 then
    begin
      S:= S + lvOutput.Items[lvOutput.ItemIndex].SubItems.Strings[0] + ',';
      S:= S + lvOutput.Items[lvOutput.ItemIndex].SubItems.Strings[1] + '): ';
    end;

    S:= S + lvOutput.Items[lvOutput.ItemIndex].Caption;
    Clipboard.SetTextBuf(PChar(S));
  end;
end;

procedure TNotesVerificationPanel.DoGoToError(Sender: TObject);
Var
  etp: TNotesEditorTabPos;
begin
  if (lvOutput.ItemIndex > -1) and (ActiveEditorTab <> nil) then
    if lvOutput.Items[lvOutput.ItemIndex].SubItems.Count = 2 then
    begin
      etp.Line:= StrToIntDef(lvOutput.Items[lvOutput.ItemIndex].SubItems.Strings[0], 1);
      etp.Col:= StrToIntDef(lvOutput.Items[lvOutput.ItemIndex].SubItems.Strings[1], 1);
      etp.FileName:= fLastFile;
      GoToEditorTabPos(etp, true);
    end;
end;

procedure TNotesVerificationPanel.getErrorReport(sender: tObject;
  level: TidyReportLevel; line, col: cardinal; msg: ansistring;
  var bWriteOut: Boolean);
Var
  I: integer;
  LI: TListItem;
  aline, acol: integer;
  c: cardinal;
begin
  case level of
    TidyWarning, TidyAccess: I:= LV_ALERT_ICO;
    TidyError, TidyBadDocument, TidyFatal: I:= LV_ERROR_ICO;
  else
    I:= LV_INFO_ICO;
  end;

  c:= High(integer);
  aline:= 0;
  acol:= 0;
  if line < c then
    aline:= line;
  if line < c then
    acol:= col;

  LI:= lvOutput.Items.Add;
  LI.Caption:= msg;
  LI.ImageIndex:= I;
  LI.SubItems.Add(IntToStr(aline));
  LI.SubItems.Add(IntToStr(acol));

  if fLastFile <> '' then
    ProblemsList.Add(aline, acol, fLastFile, msg);
end;

function TNotesVerificationPanel.getHTML: string;
begin
  if ActiveEditorTab = nil then Exit;

  DoClear(compsOwner);
  ProblemsList.Clear;

  fLastFile:= ActiveEditorTab.FullPath;

  fTidy.ParseString(ActiveEditorTab.Editor.Text);

  try
    screen.Cursor:= crHourGlass;
    lvOutput.Items.BeginUpdate;
    fTidy.Reset;
    fTidy.Mark:= false;
    fTidy.IndentContent:= TidyTrue;
    fTidy.Doctype:= 'auto';
    Result:= fTidy.HTML;
    addStatistic;
  finally
    lvOutput.Items.EndUpdate;
    screen.Cursor:= crDefault;
  end;

end;

function TNotesVerificationPanel.getXHTML: string;
begin
  if ActiveEditorTab = nil then Exit;

  DoClear(compsOwner);
  ProblemsList.Clear;

  fLastFile:= ActiveEditorTab.FullPath;

  fTidy.ParseString(ActiveEditorTab.Editor.Text);

  try
    screen.Cursor:= crHourGlass;
    lvOutput.Items.BeginUpdate;
    fTidy.Reset;
    fTidy.Mark:= false;
    fTidy.XmlDecl:= true;
    fTidy.Doctype:= 'auto';
    fTidy.IndentContent:= TidyTrue;
    Result:= fTidy.XHTML;
    addStatistic;
  finally
    lvOutput.Items.EndUpdate;
    screen.Cursor:= crDefault;
  end;
end;

function TNotesVerificationPanel.getXML: string;
begin
  if ActiveEditorTab = nil then Exit;

  DoClear(compsOwner);
  ProblemsList.Clear;

  fLastFile:= ActiveEditorTab.FullPath;

  fTidy.ParseString(ActiveEditorTab.Editor.Text);

  try
    screen.Cursor:= crHourGlass;
    lvOutput.Items.BeginUpdate;
    fTidy.Reset;
    fTidy.Mark:= false;
    fTidy.XmlDecl:= true;
    fTidy.XmlTags:= true;
    fTidy.XmlPIs:= true;
    fTidy.XmlSpace:= false;
    fTidy.IndentContent:= TidyTrue;
    fTidy.IndentCdata:= false;
    Result:= fTidy.XML;
    addStatistic;
  finally
    lvOutput.Items.EndUpdate;
    screen.Cursor:= crDefault;
  end;
end;


procedure TNotesVerificationPanel.Resize;
begin
  inherited;
  lvOutput.Left:= 2;
  lvOutput.Top:= 2;
  lvOutput.Width:= lvOutput.Parent.Width - 4;
  lvOutput.Height:= lvOutput.Parent.Height - 4;
end;

procedure TNotesVerificationPanel.addStatistic;
var
  LI: TListItem;
begin
  LI:= TListItem.Create(lvOutput.Items);
  LI.ImageIndex:= LV_INFO_ICO;
  LI.SubItems.Add('0');
  LI.SubItems.Add('0');
  lvOutput.Items.AddItem(LI, 0);

  LI.Caption:= Format(getMsgTranslation('MsgErrorsFound', 'Found %d errors, %d warnings, %d acessibility warnings in %s'),
  [fTidy.ErrorCount, fTidy.WarningCount, fTidy.AccessWarningCount, fLastFile]);

  if ActiveEditorTab <> nil then
    ActiveEditorTab.Editor.InvalidateGutter;
end;

procedure TNotesVerificationPanel.WriteOptions(var S: string);
begin
  inherited;
  //
end;

procedure TNotesVerificationPanel.Initialize;
begin
  inherited;
  //
end;

procedure TNotesVerificationPanel.ReadOptions(const S: string);
begin
  inherited;
  //
end;

function TNotesVerificationPanel.getPanelName: string;
begin
  Result:= 'Verification';
end;


function TNotesVerificationPanel.PanelExec(const PanelAction,
  Params: string): string;
begin
  if SameText(PanelAction, 'FixHTML') then
    Result:= self.getHTML
  else if SameText(PanelAction, 'FixXHTML') then
    Result:= self.getXHTML
  else if SameText(PanelAction, 'FixXML') then
    Result:= self.getXML
  else
    Result:='';
end;

initialization
  PanelReg.registerPanel(TNotesVerificationPanel);


end.
