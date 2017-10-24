//
//    frm_Search - diálogo de busca
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
  @abstract(frm_Search - diálogo de busca.)
  @author(Josimar Silva <josimar_me@yahoo.com.br>)
*)
unit frm_Search;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, NotesMRU, ComCtrls, NotesGlobals,
  RegExpr, NotesEditorTabPosList;

type
  // tipo de diálogo de busca
  TSearchDialogType = (sdNone, sdSearch, sdSearchDir, sdReplace, sdReplacedir);

  // Diálogo de busca/substituição de texto
  TfrmSearch = class(TForm)
    cbSearchStr: TComboBox;
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
    cbReplaceStr: TComboBox;
    gbFolderSearch: TGroupBox;
    cbFolder: TComboBox;
    cbMask: TComboBox;
    chScanSubFolders: TCheckBox;
    chIgnoreBinary: TCheckBox;
    gbOptions: TGroupBox;
    chWholeWords: TCheckBox;
    chWildCards: TCheckBox;
    chCaseSensitive: TCheckBox;
    chBack: TCheckBox;
    chRegExp: TCheckBox;
    btGrepHelp: TButton;
    btSearchFolder: TButton;
    lSearch: TStaticText;
    lbReplace: TStaticText;
    lDir: TStaticText;
    lMask: TStaticText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure rbClicks(Sender: TObject);
    procedure chClicks(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btReplaceClick(Sender: TObject);
    procedure btSearchAllClick(Sender: TObject);
    procedure btReplaceALLClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btGetFolderClick(Sender: TObject);
    procedure ItemPopupClick(Sender: TObject);
    procedure btGrepHelpClick(Sender: TObject);
  private
    fPopup        : TPopupMenu;
    fSearchRegex  : integer;
    fSearchDlgType: TSearchDialogType;
    fHist   : TNotesMRU;
    dHist   : TNotesMRU;
    eHist   : TNotesMRU;
    procedure setDlgType(const Value: TSearchDialogType);
  public
    property SearchDlgType: TSearchDialogType read fSearchDlgType write setDlgType;
  end;

var
  frmSearch: TfrmSearch;

implementation

{$R *.dfm}

uses NotesSearchReplace, NotesUtils, NotesEditorTab, frm_princ,
     NotesSearchPanel, NotesTabsManager;

{ TfrmSearch }

procedure TfrmSearch.setDlgType(const Value: TSearchDialogType);
begin
  fSearchDlgType := Value;

  Case Value of
    sdSearch:
    begin
      lbReplace.Visible:= false;
      cbReplaceStr.Visible:= false;
      btReplace.Visible:= false;
      btReplaceAll.Visible:= false;
      btSearch.Enabled:= true;
      paSearchOptions.Top:= 40;
      Height:= 204;
      btClose.Top:= 80;
      Caption:= 'Localizar...';
    end;
    sdSearchDir:
    begin
      lbReplace.Visible:= false;
      cbReplaceStr.Visible:= false;
      btReplace.Visible:= false;
      btReplaceAll.Visible:= false;
      btSearch.Enabled:= true;
      paSearchOptions.Top:= 40;
      gbFolderSearch.Top:= 186;
      btClose.Top:= 80;
      Height:= 330;
      Caption:= 'Localizar...';
    end;
    sdReplace:
    begin
      lbReplace.Visible:= true;
      cbReplaceStr.Visible:= true;
      btReplace.Visible:= true;
      btReplaceAll.Visible:= true;
      btSearch.Enabled:= true;
      btReplace.Enabled:= true;
      paSearchOptions.Top:= 64;
      Height:= 220;
      Caption:= 'Substituir...';
    end;
    sdReplaceDir:
    begin
      lbReplace.Visible:= true;
      cbReplaceStr.Visible:= true;
      btReplace.Visible:= true;
      btReplaceAll.Visible:= true;
      btSearch.Enabled:= false;
      btReplace.Enabled:= false;
      paSearchOptions.Top:= 64;
      Height:= 360;
      Caption:= 'Substituir...';
    end;
  end;
end;

procedure TfrmSearch.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fHist.SaveToFile(NProfile.Paths.UserDir + NOTES_SEARCHHIST_FILENAME);
  fHist.Free;
  dHist.SaveToFile(NProfile.Paths.UserDir + NOTES_GREPDIRHIST_FILENAME);
  dHist.Free;
  eHist.SaveToFile(NProfile.Paths.UserDir + NOTES_GREPEXTHIST_FILENAME);
  eHist.Free;
  Action:= caFree;
end;

procedure TfrmSearch.FormCreate(Sender: TObject);
var
  pi: TMenuItem;
begin
  fSearchRegex:= 0;
  fSearchDlgType:= sdNone;
  fHist:= TNotesMRU.Create;
  fHist.MaxItens:= 30;
  fHist.LoadFromFile(NProfile.Paths.UserDir + NOTES_SEARCHHIST_FILENAME);
  dHist:= TNotesMRU.Create;
  dHist.MaxItens:= 10;
  dHist.LoadFromFile(NProfile.Paths.UserDir + NOTES_GREPDIRHIST_FILENAME);
  eHist:= TNotesMRU.Create;
  eHist.MaxItens:= 10;
  eHist.LoadFromFile(NProfile.Paths.UserDir + NOTES_GREPEXTHIST_FILENAME);

  fPopup:= TPopupMenu.Create(self);
  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(self);
  pi.Tag     := 1;
  pi.Name    := 'mnEMail';
  pi.Caption := '&EMail';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 2;
  pi.Name    := 'mnURL';
  pi.Caption := '&URL';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 3;
  pi.Name    := 'mnIP';
  pi.Caption := '&IP';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 4;
  pi.Name    := 'mnTag';
  pi.Caption := '&Tag';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 5;
  pi.Name    := 'mnData';
  pi.Caption := '&Data';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 6;
  pi.Name    := 'mnNumeroInteiro';
  pi.Caption := '&Número inteiro';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(self);
  pi.Tag     := 7;
  pi.Name    := 'mnNumeroRomano';
  pi.Caption := '&Número romano';
  pi.OnClick := ItemPopupClick;
  fPopup.Items.Add(pi);

  btGrepHelp.PopupMenu:= fPopup;
end;

procedure TfrmSearch.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSearch.rbClicks(Sender: TObject);
begin
  chBack.Enabled := true;
  if Sender = rbFolder then
  begin
    btSearchAll.Enabled:= false;
    chBack.Enabled := false;
    if fSearchDlgType = sdSearch then
      SearchDlgType:= sdSearchDir
    else if fSearchDlgType = sdReplace then
      SearchDlgType:= sdReplaceDir;
  end
  else if (Sender = rbSelText) or (Sender = rbAllOpenedFiles) then
  begin
    btSearchAll.Enabled:= false;
    chBack.Checked := false;
    chBack.Enabled := false;
    SearchDlgType  := sdSearch;
  end
  else
  begin
    if fSearchDlgType = sdSearchDir then
      SearchDlgType:= sdSearch
    else if fSearchDlgType = sdReplaceDir then
      SearchDlgType:= sdReplace;
  end;
end;

procedure TfrmSearch.btSearchClick(Sender: TObject);
begin
  if cbSearchStr.Text = '' then
  begin
    MsgExclama('Você esqueceu de preencher o texto a ser buscado!', handle);
    if cbSearchStr.CanFocus then
      cbSearchStr.SetFocus;
    Exit;
  end;

  fHist.Add(cbSearchStr.Text);

  if (rbText.Checked) or (rbSelText.Checked) then
  begin
    SearchReplace.AutoSelect    := true;
    SearchReplace.InSelection   := rbSelText.Checked;
    SearchReplace.SearchStr     := cbSearchStr.Text;
    SearchReplace.CaseSensitive := chCaseSensitive.Checked;
    searchReplace.WholeWords    := chWholeWords.Checked;
    SearchReplace.WildCards     := chWildCards.Checked;
    SearchReplace.RegExp        := chRegExp.Checked;

    NotesSearchOptions          := SearchReplace.Options;

    if chRegExp.Checked then
    begin
      inc(fSearchRegex);
      SearchReplace.SearchRegex := fSearchRegex;
    end;

    if not SearchReplace.Search(chBack.Checked) then
      MsgOk('Não encontrado', 'Não foi possível encontrar "'+ cbSearchStr.Text + '" no texto.', handle);

    SearchReplace.Clear;
  end
  else if (rbFolder.Checked) then
  begin
    if not StrToBool(PanelGetProperty('Search', 'Running')) then
    begin

      if not DirectoryExists(cbFolder.Text) then
      begin
        MsgExclama('Por favor, escolha um diretório válido para fazer a busca!', handle);
        if cbFolder.CanFocus then
          cbFolder.SetFocus;
        Exit;
      end;

      dHist.Add(cbFolder.Text);
      eHist.Add(cbMask.Text);

      PanelSetProperty('Search', 'SearchStr', cbSearchStr.Text);
      PanelSetProperty('Search', 'DirName', cbFolder.Text);
      PanelSetProperty('Search', 'FileFilter', cbMask.Text);
      PanelSetProperty('Search', 'Where', 'dir');
      // opções de busca
      PanelSetProperty('Search', 'Regexpr', BoolToStr(chRegExp.Checked, true));
      PanelSetProperty('Search', 'WildCards', BoolToStr(chWildCards.Checked, true));
      PanelSetProperty('Search', 'WholeWords', BoolToStr(chWholeWords.Checked, true));
      PanelSetProperty('Search', 'CaseSensitive', BoolToStr(chCaseSensitive.Checked, true));
      PanelSetProperty('Search', 'Recursive', BoolToStr(chScanSubFolders.Checked, true));
      // Limpa os últimos resultados
      PanelExec('Search', 'Clear', '');
      //Executa
      PanelExec('Search', 'Search', '');
      Self.Close;
      Exit;
    end else
      MsgOk('Erro', 'Já existe uma busca sendo executada.', self.Handle);
  end
  else if (rbAllOpenedFiles.Checked) then
  begin
    if not StrToBool(PanelGetProperty('Search', 'Running')) then
    begin
      dHist.Add(cbFolder.Text);
      eHist.Add(cbMask.Text);

      PanelSetProperty('Search', 'SearchStr', cbSearchStr.Text);
      PanelSetProperty('Search', 'Where', 'openfiles');
      // opções de busca
      PanelSetProperty('Search', 'Regexpr', BoolToStr(chRegExp.Checked, true));
      PanelSetProperty('Search', 'WildCards', BoolToStr(chWildCards.Checked, true));
      PanelSetProperty('Search', 'WholeWords', BoolToStr(chWholeWords.Checked, true));
      PanelSetProperty('Search', 'CaseSensitive', BoolToStr(chCaseSensitive.Checked, true));
      // Limpa os últimos resultados
      PanelExec('Search', 'Clear', '');

      // Fechamos o form
      self.Close;

      //Executa
      PanelExec('Search', 'Search', '');
      Exit;
    end else
      MsgOk('Erro', 'Já existe uma busca sendo executada.', self.Handle);
  end;

  cbSearchStr.Items.Assign(fHist.List);
  cbFolder.Items.Assign(dHist.List);
  cbMask.Items.Assign(eHist.List);
end;

procedure TfrmSearch.btReplaceClick(Sender: TObject);
begin
  if cbSearchStr.Text = '' then Exit;
  fHist.Add(cbReplaceStr.Text);
  fHist.Add(cbSearchStr.Text);

  if (rbText.Checked) or (rbSelText.Checked) then
  begin
    SearchReplace.AutoSelect    := true;
    SearchReplace.ReplaceStr    := cbReplacestr.Text;
    SearchReplace.InSelection   := rbSelText.Checked;
    SearchReplace.SearchStr     := cbSearchStr.Text;
    SearchReplace.CaseSensitive := chCaseSensitive.Checked;
    searchReplace.WholeWords    := chWholeWords.Checked;
    SearchReplace.WildCards     := chWildCards.Checked;
    SearchReplace.RegExp        := chRegExp.Checked;

    if not SearchReplace.Replace(chBack.Checked) then
      MsgOk('Não encontrado', 'Não foi possível encontrar "'+ cbSearchStr.Text + '" no texto.', handle);

    SearchReplace.Clear;
  end;

  cbReplaceStr.Items.Assign(fHist.List);
  cbSearchStr.Items.Assign(fHist.List);
end;

procedure TfrmSearch.btSearchAllClick(Sender: TObject);
begin
  if cbSearchStr.Text = '' then Exit;
  fHist.Add(cbSearchStr.Text);

  PanelSetProperty('Search', 'SearchStr', cbSearchStr.Text);
  PanelSetProperty('Search', 'Where', 'editor');
  // opções de busca
  PanelSetProperty('Search', 'Regexpr', BoolToStr(chRegExp.Checked, true));
  PanelSetProperty('Search', 'WildCards', BoolToStr(chWildCards.Checked, true));
  PanelSetProperty('Search', 'WholeWords', BoolToStr(chWholeWords.Checked, true));
  PanelSetProperty('Search', 'CaseSensitive', BoolToStr(chCaseSensitive.Checked, true));
  // Limpa os últimos resultados
  PanelExec('Search', 'Clear', '');

  //Executa
  PanelExec('Search', 'Search', '');

  // Fechamos o form
  self.Close;
end;

procedure TfrmSearch.btReplaceALLClick(Sender: TObject);
Var
  I: integer;
begin
  if cbSearchStr.Text = '' then Exit;
  fHist.Add(cbReplaceStr.Text);
  fHist.Add(cbSearchStr.Text);

  if (rbText.Checked) or (rbSelText.Checked) then
  begin
    SearchReplace.AutoSelect:= true;
    SearchReplace.ReplaceStr:= cbReplacestr.Text;
    SearchReplace.InSelection:= rbSelText.Checked;
    SearchReplace.SearchStr:= cbSearchStr.Text;
    SearchReplace.CaseSensitive:= chCaseSensitive.Checked;
    searchReplace.WholeWords:= chWholeWords.Checked;
    SearchReplace.WildCards:= chWildCards.Checked;
    SearchReplace.RegExp:= chRegExp.Checked;

    I:= SearchReplace.ReplaceAll;

    if I < 1 then
      MsgOk('Não encontrado', 'Não foi possível encontrar "'+ cbSearchStr.Text + '" no texto.', handle)
    else
      MsgOk('Substituir Tudo', 'Foram substituídas '+ IntToStr(I) +' ocorrências.', handle);

    SearchReplace.Clear;
  end;


  cbReplaceStr.Items.Assign(fHist.List);
  cbSearchStr.Items.Assign(fHist.List);
end;

procedure TfrmSearch.FormShow(Sender: TObject);
begin
  if ActiveEditorTab = nil then Exit;

  cbReplaceStr.Items.Assign(fHist.List);
  cbSearchStr.Items.Assign(fHist.List);
  cbFolder.Items.Assign(dHist.List);
  cbMask.Items.Assign(eHist.List);

  if not ActiveEditorTab.Editor.SelAvail then
    cbSearchStr.Text:= ActiveEditorTab.Editor.WordAtCursor
  else
    cbSearchStr.Text:= ActiveEditorTab.Editor.SelText;

  if (cbSearchStr.Text = '') and (cbSearchStr.Items.Count > 0) then
    cbSearchStr.ItemIndex:= 0;
end;

procedure TfrmSearch.btGetFolderClick(Sender: TObject);
var
  Dir: string;
begin
  Try
    Dir := BrowseForFolder(Handle, 'Diretório onde a busca será realizada.');
  finally
    if Dir <> EmptyStr then
      cbFolder.Text := Dir;
  end;
end;


procedure TfrmSearch.chClicks(Sender: TObject);
begin
  chWildCards.Enabled  := (not chRegExp.Checked) and (not chBack.Checked);
  chWholeWords.Enabled := (not chRegExp.Checked);
  chBack.Enabled       := (not chRegExp.Checked);
end;

procedure TfrmSearch.ItemPopupClick(Sender: TObject);
  begin
    case (Sender as TMenuItem).Tag of
      1: cbSearchStr.Text := '[_a-zA-Z\d\-\.]+@([_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+)+)';
      2: cbSearchStr.Text := '(?i)(FTP|HTTP)://([_a-z\d\-]+(\.[_a-z\d\-]+)+)((/[_a-z\d\-\\\.]+)+)*';
      3: cbSearchStr.Text := '\b([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\b';
      4: cbSearchStr.Text := '<(?:[^>''"]*|".*?"|''.*?'')+>';
      5: cbSearchStr.Text := '(?(?=[^a-z]+[a-z])\d{2}-[a-z]{3}-\d{2}|\d{2}-\d{2}-\d{2})';
      6: cbSearchStr.Text := '\A-?(?(?=[1-9])\d*)\z';
      7: cbSearchStr.Text := '(?i)M*(D?C{0,3}|C[DM])(L?X{0,3}|X[LC])(V?I{0,3}|I[VX])';
    end;
  end;

procedure TfrmSearch.btGrepHelpClick(Sender: TObject);
var
  P : TPoint;
begin
  P:= btGrepHelp.ClientToScreen(Point(btGrepHelp.Width + -10, 10));
  fPopup.Popup(P.X, P.Y);
end;

end.
