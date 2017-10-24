//
//    TfrmConfig - diálogo das preferências gerais.
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
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

(*
  @abstract(frm_config - diálogo de preferências do Notes)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Chame o form com "ShowModal". Se o usuário clicar em OK,
  retorna mrOK, senão, retorna mrCancel.
*)
unit frm_config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, bLink, CheckLst,
  NotesConfig, NotesFileTypeOptions, SynEditHighlighter, NotesTranslation;


type
  TfrmConfig = class(TForm)
    pgPages: TPageControl;
    tsGeneral: TTabSheet;
    gbEditorLook: TGroupBox;
    lFont: TLabel;
    cbFont: TComboBox;
    lSize: TLabel;
    cbSize: TComboBox;
    btCancel: TButton;
    btOK: TButton;
    tsSystem: TTabSheet;
    gbAssoc: TGroupBox;
    clAssoc: TCheckListBox;
    lChooseAssoc: TLabel;
    gbShellExt: TGroupBox;
    chShellExt: TCheckBox;
    colorDlg: TColorDialog;
    chShowGutter: TCheckBox;
    chLineNum: TCheckBox;
    lRightMargin: TLabel;
    edRightMargin: TEdit;
    tsColors: TTabSheet;
    gbSyntax: TGroupBox;
    cbColorFileType: TComboBox;
    lFileType: TLabel;
    lsTokens: TListBox;
    chBold: TCheckBox;
    chItalic: TCheckBox;
    chUnderline: TCheckBox;
    chStrikeOut: TCheckBox;
    shForeground: TShape;
    shBackground: TShape;
    btForeground: TButton;
    btBackground: TButton;
    gbOtherColors: TGroupBox;
    lSelToken: TLabel;
    shEditorColor: TShape;
    btEditorColor: TButton;
    shGutterTextColor: TShape;
    btGutterTextColor: TButton;
    shGutterColor: TShape;
    btGutterColor: TButton;
    shMarginColor: TShape;
    btMarginColor: TButton;
    shLineColor: TShape;
    btLineColor: TButton;
    shScopeColor: TShape;
    btScopeColor: TButton;
    tsEditor: TTabSheet;
    gbGeneralEditorOpt: TGroupBox;
    chSmartHome: TCheckBox;
    chSmartTab: TCheckBox;
    chDelTrailingSpaces: TCheckBox;
    chLineColStartAt0: TCheckBox;
    gbEditorSaveOptions: TGroupBox;
    lAutoSave: TLabel;
    lMin: TLabel;
    chBackupOnSave: TCheckBox;
    edAutoSaveInterval: TEdit;
    gbOtherOptions: TGroupBox;
    lDefaultFileType: TLabel;
    lAtInit: TLabel;
    chAllowMultipleInstances: TCheckBox;
    cbDefaultFileType: TComboBox;
    cbStartup: TComboBox;
    chRemeberFileInfo: TCheckBox;
    chOfficeXP: TCheckBox;
    gbEspecificEditorOpt: TGroupBox;
    lIndentSize: TLabel;
    edIdentSize: TEdit;
    edTabSize: TEdit;
    lTabSize: TLabel;
    chTabToSpaces: TCheckBox;
    rbAutoIndent: TRadioButton;
    rbSmartIndent: TRadioButton;
    rbNoAutoIndent: TRadioButton;
    lOnEnter: TLabel;
    chAllowCaretAfterEOL: TCheckBox;
    chShowSpecialChars: TCheckBox;
    shMatchingBraketsColor: TShape;
    btMatchingBraketsColor: TButton;
    lScope: TLabel;
    cbScope: TComboBox;
    chHighlightMatchingBrackets: TCheckBox;
    lLanguage: TLabel;
    cbLanguage: TComboBox;
    lOptionsFileType: TLabel;
    cbOptionsFileType: TComboBox;
    chAutoCloseTags: TCheckBox;
    lFilter: TLabel;
    edFilters: TEdit;
    chAutoCloseSymbols: TCheckBox;
    shErrorColor: TShape;
    btErrorColor: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure DoChangeShapeColor(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorButtonClick(Sender: TObject);
    procedure DoFileTypeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DoAttrChange(Sender: TObject);
    procedure lsTokensKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edFiltersChanged(Sender: TObject);
    procedure IntEditsKeyPress(Sender: TObject; var Key: Char);
    procedure intEdChange(Sender: TObject);
    procedure cbSizeChange(Sender: TObject);
  private
    fCurAttr: TSynHighlighterAttributes;
    fCurFileType: TNotesFileTypeOptions;
    fFileTypeMan: TNotesFileTypeOptionsMan;
    fFiltersChanged: boolean;

    fTranslate: TNotesTranslation;

    procedure SaveCurFileType;
    procedure SaveCurAttr;
  public
    { Public declarations }
  end;

var
  frmConfig: TfrmConfig;

implementation

{$R *.dfm}

uses
  frm_princ, NotesGlobals, NotesUtils;

procedure TfrmConfig.FormCreate(Sender: TObject);
Var
  I : integer;
  W,L : TSize;
  Fonts: TStringList;
  SaveCanvasFont: string;
  FileTypes: PNotesFolderTree;
begin
  screen.Cursor:= crHourGlass;

  fTranslate:= TNotesTranslation.Create;
  fTranslate.TranslationFile:= NProfile.getTranslationFileForModule(self.ClassName);
  fTranslate.TranslateComponent(self);
  self.Caption:= fTranslate.getMsgTranslation('FrmCaption', 'Preferences');

  fFiltersChanged:= false;

  try
    Fonts:= TStringList.Create;
    cbColorFileType.Items.BeginUpdate;
    cbDefaultFileType.Items.BeginUpdate;
    cbOptionsFileType.Items.BeginUpdate;

    NProfile.FileTypesMgr.ListTypes( FileTypes );
    for I := 0 to High( FileTypes^ ) do
      with FileTypes^[I] do
      begin
        cbColorFileType.items.Add( Path );
        cbDefaultFileType.Items.Add( Path );
        cbOptionsFileType.Items.Add( Path );
      end;
    FreeFolderTree( FileTypes );

    cbColorFileType.Sorted:= true;
    cbDefaultFileType.Sorted:= true;
    cbOptionsFileType.Sorted:= true;
    cbColorFileType.Items.EndUpdate;
    cbDefaultFileType.Items.EndUpdate;
    cbOptionsFileType.Items.EndUpdate;

    if cbColorFileType.Items.Count > 0 then
      cbColorFileType.ItemIndex:= 0;

    if cbDefaultFileType.Items.Count > 0 then
      cbDefaultFileType.ItemIndex:= 0;

    if cbOptionsFileType.Items.Count > 0 then
      cbOptionsFileType.ItemIndex:= 0;

    //adicionar fontes monoespaçadas
    try
      SaveCanvasFont:= Canvas.Font.Name;
      cbFont.Items.BeginUpdate;
      Fonts.AddStrings(Screen.Fonts);
      for I:= 0 to Pred( Fonts.Count ) do
      begin
        Canvas.Font.Name := Fonts[I];
        W := Canvas.TextExtent('m');
        L := Canvas.TextExtent('.');
        if W.cx = L.cx then
        begin
          cbFont.Items.Add( Fonts[I] );
        end;
      end;
    finally
      Fonts.Free;
      cbFont.Sorted := true;
      cbFont.Items.EndUpdate;
      Canvas.Font.Name:= SaveCanvasFont;
    end;

    if cbFont.Items.Count > 0 then
    begin
      cbFont.ItemIndex:= cbFont.Items.IndexOf(nProfile.Config.FontName);
      if cbFont.ItemIndex = -1 then
        cbFont.ItemIndex := 0;
    end;

    with NProfile.Config do
    begin
      cbSize.Text:= IntToStr( FontSize );
      chShowGutter.Checked:= ShowGutter;
      chLineNum.Checked:= ShowLinesInGutter;
      edrightMargin.Text:= IntToStr(EditorRightMargin);
      cbDefaultFileType.ItemIndex:= cbDefaultFileType.Items.IndexOf(DefaultFileType);
      chSmarthome.Checked:= SmartHome;
      chSmartTab.Checked:= SmartTabs;
      chLineColStartAt0.Checked:= LineAndColCountStartAt0;
      chDelTrailingSpaces.Checked:= TrimTrailingSpaces;
      chAllowMultipleInstances.Checked:= AllowMultipleInstances;
      chShellExt.Checked:= ShellExt;
      chOfficeXP.Checked:= OfficeXPTheme;
      chBackupOnSave.Checked:= BackupOnSave;
      chRemeberFileInfo.Checked:= (RemeberFileInfo > 0);
      chAllowCaretAfterEOL.Checked:= CursorAfterEOL;
      chShowSpecialChars.Checked:= ShowSpecialChars;
      chHighlightMatchingBrackets.Checked:= HighlightMatchingBrakets;
      edAutoSaveInterval.Text:= IntToStr(AutoSaveInterval);
      chAutoCloseSymbols.Checked:= AutoCloseSymbols;
    end;

    // Opções específicas
    fCurAttr:= nil;
    fCurFileType:= nil;
    fFileTypeMan:= TNotesFileTypeOptionsMan.Create(True);
    cbColorFileType.ItemIndex:= cbDefaultFileType.ItemIndex;
    cbOptionsFileType.ItemIndex:= cbDefaultFileType.ItemIndex;
    DoFileTypeChange(cbOptionsFileType);

    pgPages.ActivePageIndex:= 0;
  finally
    screen.Cursor:= crDefault;
  end;
end;

procedure TfrmConfig.btOKClick(Sender: TObject);
begin
  SaveCurFileType;
  fFileTypeMan.SaveAll;

  if fFiltersChanged then
    fFileTypeMan.CompileFileFilters;

  with NProfile.Config do
  begin
    FontName:= cbFont.Text;
    FontSize:= StrToInt(cbSize.Text);
    ShowGutter:= chShowGutter.Checked;
    ShowLinesInGutter:= chLineNum.Checked;
    EditorRightMargin:= StrToInt(edrightMargin.Text);
    DefaultFileType:= cbDefaultFileType.text;
    AllowMultipleInstances:= chAllowMultipleInstances.Checked;
    SmartHome:= chSmarthome.Checked;
    SmartTabs:= chSmartTab.Checked;
    LineAndColCountStartAt0:= chLineColStartAt0.Checked;
    TrimTrailingSpaces:= chDelTrailingSpaces.Checked;
    ShellExt:= chShellExt.Checked;

    OfficeXPTheme:= chOfficeXP.Checked;
    BackupOnSave:= chBackupOnSave.Checked;
    CursorAfterEOL:= chAllowCaretAfterEOL.Checked;
    ShowSpecialChars:= chShowSpecialChars.Checked;
    HighlightMatchingBrakets:= chHighlightMatchingBrackets.Checked;
    AutoSaveInterval:= StrToIntDef(edAutoSaveInterval.Text, 0);
    AutoCloseSymbols:= chAutoCloseSymbols.Checked;

    if chRemeberFileInfo.Checked then
    begin
      if RemeberFileInfo < 1 then
        RemeberFileInfo:= 30;
    end else
      RemeberFileInfo:= 0;

    Save;
  end;
  ModalResult:= mrOK;
end;

procedure TfrmConfig.DoChangeShapeColor(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  If Sender is TShape then
  begin
    ColorDlg.Color:= (Sender As TShape).Brush.Color;
    if ColorDlg.Execute then
      (Sender As TShape).Brush.Color:= ColorDlg.Color;
  end;
end;

procedure TfrmConfig.ColorButtonClick(Sender: TObject);
var
  C: TComponent;
begin
  if Sender is TButton then
  begin
    // O botão deve ser chamado de bt[Nome] e a shape de sh[Nome].
    // O [Nome] deve ser igual para os dois
    C:= self.FindComponent('sh'+ copy((sender As TComponent).Name, 3, length((sender As TComponent).Name)));
    if C = nil then Exit;
    if C is TShape then
    begin
      colorDlg.Color:= (C As TShape).Brush.Color;
      if colorDlg.Execute then
        (C As TShape).Brush.Color:= colorDlg.Color;
    end;
  end;
end;

procedure TfrmConfig.DoFileTypeChange(Sender: TObject);
begin
  SaveCurFileType;

  if Sender is TCombobox then
  begin
    // salvamos as opções do antigo

    if (Sender As TCombobox).ItemIndex < 0 then Exit;

    cbColorFileType.ItemIndex:= (Sender As TCombobox).ItemIndex;
    cbOptionsFileType.ItemIndex:= (Sender As TCombobox).ItemIndex;

    fcurFileType:= fFileTypeMan.getFileTypeOptions((Sender As TCombobox).Text);

    if fCurFileType = nil then Exit;

    edIdentSize.Text:=  IntToStr(fCurFileType.IdentSize);
    edTabSize.Text:= IntToStr(fCurFileType.TabSize);
    edFilters.Text:= fCurFileType.FileFilters;
    chTabToSpaces.Checked:= fCurFileType.TabToSpaces;
    chAutoCloseTags.Checked:= fCurFileType.AutoCloseTags;
    shEditorColor.Brush.Color:= fCurFileType.EditorColor;
    shGutterTextColor.Brush.Color:= fCurFileType.GutterTextColor;
    shGutterColor.Brush.Color:= fCurFileType.GutterColor;
    shLineColor.Brush.Color:= fCurFileType.CurrentLineColor;
    shMarginColor.Brush.Color:= fCurFileType.MarginColor;
    shScopeColor.Brush.Color:= fCurFileType.ScopeColor;
    shMatchingBraketsColor.Brush.Color:= fCurFileType.MatchingBracketsColor;

    Case fCurFileType.LineBreakAction of
      lbNone: rbNoAutoIndent.Checked:= true;
      lbAutoIndent: rbAutoIndent.Checked:= true;
      lbSmartIndent: rbSmartIndent.Checked:= true;
    end;

    fFileTypeMan.ListStyles(cbColorFileType.Text, lsTokens.Items);
    if lstokens.Items.Count > 0 then
      lsTokens.ItemIndex:= 0;
    DoAttrChange(nil);
  end;
end;

procedure TfrmConfig.FormDestroy(Sender: TObject);
begin
  fFileTypeMan.Free;
  fTranslate.Free;
end;

procedure TfrmConfig.SaveCurFileType;
begin
  if fCurFileType = nil then Exit;

  SaveCurAttr;

  fCurFileType.IdentSize:= StrToIntDef(edIdentSize.Text, 2);
  fCurFileType.TabSize:= StrToIntDef(edTabSize.Text, 8);
  fCurFileType.FileFilters:= edFilters.Text;
  fCurFileType.TabToSpaces:= chTabToSpaces.Checked;
  fCurFileType.AutoCloseTags:= chAutoCloseTags.Checked;
  fCurFileType.EditorColor:= shEditorColor.Brush.Color;
  fCurFileType.GutterTextColor:= shGutterTextColor.Brush.Color;
  fCurFileType.GutterColor:= shGutterColor.Brush.Color;
  fCurFileType.CurrentLineColor:= shLineColor.Brush.Color;
  fCurFileType.MarginColor:= shMarginColor.Brush.Color;
  fCurFileType.ScopeColor:= shScopeColor.Brush.Color;
  fCurFileType.MatchingBracketsColor:= shMatchingBraketsColor.Brush.Color;


  fCurFileType.LineBreakAction:= lbNone;
  if rbAutoIndent.Checked then
    fCurFileType.LineBreakAction:= lbAutoIndent;
  if rbSmartIndent.Checked then
    fCurFileType.LineBreakAction:= lbSmartIndent;

end;

procedure TfrmConfig.SaveCurAttr;
begin
  if fCurAttr = nil then Exit;

  fCurAttr.Foreground:= shForeground.Brush.Color;
  fCurAttr.Background:= shBackground.Brush.Color;

  fCurAttr.Style:= [];

  if chBold.Checked then
    fCurAttr.Style:= fCurAttr.Style + [fsBold];
  if chItalic.Checked then
    fCurAttr.Style:= fCurAttr.Style + [fsItalic];
  if chUnderline.Checked then
    fCurAttr.Style:= fCurAttr.Style + [fsUnderline];
  if chStrikeOut.Checked then
    fCurAttr.Style:= fCurAttr.Style + [fsStrikeOut];
end;

procedure TfrmConfig.DoAttrChange(Sender: TObject);
begin
  SaveCurAttr;

  if lsTokens.ItemIndex > -1 then
  begin
    fCurAttr:= fCurFileType.Styles.getStyle(lsTokens.Items[lsTokens.itemIndex]);
    if fCurAttr = nil then Exit;

    shForeground.Brush.Color:= fCurAttr.Foreground;
    shBackground.Brush.Color:= fCurAttr.Background;

    chBold.Checked:= (fsBold in fCurAttr.Style);
    chItalic.Checked:= (fsItalic in fCurAttr.Style);
    chUnderline.Checked:= (fsUnderline in fCurAttr.Style);
    chStrikeOut.Checked:= (fsStrikeOut in fCurAttr.Style);
  end;
end;

procedure TfrmConfig.lsTokensKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DoAttrChange(nil);
end;

procedure TfrmConfig.edFiltersChanged(Sender: TObject);
Var
  S: string;
begin
  fFiltersChanged:= true;

  S:= edFilters.Text;
  S:= StringReplace(S, '*', '', [rfReplaceAll]);
  S:= StringReplace(S, '?', '', [rfReplaceAll]);
  S:= StringReplace(S, '.', '', [rfReplaceAll]);
  S:= StringReplace(S, '/', '', [rfReplaceAll]);
  S:= StringReplace(S, '\', '', [rfReplaceAll]);
  S:= StringReplace(S, '"', '', [rfReplaceAll]);
  S:= StringReplace(S, '|', '', [rfReplaceAll]);
  S:= StringReplace(S, '>', '', [rfReplaceAll]);
  S:= StringReplace(S, '<', '', [rfReplaceAll]);

  if edFilters.Text <> S then
  begin
    edFilters.Text:= S;
    MsgInfo( Format(fTranslate.getMsgTranslation('MsgFilters', 'The filters must be like the exemple: %s.'), ['"txt;ini;log"'])   , handle);
  end;
end;

procedure TfrmConfig.IntEditsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [#8, '0'..'9'] then exit;
  Key := #0;
end;

procedure TfrmConfig.intEdChange(Sender: TObject);
begin
  If sender is TEdit then
  begin
    if (TEdit(sender).text = '') or (StrToIntDef(TEdit(sender).Text, -99) = -99) then
      TEdit(sender).text:= '0';
  end;
end;

procedure TfrmConfig.cbSizeChange(Sender: TObject);
begin
  if (cbSize.text = '') or (StrToIntDef(cbSize.Text, -99) = -99) then
    cbSize.text:= '0';
end;

end.
