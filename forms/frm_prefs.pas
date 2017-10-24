//
//    TfrmPrefs - diálogo das preferências gerais.
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
  @abstract(frm_prefs - diálogo de preferências do Notes)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Chame o form com "ShowModal". Se o usuário clicar cancel,
  retorna mrNo se algum FileType tiver sido modificado e
  mrNotoAll se o usuário cancelar e não tiver feito nenhum
  tipo de modificação. Se o usuário clicar em OK, retorna
  mrOK.
*)

unit frm_prefs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, CheckLst, NotesConfig;

type
  {Diálogo das preferências gerais do Notes.}  
  TfrmPrefs = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    btOK: TButton;
    btCancel: TButton;
    TabSheet1: TTabSheet;
    tsGeral: TTabSheet;
    chBackup: TCheckBox;
    Bevel6: TBevel;
    Label8: TLabel;
    cbDefaultFileType: TComboBox;
    chBackupDir: TCheckBox;
    chShellExt: TCheckBox;
    tsLook: TTabSheet;
    cbFont: TComboBox;
    cbSize: TComboBox;
    spBgColor: TShape;
    Label4: TLabel;
    spSelColor: TShape;
    Label3: TLabel;
    spSelTextColor: TShape;
    Label5: TLabel;
    Bevel2: TBevel;
    chShowgutter: TCheckBox;
    chLineNum: TCheckBox;
    spGutterColor: TShape;
    Label6: TLabel;
    Label9: TLabel;
    edRightMargin: TEdit;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Bevel5: TBevel;
    chToolbarToolTips: TCheckBox;
    chShowPath: TCheckBox;
    Label10: TLabel;
    cbControlsStyle: TComboBox;
    Bevel7: TBevel;
    Bevel3: TBevel;
    chCopyAlwaysOn: TCheckBox;
    chLineColStartAt0: TCheckBox;
    chTrailingSpaces: TCheckBox;
    tsLang: TTabSheet;
    Bevel10: TBevel;
    lbFileTypes: TListBox;
    btEditFileType: TButton;
    Label15: TLabel;
    Bevel8: TBevel;
    chlbAssoc: TCheckListBox;
    Label7: TLabel;
    Bevel9: TBevel;
    Label12: TLabel;
    chSmartHome: TCheckBox;
    chSmartTabs: TCheckBox;
    chAlwaysHidePainels: TCheckBox;
    chLibSync: TCheckBox;
    chAutoHideStartPage: TCheckBox;
    chAutoExpandLibrary: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btEditFileTypeClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    FileTypeChanged: Boolean;
  public
    { Public declarations }
  end;

var
  frmPrefs: TfrmPrefs;

implementation

{$R *.dfm}

uses
  frm_princ, NotesGlobals, NotesUtils;

procedure TfrmPrefs.FormCreate(Sender: TObject);
Var
  I : integer;
  W,L : TSize;
  Fonts: TStringList;
  SaveCanvasFont: string;
  FileTypes: PNotesFolderTree;
begin
  Fonts:= TStringList.Create;
  lbFileTypes.Items.BeginUpdate;
  cbDefaultFileType.Items.BeginUpdate;

  NProfile.FileTypesMgr.ListTypes( FileTypes );
  for I := 0 to High( FileTypes^ ) do
    with FileTypes^[I] do begin
      lbFileTypes.items.Add( Path );
      cbDefaultFileType.Items.Add( Path );
    end;
  FreeFolderTree( FileTypes );

  lbFileTypes.Sorted:= true;
  cbDefaultFileType.Sorted:= true;
  lbFileTypes.Items.EndUpdate;
  cbDefaultFileType.Items.EndUpdate;

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
        cbFont.Items.Add( Fonts[I] );
    end;
  finally
    Fonts.Free;
    cbFont.Sorted := true;
    cbFont.Items.EndUpdate;
    Canvas.Font.Name:= SaveCanvasFont;
  end;

  if cbFont.Items.Count > 0 then
    cbFont.ItemIndex := 0;

  with NProfile.Config do begin
    cbFont.ItemIndex:= cbFont.Items.IndexOf( FontName );
    cbSize.Text:= IntToStr( FontSize );
    spBgColor.Brush.Color:= BgColor;
    spSelcolor.Brush.Color:= SelColor;
    spSelTextColor.Brush.Color:= SelTextColor;
    chShowGutter.Checked:= ShowGutter;
    chLineNum.Checked:= ShowLinesInGutter;
    sPGutterColor.Brush.Color:= GutterColor;
    edrightMargin.Text:= IntToStr(EditorRightMargin);
    chtoolbarTooltips.Checked:= ShowToolbarToolTips;
    chShowPath.Checked:= ShowPathInMenus;
    chAlwaysHidePainels.Checked:= HidePanelsAtStart;
    chLibsync.Checked:= SincronyzeLibrary;
    chAutoExpandLibrary.Checked:= AutoExpandLibrary;
    chAutoHideStartPage.Checked:= AutoHideStartPage;
    cbDefaultFileType.ItemIndex:= cbDefaultFileType.Items.IndexOf(DefaultFileType);

    chSmarthome.Checked:= SmartHome;
    chSmartTabs.Checked:= SmartTabs;
    chCopyAlwaysOn.Checked:= CopyWithNoSel;
    chLineColStartAt0.Checked:= LineAndColCountStartAt0;
    chTrailingSpaces.Checked:= TrimTrailingSpaces;
    if Backup <> bsNoBackup then begin
      chBackup.Checked:= true;
      chBackupDir.Enabled:= true;
      chBackupDir.Checked := Backup = bsSaveInBackupDir;
    end
    else begin
      chBackup.Checked:= false;
      chBackupDir.Checked:= false;
      chBackupDir.Enabled:= false;
    end;
  end;
end;

procedure TfrmPrefs.btEditFileTypeClick(Sender: TObject);
begin
  FileTypeChanged := (lbFileTypes.ItemIndex > 0) and NProfile.FileTypesMgr.EditFileType( lbFileTypes.Items[lbFiletypes.ItemIndex] ) or FileTypeChanged;
end;

procedure TfrmPrefs.btOKClick(Sender: TObject);
begin
  with NProfile.Config do begin
    FontName:= cbFont.Text;
    FontSize:= StrToInt(cbSize.Text);
    BgColor:= spBgColor.Brush.Color;
    SelColor:= spSelcolor.Brush.Color;
    SelTextColor:= spSelTextColor.Brush.Color;
    ShowGutter:= chShowGutter.Checked;
    ShowLinesInGutter:= chLineNum.Checked;
    GutterColor:= sPGutterColor.Brush.Color;
    EditorRightMargin:= StrToInt(edrightMargin.Text);
    ShowToolbarToolTips:= chtoolbarTooltips.Checked;
    ShowPathInMenus:= chShowPath.Checked;
    HidePanelsAtStart:= chAlwaysHidePainels.Checked;
    SincronyzeLibrary:= chLibsync.Checked;
    AutoExpandLibrary:= chAutoExpandLibrary.Checked;
    AutoHideStartPage:= chAutoHideStartPage.Checked;
    DefaultFileType:= cbDefaultFileType.text;

    SmartHome:= chSmarthome.Checked;
    SmartTabs:= chSmartTabs.Checked;
    CopyWithNoSel:= chCopyAlwaysOn.Checked;
    LineAndColCountStartAt0:= chLineColStartAt0.Checked;
    TrimTrailingSpaces:= chTrailingSpaces.Checked;

    if chBackup.Checked then
    begin
      Backup:= bsSaveWithOtherExt;

      if chBackupDir.Checked then
        Backup:= bsSaveInBackupDir;
    end
    else
      Backup := bsNoBackup;
    Save;
  end;
  ModalResult:= mrOK;
end;

procedure TfrmPrefs.btCancelClick(Sender: TObject);
begin
  ModalResult := IIF( FileTypeChanged, mrNo, mrNoToAll );
end;

end.
