//
//    TfrmEditmnuRunItem - editor de itens do menu executar.
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
@abstract(frm_editRunItem - diálogo para editar itens do menu executar do Notes.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit frm_editRunItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, NotesTranslation;

type
  { Diálogo para editar itens do menu executar do Notes.}
  TfrmEditmnuRunItem = class(TForm)
    poVars: TPopupMenu;
    pvFile: TMenuItem;
    pvPath: TMenuItem;
    pvFileName: TMenuItem;
    pvExt: TMenuItem;
    pvWordAtCursor: TMenuItem;
    pvLine: TMenuItem;
    pvCol: TMenuItem;
    pvSelLength: TMenuItem;
    pvLinesCount: TMenuItem;
    N1: TMenuItem;
    mnuInsertFileName: TMenuItem;
    mnuFileVars: TMenuItem;
    mnuVarsText: TMenuItem;
    pvSelStart: TMenuItem;
    mnuVarsProj: TMenuItem;
    N2: TMenuItem;
    mnuFuncs: TMenuItem;
    pfAskFile: TMenuItem;
    pfAskStr: TMenuItem;
    paBottom: TPanel;
    btOK: TButton;
    btCancel: TButton;
    gbPaths: TGroupBox;
    lFileToRun: TLabel;
    lArgs: TLabel;
    edCmd: TEdit;
    btProcurar: TButton;
    edArgs: TEdit;
    btArgs: TButton;
    gbOptions: TGroupBox;
    cbOut: TComboBox;
    paGbOptions: TPanel;
    chAutoUnderstandOutput: TCheckBox;
    chSaveBeforeRun: TCheckBox;
    mnuArgsFolder: TMenuItem;
    pfEnv: TMenuItem;
    pvProjName: TMenuItem;
    pvProjDir: TMenuItem;
    pvProjFile: TMenuItem;
    btOptions: TButton;
    edRegEx: TEdit;
    lRegEx: TLabel;
    edFilePos: TEdit;
    lFilePos: TLabel;
    edLinePos: TEdit;
    lLinePos: TLabel;
    lDir: TLabel;
    edDir: TEdit;
    btDir: TButton;
    pvFileTitle: TMenuItem;
    lOutput: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btArgsClick(Sender: TObject);
    procedure btDirClick(Sender: TObject);
    procedure pvClick(Sender: TObject);
    procedure btExecClick(Sender: TObject);
    procedure mnuInsertFileNameClick(Sender: TObject);
    procedure cbOutChange(Sender: TObject);
    procedure TogleOptions(Sender: TObject);
    procedure chAutoUnderstandOutputClick(Sender: TObject);
    procedure mnuArgsFolderClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ItemName: string;
    fFileName: string;
    EdSender: TObject;
    fTranslation: TNotesTranslation;
  public
    {Prepara o diálogo para editar arquivo especificado em FileName}
    procedure EditFile( const FileName: string );
  end;

var
  frmEditmnuRunItem: TfrmEditmnuRunItem;

implementation

{$R *.dfm}

uses NotesUtils, NotesGlobals, NotesProfile, NotesListEditor;

const
  FORM_SIZE = 413;
  OPT_SIZE= 207;


procedure TfrmEditmnuRunItem.EditFile( const FileName: string);
Var
  I: integer;
  Item: TNotesRunItem;
begin
  if NProfile.RunMgr.LoadItem(FileName, Item) then
  begin
    fFileName:= FileName;
    with Item do
    begin
      ItemName:= Name;
      edArgs.Text:= Args;
      edCmd.Text:= Cmd;
      edDir.Text:= Dir;
      edRegEx.Text:= OutputRegex;
      edFilePos.Text:= REFilePos;
      edLinePos.Text:= RELinePos;
      cbOut.ItemIndex := Ord( Output );
      chSaveBeforeRun.Checked:= SaveBeforeRun;
      chAutoUnderstandOutput.Checked:= AutoUnderstandOutput;
      Caption:= Format( fTranslation.getMsgTranslation('FrmCaption', 'Configuring "%s"...'), [ItemName] );
    end;
  end;

  for I := 0 to paGbOptions.ControlCount - 1 do
    if paGbOptions.Controls[I] <> chAutoUnderstandOutput then
      paGbOptions.Controls[I].Enabled:= not chAutoUnderstandOutput.Checked;
end;

procedure TfrmEditmnuRunItem.FormCreate(Sender: TObject);
begin
  fTranslation:= TNotesTranslation.Create;
  fTranslation.TranslationFile:= NProfile.getTranslationFileForModule(Self.ClassName);
  fTranslation.TranslateComponent(self);
  btOptions.Caption:= fTranslation.getMsgTranslation('btOptionsView', 'View O&ptions');

  cbOut.Items.Add(fTranslation.getMsgTranslation('OutputNone', 'Do not capture output'));
  cbOut.Items.Add(fTranslation.getMsgTranslation('OutputStandard', 'Capture output to the output panel'));
  cbOut.Items.Add(fTranslation.getMsgTranslation('OutputEditor', 'Capture output to the editor'));
  cbOut.Items.Add(fTranslation.getMsgTranslation('OutputSelText', 'Capture output to the selected text'));
  cbOut.ItemIndex:= 0;

  GbOptions.Height:= GbOptions.Height - paGbOptions.Height;
  GbOptions.Top:= GbPaths.Top + GbPaths.Height + 8;
  Height:= Height - paGbOptions.Height;
  paGbOptions.Visible:= false;

  Position:= poScreenCenter;

//  NotesMenu.InitComponent(self);
//  if not NotesMenu.Active then
//    NotesMenu.DisableSubclassing:= true;
end;

procedure TfrmEditmnuRunItem.btOKClick(Sender: TObject);
begin
  NProfile.RunMgr.SaveItem(NProfile.RunMgr.CreateItem(ItemName, edCmd.Text,
    edArgs.Text, edDir.Text, edRegEx.Text, edLinePos.Text, edFilePos.Text,
    TNotesRunOutputType( Ord(cbOut.ItemIndex) ), chSaveBeforeRun.Checked,
    chAutoUnderstandOutput.Checked), fFileName);

  modalresult := mrOK;
end;

procedure TfrmEditmnuRunItem.btExecClick(Sender: TObject);
var
  P : TPoint;
begin
  EdSender:= edCmd;
  P:= btProcurar.ClientToScreen(Point(btProcurar.Width + -10, 10));
  poVars.Popup(P.X, P.Y);
end;

procedure TfrmEditmnuRunItem.btArgsClick(Sender: TObject);
var
  P : TPoint;
begin
  EdSender:= edArgs;
  P:= btArgs.ClientToScreen(Point(btArgs.Width + -10, 10));
  poVars.Popup(P.X, P.Y);
end;

procedure TfrmEditmnuRunItem.btDirClick(Sender: TObject);
var
  P : TPoint;
begin
  EdSender:= edDir;
  P:= btDir.ClientToScreen(Point(btDir.Width + -10, 10));
  poVars.Popup(P.X, P.Y);
end;

procedure TfrmEditmnuRunItem.pvClick(Sender: TObject);
var
  S: string;
begin
  if not( EdSender is TEdit ) or not ( Sender is TMenuItem ) then Exit;
  S:=(Sender As TMenuItem).Name;
  if length(S) > 3 then
  begin
    if S[1] in ['p','P'] then
    begin

      if S[2] in ['f', 'F'] then
        S:= Copy(S, 3, length(S)) + '()'
      else
        S:= Copy(S, 3, length(S));

      (EdSender As TEdit).SelText:= '%' + S +'%';
    end;
  end;
end;

procedure TfrmEditmnuRunItem.mnuInsertFileNameClick(Sender: TObject);
begin
  if not( EdSender is TEdit ) or not ( Sender is TMenuItem ) then Exit;

  with TOpenDialog.Create(nil) do
  begin
    try
      Filter:= '(*.exe)|*.exe|(*.*)|*.*';
      if Execute then
        (EdSender As TEdit).SelText:= FileName;
    finally
      Free;
    end;
  end;
end;

procedure TfrmEditmnuRunItem.cbOutChange(Sender: TObject);
begin
  paGbOptions.Visible:= (cbOut.ItemIndex = 1) and (self.Height = FORM_SIZE);
end;

procedure TfrmEditmnuRunItem.TogleOptions(Sender: TObject);
begin
  if self.Height = FORM_SIZE then
  begin
    btOptions.Caption:= fTranslation.getMsgTranslation('btOptionsView', 'View O&ptions');
    self.Height:= FORM_SIZE - paGbOptions.Height;
    gbOptions.Height:= OPT_SIZE - paGbOptions.Height;
  end else
  begin
    btOptions.Caption:= fTranslation.getMsgTranslation('btOptionsHide', 'Hide O&ptions');
    self.Height:= FORM_SIZE;
    gbOptions.Height:= OPT_SIZE;
  end;

  cbOutChange(self);
end;

procedure TfrmEditmnuRunItem.chAutoUnderstandOutputClick(Sender: TObject);
Var
  I: integer;
begin
  for I := 0 to paGbOptions.ControlCount - 1 do
    if paGbOptions.Controls[I] <> chAutoUnderstandOutput then
      paGbOptions.Controls[I].Enabled:= not chAutoUnderstandOutput.Checked;
end;

procedure TfrmEditmnuRunItem.mnuArgsFolderClick(Sender: TObject);
var
  S: string;
begin
  Try
    S:= BrowseForFolder(Handle, fTranslation.getMsgTranslation('MsgBrowseForDir', 'Browse for a folder'));
  finally
    if (S <> EmptyStr) and (EdSender is TEdit) then
      (EdSender As TEdit).SelText:= S;
  end;
end;


procedure TfrmEditmnuRunItem.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fTranslation);
//  NotesMenu.DisableSubclassing:= false;
end;

end.
