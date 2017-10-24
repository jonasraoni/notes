//
//    NotesListEditor - diálogos para editar listas de strings
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
  @abstract(NotesListEditor - diálogos para editar listas de strings.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesListEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NotesUtils, ExtCtrls, NotesGlobals;

type
  // @code(leAddString) - se presente, o listEditor mostra um botão para adicionar strings
  // @code(leAddFile) - se presente, o listEditor mostra um botão para adicionar nomes de arquivos
  // @code(leAddFolder) - se presente, o listEditor mostra um botão para adicionar pastas
  TListEditorButton = (leAddString, leAddFile, leAddFolder);
  // Lista de botões do tipo @link(TListEditorButton)
  TListEditorButtons = set of TListEditorButton;

  { Diálogo para editar listas de strings, nomes de arquivos
   e nomes de pastas. A lista de string pode ser tanto uma
   TString/TStringList quanto uma string comum em que os
   items são separados por um delimitador. Use a função
   @link(EditList) ao invés de criar o diálogo diretamente.  }
  TfrmListEditor = class(TForm)
    lDescr: TLabel;
    lbStrs: TListBox;
    btOK: TButton;
    btCancel: TButton;
    btAddFile: TButton;
    btAddFolder: TButton;
    btAdd: TButton;
    btEdit: TButton;
    btDel: TButton;
    bv1: TBevel;
    bv2: TBevel;
    bv3: TBevel;
    gbOpts: TPanel;
    Label1: TLabel;
    edDelimiter: TEdit;
    procedure btAddFileClick(Sender: TObject);
    procedure btAddFolderClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btEditClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure edDelimiterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fDelimiter: string;
    function gertStrAsStr: string;
    function getBtns: TListEditorButtons;
    function getShowDelimiterEd: boolean;
    function getStrs: TStrings;
    procedure setBtns(const Value: TListEditorButtons);
    procedure setShowDelimiterEd(const Value: boolean);
    procedure setStrAsStr(const Value: string);
    procedure setStrs(const Value: TStrings);
    function getDescr: string;
    procedure setDescr(const Value: string);
  public
    // TStrings ou TStringList que será editado
    property Strings: TStrings read getStrs write setStrs;
    // String comum com items separados por um delimitador
    property StringsAsStr: string read gertStrAsStr write setStrAsStr;
    // Botões que serão mostrados para o usuário. Vide @link(TListEditorButtons).
    property Buttons: TListEditorButtons read getBtns write setBtns;
    // Permitir ao usuário editar o delimitador que é usado?!
    property ShowDelimiterEditor: boolean read getShowDelimiterEd write setShowDelimiterEd;
    // O delimitador
    property Delimiter: string read fDelimiter write fDelimiter;
    // Descrição que será mostrada para o usuário
    property DlgDescription: string read getDescr write setDescr;
  end;

var
  frmListEditor: TfrmListEditor;

{ Edita uma lista de strings que é uma string separada por delimitadores.
  @code(List) - lista de strings a ser editada
  @code(Btns) - botões que serão mostrados no diálogo. Vide @link(TListEditorButton).
  @code(Title) - o título do diálogo.
  @code(Description) - descrição e instruções para o usuário. }
function EditList(Var List: TStrings; Btns: TListEditorButtons; const Title, Description: string): boolean; overload; forward;

{ Edita uma lista de strings que é uma string separada por delimitadores.
  @code(S) - string separada por delimitadores
  @code(Btns) - botões que serão mostrados no diálogo. Vide @link(TListEditorButton).
  @code(Title) - o título do diálogo.
  @code(Description) - descrição e instruções para o usuário.
  @code(ADelimiter) - o delimitador que é usado para separa a lista de strings.
  @code(CanEditDelimiter) - o delmitador pode ser editado pelo usuário?! }
function EditList(Var S: string; Btns: TListEditorButtons; const Title, Description: string; Var ADelimiter: string; const CanEditDelimiter: boolean ): boolean; overload; forward;


implementation

{$R *.dfm}

procedure TfrmListEditor.btAddFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  begin
    try
      Filter := 'Todos os arquivos (*.*)|*.*';
      if Execute then
        lbStrs.Items.Add(FileName);
    finally
      Free;
    end;
  end;
end;

procedure TfrmListEditor.btAddFolderClick(Sender: TObject);
var
  S: string;
begin
  Try
    S:= BrowseForFolder(Handle, 'Escolha o diretório que você deseja adicionar.');
  finally
    if S <> EmptyStr then
      lbStrs.Items.Add(S);
  end;
end;

procedure TfrmListEditor.btAddClick(Sender: TObject);
Var
  S: string;
begin
  if not InputQuery( 'Adicionar...', 'Insira a string e clique em OK para adicioná-la a lista.',
    S) or ( S = '' ) then
    Exit;
  lbStrs.Items.Add(S);
end;

procedure TfrmListEditor.btEditClick(Sender: TObject);
Var
  S: string;
begin
  if lbStrs.ItemIndex <> -1 then
  begin
    S:= lbStrs.Items[lbStrs.itemIndex];

    if InputQuery( 'Editar...', 'Edite a string e clique ok.',
    S) and ( S <> '' ) then
      lbStrs.Items[lbStrs.itemIndex]:= S;
  end;
end;

procedure TfrmListEditor.btDelClick(Sender: TObject);
Var
  I: integer;
begin
  I:= lbStrs.ItemIndex;

  if I <> -1 then
    lbStrs.DeleteSelected;

  if lbStrs.Items.Count > 0 then
    if I - 1 > -1 then
      lbStrs.ItemIndex:= I - 1
    else
      lbStrs.ItemIndex:= 0;
end;

function TfrmListEditor.gertStrAsStr: string;
Var
  I: integer;
  a: TStringArray;
begin
  if fDelimiter <> '' then
  begin
    if lbStrs.Count > 0 then
    begin
      setLength(a, lbStrs.Count);
      for I := 0 to lbStrs.Count -1 do
        a[I]:= lbStrs.Items[I];
      Result:= ImplodeStr(a, fDelimiter[1]);
      setLength(a, 0);
    end;
  end;
end;

function TfrmListEditor.getBtns: TListEditorButtons;
begin
  if btAdd.Visible then
    Result:= [leAddString];
  if btAddFile.Visible then
    REsult:= Result + [leAddFile];
  if btAddfolder.Visible then
    Result:= Result + [leAddfolder];
end;

function TfrmListEditor.getShowDelimiterEd: boolean;
begin
  result:= gbOpts.Visible;
end;

function TfrmListEditor.getStrs: TStrings;
begin
  Result:= TStringList.Create;
  Result.AddStrings(lbStrs.Items);
end;

procedure TfrmListEditor.setBtns(const Value: TListEditorButtons);
begin
  btAdd.Visible:= (leAddString in Value);
  btAddFile.Visible:= (leAddFile in Value);
  btAddfolder.Visible:= (leAddfolder in Value);
end;

procedure TfrmListEditor.setShowDelimiterEd(const Value: boolean);
begin
  gbOpts.Visible:= Value;
end;

procedure TfrmListEditor.setStrAsStr(const Value: string);
var
  a: TStringArray;
  I: integer;
begin
  if fDelimiter <> '' then
  begin
    a:= ExplodeStr(Value, fDelimiter[1]);
    for I:=0 to High(a) do
      lbStrs.Items.Add(a[I]);
    setLength(a, 0);
  end;
end;

procedure TfrmListEditor.setStrs(const Value: TStrings);
begin
  lbStrs.Items.AddStrings(Value);
end;

procedure TfrmListEditor.edDelimiterChange(Sender: TObject);
begin
  fDelimiter:= edDelimiter.Text;
end;

function TfrmListEditor.getDescr: string;
begin
  result:= lDescr.Caption;
end;

procedure TfrmListEditor.setDescr(const Value: string);
begin
  lDescr.Caption:= Value;
end;

procedure TfrmListEditor.FormShow(Sender: TObject);
begin
  lDescr.Width:= Width - 14;

  if btAddFile.Visible = false then
  begin
    btAddFolder.Top:= btAddFolder.Top - 34;
    btAdd.Top := btAdd.Top - 34;
    bv1.Top:= bv1.top - 34;
    bv2.Top:= bv2.Top -34;
    btEdit.Top:=btEdit.Top-34;
    btDel.Top:= btDel.Top -34;
  end;

  if btAddFolder.Visible = false then
  begin
    btAdd.Top := btAdd.Top - 34;
    bv1.Top:= bv1.top - 34;
    bv2.Top:= bv2.Top -34;
    btEdit.Top:=btEdit.Top-34;
    btDel.Top:= btDel.Top -34;
  end;

  if btAdd.Visible = false then
  begin
    bv1.Top:= bv1.top - 34;
    bv2.Top:= bv2.Top -34;
    btEdit.Top:=btEdit.Top-34;
    btDel.Top:= btDel.Top -34;
  end;

  if gbOpts.Visible = false then
  begin
    bv3.Top := bv3.Top - 40;
    Height:= Height - 40;
    btOk.Top:= btOk.Top -40;
    btCancel.Top:=btCancel.Top - 40;
  end;
end;

function EditList(Var List: TStrings; Btns: TListEditorButtons; const Title, Description: string): boolean;
begin
  Result:= false;
  with TfrmListEditor.Create(nil) do
  begin
    try
      ShowDelimiterEditor:= false;
      Delimiter:= ';';
      Buttons:= Btns;
      Caption:= Title;
      DlgDescription:= Description;
      Strings:= List;
      if ShowModal = mrOk then
      begin
        Result:= true;
        List.Clear;
        List.AddStrings(Strings);
      end;
    finally
      Free;
    end;
  end;
end;


function EditList(Var S: string; Btns: TListEditorButtons; const Title, Description: string; Var ADelimiter: string; const CanEditDelimiter: boolean ): boolean;
begin
  Result:= false;
  with TfrmListEditor.Create(nil) do
  begin
    try
      ShowDelimiterEditor:= CanEditDelimiter;
      Delimiter:= ADelimiter;
      Buttons:= Btns;
      Caption:= Title;
      DlgDescription:= Description;
      StringsAsStr:= S;
      if ShowModal = mrOk then
      begin
        Result:= true;
        S:= StringsAsStr;
        if CanEditDelimiter then
          ADelimiter:= Delimiter;
      end;
    finally
      Free;
    end;
  end;
end;


end.
