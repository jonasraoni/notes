//
//    TfrmAddLibItem - form para adicionar/editar itens da biblioteca.
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
@abstract(frm_addItem - diálogo que permite adicionar/editar itens da biblioteca de códigos do Notes.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit frm_addItem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NotesTranslation;

type
  {Diálogo para adicionar/editar itens da biblioteca.}
  TfrmAddLibItem = class(TForm)
    meCode: TMemo;
    lCode: TLabel;
    lHelp: TLabel;
    meHelp: TMemo;
    lName: TLabel;
    edName: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FItemPath: String;
    FEditMode: boolean;
    fTranslate: TNotesTranslation;
  public
    {Configura o diálogo para editar um item existente.}
    procedure EditMode(ItemPathAndName: string);
    {Configura o diálogo para criar um novo item.}
    procedure AddMode (ItemPath: string);
  end;

var
  frmAddLibItem: TfrmAddLibItem;

implementation

{$R *.dfm}

uses NotesUtils, NotesGlobals;

procedure TfrmAddLibItem.AddMode(ItemPath: string);
begin
  FEditMode := FALSE;
  FItemPath := ItemPath;
  Caption := fTranslate.getMsgTranslation('CaptionAddItem', 'Add item');
end;

procedure TfrmAddLibItem.EditMode(ItemPathAndName: string);
begin
  FEditMode := TRUE;
  FItemPath := ItemPathAndName;
  Caption := fTranslate.getMsgTranslation('CaptionEditItem', 'Edit item');
  edName.Text := ExtractFileName(ItemPathAndName);
  edName.Modified := false;
  meCode.Lines.LoadFromFile( ItemPathAndName+'.ncl' );
  if FileExists( ItemPathAndName+'.help' ) then
    meHelp.Lines.LoadFromFile( ItemPathAndName+'.help' );
end;

procedure TfrmAddLibItem.btOKClick(Sender: TObject);
begin
  if FEditMode then begin
    if edName.Modified then begin
      RenameFile(FItemPath + '.ncl', ExtractFilePath(FItemPath) + edName.Text + '.ncl');

      if FileExists( FItemPath+'.help' ) then
        RenameFile( FItemPath + '.help', ExtractFilePath(FItemPath) + edName.Text + '.help' );
    end;
    meCode.Lines.SaveToFile(ExtractFilePath(FItemPath) + edName.Text + '.ncl');
    meHelp.Lines.SaveToFile(ExtractFilePath(FItemPath) + edName.Text + '.help');
  end
  else begin
    meCode.Lines.SaveToFile(AddSlash(FItemPath) + edName.Text + '.ncl');
    meHelp.Lines.SaveToFile(AddSlash(FItemPath) + edName.Text + '.help');
  end;
  ModalResult := mrOK;
end;


procedure TfrmAddLibItem.FormCreate(Sender: TObject);
begin
  screen.Cursor:= crHourGlass;

  fTranslate:= TNotesTranslation.Create;
  fTranslate.TranslationFile:= NProfile.getTranslationFileForModule(self.ClassName);
  fTranslate.TranslateComponent(self);

  screen.Cursor:= crDefault;
end;

procedure TfrmAddLibItem.FormDestroy(Sender: TObject);
begin
  fTranslate.Free;
end;

end.
