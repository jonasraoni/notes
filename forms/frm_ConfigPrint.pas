//
//    frm_ConfigPrint - Confirações pré-impressão e impressão do documento atual.
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
@abstract(frm_ConfigPrint - Configurações de impressão e impressão do documento.)
@author(Josimar Silva <josimar_me@yahoo.com.br>)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)

unit frm_ConfigPrint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEditPrint, SynEditPrintTypes, NotesEditorTab;

type
  { Form que permite ao usuário configurar algumas opções
  de impressão e imprime o documento. Para mostrar este
  form, use o método @code(ShowDialog) ao invès de @(Show)
  e @code(ShowModal).}
  TfrmConfigPrint = class(TForm)
    btnOK           : TButton;
    btnCancelar     : TButton;
    GroupBox1: TGroupBox;
    chkCabecalho: TCheckBox;
    chkNumeroPagina: TCheckBox;
    chkNumeroLinha: TCheckBox;
    GroupBox2: TGroupBox;
    chkQuebrarLinha: TCheckBox;
    chkCores: TCheckBox;
    chkSyntax: TCheckBox;
    GroupBox3: TGroupBox;
    btnConfigurar: TButton;
    Label1: TLabel;

    procedure btnOKClick(Sender: TObject);
    procedure btnConfigurarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    { Método usado para mostrar o diálogo. Passe o ActiveEditorTab
    no parâmetro @code(EditorTabToPrint).}
    procedure ShowDialog(const EditorTabToPrint: TNotesEditorTab);
  private
    fEditorTabToPrint: TNotesEditorTab;
    prnOptions       : TSynEditPrint;
    prnConfigureDlg  : TPrinterSetupDialog;
  end;

var
  frmConfigPrint: TfrmConfigPrint;

implementation

{$R *.dfm}

procedure TfrmConfigPrint.btnOKClick(Sender: TObject);
var
  AFont: TFont;
begin
  prnOptions := TSynEditPrint.Create(Self);
  prnOptions.Title := fEditorTabToPrint.FileName + ' (' + fEditorTabToPrint.FullPath +')';
  prnOptions.SynEdit := fEditorTabToPrint.Editor;

  prnOptions.LineNumbers := chkNumeroLinha.Checked;
  prnOptions.Wrap        := chkQuebrarLinha.Checked;
  prnOptions.Highlight   := chkSyntax.Checked;
  prnOptions.Colors      := chkCores.Checked;

  AFont := TFont.Create;

  if chkCabecalho.Checked then
  begin
    prnOptions.Header.Clear;
    prnOptions.Header.Add('$TITLE$', AFont, taRightJustify, 1);
    prnOptions.Header.FrameTypes := prnOptions.Footer.FrameTypes + [ftLine];
  end;

  if chkNumeroPagina.Checked then
  begin
    prnOptions.Footer.Clear;
    prnOptions.Footer.Add('$PAGENUM$/$PAGECOUNT$', AFont, taRightJustify, 1);
    prnOptions.Footer.FrameTypes := prnOptions.Footer.FrameTypes + [ftLine];
  end;

  prnOptions.Print;
  Close;
end;

procedure TfrmConfigPrint.btnConfigurarClick(Sender: TObject);
begin
  prnConfigureDlg := TPrinterSetupDialog.Create(Self);
  try
    prnConfigureDlg.Execute;
  except
    raise Exception.Create('Erro ao abrir o diálogo de "Configurações de Impressão". Verifique se a sua impressora está corretamente instalada.');
  end;
end;

procedure TfrmConfigPrint.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmConfigPrint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(prnOptions) then
    prnOptions.Free;
  if Assigned(prnConfigureDlg) then
    prnConfigureDlg.Free;
end;

procedure TfrmConfigPrint.ShowDialog(
  const EditorTabToPrint: TNotesEditorTab);
begin
  fEditorTabToPrint:= EditorTabToPrint;
  Caption:= 'Imprimir "' + fEditorTabToPrint.Tab.Caption + '"';
  ShowModal;
end;

end.
