//
//    TfrmList - form que apresenta um listbox permitindo
//      ao usuário selecionar um dos items.
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
  @abstract(TfrmList - form que apresenta um listbox permitindo ao usuário selecionar um dos items.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit frm_ListDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmList = class(TForm)
    paBottom: TPanel;
    btOk: TButton;
    btCancel: TButton;
    Bevel1: TBevel;
    paCenter: TPanel;
    lbItems: TListBox;
    paTop: TPanel;
    edSel: TEdit;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edSelChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    fValues: TStringList;
    fSelected: string;
    function getAutoCompleteField: boolean;
    procedure setAutoCompleteField(const Value: boolean);
    procedure ShowAllItems;
    { Private declarations }
  public
    function getSelectedIndex: integer;
    property AutoCompleteField: boolean read getAutoCompleteField write setAutoCompleteField;
    property Values: TStringList read fValues;
    property Selected: string read fSelected;
  end;

var
  frmList: TfrmList;

implementation

uses NotesUtils;

{$R *.dfm}

procedure TfrmList.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if getFocus <> edSel.Handle then
    if edSel.CanFocus then
    begin
      edSel.SetFocus;
      edSel.Text:= edSel.Text + key;
      edSel.SelStart:= length(edSel.Text);
    end;
end;

procedure TfrmList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
    begin
      if getFocus <> lbItems.Handle then
        if lbItems.CanFocus then
        begin
          if key = VK_UP then
            if lbItems.ItemIndex > 0 then
              lbItems.ItemIndex:= lbItems.ItemIndex -1;

          if key = VK_DOWN then
            if lbItems.ItemIndex < lbItems.Count -1 then
              lbItems.ItemIndex:= lbItems.ItemIndex + 1;

          Key:= 0;
          lbItems.SetFocus;
        end;
    end;
    VK_LEFT, VK_RIGHT:
    begin
      if getFocus <> edSel.Handle then
        if edSel.CanFocus then
        begin
          if key = VK_LEFT then
            edSel.SelStart:= edSel.SelStart - 1
          else
            edSel.SelStart:= edSel.SelStart + 1;
          edSel.SetFocus;
          Key:= 0;
        end;
    end;
  end;
end;


function TfrmList.getAutoCompleteField: boolean;
begin
  result:= paTop.Visible;
end;

procedure TfrmList.setAutoCompleteField(const Value: boolean);
begin
  paTop.Enabled:= Value;
  edSel.Enabled:= Value;
  paTop.Visible:= Value;
end;

procedure TfrmList.FormCreate(Sender: TObject);
begin
  fValues:= TStringList.Create;
end;

procedure TfrmList.FormDestroy(Sender: TObject);
begin
  fValues.Free;
end;

function TfrmList.getSelectedIndex: integer;
begin
  Result:= -1;
  if fSelected <> '' then
    result:= fValues.IndexOf(fSelected);
end;

procedure TfrmList.edSelChange(Sender: TObject);
var
  S, edTxt: string;
  I, aPos, lowerPos, ToSel: integer;
begin
  lowerPos:= fValues.Count -1;
  edTxt:= AnsiLowerCase(edSel.Text);
  if edTxt = '' then
  begin
    ShowAllItems;
    Exit;
  end;
  toSel:= 0;
  lbItems.Items.BeginUpdate;
  lbItems.Clear;
  try
    for I:= 0 to fValues.Count -1 do
    begin
      S:= AnsiLowerCase(fValues.Strings[I]);
      aPos:= AnsiPos(edTxt, S);
      if aPos > 0 then
      begin
        if aPos < lowerPos then
        begin
          lowerPos:= aPos;
          toSel:= lbItems.Items.Add(fValues.Strings[I]);
        end else
          lbItems.Items.Add(fValues.Strings[I]);
      end;
    end;

    if (toSel > -1) and (toSel < lbItems.Count) and (lbItems.Count > 0) then
      lbItems.ItemIndex:= toSel
    else if lbItems.Count > 0 then
      lbItems.ItemIndex:= 0;

  finally
    lbItems.Items.EndUpdate;
  end;
end;

procedure TfrmList.ShowAllItems;
begin
  lbItems.Items.BeginUpdate;
  lbItems.Items.Clear;
  lbItems.Items.AddStrings(fValues);
  if lbItems.Count > 0 then
    lbItems.ItemIndex:= 0;
  lbItems.Items.EndUpdate;
end;

procedure TfrmList.FormShow(Sender: TObject);
begin
  ShowAllItems;
end;

procedure TfrmList.btOkClick(Sender: TObject);
begin
  if lbItems.ItemIndex > -1 then
  begin
    fSelected:= lbItems.Items[lbItems.ItemIndex];
    modalResult:= mrOk;
  end;
end;

end.
