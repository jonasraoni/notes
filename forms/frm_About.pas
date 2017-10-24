//
//    TfrmAbout - form "sobre" do Notes.
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
@abstract(frm_About - diálogo sobre do Notes.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit frm_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, bLink, StdCtrls;

type
  {Diálogo "Sobre o Notes".}
  TfrmAbout = class(TForm)
    Shape1: TShape;
    Shape2: TShape;
    btOK: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    blCredit: TbLink;
    bLink1: TbLink;
    procedure bLink1Click(Sender: TObject);
    procedure blCreditClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses ShellApi, NotesGlobals;
{$R *.dfm}

procedure TfrmAbout.bLink1Click(Sender: TObject);
begin
  ShellExecute( Handle, 'open','https://github.com/jonasraoni/notes', nil, nil, SW_SHOWNORMAL );
end;

procedure TfrmAbout.blCreditClick(Sender: TObject);
begin
  ShellExecute( Handle, 'open', PChar(NExePath + 'help\other-credits.htm'), nil, nil, SW_SHOWNORMAL );
end;

end.
