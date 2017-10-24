//
//    NotesTooltips - componente de tooltips do Notes
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
  @abstract(NotesTooltips - componente de tooltips do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesToolTip;

interface

uses classes, Forms, SysUtils, Graphics, windows, Controls, NotesTimers,
  StdCtrls;

type

  // Form especial usado pelas tooltips
  TNotesToolTipForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  // classe base para as classes de ToolTips do Notes
  TNotesCustomToolTip = class(TObject)
  private
    fForm: TNotesToolTipForm;
    fTitle: string;
    fLines: TStringList;
    fWrap: integer;
    fShowing: boolean;
    function CharHeigth: integer;
    function GetText: string;
    procedure SetText(const Value: string);
    procedure setWrap(const Value: integer);
    procedure AdjustToWorkArea;
    procedure setTitle(const Value: string);
  protected
    // Pega a o tamanho da maior linha. Usado para calcular
    // a width da tooltip.
    function getMaxLineWidth: integer; virtual;
    // Pinta a tooltip
    procedure doPaint(Sender: TObject); virtual;
    // Pelo comportamento padrão, quando o usuário
    // clica na tooltip, ela some.
    procedure doClick(Sender: TObject); virtual;
    // Calcula e seta a posição em que o form será mostrado
    procedure CalcFormLocation; Virtual;
    // Calcula e seta o tamanho do form
    procedure CalcFormSize; Virtual;
  public
    constructor create;
    destructor  destroy; override;
    // Mostra a ToolTip
    procedure Show; virtual;
    // Esconde a ToolTip
    procedure Hide; virtual;
    // Título da tooltip: ele é mostrado em negrito
    // Se nenhum título for setado, a tooltip mostrará
    // apenas texto normal
    property Title: string read fTitle write setTitle;
    // seta o texto da tooltip
    property Text: string read GetText write SetText;
    // Permite controlar o tamanho da linhas de texto da
    // tooltip relativo a número de caracteres.
    property Wrap: integer read fWrap write setWrap;
    // se a tooltip está sendo mostrada
    property isShowing: boolean read fShowing;
  end;

  { Tooltips do Notes. A tooltip é mostrada de acordo
    com a posição do mouse e é escondida automaticamente se o
    usuário move o mouse. }
  TNotesToolTip = class(TNotesCustomToolTip)
  private
    fMousePos: TPoint;
    // checa se o mouse se moveu e fecha a tooltip caso isto
    // tenha ocorrido.
    procedure CheckMouseMoves;
  protected
    // substituímos @code(CalcFormLocation) para que a tooltip
    // possa aparecer de acordo com a posição do mouse.
    procedure CalcFormLocation; override;
  end;


implementation

uses NotesUtils;

{ TNotesDropShadowForm}

constructor TNotesToolTipForm.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
      if OldCreateOrder then DoCreate;
  finally
    GlobalNameSpace.EndWrite;
  end;

  width:= 220;
  height:= 160;
  FormStyle:= fsStayOnTop;
  BorderStyle:= bsNone;
  Color:= clInfoBk;
  Canvas.Font.Color:= clInfoText;
  Font:= Screen.HintFont;
end;

procedure TNotesToolTipForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  Inherited CreateParams(Params);
  Params.ExStyle:= WS_EX_TOOLWINDOW;

  // Habilita DropShadow em winXP e win2003
{$IFDEF MSWINDOWS}
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) ) then
  begin
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
  end;
{$ENDIF}
end;


{ TNotesCustomToolTip }

procedure TNotesCustomToolTip.AdjustToWorkArea;
begin
  if fForm.Width <= screen.WorkAreaWidth then
  begin
    if (fForm.Left + fForm.Width) > Screen.WorkAreaWidth then
      fForm.Left:= Screen.WorkAreaWidth - fForm.Width - 2;
  end;

  if fForm.Height <= Screen.WorkAreaHeight then
  begin
    if (fForm.Top + fForm.Height) > Screen.WorkAreaHeight then
      fForm.Top:= Screen.WorkAreaHeight - fForm.Height - 2
    else if fForm.Top < 4 then
      fForm.Top:= 36;
  end;
end;


procedure TNotesCustomToolTip.CalcFormLocation;
begin
{  fForm.Top:= 300;
  fForm.Left:= 200;
  fForm.Position:= poDesigned;   }
end;

procedure TNotesCustomToolTip.CalcFormSize;
begin
  if fTitle <> '' then
    fForm.Height:= (fLines.Count + 1) * CharHeigth + 10
  else
    fForm.Height:= (fLines.Count * CharHeigth) + 10;

  fForm.Width:= getMaxLineWidth + 10;
end;

function TNotesCustomToolTip.CharHeigth: integer;
begin
  Result:= fForm.Canvas.TextHeight('\ÂÃ/Î');
end;

constructor TNotesCustomToolTip.create;
begin
  inherited;
  fForm:= TNotesToolTipForm.Create(nil);
  fForm.OnPaint:= doPaint;
  fForm.OnClick:= doClick;
  fLines:= TStringList.Create;
end;

destructor TNotesCustomToolTip.destroy;
begin
  fForm.Free;
  fLines.Free;
  inherited;
end;

procedure TNotesCustomToolTip.doClick(Sender: TObject);
begin
  Hide;
end;

procedure TNotesCustomToolTip.doPaint(Sender: TObject);
Var
  I: integer;
begin
  // TÍTULO
  fForm.Canvas.Font.Style:= [fsBold];
  fForm.Canvas.TextOut(4, 4, fTitle);

  // TEXTO
  fForm.Canvas.Font.Style:= [];
  if fTitle <> EmptyStr then
  begin
    for I:= 1 to fLines.Count do
      fForm.Canvas.TextOut(4, (I * charHeigth) + 6, fLines.Strings[I-1]);
  end else
  begin
    if fLines.Count < 1 then Exit;

    fForm.Canvas.TextOut(4, 4, fLines.Strings[0]);
    for I:= 1 to fLines.Count - 1 do
      fForm.Canvas.TextOut(4, (I * charHeigth) + 4, fLines.Strings[I]);
  end;

  // BORDER TOP-LEFT
  fForm.Canvas.Pen.Color:= cl3DLight;
  fForm.Canvas.MoveTo(fForm.Width,1);
  fForm.Canvas.LineTo(1 , 1);
  fForm.Canvas.MoveTo(1,0);
  fForm.Canvas.LineTo(1, fForm.Height);

  // BORDER BOTTOM-RIGHT
  fForm.Canvas.Pen.Color:= cl3DDkShadow;
  fForm.Canvas.MoveTo(0, fForm.Height-1);
  fForm.Canvas.LineTo(fForm.Width , fForm.Height-1);
  fForm.Canvas.MoveTo(fForm.Width-1 , fForm.Height);
  fForm.Canvas.LineTo(fForm.Width-1, -1);

end;

function TNotesCustomToolTip.getMaxLineWidth: integer;
Var
  I, W: integer;
begin
  fForm.Canvas.Font.Style:= [fsBold];
  Result:= fForm.Canvas.TextWidth(fTitle);
  fForm.Canvas.Font.Style:= [];

  for I:= 0 to fLines.Count - 1 do
  begin
    W:= fForm.Canvas.TextWidth(fLines.Strings[I]);
    if Result < W then
      Result:= W;
  end;

end;

function TNotesCustomToolTip.GetText: string;
begin
  Result:= fLines.text;
end;

procedure TNotesCustomToolTip.Hide;
begin
  ShowWindow(fForm.Handle, SW_HIDE);
  fShowing:= false;
end;

procedure TNotesCustomToolTip.SetText(const Value: string);
begin
  if fWrap > 0 then
    fLines.Text:= WordWrap(Value, fWrap)
  else
    fLines.Text:= Value;

  CalcFormSize;
end;

procedure TNotesCustomToolTip.setTitle(const Value: string);
begin
  CalcFormSize;
  fTitle := Value;
end;

procedure TNotesCustomToolTip.setWrap(const Value: integer);
begin
  fWrap := Value;
  fLines.text:= WordWrap(fLines.text, Value);
  CalcFormSize;
end;

procedure TNotesCustomToolTip.Show;
begin
  if fShowing then
    Hide;

  CalcFormLocation;
  // A tooltip é ajustada para que apareça sempre inteira.
  AdjustToWorkArea;

  fShowing:= true;
  ShowWindow(fForm.Handle, SW_SHOWNOACTIVATE);
end;

{ TNotesToolTip }

procedure TNotesToolTip.CalcFormLocation;
begin
  getCursorPos(fMousePos);
  fForm.Left:= fMousePos.X + 4;
  fForm.Top:= fMousePos.Y - fForm.Height - 4;

  if fForm.Top < 4 then
    fForm.Top:= fMousePos.Y + 28;

  timers.setTimeOut(400, CheckMouseMoves)
end;

procedure TNotesToolTip.CheckMouseMoves;
Var
  NewMousePos: TPoint;
begin
  getCursorPos(NewMousePos);
  if (NewMousePos.X <> fMousePos.X) or (NewMousePos.Y <> fMousePos.Y) then
    Hide
  else if fShowing then
    timers.setTimeOut(200, CheckMouseMoves);
end;



end.
