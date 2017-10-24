//
//    NotesCodeCompletitionDlg - diálogo de code completition (Intellisens)
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
  @abstract(NotesCodeCompletitionDlg - diálogo de code completition (Intellisens).)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Para criar um diálogo de code completition, crie uma objeto do tipo
  NotesCodeCompletitionDlg, preencha os items para completar o código e
  chame o método execute.
  @code(Problemas conhecidos) - ás vezes o foco não fica no editor como deveria
  se o usuário usa o mouse para selecionar o item, principalmente mexendo na
  scrollbar.
*)
unit NotesCodeCompletitionDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEdit, SynEditKeyCmds;

type
  // Form com listbox usado para fazer o Code Completition
  TfrmComplete = class(TCustomForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    flbOptions: TListBox;
    fEditor: TSynEdit;
    fItemHeigth: integer;
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure ResizeListBox(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    // Foca o editor
    procedure FocusEditor(Sender: TObject);
    // Listbox mostrado no diálogo
    property lbOptions: TListBox read flbOptions write flbOptions;
    // Editor em que o code completition será feita
    property Editor: TSynEdit read fEditor write fEditor;
    // Heigth dos items do Listbox (usado em TNotesCodeCompletitionDlg)
    property ListItemHeigth: integer read fItemHeigth;
  end;

type
  // Diálogo de code completition (Intellisense) do Notes
  TNotesCodeCompletitionDlg = class(TComponent)
  private
    // Usamos um timer para manter o foco no Editor. Não podemos capturar
    // todos os eventos (por exemplo, aparentemente não há como capturar
    // clicks na scrollbar do listbox), então este foi o workaround encontrado
    fTimer: TTimer;
    fForm: TfrmComplete;
    fList: TStrings;
    fEditor: TSynEdit;
    procedure HandleFormClose(Sender: TObject; var Action: TCloseAction);
    procedure SelectCompletitionItem;
    procedure EditorCommandHandler(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: char;
      Data: pointer; HandlerData: pointer);
    procedure EditorKeyDownHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditorMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoTimer(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure DoCompletition;
    function getVisibleList: TStrings;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    // Limpa as listas
    procedure ClearLists;
    // Retorna o número do item selecionado
    function  GetSelectedItem: integer;
    // Retorna o item selecionado
    function  GetSelItemText: string;
    // Executa o diálogo. Passe o editor em que o CodeCompletition deve ser feito
    procedure Execute(const Editor: TSynEdit);
    // Fecha o diálogo
    procedure Close;
    // Items visíveis
    property VisibleItems: TStrings read getVisibleList;
    // Lista de items invisíveis. Se esta lista existir
    // o item inserido será o item da lista invisível.
    // Se não exisitir, o item inserido será o da lista
    // VisibleItems
    property HiddenItems: TStrings read fList;
  end;

implementation

uses
  NotesGlobals;

const
  WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','.', ','];

// Pega a palavra atual dependendo da coluna em que está o cursor
function getCurWord(const AEditor: TSynEdit): string;
var
  S: string;
  I, Len: integer;
begin
  Result:= '';
  S:= AEditor.LineText;
  Len:= length(S);
  if Len = 0 then Exit;
  I:= AEditor.CaretX;

  if I > Len then
    I:= Len;

  for I:= I downto 1 do
  begin
    if S[I] in WhiteSpaces then
      Break;
  end;
  Result:= Copy(S, I+1, length(S)-I+1);
end;

{ TfrmComplete }

procedure TfrmComplete.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  Inherited;
  // Dropshadow no Windows XP
  if ((Win32Platform and VER_PLATFORM_WIN32_NT) <> 0)
    and (Win32MajorVersion > 4) and (Win32MinorVersion > 0) then
    Params.WindowClass.style := Params.WindowClass.style or CS_DROPSHADOW;

  Params.Style:= (Params.Style or WS_POPUP) and (not WS_DLGFRAME);
  Params.ExStyle := WS_EX_TOOLWINDOW;
end;

constructor TfrmComplete.Create;
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
      if OldCreateOrder then DoCreate;
  finally
    GlobalNameSpace.EndWrite;
  end;

  left:= 0;
  top:= 0;

  BorderWidth:= 0;
  fEditor:= nil;
  width:= 220;
  height:= 160;
  KeyPreview:= true;
  FormStyle:= fsStayOnTop;

  flbOptions := TListBox.Create(Self);
  with flbOptions do
  begin
    Parent := Self;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    BorderStyle := bsNone;
    TabOrder := 0;
    Left:= 0;
    Top:= 0;
  end;

  fItemHeigth:= lbOptions.Canvas.TextHeight('XVY\|A&¨#!@^/´"*@§]{%');
  if fItemHeigth < 13 then
    fItemHeigth:= 13;
  lbOptions.ItemHeight:= fItemHeigth;

  OnResize:= ResizeListBox;
  OnCanResize:= FormCanResize;
  lbOptions.OnClick:= FocusEditor;
  OnClick:= FocusEditor;
end;

procedure TfrmComplete.FocusEditor(Sender: TObject);
begin
  if Assigned(fEditor) then
    if fEditor.CanFocus then
      fEditor.SetFocus
    else
      Close;
end;

procedure TfrmComplete.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewHeight < 120 then
    NewHeight:= 130;
  NewHeight:= NewHeight - (NewHeight mod fItemHeigth) -3;
  if NewWidth < 160 then
    NewWidth:= 160;
  Resize:= true;
end;

procedure TfrmComplete.ResizeListBox(Sender: TObject);
begin
  lbOptions.Width:= Width-9;
  lbOptions.Height:= Height-9;
end;

{ TNotesCodeCompletitionDlg }

constructor TNotesCodeCompletitionDlg.Create;
begin
  inherited;
  fList:= TStringList.Create;
  fForm:= TfrmComplete.Create(Self);
  fForm.OnClose:= HandleFormClose;
  fForm.lbOptions.OnDblClick:= ListDblClick;
  fTimer:= TTimer.Create(self);
  fTimer.Interval:= 100;
  fTimer.Enabled:= false;
  fTimer.OnTimer:= DoTimer;
end;

destructor TNotesCodeCompletitionDlg.Destroy;
begin
  fList.Free;
  inherited;
end;

procedure TNotesCodeCompletitionDlg.ClearLists;
begin
  fList.Clear;
  fForm.lbOptions.Clear;
end;

procedure TNotesCodeCompletitionDlg.Execute(const Editor: TSynEdit);
var
  I, ItemWidth: integer;
  P: TPoint;
begin
  if Assigned(Editor) then
  begin
    fEditor:= Editor;
    fForm.Editor:= Editor;
    SelectCompletitionItem;

    fForm.Height:= 160;
    ItemWidth:= 300;
    for I := 0 to fForm.lbOptions.Items.Count - 1 do
      if (fForm.lbOptions.Canvas.TextWidth(fForm.lbOptions.Items[I]) > ItemWidth) then
        ItemWidth := fForm.lbOptions.Canvas.TextWidth(fForm.lbOptions.Items[I]) + 30;
    if (ItemWidth > 200) and (ItemWidth < 400) then
      fForm.Width:= ItemWidth
    else if ItemWidth >= 400 then
      fForm.Width:= 400;

    P:= fEditor.RowColumnToPixels(fEditor.DisplayXY);
    P:= fEditor.ClientToScreen(P);
    fForm.left:= P.X;
    fForm.top:= P.Y + fEditor.LineHeight + 1;

    // Mostra a janela sem ativar. Outra possibilidade é usar fEditor.setFocus
    // logo após ativar a janela, mas não fica tãããooo bom...
    ShowWindow(fForm.Handle, SW_SHOWNOACTIVATE);
    fForm.Visible:= true;
    fEditor.RegisterCommandHandler(EditorCommandHandler, self);
    fEditor.AddKeyDownHandler(EditorKeyDownHandler);
    fEditor.AddMouseDownHandler(EditorMouseDownHandler);
    fTimer.Enabled:= true;

    // !! IMPORTANTE
    // Sem parar as shortcuts do Notes, nada vai funcionar
    // Lembrar de restaurá-las no onclose do form
    stopNotesShortcuts(self);
  end;
end;

function TNotesCodeCompletitionDlg.GetSelectedItem: integer;
begin
  result:= fForm.lbOptions.ItemIndex;
end;

function TNotesCodeCompletitionDlg.GetSelItemText: string;
Var
  I: integer;
begin
  I:= GetSelectedItem;
  if I > -1 then
  begin
    if fList.Count > I then
      Result:= fList.Strings[I]
    else
      Result:= fForm.lbOptions.Items.Strings[I];
  end;
end;

procedure TNotesCodeCompletitionDlg.HandleFormClose(Sender: TObject; var Action: TCloseAction);
begin
  fTimer.Enabled:= false;

  // !! IMPORTANTE
  restoreNotesShortcuts(self);

  if Assigned(fEditor) then
  begin
    fEditor.UpdateCaret;
    FEditor.Update;
    fEditor.UnregisterCommandHandler(EditorCommandHandler);
    fEditor.RemoveKeyDownHandler(EditorKeyDownHandler);
    fEditor.RemoveMouseDownHandler(EditorMouseDownHandler);
  end;
  ClearLists;
  fForm.Editor:= nil;
  fEditor:= nil;
end;

procedure TNotesCodeCompletitionDlg.SelectCompletitionItem;
var
  S: string;
  I, Len: integer;
  found: boolean;
begin
  if not Assigned(fEditor) then Exit;
  if fForm.lbOptions.Count < 1 then Exit;

  S:= AnsiLowerCase(Trim(getCurWord(fEditor)));
  Len:= length(S);
  if Len = 0 then
  begin
    fForm.lbOptions.ItemIndex:= -1;
    Exit;
  end;

  found:= false;
  for I:= 0 to fForm.lbOptions.Count -1 do
  begin
    if AnsiLowerCase(Copy(fForm.lbOptions.Items[I], 1, Len)) = S then
    begin
      found:= true;
      Break;
    end;
  end;

  if found then
    fForm.lbOptions.ItemIndex:= I
  else
    fForm.lbOptions.ItemIndex:= -1;
end;

procedure TNotesCodeCompletitionDlg.Close;
begin
  if fForm.Visible then
    fForm.Close;
end;

procedure TNotesCodeCompletitionDlg.EditorCommandHandler(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean; var Command: TSynEditorCommand;
  var AChar: char; Data: pointer; HandlerData: pointer);
begin
  if AfterProcessing then
  begin
    if ( (Command >= ecSelLeft) and (Command <= ecScrollRight) ) or
       ( (Command = ecChar) and (AChar in WhiteSpaces) and
         (AChar in [#9, #10, #13, #32] = false) ) then
      fForm.Close
    else
      SelectCompletitionItem;
  end;
end;


procedure TNotesCodeCompletitionDlg.DoTimer(Sender: TObject);
begin
  if fForm.Visible then
  begin
    if not Application.Active then
    begin
      fForm.Close;
      Exit;
    end;

    if fEditor.Handle <> getFocus then
    begin
      // Se o usuário inventar de mandar o foco para qualquer outro lugar
      if (fForm.Handle <> getFocus) and (fForm.lbOptions.Handle <> getFocus) then
      begin
        fTimer.Enabled:= false;
        fForm.Close;
        Exit;
      end;

      if fEditor.CanFocus then
        fEditor.SetFocus
      else
      begin
        fTimer.Enabled:= False;
        fForm.Close;
      end;
    end;
  end else
    fTimer.Enabled:= false;
end;

procedure TNotesCodeCompletitionDlg.EditorKeyDownHandler(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  DisplayedItemsCount: integer;
begin
  if (fForm.Visible) and (Assigned(fEditor)) then
  begin
    DisplayedItemsCount:= 1;
    if (fForm.Height > 0) and (fForm.ListItemHeigth > 0) then
      DisplayedItemsCount:= fForm.Height div fForm.ListItemHeigth -1;

    case Key of
     VK_BACK: fEditor.ExecuteCommand(501, #0, nil);
     VK_LEFT: fEditor.ExecuteCommand(1, #0, nil);
     VK_RIGHT: fEditor.ExecuteCommand(2, #0, nil);
     VK_DELETE: fEditor.ExecuteCommand(502, #0, nil);
     VK_PRIOR:
          begin
            if fForm.lbOptions.ItemIndex - DisplayedItemsCount > -1 then
              fForm.lbOptions.ItemIndex:= fForm.lbOptions.ItemIndex - DisplayedItemsCount
            else if fForm.lbOptions.Count > 0 then
              fForm.lbOptions.ItemIndex:= 0;
          end;
     VK_NEXT:
          begin
            if fForm.lbOptions.ItemIndex + DisplayedItemsCount < fForm.lbOptions.Count then
              fForm.lbOptions.ItemIndex:= fForm.lbOptions.ItemIndex + DisplayedItemsCount
            else
              fForm.lbOptions.ItemIndex:= fForm.lbOptions.Count -1;
          end;
     VK_UP:
        begin
          Key:= 0;
          if fForm.lbOptions.ItemIndex -1 > -1 then
            fForm.lbOptions.ItemIndex:= fForm.lbOptions.ItemIndex -1;
        end;
     VK_DOWN:
        begin
          Key:= 0;
          if fForm.lbOptions.ItemIndex + 1 < fForm.lbOptions.Count then
            fForm.lbOptions.ItemIndex:= fForm.lbOptions.ItemIndex + 1;
        end;
      VK_RETURN, VK_SPACE, VK_TAB:
        begin
          Key:= 0;
          DoCompletition;
        end;
      VK_ESCAPE:
        begin
          Key:= 0;
          fForm.Close;
        end;
      VK_HOME:
        begin
          Key:= 0;
          if fForm.lbOptions.Count > 0 then
            fForm.lbOptions.ItemIndex:= 0;
        end;
      VK_END:
        begin
          Key:= 0;
          if fForm.lbOptions.Count > 0 then
            fForm.lbOptions.ItemIndex:= fForm.lbOptions.Count -1;
        end;
    end;
  end;
end;

procedure TNotesCodeCompletitionDlg.DoCompletition;
var
  I: integer;
begin
  if not Assigned(fEditor) then Exit;
  I:= length(Trim(fEditor.LineText));
  fEditor.BeginUpdate;
  try
    if I > 0 then
    begin
      I:= length(getCurWord(fEditor));
      fEditor.CaretX:= fEditor.CaretX - I;
      fEditor.SelLength:= I;
      fEditor.SelText:= GetSelItemText;
    end else
    begin
      fEditor.SelText:= GetSelItemText;
    end;
  finally
    fEditor.EndUpdate;
  end;
  fForm.Close;
end;

procedure TNotesCodeCompletitionDlg.ListDblClick(Sender: TObject);
begin
  DoCompletition;
end;

procedure TNotesCodeCompletitionDlg.EditorMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if fForm.Visible then
    fForm.Close;
end;

function TNotesCodeCompletitionDlg.getVisibleList: TStrings;
begin
  result:= fForm.flbOptions.Items;
end;

end.
