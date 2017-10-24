unit frm_EditShortcut;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls, Menus, ActnList;

type
  TfrmEditShotcut = class(TForm)
    lInstructions: TLabel;
    btOK: TButton;
    btCancel: TButton;
    cbShortcut: TComboBox;
    btClear: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbShortcutChange(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    fShortcut: TShortCut;
    fStartShortcut: TShortCut;
    fActions: TActionList;
    procedure setShortcut(const Value: TShortCut);
  public
    property ShortCut: TShortCut read fShortcut write setShortcut;
    property ActionList: TActionList read fActions write fActions;
  end;

// Mostra um diálogo que permite ao usuário editar a Shortcut
// passada em Shortcut. Se o usuário clicar em OK, retorna true.
// AShortcut só é modificado se o usuário clica em OK.
function ShowEditShortcutDialog(Var AShortcut: TShortCut; const anActionList: TActionList): boolean; forward;

implementation

{$R *.dfm}

uses NotesUtils, NotesFolderMenus;

const
  VK_0 = $30;
  VK_9 = $39;
  VK_A = $41;
  VK_Z = $5A;

function ShowEditShortcutDialog(Var AShortcut: TShortCut;
  const anActionList: TActionList): boolean;
begin
  Result:= false;
  With TfrmEditShotcut.Create(nil) do
  begin
    try
      Shortcut:= AShortcut;
      ActionList:= anActionList;
      ShowModal;
      if ModalResult= mrOk then
      begin
        Result:= true;
        AShortcut:= Shortcut;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmEditShotcut.setShortcut(const Value: TShortCut);
begin
  fShortcut := Value;
  cbShortcut.Text:= ShortcutToText( fShortcut );
end;

procedure TfrmEditShotcut.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Shift = []) or (Shift = [ssShift])) and
     (Key in
     [VK_0..VK_9, VK_A..VK_Z, VK_NUMPAD0..VK_NUMPAD9]) then begin
    Key := 0;
    Exit;
  end;
  if ((Key = VK_CONTROL) and (ssCtrl in Shift)) or
     ((Key = VK_SHIFT) and (ssShift in Shift)) or
     ((Key = VK_MENU) and (ssAlt in Shift)) then begin
    Key := 0;
    Exit;
  end;
  // As setas devem poder ser usadas para escolher os
  // items no combobox, para que seja possível selecionar
  // sem usar o mouse
  if (Key = VK_UP) or (Key = VK_DOWN) then Exit;
  // O Enter é usado para dar Ok/Cancelar. Assim, não
  // deve ser capturado
  if Key = VK_RETURN then Exit;
  fShortcut:= Menus.ShortCut(Key, Shift);
  cbShortcut.Text := ShortCutToText(fShortcut);
  Key:=0;
end;

procedure TfrmEditShotcut.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TfrmEditShotcut.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0;
  Shift:= [];
end;

procedure TfrmEditShotcut.cbShortcutChange(Sender: TObject);
begin
  if TextToShortcut(cbShortcut.Text) <> 0 then
  begin
    fShortcut:= TextToShortcut(cbShortcut.Text);
    cbShortcut.Text:= '';
  end;
  cbShortcut.Text:= ShortcutToText( fShortcut );
end;

procedure TfrmEditShotcut.btOKClick(Sender: TObject);
var
  I: integer;
  S: string;
begin
  if (fStartShortcut = fShortcut) or (fShortcut = 0) then
  begin
    modalresult:= mrOK;
    Exit;
  end;

  S:= folderMenusKeymap.getItemCaptionForShortcut(fShortcut);

  if (S = '') and (Assigned(fActions)) then
  begin
    for I:= 0 to fActions.ActionCount -1 do
    begin
      if TAction(fActions.Actions[I]).ShortCut = fShortcut then
      begin
        S:= TAction(fActions.Actions[I]).Caption;
        Break;
      end;
      if (S = '') and (TAction(fActions.Actions[I]).SecondaryShortCuts.Count > 0) then
      begin
        if TAction(fActions.Actions[I]).SecondaryShortCuts.IndexOfShortCut(fShortcut) > -1 then
        begin
          S:= TAction(fActions.Actions[I]).Caption;
          Break;
        end;
      end;
    end;
  end;

  if S <> '' then
  begin
    S:= StringReplace(S, '&', '', []);
    S:= StringReplace(S, '...', '', []);

    if msgYesNo('A tecla de atalho escolhida já está sendo usada na ação "'+S+'". Você quer usá-la mesmo assim?!', handle) = IDNo then
      Exit;

  end;

  ModalResult:= mrOK;
end;

procedure TfrmEditShotcut.FormShow(Sender: TObject);
begin
  fStartShortcut:= fShortcut;
end;

procedure TfrmEditShotcut.btClearClick(Sender: TObject);
begin
  fShortcut:= 0;
  cbShortcut.Text:= '';
end;

end.
