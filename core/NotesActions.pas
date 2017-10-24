//
//    NotesActions - classe para gerenciamento de TActions
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
  @abstract(NotesActions - classe para gerenciamento de TActions.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesActions;

interface

uses Windows, SysUtils, Classes, ActnList, Menus, Contnrs;

type
  { Condições para habilitar/desabilitar as ações:
    * ecNotReadonly - habilita se não for read-only
    * ecHasSelection - habilita se tiver texto selecionado
    * ecHasFileName -  habilita se o arquivo estiver salvo no HD
    * ecHasContent - habilita se tiver algum conteúdo no editor
    * ecHasEditorTab - habilita se a tab atual for um editor
    * ecCanUndo - habilita se puder desfazer
    * ecCanRedo - habilita se puder refazer
    * ecMacroLoaded - pode salvar/executar a macro temporária gravada }
  TNotesActionEnableCondition = ( ecNotReadonly, ecHasSelection, ecHasFileName,
    ecHasContent, ecHasEditorTab, ecCanUndo, ecCanRedo, ecHasProject, ecCanPaste,
    ecMacroLoaded );
  TNotesActionEnableConditions = set of TNotesActionEnableCondition;

  { Categorias das ações }
  TNotesActionCategory = ( mcNone, mcFile, mcEdit, mcSearch, mcView, mcProject,
    mcRun, mcCommands, mcOptions, mcHelp, mcEditorCmds );

  { Classe capaz de controlar uma TBasicAction para que ela
    seja habilitada/desabilitada segundo uma série de condições. }
  TActionControllerItem = class(TObject)
  private
    fAction: TAction;
    fEnableConditions: TNotesActionEnableConditions;
  public
    // Checa as condições e habilita/desabuilita a classe de acordo
    procedure CheckConditions(Conditions: TNotesActionEnableConditions);
    // Ação que ela vai controlar
    property Action: TAction read fAction write fAction;
    // Condições para habilitar a ação
    property EnableConditions: TNotesActionEnableConditions read fEnableConditions write fEnableConditions;
  end;

  { Classe usada para criar ações/items de menus em run-time e
    para controlar o estado habilitado/desabilitado das ações
    do Notes }
  TNotesActionManager = class(TObject)
  private
    fList: TObjectList;
    fActionList: TActionList;
    fFileMn: TMenuItem;
    fEditMn: TMenuItem;
    fSearchMn: TMenuItem;
    fViewMn: TMenuItem;
    fProjectMn: TMenuItem;
    fRunMn: TMenuItem;
    fCmdMn: TMenuItem;
    fOptMn: TMenuItem;
    fHelpMn: TMenuItem;
    function getCorrectName(AName, ACaption: string; Comp: TComponent): string;
  public
    constructor Create(ActionList: TActionList; FileMn, EditMn, SearchMn, ViewMn, ProjectMn, RunMn, CmdMn, OptMn, HelpMn: TMenuItem);
    destructor Destroy; override;
    // Passa a controlar a ação passada
    procedure ControlAction(Action: TAction; Conditions: TNotesActionEnableConditions);
    // Pára de controlar a ação passada
    procedure UnControlAction(Action: TAction);
    // Habilita/Desabilita as ações dependendo das condições
    procedure CheckConditions(Conditions: TNotesActionEnableConditions);
    // Desabilita todas as ações controladas
    procedure DisableAll;
    // Habilita todas as ações
    procedure EnableAll;
    // Cria uma nova ação "controlada" e adiciona a TActionList do Notes
    function NewNotesAction(AName, ACaption, AHint: string; Category: TNotesActionCategory; AOnExecute: TNotifyEvent): TAction;
    // Destróia uma ação criada com NewNotesAction
    procedure DeleteNotesAction(Action: TAction);
    // Cria um novo menu item controlado. O item será anexado ao ParentItem passado OU ao menu do Notes correspondente a categoria passada
    function NewNotesMenuItem(Name, Caption, Hint: string; Category: TNotesActionCategory; AOnClick: TNotifyEvent; ParentItem: TMenuItem): TMenuItem;
    // Use para criar um parent para submenus.
    function NewNotesMenuItemParent(Name, Caption: string; Category: TNotesActionCategory; ParentItem: TMenuItem): TMenuItem;
    // Deleta um Menuitem criado com NewNotesMenuItem ou NewNotesMenuItemParent
    procedure DeleteNotesMenuItem(Mi: TMenuItem);
  end;

  // Converte uma categoria de ação para string
  function  ActionCategoryToString(Category: TNotesActionCategory): string; forward;
  // Converte uma string para a categoria de uma ação
  function  StringToActionCategory(S: string): TNotesActionCategory;

Var
  ActionManager: TNotesActionManager;

implementation

function  ActionCategoryToString(Category: TNotesActionCategory): string;
begin
  Result:= '';
end;

function  StringToActionCategory(S: string): TNotesActionCategory;
begin
  Result:= mcNone;
end;


{ TActionControllerItem }

procedure TActionControllerItem.CheckConditions(
  Conditions: TNotesActionEnableConditions);
begin
  fAction.Enabled:= fEnableConditions <= Conditions;
end;

{ TNotesActionManager }

procedure TNotesActionManager.CheckConditions(
  Conditions: TNotesActionEnableConditions);
Var
  I: integer;
begin
  for I:= 0 to fList.Count - 1 do
    (fList.Items[I] As TActionControllerItem).CheckConditions(Conditions);
end;

procedure TNotesActionManager.ControlAction(Action: TAction;
  Conditions: TNotesActionEnableConditions);
Var
  a: TActionControllerItem;
begin
  a:= TActionControllerItem.Create;
  a.Action:= Action;
  a.EnableConditions:= Conditions;
  fList.Add(a);
end;

constructor TNotesActionManager.Create(ActionList: TActionList; FileMn,
  EditMn, SearchMn, ViewMn, ProjectMn, RunMn, CmdMn, OptMn,
  HelpMn: TMenuItem);
begin
  fFileMn:= FileMn;
  fEditMn:= EditMn;
  fSearchMn:= SearchMn;
  fViewMn:= ViewMn;
  fProjectMn:= ProjectMn;
  fRunMn:= RunMn;
  fCmdMn:= CmdMn;
  fOptMn:= OptMn;
  fHelpMn:= HelpMn;
  fActionList:= ActionList;

  fList:= TObjectList.Create(true);
end;

procedure TNotesActionManager.DeleteNotesAction(Action: TAction);
Var
  I: integer;
begin
  if assigned(Action) then
  begin
    for I:= 0 to fList.Count -1 do
      if fList.Items[I] = Action then
      begin
        (fList.Items[I] As TActionControllerItem).Action.Free;
        fList.Delete(I);
        Exit;
      end;
  end;
end;

procedure TNotesActionManager.DeleteNotesMenuItem(Mi: TMenuItem);
begin
  if (assigned(Mi.Action)) and (Mi.Action is TAction) then
    self.DeleteNotesAction(Mi.Action as TAction);
  if assigned(Mi.Owner) then
    Mi.Owner.RemoveComponent(Mi);
  FreeAndNil(Mi);
end;

destructor TNotesActionManager.Destroy;
begin
  fList.Free;
  inherited;
end;

procedure TNotesActionManager.DisableAll;
Var
  I: integer;
begin
  for I:= 0 to fList.Count - 1 do
    if Assigned((fList.Items[I] As TActionControllerItem).Action) then
      (fList.Items[I] As TActionControllerItem).Action.Enabled:= false;
end;

procedure TNotesActionManager.EnableAll;
Var
  I: integer;
begin
  for I:= 0 to fList.Count - 1 do
    if Assigned((fList.Items[I] As TActionControllerItem).Action) then
      (fList.Items[I] As TActionControllerItem).Action.Enabled:= true;
end;

function TNotesActionManager.getCorrectName(AName, ACaption: string;
  Comp: TComponent): string;
Var
  I: integer;
  S: string;
begin
  Result:= AName;
  if not Assigned(Comp.Owner) then Exit;

  if AName <> '' then
  begin
    if Comp.Owner.FindComponent(AName) = nil then
      Exit;
    // se o nome já existe, adicionamos uma unidade ao final
    for I:= 1 to 10000 do
    begin
      if Comp.Owner.FindComponent(AName + IntToStr(I)) = nil then
      begin
        Comp.Name:= AName + IntToStr(I);
        Exit;
      end;
    end;
  end else
  begin
    if ACaption <> '' then
    begin
      // pegamos apenas os caracteres de 'a' a 'z'.
      for I:= 0 to length(ACaption) - 1 do
        if ACaption[I] in ['a'..'z', 'A'..'Z'] then
          S:= S + String(ACaption[I]);
    end else
      S:='a';

    // se o nome já existe, adicionamos uma unidade ao final
    for I:= 1 to 10000 do
    begin
      if Comp.Owner.FindComponent(S + IntToStr(I)) = nil then
      begin
        Comp.Name:= S + IntToStr(I);
        Exit;
      end;
    end;
  end;
end;

function TNotesActionManager.NewNotesAction(AName, ACaption, AHint: string;
  Category: TNotesActionCategory; AOnExecute: TNotifyEvent): TAction;
begin
  if assigned(fActionList.Owner) then
    Result:= TAction.Create(fActionList.Owner)
  else
    Result:= TAction.Create(fActionList);

  Result.Name:= getCorrectName( AName, ACaption, Result );
  Result.Caption:= ACaption;
  Result.Hint:= AHint;
  Result.Category:= ActionCategoryToString( Category );
  Result.OnExecute:= AOnExecute;
  Result.ActionList:= fActionList;
end;

function TNotesActionManager.NewNotesMenuItem(Name, Caption, Hint: string;
  Category: TNotesActionCategory; AOnClick: TNotifyEvent;
  ParentItem: TMenuItem): TMenuItem;
begin
  if Category in [mcNone, mcEditorCmds] then
    raise Exception.Create(ClassName +'NewNotesMenuItem: a menu item can not be in the none or in the EditorCommands category.');

  if assigned(fActionList.Owner) then
    Result:= TMenuItem.Create(fActionList.Owner)
  else
    Result:= TMenuItem.Create(fActionList);

  Result.Name:= Name;
  Result.Action:= NewNotesAction(Name, Caption, Hint, Category, AOnClick);

  if assigned(ParentItem) then
    ParentItem.Add(Result)
  else begin
    case Category of
      mcFile: fFilemn.Add(Result);
      mcEdit: fEditMn.Add(Result);
      mcSearch: fSearchMn.Add(Result);
      mcView: fViewMn.Add(Result);
      mcProject: fProjectMn.Add(Result);
      mcRun: fRunMn.Add(Result);
      mcCommands: fCmdMn.Add(Result);
      mcOptions: fOptMn.Add(Result);
      mcHelp: fHelpMn.Add(Result);
    end;
  end;
end;

function TNotesActionManager.NewNotesMenuItemParent(Name, Caption: string;
  Category: TNotesActionCategory; ParentItem: TMenuItem): TMenuItem;
begin
  if assigned(fActionList.Owner) then
    Result:= TMenuItem.Create(fActionList.Owner)
  else
    Result:= TMenuItem.Create(fActionList);

  Result.Name:= Name;
  Result.Caption:= Caption;

  if assigned(ParentItem) then
    ParentItem.Add(Result)
  else begin
    case Category of
      mcFile: fFilemn.Add(Result);
      mcEdit: fEditMn.Add(Result);
      mcSearch: fSearchMn.Add(Result);
      mcView: fViewMn.Add(Result);
      mcProject: fProjectMn.Add(Result);
      mcRun: fRunMn.Add(Result);
      mcCommands: fCmdMn.Add(Result);
      mcOptions: fOptMn.Add(Result);
      mcHelp: fHelpMn.Add(Result);
    end;
  end;

end;

procedure TNotesActionManager.UnControlAction(Action: TAction);
Var
  I: integer;
begin
  if assigned(Action) then
  begin
    for I:= 0 to fList.Count -1 do
      if fList.Items[I] = Action then
      begin
        fList.Delete(I);
        Exit;
      end;
  end;
end;

end.
