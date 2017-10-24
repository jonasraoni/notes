//
//    NotesStartPage - StartPage do Notes
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
  @abstract(NotesStartPage - StartPage do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  A classe TNotesStartPage usa a tecnologia XG (Xml Guis) para mostra a
  StartPage do Notes. O código da gui da StartPage fica no diretório
  chrome do Notes. A classe TNotesStartPage apenas carrega o XG
  e completa as informações necessárias (listas de arquivos recentes,
  etc.).
*)
unit NotesStartPage;

interface

uses Windows, SysUtils, Classes, Controls, XG, XGUtils;

type

  TNotesOpenMruItem = procedure (FileToOpen: string; const IsProject: boolean) of object;

  // StartPage do Notes
  TNotesStartPage = class(TComponent)
  private
    fXG: TXGFrame;
    fOnNewProject: TNotifyEvent;
    fOnOpenProject: TNotifyEvent;
    fOnNewFile: TNotifyEvent;
    fOnOpenFile: TNotifyEvent;
    fOpenMru: TNotesOpenMruItem;
  public
    constructor Create(Owner: TComponent); override;
    // Cahmae para atualiza a StartPage passando os MRUs
    procedure ReloadStartPage(Files, Projects: TStrings);
    procedure getLinksClick(Sender: TObject; const FileName: string; var CanLoad: boolean);
  published
    // Função disponível para ser usada pelo arquivo XG
    procedure DoOpenFile(Sender: TObject);
    // Função disponível para ser usada pelo arquivo XG
    procedure DoNewFile(Sender: TObject);
    // Função disponível para ser usada pelo arquivo XG
    procedure DoOpenProject(Sender: TObject);
    // Função disponível para ser usada pelo arquivo XG
    procedure DoNewProject(Sender: TObject);
    // Passe a função do Notes que responderá para abrir arquivos da lista de MRU
    property  OnOpenMruItem: TNotesOpenMruItem read fOpenMru write fOpenMru;
    // Passe a função do Notes para "novo arquivo"
    property  OnRequestNewFile: TNotifyEvent read fOnNewFile write fOnNewFile;
    // Passe a função do Notes para "abrir arquivo"
    property  OnRequestOpenFile: TNotifyEvent read fOnOpenFile write fOnOpenFile;
    // Passe a função do Notes para "novo projeto"
    property  OnRequestNewProject: TNotifyEvent read fOnNewProject write fOnNewProject;
    // Passe a função do Notes para "abrir projeto"
    property  OnRequestOpenProject: TNotifyEvent read fOnOpenProject write fOnOpenProject;
  end;

implementation

uses XGComps;

{ TNotesStartPage }

constructor TNotesStartPage.Create(Owner: TComponent);
begin
  inherited;
  fXG:= TXGFrame.Create(Owner);
  fXG.Parent:= Owner As TWinControl;
  fXG.Align:= alClient;
  fXG.Bind:= self;
  fXG.OnOpenUri:= getLinksClick;
  fXG.AutoScroll:= false;
  fXG.loadFromFile('chrome://content/startpage.xg');
end;

procedure TNotesStartPage.DoNewFile(Sender: TObject);
begin
  if Assigned(fOnNewFile) then
    fOnNewFile(sender);
end;

procedure TNotesStartPage.DoNewProject(Sender: TObject);
begin
  if Assigned(fOnNewProject) then
    fOnNewProject(sender);
end;

procedure TNotesStartPage.DoOpenFile(Sender: TObject);
begin
  if Assigned(fOnOpenFile) then
    fOnOpenFile(sender);
end;

procedure TNotesStartPage.DoOpenProject(Sender: TObject);
begin
  if Assigned(fOnOpenProject) then
    fOnOpenProject(sender);
end;

procedure TNotesStartPage.getLinksClick(Sender: TObject;
  const FileName: string; var CanLoad: boolean);
var
  b: boolean;
begin
  CanLoad:= false;

  b:= false;
  if SameText(Copy((Sender As TComponent).Name, 1, 4), 'file') then
    b:= true;

  if Assigned(fOpenMru) then
    fOpenMru(FileName, b);
end;

procedure TNotesStartPage.ReloadStartPage(Files, Projects: TStrings);
Var
  I: integer;
  C: TComponent;
begin

  for I:= 0 to 10 do
  begin
    C:= fXG.FindComponent('file' + IntToStr(I+1));
    if (C is TXgHyperLink) then
    begin
      (C as TXgHyperLink).Enabled:= false;
      (C as TXgHyperLink).Caption:='--';
    end;
    C:= fXG.FindComponent('proj' + IntToStr(I+1));
    if (C is TXgHyperLink) then
    begin
      (C as TXgHyperLink).Enabled:= false;
      (C as TXgHyperLink).Caption:='--';
    end;
  end;

  if Assigned(Files) then
  begin
    for I:= 0 to Files.Count -1 do
    begin
      C:= fXG.FindComponent('file' + IntToStr(I+1));
      if (fXG.ComponentCount > 0) and (C <> nil) and (C is TXgHyperLink) then
      begin
        (C As TXgHyperLink).Caption:= ExtractFileName(files.Strings[I]);
        (C AS TXgHyperLink).Uri:= files.Strings[I];
        (C AS TXgHyperLink).Hint:= files.Strings[I];
        (C AS TXgHyperLink).Enabled:= true;
      end else
        Break;
    end;
  end;

  if Assigned(Projects) then
  begin
    for I:= 0 to Projects.Count -1 do
    begin
      C:= fXG.FindComponent('proj' + IntToStr(I+1));
      if (C <> nil) and (C is TXgHyperLink) then
      begin
        (C As TXgHyperLink).Caption:= ExtractFileName(Projects.Strings[I]);
        (C AS TXgHyperLink).Uri:= Projects.Strings[I];
        (C AS TXgHyperLink).Hint:= Projects.Strings[I];
        (C AS TXgHyperLink).Enabled:= true;
      end else
        Break;
    end;
  end;


end;

end.
