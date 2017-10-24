//
//    NotesTemplates - rotinas para trabalhar com os templates de arquivos
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
  @abstract(NotesTemplates - rotinas para trabalhar com os templates de arquivos.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Use através da variável @code(Templates).
*)

unit NotesTemplates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TNotesTemplates= class(TObject)
  public
    // Lista as categorias dos templates em um treeview. Passe a propriedade
    // @code(items) de um treeview em @code(nodes). Você pode especificar
    // um imageindex a ser usado (normalmente usamos para mostrar as categorias
    // com um ícone de pasta).
    procedure ListTemplatesCategories(const nodes: TTreeNodes; const imgIndex: integer);
    // Lista os templates. Ele pega a categoria atual do @code(node) passado
    function  ListTemplates(node: TTreeNode): TStrings;
    // Pega o caminho real no HD de um node.
    function  getNodePath(const node: TTreeNode): string;
    // retorna o tipo de arquivo de um arquivo de template
    function  getTemplateFileType(const FileName: string): string;
    // retorna a pasta de templates de um tipo de arquivo
    function  getFileTypeFileTemplatesFolder(FileType: string): string;
  end;

var
  Templates: TNotesTemplates;

implementation

uses NotesProfile, NotesUtils, NotesGlobals;

{ TNotesTemplates }

function TNotesTemplates.getFileTypeFileTemplatesFolder(
  FileType: string): string;
begin
  Result:= addSlash(NotesGlobals.NProfile.Paths.FileTypesDir) + FileType + '\templates\'; 
end;

function TNotesTemplates.getNodePath(const node: TTreeNode): string;
Var
  curNode: TtreeNode;
begin
  if node.Parent = nil then
  begin
    Result:= getFileTypeFileTemplatesFolder(node.Text);
  end else
  begin
     curNode:= node;
     repeat
       Result:= curNode.Text + '\' + Result;
       curNode:= curNode.Parent;
     until curNode.Parent = nil;
     Result:= getFileTypeFileTemplatesFolder(curNode.Text) + Result;
  end;
end;

function TNotesTemplates.getTemplateFileType(
  const FileName: string): string;
Var
  SA: TStringArray;
  I, Len: integer;
begin
  SA:= ExplodeStr(FileName, '\');
  Len:= length(SA);
  try
    for I:= 0 to Len -1 do
    begin
      if SameText(SA[I], 'FileTypes') then
      begin
        if Len > (I+1) then
        begin
          Result:= SA[I+1];
          break;
        end;
      end;
      if SameText(SA[I], 'Templates') then
      begin
        if I > 0 then
        begin
          Result:= SA[I-1];
          break;
        end;
      end;
    end;
  finally
    setLength(SA, 0);
  end;
end;

function TNotesTemplates.ListTemplates(node: TTreeNode): TStrings;
Var
  S: string;
  Tree: PNotesFolderTree;
  I: integer;
begin
  Result:= TStringList.Create;
  S:= Templates.getNodePath(node);
  if DirectoryExists(S) then
  begin
    Result.BeginUpdate;
    BuildFolderTree(Tree, S, ftOnlyFiles, false, '*', '*.ntpl');
    for I:= 0 to High(Tree^) do
    begin
      With Tree^[I] do
      begin
        Result.Add(ChangeFileExt(Path, ''));
      end;
    end;
    Result.EndUpdate;
    FreeFolderTree(Tree);
  end;
end;

procedure TNotesTemplates.ListTemplatesCategories(const nodes: TTreeNodes; const imgIndex: integer);
Var
  Tree: PNotesFolderTree;
  CatTree: PNotesFolderTree;
  i: integer;
  tplfolder: string;
  curNode: TTreeNode;

  procedure addCats(nodes: TTreeNodes; ParentNode: TTreeNode; cats: PNotesFolderTree; const imgIndex: integer);
  var
    I: integer;
    newnode: TTreeNode;
  begin
    for I:= 0 to High(cats^) do
    begin
      with cats^[I] do
      begin
        newnode:= nodes.AddChild(ParentNode, Path);
        newnode.ImageIndex:= imgIndex;
        newnode.SelectedIndex:= imgIndex;

        if isFolder then
          addCats(nodes, newnode, items, imgIndex);
      end;
    end;
  end;

begin
  NotesGlobals.NProfile.FileTypesMgr.ListTypes(Tree);

  for I:=0 to High(Tree^) do
  begin
    with Tree^[I] do
    begin
      if IsFolder then
      begin
        tplfolder:= getFileTypeFileTemplatesFolder( Path );
        if DirectoryExists( tplfolder ) then
        begin
          curNode:= nodes.AddChild( nil, Path );
          curNode.ImageIndex:= imgIndex;
          curNode.SelectedIndex:= imgIndex;
          BuildFolderTree( catTree, tplfolder, ftOnlyFolders, true );
          addCats( nodes, curNode, catTree, imgIndex );
          FreeFolderTree(catTree);
        end;
      end;
    end;
  end;
  FreeFolderTree(Tree);
end;

initialization
  Templates:= TNotesTemplates.Create;
finalization
  Templates.Free;
end.
