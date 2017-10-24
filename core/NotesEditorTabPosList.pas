//
//    NotesEditorTabPosList - records e classes para guardar em conjunto
//                            o nome de um arquivo e uma posição nele.
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
  @abstract(NotesEditorTabPosList - records e classes para guardar em conjunto o nome de um arquivo e uma posição nele.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesEditorTabPosList;

interface

uses Classes;

type
  // Record genérico para guardar uma posição de
  // um arquivo com várias informações
  TNotesEditorTabPos = record
    // Linha
    Line: integer;
    // Coluna
    Col: integer;
    // Nome do arquivo com caminho completo
    FileName: string;
    // Informações adicionais
    Info: string;
    // Pointer para dados adicionais
    Data: Pointer;
  end;

  PTNotesEditorTabPos = ^TNotesEditorTabPos;

  // Tipo de array usado em @link(TNotesEditorTabPosList)
  TNotesEditorTabPosItems = array of TNotesEditorTabPos;

  // Lista de posições do tipo @link(TNotesEditorTabPos)
  TNotesEditorTabPosList= class(TObject)
  private
    fItems: TNotesEditorTabPosItems;
    fCount: integer;
    fLastPos: integer;
    fGoAsError: boolean;
    fMaxItems: integer;
    procedure Delete(Index: integer);
    function  get(index: integer): TNotesEditorTabPos;
    procedure put(index: integer; value: TNotesEditorTabPos);
    function getFirst: TNotesEditorTabPos;
    function getLast: TNotesEditorTabPos;
  public
    // Adiciona uma posição
    function  Add(ALine, ACol: integer; FileName: string; AInfo: string): Integer;
    // Adiciona informações a um item pré-existente
    procedure AddInfo(const index: integer; const AInfo: string);
    // Retorna o index do item
    function  IndexOf(Line: integer; FileName: string): integer; overload;
    // Remove o item
    function  Remove(Line: integer; FileName: string): Integer;
    // Vai para a próxima posição
    procedure gotoNext;
    // Volta para a posição anterior
    procedure gotoPrevius;
    // Ajusta a lista quando uma linha é adicionada ao editor
    procedure AdjustToLineAdded(Line: integer; FileName: string);
    // Ajusta a lista quando uma linha é removida do editor
    procedure AdjustToLineRemoved(Line: integer; FileName: string);
    // Troca o item especificado em Pos1 pelo item Pos2
    procedure Exchange(const Pos1, Pos2: integer);
    // Limpa todos os items que possúem o nome do arquivo passado
    procedure ClearItemsForFile(FileName: string);
    // Limpa a lista inteira
    procedure Clear;
    destructor destroy; override;
  public
    // Permite acessar os items guardados na classe.
    property Items[Index: Integer]: TNotesEditorTabPos read Get write Put; default;
    // Retorna o número de intems guradados
    property Count: integer read fCount;
    // Se @code(True), ao ir para linha ele irá marcar a linha como uma linha de erro.
    property GoToLineAsError: boolean read fGoAsError write fGoAsError;
    // Permite fixar um número máximo de items a serem guardados
    property MaxItems: integer read fMaxItems write fMaxItems;
    // Retonar o primeiro item da lista
    property First: TNotesEditorTabPos read getFirst;
    // Retonar o último item da lista
    property Last: TNotesEditorTabPos read getLast;
  end;


implementation

uses NotesGlobals, SysUtils;

{ TNotesEditorTabPosList }

function TNotesEditorTabPosList.Add(ALine, ACol: integer;
  FileName: string; AInfo: string): Integer;
begin
  if fCount = 0 then
    fLastPos:= -1;
  setLength(fItems, fCount +1);
  fItems[fCount].Line:= ALine;
  fItems[fCount].Col:= ACol;
  fItems[fCount].FileName:= FileName;
  fItems[fCount].Info:= AInfo;
  Result:= fCount;
  inc(fCount);

  if (fMaxItems > 0) and (fCount > fMaxItems) then
    Delete(0);

end;

procedure TNotesEditorTabPosList.AddInfo(const index: integer;
  const AInfo: string);
begin
  fItems[index].Info:= fItems[index].Info + AInfo;
end;

procedure TNotesEditorTabPosList.AdjustToLineAdded(Line: integer;
  FileName: string);
Var
  I: integer;
begin
  for I := 0 to fCount-1 do
  begin
    if (SameText(fItems[I].FileName, FileName)) and (fItems[I].Line > Line) then
      fItems[I].Line:= fItems[I].Line +1;
  end;
end;

procedure TNotesEditorTabPosList.AdjustToLineRemoved(Line: integer;
  FileName: string);
Var
  I: integer;
begin
  if fCount < 1 then Exit;
  Remove(Line, FileName);
  for I := 0 to fCount-1  do
  begin
    if (SameText(fItems[I].FileName, FileName)) and (fItems[I].Line > Line) then
      fItems[I].Line := fItems[I].Line  -1;
  end;
end;

procedure TNotesEditorTabPosList.Clear;
begin
  fCount:= 0;
  SetLength(fItems, 0);
end;

procedure TNotesEditorTabPosList.ClearItemsForFile(FileName: string);
Var
  I, J, C: integer;
begin
  for J:= 0 to fCount - 1 do
  begin
    C:= fCount;
    for I:= 0 to fCount - 1 do
    begin
      if SameText(fItems[I].FileName, FileName) then
      begin
        Delete(I);
        Break;
      end;
    end;
    // se nada tiver sido deletado, saímos
    if C = fCount then
      Exit;
  end;
end;

procedure TNotesEditorTabPosList.Delete(Index: integer);
Var
  I: integer;
begin
  if (Index < 0) or (index > pred(fcount)) then Exit;

  if fLastPos = Index then
    inc(fLastPos);
  if fLastPos > fCount -1 then
    fLastPos:= 0;

  if Index < (fCount - 1) then
    for I:= Index to fCount -2 do
      Exchange(I, I+1);

  Dec(fCount);
  SetLength(fItems, fCount);

  if fCount < 1 then
    fLastPos:= -1;
end;

destructor TNotesEditorTabPosList.destroy;
begin
  Clear;
  inherited;
end;

procedure TNotesEditorTabPosList.Exchange(const Pos1, Pos2: integer);
Var
  tmp: TNotesEditorTabPos;
begin
  tmp:= fItems[Pos1];
  fItems[Pos1]:= fItems[Pos2];
  fItems[Pos2]:= tmp;
end;

function TNotesEditorTabPosList.get(index: integer): TNotesEditorTabPos;
begin
  Result:= fItems[index];
end;

function TNotesEditorTabPosList.getFirst: TNotesEditorTabPos;
begin
  if fCount > 0 then
    Result:= fItems[0];
end;

function TNotesEditorTabPosList.getLast: TNotesEditorTabPos;
begin
  if fCount > 0 then
    Result:= fItems[fCount-1];
end;

procedure TNotesEditorTabPosList.gotoNext;
begin
  if fLastPos < 0 then Exit;
  if fCount = 0  then Exit;

  inc(fLastPos);

  if fLastPos >= fCount then
    fLastPos := 0;

  GoToEditorTabPos(fItems[fLastPos] ,fGoAsError)

end;

procedure TNotesEditorTabPosList.gotoPrevius;
begin
  if fCount = 0  then Exit;
  Dec(fLastPos);
  if fLastPos < 0 then
    fLastPos:= fCount -1;

  GoToEditorTabPos(fItems[fLastPos] ,fGoAsError);
end;

function TNotesEditorTabPosList.IndexOf(Line: integer;
  FileName: string): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I := 0 to fCount - 1 do
  begin
    if (fItems[I].Line = Line) and (SameText(fItems[I].FileName, FileName)) then
    begin
      Result:= I;
      Exit;
    end;
  end;
end;

procedure TNotesEditorTabPosList.put(index: integer; value: TNotesEditorTabPos);
begin
  fItems[index]:= Value;
end;

function TNotesEditorTabPosList.Remove(Line: integer;
  FileName: string): Integer;
Var
  I: integer;
begin
  Result:= -1;
  I:= IndexOf(Line, FileName);
  if I < 0 then Exit;
  Result:= I;

  Delete(I);
end;


end.
