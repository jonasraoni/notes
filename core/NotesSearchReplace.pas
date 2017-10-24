//
//    NotesSearchReplace - classe para localizar/substituir do editor
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
  @abstract(NotesSearchReplace - classe para localizar/substituir do editor.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  @author( Josimar Batista da Silva  <josimar.silva@me.com.br>)
*)
unit NotesSearchReplace;

interface

uses Classes, NotesSeeker, SynEdit, SysUtils, RegExpr;

type
  // Classe para fazer/busca e substituição no editor atual
  TNotesSearchReplace = Class(TObject)
  private
    fSearchRegex  : integer;
    fSearchStr    : string;
    fReplaceStr   : string;
    fPos          : integer;
    fAutoSel      : boolean;
    fWholeWords   : boolean;
    fWildCards    : boolean;
    fCaseSensitive: boolean;
    fRegExp       : boolean;
    fInSelection  : boolean;
    fOptions      : integer;
    FRegex        : TRegExpr;
    fSeeker       : TNotesSeeker;
  public
    destructor destroy; override;
    // Faz a busca. Se @code(Back) for true, faz de trás para frente.
    function  Search(back: boolean): boolean;
    // Faz substituição. Se @code(Back) for true, faz de trás para frente.
    function  Replace(back: boolean): boolean;
    // Substituí todas as ocorrências da SerachStr no texto. Retorna o número
    // de substituíções.
    function ReplaceAll: integer;
    // Busca a próxima ocorrência. Retorna true se encontrar algo.
    function Next: boolean;
    // Busca a ocorrência anterior (busca de trás para frente no texto).
    // Retorna true se encontrar algo.
    function Previus: boolean;
    // Chame quando não for mais precisar da classe dentro do procedimento
    // para liberar recursos. Limpa o texto atual, a posição e destrói
    // classes e componentes que são usados internamente.
    procedure Clear;
    // String a ser buscada no texto
    property SearchStr: string read fSearchStr write fSearchStr;
    property ReplaceStr: string read fReplaceStr write fReplaceStr;
    // Posição atual/ Posição do último resultado
    property CurPos: integer read fPos write fPos;
    // Se true, a classe seleciona automaticamente a string encontrada
    property AutoSelect: Boolean read fAutoSel write fAutoSel;
    // Diferencia maiúsculas de minúsculas
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    // Retorna apenas palavras inteiras
    property WholeWords: boolean read fWholeWords write fWholeWords;
    // Usa coringas
    property WildCards: boolean read fWildCards write fWildCards;
    // Proxima busca do regex
    property SearchRegex: integer read fSearchRegex write fSearchRegex;
    // Expressões regulares
    property RegExp: boolean read fRegExp write fRegExp;
    // Procura no texto selecionado
    property InSelection: boolean read fInSelection write fInSelection;
    // Todas as opções de pesquisa
    property Options: integer read fOptions write fOptions;
  end;

var
  SearchReplace: TNotesSearchReplace;

implementation

uses NotesGlobals, NotesEditorTab, Types, SynEditTypes, frm_Search;

{ TNotesSearchReplace }

procedure TNotesSearchReplace.Clear;
begin
  if Assigned(FSeeker) then
    FreeAndNil(FSeeker);
  fPos:= 0;
end;

destructor TNotesSearchReplace.destroy;
begin
  Clear;
  inherited;
end;

function TNotesSearchReplace.Next: boolean;
begin
  Result := Search(false);
end;

function TNotesSearchReplace.Previus: boolean;
begin
  Result:= Search(true);
end;

function TNotesSearchReplace.Replace(back: boolean): boolean;
begin
  Result:= false;
  if Search(back) then
  begin
    ActiveEditorTab.Editor.SelText:= fReplaceStr;
    Result:= true;
  end;
end;

function TNotesSearchReplace.ReplaceAll: integer;
Var
  P: TBufferCoord;
  F: Integer;
begin
  Result:= 0;
  if ActiveEditorTab = nil then Exit;
  if SearchStr = '' then Exit;

  if (fOptions and NOTES_SEARCH_REGEXP) <> 0 then
  begin
    //
  end
  else
  begin
    if Not Assigned(fSeeker) then
      fSeeker:= TNotesSeeker.Create;

    if not fInSelection then
      fSeeker.Text:= ActiveEditorTab.Editor.Text
    else
      fSeeker.Text:= ActiveEditorTab.Editor.SelText;

    fSeeker.SearchStr:= fSearchStr;
    fSeeker.EnableOptions(fCaseSensitive, fWholeWords, true, fWildCards, false);
    fSeeker.StartSearch;

    While fSeeker.Search do
    begin
      fSeeker.Replace(ReplaceStr);
      inc(Result);
    end;

    if Result = 0 then Exit;

    if fInSelection then
    begin
      ActiveEditorTab.Editor.SelText:= fSeeker.ReplacedText;
    end else
    begin
      P:= ActiveEditorTab.Editor.CaretXY;
      F:= ActiveeditorTab.Editor.TopLine;
      ActiveEditorTab.Editor.Text:= fSeeker.ReplacedText;
      ActiveeditorTab.Editor.TopLine:= F;
      ActiveEditorTab.Editor.CaretXY := P;
    end;
  end;
end;

function TNotesSearchReplace.Search(back: boolean): boolean;
Var
  C: cardinal;
begin
  Result:= false;
  if ActiveEditorTab = nil then Exit;
  if SearchStr = '' then Exit;

  if (fRegExp) then
  begin
    FRegex := TRegExpr.Create;
    FRegex.Expression := fSearchStr;

    if (FOptions and NOTES_SEARCH_CASESENSITIVE) = 0 then
      FRegex.ModifierI := true;

    if fInSelection then
    begin
      if FRegex.Exec(ActiveEditorTab.Editor.SelText) then
        fPos := FRegex.MatchPos[0] -1;
    end
    else
    begin
      if FRegex.Exec(ActiveEditorTab.Editor.Text) then
        fPos := FRegex.MatchPos[0] -1;
      if FSearchRegex > 1 then
        if FRegex.ExecNext then
            fPos := FRegex.MatchPos[0] -1
        else
          begin
            Exception.Create('Pesquisa finalizada.');
            exit;
          end;
(*
      if FSearchRegex > 1 then
        for I := 1 to FSearchRegex -1 do
          if FRegex.ExecNext then
            fPos := FRegex.MatchPos[0] -1
          else
          begin
            Exception.Create('Pesquisa finalizada.');
            exit;
          end;
*)
    end;

    Result:= true;

    if fAutoSel then
    begin
      if fInSelection then
        ActiveEditorTab.Editor.SelStart:= fPos + ActiveEditorTab.Editor.SelStart
      else
        ActiveEditorTab.Editor.SelStart:= fPos;
      ActiveEditorTab.Editor.SelLength:= FRegex.MatchLen[0];
    end;
  end
  else
  begin
    if Not Assigned(fSeeker) then
      fSeeker:= TNotesSeeker.Create;

    if fInSelection then
    begin
      If ActiveEditorTab.Editor.SelText = '' then
      begin
        Exception.Create('Nenhum conteudo foi selecionado para pesquisa.');
        Exit;
      end;

      fSeeker.Text    := ActiveEditorTab.Editor.SelText;
      fSeeker.Selection := True;
      fSeeker.StartAt := ActiveEditorTab.Editor.SelStart;
    end
    else
    begin
      fSeeker.Text:= ActiveEditorTab.Editor.Text;

      if not Back then
        fSeeker.StartAt:= ActiveEditorTab.Editor.SelStart + 1
      else
        fSeeker.StartAt:= ActiveEditorTab.Editor.SelStart - 1;
    end;

    fSeeker.SearchStr:= fSearchStr;
    fSeeker.EnableOptions(fCaseSensitive, fWholeWords, true, fWildCards, Back);
    fSeeker.StartSearch;

    if fSeeker.Search then
    begin
      C:= High(Integer);
      if fSeeker.CurByte < C then
        fPos:= fSeeker.CurByte
      else
        Exception.Create('O Notes econtrou a string, mas o editor não pode mostrá-la por que o texto é muito grande.');

      Result:= true;

      if fAutoSel then
      begin
        ActiveEditorTab.Editor.SelStart:= fPos;
        ActiveEditorTab.Editor.SelLength:= fSeeker.MatchLen;
      end;
    end;
  end;
end;

initialization
  SearchReplace:= TNotesSearchReplace.Create;

finalization
  SearchReplace.Free;

end.
