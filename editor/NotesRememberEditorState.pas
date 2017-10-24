//
//    NotesRememberEditorState - classes para lembrar o estado do editor para
//    um arquivo e para limpar os dados do cache
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
  @abstract(NotesRememberEditorState - classes para lembrar o estado do editor para um arquivo e para limpar os dados do cache.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesRememberEditorState;

interface

uses SysUtils, Classes, SynEdit, NotesEditorTab;

type
  // Salva/Carrega o estado do editor dependendo do arquivo que está aberto nele
  TNotesRemeberEditorState = class(TObject)
  private
    fEdTab: TNotesEditorTab;
  public
    constructor Create(const EditorTab: TNotesEditorTab);
    // Carrega o estado
    procedure LoadState;
    // Salva o estado
    procedure SaveState;
  end;

type
  // Limpa os dados para arquivos abertos a mais de "DaysOld"
  // Passe True no método create para poder modificar as propriedades
  // e depois chame o método Execute. Depois que Execute for chamado
  // a ação será feita em uma Threaad em background.
  TNotesEditorStateCleanUp = class(TThread)
  private
    fDaysOld: integer;
    fDir: string;
    fExt: string;
  public
    // Limpa os dados antigos
    procedure Execute; override;
    // número máximo de dias que os dados devem ter
    property DaysOld: integer read fDaysOld write fDaysOld;
    // pasta onde os dados estão gravados
    property EditorStateDir: string read fDir write fDir;
    // Extensão dos arquivos usados para guardar o estado do editor
    property EditorStateExt: string read fExt write fExt;
  end;

// Retorna o nome do arquivo que contém o estado do editor para um arquivo,
// mesmo que um arquivo de estado ainda não existe
function getStateFileForFile(const FileName: string): string;


implementation

uses NotesGlobals;

const
  NOTES_EDITOR_STATE_SIGLEN = 3;
  NOTES_EDITOR_STATE_SINATURE: string[NOTES_EDITOR_STATE_SIGLEN] = 'NES';

function getStateFileForFile(const FileName: string): string;
Var
  I: integer;
begin
  Result:= StringReplace(FileName, '/', '.', [rfReplaceall]);
  Result:= StringReplace(Result, ':\', '.', [rfReplaceall]);
  Result:= StringReplace(Result, ':/', '.', [rfReplaceall]);  
  Result:= StringReplace(Result, '\', '.', [rfReplaceall]);

  I:= length(Result);
  // se for mt grande o caminho, pegamos só os 100 primeiros caracteres
  if I > 100 then
    Result:= Copy(Result, I-100, I);

  Result:= NProfile.Paths.EditorStateDir + Result + NOTES_EDITOR_STATE_EXT;
end;


{ TNotesRemeberEditorState }

{
--------------------
  NSE
--------------------
topline
--------------------
line
--------------------
col
--------------------
marks count
--------------------
 marks
        x  ( mark value )
 count
-------------------------
}

constructor TNotesRemeberEditorState.Create(
  const EditorTab: TNotesEditorTab);
begin
  fEdTab:= EditorTab;
  if EditorTab = nil then
    raise Exception.Create(ClassName + '.Create - EditorTab can not be nil!!!');
end;

procedure TNotesRemeberEditorState.LoadState;
var
  fs: TFileStream;
  I, MCount, MarkValue: integer;
  S: string[NOTES_EDITOR_STATE_SIGLEN];
  FileN: string;
begin
  if not FileExists(fEdTab.FullPath) then Exit;
  FileN:= getStateFileForFile(fEdTab.FullPath);
  if not FileExists(FileN) then Exit;
  fs:= TFileStream.Create(FileN, fmOpenRead);
  try
    fs.Read(S, sizeOf(S));
    if S <> NOTES_EDITOR_STATE_SINATURE then
      raise Exception.Create(ClassName + '.LoadState - invalid signature! Can not load editor state for this file.');
    fs.Read(MCount, sizeOf(Integer));
    fEdTab.Editor.topline:= MCount;
    fs.Read(MCount, sizeOf(Integer));
    fEdTab.Editor.caretY:= MCount;
    fs.Read(MCount, sizeOf(Integer));
    fEdTab.Editor.caretX:= MCount;
    fs.Read(MCount, sizeOf(integer));
    for I:= 0 to MCount -1 do
    begin
      fs.Read(MarkValue, sizeOf(Integer));
      fEdTab.Marks.Add(MarkValue);
    end;
  finally
    fs.Free;
  end;
end;

procedure TNotesRemeberEditorState.SaveState;
var
  fs: TFileStream;
  I, MarkValue, Buf: integer;
  S: string[NOTES_EDITOR_STATE_SIGLEN];
begin
  if fEdTab.FileName = '' then Exit;
  fs:= TFileStream.Create(getStateFileForFile(fEdTab.FullPath), fmCreate);
  try
    S:= NOTES_EDITOR_STATE_SINATURE;
    fs.Write(S, sizeOf(S));
    Buf:= fEdTab.Editor.topline;
    fs.Write(Buf, sizeOf(Integer));
    Buf:= fEdTab.Editor.caretY;
    fs.Write(Buf, sizeOf(Integer));
    Buf:= fEdTab.Editor.caretX;
    fs.Write(Buf, sizeOf(Integer));
    Buf:= fEdTab.Marks.Count;
    fs.Write(Buf, sizeOf(integer));
    for I:= 0 to fEdTab.Marks.Count -1 do
    begin
      MarkValue:= fEdTab.Marks.items[I];
      fs.Write(MarkValue, sizeOf(Integer));
    end;
  finally
    fs.Free;
  end;
end;

{ TNotesEditorStateCleanUp }

procedure TNotesEditorStateCleanUp.Execute;
Var
  SR: TSearchRec;
  D: TDateTime;
begin
  inherited;
  // Se fDaysOld for 0, vai deletar todos os arquivos da pasta
  D:= Now - fDaysOld;
  if FindFirst(fDir + '*' + fExt, faAnyFile, SR) <> 0 then Exit;
  try
    repeat
      if FileDateToDateTime(SR.Time) <= D then
        DeleteFile(fDir + SR.Name);
      if Terminated then
        Exit;
    until FindNext(SR) <> 0;
  finally
    findClose(SR);
  end;
end;

end.
