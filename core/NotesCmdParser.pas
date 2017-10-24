//
//    NotesCmdParser - parser para a linha de comando do Notes
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
  @abstract(NotesCmdParser - parser para a linha de comando do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Esta unit possuí a classe @link(TNotesCmdParser) que é usada pelo Notes
  para "entender" as instruções da linha de comando. A classe é usada
  tanto para pegar a linha de comando passada pelo usuário quando o Notes
  é iniciado quanto a linha de comando advinda de outras instâncias que
  o usuário tentar rodar. Ou seja, toda a vez que o usuário tentar abrir
  um arquivo no Notes pelo explorer, será este parser que entenderá a linha
  de comando :)
  
*)
unit NotesCmdParser;

interface

uses classes, sysutils, NotesUtils;

type
 // Parser para a linha de comando.
 TNotesCmdParser = class(TObject)
 private
   fFilesToOpen: TStrings;
   fTemplateToOpen: string;
   fFileToPrint: string;
   fFileToShowInfo: string;
   fSwitchToProfile: string;
   fLastPos: integer;
   fCmd: string;
   fCmdLen: integer;
   fCurStr: string;
   procedure setCmd(Value: string);
   function EspecialTrim(const S: string): string;
   function Next: boolean;
 public
   constructor create;
   destructor destroy; override;
   { Chame @code(parse) para interpretar a linha de comando.}
   procedure Parse;
   // linha de comando passada para o Notes
   property Cmd:string read fCmd write setCmd;
   { depois que o parser for executado, contém os arquivos
    que deverão ser abertos pelo Notes.}
   property FilesToOpen: TStrings read fFilesToOpen;
   { Contém o arquivo que deve ser aberto no notes como um template.}
   property TemplateToOpen: string read FTemplateToOpen;
   { Contém o arquivo que deve ser impresso no Notes.}
   property FileToPrint: string read FFileToPrint;
   { Contém o arquivo para o qual deve msotrar informações e estatísticas.}
   property FileToShowInfo: string read FFileToShowInfo;
   { Contém o nome do profile para o qual o Notes deverá mudar assim que iniciar.}
   property SwitchToProfile: string read FSwitchToProfile;
 end;

// função do windows para pegar a linha de comando de um aplicativo
function GetCommandLine: PChar; stdcall;
  external 'kernel32.dll' name 'GetCommandLineA'; 

implementation

{ TNotesCmdParser }

constructor TNotesCmdParser.create;
begin
  inherited;
  fFilesToOpen:= TStringList.Create;
end;

destructor TNotesCmdParser.destroy;
begin
  fFilesToOpen.Free;
  inherited;
end;

function TNotesCmdParser.EspecialTrim(const S: string): string;
begin
  Result:= StringReplace(Trim(S), '"', '', [rfReplaceAll]);
end;

function TNotesCmdParser.Next: boolean;
Var
  InQuotes: boolean;
  I: integer;
begin
  result:= false;
  inQuotes:= false;

  for I:= fLastPos+1 to fCmdLen do
  begin
    if fCmd[I] = '"' then
    begin
      inQuotes :=  not inQuotes
    end else
    if (fCmd[I]= ' ') and (inQuotes = false) then
    begin
      fCurStr:= EspecialTrim(Copy(fCmd, fLastPos, I-fLastPos));
      fLastPos:= I;
      Result:= true;
      Exit;
    end else
    if (I >= fCmdLen-1) and (I > fLastPos) then
    begin
      fCurStr:= EspecialTrim(Copy(fCmd, fLastPos, fCmdLen));
      fLastPos:= I;
      Result:= true;
      Exit;
    end;
  end;
end;

procedure TNotesCmdParser.Parse;
begin
  fFilesToOpen.Clear;
  fTemplateToOpen:= '';
  fFileToPrint:= '';
  fFileToShowInfo:= '';
  fSwitchToProfile:= '';

  if fCmdLen = 0 then Exit;

  // CHAMAMOS NEXT UMA VEZ PARA IGNORAR O NOME DO APLICATIVO!
  Next;

    While Next do
    begin
      // -t --template
      if (SameText(fCurStr, '-t')) or (SameText(fCurStr, '--template')) then
      begin
        if Next then
          if FileExists(fCurStr) then
            fTemplateToOpen:= fCurStr;
      end else
      // -i --info
      if (SameText(fCurStr, '-i')) or (SameText(fCurStr, '--info')) then
      begin
        if Next then
          if FileExists(fCurStr) then
            fFileToShowInfo:= fCurStr;
      end else
      // -p --print
      if (SameText(fCurStr, '-p')) or (SameText(fCurStr, '--print')) then
      begin
        if Next then
          if FileExists(fCurStr) then
            fFileToPrint:= fCurStr;
      end else
      // -sp --switchtoprofile
      if (SameText(fCurStr, '-sp')) or (SameText(fCurStr, '--switchtoprofile')) then
      begin
        if Next then
          fSwitchToProfile:= fCurStr;
      end else
      // else: é arquivo :)
      begin
        if FileExists(fCurStr) then
          fFilesToOpen.Add(fCurStr);
      end;

    end;

end;

procedure TNotesCmdParser.setCmd(Value: string);
begin
  fCmd:= Value;
  fCmdLen:= length(Value);
  fLastPos:= 0;
end;

end.
