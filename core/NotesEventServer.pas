//
//    NotesEventServer - servidor de eventos do Notes
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
  @abstract(NotesEventServer - servidor de eventos do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesEventServer;

interface

uses  SysUtils, Classes;

{
 Eventos que podem ser capturados
 (os nomes são sempre passado em letras minúsculas,
 não usei minúsculas aqui para facilitar a leitura)

NotesLoad(CommandLine)
NotesUnload
FileCreated(FileType)
FileClosed(FileName)
BeforeFileOpen(fileName)
AfterFileOpen(fileName)
AfterFileSave(fileName)
BeforeFileSave(fileName)
FileTypeChanged(filetype)
TabChanged
EditorSelChanged
CharPressed(Key) // character pressionado
keysPressed(Shortcut) // shortcut = 'CTRL+A', 'SHIFT+ALT+6', etc.
}

type
  // Tipo de procedimento usado para "ouvir" eventos quando eles acontecem
  TNotesEventListnerHandler = procedure(Sender: TObject; const Event, EventData: string) of object;

  // Servidor de eventos do Notes
  // Uma classe/componente "cliente" registra um procedimento para que possa
  // "ouvir" os eventos que ocorrem no Notes. A cada evento que acontece (para
  //  isto alguma outra classe chama o método NotifyEvent) o método registrado
  // pela classe é chamado e recebe o nome do evento e quem enviou. A classe
  // deve lembrar de remover o procedimento usado para ouvir eventos quando
  // for destruída.
  TNotesEventServer= class(TObject)
  private
    fItems: array of TNotesEventListnerHandler;
    fCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Registra um procedimento que irá ouvir os eventos do Notes
    procedure RegisterEventListner(evthandler: TNotesEventListnerHandler);
    // Remove o procedimento (use ao destruir a classe que estava recebendo eventos)
    procedure RemoveEventListner(evthandler: TNotesEventListnerHandler);
    // Notifica o servidor que um evento ocorreu
    procedure NotifyEvent(Sender: TObject; const Event, EventData: string);
  end;

var
  // Objeto servidor de eventos
  EventServer: TNotesEventServer;

implementation

constructor TNotesEventServer.Create;
begin
  fCount:= 0;
end;

destructor TNotesEventServer.Destroy;
begin
  setLength(fItems, 0);
end;

procedure TNotesEventServer.RegisterEventListner(evthandler: TNotesEventListnerHandler);
begin
  inc(fCount);
  setLength(fItems, fCount);
  fItems[fCount - 1]:= evthandler;
end;

procedure TNotesEventServer.RemoveEventListner(evthandler: TNotesEventListnerHandler);
var
  i: integer;
begin
  for i:= 0 to fCount -1 do
    if addr(fItems[i]) = addr(evthandler) then
    begin
      fItems[fCount - 1]:= fItems[i];
      Dec(fCount);
      setLength(fItems, fCount);
      Exit;
    end;
end;

procedure TNotesEventServer.NotifyEvent(Sender: TObject; const Event, EventData: string);
var
  i: integer;
  eh: TNotesEventListnerHandler;
begin
  eh:= nil;
  for i:= 0 to fCount -1 do
  begin
    eh:= fItems[i];
    if assigned(eh) then
      eh(Sender, Event, EventData);
  end;
end;

initialization
  EventServer:= TNotesEventServer.Create;
finalization
  EventServer.Free;

end.

