//
//    NotesOptions - gravar/salvar opções do Notes
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
  @abstract(NotesOptions - gravar/salvar opções do Notes.)
  @author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
  Esta Unit possuí a classe @link(TNotesOptions) que deve ser
  usada para gravar e salvar as opções do Notes. Não confundir
  "opções" com "configurações". As opções representam o último
  estado das opções do usuário, como o tamanho da janela, 
  se os painéis estavam abertos ou não, se a janela estava
  maximizada, etc.
*)
unit NotesOptions;

interface

uses
  Classes;


type
  TNotesPanelLocation = (plLeft, plRight, plBottom);

type
  TNotesOptions = class
  private
    FShowStatusBar: Boolean;
    FMaximized: Boolean;
    FShowLeftPanel: Boolean;
    FShowRightPanel: Boolean;
    FShowBottomPanel: Boolean;
    FShowToolBar: Boolean;
    FLastLibFileType: string;
    FFormHeight: Integer;
    FFormWidth: Integer;
    FFormTop: Integer;
    FFormLeft: Integer;
    fLibraryLocation: TNotesPanelLocation;
    fProjectsLocation: TNotesPanelLocation;

  public
    procedure Save;
    procedure Load;

    property ShowStatusBar: Boolean read FShowStatusBar write FShowStatusBar;
    property ShowToolBar: Boolean read FShowToolBar write FShowToolBar;
    property LastLibFileType: string read FLastLibFileType write FLastLibFileType;
    property ShowLeftPanel: Boolean read FShowLeftPanel write FShowLeftPanel;
    property ShowRightPanel: Boolean read FShowRightPanel write FShowRightPanel;
    property ShowBottomPanel: Boolean read FShowBottomPanel write FShowBottomPanel;
    property Maximized: Boolean read FMaximized write FMaximized;
    property FormTop: Integer read FFormTop write FFormTop;
    property FormHeight: Integer read FFormHeight write FFormHeight;
    property FormLeft: Integer read FFormLeft write FFormLeft;
    property FormWidth: Integer read FFormWidth write FFormWidth;
    property LibraryLocation: TNotesPanelLocation read fLibraryLocation write fLibraryLocation;
    property ProjectsLocation: TNotesPanelLocation read fProjectsLocation write fProjectsLocation;
  end;

implementation

uses
  NotesGlobals, NotesUtils, NotesXML;

{ TNotesOptions }

procedure TNotesOptions.Load;
var
  XML: string;
begin
  XML := FileToStr( NProfile.Paths.OptionsFile );

  FShowStatusBar := ReadTagBool( XML, 'ShowStatusBar', True );
  FShowToolBar := ReadTagBool( XML, 'ShowToolBar', True );
  FShowLeftPanel := ReadTagBool( XML, 'ShowLeftPanel', True );
  FShowRightPanel := ReadTagBool( XML, 'ShowRightPanel', True );
  FShowBottomPanel := ReadTagBool( XML, 'ShowBottomPanel', True );
  FMaximized := ReadTagBool( XML, 'Maximized', False );
  FLastLibFileType := ReadTagStr( XML, 'LastLibFileType', '' );
  FFormLeft := ReadTagInt ( XML, 'Left', 0 );
  FFormTop := ReadTagInt ( XML, 'Top', 0 );
  FFormWidth := ReadTagInt ( XML, 'Width', 700 );
  FFormHeight := ReadTagInt ( XML, 'Height', 500 );
  fLibraryLocation:= TNotesPanelLocation(ReadTagInt ( XML, 'LibraryLocation', 0 ));
  fProjectsLocation:= TNotesPanelLocation(ReadTagInt ( XML, 'ProjectsLocation', 0 ));

end;

procedure TNotesOptions.Save;
var
  XML: string;
begin
  WriteTagBool( @XML, 'ShowStatusBar', FShowStatusBar );
  WriteTagBool( @XML, 'ShowToolBar', FShowToolBar );
  WriteTagBool( @XML, 'ShowLeftPanel', FShowLeftPanel );
  WriteTagBool( @XML, 'ShowRightPanel', FShowRightPanel );
  WriteTagBool( @XML, 'ShowBottomPanel', FShowBottomPanel );
  WriteTagBool( @XML, 'Maximized', FMaximized );
  WriteTagInt ( @XML, 'Left', FFormLeft );
  WriteTagInt ( @XML, 'Top', FFormTop );
  WriteTagInt ( @XML, 'Width', FFormWidth  );
  WriteTagInt ( @XML, 'Height', FFormHeight );
  WriteTagStr ( @XML, 'LastLibFileType', FLastLibFileType );
  WriteTagInt ( @XML, 'LibraryLocation', ord(fLibraryLocation) );
  WriteTagInt ( @XML, 'ProjectsLocation', ord(fProjectsLocation) );

  addStandardXMLTags( XML, 'NotesOptions') ;
  StrToFile( NProfile.Paths.OptionsFile, XML );
end;

end.
