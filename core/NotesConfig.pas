//
//    NotesConfig - classes e types para armazenar as configurações gerais do Notes.
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


{@abstract(NotesConfig - classes e types para armazenar as configurações gerais do Notes.)
 @author(Anderson R. Barbieri <notesnr@ig.com.br>)
 @author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
}
unit NotesConfig;

interface

uses
  Classes, Graphics, NotesEditorTab;

type
  // O que o Notes deve fazer após iniciar.
  TNotesStartUp = (suShowStartPage, suLoadLastFiles, suLoadLastProject, suDoNothing);

type
  { Carrega/Salva/Armazena as configurações gerais do Notes.}
  TNotesConfig = class(TObject)
  private
    fFontName: string;
    fFontSize: integer;
    fShowGutter: boolean;
    fShowLinesInGutter: boolean;
    fEditorRightMargin: integer;
    fDefaultFileType: string;
    fSmartHome: boolean;
    fSmartTabs: boolean;
    fTrimTrailingSpaces: boolean;
    fAllowMultipleInstances: boolean;
    fShellExt: Boolean;
    fLanguage: string;
    fRemeberFileInfo: integer;
    fAutoCloseSymbs: boolean;
    fOfficeXPTheme: boolean;
    fAfterEOL: boolean;
    fSpecialChars: boolean;
    fBrakets: boolean;
    fStartUp: TNotesStartUp;
    fStartAt0: boolean;
    fBackup: boolean;
    fAutoSaveInterval: integer;
    procedure SetShellExt(const Value: boolean);
  public
    { Carrega as configurações de um arquivo.}
    procedure Load;
    { Salva as configurações para um arquivo.}
    procedure Save;

    // Seta as configurações do EditorTab passado conforme as configurações
    procedure SetEditorTabPropertys(const EditorTab: TNotesEditorTab);

    { Nome da fonte usada no editor.}
    property FontName: string read fFontName write fFontName;
    { Tamanho da fonte usada no editor.}
    property FontSize: integer read fFontSize write fFontSize;
    { Mostrar gutter.}
    property ShowGutter: boolean read fShowGutter write fShowGutter;
    { Mostrar número das linhas na gutter.}
    property ShowLinesInGutter: boolean read fShowLinesInGutter write fShowLinesInGutter;
    { Tamanho da margem direta do editor em espaços.}
    property EditorRightMargin: integer read fEditorRightMargin write fEditorRightMargin;
    { Tipo de arquivo padrão.}
    property DefaultFileType: string read fDefaultFileType write fDefaultFileType;
    { Permitir multiplas instâncias do Notes?!  }
    property AllowMultipleInstances: boolean read fAllowMultipleInstances write fAllowMultipleInstances;
    { Permite setar se a ShellExtension do Notes deve ou não ser usada.  }
    property ShellExt: boolean read fShellExt write SetShellExt;
    { Permite configurar a tradução a ser usada. }
    property Language: string read fLanguage write fLanguage;
    { Número de dias pelos quais o Notes deve lembrar as informações
      de um arquivo. A interface mostra como um checkbox: se ativado
      ele seta para 20 dias, se não, seta para Zero. Zero desabilita
      o cache, obviamente. Usuários avançados podem modificar o número
      de dias editando o arquivo de configuração. }
    property RemeberFileInfo: integer read fRemeberFileInfo write fRemeberFileInfo;
    { SmatHome.}
    property SmartHome: boolean read fSmartHome write fSmartHome;
    { SmatTab e SmartDelete.}
    property SmartTabs: boolean read fSmartTabs write fSmartTabs;

    { Usa o componente XPMenu pra desenhar os controles. }
    property OfficeXPTheme: boolean read fOfficeXPTheme write fOfficeXPTheme;
    { Determina o que o Notes deve fazer ao iniciar. }
    property StartUp: TNotesStartUp read fStartUp write fStartUp;
    { Determina se o editor deve fechar símbolos de programação automaticamente.}
    property AutoCloseSymbols: boolean read fAutoCloseSymbs write fAutoCloseSymbs;
    { Determina se o cursor pode passar do final da linha/final do arquivo. }
    property CursorAfterEOL: boolean read fAfterEOL write fAfterEOL;
    { Determina se o editor deve colorir braços pares. }
    property HighlightMatchingBrakets: boolean read fBrakets write fBrakets;
    { Determina se o editor deve mostrar os caracteres especiais. }
    property ShowSpecialChars: boolean read fSpecialChars write fSpecialChars;
    { Determina se o Notes deve pensar linhas e colunas inciando por zero. }
    property LineAndColCountStartAt0: boolean read fStartAt0 write fStartAt0;
    { Determina se os espaços ao fim da linha devems ser deletados. }
    property TrimTrailingSpaces: boolean read fTrimTrailingSpaces write fTrimTrailingSpaces;
    { Determina se o Notes deve fazer um backup do arquivo ao salvar. }
    property BackupOnSave: boolean read fBackup write fBackup;
    { Determina o intervalo em que o Notes salvara os arquivos automaticamente. }
    property AutoSaveInterval: integer read fAutoSaveInterval write fAutoSaveInterval;
  end;

implementation

uses NotesXML, NotesUtils, NotesGlobals, Windows, ShellApi, SynEdit;

{ TNotesConfig }

procedure TNotesConfig.Load;
var
  XML: string;
begin
  XML := FileToStr( NProfile.Paths.ConfigFile );
  fFontName:= ReadTagStr(XML, 'FontName', 'Courier New');
  fLanguage:= ReadTagStr(XML, 'Language', '');
  fFontSize:= ReadTagInt(XML, 'FontSize', 10);
  fRemeberFileInfo:= ReadTagInt(XML, 'RememberFileInfo', 30);
  fAutoSaveInterval:= ReadTagInt(XML, 'AutoSaveInterval', 0);
  fShowGutter:= ReadTagBool(XML, 'ShowGutter', true);
  fShowLinesInGutter:= ReadTagBool(XML, 'ShowLinesInGutter', true);
  fEditorRightMargin:= ReadTagInt(XML, 'RightMargin', 72);
  fDefaultFileType:= ReadTagStr(XML, 'DefaultFileType', '');
  fSmartHome:= ReadTagBool(XML, 'SmartHome', true);
  fSmartTabs:= ReadTagBool(XML, 'SmartTabs', true);
  fTrimTrailingSpaces:= ReadTagBool(XML, 'TrimTrailingSpaces', true);
  fAllowMultipleInstances:= ReadTagBool(XML, 'AllowMultipleInstances', false);
  fShellExt:= ReadTagBool(XML, 'ShellExt', false);
  fSmartHome:= ReadTagBool(XML, 'SmartHome', true);
  fSmartTabs:= ReadTagBool(XML, 'SmartTabs', true);
  fStartUp:= TNotesStartUp(ReadTagInt(XML, 'StartUp', 0));
  fOfficeXPTheme:= ReadTagBool(XML, 'OfficeXPTheme', false);
  fAutoCloseSymbs:= ReadTagBool(XML, 'AutoCloseSymbols', True);
  fAfterEOL:= ReadTagBool(XML, 'CaretAfterEOL', True);
  fBrakets:= ReadTagBool(XML, 'HighlightBrakets', True);
  fSpecialChars:= ReadTagBool(XML, 'ShowSpecialChars', False);
  fStartAt0:= ReadTagBool(XML, 'LineAndColCountStartAt0', False);
  fTrimTrailingSpaces:= ReadTagBool(XML, 'TrimTrailingSpaces', True);
  fBackup:=  ReadTagBool(XML, 'BackupOnSave', false);
end;

procedure TNotesConfig.Save;
var
  XML: string;
begin
  WriteTagStr(@XML, 'FontName', fFontName);
  WriteTagStr(@XML, 'Language', fLanguage);
  WriteTagInt(@XML, 'FontSize', fFontSize);
  WriteTagInt(@XML, 'RememberFileInfo', fRemeberFileInfo);
  WriteTagInt(@XML, 'StartUp', Ord(fStartUp));
  WriteTagInt(@XML, 'AutoSaveInterval', fAutoSaveInterval);
  WriteTagBool(@XML, 'ShowGutter', fShowGutter);
  WriteTagBool(@XML, 'ShowLinesInGutter', fShowLinesInGutter);
  WriteTagInt(@XML, 'RightMargin', fEditorRightMargin);
  WriteTagStr(@XML, 'DefaultFileType',fDefaultFileType);
  WriteTagBool(@XML, 'SmartHome', fSmartHome);
  WriteTagBool(@XML, 'SmartTabs', fSmartTabs);
  WriteTagBool(@XML, 'TrimTrailingSpaces', fTrimTrailingSpaces);
  WriteTagBool(@XML, 'AllowMultipleInstances', fAllowMultipleInstances);
  WriteTagBool(@XML, 'ShellExt', fShellExt);
  WriteTagBool(@XML, 'SmartHome', fSmartHome);
  WriteTagBool(@XML, 'SmartTabs', fSmartTabs);
  WriteTagBool(@XML, 'OfficeXPTheme', fOfficeXPTheme);
  WriteTagBool(@XML, 'AutoCloseSymbols', fAutoCloseSymbs);
  WriteTagBool(@XML, 'CaretAfterEOL', fAfterEOL);
  WriteTagBool(@XML, 'HighlightBrakets', fBrakets);
  WriteTagBool(@XML, 'ShowSpecialChars', fSpecialChars);
  WriteTagBool(@XML, 'LineAndColCountStartAt0', fStartAt0);
  WriteTagBool(@XML, 'TrimTrailingSpaces', fTrimTrailingSpaces);
  WriteTagBool(@XML, 'BackupOnSave', fBackup);

  addStandardXMLTags( XML, 'NotesConfig' );
  StrtoFile( NProfile.Paths.ConfigFile, XML );
end;

procedure TNotesConfig.SetEditorTabPropertys(
  const EditorTab: TNotesEditorTab);
begin
  if EditorTab = nil then Exit;
  EditorTab.Editor.Font.Name:= fFontName;
  Editortab.Editor.Font.Size:= fFontsize;
  EditorTab.Editor.Gutter.Visible:= fShowGutter;
  EditorTab.Editor.Gutter.ShowLineNumbers:= fShowLinesInGutter;
  EditorTab.Editor.RightEdge:= fEditorRightMargin;
  EditorTab.SetEditorOption(eoSmartTabs, fSmartTabs);
  EditorTab.SetEditorOption(eoSmartTabDelete, fSmartTabs);
  EditorTab.SetEditorOption(eoEnhanceHomeKey, fSmartHome);
  EditorTab.SetEditorOption(eoScrollPastEol, fAfterEOL);
  EditorTab.SetEditorOption(eoScrollPastEof, fAfterEOL);
  EditorTab.SetEditorOption(eoTrimTrailingSpaces, fTrimTrailingSpaces);
  EditorTab.AutoClose:= fAutoCloseSymbs;
  EditorTab.SetEditorOption(eoShowSpecialChars, fSpecialChars);
  EditorTab.highlightSymbols:= fBrakets;
end;

procedure TNotesConfig.SetShellExt(const Value: boolean);
begin
  fShellExt := Value;
  if Value then
    ShellExecute(0, 'open', 'regsvr32', PChar('/s "'+ NExePath + 'NotesShellExt.dll"'), '', SW_HIDE)
  else
    ShellExecute(0, 'open', 'regsvr32', PChar('/u /s "'+ NExePath + 'NotesShellExt.dll"'), '', SW_HIDE)
end;

end.
