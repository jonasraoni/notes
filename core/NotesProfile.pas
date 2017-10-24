//
//    NotesProfile - classes para manipular os profiles do Notes
//
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
  @abstract(NotesProfile -  classes para manipular os profiles do Notes.)
  @author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Esta unit contém classes para gerenciar os profiles do Notes, incluído
  aí os itens do menu executar, as associações entre extensões e tipos de
  arquivos, etc.
*)
unit NotesProfile;

interface

uses
  Classes, SysUtils, NotesUtils, NotesMRU, NotesConfig, NotesXML, NotesOptions;

type
  { Pré-declarar classe}
  TNotesProfile = class;

  TNotesFileTypesManager = class
  private
    FProfile: TNotesProfile;
    function GetFileTypeExtensions(const FileType: string): string;
    procedure SetFileTypeExtensions(const FileType, Value: string);
  public
    constructor Create( AProfile: TNotesProfile );
    // retorna true se o FileType existir
    function FileTypeExists( const FileType: string ): Boolean;
    // Pega a primeira extensão (filtro de arquivo) para o tipo de arquivo passado
    function getFirstFileTypeExtension( const FileType: string ): string;
    // Lista os tipos de arquivo (*o parâmetro não deveria ser TStrings aqui?*)
    procedure ListTypes( out FolderTree: PNotesFolderTree );
    // lista arquivos da biblitoeca
    procedure ListLibrary( const FileType: string; out FolderTree: PNotesFolderTree; const StartAtLibrary: string = '' );
    // Lista templates
    procedure ListTemplates( const FileType: string; out FolderTree: PNotesFolderTree; const StartAtTemplate: string = '' );
    // permite escrever/ler as extensões associadas a um fileType
    property  FileTypeExtensions[const FileType: string]: string read GetFileTypeExtensions write SetFileTypeExtensions;
  end;

  { Tipos de saída.}
  TNotesRunOutputType = ( otNone, otStandardOutput, otActiveEditor, otSelectedText );
  { Record com informações sobre o item da pasta run }
  TNotesRunItem = record
    Name: string;
    Cmd: string;
    Args: string;
    Dir: string;
    OutputRegex: string;
    REFilePos: string;
    RELinePos: string;
    Output: TNotesRunOutputType;
    AutoUnderstandOutput: boolean;
    SaveBeforeRun: Boolean;
  end;

  { Classe para centralizar operações sobre a pasta run.
   Permite manipular TNotesRunItems  }
  TNotesRunManager = class
  private
    FProfile: TNotesProfile;
  public
    constructor Create( AProfile: TNotesProfile);
    // Carrega um TNotesRunItem de um arquivo
    function  LoadItem( const FileName: string; out Item: TNotesRunItem) : Boolean;
    // Salva um TNotesRunItem para o arquivo passado
    procedure SaveItem(const Item: TNotesRunItem; const FileName: string);
    // Cria um novo item com as opções padrão
    procedure NewItem(const FileName: string);
    // Cria um TNotesRunItem através dos parâmetros passados.
    class function CreateItem(const Name, Cmd, Args, Dir, OutputRegex,
      RELinePos, REFilePos: string; Output: TNotesRunOutputType; SaveBeforeRun,
      AutoUnderstandOutput: Boolean) : TNotesRunItem;
  end;

  {Usado para pegar a extensão (naExt) e  linguagem/highlighter (naLanguage) em @link(TNotesAssociations).}
  TNotesAssociationType = ( naExtension, naLanguage );
  { Matriz que contém um par [Extensão, highlighter/linguagem] }
  TNotesAssociation = array[ TNotesAssociationType ] of string;
  { Matriz bidimensional e dinâmica contendo associações entre extensões e linguagens
  guardado no formato da matriz @link(TNotesAssociation)}
  TNotesAssociations = array of TNotesAssociation;

  // vários caminhos para pastas e arquivos do profile do Notes
  TNotesProfilePaths = record
    OptionsFile,
    PanelsOptionsFile,
    AsciiDir,
    RunDir,
    MRUFile,
    FileTypesDir,
    ConfigFile,
    UserDir,
    AssociationsFile,
    DlgFiltersFile,
    FavoritesFile,
    KeymapFile,
    FolderMenusKeymapFile,
    GlobalScriptFile,
    KeymapsDir,
    LocalesDir,
    EditorStateDir,
    MacrosDir,
    ScriptsDir: string;
  end;

  // Permite manipular um profile
  TNotesProfile = class
  private
    {Armazena paths importantes.}
    FPaths: TNotesProfilePaths;
    {Array que guarda a associação entre extensões e linguagens.}
    FAssociations: TNotesAssociations;

    {Controla o menu favoritos.}
    FFavorites: TNotesMRUMenu;
    {Controla o menu arquivos recentes.}
    FMRU: TNotesMRUMenu;

    {Carrega e guarda as configurações do Notes. Veja @link(TNotesConfig).}
    FConfig: TNotesConfig;
    FRunMgr: TNotesRunManager;
    FFileTypesMgr: TNotesFileTypesManager;
    FOptions: TNotesOptions;
  public
    constructor Create;
    destructor Destroy; override;
    // Seta o Profile ativo (ou seja, qual pasta de configuração deve
    // ser usada dentro da pasta dos profiles)
    function SetProfile( const Profile: string ): Boolean;
    // Retorna o profile que o usuário escolheu usar e que deve
    // ser usado pelo Notes por padrão
    function getActiveProfile: string;
    // Retorna true se o profile existir
    function ProfileExists(const Profile: string): boolean;
    // Pega o arquivo correto dependendo da cultura
    function getTranslationFileForModule(const ModuleName: string): string;
    // Pega os filtros de arquivo a serem usado no diálogo abrir/salvar
    function getDlgFilters: string;
    { Carrega associações entre extensão de arquivos e linguagens }
    procedure LoadAssociations;

    property Associations: TNotesAssociations read FAssociations;
    property Paths: TNotesProfilePaths read FPaths;

    property MRU: TNotesMRUMenu read FMRU;
    property Favorites: TNotesMRUMenu read FFavorites;

    property Config: TNotesConfig read FConfig;
    property Options: TNotesOptions read FOptions;
    property FileTypesMgr: TNotesFileTypesManager read FFileTypesMgr;
    property RunMgr: TNotesRunManager read FRunMgr;
  end;

implementation

uses NotesGlobals;

{ TNotesProfile }

constructor TNotesProfile.Create;
begin
  FRunMgr := TNotesRunManager.Create( Self );
  FFileTypesMgr := TNotesFileTypesManager.Create( Self );
  FOptions := TNotesOptions.Create;

  FConfig := TNotesConfig.Create;
  FMRU := TNotesMRUMenu.Create;
  FFavorites := TNotesMRUMenu.Create;
end;

destructor TNotesProfile.Destroy;
begin
  FRunMgr.Free;
  FFileTypesMgr.Free;
  FOptions.Free;
  SetLength( FAssociations, 0);

  FConfig.Free;
  FMRU.Free;
  FFavorites.Free;
  inherited Destroy;
end;

function TNotesProfile.getActiveProfile: string;
begin
  Result:= FileToStr(addSlash(NProfilesPath) + NOTES_ACTIVE_PROFILE_FILENAME);
end;

function TNotesProfile.getDlgFilters: string;
begin
  Result:= FileToStr(FPaths.DlgFiltersFile);
end;

function TNotesProfile.getTranslationFileForModule(const ModuleName: string): string;
begin
  if (DirectoryExists(paths.UserDir + 'locale')) then
    Result:= addSlash(paths.UserDir + 'locale')+ ModuleName + NOTES_LOCALEFILE_EXT
  else
    Result:='';
end;

procedure TNotesProfile.LoadAssociations;
Var
  I, Len, LineLen: integer;
  TxtFile : TextFile;
  S: string;
begin
  Len := 0;
  if not FileExists( FPaths.AssociationsFile ) then
    Exit;
  AssignFile( TxtFile, FPaths.AssociationsFile );
  Reset( TxtFile );
  while not EOF( TxtFile ) do
  begin
    Inc( Len );
    SetLength( FAssociations, Len );
    ReadLn( TxtFile, S );
    LineLen := Length( S );
    for I := 1 to LineLen do
      if S[I] = ':' then
        Break;
    if I > 1 then
      FAssociations[Len-1, naExtension] := Copy( S, 1, I-1 );
    if LineLen > I+1 then
      FAssociations[Len-1, naLanguage] := Copy( S, I+1, LineLen );
  end;
  CloseFile(TxtFile);
end;

function TNotesProfile.ProfileExists(const Profile: string): boolean;
begin
  result:= DirectoryExists( NProfilesPath + Profile +'\');
end;

function TNotesProfile.SetProfile(const Profile: string): Boolean;
begin
  Result := False;

  if not ProfileExists(Profile) then Exit;

  with FPaths do
  begin
    UserDir := AddSlash( NProfilesPath + Profile );
    FileTypesDir := AddSlash( UserDir + NOTES_FILETYPES_DIRNAME );
    RunDir := AddSlash( UserDir + NOTES_RUN_DIRNAME );
    AssociationsFile := UserDir + NOTES_ASSOCIATIONS_FILENAME;
    DlgFiltersFile:= UserDir + NOTES_DLGFILTERS_FILENAME;
    FavoritesFile := UserDir + NOTES_FAVORITES_FILENAME;
    FolderMenusKeymapFile:= UserDir + NOTES_FOLDER_MENUS_KEYMAP_FILENAME;
    MRUFile := UserDir + NOTES_MRU_FILENAME;
    ConfigFile := UserDir + NOTES_CONFIG_FILENAME;
    OptionsFile := UserDir + NOTES_OPTIONS_FILENAME;
    PanelsOptionsFile:= UserDir + NOTES_PANELS_OPTIONS_FILENAME;
    KeymapFile:= UserDir + NOTES_KEYMAP_FILENAME;
    KeymapsDir:= AddSlash( UserDir + NOTES_KEYMAPS_DIRNAME );
    LocalesDir:= AddSlash( AddSlash(NExePath) + 'data\' + NOTES_LOCALE_DIRNAME );
    EditorStateDir:= AddSlash( UserDir + NOTES_EDITOR_STATE_DIRNAME );
    MacrosDir:= AddSlash( UserDir + NOTES_MACROS_DIRNAME );
  end;
  FConfig.Load;
  FOptions.Load;
  Result:= true;
end;

{ TNotesRunManager }

procedure TNotesRunManager.NewItem(const FileName: string);
var
  S: string;
begin
  if FileName = '' then
    Exit;

  WriteTagStr( @S, 'Cmd', '' );
  WriteTagStr( @S, 'Args', '"%file%"' );
  WriteTagStr( @S, 'Dir', '%path%' );
  WriteTagStr( @S, 'OutputRegex', '' );
  WriteTagBool(@S, 'SaveBeforeRun', True );
  WriteTagInt( @S, 'Output', 0 );
  WriteTagBool(@S, 'AutoUnderstandOutput', True );
  WriteTagStr( @S, 'REFilePos', '1');
  WriteTagStr( @S, 'RELinePos', '2' );

  addStandardXMLTags( S, 'NotesRun' );
  SysUtils.ForceDirectories(FProfile.Paths.RunDir);
  StrToFile( FileName , S );
end;

procedure TNotesRunManager.SaveItem(const Item: TNotesRunItem; const FileName: string);
var
  S: string;
begin
  if FileName = '' then
    Exit;

  WriteTagStr( @S, 'Cmd', Item.Cmd );
  WriteTagStr( @S, 'Args', Item.Args );
  WriteTagStr( @S, 'Dir', Item.Dir );
  WriteTagStr( @S, 'OutputRegex', Item.OutputRegex );
  WriteTagBool(@S, 'SaveBeforeRun', Item.SaveBeforeRun );
  WriteTagInt( @S, 'Output', Ord( Item.Output ) );
  WriteTagBool(@S, 'AutoUnderstandOutput', Item.AutoUnderstandOutput );
  WriteTagStr( @S, 'REFilePos', Item.REFilePos);
  WriteTagStr( @S, 'RELinePos', Item.RELinePos );

  addStandardXMLTags( S, 'NotesRun' );
  SysUtils.ForceDirectories(FProfile.Paths.RunDir);
  StrToFile( FileName , S );
end;

constructor TNotesRunManager.Create(AProfile: TNotesProfile);
begin
  FProfile := AProfile;
end;

class function TNotesRunManager.CreateItem(const Name, Cmd, Args, Dir, OutputRegex,
      RELinePos, REFilePos: string; Output: TNotesRunOutputType; SaveBeforeRun,
      AutoUnderstandOutput: Boolean): TNotesRunItem;
begin
  Result.Name := Name;
  Result.Cmd := Cmd;
  Result.Args := Args;
  Result.Dir := Dir;
  Result.Output := Output;
  Result.SaveBeforeRun := SaveBeforeRun;
  Result.OutputRegex:= OutputRegex;
  Result.REFilePos:= REFilePos;
  Result.RELinePos:= RELinePos;
  Result.AutoUnderstandOutput:= AutoUnderstandOutput;
end;

function TNotesRunManager.LoadItem( const FileName: string; out Item: TNotesRunItem) : Boolean;
var
  S: string;
begin
  Result := True;
  if not FileExists( FileName ) then
  begin
    Result := False;
    Exit;
  end;
  S := FileToStr( FileName );
  with Item do
  begin
    Name := GetFileName(FileName);
    Cmd := ReadtagStr( S, 'Cmd', '' );
    Args := ReadtagStr( S, 'Args', '' );
    Dir := ReadTagStr( S, 'Dir', '' );
    OutputRegex := ReadTagStr(S, 'OutputRegex', '');
    REFilePos := ReadTagStr(S, 'REFilePos', '');
    RELinePos := ReadTagStr(S, 'RELinePos', '');
    Output := TNotesRunOutputType( ReadTagInt(S, 'Output', 0 ) );
    AutoUnderstandOutput:= ReadTagBool( S, 'AutoUnderstandOutput', True );
    SaveBeforeRun := ReadTagBool( S, 'SaveBeforeRun', True );
  end;
end;


{ TNotesDataTypesManager }

constructor TNotesFileTypesManager.Create( AProfile: TNotesProfile );
begin
  FProfile := AProfile;
end;


function TNotesFileTypesManager.FileTypeExists( const FileType: string): Boolean;
begin
  Result := DirectoryExists( FProfile.Paths.FileTypesDir + FileType );
end;

function TNotesFileTypesManager.GetFileTypeExtensions(
  const FileType: string): string;
begin
  Result := FileToStr( AddSlash( FProfile.Paths.FileTypesDir + FileType ) + NOTES_FILETYPE_EXTENSION_FILENAME );
end;

procedure TNotesFileTypesManager.ListTypes( out FolderTree: PNotesFolderTree );
begin
  BuildFolderTree( FolderTree, FProfile.Paths.FileTypesDir, ftOnlyFolders );
end;

procedure TNotesFileTypesManager.ListLibrary( const FileType: string; out FolderTree: PNotesFolderTree; const StartAtLibrary: string = '' );
begin
  BuildFolderTree( FolderTree, FProfile.Paths.FileTypesDir + AddSlash( FileType ) + AddSlash( NOTES_CODELIBRARY_DIRNAME ) + StartAtLibrary, ftAll, true, '*', '*'+NOTES_CODELIBRARY_EXTENSION );
end;

procedure TNotesFileTypesManager.SetFileTypeExtensions(const FileType, Value: string);
begin
  if FileTypeExists( FileType ) then
    StrToFile( FProfile.Paths.FileTypesDir + AddSlash( FileType ) + NOTES_FILETYPE_EXTENSION_FILENAME, Value );
end;

procedure TNotesFileTypesManager.ListTemplates( const FileType: string; out FolderTree: PNotesFolderTree; const StartAtTemplate: string = '' );
begin
  BuildFolderTree( FolderTree, FProfile.Paths.FileTypesDir + AddSlash( FileType ) + AddSlash( NOTES_TEMPLATELIBRARY_DIRNAME ) + StartAtTemplate, ftAll, true );
end;

function TNotesFileTypesManager.getFirstFileTypeExtension(
  const FileType: string): string;
var
  a: TStringArray;
begin
  a:= ExplodeStr(GetFileTypeExtensions(FileType), ';');
  if length(a) > 0 then
    Result:= '.' + a[0]
  else
    Result:='.txt';
  SetLength(a, 0);
end;


end.
