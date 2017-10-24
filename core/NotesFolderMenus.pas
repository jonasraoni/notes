unit NotesFolderMenus;

interface

uses Windows, SysUtils, Classes, Contnrs, Menus, Controls, Forms;

type
  // Item usado em @link(TNotesFolderMenusKeymap)
  TNotesFolderMenusKeymapItems = class(TObject)
  private
    fFile: string;
    fShortcut: TShortcut;
    procedure setRelative(const Value: string);
    function getRelative: string;
  public
    // Nome do arquivo relativo ao profile atual
    property RelativeFileName: string read getRelative write setRelative;
    // Nome do arquivo completo
    property FileName: string read fFile write fFile;
    // Shortcut associada ao arquivo
    property TheShortcut: TShortcut read fShortcut write fShortcut;
  end;

  // Keymap para items de menus criados a partir de pastas e arquivos
  // Os dados são guardados em um arquivo texto em que cada linha
  // possui a tecla de atalho e o nome do arquivo *relativo a pasta
  // do profile atual* (o que permite mover o arquivo para outros
  // profiles ou computadores sem haver perdade de dados)
  TNotesFolderMenusKeymap = class(TObjectList)
  public
    // Retorna a tecla de atalho do arquivo
    function getShortcutForFile(const FileName: string): TShortcut;
    // Seta a tecla de atalho para um arquivo
    procedure setShortcutForFile(const FileName: string; const AShortcut: TShortcut);
    // Se um arquivo tiver a Shortcut passada, retorna ele como
    // sua "caption" correspondente, pronta para ser msotrada ao usuário
    function getItemCaptionForShortcut(const AShortcut: TShortcut): string;
    // Carrega configurações
    procedure Load;
    // Salva as modificações
    procedure Save;
  end;

// Adiciona um menu, chamado pelo BuildMenuFromFolder
function  AddMenuItemForFileOrFolder(AOwner: TComponent; ACaption: TCaption; AOnClick: TNotifyEvent;
  AHint: String; AMenuItem: TMenuItem): TMenuItem; forward;
{ Permite  construir um menu a partir de arquivos e subpastas de
  um diretório. AFolder é o caminho para diretório (veja adiante sobre
  coringas) onde a pesquisa será inciada. AMi é o item onde os novos
  itens de menu serão inseridos. aOwner será um owner para os itens do
  menu. Este owner deve ser específico para os novos itens de menu, pois
  assim você pode utilizá-lo para atualizar o menu toda vez que o menu
  for requesitado. AonClick, função que será chamada qdo o usuário clicar
  em um dos novos itens. InternalCall é usado pela própria função, sempre
  use "false". AFilter permite você restringir a pesquisa a certos tipos
  de arquivos usando coringas. Exemplo: "*.ncl". ScanSubfolders indica se
  a função deve escanear os subdiretórios recursivamente ou não.}
procedure BuildMenuFromFolder( AFolder: string; AMi: TMenuItem; AOwner: TComponent; AonClick: TNotifyEvent; InternalCall: boolean; const AFilter: string; ScanSubfolders: Boolean); forward;

var
  folderMenusKeymap: TNotesFolderMenusKeymap;

implementation

uses
  NotesGlobals, NotesUtils;

{ TNotesFolderMenusKeymapItems }

function TNotesFolderMenusKeymapItems.getRelative: string;
begin
  Result:= Copy(fFile, length(NProfile.Paths.UserDir), length(fFile));
end;

procedure TNotesFolderMenusKeymapItems.setRelative(const Value: string);
begin
  fFile:= nProfile.Paths.UserDir + Value;
  fFile:= StringReplace(fFile, '\\', '\', [rfReplaceAll]);
end;

{ TNotesFolderMenusKeymap }

function TNotesFolderMenusKeymap.getItemCaptionForShortcut(
  const AShortcut: TShortcut): string;
Var
  I: integer;
begin
  Result:= '';
  for I:= 0 to self.Count -1 do
    if AShortcut = TNotesFolderMenusKeymapItems(Items[I]).TheShortcut then
    begin
      Result:= GetFileName(TNotesFolderMenusKeymapItems(Items[I]).FileName);
      Exit;
    end;
end;

function TNotesFolderMenusKeymap.getShortcutForFile(
  const FileName: string): TShortcut;
Var
  I: integer;
begin
  Result:= 0;
  for I:= 0 to self.Count -1 do
    if SameText(FileName, TNotesFolderMenusKeymapItems(Items[I]).FileName) then
    begin
      Result:= TNotesFolderMenusKeymapItems(Items[I]).TheShortcut;
      Exit;
    end;
end;

procedure TNotesFolderMenusKeymap.Load;
Var
  I, Idx, LineLen: integer;
  TxtFile : TextFile;
  S: string;
begin
  self.Clear;
  if not FileExists( nProfile.Paths.FolderMenusKeymapFile ) then
    Exit;
  AssignFile( TxtFile, nProfile.Paths.FolderMenusKeymapFile );
  Reset( TxtFile );
  while not EOF( TxtFile ) do
  begin
    ReadLn( TxtFile, S );
    idx:= self.Add(TNotesFolderMenusKeymapItems.Create);
    LineLen := Length( S );
    for I := 1 to LineLen do
      if S[I] = '=' then
        Break;
    if I > 1 then
      TNotesFolderMenusKeymapItems(Items[idx]).TheShortcut := StrToIntDef(Copy( S, 1, I-1 ), 0);
    if LineLen > I+1 then
      TNotesFolderMenusKeymapItems(Items[idx]).RelativeFileName := Copy( S, I+1, LineLen );
  end;
  CloseFile(TxtFile);
end;

procedure TNotesFolderMenusKeymap.Save;
Var
  I: integer;
  TxtFile : TextFile;
  S: string;
begin
  AssignFile( TxtFile, nProfile.Paths.FolderMenusKeymapFile );
  Rewrite( TxtFile );
  for I:=0 to self.Count -1 do
  begin
    if TNotesFolderMenusKeymapItems(Items[I]).TheShortcut <> 0 then
    begin
      S:= IntToStr(TNotesFolderMenusKeymapItems(Items[I]).TheShortcut) + '=' + TNotesFolderMenusKeymapItems(Items[I]).RelativeFileName;
      Writeln(TxtFile, S);
    end;
  end;
  CloseFile(TxtFile);
end;

procedure TNotesFolderMenusKeymap.setShortcutForFile(
  const FileName: string; const AShortcut: TShortcut);
Var
  I: integer;
begin
  for I:= 0 to self.Count -1 do
    if SameText(FileName, TNotesFolderMenusKeymapItems(Items[I]).FileName) then
    begin
      TNotesFolderMenusKeymapItems(Items[I]).TheShortcut:= AShortcut;
      Exit;
    end;
  // Se não saiu, o item ainda não existe

  I:= self.Add(TNotesFolderMenusKeymapItems.Create);
  TNotesFolderMenusKeymapItems(Items[I]).TheShortcut:= AShortcut;
  TNotesFolderMenusKeymapItems(Items[I]).FileName:= FileName;
end;

procedure BuildMenuFromFolder( AFolder: string; AMi: TMenuItem;
  AOwner: TComponent; AOnClick: TNotifyEvent; InternalCall: boolean;
  const AFilter: string; ScanSubfolders: Boolean);
var
  SearchRec: TSearchRec;
  FoundMi: TMenuItem;
  S: string;
begin
  AFolder := AddSlash( AFolder );
  //destroy TODOS os componetes do owner. O owner deve ser específico para os itens do menu!!!
  if not InternalCall then
    AOwner.DestroyComponents;

  if FindFirst( AFolder + '*', faAnyFile, SearchRec ) = 0 then
  begin
    repeat
      S := SearchRec.Name;
      if S <> '' then
      begin
        if SearchRec.Attr and faDirectory = 0 then
        begin
          if ( AFilter = '' ) or SameText( ExtractFileExt( S ), AFilter ) then
            AddMenuItemForFileOrFolder( AOwner, S, AOnClick , AFolder + S, AMI );
        end
        else if ScanSubFolders and ( S[1] <> '.' ) then
        begin
          FoundMI := AddMenuItemForFileOrFolder( AOwner, S+'.xyz', nil, '', AMi );
          if FoundMi <> nil then
            BuildMenuFromFolder( AFolder+S, FoundMI, AOwner, AOnClick, true, Afilter, true );
          FoundMI.Caption:= ChangeFileExt(FoundMi.Caption, '');
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function AddMenuItemForFileOrFolder(AOwner: TComponent; ACaption: TCaption; AOnClick: TNotifyEvent;
  AHint: String; AMenuItem: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  Result.OnClick := AOnClick;
  if AHint <> '' then
  begin
    Result.Caption := ChangeFileExt( ACaption, '' );
    Result.Hint := AHint;
    Result.Shortcut:= folderMenusKeymap.getShortcutForFile(AHint);
  end
  else
  begin
    Result.Caption := ACaption;
    Result.ImageIndex := 19;
    Result.Hint := '';
  end;
    AMenuItem.Insert( AMenuItem.Count, Result );
//    if NProfile.Config.OfficeXPTheme then
//      NotesMenu.ActivateMenuItem( Result, False );

end;

initialization
  folderMenusKeymap:= TNotesFolderMenusKeymap.Create(true);
finalization
  FreeAndNil(folderMenusKeymap);

end.
