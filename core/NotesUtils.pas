//
//    NotesUtils - várias funções usadas no notes
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
  @abstract(NotesUtils - várias funções usadas no notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  @author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
*)
unit NotesUtils;

interface

uses
  SysUtils, Classes, ComCtrls, Dialogs, Windows;

type
  {Tipos de finais de linha de arquivos ASCII: Windows, Mac e Linux.
   gtNone deve ser retornado quando o tipo não puder ser reconhecido.
   gtBin deve ser retornado se o arquivo for binário.}
  TNotesBreakType = ( btNone, btWin, btMac, btLin, btBin );
  { Tipos de diretórios especiais }
  TNotesSpecialFolder = (sfDesktop, sfAppData, sfTemplates, sfPrograms, sfPersonal, sfFavorites, sfStartup, sfRecent, sfSendTo, sfStartMenu, sfFonts, sfHistory, sfCookies, sfInternetCache, sfCommonFavorites, sfCommonDesktop, sfCommonStartup, sfCommonPrograms, sfCommonStartMenu );
  { Tipos retornados na listagem }
  TNotesBuildFolderTreeOptions = ( ftOnlyFiles, ftOnlyFolders, ftAll );

  //Ponteiro para a estrutura TNotesFolderTree
  PNotesFolderTree = ^TNotesFolderTree;

  { Estrutra retornada pela função BuildFolderTree
    Várias funções estão disponíveis para copiar,
    deletar, contar, etc. os items deste tipo
    de estrutura.   }
  TNotesFolderTree = array of record
    Path: string;
    case IsFolder: Boolean of
      True: ( Items: PNotesFolderTree; );
  end;

  // Callback para as operações Copiar/Deletar FolderTrees
  TNotesFolderTreeOperationCallBack = procedure (const CurrentPath: string;
    const CurrentItem: integer; Var Abort: boolean) of object;

  // Array dinâmico de strings
  TStringArray = array of String;
  // Array dinâmico de integers
  TIntegerArray = array of integer;

{ Copia um diretório com toda a sua estrutura. }
procedure CopyDir(const fromDir, toDir: string);
{Retorna só o nome do arquivo, sem extensões.}
function GetFileName( const Filename: string ): string; forward;
{Retorna o conteúdo do arquivo passado em nArq.}
function FileToStr( Filename: string ): string; forward;
{Salva a string passada em oQporNoArq no arquivo nArq. }
procedure StrToFile( const Filename: string; const Content: string ); forward;
{ Do Project JEDI Code Library (JCL), pega o caminho "longo" para um arquivo que foi especificado com o
nome no estilo Dos.}
function PathGetLongName( Path: string ): string; forward;
{Retorna o tipo de final de linha do arquivo.}
function GetBreakType( const Filename: string; const MaxDataToRead: Cardinal = 5*1024 ): TNotesBreakType; forward;
{Retorna true se a string for um número.}
function IsNumber( S: string ): Boolean; forward;
{Substituí todas as ocrrências de aSearch por aReplace recursivamente.}
procedure StrReplaceAll( var S: string; const Search, Replace: string ); forward;
{Deleta a última extensão do nome de um arquivo.}
function DelFileExt( const FileName: string ): string; forward;
{Quebra uma string em linhas cada vez que WrapLen é atingido.}
function WordWrap( Source: string; MaxLineLen: Integer; const LineBreak: string = #13#10 ): string; forward;
{MessageBox com ícone de informação e botão OK.}
Procedure MsgInfo ( const MsgTxt: string; FrmHandle: THandle ); forward;
{MessageBox com ícone de pergunta e botão Yes, No.}
Function MsgYesNo ( const MsgTxt: string; FrmHandle: THandle ): Integer ; forward;
{MessageBox com ícone de pergunta e botão Yes, No e Cancel.}
Function MsgQuest ( const MsgTxt: string; FrmHandle: THandle ): Integer ; forward;
{Messagebox com botão OK apenas.}
Procedure MsgOk ( const MsgTitle, MsgTxt: string; FrmHandle: THandle ); forward;
{MessageBox com ícone de exclamação e botão OK. Usado pra informar erros e problemas.}
Procedure MsgExclama ( const MsgTxt: string; FrmHandle: THandle ); forward;
{Envia o arquivo ou pasta para a lixeira do windows.}
procedure SendToTrash( const FileOrFolder: string; FrmHandle: THandle ); forward;
{Pegar path's especiais sob o windows}
function GetSpecialFolder( const FolderType: TNotesSpecialFolder ): string; forward;
{ Adiciona uma barra no final do path "\" para windows e "/" para linux... }
function AddSlash( const Path: string ): string; forward;
{ IIF para Inteiros }
function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: Integer ): Integer; overload;
{ IIF para Strings }
function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: String ): String; overload;
{ IIF para Ponteiros }
function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: Pointer ): Pointer; overload;

{ Transforma um diretório na estrutra TNotesfolderTree }
procedure BuildFolderTree( var TheFolderTree: PNotesFolderTree; Folder: string; const Options: TNotesBuildFolderTreeOptions = ftAll; const ScanSubFolders: Boolean = false; const FolderMask: string = '*'; const FileMask: string = '*' ); forward;
{ Libera todos os recursos utilizados por uma estrutura TNotesFolderTree }
procedure FreeFolderTree( FolderTree: PNotesFolderTree ); forward;
{ Conta o número total de items de um TNotesfolderTree}
procedure CountFolderTree(Tree: PNotesFolderTree; Var Count: integer); forward;
{ Copia os arquivos listados em um TNotesfolderTree para o diretório @code(Todir) }
procedure CopyFolderTree(Tree: PNotesFolderTree; const FromDir, ToDir: string; const OverwriteOldFiles: boolean; CallBack: TNotesFolderTreeOperationCallBack); forward;
{ Deleta os arquivos especificaddos no TNotesfolderTree}
procedure DeleteFolderTree(FolderTree: PNotesFolderTree; FromPath: string; CallBack: TNotesFolderTreeOperationCallBack); forward;

{ Busca strings }
function  FindStr(const AString: string; ASearchStr: string; const CrLfAndTab, BackWard, MatchCase: Boolean; const StartAt: integer): Integer; forward;
{ Converte AnsiString para UTF-8 }
function AnsiToUtf8(x: ansistring): string; forward;
{ Converte UTF-8 para AnsiString }
function UTF8ToAnsi(x: string): ansistring; forward;

{ Mostra o diálogo padrão do windows para escolher pastas... }
function BrowseForFolder(AParent: HWND; ATitle : string) : string; forward;

{ Separa uma string em várias partes delimitadas pelo Delimiter passado. }
function ExplodeStr(S: string; const Delimiter: Char): TStringArray; forward;
{ Junta um array de strings em uma só string }
function ImplodeStr(arr: TStringArray; const Delimiter: Char): string; forward;
{ Separa uma string em uma lista de integers }
function ExplodeInt(S: string; const Delimiter: Char): TIntegerArray; forward;
{ Junta uma lista de integers em uma stirng }
function ImplodeInt(arr: TIntegerArray; const Delimiter: Char): string; forward;

{ Calcula um hash para uma string. A função é a mesma usada no GNU gettext
 e no FreePascal, porém mesmo assim não garante resultados 100% únicos. }
function strHash(S: string): integer; forward;

{ Busca uma quebra de linha em uma string. Entende como
quebra de linha a primeira ocorrência de um caracter #10
ou #13, sendo assim compatível com a terminação de linha
de todas as plataformas.
  S - string onde fazer a busca
  offset - onde iniciar a busca
  Backwards - se deve buscar de trás para frente.}
function findLineBreak(const S: string; const offset: integer; const Backwards: boolean): integer; forward;

//
function  SplitFilter(Filter: string): TStringList;
// Compara usando wildcards (coringas)
function  WildcardCompare(FileWild, FileName: string): boolean;


{ Seleciona o item do treeview que possui o texto passado }
procedure SelectTreeItem(const ATree: TTreeView; const ItemTxt, ParentTxt: string); forward;

const
  ASCII_LF = #10;
  ASCII_CR = #13;
  ASCII_BEEP = #7;
  ASCII_CRLF = ASCII_CR+ASCII_LF;
  NOTES_WHITESPACES: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','_','.',','];


implementation

uses ShlObj, ShellApi, FastStrings, NotesGlobals;


const
{ Compatibilidade Delphi 6 }

  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_NEWDIALOGSTYLE     = $0040;


{ implementações }

procedure CopyDir(const fromDir, toDir: string);
var
  OpStruc: TSHFileOpStruct;
  frombuf, tobuf: Array [0..128] of Char;
Begin
  FillChar( frombuf, Sizeof(frombuf), 0 );
  FillChar( tobuf, Sizeof(tobuf), 0 );
  StrPCopy( frombuf, fromDir +  '*' );
  StrPCopy( tobuf, toDir );
  With OpStruc Do
  Begin
    Wnd:= 0;
    wFunc:= FO_COPY;
    pFrom:= @frombuf;
    pTo:= @tobuf;
    fFlags:= FOF_NOCONFIRMATION or FOF_RENAMEONCOLLISION or FOF_NOCONFIRMMKDIR or FOF_SIMPLEPROGRESS;
    fAnyOperationsAborted:= False;
    hNameMappings:= Nil;
    lpszProgressTitle:= Nil;
  end;
  ShFileOperation( OpStruc );
end;


procedure BuildFolderTree( var TheFolderTree: PNotesFolderTree; Folder: string; const Options: TNotesBuildFolderTreeOptions = ftAll; const ScanSubFolders: Boolean = false; const FolderMask: string = '*'; const FileMask: string = '*' );
type
 PFolderStackItem = ^TFolderStackItem;

 TFolderStackItem = record
   FolderTree: PNotesFolderTree;
   Folder: string;
 end;
 TFolderStack = array of PFolderStackItem;

var
 FolderStack: TFolderStack;
 SearchRec: TSearchRec;
 CurStack: PFolderStackItem;

  procedure PushStack( var FT: PNotesFolderTree; const AFolder: string );
  var
    FSI: PFolderStackItem;
  begin
   FSI := AllocMem( SizeOf( TFolderStackItem ) );
   if not Assigned( FT ) then
     FT := AllocMem( SizeOf( TNotesFolderTree ) );
   with FSI^ do begin
     FolderTree := FT;
     Folder := AFolder;
   end;
   SetLength( FolderStack, Length( FolderStack ) + 1 );
   FolderStack[ High( FolderStack ) ] := FSI;
  end;

  procedure PopStack;
  begin
    CurStack := FolderStack[ High( FolderStack ) ];
    SetLength( FolderStack, High( FolderStack ) );
  end;

begin
  TheFolderTree := nil;
  PushStack( TheFolderTree, AddSlash( Folder ) );

  while Length( FolderStack ) > 0 do begin
    PopStack;

   if FindFirst( CurStack^.Folder + FolderMask, faAnyFile, SearchRec ) = 0 then
   begin
     repeat
       if ( SearchRec.Attr and faDirectory <> 0 ) and ( SearchRec.Name[1] <> '.' ) then
       begin
         if Options = ftOnlyFiles then
         begin
           if ScanSubFolders then
             PushStack( CurStack^.FolderTree, AddSlash( CurStack^.Folder + SearchRec.Name ) )
           else
             Break;
         end else
         begin
           SetLength( CurStack^.FolderTree^, Length( CurStack^.FolderTree^ ) + 1 );
           with CurStack^.FolderTree^[ High( CurStack^.FolderTree^ ) ] do begin
             Path := SearchRec.Name;
             IsFolder := True;
             if ScanSubFolders then
               PushStack( Items, AddSlash( CurStack^.Folder + Path ) );
           end;
         end
       end;
     until FindNext( SearchRec ) <> 0;
   end;
   SysUtils.FindClose( SearchRec );

    if Options <> ftOnlyFolders then
    begin
     if FindFirst( CurStack^.Folder + FileMask, faAnyFile - faDirectory, SearchRec ) = 0 then
       repeat
         SetLength( CurStack^.FolderTree^, Length( CurStack^.FolderTree^ ) + 1 );
         with CurStack^.FolderTree^[ High( CurStack^.FolderTree^ ) ] do begin
           Path := SearchRec.Name;
           IsFolder := False;
         end;
       until FindNext( SearchRec ) <> 0;
     SysUtils.FindClose( SearchRec );
    end;

   CurStack^.Folder := '';
   FreeMem( CurStack );
  end;
end;

{
procedure BuildFolderTree( var TheFolderTree: PNotesFolderTree; Folder: string;
  const Options: TNotesBuildFolderTreeOptions = ftAll; const ScanSubFolders: Boolean = false;
  const FolderMask: string = '*'; const FileMask: string = '*' );

type

  PFolderStackItem = ^TFolderStackItem;
  TFolderStackItem = record
    FolderTree: PNotesFolderTree;
    Folder: string;
  end;

  TFolderStack = array of PFolderStackItem;

var
  FolderStack: TFolderStack;
  SearchRec: TSearchRec;
  CurStack: PFolderStackItem;
  SameMask: Boolean;

  procedure PushStack( var FT: PNotesFolderTree; const AFolder: string);
  var
    FSI: PFolderStackItem;
  begin
    FSI := AllocMem( SizeOf( TFolderStackItem ) );
    if not Assigned( FT ) then
      FT := AllocMem( SizeOf( TNotesFolderTree ) );
    with FSI^ do begin
      FolderTree := FT;
      Folder := AFolder;
    end;
    SetLength( FolderStack, Length( FolderStack ) + 1 );
    FolderStack[ High( FolderStack ) ] := FSI;
  end;

  procedure PopStack;
  begin
    CurStack := FolderStack[ High( FolderStack ) ];
    SetLength( FolderStack, High( FolderStack ) );
  end;

begin
  TheFolderTree := nil;
  SameMask := (FolderMask = FileMask);
  Folder := AddSlash( Folder );
  PushStack( TheFolderTree, Folder );

  while Length( FolderStack ) > 0 do begin
    PopStack;

    if ( Options in [ftAll,ftOnlyFolders] ) and ( FindFirst(CurStack^.Folder + FolderMask,
      faAnyFile, SearchRec ) = 0 ) then
    begin
      repeat
        if ( SearchRec.Attr and faDirectory <> 0 ) then
        begin
          if( SearchRec.Name[1] <> '.' ) then
          begin
            SetLength( CurStack^.FolderTree^, Length(CurStack^.FolderTree^ ) + 1 );
            with CurStack^.FolderTree^[ High( CurStack^.FolderTree^ ) ] do
            begin
              Path := SearchRec.Name;
              IsFolder := True;
              if ScanSubFolders then
                PushStack( Items, AddSlash( CurStack^.Folder + Path ) );
            end;
          end;
        end
        else if ( Options = ftAll ) and SameMask then
        begin
          SetLength( CurStack^.FolderTree^, Length(CurStack^.FolderTree^ ) + 1 );
          with CurStack^.FolderTree^[ High( CurStack^.FolderTree^ ) ] do
          begin
            Path := SearchRec.Name;
            IsFolder := False;
          end;
        end;
      until FindNext( SearchRec ) <> 0;
      SysUtils.FindClose( SearchRec );
    end
    else if FindFirst( CurStack^.Folder + FileMask, faAnyFile, SearchRec) = 0 then
    begin
      repeat
        if ( SearchRec.Attr and faDirectory <> 0 ) and (SearchRec.Name[1] <> '.' )
          and (ScanSubFolders) then
        begin
          PushStack( CurStack^.FolderTree, AddSlash( CurStack^.Folder + SearchRec.Name ) );
        end else
        if SearchRec.Attr and faDirectory = 0 then
        begin
          SetLength( CurStack^.FolderTree^, Length(CurStack^.FolderTree^ ) + 1 );
          with CurStack^.FolderTree^[ High( CurStack^.FolderTree^ ) ] do
          begin
            Path := SearchRec.Name;
            IsFolder := False;
          end;
        end;
      until FindNext( SearchRec ) <> 0;

      SysUtils.FindClose( SearchRec );

    end;
    CurStack^.Folder := '';
    FreeMem( CurStack );
  end;
end;         }

procedure FreeFolderTree( FolderTree: PNotesFolderTree );
var
  I: Integer;
Begin
  if Assigned( FolderTree ) then begin
    for I := 0 to High( FolderTree^ ) do
      if FolderTree^[I].IsFolder then
        FreeFolderTree( FolderTree^[I].Items );
    FreeMem( FolderTree );
  end;
end;


function  FindStr;
begin
 // SUPORTE PARA CORINGAS: \n igual a nova linha, \t = tab
  if CrLfAndTab then begin
    ASearchStr:=FastReplace(ASearchStr,'\\',''#7,False);
    ASearchStr:=FastReplace(ASearchStr,'\n',''#13#10,False);
    ASearchStr:=FastReplace(ASearchStr,'\t',''#9,False);
    ASearchStr:=FastReplace(ASearchStr,''#7,'\',False);
  end;
  Result:= -1;
  if MatchCase and BackWard then  // case e pra trás
    Result:= FastPosBack(AString,ASearchStr,length(AString),length(ASearchStr),StartAt) -1;
  if not MatchCase and BackWard then //nocase e pra trás
    Result:= FastPosBackNoCase(AString,ASearchStr,length(AString),length(ASearchStr),StartAt) -1;
  if MatchCase and not BackWard then // Case e pra frente
    Result:= FastPos(AString,ASearchStr,length(AString),length(ASearchStr),StartAt+1) -1;
  if not MatchCase and not BackWard then //nocase e pra frente
    Result:= FastPosNoCase(AString,ASearchStr,length(AString),length(ASearchStr),StartAt+1) -1;
end;

function GetFileName;
begin
  Result := DelFileExt( ExtractFileName( Filename ) );
end;

function AddSlash;
begin
  Result := Path;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: Integer ): Integer;
begin
  if Condition then
    Result := CaseTrue
  else
    Result := CaseFalse;
end;

function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: Pointer ): Pointer;
begin
  if Condition then
    Result := CaseTrue
  else
    Result := CaseFalse;
end;

function IIF( const Condition: Boolean; const CaseTrue, CaseFalse: String ): String;
begin
  if Condition then
    Result := CaseTrue
  else
    Result := CaseFalse;
end;

function GetSpecialFolder;
const
  FoldersMap: array[TNotesSpecialFolder] of Cardinal = ( CSIDL_DESKTOP, CSIDL_APPDATA, CSIDL_TEMPLATES, CSIDL_PROGRAMS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_STARTMENU, CSIDL_FONTS, CSIDL_HISTORY, CSIDL_COOKIES, CSIDL_INTERNET_CACHE, CSIDL_COMMON_FAVORITES, CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_COMMON_STARTUP, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTMENU );
var
  Path: PChar;
  Folder: PItemIDList;
begin
  Path := AllocMem(MAX_PATH);
  //API do Shell que retorna um ponteiro para um ItemIdList
  SHGetSpecialFolderLocation( 0, FoldersMap[FolderType], Folder );
  //Retorna o path dado um ItemIDList
  SHGetPathFromIDList( Folder, Path );
  //Retonar o resultado
  Result := AddSlash( StrPas( Path ) );
  //Liberar memória alocada
  FreeMem( Path );
end;

function FileToStr;
var
  FS: TFileStream;
  Len: Integer;
begin
  Result := '';
  Filename := PathGetLongName( Filename );
  if not FileExists( Filename ) then
    Exit;
  FS := TFileStream.Create( Filename, fmOpenRead);
  try
    Len := FS.Size;
    // Como usamos result[1] mais para frente, precisamos
    // sair no caso do arquivo estar vazio, ou ganharemos
    // um range check error... (uma acess violation, heheh)
    if Len = 0 then
    begin
      Result:='';
      // o finally executa mesmo como o exit :)
      Exit;
    end;
    SetLength( Result, Len );
    FS.Read( Result[1], Len );
  finally
    FS.Free;
  end;
end;

procedure StrToFile;
begin
  with TFileStream.Create( PathGetLongName( Filename ), fmCreate ) do
    try
      Write( Pointer( Content )^, Length( Content ) );
    finally
      Free;
    end;
end;

function PathGetLongName;
var
  I : Integer;
  SearchHandle : THandle;
  FindData : TWin32FindData;
  IsBackSlash : Boolean;
begin
  Path := ExpandFileName(Path);
  Result := ExtractFileDrive(Path);
  I := Length(Result);
  if Length(Path) <= I then
    Exit;   // only drive
  if Path[I + 1] = '\' then
  begin
    Result := Result + '\';
    Inc(I);
  end;
  Delete(Path, 1, I);
  repeat
    I := Pos('\', Path);
    IsBackSlash := I > 0;
    if Not IsBackSlash then
      I := Length(Path) + 1;
    SearchHandle := FindFirstFile(PChar(Result + Copy(Path, 1,
      I - 1)), FindData);
    if SearchHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        Result := Result + FindData.cFileName;
        if IsBackSlash then
          Result := Result + '\';
      finally
        Windows.FindClose(SearchHandle);
      end;
    end
    else
    begin
      Result := Result + Path;
      Break;
    end;
    Delete(Path, 1, I);
  until Length(Path) = 0;
end;

function GetBreakType;
var
  FS: TFileStream;
  Buffer, BufferStart, BufferEnd: PChar;
begin
  Result := btNone;
  if not FileExists( Filename ) then
    raise Exception.Create( 'GetBreakType: nome de arquivo inválido' );
  try
    FS := TFileStream.Create( Filename, fmOpenRead );
    GetMem( Buffer, MaxDataToRead+1 );
    BufferEnd := ( Buffer + FS.Read( Buffer^, MaxDataToRead ) );
    BufferStart := Buffer;
    BufferEnd^ := #0;
  except
    raise Exception.Create( 'GetBreakType: erro alocando memória.' );
  end;
  try
    while Buffer^ <> #0 do begin
      if Result = btNone then
        if Buffer^ = ASCII_CR then begin
          if (Buffer+1)^ = ASCII_LF then begin
            Result := btWin;
            Inc( Buffer );
          end
          else
            Result := btMac;
        end
        else if Buffer^ = ASCII_LF then
          Result := btLin;
      Inc( Buffer );
    end;
    if Buffer <> BufferEnd then
      Result := btBin;
  finally
    FreeMem( BufferStart, MaxDataToRead+1 );
    FS.Free;
  end;
end;

function IsNumber;
var
  pcString: PChar;
begin
  Result := True;
  if Length( S ) < 10 then begin
    pcString:= PChar( S );
    while pcString^ <> #0 do begin
      if not (pcString^ in ['0'..'9']) then begin
        Result := False;
        Exit;
      end;
      Inc( pcString );
    end;
  end
  else
    Result:= false;
end;

procedure StrReplaceAll;
var
  LenS, LenSearch: integer;
begin
  LenS:= Length(S);
  LenSearch := Length( Search );
  while FastPosNoCase( S, Search, LenS, LenSearch, 1 ) > 0 do
    S:= FastReplace( S, Search, Replace, False );

end;

function DelFileExt;
Var
  I: integer;
begin
  Result:= FileName;
  if FileName <> EmptyStr then
    for I:= length(FileName) downto 1 do
      if FileName[I] = '.' then begin
        SetLength(Result, I-1);
        Exit;
      end;
end;

function UTF8ToAnsi(x: string): ansistring;
var
  i: integer;
  b1, b2: byte;
begin
  Result := x;
  i := 1;
  while i <= Length(Result) do begin
    if (ord(Result[i]) and $80) <> 0 then begin
      b1 := ord(Result[i]);
      b2 := ord(Result[i + 1]);
      if (b1 and $F0) <> $C0 then
        Result[i] := #128
      else begin
        Result[i] := Chr((b1 shl 6) or (b2 and $3F));
        Delete(Result, i + 1, 1);
      end;
    end;
    inc(i);
  end;
end;

function AnsiToUtf8(x: ansistring): string;
var
  i: integer;
  b1, b2: byte;
begin
  Result := x;
  for i := Length(Result) downto 1 do
    if Result[i] >= #127 then begin
      b1 := $C0 or (ord(Result[i]) shr 6);
      b2 := $80 or (ord(Result[i]) and $3F);
      Result[i] := chr(b1);
      Insert(chr(b2), Result, i + 1);
    end;
end;

function WordWrap;
var
  I: integer;
  ProcessedChar: integer;
  SourceLen: integer;

  function LastSpace(ABegin, AEnd : Integer; S: string): integer;
  var
   I: integer;
  begin
    // resulta -1 se nenhum espaço for encontrado...
    Result:= -1;
    //é mais fácil achar o último começando do fim...
    for I:= AEnd downto ABegin do begin
      if S[I] = ' ' then begin
        Result:= I;
        Break;
      end;
    end;
  end;

begin
  //fastring para transformar CRLF em espaços
  Source := FastReplace( Source, LineBreak ,' ', True);
  I:= 1;
  ProcessedChar := I;
  SourceLen:= Length( Source );
  while (I < (SourceLen - MaxLineLen)) do begin
    I := LastSpace(I, I + MaxLineLen, Source);
    if I > 0 then begin
      ProcessedChar:= I;
      //usamos o caracter #7 para marcar o lugar da nova quebra de linha
      Source[I] := ASCII_BEEP;
    end
    else begin
      I:= FastPos( Source,' ', Length( Source ), 1, ProcessedChar );
      if I <= 0  then
        break;
    end;
  end;
  Result:= FastReplace( Source, ASCII_BEEP, LineBreak, True );
end;

procedure msgInfo;
begin
  MessageBox(FrmHandle, PChar(MsgTxt), NOTES_NICKNAME,  $40);
end;

function msgYesNo;
begin
  Result := MessageBox( FrmHandle, PChar(MsgTxt), NOTES_NICKNAME, $24);
end;

function msgQuest;
begin
  Result := MessageBox( FrmHandle, PChar(MsgTxt), NOTES_NICKNAME, $23);
end;

procedure msgOk;
begin
  MessageBox(FrmHandle , PChar(MsgTxt), PChar(MsgTitle),   $0);
end;

procedure msgExclama;
begin
  MessageBox( FrmHandle, PChar(MsgTxt), NOTES_NICKNAME, $1030);
end;

procedure SendToTrash;
var
  F : TShFileOpStruct;
begin
  F.Wnd := FrmHandle;
  F.wFunc := FO_DELETE;
  F.pFrom := PChar(FileOrFolder);
  F.fFlags := FOF_ALLOWUNDO;
  ShFileOperation(F);
end;

// CALLBACK usada na função BrowseForFolder
function BFFCallBack(Wnd: HWND; uMsg: UINT;
        lParam, lpData: LPARAM): Integer; stdcall;
Var
  StartFolder: string;
begin
  if uMsg = BFFM_INITIALIZED then
  begin
    StartFolder:= FileToStr(NProfile.Paths.UserDir + 'bff.dat');
    if DirectoryExists(StartFolder) then
      SendMessage(Wnd,BFFM_SETSELECTION,1,Integer(@StartFolder[1]));
  end;
  result := 0;
end;

function BrowseForFolder(AParent: HWND; ATitle : string) : string;
Var
  pIdList: PItemIDList;
  bi: TBrowseInfo;
  pcFolder: array[0..MAX_PATH] of char;
begin
  strpcopy(@pcFolder[0],ATitle);
  FillChar(bi,SizeOf(bi), #0);
  with bi do
  begin
    hwndOwner := AParent;
    pszDisplayName := nil;
    lpszTitle := @pcFolder[0];
    ulFlags := BIF_RETURNONLYFSDIRS or BIF_RETURNFSANCESTORS or BIF_NEWDIALOGSTYLE;
    lpfn := BFFCallBack;
    lParam := 0;
    iImage := 0;
  end;
  pIdList := SHBrowseForFolder(bi);

  if assigned(pIdList) then
  begin
    if SHGetPathFromIDList(pIdList,@pcFolder) then
    begin
      Result := StrPas(@pcFolder[0]);
      StrToFile(NProfile.Paths.UserDir + 'bff.dat', Result);
    end;
      GlobalFreePtr(pIdList);
  end;
end;

function ExplodeStr(S: string; const Delimiter: Char): TStringArray;
Var
  I, tStart, tEnd, tCount, Len: integer;
begin
  len:= length(S);
  tStart:= 1;
  tCount:= 0;

  if Len = 0 then Exit;

  if S[Len] = Delimiter then
  begin
    Dec(Len);
    setLength(S, Len);
  end;

  for I:= 1 to Len do
  begin
    if S[I] = Delimiter then
    begin
      tEnd:= I - 1;
      inc(tCount);
      setLength(Result, tCount);
      Result[tCount-1] := copy(S, tStart, tEnd - tStart +1);
      tStart:= tEnd + 2;
    end;
  end;
  if tStart <= Len then
  begin
    inc(tCount);
    setLength(Result, tCount);
    Result[tCount-1] := copy(S, tStart, Len);
  end;
end;

function ImplodeStr(arr: TStringArray; const Delimiter: Char): string;
Var
  I: integer;
begin
  for I:=0 to High(arr) do
    Result:= Result + String(Delimiter) + arr[I];
  // retira o primeiro ';'
  if length(Result) > 0 then
    Result:= Copy(Result, 2, length(Result) - 1);
end;

function ExplodeInt(S: string; const Delimiter: Char): TIntegerArray;
Var
  I, tStart, tEnd, tCount, Len: integer;
begin
  len:= length(S);
  tStart:= 1;
  tCount:= 0;

  if Len = 0 then Exit;

  if S[Len] = Delimiter then
  begin
    Dec(Len);
    setLength(S, Len);
  end;

  for I:= 1 to Len -1 do
  begin
    if S[I] = Delimiter then
    begin
      tEnd:= I - 1;
      inc(tCount);
      setLength(Result, tCount);
      Result[tCount-1] := StrToIntDef(copy(S, tStart, Len), -1);
      tStart:= tEnd + 2;
    end;
  end;

  if tStart < Len - 1 then
  begin
    inc(tCount);
    setLength(Result, tCount);
    Result[tCount-1] := StrToIntDef(copy(S, tStart, Len - tStart + 1), -1);
  end;

end;

function ImplodeInt(arr: TIntegerArray; const Delimiter: Char): string;
Var
  I: integer;
begin
  for I:=0 to High(arr) do
    Result:= Result + String(Delimiter) + IntToStr( arr[I] );
  // retira o primeiro ';'
  if length(Result) > 0 then
    Result:= Copy(Result, 2, length(Result) - 1);
end;


procedure DeleteFolderTree(FolderTree: PNotesFolderTree; FromPath: string; CallBack: TNotesFolderTreeOperationCallBack);
Var
  I: integer;
  DoAbort: boolean;

  procedure DelFT(Tree: PNotesFolderTree; FromPath: string; CallBack: TNotesFolderTreeOperationCallBack; Var CurCount: integer; Var Stop: boolean);
  Var
    I: integer;
    S: string;
  begin
    for I := 0 to High( Tree^ ) do
    begin

      if Stop then Exit;

      with Tree^[I] do
      begin
        if IsFolder then
        begin
          DelFT(Items,Addslash(Addslash(FromPath) + Path), CallBack, Curcount, Stop);
          if RemoveDir(Addslash(Addslash(FromPath) + Path)) then
          begin
            inc(CurCount);
            if Assigned(CallBack) then
              CallBack(Addslash(FromPath) + Path, CurCount, Stop);
          end;
        end else
        begin
          S:= Addslash(FromPath) + Path;
          if SysUtils.DeleteFile(S) then
          begin
            inc(CurCount);
            if assigned(CallBack) then
              CallBack(Addslash(FromPath) + Path, CurCount, Stop);
          end;
        end;
      end;
    end;
  end;

begin
  I:= 0;
  DelFT(FolderTree, fromPath, CallBack, I, DoAbort);
end;

procedure CopyFolderTree(Tree: PNotesFolderTree; const FromDir, ToDir: string; const OverwriteOldFiles: boolean; CallBack: TNotesFolderTreeOperationCallBack);
Var
  I: integer;
  DoAbort: boolean;

  procedure CopyFT(Tree: PNotesFolderTree; const FromDir, ToDir: string; Count: integer; const OverwriteOldFiles: boolean; CallBack: TNotesFolderTreeOperationCallBack; Var Stop: boolean);
  Var
    I: integer;
  begin
    for I := 0 to High( Tree^ ) do
    begin

     if Stop then Exit;

      with Tree^[I] do
      begin
        if IsFolder then
        begin
          SysUtils.CreateDir(AddSlash(addSlash(ToDir) + Path ));
          CopyFT( Items, AddSlash(addSlash(fromDir) + Path ), AddSlash(addSlash(ToDir) + Path ) , Count, OverwriteOldFiles, CallBack , Stop);
        end else
        begin
          CopyFile(PChar(addSlash(FromDir) + Path), PChar(addSlash(ToDir) + Path), OverwriteOldFiles);
        end;
        Inc(Count);
        if assigned(CallBack) then
          CallBack(addSlash(ToDir) + Path, Count, Stop);
      end;
    end;
  end;

begin
  I:= 0;
  DoAbort:= false;
  CopyFT(Tree, FromDir, ToDir, I, OverwriteOldFiles, CallBack, DoAbort);
end;

procedure CountFolderTree(Tree: PNotesFolderTree; Var Count: integer);
Var
  I: integer;
begin
    for I := 0 to High( Tree^ ) do
    begin
      with Tree^[I] do
      begin
        if IsFolder then
          CountFolderTree(Items, Count);
        Inc(Count);
      end;
    end;
end;


function strHash(S: string): integer;
var
  I, G: integer;
begin
  Result:= 0;
  for I:= 1 to length(S) do
  begin
    Result:= Result shl 4;
    inc(Result, Ord(S[I]));
    G:= Result and integer($f shl 28);
    if G <> 0 then
    begin
      Result:= Result xor (G shr 24);
      Result:= Result xor  G;
    end;
  end;
  
  if Result = 0 then
    Result:= Not(0);
end;

function findLineBreak(const S: string; const offset: integer; const Backwards: boolean): integer;
Var
  I, Len: integer;
begin
  Len:= length(S);
  Result:= -1;
  if Len = 0 then Exit;
  if (offset > Len) or (offset < 1) then
    raise Exception.Create('findLineBreak - offset can not be bigger than the length of the string or less than 1.');
  if not Backwards then
  begin
    for I:= offset to Len do
      if S[I] in [#10, #13] then
      begin
        Result:= I;
        Exit;
      end;
  end else
  begin
    for I:= offset downto 1 do
      if S[I] in [#10, #13] then
      begin
        Result:= I;
        Exit;
      end;
  end;
end;

procedure SelectTreeItem(const ATree: TTreeView; const ItemTxt, ParentTxt: string);
var
  Node: TTreeNode;
begin
  if ATree.Items.Count = 0 then Exit;
  Node := ATree.Items[0];
  while Node <> nil do
  begin
    if SameText(Node.Text, ItemTxt) then
    begin
      if ((Node.Parent = nil) and (ParentTxt = '')) or ( (Node.Parent <> nil) and (SameText(Node.Parent.Text, ParentTxt)) ) then
      begin
        Node.MakeVisible;
        ATree.Select(Node);
        Exit;
      end;
    end;
    Node := Node.GetNext;
  end;
  // se não encontramos, selecionamos o primeiro
  ATree.Select(ATree.Items[0]);
end;

function SplitFilter(Filter: string): TStringList;
var
    P : integer;
begin
    // Separa uma lista de filtros separados por ; em um stringlist

    Result := TStringList.Create;
//    Filter := StrRemoveChars(Filter, [' ']);

    while Filter <> '' do
    begin
      P := Pos(';', Filter);

      if P = 0 then
        begin
          Result.Add(Filter);
          Exit;
        end
      else
        begin
          Result.Add(Copy(Filter, 1, P-1));
          Delete(Filter, 1, P);
        end;
    end;
end;

function WildcardCompare(FileWild, FileName: string): boolean;
var
  C        : integer;
  NameMask : string;
  ExtMask  : string;

    function WComp(Mask,Name: string): boolean;
    var
      ChM     : char;
      I,J,L,P : integer;
      LenM    : integer;
      LenN    : integer;
    begin
      LenM := Length(Mask);
      LenN := Length(Name);

      I := 1;
      J := 1;

      while (I <= LenM) do
      begin
        ChM := Mask[I];

        if ChM = '*' then
          begin
            if I = LenM then
              begin
                Result := true;
                Exit;
              end
            else
              begin
                // we need to synchronize

                L := I + 1;

                while (L < LenM) and (Mask[L+1] <> '*') do
                  Inc(L);

                P := Pos(Copy(Mask, I+1, L-I), Name);

                if P > 0 then
                  J := P-1
                else
                  begin
                    Result := false;
                    Exit;
                  end;
              end;
          end
        else if (ChM <> '?') and ((LenN < I) or (ChM <> Name[J])) then
          begin
            Result := false;
            Exit;
          end;

        Inc(I);
        Inc(J);
      end;

      Result := (J > LenN);
    end;

begin
    C := Pos('.', FileWild);

    // Se nao tem extensao coloco * para casar com qualquer coisa

    if C = 0 then
      begin
        NameMask := FileWild;
        ExtMask  := '*';
      end
    else
      begin
        NameMask := Copy(FileWild, 1, C-1);
        ExtMask  := Copy(FileWild, C+1, Length(FileWild));
      end;

    C := Pos('.', FileName);

    if C = 0 then
      C := Length(FileName) + 1;

    Result := WComp(NameMask, Copy(FileName, 1, C-1)) and
              WComp(ExtMask,  Copy(FileName, C+1, Length(FileName)));
end;


end.
