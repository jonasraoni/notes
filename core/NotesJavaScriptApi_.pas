//
//    NotesJavaScriptApi - implementação da api para JavaScripts.
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
  @abstract(NotesJavaScriptApi - implementação da api para JavaScripts.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  Esta unit possui as funções básicas que podem ser chamadas
  de qualquer JavaScript no Notes. Para o script poder usar
  estas funções, adicione cada uma delas a engine e depois
  crie as classes e conecte-as a engine.
*)
unit NotesJavaScriptApi;

interface

uses Windows, SysUtils, Classes, dialogs, jsintf, jsbridge, js15decl,
  NotesGlobals, NotesEditorTab, SynEditTypes;

type
  // Classe que dá acesso a funções e propriedades do editor por meio de JS
  TNotesJSEditorApi = class(TJSBridge)
  private
    fViewPos: TBufferCoord;
    fViewTopLine: integer;
    function getactiveLineText: string;
    function getcol: integer;
    function getindentSize: integer;
    function getline: integer;
    function getlinesCount: integer;
    function getmodified: boolean;
    function getReadOnly: boolean;
    function getselLength: integer;
    function getselStart: integer;
    function getSelText: string;
    function gettabSize: integer;
    function gettext: string;
    procedure setactiveLineText(const Value: string);
    procedure setcol(const Value: integer);
    procedure setfileType(const Value: string);
    procedure setindentSize(const Value: integer);
    procedure setline(const Value: integer);
    procedure setmodified(const Value: boolean);
    procedure setReadOnly(const Value: boolean);
    procedure setselLength(const Value: integer);
    procedure setselStart(const Value: integer);
    procedure setSelText(const Value: string);
    procedure settabSize(const Value: integer);
    procedure settext(const Value: string);
    function getfileType: string;
    function getIsActive: boolean;
  published
    procedure saveView;
    procedure restoreView;
    procedure insert(const S: string);
    property text: string read gettext write settext;
    property selText: string read getSelText write setSelText;
    property lineText: string read getactiveLineText write setactiveLineText;
    property line: integer read getline write setline;
    property col: integer read getcol write setcol;
    property selStart: integer read getselStart write setselStart;
    property selLength: integer read getselLength write setselLength;
    property linesCount: integer read getlinesCount;
    property fileType: string read getfileType write setfileType;
    property tabSize: integer read gettabSize write settabSize;
    property indentSize: integer read getindentSize write setindentSize;
    property modified: boolean read getmodified write setmodified;
    property isReadOnly: boolean read getReadOnly write setReadOnly;
    property isActive: boolean read getIsActive;
  end;

type
  // Classe que dá acesso a arquivos/tabs do Notes ao JS
  TNotesJSTabsApi = class(TJSBridge)
  private
    function getActiveCaption: string;
    function getActiveFileName: string;
    function getActiveType: string;
    function getCount: integer;
    function getTabIndex: integer;
    procedure setTabIndex(const Value: integer);
  published
    procedure openFile(const fileName: string);
    procedure nextTab;
    procedure previusTab;
    procedure gotoFile(const fileName: string);
    procedure gotoFileAndMarkLine(const fileName: string; line: integer);
    property  activeTabIndex: integer read getTabIndex write setTabIndex;
    property  activeTabCaption: string read getActiveCaption;
    property  activeFileName: string read getActiveFileName;
    property  activeTabType: string read getActiveType;
    property  count: integer read getCount;
  end;

function js_notesExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_notesPanelExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notesPanelSetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notesPanelGetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_shellExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_getClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_setClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_setEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_getEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_getCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_setCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_getNotesDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_getNotesSettingsDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_getActiveProfile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_fileExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileGetContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_filePutContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileIsReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_fileSearch(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_moveOrRename(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dirExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dirCreate(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dirCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dirDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dirDelTree(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_confirmDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_msgDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_errorDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_inputDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_colorDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_openDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_saveDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;


implementation

uses NotesUtils, NotesCopyDeldialog, ShellApi, NotesEditorTabPosList, clipbrd, Forms;

const
  DLGS_TITLE: PChar = 'Notes Script';

function js_notesExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  Application.ProcessMessages;
  if argc = 1 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    S := JSStringToString(JSValToJSString(argv^));

    if S = '' then
    begin
      Result:= JS_FALSE;
      Exit;
    end;

    NotesExec(S);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_notesPanelExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sAction, sParam: string;
begin
  Application.ProcessMessages;
  if argc >= 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sName := JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sAction:= JSStringToString(JSValToJSString(argv^));
    sParam:= '';
    if argc = 3 then
    begin
      inc(argv);
      if not JSValIsString(argv^) then
      begin
        result:= JS_FALSE;
        Exit;
      end;
      sParam:= JSStringToString(JSValToJSString(argv^));
    end;

    rval^:= JSStringToJSVal(StringToJSString(cx, PanelExec(sName, sAction, sParam) ));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_notesPanelSetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sProp, sVal: string;
begin
  if argc = 3 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sName := JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sProp:= JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sVal:= JSStringToString(JSValToJSString(argv^));

    PanelSetProperty(sName, sProp, sVal);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_notesPanelGetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sProp: string;
begin
  if argc = 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    sName := JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    sProp:= JSStringToString(JSValToJSString(argv^));

    rval^:= JSStringToJSVal(StringToJSString(cx, PanelGetProperty(sName, sProp) ));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_shellExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sParam: string;
begin
  Application.ProcessMessages;
  if argc >= 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    sName := JSStringToString(JSValToJSString(argv^));
    sParam:='';
    if argc = 2 then
    begin
      inc(argv);
      if not JSValIsString(argv^) then
      begin
        result:= JS_FALSE;
        Exit;
      end;

      sParam:= JSStringToString(JSValToJSString(argv^));
    end;

    ShellExecute(0, 'open', PChar(sName), PChar(sParam), '', SW_SHOWNORMAL);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_getNotesDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NExePath ));
  Result:= JS_TRUE;
end;

function js_getNotesSettingsDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NDataPath ));
  Result:= JS_TRUE;
end;

function js_getActiveProfile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NProfile.getActiveProfile ));
  Result:= JS_TRUE;
end;

//
//  File System APIs
//

function js_fileExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if argc = 1 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    S := JSStringToString(JSValToJSString(argv^));

    rval^:= BoolToJSVal(FileExists(S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_fileGetContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  Application.ProcessMessages;
  if argc = 1 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    S := JSStringToString(JSValToJSString(argv^));
    rval^:= JSStringToJSVal(StringToJSString(cx, FileToStr(S)));
    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_filePutContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S, S2: string;
begin
  if argc = 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    S := JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    S2 := JSStringToString(JSValToJSString(argv^));
    StrToFile(S, S2);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_fileCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  fromFile, toFile: string;
  overw: longbool;
begin
  Application.ProcessMessages;
  if argc > 1 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    fromFile := JSStringToString(JSValToJSString(argv^));
    inc(argv);

    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    toFile := JSStringToString(JSValToJSString(argv^));
    // o terceiro argumento é opcional...
    if argc > 2 then
    begin
      inc(argv);

      if not JSValIsBoolean(argv^) then
      begin
        result:= JS_FALSE;
        Exit;
      end;

      overw:= JSValToBoolean(argv^);
    end else
      overw:= true;

    rval^:= BoolToJSVal( Windows.CopyFile(PChar(fromFile), PChar(toFile), overw) );
    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_fileDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    rval^:= BoolToJSVal(DeleteFile(S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_fileIsReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    rval^:= BoolToJSVal( FileIsReadOnly(S) );

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_fileSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
  FS: TFileStream;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    if not FileExists(S) then
    begin
      Result:= JS_FALSE;
      Exit;
    end;
    FS := TFileStream.Create(S , fmOpenRead);
    try
      // não temos como passar algo maior que um integer...
      if FS.Size > High(Integer) then
        rval^:= IntToJsval(High(Integer))
      else
        rval^:= IntToJsval(FS.Size);
    finally
      FS.Free;
    end;

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


// move ou renomeia diretórios e arquivos
function js_moveOrRename(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  fromFile, toFile: string;
  overw: longbool;
begin
  if (argc > 1 ) and (JSValIsString(argv^)) then
  begin
    fromFile := JSStringToString(JSValToJSString(argv^));
    inc(argv);

    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    toFile := JSStringToString(JSValToJSString(argv^));
    overw:= true;
    // o terceiro argumento é opcional...
    if argc = 3 then
    begin
      inc(argv);
      if JSValIsBoolean(argv^) then
        overw:= JSValToBoolean(argv^);
    end;

    if (overw) and (FileExists(toFile)) then
      DeleteFile(ToFile);

    if (overw = false) and ((FileExists(toFile)) or (DirectoryExists(toFile))) then
    begin
      rval^:= BoolToJSVal( false );
      Result:= JS_TRUE;
      Exit;
    end;

    rval^:= BoolToJSVal( Windows.MoveFile(PChar(FromFile), PChar(ToFile)) );
    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

// Função usada por js_fileSearch
// filefind espera receber um diretório com o backslash "\"
// todos os diretórios acionados a str posseum o backslash
procedure FileFind(Var FindResult: string; const dir, mask: string; recurse: boolean);
var
  sr: TSearchRec;
begin
  Application.ProcessMessages;
  if findFirst(dir+ mask, faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) <> 0 then
      begin
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          FindResult:= FindResult +#10''+ addSlash( dir + sr.Name );
          if recurse then
            FileFind(FindResult, addSlash(dir + sr.Name), mask, true);
        end;
      end else
        FindResult:= FindResult +#10''+ dir + sr.Name;
    until findNext(sr) <> 0;
  end;
  findClose(sr);
end;


function js_fileSearch(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  dirname, mask, retVal: string;
  recursive: boolean;
begin
  if (argc > 0 ) and (JSValIsString(argv^)) then
  begin
    dirname:= JSStringToString(JSValToJSString(argv^));
    dirname:= addSlash(dirname);
    mask:='*';
    recursive:= false;
    if argc > 1 then
    begin
      inc(argv);
      if not JSValIsString(argv^) then
      begin
        result:= JS_FALSE;
        Exit;
      end;
      mask:= JSStringToString(JSValToJSString(argv^));
      if argc= 3 then
      begin
        inc(argv);
        if JSValIsBoolean(argv^) then
        recursive:= JSValToBoolean(argv^);
      end;
    end;

    retVal:= '';
    FileFind(retVal, dirname, mask, recursive);
    // deletamos a primeira quebra de linha
    if (retVal <> '') and (retVal[1] =#10) then
      System.Delete(retVal, 1, 1);

    rval^:= JSStringToJSVal(StringToJSString(cx, retVal));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_dirDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    rval^:= BoolToJSVal( RemoveDir(S) );

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_dirCreate(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
  Force: boolean;
begin
  Application.ProcessMessages;
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    force:= false;
    if (argc = 2) then
    begin
      inc(argv);
      if JSValIsBoolean(argv^) then
        force:= JSValToBoolean(argv^);
    end;

    if force then
      rval^:= BoolToJSVal( ForceDirectories(S) )
    else
      rval^:= BoolToJSVal( CreateDir(S) );

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_dirExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    rval^:= BoolToJSVal(DirectoryExists(S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_dirCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  TaskOk, Overw: boolean;
  fPath, tPath, fMask: string;
begin
  // (from, to, mask, overwrite )

  if (argc < 2) or (JSValIsString(rval^) = false) then
  begin
    Result:= JS_FALSE;
    Exit;
  end;

  fPath:= JSStringToString(JSValToJSString(argv^));
  inc(argv);

  if not JSValIsString(argv^) then
  begin
    result:= JS_FALSE;
    Exit;
  end;

  tPath := JSStringToString(JSValToJSString(argv^));

  fMask:= '*';
  if argc >= 3 then
  begin
    inc(argv);
    if JSValIsString(rval^) then
      fMask := JSStringToString(JSValToJSString(argv^));
  end;

  overw:= true;
  if argc = 4 then
  begin
    inc(argv);
    if JSValIsBoolean(rval^) then
      overw:= JSValToBoolean(argv^);
  end;

  with TfrmCopyDel.Create(nil) do
  begin
    try
      TheCaption:= string(DLGS_TITLE);
      OverwriteOldFiles:= Overw;
      Mask:= fMask;
      Path:= fPath;
      ToPath:= tPath;
      Operation:= foCopy;
      TaskOk:= Execute;
    finally
      Free;
    end;
  end;

  rval^:= BoolToJSVal( TaskOk );

  result:= JS_TRUE;
end;

function js_dirDelTree(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  TaskOk: boolean;
  fPath, fMask: string;
begin
  // (dir, mask)

  if (argc < 1) or ( JSValIsString(rval^) = false ) then
  begin
    Result:= JS_FALSE;
    Exit;
  end;

  fPath:= JSStringToString(JSValToJSString(argv^));

  fMask:= '*';
  if argc = 2 then
  begin
    inc(argv);
    if JSValIsString(rval^) then
      fMask := JSStringToString(JSValToJSString(argv^));
  end;

  with TfrmCopyDel.Create(nil) do
  begin
    try
      TheCaption:= string(DLGS_TITLE);
      Mask:= fMask;
      Path:= fPath;
      Operation:= foDel;
      TaskOk:= Execute;
    finally
      Free;
    end;
  end;

  rval^:= BoolToJSVal( TaskOk );

  result:= JS_TRUE;
end;

///
///  OS FUNCTIONS...
///

function js_getClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  S:= '';

  if Clipboard.HasFormat(CF_TEXT) then
    S:= clipboard.AsText;

  rval^:= JSStringToJSVal(StringToJSString(cx, S ));
  Result:= JS_TRUE;
end;

function js_setClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    clipboard.AsText:= S;

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_setEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sVal: string;
begin
  if (argc = 2 ) and (JSValIsString(argv^)) then
  begin
    sName := JSStringToString(JSValToJSString(argv^));

    if sName = '' then
    begin
      Result:= JS_FALSE;
      Exit;
    end;

    inc(argv);

    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    sVal := JSStringToString(JSValToJSString(argv^));

    SetEnvironmentVariable(PChar(sName), PChar(sVal));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_getEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    if S = '' then
    begin
      Result:= JS_FALSE;
      Exit;
    end;

    S:= GetEnvironmentVariable(S);

    rval^:= JSStringToJSVal(StringToJSString(cx, S ));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_getCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, GetCurrentDir ));
  Result:= JS_TRUE;
end;

function js_setCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    if (S = '') or (DirectoryExists(S) = false) then
    begin
      Result:= JS_FALSE;
      Exit;
    end;

    SetCurrentDir(S);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

///
/// DIALOGS APIS
///

function js_confirmDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    case MessageBox(0, PChar(S), DLGS_TITLE, $23) of
      IDYES: S:='yes';
      IDNO: S:= 'no';
      IDCANCEL: S:= 'cancel';
    end;

    rval^:= JSStringToJSVal(StringToJSString(cx, S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_msgDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    MessageBox(0 , PChar(S), DLGS_TITLE,  $40);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_errorDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    MessageBox(0 , PChar(S), DLGS_TITLE, $1030);

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_inputDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S, S2: string;
begin
  if (argc > 0 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    S2:= '';
    if argc = 2 then
    begin
      inc(argv);
      if JSValIsString(rval^) then
        S2 := JSStringToString(JSValToJSString(argv^));
    end;

    S:= InputBox(DLGS_TITLE, S, S2);
    rval^:= JSStringToJSVal(StringToJSString(cx, S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_colorDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  S:='';

  with TColorDialog.Create(nil) do
  begin
    try
      if Execute then
      begin
        // coversão para o formato HTML
        S:= IntToHex(Color, 6);
        S:= '#' + Copy(S, 5, 2) + Copy(S, 3, 2) + Copy(S, 1, 2);
      end;
     finally
       Free;
     end;
  end;

  rval^:= JSStringToJSVal(StringToJSString(cx, S));

  Result:= JS_TRUE;
end;

function js_openDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  S:='';

  with TOpenDialog.Create(nil) do
  begin
    try
      Filter:='(*.*)|*.*';
      if Execute then
        S:= FileName;
    finally
      Free;
    end;
  end;

  rval^:= JSStringToJSVal(StringToJSString(cx, S));
  Result:= JS_TRUE;
end;

function js_saveDlg(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  S:='';

  with TSaveDialog.Create(nil) do
  begin
    try
      Filter:='(*.*)|*.*';
      if Execute then
        S:= FileName;
    finally
      Free;
    end;
  end;

  rval^:= JSStringToJSVal(StringToJSString(cx, S));
  Result:= JS_TRUE;
end;


{ TNotesJSEditorApi }

function TNotesJSEditorApi.getactiveLineText: string;
begin
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.LineText;
end;

function TNotesJSEditorApi.getcol: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.CaretX;
end;

function TNotesJSEditorApi.getfileType: string;
begin
  Application.ProcessMessages;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.FileType;
end;

function TNotesJSEditorApi.getindentSize: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.IndentSize;
end;

function TNotesJSEditorApi.getIsActive: boolean;
begin
  Application.ProcessMessages;
  result:= ActiveEditorTab <> nil;
end;

function TNotesJSEditorApi.getline: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.CaretY;
end;

function TNotesJSEditorApi.getlinesCount: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.Lines.Count;
end;

function TNotesJSEditorApi.getmodified: boolean;
begin
  Result:= false;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.Modified;
end;

function TNotesJSEditorApi.getReadOnly: boolean;
begin
  Result:= true;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.ReadOnly;
end;

function TNotesJSEditorApi.getselLength: integer;
begin
  Result:= 0;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.SelLength;
end;

function TNotesJSEditorApi.getselStart: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.SelStart;
end;

function TNotesJSEditorApi.getSelText: string;
begin
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.SelText;
end;

function TNotesJSEditorApi.gettabSize: integer;
begin
  Result:= 1;
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.TabWidth;
end;

function TNotesJSEditorApi.gettext: string;
begin
  if ActiveEditorTab <> nil then
    result:= ActiveEditorTab.Editor.Text;
end;

procedure TNotesJSEditorApi.insert(const S: string);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.SelText:= S;
end;

procedure TNotesJSEditorApi.restoreView;
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.TopLine:= fViewTopLine;
  ActiveEditorTab.Editor.CaretXY:= fViewPos;
end;

procedure TNotesJSEditorApi.saveView;
begin
  if ActiveEditorTab = nil then Exit;
  fViewTopLine:= ActiveEditorTab.Editor.TopLine;
  fViewPos:= ActiveEditorTab.Editor.CaretXY;
end;

procedure TNotesJSEditorApi.setactiveLineText(const Value: string);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.LineText:= Value;
end;

procedure TNotesJSEditorApi.setcol(const Value: integer);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.CaretX:= Value;
end;

procedure TNotesJSEditorApi.setfileType(const Value: string);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.FileType:= Value;
  Application.ProcessMessages;
end;

procedure TNotesJSEditorApi.setindentSize(const Value: integer);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.IndentSize:= Value;
end;

procedure TNotesJSEditorApi.setline(const Value: integer);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.CaretY:= Value;
end;

procedure TNotesJSEditorApi.setmodified(const Value: boolean);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.Modified:= Value;
end;

procedure TNotesJSEditorApi.setReadOnly(const Value: boolean);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.ReadOnly:= Value;
end;

procedure TNotesJSEditorApi.setselLength(const Value: integer);
begin
  Application.ProcessMessages;
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.SelLength:= Value;
end;

procedure TNotesJSEditorApi.setselStart(const Value: integer);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.SelStart:= Value;
end;

procedure TNotesJSEditorApi.setSelText(const Value: string);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.SelText:= Value;
end;

procedure TNotesJSEditorApi.settabSize(const Value: integer);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.TabWidth:= Value;
  ActiveEditorTab.Editor.Invalidate;
end;

procedure TNotesJSEditorApi.settext(const Value: string);
begin
  if ActiveEditorTab = nil then Exit;
  ActiveEditorTab.Editor.Text:= Value;
  Application.ProcessMessages;
end;

{ TNotesJSTabsApi }

function TNotesJSTabsApi.getActiveCaption: string;
begin
  Result:='';
  if ActiveEditorTab <> nil then
    Result:= ActiveEditorTab.Tab.Caption;
end;

function TNotesJSTabsApi.getActiveFileName: string;
begin
  Result:='';
  if ActiveEditorTab <> nil then
    Result:= ActiveEditorTab.FullPath;
end;

function TNotesJSTabsApi.getActiveType: string;
begin
  if tabs.IsStartPageActivated then
    result:= 'startpage'
  else if tabs.IsEditorActivated then
  begin
    if ActiveEditorTab.FullPath <> '' then
      result:= 'file'
    else
      result:= 'newfile';
  end else
    result:= 'none';
end;

function TNotesJSTabsApi.getCount: integer;
begin
  Result:= tabs.PageControl.PageCount;
end;

function TNotesJSTabsApi.getTabIndex: integer;
begin
  Result:= -1;
  if ActiveEditorTab <> nil then
    activeEditortab.Tab.TabIndex;
end;

procedure TNotesJSTabsApi.gotoFile(const fileName: string);
Var
  I: integer;
begin
  Application.ProcessMessages;
  for I:= pred(tabs.EditorsTabCount) to 0 do
    if SameText(tabs.EditorsTab[I].FullPath, fileName) then
    begin
      tabs.PageControl.ActivePage:= tabs.EditorsTab[I].Tab;
      if assigned(tabs.PageControl.onChange) then
        tabs.PageControl.onChange(nil);
      Exit;
    end;
end;

procedure TNotesJSTabsApi.gotoFileAndMarkLine(const fileName: string;
  line: integer);
var
  et: TNotesEditorTabPos;
begin
  et.Line:= line;
  et.Col:= 1;
  et.FileName:= fileName;
  GoToEditorTabPos(et, true);
end;

procedure TNotesJSTabsApi.nextTab;
begin
  Application.ProcessMessages;
  NotesExec('actViewNexttab');
end;

procedure TNotesJSTabsApi.openFile(const fileName: string);
begin
  if tabs.LoadFile(fileName) then
  begin
    tabs.PageControl.ActivePageIndex:= pred(tabs.PageControl.PageCount);
    NProfile.MRU.Add(fileName);
    if assigned(tabs.PageControl.onChange) then
      tabs.PageControl.onChange(self);
  end;
end;

procedure TNotesJSTabsApi.previusTab;
begin
  Application.ProcessMessages;
  NotesExec('actViewPreviustab');
end;

procedure TNotesJSTabsApi.setTabIndex(const Value: integer);
begin
  Application.ProcessMessages;
  if (Value <= tabs.PageControl.PageCount) and (Value > -1) then
    tabs.PageControl.ActivePageIndex:= Value;
end;

end.
