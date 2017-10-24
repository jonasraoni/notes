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
  estas funções, adicione cada uma delas a engine.
*)
unit NotesJavaScriptApi;

interface

uses Windows, SysUtils, Classes, dialogs, jsintf, jsbridge, js15decl,
  Controls, NotesGlobals, NotesEditorTab, SynEditTypes, Forms;


function js_notes_getNotesDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_getMacrosDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_getSettingsDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_getActiveProfile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_getDefaultFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_exec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_addEvent(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_removeEvent(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_update(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_panelSetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_panelGetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_panelExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_notes_status(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_editor_insert(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getSelText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setSelText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getLineText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setLineText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setCol(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getCol(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getSelStart(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setSelStart(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getSelLength(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setSelLength(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getLinesCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getTabSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setTabSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getIndentSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setIndentSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getModified(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setModified(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_setReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_lockUpdates(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_update(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_inserLineBreak(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_getMarksCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_addMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_removeMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_editor_clearMarks(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_tabs_nextTab(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_previusTab(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_gotoFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_gotoFileAndMarkLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_getTabIndex(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_setTabIndex(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_getTabCaption(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_getFileName(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_openFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_getTabType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_getCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_newFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_tabs_hasEditor(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_os_exec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_execAndWait(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_setClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_setCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_setEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getDirDelimiter(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getName(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getUser(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getVersion(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_getUptime(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileGetContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_filePutContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileAppend(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileSearch(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileIsReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_fileSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_dirExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_dirCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_dirDelTree(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_dirDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_dirCreate(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_move(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_os_rename(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

function js_dlgs_message(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_confirm(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_question(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_error(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_input(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_fileOpen(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_fileSave(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_folder(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_color(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_list(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
function js_dlgs_codeCompletition(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;

const
  // JSClass that represents the "notes" object
  jcNotesObj : JSClass = ( name : 'NotesNotesObject'; flags : JSCLASS_HAS_PRIVATE;
               addProperty : JS_PropertyStub; delProperty : JS_PropertyStub;
               getProperty : JS_PropertyStub; setProperty : JS_PropertyStub;
               enumerate : JS_EnumerateStub; resolve : JS_ResolveStub;
               convert : JS_ConvertStub;  finalize : JS_FinalizeStub );

  // JSClass that represents the "editor" object
  jcEditorObj : JSClass = ( name : 'NotesEditorObject'; flags : JSCLASS_HAS_PRIVATE;
               addProperty : JS_PropertyStub; delProperty : JS_PropertyStub;
               getProperty : JS_PropertyStub; setProperty : JS_PropertyStub;
               enumerate : JS_EnumerateStub; resolve : JS_ResolveStub;
               convert : JS_ConvertStub;  finalize : JS_FinalizeStub );

  // JSClass that represents the "os" object
  jcOsObj : JSClass = ( name : 'NotesOsObject'; flags : JSCLASS_HAS_PRIVATE;
               addProperty : JS_PropertyStub; delProperty : JS_PropertyStub;
               getProperty : JS_PropertyStub; setProperty : JS_PropertyStub;
               enumerate : JS_EnumerateStub; resolve : JS_ResolveStub;
               convert : JS_ConvertStub;  finalize : JS_FinalizeStub );

  // JSClass that represents the "tabs" object
  jcTabsObj : JSClass = ( name : 'NotesTabsObject'; flags : JSCLASS_HAS_PRIVATE;
               addProperty : JS_PropertyStub; delProperty : JS_PropertyStub;
               getProperty : JS_PropertyStub; setProperty : JS_PropertyStub;
               enumerate : JS_EnumerateStub; resolve : JS_ResolveStub;
               convert : JS_ConvertStub;  finalize : JS_FinalizeStub );

  // JSClass that represents the "dialogs" object
  jcDlgsObj : JSClass = ( name : 'NotesDlgsObject'; flags : JSCLASS_HAS_PRIVATE;
               addProperty : JS_PropertyStub; delProperty : JS_PropertyStub;
               getProperty : JS_PropertyStub; setProperty : JS_PropertyStub;
               enumerate : JS_EnumerateStub; resolve : JS_ResolveStub;
               convert : JS_ConvertStub;  finalize : JS_FinalizeStub );


implementation

uses NotesUtils, NotesCopyDeldialog, ShellApi, NotesEditorTabPosList,
  clipbrd, frm_ListDlg;

const
  DLGS_TITLE: PChar = 'Notes Script';

function js_notes_getNotesDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NExePath ));
  result:= JS_TRUE;
end;

function js_notes_getMacrosDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, nProfile.Paths.MacrosDir));
  result:= JS_TRUE;
end;

function js_notes_getSettingsDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NDataPath ));
  result:= JS_TRUE;
end;

function js_notes_getActiveProfile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, NProfile.getActiveProfile ));
  result:= JS_TRUE;
end;

function js_notes_getDefaultFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, nProfile.Config.DefaultFileType ));
  result:= JS_TRUE;
end;

function js_notes_exec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  Application.ProcessMessages;
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
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

function js_notes_addEvent(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  ///////////////////////
    result:= JS_TRUE;
end;

function js_notes_removeEvent(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
    //////////////////////
    result:= JS_TRUE;
end;

function js_notes_update(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  Application.ProcessMessages;
  if ActiveEditorTab <> nil then
    ActiveEditorTab.Editor.invalidate;
  Sleep(10);
  Application.ProcessMessages;
  result:= JS_TRUE;
end;

function js_notes_panelSetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_notes_panelGetProperty(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_notes_panelExec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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


function js_notes_status(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    setStatusMsg(JSStringToString(JSValToJSString(argv^)));
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;


function js_editor_insert(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  if argc > 0 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    edtab:= ActiveEditorTab;
    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    S := JSStringToString(JSValToJSString(argv^));
    if edtab = nil then Exit;
    edtab.Editor.SelText:= S;
    result:= JS_TRUE;
  end;
end;

function js_editor_getText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  edtab:= ActiveEditorTab;

  if argc = 1 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  if edtab = nil then Exit;
  rval^:= JSStringToJSVal(StringToJSString(cx, edtab.Editor.Text));

  Application.ProcessMessages;
  result:= JS_TRUE;
end;

function js_editor_setText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  if argc > 0 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    S := JSStringToString(JSValToJSString(argv^));

    edtab:= ActiveEditorTab;
    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;
    edtab.Editor.Text:= S;
    result:= JS_TRUE;
  end;
end;

function js_editor_getSelText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  edtab:= ActiveEditorTab;

  if argc = 1 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  if edtab = nil then Exit;
  rval^:= JSStringToJSVal(StringToJSString(cx, edtab.Editor.SelText));

  result:= JS_TRUE;
end;

function js_editor_setSelText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  if argc > 0 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    S := JSStringToString(JSValToJSString(argv^));

    edtab:= ActiveEditorTab;
    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;
    edtab.Editor.SelText:= S;
    result:= JS_TRUE;
  end;
end;

function js_editor_getLineText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  I: integer;
begin
  result:= JS_FALSE;
  edtab:= ActiveEditorTab;

  if (argc < 1) or (JSValIsInt(argv^) = false) then
    I:= -1
  else
    I:= JSValToInt(argv^);

  if argc = 2 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  if edtab = nil then Exit;

  if (I < 0) or (I > edtab.Editor.Lines.Count -1) then
    I:= edtab.Editor.CaretY -1;

  rval^:= JSStringToJSVal(StringToJSString(cx, edtab.Editor.Lines[I]));
  result:= JS_TRUE;
end;

function js_editor_setLineText(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  savepos: TBufferCoord;
  savetopline: Integer;
  line: Integer;
  txt: string;
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  if argc > 0 then
  begin
    if not JSValIsString(argv^) then
      Exit;

    txt := JSStringToString(JSValToJSString(argv^));

    line:= -1;
    edtab:= ActiveEditorTab;
    // texto [linha] [editor]

    if argc > 1 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        line:= JSValToInt(argv^);
    end;

    if argc = 3 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:=  tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    if (line < 0) or (line > edtab.Editor.Lines.Count -1) then
      line:= edtab.Editor.CaretY -1;

    with edtab.Editor do
    begin
      savepos:= CaretXY;
      savetopline:= Topline;
      BeginUpdate;
      CaretY:= line;
      CaretX:= 1;
      selLength:= length(lineText);
      selText:= txt;
      Topline:= savetopline;
      CaretXY:= savepos;
      EndUpdate;
    end;
    result:= JS_TRUE;
  end;
end;


function js_editor_getLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.CaretY-1);
  result:= JS_TRUE;
end;


function js_editor_setLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  ln: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    ln:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.CaretY:= ln + 1;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_setCol(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  col: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    col:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.CaretX:= col + 1;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getCol(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.CaretX-1);
  result:= JS_TRUE;
end;

function js_editor_getSelStart(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.SelStart-1);
  result:= JS_TRUE;
end;

function js_editor_setSelStart(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  sel: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    sel:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.SelStart:= sel + 1;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getSelLength(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.SelLength);
  result:= JS_TRUE;
end;

function js_editor_setSelLength(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  sel: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    sel:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.SelLength:= sel;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getLinesCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.Lines.Count);
  result:= JS_TRUE;
end;

function js_editor_getFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= JSStringToJSVal(StringToJSString(cx, edtab.FileType));
  result:= JS_TRUE;
end;

function js_editor_setFileType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;
  if argc > 0 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;
    S := JSStringToString(JSValToJSString(argv^));

    edtab:= ActiveEditorTab;
    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;
    edtab.FileType:= S;
    result:= JS_TRUE;
  end;
end;

function js_editor_getTabSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Editor.TabWidth);
  result:= JS_TRUE;
end;

function js_editor_setTabSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  size: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    size:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.TabWidth:= size;

    edtab.Editor.Invalidate;
    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getIndentSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.IndentSize);
  result:= JS_TRUE;
end;

function js_editor_setIndentSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  size: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    size:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.IndentSize:= size;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getModified(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= BoolToJSVal(edtab.Editor.Modified);
  result:= JS_TRUE;
end;

function js_editor_setReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  modif: boolean;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsBoolean(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    modif:= JSValToBoolean(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.ReadOnly:= modif;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;


function js_editor_setModified(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  modif: boolean;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsBoolean(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    modif:= JSValToBoolean(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    edtab.Editor.Modified:= modif;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= BoolToJSVal(edtab.Editor.ReadOnly);
  result:= JS_TRUE;
end;

function js_editor_lockUpdates(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  if ActiveEditorTab = nil then
  begin
    Result:= JS_FALSE;
    Exit;
  end;

  ActiveEditorTab.Editor.BeginUpdate;
  Application.ProcessMessages;
  result:= JS_TRUE;
end;

function js_editor_update(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  if ActiveEditorTab = nil then
  begin
    Result:= JS_FALSE;
    Exit;
  end;

  while ActiveEditorTab.Editor.PaintLock > 0 do
    ActiveEditorTab.Editor.EndUpdate;

  ActiveEditorTab.Editor.Update;
  Application.ProcessMessages;

  result:= JS_TRUE;
end;

function js_editor_inserLineBreak(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
const
  ecLineBreak= 509;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  edtab.Editor.ExecuteCommand(ecLineBreak, #0, nil);
  result:= JS_TRUE;
end;

function js_editor_getMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  I: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    I:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    if (I < 0) or (I > edTab.Marks.Count-1) then
      Exit;

    rval^:= IntToJSVal(edTab.Marks.Items[I]);

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_getMarksCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;

  rval^:= IntToJSVal(edtab.Marks.Count);
  result:= JS_TRUE;
end;

function js_editor_AddMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  I: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    I:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    if (I < 0) or (I > edTab.Marks.Count-1) then
      Exit;

    edTab.Marks.Add(I);
    edTab.Editor.InvalidateGutter;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_removeMark(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  I: integer;
begin
  Result:= JS_FALSE;

  if (argc > 0) and (JSValIsInt(argv^)) then
  begin
    edtab:= ActiveEditorTab;
    I:= JSValToInt(argv^);

    if argc = 2 then
    begin
      inc(argv);
      if JSValIsInt(argv^) then
        edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
    end;

    if edtab = nil then Exit;

    if (I < 0) or (I > edTab.Marks.Count-1) then
      Exit;

    edTab.Marks.Remove(I);
    edTab.Editor.InvalidateGutter;

    Application.ProcessMessages;
    result:= JS_TRUE;
  end;
end;

function js_editor_clearMarks(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  result:= JS_FALSE;

  edtab:= ActiveEditorTab;
  if (argc = 1) and (JSValIsInt(argv^)) then
    edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));

  if edtab = nil then Exit;
  edTab.Marks.Clear;
  edTab.Editor.InvalidateGutter;

  result:= JS_TRUE;
end;

function js_tabs_nextTab(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  Application.ProcessMessages;
  NotesExec('actViewNexttab');
  result:= JS_TRUE;
end;

function js_tabs_previusTab(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  Application.ProcessMessages;
  NotesExec('actViewPreviusTab');
  result:= JS_TRUE;
end;

function js_tabs_gotoFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  I: integer;
  FileName: string;
begin
  result:= JS_FALSE;
  if argc = 1 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    FileName:= JSStringToString(JSValToJSString(argv^));
    Application.ProcessMessages;
    for I:= pred(tabs.EditorsTabCount) to 0 do
      if SameText(tabs.EditorsTab[I].FullPath, fileName) then
      begin
        tabs.PageControl.ActivePage:= tabs.EditorsTab[I].Tab;
        if assigned(tabs.PageControl.onChange) then
          tabs.PageControl.onChange(nil);
        Exit;
      end;
    result:= JS_TRUE;

  end;
end;

function js_tabs_gotoFileAndMarkLine(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  et: TNotesEditorTabPos;
begin

  if argc = 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    et.FileName:= JSStringToString(JSValToJSString(argv^));
    Application.ProcessMessages;

    inc(argv);
    if JSValIsInt(argv^) then
    begin
      et.Line:= JSValToInt(argv^);
      et.Col:= 1;
    end else
    begin
      result:= JS_FALSE;
      Exit;
    end;

    GoToEditorTabPos(et, true);
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;

function js_tabs_getTabIndex(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= IntToJSVal(tabs.PageControl.ActivePageIndex);
  result:= JS_TRUE;
end;

function js_tabs_setTabIndex(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  if (argc= 1) and (JSValIsInt(argv^)) then
  begin
    tabs.PageControl.ActivePageIndex:= JSValToInt(argv^);
    if Assigned(tabs.PageControl.OnChange) then
      tabs.PageControl.OnChange(nil);
    Application.ProcessMessages;
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;

function js_tabs_getTabCaption(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  result:= JS_TRUE;
  if (argc= 1) and (JSValIsInt(argv^)) then
    rval^:= JSStringToJSVal(StringToJSString(cx, tabs.PageControl.Pages[JSValToInt(argv^)].Caption))
  else
    rval^:= JSStringToJSVal(StringToJSString(cx, tabs.PageControl.ActivePage.Caption));
end;

function js_tabs_getFileName(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  edtab:= ActiveEditorTab;

  if argc = 1 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  if edtab <> nil then
    rval^:= JSStringToJSVal(StringToJSString(cx, edtab.FullPath))
  else
    rval^:= JSStringToJSVal(StringToJSString(cx, ''));

  Application.ProcessMessages;
  result:= JS_TRUE;
end;

function js_tabs_openFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  fileName: string;
begin
  if JSValIsString(argv^) then
  begin
    fileName:= JSStringToString(JSValToJSString(argv^));
  end else
  begin
    result:= JS_FALSE;
    exit;
  end;

  if tabs.LoadFile(fileName) then
  begin
    tabs.PageControl.ActivePageIndex:= pred(tabs.PageControl.PageCount);
    NProfile.MRU.Add(fileName);
    if assigned(tabs.PageControl.onChange) then
      tabs.PageControl.onChange(nil);
  end;
  result:= JS_TRUE;
end;

function js_tabs_hasEditor(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
begin
  edtab:= ActiveEditorTab;

  if argc = 1 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  rval^:= BoolToJSVal(edTab <> nil);
  result:= JS_TRUE;
end;


function js_tabs_getTabType(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  edtab: TNotesEditorTab;
  S: string;
begin
  edtab:= ActiveEditorTab;

  if argc = 1 then
  begin
    inc(argv);
    if JSValIsInt(argv^) then
      edtab:= tabs.getEditorByTabIndex(JSValToInt(argv^));
  end;

  if edtab <> nil then
  begin
    if edTab.FullPath <> '' then
      S:= 'file'
    else
      S:= 'newfile';
  end else
  begin
    if tabs.PageControl.PageCount > 0 then
      S:= 'StartPage'
    else
      S:= 'none';
  end;

  rval^:= JSStringToJSVal(StringToJSString(cx, S));

  Application.ProcessMessages;
  result:= JS_TRUE;
end;


function js_tabs_getCount(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= IntToJSval(tabs.PageControl.PageCount);
  result:= JS_TRUE;
end;

function js_tabs_newFile(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  tabs.CreateNewEditor;
  tabs.PageControl.ActivePageIndex:= pred( tabs.PageControl.PageCount );
  if ActiveEditorTab <> nil then
  begin
    S:= nProfile.Config.DefaultFileType;
    if argc = 1 then
    begin
      if JSValIsString(argv^) then
        S:= JSStringToString(JSValToJSString(argv^));
    end;
    ActiveEditorTab.FileType:= S;
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;

function js_os_exec(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_execAndWait(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  sName, sParam: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Result:= JS_FALSE;
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

    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := Application.Handle;
      lpFile := PChar(sName);
      lpParameters := PChar(sParam);
      nShow := SW_SHOWNORMAL;
    end;

    if ShellExecuteEx(@SEInfo) then
    begin
      repeat
        Application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or Application.Terminated;

      rval^:= IntToJSVal(ExitCode);
    end else
      rval^:= IntToJSVal(-1);

    Result:= JS_TRUE;
  end;
end;


function js_os_getClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  S: string;
begin
  S:= '';

  if Clipboard.HasFormat(CF_TEXT) then
    S:= clipboard.AsText;

  rval^:= JSStringToJSVal(StringToJSString(cx, S ));
  Result:= JS_TRUE;
end;

function js_os_setClipboard(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_getCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  rval^:= JSStringToJSVal(StringToJSString(cx, GetCurrentDir ));
  Result:= JS_TRUE;
end;

function js_os_setCurDir(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_setEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_getEnv(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_getDirDelimiter(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  // !!!windows specific!!!
  rval^:= JSStringToJSVal(StringToJSString(cx, '\'));
  result:= JS_TRUE;
end;

function js_os_getName(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  // !!!windows specific!!!
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    rval^:= JSStringToJSVal(StringToJSString(cx, 'win'))
  else
    rval^:= JSStringToJSVal(StringToJSString(cx, 'winnt'));

  result:= JS_TRUE;
end;

function js_os_getUser(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  user : string;
  len : Dword;
begin
  // !!!windows specific!!!
  len := 255;
  SetLength(user, len) ;
  if GetUserName(PChar(user), len) then
    user := Copy(user, 1, len - 1)
  else
    user := '';

  rval^:= JSStringToJSVal(StringToJSString(cx, user));
  result:= JS_TRUE;
end;

function js_os_getVersion(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  // !!!windows specific!!!
  rval^:= JSStringToJSVal(StringToJSString(cx, Format('%d.%d.%d', [Win32MajorVersion, Win32MinorVersion, Win32BuildNumber])));
  result:= JS_TRUE;
end;

function js_os_getUptime(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  // !!!windows specific!!!
  rval^:= IntToJSVal(getTickCount);
  result:= JS_TRUE;
end;

function js_os_fileGetContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_filePutContents(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_fileAppend(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  fileName: string;
  SL: TStringList;
  I: integer;
  f: TextFile;
begin
  result:= JS_FALSE;
  if argc = 2 then
  begin
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    fileName := JSStringToString(JSValToJSString(argv^));
    inc(argv);
    if not JSValIsString(argv^) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    if not FileExists(fileName) then
      StrToFile(fileName, '');
    if not FileExists(fileName) then
    begin
      result:= JS_FALSE;
      Exit;
    end;

    SL:= TStringList.Create;
    SL.text:= JSStringToString(JSValToJSString(argv^));

    AssignFile(f, fileName);
    Append(f);
    try
      if SL.count = 0 then Exit;
      for I:= 0 to SL.Count - 1 do
        WriteLn(f, SL.Strings[I]);
    finally
      SL.Free;
      CloseFile(f);
    end;

    Result:= JS_TRUE;
  end;
end;

function js_os_fileCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_fileDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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


function js_os_fileSearch(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

    screen.Cursor:= crHourGlass;
    try
      retVal:= '';
      FileFind(retVal, dirname, mask, recursive);
      // deletamos a primeira quebra de linha
      if (retVal <> '') and (retVal[1] = #10) then
        System.Delete(retVal, 1, 1);

      rval^:= JSStringToJSVal(StringToJSString(cx, retVal));
    finally
      screen.Cursor:= crDefault;
    end;

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_os_fileExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1 ) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));
    rval^:= BoolToJSVal( FileExists(S) );

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_os_fileIsReadOnly(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_fileSize(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_dirExists(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_dirCopy(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  TaskOk, Overw: boolean;
  fPath, tPath, fMask: string;
begin
  // (from, to, mask, overwrite )

  if (argc < 2) or (JSValIsString(argv^) = false) then
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
    if JSValIsString(argv^) then
      fMask := JSStringToString(JSValToJSString(argv^));
  end;

  overw:= true;
  if argc = 4 then
  begin
    inc(argv);
    if JSValIsBoolean(argv^) then
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

function js_os_dirDelTree(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  TaskOk: boolean;
  fPath, fMask: string;
begin
  // (dir, mask)

  if (argc < 1) or ( JSValIsString(argv^) = false ) then
  begin
    Result:= JS_FALSE;
    Exit;
  end;

  fPath:= JSStringToString(JSValToJSString(argv^));

  fMask:= '*';
  if argc = 2 then
  begin
    inc(argv);
    if JSValIsString(argv^) then
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

function js_os_dirDelete(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_dirCreate(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_move(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_os_rename(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
begin
  // renomear e mover são a mesma coisa!
  result:= js_os_move(cx, obj, argc, argv, rval);
end;

function js_dlgs_message(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_dlgs_confirm(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    Application.ProcessMessages;

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

function js_dlgs_question(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
Var
  S: string;
begin
  if (argc = 1) and (JSValIsString(argv^)) then
  begin
    S := JSStringToString(JSValToJSString(argv^));

    Application.ProcessMessages;

    case MessageBox(0, PChar(S), DLGS_TITLE, $24) of
      IDYES: S:='yes';
      IDNO: S:= 'no';
    end;

    rval^:= JSStringToJSVal(StringToJSString(cx, S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;


function js_dlgs_error(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_dlgs_input(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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
      if JSValIsString(argv^) then
        S2 := JSStringToString(JSValToJSString(argv^));
    end;

    S:= InputBox(DLGS_TITLE, S, S2);
    rval^:= JSStringToJSVal(StringToJSString(cx, S));

    Result:= JS_TRUE;
  end else
    Result:= JS_FALSE;
end;

function js_dlgs_fileOpen(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_dlgs_fileSave(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

function js_dlgs_color(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
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

// title
function js_dlgs_folder(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  tit: string;
begin
  if argc = 1 then
  begin
    if JSValIsString(argv^) then
      tit:= JSStringToString(JSValToJSString(argv^));
    if tit <> '' then
      tit:= tit + ' - ' + DLGS_TITLE
    else
      tit:= DLGS_TITLE;

    rval^:= JSStringToJSVal(StringToJSString(cx, BrowseForFolder(0, tit)));
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;

// title, values [, quickfind]
function js_dlgs_list(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  tit, vals: string;
  quickf: boolean;
begin
  if argc > 1 then
  begin
    if JSValIsString(argv^) then
      tit:= JSStringToString(JSValToJSString(argv^));
    if tit <> '' then
      tit:= tit + ' - ' + DLGS_TITLE
    else
      tit:= DLGS_TITLE;
    inc(argv);
    if JSValIsString(argv^) then
      vals:= JSStringToString(JSValToJSString(argv^));

    quickf:= true;
    if argc = 3 then
    begin
      inc(argv);
      if JSValIsBoolean(argv^) then
        quickf:= JSValToBoolean(argv^);
    end;

    with TfrmList.Create(nil) do
    begin
      try
        Values.BeginUpdate;
        Values.Text:= Vals;
        Values.EndUpdate;
        AutoCompleteField:= quickf;
        Caption:= tit;
        if ShowModal = mrOk then
          rval^:= JSStringToJSVal(StringToJSString(cx, Selected))
        else
          rval^:= JSStringToJSVal(StringToJSString(cx, ''));
      finally
        free;
      end;
    end;
    result:= JS_TRUE;
  end else
    result:= JS_FALSE;
end;

function js_dlgs_codeCompletition(cx: PJSContext; obj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
var
  visib, hidden: string;
begin
  result:= JS_FALSE;
  CodeCompletition.Close;
  if ActiveEditorTab = nil then Exit;

  if argc > 0 then
  begin
    if JSValIsString(argv^) then
      visib:= JSStringToString(JSValToJSString(argv^));
    if visib <> '' then
    begin
      hidden:= '';
      if argc = 2 then
      begin
        inc(argv);
        if JSValIsString(argv^) then
         hidden:= JSStringToString(JSValToJSString(argv^));
      end;

      CodeCompletition.VisibleItems.BeginUpdate;
      try
        CodeCompletition.VisibleItems.Text:= visib;
      finally
        CodeCompletition.VisibleItems.EndUpdate;
      end;

      if hidden <> '' then
      begin
        CodeCompletition.HiddenItems.BeginUpdate;
        try
          CodeCompletition.HiddenItems.Text:= Hidden;
        finally
          CodeCompletition.HiddenItems.EndUpdate;
        end;
      end;

      CodeCompletition.Execute(ActiveEditorTab.Editor);

      result:= JS_TRUE;
    end;
  end;
end;

end.
