unit NotesMacros;

interface

uses Windows, SysUtils, Classes, dialogs, jsintf, jsbridge;

type

  TNotesMacros = class(TObject)
  private
    fCode: string;
    fEngine: TJSEngine;
    fPlaying: boolean;
    fRecording: boolean;
    // O buffer vai sendo enchido com os caracteres entrados pelo usuário
    // é salvo com uma string por addBuffer
    fRecordingBuffer: string;
    function getHasMacro: boolean;
    procedure addToRecordingBuffer(key: Char);
    procedure addBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RecordMacro;
    procedure PlayMacro;
    procedure ClearCode;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStr(const S: string);
    procedure SaveToFile(const FileName: string);
    procedure RecordAction(const actionName: string);
    procedure RecordKeystroke(const key: char);
    property  Recording: boolean read fRecording;
    property  Playing: boolean read fPlaying;
    property  HasMacro: boolean read getHasMacro;
  end;


implementation

uses NotesUtils, js15decl, NotesJavaScriptApi;

procedure macro_ErrorReporter(cx: PJSContext; message: PChar; report: PJSErrorReport); cdecl;
var
  msg: String;
begin
  if (report^.flags and JSREPORT_EXCEPTION <> 0) then  // ignore js-catchable exceptions
    exit;

  msg:= 'Notes Macro ';

  if (report^.flags and JSREPORT_WARNING <> 0) then
    msg := msg +'Warning'
  else
    msg := msg +'Error';

  msg := msg + ': '#10#10 + message +#10#10'Line: ' +IntToStr(report^.lineno);

  MessageBox(0 , PChar(msg), 'Notes Script Error!', $1030);
end;

{ TNotesMacros }

procedure TNotesMacros.addBuffer;
begin
  if fRecordingBuffer = '' then Exit;
  fCode:= fCode + ASCII_CRLF + '  editor.insert("'+ fRecordingBuffer +'");';
  fRecordingBuffer:= '';
end;

procedure TNotesMacros.addToRecordingBuffer(key: Char);
var
  S: string;
begin
  // tabs e coisas do tipo possuem ações próprias e não devem ser capturados como caracteres
  if key in [#0..#31] then Exit;

  if key in ['\', '"', ''''] then
    S:= '\' + key
  else
    S:= string(key);

  fRecordingBuffer:= fRecordingBuffer + S;
end;

procedure TNotesMacros.ClearCode;
begin
  fCode:= '';
  fRecordingBuffer:='';
end;

constructor TNotesMacros.Create;
begin
  fEngine:= nil;
  fRecording:= false;
  fPlaying:= false;
end;

destructor TNotesMacros.Destroy;
begin
  //
  inherited;
end;

function TNotesMacros.getHasMacro: boolean;
begin
  Result:= fCode <> '';
end;

procedure TNotesMacros.LoadFromFile(const FileName: string);
begin
  if FileExists(FileName) then
    LoadFromStr(FileToStr(FileName))
  else
    raise Exception.Create(ClassName + '.LoadFromFile - the file does not exists.');
end;

procedure TNotesMacros.LoadFromStr(const S: string);
begin
  ClearCode;
  fCode:= S;
end;

procedure TNotesMacros.PlayMacro;
var
  obj: TJSObject;
  NotesClass, EditorClass, TabsClass, DlgsClass, OsClass: JSClass;
begin
  if fCode='' then Exit;
  if fPlaying then Exit;
  if fRecording then Exit;

  fPlaying:= true;
  // O número passado é a quantidade de bytes
  // que o garbage collector vai aguardar ser carregado
  // antes de rodar. Quanto mais memória, mais rápido
  // o script rodará (e mais memória consumirá!)
  fEngine:= TJSEngine.Create(1000000);
  try
    fEngine.SetErrorReporter(macro_ErrorReporter);

    // Iniciamos a API
    NotesClass:= jcNotesObj;
    EditorClass:= jcEditorObj;
    TabsClass:= jcTabsObj;
    DlgsClass:= jcDlgsObj;
    OsClass:= jcOsObj;

    fengine.Global.AddMethod('alert', js_dlgs_message, 1);
    fengine.Global.AddMethod('prompt', js_dlgs_input, 2);

    // notes
    obj:= fengine.Global.AddObject(NotesClass, 'notes');
    obj.addMethod('getNotesDir', js_notes_getNotesDir, 0);
    obj.addMethod('getMacrosDir', js_notes_getMacrosDir, 0);
    obj.addMethod('getSettingsDir', js_notes_getSettingsDir, 0);
    obj.addMethod('getActiveProfile', js_notes_getActiveProfile, 0);
    obj.addMethod('getDefaultFileType', js_notes_getDefaultFileType, 0);
    obj.addMethod('exec', js_notes_exec, 1);
    obj.addMethod('update', js_notes_update, 0);
    obj.addMethod('panelSetProperty', js_notes_panelSetProperty, 3);
    obj.addMethod('panelGetProperty', js_notes_panelGetProperty, 2);
    obj.addMethod('panelExec', js_notes_panelExec, 3);
    obj.addMethod('status', js_notes_status, 1);

    //os
    obj:= fengine.Global.AddObject(OsClass, 'os');
    obj.addMethod('exec', js_os_exec, 2);
    obj.addMethod('execAndWait', js_os_execAndWait, 2);
    obj.addMethod('getClipboard', js_os_getClipboard, 0);
    obj.addMethod('getCurDir', js_os_getCurDir, 0);
    obj.addMethod('getEnv', js_os_getEnv, 1);
    obj.addMethod('getDirDelimiter', js_os_getDirDelimiter, 0);
    obj.addMethod('getName', js_os_getName, 0);
    obj.addMethod('getUser', js_os_getUser, 0);
    obj.addMethod('getVersion', js_os_getVersion, 0);
    obj.addMethod('getUptime', js_os_getUptime, 0);
    obj.addMethod('setCurDir', js_os_setCurDir, 1);
    obj.addMethod('setClipboard', js_os_setClipboard, 1);
    obj.addMethod('setEnv', js_os_setEnv, 2);
    obj.addMethod('fileGetContents', js_os_fileGetContents, 1);
    obj.addMethod('fileSearch', js_os_fileSearch, 3);
    obj.addMethod('fileIsReadOnly', js_os_fileIsReadOnly, 1);
    obj.addMethod('fileExists', js_os_fileExists, 1);
    obj.addMethod('fileSize', js_os_fileSize, 1);
    obj.addMethod('dirExists', js_os_dirExists, 1);
    obj.addMethod('filePutContents', js_os_filePutContents, 2);
    obj.addMethod('fileAppend', js_os_fileAppend, 2);
    obj.addMethod('fileCopy', js_os_fileCopy, 3);
    obj.addMethod('fileDelete', js_os_fileDelete, 1);
    obj.addMethod('dirCopy', js_os_dirCopy, 4);
    obj.addMethod('dirDelTree', js_os_dirDelTree, 2);
    obj.addMethod('dirDelete', js_os_dirDelete, 1);
    obj.addMethod('dirCreate', js_os_dirCreate, 2);
    obj.addMethod('move', js_os_move, 3);
    obj.addMethod('rename', js_os_rename, 3);

    // dlgs
    obj:= fengine.Global.AddObject(DlgsClass, 'dlgs');
    obj.addMethod('message', js_dlgs_message, 1);
    obj.addMethod('confirm', js_dlgs_confirm, 1);
    obj.addMethod('question', js_dlgs_question, 1);
    obj.addMethod('error', js_dlgs_error, 1);
    obj.addMethod('input', js_dlgs_input, 2);
    obj.addMethod('fileOpen', js_dlgs_fileOpen, 0);
    obj.addMethod('fileSave', js_dlgs_fileSave, 0);
    obj.addMethod('folder', js_dlgs_folder, 1);
    obj.addMethod('color', js_dlgs_color, 0);
    obj.addMethod('list', js_dlgs_list, 3);
    obj.addMethod('codeCompletition', js_dlgs_codeCompletition, 2);

    // editor
    obj:= fengine.Global.AddObject(EditorClass, 'editor');
    obj.addMethod('insert', js_editor_insert, 1);
    obj.addMethod('getText', js_editor_getText, 1);
    obj.addMethod('setText', js_editor_setText, 1);
    obj.addMethod('getSelText', js_editor_getSelText, 1);
    obj.addMethod('setSelText', js_editor_setSelText, 1);
    obj.addMethod('getLineText', js_editor_getLineText, 2);
    obj.addMethod('setLineText', js_editor_setLineText, 3);
    obj.addMethod('getLine', js_editor_getLine, 1);
    obj.addMethod('setLine', js_editor_setLine, 2);
    obj.addMethod('setCol', js_editor_setCol, 2);
    obj.addMethod('getCol', js_editor_getCol, 1);
    obj.addMethod('getSelStart', js_editor_getSelStart, 1);
    obj.addMethod('setSelStart', js_editor_setSelStart, 2);
    obj.addMethod('getSelLength', js_editor_getSelLength, 1);
    obj.addMethod('setSelLength', js_editor_setSelLength, 2);
    obj.addMethod('getLinesCount', js_editor_getLinesCount, 1);
    obj.addMethod('getFileType', js_editor_getfileType, 1);
    obj.addMethod('setFileType', js_editor_setfileType, 2);
    obj.addMethod('getTabSize', js_editor_getTabSize, 1);
    obj.addMethod('getIndentSize', js_editor_getIndentSize, 1);
    obj.addMethod('getModified', js_editor_getModified, 1);
    obj.addMethod('getReadOnly', js_editor_getReadOnly, 1);
    obj.addMethod('inserLineBreak', js_editor_inserLineBreak, 1);
    obj.addMethod('getMark', js_editor_getMark, 2);
    obj.addMethod('getMarksCount', js_editor_getMarksCount, 1);
    obj.addMethod('update', js_editor_update, 0);
    obj.addMethod('setReadOnly', js_editor_setReadOnly, 2);
    obj.addMethod('setTabSize', js_editor_setTabSize, 2);
    obj.addMethod('setIndentSize', js_editor_setIndentSize, 2);
    obj.addMethod('lockUpdates', js_editor_lockUpdates, 0);
    obj.addMethod('setModified', js_editor_setModified, 2);
    obj.addMethod('addMark', js_editor_addMark, 2);
    obj.addMethod('removeMark', js_editor_removeMark, 2);
    obj.addMethod('clearMarks', js_editor_clearMarks, 1);

    // tabs
    obj:= fengine.Global.AddObject(TabsClass, 'tabs');
    obj.addMethod('nextTab', js_tabs_nextTab, 0);
    obj.addMethod('previusTab', js_tabs_previusTab, 0);
    obj.addMethod('gotoFile', js_tabs_gotoFile, 1);
    obj.addMethod('gotoFileAndMarkLine', js_tabs_gotoFileAndMarkLine, 2);
    obj.addMethod('getTabIndex', js_tabs_getTabIndex, 0);
    obj.addMethod('setTabIndex', js_tabs_setTabIndex, 0);
    obj.addMethod('getTabCaption', js_tabs_getTabCaption, 1);
    obj.addMethod('getFileName', js_tabs_getFileName, 1);
    obj.addMethod('openFile', js_tabs_openFile, 1);
    obj.addMethod('getTabType', js_tabs_getTabType, 1);
    obj.addMethod('hasEditor', js_tabs_hasEditor, 1);
    obj.addMethod('getCount', js_tabs_getCount, 1);
    obj.addMethod('newFile', js_tabs_newFile, 1);

    // Rodamos
    FEngine.Evaluate(fCode);
  finally
    FreeAndNil(fEngine);
    fPlaying:= false;
  end;
end;

procedure TNotesMacros.RecordAction(const actionName: string);
begin
  if not fRecording then Exit;
  if (SameText(actionName, 'actMacrosRecord')) or (SameText(actionName, 'actMacrosRun')) or
    (SameText(actionName, 'actMacrosSave')) then Exit;

  addBuffer;
  fCode:= fCode + ASCII_CRLF + '  notes.exec('''+actionName+''');';
end;

procedure TNotesMacros.RecordKeystroke(const key: char);
begin
  if not fRecording then exit;
  addToRecordingBuffer(Key);
end;

procedure TNotesMacros.RecordMacro;
begin
  if fRecording = false then
  begin
    ClearCode;
    fRecording:= true;
  end else
  begin
    addBuffer;
    fRecording:= false;
  end;
end;

procedure TNotesMacros.SaveToFile(const FileName: string);
begin
  if fCode <> '' then
    StrToFile(FileName, fCode);
end;

end.