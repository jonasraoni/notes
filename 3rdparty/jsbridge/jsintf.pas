(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is JavaScript Bridge.
 *
 * The Initial Developer of the Original Code is
 * Sterling Bates.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Theo Lustenberger <theo@theo.ch>
 *   Dominique Louis <Dominique@SavageSoftware.com.au>
 *   Bram Kuijvenhoven, Eljakim IT BV (bkuijvenhoven at eljakim.nl)
 *     - Rewrote implementation for TJSArray.Reverse (May 14, 2004) for
 *       FreePascal compatibility and speed
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** *)

(*
 *  Please send comments for enhancements, fixes, etc, to whoelse@sterlingbates.com
 *
 *  Contents: Delphi-based class interface to Mozilla's javascript engine.
 *
 *   Revision history: (for latest revisions, please check the CVS logs at delphi.mozdev.org)
 *    July 14, 2003 - Initial release
 *    July 15, 2003 - Initializing standard class in TJSEngine.AddClass now; fixes shutdown AV.
 *                  - Numerous parameter fixes, which were also causing AVs and Privileged Instruction errors
 *                  - Fixed StringToJSString to properly pre-allocate room for PJSString
 *                  - GetProperty & SetProperty now work
 *                  - Added DefineMethods and DefineProperties for externally defined objects
 *                  - Finally implemented first working native function call
 *    July 18, 2003 - Faster TJSEngine.Declare(Integer) method
 *                  - Proper implementation of TJSEngine.Declare(Double)
 *   August 5, 2003 - Updated file notes
 *  August 13, 2003 - Implemented TEngine.Serialize and .Deserialize
 *                  - Wrote basic method-by-method documentation for TJSEngine
 *     Oct 22, 2003 - Added compatibility with object-based property accessors via MethodAccessors conditional define (later removed)
 *     Oct 25, 2003 - Removed unnecessary JS_InitStandardClasses call from TJSEngine.AddClass
 *     Oct 30, 2003 - Continued standardizing the class through the SpiderMonkey interface
 *      Nov 4, 2003 - Changed class flag from JSCLASS_NEW_RESOLVE to JSCLASS_HAS_PRIVATE
 *      Nov 7, 2003 - Returning a string from GetProperty requires a call to UniqueString or Delphi prematurely frees the string
 *     Nov 11, 2003 - Added |AddNativeObject| method, which allows any TObject descendent to interact with script.
 *                    All fields and methods required to work with scripting _must_ be published.
 *     Nov 20, 2003 - Eliminated dependency on global |engine| variable
 *                  - Fixed numerous memory leaks
 *     Nov 23, 2003 - Added quick'n'dirty error reporter which can be overridden by the developer by |SetErrorReporter| method
 *     Nov 24, 2003 - Added TJSInteger, TJSDouble, and TJSString; all very lo-tech, but interoperate with JS & Delphi
 *                  - TJSEngine.Declare(val: String) now returns a jsval, not a PJSString (changed, see next comment)
 *                  - Modified all functions to work with TJSBase-derived objects, rather than mysterious jsval's.
 *     Nov 26, 2003 - Added a prototype TJSArray class; extremely likely that it has bugs at this point.
 *                  - There are a few AnsiString leaks that should be sorted out later
 *     Nov 27, 2003 - Added TJSBoolean and TJSObject classes
 *                  - Copied most of TJSObject's methods and functionality from TJSEngine
 *     Nov 28, 2003 - Downsized TJSEngine
 *       Dec 2, 2003 - Added TJSFunction object, with Call method shortcuts
 *      Dec 6, 2003 - Added declaration of JSErrorReport to js15decl.  Will add TJSErrorReporter later as an exception catching object.
 *      Dec 7, 2003 - Added enumeration of javascript objects to TJSObject
 *      Dec 9, 2003 - Removed Serialize: TMemoryStream method from TJSEngine - it wasn't working; other Serialize method still works
 *                  - Modified TJSEngine.Serialize(Script: PJSScript): TMemoryStream to return nil if the serialization failed
 *     Dec 10, 2003 - Fixed up TJSObject.IsFunction and TJSObject.TypeOf to work properly
 *     Dec 13, 2003 - Started some work on GC protection; it'll have to wait.  In the meantime I don't think there's a real problem.
 *                  - Noticed a small issue with SpiderMonkey: apparently properties named "int" don't work well :)
 *                  - Fixed up a couple of crasher bugs
 *                  - Added TJSObject.IsInteger to determine whether a JSTYPE_NUMBER is int or floating point
 *     Dec 14, 2003 - Added TJSScript class
 *                  - Removed Deserialize and Serialize methods from TJSEngine, and moved Compile methods to private
 *                  - Win32 only (for now) calls JS_Shutdown once all TJSEngines are freed; fixes occassional property confusion bug
 *     Dec 16, 2003 - Linux has a JS_Shutdown call for now, but is not at all threadsafe
 *     Dec 17, 2003 - TJSObject.Declare(array) is finally done
 *                  - TJSArray's functionality is now pretty much final; class is not yet finalized, but
 *                    now implements several js-side methods: splice, join, push, pop, and reverse
 *                  - Added TJSObject.ClassType to determine whether a given property is a Date or Array class (added proc to samples)
 *                  - TJSEngine now exposes ArrayClass and DateClass properties -- not for general use!
 *                  - Added TJSBase.ToString method
 *     Dec 18, 2003 - Finished TJSObject.GetFunction
 *     Mar 31, 2004 - Miscellaneous clean up around garbage collection
 *)

unit jsintf;

{$I jsconfig.inc}

interface

uses Classes, ptrarray, namedarray, jsintf_bridge, TypInfo, js15decl,
     SysUtils{$IFNDEF LINUX}, Windows{$ENDIF};

const
  global_class: JSClass = (name: 'global'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
                           delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
                           enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
                           finalize: JS_FinalizeStub);

type
  PWord = ^Word;
  PInteger = ^Integer;

  JSClassType = (ctDate, ctArray, ctString, ctNumber, ctBoolean, ctUnknownClass, ctUnknownType);
  TNativeParam = (npUnknown, npReal, npInteger, npString, npObject, npPointer, npArray);

  PNativeMethod = ^TNativeMethod;
  TNativeMethod = record
    Name: TBridgeString;       // Method name
    Obj: TObject;              // Object containing method
    JS: PJSObject;             // Corresponding JS object
  end;

  PBridgeData = ^TBridgeData;
  TBridgeData = record
    container: Pointer;
    data: Pointer;
  end;

  TJSBase = class;
  TJSInteger = class;
  TJSDouble = class;
  TJSArray = class;
  TJSString = class;
  TJSBoolean = class;
  TJSObject = class;
  TJSFunction = class;

  TStringArray = Array of TBridgeString;
  TJSValArray = Array of jsval;
  TJSBaseArray = Array of TJSBase;

  TJSEngine = class
  private
    farrayclass: PJSClass;
    fbooleanclass: PJSClass;
    fclasses: Array of JSClass;
    fcx: PJSContext;
    fdateclass: PJSClass;
    fglobal: PJSObject;
    fnativeglobal: TJSObject;
    fnatives: TPtrArray;
    fnumberclass: PJSClass;
    fptrs: Array of Pointer;
    frt: PJSRuntime;
    fstackSize: Cardinal;
    fstringclass: PJSClass;
    fvarcount: Integer;
    fvarlist: Array of TJSBase;

    procedure AddPtr(const Ptr: Pointer);
    procedure AddVar(Obj: TJSBase);
    function Compile(const Code: TBridgeString): PJSScript;
    procedure DeleteClasses;
    procedure DeletePtrs;
    procedure DeleteVarList;
    function Execute(Script: PJSScript): Boolean; overload;
    function Execute(Script: PJSScript; Scope: TJSObject): Boolean; overload;
    procedure GetStandardClasses;
    procedure GrowVarList;
    function InternalCall(const Func: PJSFunction; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
    function InternalCallName(const Func: TBridgeString; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
    function InternalGet(const name: TBridgeString; obj: PJSObject; var rval: jsval): Boolean;
    procedure NewMethodClass(const AName: TBridgeString);
    procedure RemoveVar(Obj: TJSBase);
    procedure ScanMethods(AClass: TClass; AParent: PJSObject; AList: TNamedArray; Container: TJSObject);
  public
    property ArrayClass: PJSClass read farrayclass;
    property BooleanClass: PJSClass read fbooleanclass;
    property Context: PJSContext read fcx;
    property DateClass: PJSClass read fdateclass;
    property Global: TJSObject read fnativeglobal;
    property NumberClass: PJSClass read fnumberclass;
    property StringClass: PJSClass read fstringclass;

    constructor Create(MaxMemory: Cardinal);
    destructor Destroy; override;

    function Declare(val: Integer; const name: TBridgeString = ''): TJSInteger; overload;
    function Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray; overload;
    function Declare(val: Double; const name: TBridgeString = ''): TJSDouble; overload;
    function Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString; overload;
    function Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean; overload;
    function Evaluate(const code: TBridgeString; scope: TJSObject): Boolean; overload;
    function Evaluate(const code: TBridgeString): Boolean; overload;
    procedure GarbageCollect;
    function IsExceptionRaised: Boolean;
    function IsValidCode(const code: TBridgeString): Boolean;
    function NewJSObject: TJSObject; overload;
    function NewJSObject(const name: TBridgeString): TJSObject; overload;
    function NewJSObject(const name: TBridgeString; parent: TJSObject): TJSObject; overload;
    procedure SetErrorReporter(proc: JSErrorReporter);
  end;

  (*
   * These were initially set up to be ref-counted, but that may be more effort than its worth.
   * On the other hand, it may open up thread safety for a single TJSBase to be used within
   * multiple threads.  Need to do more reading to see if this is correct or not :)
   *)
  TJSBase = class
  protected
    FConnected: Boolean;
    FDestroying: Boolean;
    FEngine: TJSEngine;
    FJSVal: jsval;
    FName: TBridgeString;
    FRefCnt: Integer;
    FScope: TJSObject;

    procedure AddRef;
    function CanGoLive: Boolean; virtual;
    procedure InternalConnect; virtual;
    procedure DecRef;
    function IsLive: Boolean; virtual;
    procedure SetConnected;
    procedure SetEngine(const Value: TJSEngine);
    procedure SetName(const Value: TBridgeString);
    procedure SetScope(const Value: TJSObject);
  public
    property Connected: Boolean read FConnected;
    property Destroying: Boolean read FDestroying write FDestroying;
    property Engine: TJSEngine read FEngine write SetEngine;
    property JScriptVal: jsval read FJSVal;
    property JSName: TBridgeString read FName write SetName;
    property Parent: TJSObject read FScope write SetScope;

    constructor Create(AEngine: TJSEngine; AName: TBridgeString); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Connect(AEngine: TJSEngine; AName: TBridgeString; AParent: TJSObject); overload;
    procedure Connect(AEngine: TJSEngine; AName: TBridgeString); overload;
    function ToString: TBridgeString;
  end;

  TJSInteger = class(TJSBase)
  protected
    FValue: Integer;

    procedure InternalConnect; override;
    procedure SetValue(const Value: Integer);
    function GetValue: Integer;
  public
    property Value: Integer read GetValue write SetValue;

    constructor Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
  end;

  TJSDouble = class(TJSBase)
  protected
    FValue: Double;
    FJSDouble: pjsdouble;

    procedure InternalConnect; override;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  public
    property Value: Double read GetValue write SetValue;

    constructor Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;
  end;

  (*
   * This class is just ugly.  In order to properly track variable setting and getting in javascript, 
   * I have to mock up a semi-Array-style class.  The current definition below _will not_ be final
   * product, but I'm keeping it here while I get things rolling.
   *)
  TJSArray = class(TJSBase)
  private
    function FindJSVal(val: jsval): TJSBase;
    function GetLength: Integer;
  protected
    FJSObj: PJSObject;
    FValues: Array of TJSBase;

    procedure InternalAdd(Value: TJSBase; Idx: Integer);
    procedure InternalConnect; override;
    procedure ConvertValue(Idx: Integer; val: jsval);
    function GetValue(Idx: Integer): TJSBase;
    procedure SetValue(Idx: Integer; const Value: TJSBase);
  public
    property JSObj: PJSObject read FJSObj;
    property Len: Integer read GetLength;
    property Value[Idx: Integer]: TJSBase read GetValue write SetValue; default;

    function Add(Value: Integer): TJSInteger; overload;
    function Add(const Value: TBridgeString): TJSString; overload;
    function Add(Value: Double): TJSDouble; overload;
    procedure Add(Obj: TJSBase); overload;
    procedure AddJSValAt(Idx: Integer; val: jsval);
    function GetJSValAt(Idx: Integer): jsval;
    function Join: TBridgeString;
    procedure RemoveItemAt(Idx: Integer);
    procedure Reverse;
    procedure SetSize(Val: Integer);
  end;

  (*
   * To be completed when TJSArray is finalized.
   *)
  TJSNamedArray = class(TJSBase)
  protected
    FValues: Array of TJSBase;

    procedure InternalConnect; override;
    function GetValue(Key: TBridgeString): TJSBase;
    procedure SetValue(Key: TBridgeString; const Value: TJSBase);
  public
    property Value[Key: TBridgeString]: TJSBase read GetValue write SetValue; default;
  end;

  TJSString = class(TJSBase)
  protected
    FValue: TBridgeString;
    FJSString: PJSString;

    procedure InternalConnect; override;
    function GetValue: TBridgeString;
    procedure SetValue(const Value: TBridgeString);
  public
    property Value: TBridgeString read GetValue write SetValue;

    constructor Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;
  end;

  TJSBoolean = class(TJSBase)
  protected
    FValue: Boolean;

    procedure InternalConnect; override;
    function GetValue: Boolean;
    procedure SetValue(Value: Boolean);
  public
    property Value: Boolean read GetValue write SetValue;

    constructor Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
  end;

  TJSObject = class(TJSBase)
  protected
    FJSObj: PJSObject;
    FNatives: TPtrArray;

    procedure CheckConnection;
    procedure InternalConnect; override;
    procedure Init;
  public
    property JSObject: PJSObject read FJSObj write FJSObj;

    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;

    function AddMethod(const name: TBridgeString; proc: JSNative; paramcount: Integer): TJSFunction;
    function AddMethods(var methods: TJSFunctionSpecArray): Boolean;
    function AddNativeObject(Obj: TObject; const InstanceName: TBridgeString): TJSObject;
    function AddObject(var cls: JSClass; const AName: TBridgeString): TJSObject;
    function AddProperties(var props: TJSPropertySpecArray): Boolean;
    function Call(const Func: TBridgeString; params: Array of TJSBase; var str: TBridgeString): Boolean; overload;
    function Call(const Func: TBridgeString; params: Array of TJSBase; var int: Integer): Boolean; overload;
    function Call(const Func: TBridgeString; params: Array of TJSBase; var dbl: Double): Boolean; overload;
    function Call(const Func: TBridgeString; params: Array of TJSBase; var res: TJSObject): Boolean; overload;
    function Call(const Func: TBridgeString; params: Array of TJSBase; var bool: Boolean): Boolean; overload;
    function ClassType(const Name: TBridgeString): JSClassType;
    function Declare(val: Double; const name: TBridgeString = ''): TJSDouble; overload;
    function Declare(val: Integer; const name: TBridgeString = ''): TJSInteger; overload;
    function Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString; overload;
    function Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray; overload;
    function Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean; overload;
    function DeclareObject(const name: TBridgeString): TJSObject;
    function Enumerate: TStringArray;
    function Evaluate(const code: TBridgeString): Boolean;
    function GetFunction(const name: TBridgeString): TJSFunction;
    function GetNativeProperty(Obj: TObject; AName: TBridgeString): jsval;
    function GetProperty(const name: TBridgeString; var dbl: Double): Boolean; overload;
    function GetProperty(const name: TBridgeString; var int: Integer): Boolean; overload;
    function GetProperty(const name: TBridgeString; var ret: TJSObject): Boolean; overload;
    function GetProperty(const name: TBridgeString; var str: TBridgeString): Boolean; overload;
    function GetProperty(const name: TBridgeString; var bool: Boolean): Boolean; overload;
    function HasProperty(const name: TBridgeString): Boolean;
    function IsFunction(const name: TBridgeString): Boolean;
    function IsInteger(const name: TBridgeString): Boolean;
    procedure RemoveObject(Obj: TJSBase);
    function SetNativeProperty(Obj: TObject; AName: TBridgeString; AValue: Variant): JSBool;
    function SetProperty(const name: TBridgeString; val: TJSBase): Boolean;
    function TypeOf(const name: TBridgeString): JSType;
  end;

  TJSFunction = class(TJSBase)
  protected
    FArgCount: Integer;
    FCall: JSNative;
    FJSFun: PJSFunction;

    procedure InternalConnect; override;
  public
    property JSFunction: PJSFunction read FJSFun;

    constructor Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString); overload;
    constructor Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject); overload;
    destructor Destroy; override;

    function Call(params: Array of TJSBase; var str: TBridgeString): Boolean; overload;
    function Call(params: Array of TJSBase; var int: Integer): Boolean; overload;
    function Call(params: Array of TJSBase; var dbl: Double): Boolean; overload;
    function Call(params: Array of TJSBase; var res: TJSObject): Boolean; overload;
    function Call(params: Array of TJSBase; var bool: Boolean): Boolean; overload;

    function Call(var str: TBridgeString): Boolean; overload;
    function Call(var int: Integer): Boolean; overload;
    function Call(var dbl: Double): Boolean; overload;
    function Call(var res: TJSObject): Boolean; overload;
    function Call(var bool: Boolean): Boolean; overload;
  end;

  TJSScript = class
  private
    FCode: TBridgeString;
    FCompiled: Boolean;
    FScript: PJSScript;
  public
    property Code: TBridgeString read FCode write FCode;
    property Compiled: Boolean read FCompiled;

    constructor Create; overload;
    constructor Create(const ACode: TBridgeString); overload;
    constructor Create(const ACode: TBridgeString; AEngine: TJSEngine); overload;

    procedure Compile(AEngine: TJSEngine);
    procedure Execute(AEngine: TJSEngine); overload;
    procedure Execute(AEngine: TJSEngine; AScope: TJSObject); overload;
    procedure LoadCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
    procedure LoadCompiledFromStream(AStream: TStream; AEngine: TJSEngine);
    procedure LoadRaw(const AFile: TBridgeString);
    procedure SaveCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
    procedure SaveCompiledToStream(AStream: TStream; AEngine: TJSEngine);
    procedure SaveRaw(const AFile: TBridgeString);
  end;

var
  __JSEngines: Integer;

implementation

{ TJSEngine }

procedure TJSEngine.AddPtr(const Ptr: Pointer);
begin
  SetLength(fptrs, Length(fptrs) + 1);
  fptrs[Length(fptrs) - 1] := Ptr;
end;

procedure TJSEngine.AddVar(Obj: TJSBase);
begin
  if (fvarcount >= Length(fvarlist)) then
    GrowVarList;
  fvarlist[fvarcount] := Obj;
  Inc(fvarcount);
end;

function TJSEngine.Compile(const Code: TBridgeString): PJSScript;
begin
  {$IFNDEF JSUnicode}
  Result := JS_CompileScript(fcx, fglobal, PBridgeChar(Code), Length(Code), '', 0);
  {$ELSE}
  Result := JS_CompileUCScript(fcx, fglobal, PBridgeChar(Code), Length(Code), '', 0);
  {$ENDIF}
end;

constructor TJSEngine.Create(MaxMemory: Cardinal);
begin
  {$IFDEF LINUX}Inc(__JSEngines);{$ELSE}InterlockedIncrement(__JSEngines);{$ENDIF}
  fstackSize := 8192;

  frt := JS_Init(MaxMemory);
  fcx := JS_NewContext(frt, fstackSize);

  fglobal := JS_NewObject(fcx, @global_class, nil, nil);
  JS_InitStandardClasses(fcx, fglobal);
  JS_SetErrorReporter(fcx, IntfBridge_ErrorReporter);

  fnatives := TPtrArray.Create;
  fnatives.OwnsValues := true;

  fnativeglobal := TJSObject.Create(fglobal, self, '');

  GetStandardClasses;
end;

function TJSEngine.Declare(val: Integer; const name: TBridgeString = ''): TJSInteger;
begin
  Result := TJSInteger.Create(val, self, name, fnativeglobal);
  AddVar(Result);
end;

function TJSEngine.Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray;
begin
  Result := TJSArray.Create(self, name);
  AddVar(Result);
end;

function TJSEngine.Declare(val: Double; const name: TBridgeString = ''): TJSDouble;
begin
  Result := TJSDouble.Create(val, self, name, fnativeglobal);
  AddVar(Result);
end;

function TJSEngine.Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString;
begin
  Result := TJSString.Create(val, self, name, fnativeglobal);
  AddVar(Result);
end;

function TJSEngine.Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean;
begin
  Result := TJSBoolean.Create(val, self, name, fnativeglobal);
  AddVar(Result);
end;

procedure TJSEngine.DeleteClasses;
begin
  SetLength(fclasses, 0);
end;

procedure TJSEngine.DeletePtrs;
var
  i: Integer;
begin
  for i := 0 to Length(fptrs) - 1 do
    Dispose(fptrs[i]);
  SetLength(fptrs, 0);
end;

procedure TJSEngine.DeleteVarList;
var
  i: Integer;
begin
  for i := 0 to Length(fvarlist) - 1 do
    if (fvarlist[i] <> nil) then
    begin
      fvarlist[i].Destroying := true;
      fvarlist[i].Free;
    end;
  SetLength(fvarlist, 0);
end;

destructor TJSEngine.Destroy;
begin
  JS_DestroyContext(fcx);
  JS_Finish(frt);

  fnatives.Free;
  fnativeglobal.Free;

  DeletePtrs;
  DeleteClasses;
  DeleteVarList;

{$IFDEF LINUX}
  Dec(__JSEngines);
  if (__JSEngines = 0) then
    JS_Shutdown;
{$ELSE}
  if (InterlockedDecrement(__JSEngines) = 0) then
    JS_Shutdown;
{$ENDIF}

  inherited;
end;

function TJSEngine.Evaluate(const code: TBridgeString; scope: TJSObject): Boolean;
begin
  Result := scope.Evaluate(code);
end;

function TJSEngine.Evaluate(const code: TBridgeString): Boolean;
begin
  Result := fnativeglobal.Evaluate(code);
end;

function TJSEngine.Execute(Script: PJSScript): Boolean;
begin
  Result := Execute(Script, fnativeglobal);
end;

function TJSEngine.Execute(Script: PJSScript; Scope: TJSObject): Boolean;
var
  rval: jsval;
begin
  if (Scope = nil) then
    Scope := fglobal;
  Result := (JS_ExecuteScript(fcx, Scope.JSObject, Script, @rval) = JS_TRUE);
end;

procedure TJSEngine.GarbageCollect;
begin
  JS_GC(fcx);
end;

procedure TJSEngine.GetStandardClasses;
var
  obj: PJSObject;

  function Eval(const str: TBridgeString): jsval;
  begin
    {$IFNDEF JSUnicode}
    JS_EvaluateScript(fcx, fglobal, PBridgeChar(str), Length(str), nil, 0, @Result);
    {$ELSE}
    JS_EvaluateUCScript(fcx, fglobal, PBridgeChar(str), Length(str), nil, 0, @Result); 
    {$ENDIF}
  end;

begin
  obj := JSValToObject(Eval('Date.prototype'));
  fdateclass := JS_GetClass(obj);

  obj := JSValToObject(Eval('Array.prototype'));
  farrayclass := JS_GetClass(obj);

  obj := JSValToObject(Eval('Boolean.prototype'));
  fbooleanclass := JS_GetClass(obj);

  obj := JSValToObject(Eval('String.prototype'));
  fstringclass := JS_GetClass(obj);

  obj := JSValToObject(Eval('Number.prototype'));
  fnumberclass := JS_GetClass(obj);
end;

procedure TJSEngine.GrowVarList;
begin
  SetLength(fvarlist, Length(fvarlist) + 16);
end;

function TJSEngine.InternalCall(const Func: PJSFunction; obj: PJSObject; var args: Array of TJSBase; rval: pjsval): Boolean;
var
  myargs: TJSValArray;
  i: Integer;
begin
  if (obj = nil) then
    obj := fglobal;

  if (Length(args) = 0) then
    Result := (JS_CallFunction(fcx, obj, Func, 0, nil, rval) = JS_TRUE)
  else
  begin
    SetLength(myargs, Length(args));
    for i := 0 to Length(args) - 1 do
      myargs[i] := args[i].JScriptVal;

    Result := (JS_CallFunction(fcx, obj, Func, Length(myargs), @myargs[0], rval) = JS_TRUE);
    SetLength(myargs, 0);
  end;
end;

function TJSEngine.InternalCallName(const Func: TBridgeString; obj: PJSObject; var args: array of TJSBase; rval: pjsval): Boolean;
var
  fval: jsval;
begin
  {$IFNDEF JSUnicode}
  JS_GetProperty(fcx, obj, PBridgeChar(Func), @fval);
  {$ELSE}
  JS_GetUCProperty(fcx, obj, PBridgeChar(Func), Length(Func), @fval);
  {$ENDIF}

  Result := InternalCall(JS_ValueToFunction(fcx, fval), obj, args, rval); 
end;

function TJSEngine.InternalGet(const name: TBridgeString; obj: PJSObject; var rval: jsval): Boolean;
begin
  if (obj = nil) then
    obj := fglobal;
  {$IFNDEF JSUnicode}
  Result := (JS_GetProperty(fcx, obj, PBridgeChar(name), @rval) = JS_TRUE);
  {$ELSE}
  Result := (JS_GetUCProperty(fcx, obj, PBridgeChar(name), Length(name), @rval) = JS_TRUE);
  {$ENDIF}
end;

function TJSEngine.IsExceptionRaised: Boolean;
begin
  Result := (JS_IsExceptionPending(fcx) = JS_TRUE);
end;

function TJSEngine.IsValidCode(const code: TBridgeString): Boolean;
begin
  {$IFNDEF JSUnicode}
  Result := (JS_BufferIsCompilableUnit(fcx, fglobal, PBridgeChar(code), Length(code)) = JS_TRUE);
  {$ELSE}
  Result := (JS_UCBufferIsCompilableUnit(fcx, fglobal, PBridgeChar(code), Length(code)) = JS_TRUE);
  {$ENDIF}
end;

function TJSEngine.NewJSObject: TJSObject;
begin
  Result := TJSObject.Create(nil, self, '');
end;

function TJSEngine.NewJSObject(const name: TBridgeString): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name);
end;

function TJSEngine.NewJSObject(const name: TBridgeString; parent: TJSObject): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name, parent);
end;

procedure TJSEngine.NewMethodClass(const AName: TBridgeString);
var
  len: Integer;
begin
  len := Length(fclasses);
  SetLength(fclasses, len+1);

  with fclasses[len] do
  begin
    name := CreateAnsiString(AName);
    flags := JSCLASS_HAS_PRIVATE;
    addProperty := JS_PropertyStub;
    delProperty := JS_PropertyStub;
    getProperty := JS_PropertyStub;
    setProperty := JS_PropertyStub;
    enumerate := JS_EnumerateStub;
    resolve := JS_ResolveStub;
    convert := JS_ConvertStub;
    finalize := JS_FinalizeStub;
    call := IntfBridge_MethodCall;
  end;
end;

procedure TJSEngine.RemoveVar(Obj: TJSBase);
var
  i: Integer;
begin
  for i := 0 to Length(fvarlist) - 1 do
    if (fvarlist[i] = Obj) then
    begin
      fvarlist[i] := nil;
      exit;
    end;
end;

procedure TJSEngine.ScanMethods(AClass: TClass; AParent: PJSObject; AList: TNamedArray; Container: TJSObject);
type
  TMethodtableEntry = packed record
    len: Word;
    addr: Pointer;
    name: ShortString;
  end;
var
  pp: ^Pointer;
  pMethodTable: Pointer;
  pMethodEntry: ^TMethodTableEntry;
  i, numEntries: Word;
  str: String;
  jsobj: PJSObject;
  data: PBridgeData;
begin
  if (AClass = nil) then exit;

  pp := Pointer(Integer(AClass) + vmtMethodtable);
  pMethodTable := pp^;

  if (pMethodtable <> nil) then
  begin
    numEntries := PWord(pMethodTable)^;
    pMethodEntry := Pointer(Integer(pMethodTable) + 2);
    for i := 1 to numEntries do
    begin
      str := pMethodEntry^.name;
      NewMethodClass(str);

      {$IFNDEF JSUnicode}
      jsobj := JS_DefineObject(fcx, AParent, CreateAnsiString(str), @intf_bridge_method_class, nil, JSPROP_ENUMERATE);
      {$ELSE}
      jsobj := JS_NewObject(fcx, @intf_bridge_method_class, nil, AParent);
      JS_DefineUCProperty(fcx, AParent, CreateWideString(str), Length(str), JSObjectToJSVal(jsobj), nil, nil, JSPROP_ENUMERATE);
      {$ENDIF}

      New(data);
      data^.container := Container;
      data^.data := pMethodEntry^.addr;
      JS_SetPrivate(fcx, jsobj, data);
      AddPtr(data);

      AList[str] := jsobj;

      pMethodEntry := Pointer(Integer(pMethodEntry) + pMethodEntry^.len);
    end;
  end;
  ScanMethods(AClass.ClassParent, AParent, AList, Container);
end;

procedure TJSEngine.SetErrorReporter(proc: JSErrorReporter);
begin
  JS_SetErrorReporter(fcx, proc);
end;

{ TJSBase }

procedure TJSBase.AddRef;
begin
  Inc(FRefCnt);
end;

function TJSBase.CanGoLive: Boolean;
begin
  Result := (FName <> '') and (FScope <> nil) and (FScope.Connected);
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: TBridgeString; AParent: TJSObject);
begin
  Engine := AEngine;
  Parent := AParent;
  JSName := AName;
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: TBridgeString);
begin
  Engine := AEngine;
  Parent := AEngine.Global;
  JSName := AName;
end;

constructor TJSBase.Create(AEngine: TJSEngine; AName: TBridgeString);
begin
  Engine := AEngine;
  JSName := AName;
  Parent := FEngine.Global;
end;

constructor TJSBase.Create;
begin
  FEngine := nil;
  FScope := nil;
end;

procedure TJSBase.DecRef;
begin
  Dec(FRefCnt);
  if (FRefCnt = 0) then
    Free;
end;

destructor TJSBase.Destroy;
{$IFDEF JSUnicode}
var
  rval: jsval;
{$ENDIF}
begin
  if (FEngine <> nil) and (not FDestroying) then
  try
    FEngine.RemoveVar(self);  // only for ref-counting, and we'll probably move to something else soon
    if (FName <> '') and (FScope <> nil) then
      {$IFNDEF JSUnicode}
      JS_DeleteProperty(FEngine.Context, FScope.JSObject, PBridgeChar(FName));
      {$ELSE}
      JS_DeleteUCProperty2(FEngine.Context, FScope.JSObject, PBridgeChar(FName), Length(FName), @rval);
      {$ENDIF}
  except
  end;
end;

procedure TJSBase.InternalConnect;
begin
end;

function TJSBase.IsLive: Boolean;
begin
  (*
   * This may not be the fastest way to determine whether the property already exists in FScope.
   *)
  //Result := (FScope <> nil) and FScope.HasProperty(FName);
  Result := false;
end;

procedure TJSBase.SetConnected;
begin
  FConnected := (FEngine <> nil);
  if (FConnected) then
    InternalConnect;
end;

procedure TJSBase.SetEngine(const Value: TJSEngine);
begin
  FEngine := Value;
  SetConnected;
end;

procedure TJSBase.SetName(const Value: TBridgeString);
begin
  FName := Value;
  SetConnected;
end;

procedure TJSBase.SetScope(const Value: TJSObject);
begin
  if (FEngine <> nil) and (Value = nil) then
    FScope := FEngine.Global
  else
    FScope := Value;
  SetConnected;
end;

function TJSBase.ToString: TBridgeString;
begin
  Result := GetParamName(FEngine.Context, FJSVal);
end;

{ TJSInteger }

procedure TJSInteger.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

constructor TJSInteger.Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSInteger.Create(AValue: Integer; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

function TJSInteger.GetValue: Integer;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSInteger.SetValue(const Value: Integer);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := IntToJSVal(FValue);
    if (not IsLive) and (CanGoLive) then
      FScope.SetProperty(FName, self);
  end;
end;

{ TJSDouble }

procedure TJSDouble.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

constructor TJSDouble.Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSDouble.Create(AValue: Double; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSDouble.Destroy;
begin
  inherited;
end;

function TJSDouble.GetValue: Double;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSDouble.SetValue(const Value: Double);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := DoubleToJSVal(FEngine.Context, FValue);
    if (not IsLive) and (CanGoLive) then
    begin
      FScope.SetProperty(FName, self);
    end;
  end;
end;

{ TJSArray }

function TJSArray.Add(Value: Integer): TJSInteger;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

function TJSArray.Add(const Value: TBridgeString): TJSString;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

function TJSArray.Add(Value: Double): TJSDouble;
begin
  Result := FEngine.Declare(Value);
  InternalAdd(Result, Length(FValues));
end;

procedure TJSArray.Add(Obj: TJSBase);
begin
  InternalAdd(Obj, Length(FValues));
end;

procedure TJSArray.AddJSValAt(Idx: Integer; val: jsval);
var
  isnew: Boolean;
  obj: TJSObject;
begin
  isnew := (Idx >= 0) and (Idx >= Length(FValues));

  if (isnew) then
    case JS_TypeOfValue(FEngine.Context, val) of
      JSTYPE_STRING: InternalAdd(FEngine.Declare(JSStringToString(JSValToJSString(val))), Idx);
      JSTYPE_NUMBER:
        if (JSValIsInt(val)) then
          InternalAdd(FEngine.Declare(JSValToInt(val)), Idx)
        else
          InternalAdd(FEngine.Declare(JSValToDouble(FEngine.Context, val)), Idx);
      JSTYPE_OBJECT:
        begin
          obj := FEngine.NewJSObject;
          obj.JSObject := JSValToObject(val);
          InternalAdd(obj, Idx);
        end;
      JSTYPE_BOOLEAN:
        InternalAdd(FEngine.Declare(JSValToBoolean(val)), Idx);
    end
  else
    ConvertValue(Idx, val);
end;

procedure TJSArray.ConvertValue(Idx: Integer; val: jsval);
var
  b: TJSBase;
  obj: TJSObject;
begin
  b := FValues[Idx];

  case JS_TypeOfValue(FEngine.Context, val) of
    JSTYPE_NUMBER:
      if (JSValIsInt(val)) then
      begin
        if (b is TJSInteger) then
          TJSInteger(b).Value := JSValToInt(val)
        else
          InternalAdd(FEngine.Declare(JSValToInt(val)), Idx);
      end
      else if (JSValIsDouble(val)) then
      begin
        if (b is TJSDouble) then
          TJSDouble(b).Value := JSValToDouble(FEngine.Context, val)
        else
          InternalAdd(FEngine.Declare(JSValToDouble(FEngine.Context, val)), Idx);
      end;
    JSTYPE_STRING:
      if (b is TJSString) then
        TJSString(b).Value := JSStringToString(JSValToJSString(val))
      else
        InternalAdd(FEngine.Declare(JSStringToString(JSValToJSString(val))), Idx);
    JSTYPE_BOOLEAN:
      if (b is TJSBoolean) then
        TJSBoolean(b).Value := JSValToBoolean(val)
      else
        InternalAdd(FEngine.Declare(JSValToBoolean(val)), Idx);
    JSTYPE_OBJECT:
      if (b is TJSObject) then
        TJSObject(b).JSObject := JSValToObject(val)
      else
      begin
        obj := FEngine.NewJSObject;
        obj.JSObject := JSValToObject(val);
        InternalAdd(obj, Idx);
      end;
  end;
end;

function TJSArray.FindJSVal(val: jsval): TJSBase;
var
  i: Integer;
begin
  for i := 0 to Length(FValues)-1 do
  begin
    if (val = FValues[i].JScriptVal) then
    begin
      Result := FValues[i];
      exit;
    end;
  end;
  Result := nil;
end;

function TJSArray.GetJSValAt(Idx: Integer): jsval;
begin
  if (Idx >= 0) and (Idx < Length(FValues)) then
  begin
    if (FValues[Idx] <> nil) then
      Result := FValues[Idx].JScriptVal
    else
      Result := JSVAL_NULL;
  end
  else
    Result := JSVAL_VOID;
end;

function TJSArray.GetLength: Integer;
begin
  Result := Length(FValues);
end;

(* Deprecated *)
function TJSArray.GetValue(Idx: Integer): TJSBase;
var
  rval: jsval;
begin
  if (FConnected) and (FName <> '') then
  begin
    JS_GetElement(FEngine.Context, FJSObj, Idx, @rval);
    Result := FindJSVal(rval);
  end
  else
    Result := FValues[Idx];
end;

procedure TJSArray.InternalAdd(Value: TJSBase; Idx: Integer);
begin
  if (Idx >= 0) and (Idx < Length(FValues)) then
    FValues[Idx] := Value
  else
  begin
    SetLength(FValues, Idx+1);
    FValues[Idx] := Value;
  end;
end;

procedure TJSArray.InternalConnect;
var
  data: PBridgeData;
begin
  FJSObj := JS_NewObject(FEngine.Context, @base_bridge_array, nil, nil);
  FJSVal := jsval(FJSObj) or JSVAL_OBJECT;

  New(data);
  data^.container := FEngine;
  data^.data := self;
  FEngine.AddPtr(data);
  JS_SetPrivate(FEngine.Context, FJSObj, data);

  JS_DefineFunctions(FEngine.Context, FJSObj, @base_bridge_array_methods);
  {$IFNDEF JSUnicode}
  JS_DefineProperty(FEngine.Context, FJSObj, 'length', IntToJSVal(Len), ArrayBridge_GetLength, ArrayBridge_SetLength, JSPROP_PERMANENT);
  {$ELSE}
  JS_DefineUCProperty(FEngine.Context, FJSObj, 'length', Length('length'), IntToJSVal(Len), ArrayBridge_GetLength, ArrayBridge_SetLength, JSPROP_PERMANENT);
  {$ENDIF}

  if (not IsLive) and (CanGoLive) then
    FScope.SetProperty(FName, self);
end;

function TJSArray.Join: TBridgeString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(FValues)-1 do
  begin
    if (FValues[i] <> nil) then
      Result := Result +FValues[i].ToString
    else
      Result := Result +'null';
    if (i < Length(FValues)-1) then
      Result := Result +', ';
  end;
end;

procedure TJSArray.RemoveItemAt(Idx: Integer);
var
  i: Integer;
begin
  // This for loop can be shrunk to a move command which would speed things up
  for i := Idx+1 to Length(FValues)-1 do
    FValues[i-1] := FValues[i];
  SetLength(FValues, Length(FValues)-1);
end;

procedure TJSArray.Reverse;
var
  tmp: TJSBase;
  i: Integer;
  j: Integer;
  max: Integer;
  len: Integer;
begin
  len := Length(FValues);
  i := 0;
  j := len-1;
  max := len div 2;

  while i < max do
  begin
    tmp := FValues[i];
    FValues[i] := FValues[j];
    FValues[j] := tmp;
    Inc(i);
    Dec(j);
  end;
end;

procedure TJSArray.SetSize(Val: Integer);
begin
  SetLength(FValues, Val);
end;

procedure TJSArray.SetValue(Idx: Integer; const Value: TJSBase);
begin
  if (Idx >= Length(FValues)) then
    SetLength(FValues, Idx+1);
  FValues[Idx] := Value;
end;

{ TJSNamedArray }

function TJSNamedArray.GetValue(Key: TBridgeString): TJSBase;
begin
  Result := nil;
end;

procedure TJSNamedArray.InternalConnect;
begin
 //
end;

procedure TJSNamedArray.SetValue(Key: TBridgeString; const Value: TJSBase);
begin

end;

{ TJSString }

constructor TJSString.Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSString.Create(AValue: TBridgeString; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSString.Destroy;
begin
  inherited;
end;

function TJSString.GetValue: TBridgeString;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSString.InternalConnect;
begin
  inherited;
  SetValue(FValue);
end;

procedure TJSString.SetValue(const Value: TBridgeString);
begin
  FValue := Value;
  if (FConnected) then
  begin
    {$IFNDEF JSUnicode}
    FJSString := JS_NewStringCopyN(FEngine.Context, PBridgeChar(Value), Length(Value));
    {$ELSE}
    FJSString := JS_NewUCStringCopyN(FEngine.Context, PBridgeChar(Value), Length(Value));
    {$ENDIF}
    FJSVal := jsval(FJSString) or JSVAL_STRING;

    if (not IsLive) and (CanGoLive) then
    begin
      JS_RemoveRoot(FEngine.Context, @FJSVal);
      FScope.SetProperty(FName, self);
    end
    else
      JS_AddRoot(FEngine.Context, @FJSVal);
  end;
end;

{ TJSBoolean }

procedure TJSBoolean.InternalConnect;
begin
  SetValue(FValue);
end;

constructor TJSBoolean.Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSBoolean.Create(AValue: Boolean; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FValue := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

function TJSBoolean.GetValue: Boolean;
begin
  if (FConnected) and (FName <> '') then
    FScope.GetProperty(FName, Result)
  else
    Result := FValue;
end;

procedure TJSBoolean.SetValue(Value: Boolean);
begin
  FValue := Value;
  if (FConnected) then
  begin
    FJSVal := BoolToJSVal(FValue);
    if (not IsLive) and (CanGoLive) then
      FScope.SetProperty(FName, self);
  end;
end;

{ TJSObject }

function TJSObject.AddMethod(const name: TBridgeString; proc: JSNative; paramcount: Integer): TJSFunction;
var
  p: PJSFunction;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  p := JS_DefineFunction(FEngine.Context, FJSObj, CreateAnsiString(name), proc, paramcount, JSPROP_ENUMERATE);
  {$ELSE}
  p := JS_DefineUCFunction(FEngine.Context, FJSObj, CreateWideString(name), Length(name), proc, paramcount, JSPROP_ENUMERATE);
  {$ENDIF}
  if (p <> nil) then
    Result := TJSFunction.Create(p, FEngine, name, self)
  else
    Result := nil;
end;

function TJSObject.AddMethods(var methods: TJSFunctionSpecArray): Boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |methods| must be blank *)
  len := Length(methods);
  SetLength(methods, len + 1);
  FillChar(methods[len], SizeOf(JSFunctionSpec), #0);

  Result := (JS_DefineFunctions(FEngine.Context, FJSObj, @methods[0]) = JS_TRUE);
end;

function TJSObject.AddNativeObject(Obj: TObject; const InstanceName: TBridgeString): TJSObject;
var
  tinfo: PTypeInfo;
  ptypes: TTypeKinds;
  plist: PPropList;
  i: Integer;
  count: Integer;
  LName: String;
  methods: TNamedArray;
  data: PBridgeData;
  js: PJSObject;
  cx: PJSContext;
  cls: TClass;
begin
  CheckConnection;
  Result := nil;

  if (fnatives[Obj] = nil) then
  begin
    cx := FEngine.Context;
    js := JS_NewObject(cx, @intf_bridge_class, nil, FJSObj);
    Result := TJSObject.Create(js, FEngine, InstanceName, self);

    New(data);
    FEngine.AddPtr(data);

    data^.container := self;
    data^.data := Obj;
    JS_SetPrivate(cx, js, data);

    cls := Obj.ClassType;
    tinfo := PTypeInfo(cls.ClassInfo);

    count := GetPropList(tinfo, plist);
    ptypes := [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkWChar, tkLString, tkWString, tkInt64];
    for i := 0 to count-1 do
      if (plist^[i]^.PropType^.Kind in ptypes) then
      begin
        LName := String(plist[i]^.Name);
        {$IFNDEF JSUnicode}
        JS_DefineProperty(cx, js, CreateAnsiString(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
        {$ELSE}
        JS_DefineUCProperty(cx, js, PWideChar(CreateAnsiString(LName)), Length(LName), JSVAL_NULL, nil, nil, JSPROP_ENUMERATE or JSPROP_PERMANENT);
        {$ENDIF}
      end;
    FreeMem(plist);

    methods := TNamedArray.Create;
    methods.OwnsValues := false;
    FEngine.ScanMethods(Obj.ClassType, js, methods, self);
    FNatives[Obj] := methods;

    FEngine.AddVar(Result);
  end;
end;

function TJSObject.AddObject(var cls: JSClass; const AName: TBridgeString): TJSObject;
var
  jsobj: PJSObject;
begin
  CheckConnection;
  jsobj := JS_DefineObject(FEngine.Context, FJSObj, CreateAnsiString(AName), @cls, nil, JSPROP_ENUMERATE);
  Result := TJSObject.Create(jsobj, FEngine, AName, self);
end;

function TJSObject.AddProperties(var props: TJSPropertySpecArray): Boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |props| must be blank *)
  len := Length(props);
  SetLength(props, len + 1);
  FillChar(props[len], SizeOf(JSPropertySpec), #0);

  Result := (JS_DefineProperties(FEngine.Context, FJSObj, @props[0]) = JS_TRUE);
end;

function TJSObject.Call(const Func: TBridgeString; params: Array of TJSBase; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, FJSObj, params, @rval)) then
  begin
    str := '';
    exit;
  end;

  str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
  UniqueString(str);
end;

function TJSObject.Call(const Func: TBridgeString; params: Array of TJSBase; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, FJSObj, params, @rval)) then
  begin
    dbl := JSVAL_NULL;
    exit;
  end;

  if (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSObject.Call(const Func: TBridgeString; params: Array of TJSBase; var int: Integer): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, FJSObj, params, @rval)) then
  begin
    int := JSVAL_NULL;
    exit;
  end;

  int := JSValToInt(rval);
end;

function TJSObject.Call(const Func: TBridgeString; params: Array of TJSBase; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, FJSObj, params, @rval)) then
  begin
    bool := false;
    exit;
  end;

  bool := JSValToBoolean(rval);
end;

function TJSObject.Call(const Func: TBridgeString; params: Array of TJSBase; var res: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
  t: Pointer;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, FJSObj, params, @rval)) then
  begin
    res := nil;
    exit;
  end;

  (* !!!
   * This isn't complete yet.  We need to query the object's parent and name to make this work.
   *)
  JS_ValueToObject(FEngine.Context, rval, p);
  t := JS_GetPrivate(FEngine.Context, p);
  if (t <> nil) then
    res := TJSObject(t)
  else
    res := TJSObject.Create(p, FEngine, '');
end;

procedure TJSObject.CheckConnection;
begin
  if (not FConnected) then
    raise Exception.Create('Connection to TJSEngine instance expected.  Assign Engine property of TJSObject instance.');
end;

function TJSObject.ClassType(const Name: TBridgeString): JSClassType;
var
  rval: jsval;
  cls: PJSClass;
begin
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(Name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(Name), Length(Name), @rval);
  {$ENDIF}
  if (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_OBJECT) then
  begin
    cls := JS_GetClass(JSValToObject(rval));
    Result := ctUnknownClass;
  end
  else
  begin
    cls := nil;
    Result := ctUnknownType;
  end;

  if (cls = FEngine.farrayclass) then
    Result := ctArray
  else if (cls = FEngine.fdateclass) then
    Result := ctDate
  else if (cls = FEngine.fbooleanclass) then
    Result := ctBoolean
  else if (cls = FEngine.fnumberclass) then
    Result := ctNumber
  else if (cls = FEngine.fstringclass) then
    Result := ctString
  else
    case JS_TypeOfValue(FEngine.Context, rval) of
      JSTYPE_STRING: Result := ctString;
      JSTYPE_BOOLEAN: Result := ctBoolean;
      JSTYPE_NUMBER: Result := ctNumber;
    end;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FJSObj := AValue;
  FJSVal := JSObjectToJSVal(FJSObj);

  Engine := AEngine;
  if (AEngine <> nil) then
    Parent := FEngine.Global; // Set this before we
  JSName := AName;
  Init;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FJSObj := AValue;
  FJSVal := JSObjectToJSVal(FJSObj);

  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
  Init;
end;

function TJSObject.Declare(const val: TBridgeString; const name: TBridgeString = ''): TJSString;
begin
  CheckConnection;
  Result := TJSString.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Integer; const name: TBridgeString = ''): TJSInteger;
begin
  CheckConnection;
  Result := TJSInteger.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Double; const name: TBridgeString = ''): TJSDouble;
begin
  CheckConnection;
  Result := TJSDouble.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.DeclareObject(const name: TBridgeString): TJSObject;
begin
  CheckConnection;
  Result := TJSObject.Create(nil, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(val: Boolean; const name: TBridgeString = ''): TJSBoolean;
begin
  CheckConnection;
  Result := TJSBoolean.Create(val, FEngine, name, self);
  FEngine.AddVar(Result);
end;

function TJSObject.Declare(var val: TJSBaseArray; const name: TBridgeString = ''): TJSArray;
var
  i: Integer;
begin
  CheckConnection;
  Result := TJSArray.Create(FEngine, name);
  Result.Parent := self;
  for i := 0 to Length(val)-1 do
    Result.Add(val[i]);
  FEngine.AddVar(Result);
end;

destructor TJSObject.Destroy;
begin
  inherited;
  FNatives.Free;
end;

function TJSObject.Enumerate: TStringArray;
var
  list: PJSIdArray;
  curid: pjsid;
  val: jsval;
  i: Integer;
begin
  CheckConnection;
  list := JS_Enumerate(FEngine.Context, FJSObj);
  curid := @list^.vector;

  SetLength(Result, list^.length);
  for i := 0 to list^.length-1 do
  begin
    JS_IdToValue(FEngine.Context, curid^, @val);
    Result[i] := TBridgeString(JS_GetStringChars(JS_ValueToString(FEngine.Context, val)));
    Inc(curid);
  end;
end;

function TJSObject.Evaluate(const code: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := false;

  {$IFNDEF JSUnicode}
  JS_EvaluateScript(FEngine.Context, FJSObj, PBridgeChar(code), Length(code), nil, 0, @rval);
  {$ELSE}
  JS_EvaluateUCScript(FEngine.Context, FJSObj, PBridgeChar(code), Length(code), nil, 0, @rval);
  {$ENDIF}
end;

function TJSObject.GetFunction(const name: TBridgeString): TJSFunction;
var
  rval: jsval;
  jsfun: PJSFunction;
begin
  Result := nil;

  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval = JSVAL_VOID) or (rval = JSVAL_NULL) then
    exit;
    
  jsfun := JS_ValueToFunction(FEngine.Context, rval);
  if (jsfun <> nil) then
    Result := TJSFunction.Create(jsfun, FEngine, name, self);
end;

function TJSObject.GetNativeProperty(Obj: TObject; AName: TBridgeString): jsval;
var
  ts: TBridgeString;
  PInfo: PPropInfo;
  methods: TNamedArray;
  jsobj: PJSObject;
begin
  CheckConnection;
  (* Method calls also come here to get the jsval for object functions *)
  PInfo := GetPropInfo(Obj.ClassInfo, AName);
  if (PInfo <> nil) then
    {$IFNDEF FPC}
    case PInfo^.PropType^^.Kind of
    {$ELSE}
    case PInfo^.PropType^.Kind of     // <-- freepascal-compatible statement
    {$ENDIF}
      tkString,
      tkLString,
      tkWString:
        begin
          {$IFNDEF JSUnicode}
          ts := TBridgeString(GetStrProp(Obj, AName));
          Result := jsval(JS_NewStringCopyN(FEngine.Context, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ELSE}
          ts := TBridgeString(GetWideStrProp(Obj, AName));
          Result := jsval(JS_NewUCStringCopyN(FEngine.Context, PBridgeChar(ts), Length(ts))) or JSVAL_STRING;
          {$ENDIF}
        end;
      tkInteger, tkChar, tkWChar, tkClass, tkEnumeration:
        Result := IntToJSVal(GetOrdProp(Obj, AName));
    else
      Result := JSVAL_NULL;
    end
  else
  begin
    methods := TNamedArray(FNatives[Obj]);
    jsobj := methods[AName];
    if (jsobj = nil) then
    begin
      // Raise a JS error here
      Result := JS_FALSE;
    end
    else
      Result := jsval(jsobj);
  end;
end;

function TJSObject.GetProperty(const name: TBridgeString; var int: Integer): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToInt32(FEngine.Context, rval, @int)
  else
    int := JSVAL_NULL;
end;

function TJSObject.GetProperty(const name: TBridgeString; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl)
  else
    dbl := JSVAL_NULL;
end;

function TJSObject.GetProperty(const name: TBridgeString; var ret: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    JS_ValueToObject(FEngine.Context, rval, p);
    (* !!!
     * This is wasteful.  We need to figure out how to find existing wrappers
     * for instance |p|.
     *)
    ret := TJSObject.Create(p, FEngine, name, self);
  end
  else
    ret := nil;
end;

function TJSObject.GetProperty(const name: TBridgeString; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    bool := JSValToBoolean(rval)
  else
    bool := false;
end;

function TJSObject.GetProperty(const name: TBridgeString; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, FJSObj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    {$IFNDEF JSUnicode}
    str := JS_GetStringBytes(JS_ValueToString(FEngine.Context, rval));
    {$ELSE}
    str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
    {$ENDIF}
    UniqueString(str);
  end
  else
    str := '';
end;

function TJSObject.HasProperty(const name: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval); 
  {$ENDIF}
  Result := (rval <> JSVAL_VOID);
end;

procedure TJSObject.Init;
begin
  FNatives := TPtrArray.Create;
  FNatives.OwnsValues := true;
end;

procedure TJSObject.InternalConnect;
begin
  if (FJSObj = nil) then
  begin
    FJSObj := JS_NewObject(FEngine.Context, @intf_general_class, nil, nil);
    FJSVal := JSObjectToJSVal(FJSObj);
    FScope := FEngine.Global;
  end;

  if (not IsLive) and (CanGoLive) then
  begin
    JS_RemoveRoot(FEngine.Context, @FJSVal);
    FScope.SetProperty(FName, self);
  end;
end;

function TJSObject.IsFunction(const name: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval <> JSVAL_VOID) then
    Result := (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_FUNCTION)
  else
    Result := false;
end;

function TJSObject.IsInteger(const name: TBridgeString): Boolean;
var
  rval: jsval;
begin
  CheckConnection;
  {$IFNDEF JSUnicode}
  JS_LookupProperty(FEngine.Context, FJSObj, PBridgeChar(name), @rval);
  {$ELSE}
  JS_LookupUCProperty(FEngine.Context, FJSObj, PBridgeChar(name), Length(name), @rval);
  {$ENDIF}
  if (rval <> JSVAL_VOID) then
    Result := JSValIsInt(rval)
  else
    Result := false;
end;

procedure TJSObject.RemoveObject(Obj: TJSBase);
var
  parent: PJSObject;
  {$IFDEF JSUnicode}
  rval: jsval;
  {$ENDIF}
begin
  CheckConnection;
  parent := Obj.Parent.JSObject;
  {$IFNDEF JSUnicode}
  JS_DeleteProperty(FEngine.Context, parent, PBridgeChar(Obj.JSName));
  {$ELSE}
  JS_DeleteUCProperty2(FEngine.Context, parent, PBridgeChar(Obj.JSName), Length(Obj.JSName), @rval);
  {$ENDIF}
  Obj.Free;
end;

function TJSObject.SetNativeProperty(Obj: TObject; AName: TBridgeString; AValue: Variant): JSBool;
var
  PInfo: PPropInfo;
begin
  CheckConnection;
  PInfo := GetPropInfo(Obj.ClassInfo, AName);
  if (PInfo <> nil) then
  begin
    {$IFNDEF FPC}
    if (PInfo^.PropType^^.Kind <> tkClass) then
    {$ELSE}
    if (PInfo^.PropType^.Kind <> tkClass) then
    {$ENDIF}
      SetPropValue(Obj, AName, AValue);
    Result := JS_TRUE;
  end
  else
  begin
    // Set a JS error here
    Result := JS_FALSE;
  end;
end;

function TJSObject.SetProperty(const name: TBridgeString; val: TJSBase): Boolean;
begin
  CheckConnection;
  if (HasProperty(name)) then
    {$IFNDEF JSUnicode}
    Result := (JS_SetProperty(FEngine.Context, FJSObj, CreateAnsiString(name), @val.JScriptVal) = JS_TRUE)
    {$ELSE}
    Result := (JS_SetUCProperty(FEngine.Context, FJSObj, CreateWideString(name), Length(name), @val.JScriptVal) = JS_TRUE)
    {$ENDIF}
  else
    {$IFNDEF JSUnicode}
    Result := (JS_DefineProperty(FEngine.Context, FJSObj, CreateAnsiString(name), val.JScriptVal, nil, nil, JSPROP_ENUMERATE) = JS_TRUE);
    {$ELSE}
    Result := (JS_DefineUCProperty(FEngine.Context, FJSObj, CreateWideString(name), Length(name), val.JScriptVal, nil, nil, JSPROP_ENUMERATE) = JS_TRUE);
    {$ENDIF}
end;

function TJSObject.TypeOf(const name: TBridgeString): JSType;
var
  rval: jsval;
begin
  CheckConnection;
  if (FEngine.InternalGet(name, FJSObj, rval)) then
    Result := JS_TypeOfValue(FEngine.Context, rval)
  else
    Result := JSTYPE_VOID;
end;

{ TJSFunction }

function TJSFunction.Call(params: Array of TJSBase; var int: Integer): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    int := JSVAL_NULL;
    exit;
  end;

  int := JSValToInt(rval);
end;

function TJSFunction.Call(params: Array of TJSBase; var str: TBridgeString): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    str := '';
    exit;
  end;

  str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
  UniqueString(str);
end;

function TJSFunction.Call(params: Array of TJSBase; var dbl: Double): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    dbl := JSVAL_NULL;
    exit;
  end;

  if (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSFunction.Call(params: Array of TJSBase; var bool: Boolean): Boolean;
var
  rval: jsval;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    bool := false;
    exit;
  end;

  bool := JSValToBoolean(rval);
end;

function TJSFunction.Call(params: Array of TJSBase; var res: TJSObject): Boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  FScope.CheckConnection;

  Result := false;
  if (not FEngine.InternalCall(FJSFun, FScope.JSObject, params, @rval)) then
  begin
    res := nil;
    exit;
  end;

  (*
   * This isn't complete yet.  We need to query the object's parent and name to make this work.
   *)
  JS_ValueToObject(FEngine.Context, rval, p);
  res := TJSObject.Create(p, FEngine, '');
end;

function TJSFunction.Call(var int: Integer): Boolean;
begin
  Result := Call([], int);
end;

function TJSFunction.Call(var str: TBridgeString): Boolean;
begin
  Result := Call([], str);
end;

function TJSFunction.Call(var dbl: Double): Boolean;
begin
  Result := Call([], dbl);
end;

function TJSFunction.Call(var bool: Boolean): Boolean;
begin
  Result := Call([], bool);
end;

function TJSFunction.Call(var res: TJSObject): Boolean;
begin
  Result := Call([], res);
end;

constructor TJSFunction.Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString);
begin
  FJSFun := AValue;
  Engine := AEngine;
  JSName := AName;
  if (FConnected) then
    Parent := FEngine.Global;
end;

constructor TJSFunction.Create(AValue: PJSFunction; AEngine: TJSEngine; const AName: TBridgeString; AParent: TJSObject);
begin
  FJSFun := AValue;
  Engine := AEngine;
  JSName := AName;
  Parent := AParent;
end;

destructor TJSFunction.Destroy;
begin
  inherited;
end;

procedure TJSFunction.InternalConnect;
begin
  if (FJSFun = nil) then
    {$IFNDEF JSUnicode}
    FJSFun := JS_DefineFunction(FEngine.Context, FScope.JSObject, PBridgeChar(FName), FCall, FArgCount, JSPROP_ENUMERATE);
    {$ELSE}
    FJSFun := JS_DefineUCFunction(FEngine.Context, FScope.JSObject, PBridgeChar(FName), Length(FName), FCall, FArgCount, JSPROP_ENUMERATE);
    {$ENDIF}
  // not sure yet :)
end;

{ TJSScript }

procedure TJSScript.Compile(AEngine: TJSEngine);
begin
  FScript := AEngine.Compile(FCode);
  FCompiled := true;
end;

constructor TJSScript.Create;
begin
  FCode := '';
  FScript := nil;
end;

constructor TJSScript.Create(const ACode: TBridgeString);
begin
  FCode := ACode;
end;

constructor TJSScript.Create(const ACode: TBridgeString; AEngine: TJSEngine);
begin
  FCode := ACode;
  Compile(AEngine);
end;

procedure TJSScript.Execute(AEngine: TJSEngine);
begin
  Execute(AEngine, AEngine.Global);
end;

procedure TJSScript.Execute(AEngine: TJSEngine; AScope: TJSObject);
begin
  if (not FCompiled) then
    Compile(AEngine);
  AEngine.Execute(FScript, AScope);
end;

procedure TJSScript.LoadCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmOpenRead);
  try
    LoadCompiledFromStream(fs, AEngine);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.LoadCompiledFromStream(AStream: TStream; AEngine: TJSEngine);
var
  ms: TMemoryStream;
  xdr: PJSXDRState;
  data: PBridgeChar;
  len: size_t;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(AStream);

    ms.Position := 0;
    data := ms.Memory;
    len := ms.Size;

    xdr := JS_XDRNewMem(AEngine.Context, JSXDR_DECODE);
    if (xdr <> nil) then
    begin
      JS_XDRMemSetData(xdr, data, len);
      JS_XDRScript(xdr, FScript);
    end;

    FCompiled := true;
    FCode := '';
  finally
    ms.Free;
  end;
end;

procedure TJSScript.LoadRaw(const AFile: TBridgeString);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.LoadFromFile(AFile);
    FCode := S.Text;

    FCompiled := false;
    FScript := nil;
  finally
    S.Free;
  end;
end;

procedure TJSScript.SaveCompiled(const AFile: TBridgeString; AEngine: TJSEngine);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  try
    SaveCompiledToStream(fs, AEngine);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.SaveCompiledToStream(AStream: TStream; AEngine: TJSEngine);
var
  xdr: PJSXDRState;
  data: Pointer;
  len: size_t;
begin
  if (not FCompiled) then
    Compile(AEngine);

  xdr := JS_XDRNewMem(AEngine.Context, JSXDR_ENCODE);
  if (xdr <> nil) and (JS_XDRScript(xdr, FScript) = JS_TRUE) then
  begin
    data := JS_XDRMemGetData(xdr, @len);
    AStream.Write(data^, len);
  end
  else
    raise Exception.Create('The compiled script code may be corrupted; unable to save it to disk.');
end;

procedure TJSScript.SaveRaw(const AFile: TBridgeString);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Text := FCode;
    S.SaveToFile(AFile);
  finally
    S.Free;
  end;
end;

end.

