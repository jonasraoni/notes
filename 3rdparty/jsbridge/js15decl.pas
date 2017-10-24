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
 *   Andrew J. Howard <andrew.j.howard@team.telstra.com>
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

unit js15decl;

{$I jsconfig.inc}

interface

uses SysUtils;

type
  TBridgeString = {$IFNDEF JSUnicode}String{$ELSE}WideString{$ENDIF};
  PBridgeChar = {$IFNDEF JSUnicode}PChar{$ELSE}PWideChar{$ENDIF};
  
const
  {$IFDEF LINUX}
    LibName='libjs.so';
  {$ELSE}
    {$IFNDEF JSDebug}LibName = 'libjs.dll'{$ELSE}LibName = 'libjs.dll'{$ENDIF};
  {$ENDIF}

const
  (* A couple static values for reserving slots in a custom JSClass *)
  JSCLASS_RESERVE_SLOTS_1   : Cardinal = 512;
  JSCLASS_RESERVE_SLOTS_2   : Cardinal = 1024;

  (* Pretty near all of these are enums within the API *)
  JSVAL_OBJECT = $0;
  JSVAL_INT = $1;
  JSVAL_DOUBLE = $2;
  JSVAL_STRING = $4;
  JSVAL_BOOLEAN = $6;
  JSVAL_TAGMASK = $1 or $2 or $4;
  JSVAL_TAGBITS = 3;

  (* Flags for JSClass |flags| property *)
  JSCLASS_HAS_PRIVATE = 1;
  JSCLASS_NEW_ENUMERATE = 2;
  JSCLASS_NEW_RESOLVE = 4;
  JSCLASS_PRIVATE_IS_NSISUPPORTS = 8;
  JSCLASS_SHARE_ALL_PROPERTIES = 16;
  JSCLASS_NEW_RESOLVE_GETS_START = 32;

  (* May be private *)
  JS_MAP_GCROOT_NEXT = 0;
  JS_MAP_GCROOT_STOP = 1;
  JS_MAP_GCROOT_REMOVE = 2;

  (* May be private *)
  JSOPTION_STRICT = 0;
  JSOPTION_WERROR = 1;
  JSOPTION_VAROBJFIX = 2;
  JSOPTION_PRIVATE_IS_NSISUPPORTS = 4;

  (* Numeric equivalents of javascript versions *)
  JSVERSION_1_0 = 100;
  JSVERSION_1_1 = 110;
  JSVERSION_1_2 = 120;
  JSVERSION_1_3 = 130;
  JSVERSION_1_4 = 140;
  JSVERSION_ECMA_3 = 148;
  JSVERSION_1_5 = 150;
  JSVERSION_DEFAULT = 0;
  JSVERSION_UNKNOWN = -1;

  (* Property attributes *)
  JSPROP_ENUMERATE = $01;
  JSPROP_READONLY = $02;
  JSPROP_PERMANENT = $04;
  JSPROP_EXPORTED = $08;
  JSPROP_GETTER = $10;
  JSPROP_SETTER = $20;
  JSPROP_SHARED = $40;
  JSPROP_INDEX = $80;

  (* Function attributes *)
  JSFUN_LAMBDA = $08;
  JSFUN_GETTER = JSPROP_GETTER;
  JSFUN_SETTER = JSPROP_SETTER;
  JSFUN_BOUND_METHOD = $40;
  JSFUN_HEAVYWEIGHT = $80;
  JSFUN_FLAGS_MASK = $F8;

  (*  *)
  JSPD_ENUMERATE = $01;
  JSPD_READONLY = $02;
  JSPD_PERMANENT = $04;
  JSPD_ALIAS = $08;
  JSPD_ARGUMENT = $10;
  JSPD_VARIABLE = $20;
  JSPD_EXCEPTION = $40;
  JSPD_ERROR = $80;

  (* De/serialization modes *)
  JSXDR_ENCODE = 0;
  JSXDR_DECODE = 1;
  JSXDR_FREE = 2;

  (* De/serialization stream positions *)
  JSXDR_SEEK_SET = 0;
  JSXDR_SEEK_CUR = 1;
  JSXDR_SEEK_END = 2;

  (* Error report flag values *)
  JSREPORT_ERROR = $0;
  JSREPORT_WARNING = $1;
  JSREPORT_EXCEPTION = $2;
  JSREPORT_STRICT = $4;

  (* An example of an enumerated type that needs to be constant, but...well, see below.
  JSACC_PROTO = 0;
  JSACC_PARENT = 1;
  JSACC_IMPORT = 2;
  JSACC_WATCH = 3;
  JSACC_READ = 4;
  JSACC_WRITE = 8;
  *)

type
  (* These are OK as enums, since the values are sequential from 0 *)
  JSType = (JSTYPE_VOID,JSTYPE_OBJECT,JSTYPE_FUNCTION,JSTYPE_STRING,JSTYPE_NUMBER,JSTYPE_BOOLEAN);
  JSIterateOp = (JSENUMERATE_INIT, JSENUMERATE_NEXT, JSENUMERATE_DESTROY);
  JSGCStatus = (JSGC_BEGIN, JSGC_END, JSGC_MARK_END, JSGC_FINALIZE_END);

  (* These need to move out as constants, but they're also "types" in that one or more APIs want a
   * specific range of values.  These are not sequential values, and I don't really know a decent
   * balance between the two situations.  (See above commented const declaration. *)
  JSAccessMode = (JSACC_PROTO, JSACC_PARENT, JSACC_IMPORT, JSACC_WATCH, JSACC_READ, JSACC_WRITE, JSACC_LIMIT);
  JSExecPart = (JSEXEC_PROLOG, JSEXEC_MAIN);
  JSTrapStatus = (JSTRAP_ERROR, JSTRAP_CONTINUE, JSTRAP_RETURN, JSTRAP_THROW, JSTRAP_LIMIT);

  size_t = Cardinal;
  ptrdiff_t = Integer;
  uptrdiff_t = Cardinal;
  int8 = ShortInt;
  uint8 = Byte;
  uintN = Cardinal;
  intN = Integer;
  uint32 = Cardinal;
  int32 = Integer;
  uint16 = Word;
  int16 = SmallInt;

  puintN = ^uintN;
  pintN = ^intN;
  puint32 = ^uint32;
  pint32 = ^int32;
  puint16 = ^uint16;
  pint16 = ^int16;
  pint8 = ^int8;
  puint8 = ^uint8;

  JSUintn = Cardinal;
  JSIntn = Integer;
  JSUint8 = Byte;
  JSInt8 = ShortInt;
  JSUint16 = Word;
  JSInt16 = SmallInt;
  JSUint32 = Cardinal;
  JSInt32 = Integer;
  JSUint64 = Int64; // no such thing as unsigned 64-bit numbers in Delphi 5!
  JSInt64 = Int64;
  JSFloat32 = Single;
  JSFloat64 = Double;
  JSWord = Integer;
  JSUWord = Cardinal;
  JSBool = JSIntn;
  JSPackedBool = JSUint8;
  JSSize = size_t;
  JSPtrdiff = ptrdiff_t;
  JSUptrdiff = uptrdiff_t;
  JSVersion = Integer;
  JSHashNumber = uint32;
  JSXDRMode = uint32;

  jschar = JSUint16;
  jsint = JSInt32;
  jsuint = JSUint32;
  jsdouble = JSFloat64;
  jsval = JSWord;
  jsid = JSWord;
  jsrefcount = JSInt32;
  jsbytecode = uint8;
  jsatomid = uint32;

  pjschar = PWideChar;
  ppjschar = ^pjschar;
  pjsint = ^jsint;
  pjsuint = ^jsuint;
  pjsdouble = ^jsdouble;
  ppjsval = ^pjsval;
  pjsval = ^jsval;
  pjsid = ^jsid;
  pjsrefcount = ^jsrefcount;
  pjsbytecode = ^jsbytecode;

  PJSBool = ^JSBool;

const
  JS_TRUE: JSIntn = 1;
  JS_FALSE: JSIntn = 0;

  JSVAL_VOID: jsval = 0 - $40000000;
  JSVAL_NULL: jsval = 0;
  JSVAL_ZERO: jsval = 0;
  JSVAL_ONE: jsval = 1;
  JSVAL_FALSE: jsval = 0;
  JSVAL_TRUE: jsval = 1;

type
  PFile = ^File;
  (* Some of the following are intended to be opaque pointers, others accessible.  Haven't sorted out which, though. *)
  PJSClass = ^JSClass;
  PJSScript = Pointer;
  PJSObject = Pointer; // ^JSObject;
  PJSContext = Pointer;
  PJSRuntime = Pointer;
  PJSErrorReport = ^JSErrorReport;
  PJSString = ^JSString;
  PJSIdArray = ^JSIdArray;
  PJSPrincipals = Pointer;
  PJSFunction = ^JSFunction;
  PJSFunctionSpec = ^JSFunctionSpec;
  PJSLocaleCallbacks = ^JSLocaleCallbacks;
  PJSExceptionState = ^JSExceptionState;
  PJSHashTable = Pointer;
  PPJSHashEntry = ^PJSHashEntry;
  PJSHashEntry = Pointer;
  PJSConstDoubleSpec = ^JSConstDoubleSpec;
  PJSPropertySpec = ^JSPropertySpec;
  PJSPropertyDesc = ^JSPropertyDesc;
  PJSPropertyDescArray = ^JSPropertyDescArray;
  PJSScopeProperty = ^JSScopeProperty;
  PJSStackFrame = Pointer;
  PJSAtom = Pointer; // ^JSAtom
  PPJSAtom = ^PJSAtom;
  PJSObjectMap = ^JSObjectMap;
  PJSObjectOps = ^JSObjectOps;
  PJSProperty = ^JSProperty;
  PJSXDRState = ^JSXDRState;
  JSXDRState = Pointer;

  JSPropertyOp = function(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
  JSNewEnumerateOp = function(cx: PJSContext; obj: PJSObject; enum_op: JSIterateOp; statep: pjsval; idp: pjsid): JSBool; cdecl;
  JSEnumerateOp = function(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
  JSResolveOp = function(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
  JSConvertOp = function(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
  JSFinalizeOp = procedure(cx: PJSContext; obj: PJSObject); cdecl;

  JSGetObjectOps = function(cx: PJSContext; clasp: PJSClass): Pointer; cdecl;
  JSCheckAccessOp = function(cx: PJSContext; obj: PJSObject; id: jsval; mode: JSAccessMode; vp: pjsval): JSBool; cdecl;
  JSXDRObjectOp = function(xdr: PJSXDRState; var objp: PJSObject): JSBool; cdecl;
  JSHasInstanceOp = function(cx: PJSContext; obj: PJSObject; v: jsval; bp: PJSBool): JSBool; cdecl;
  JSMarkOp = function(cx: PJSContext; obj: PJSObject; arg: Pointer): uint32; cdecl;
  JSNewObjectMapOp = function(cx: PJSContext; nrefs: jsrefcount; ops: PJSObjectOps; clasp: PJSClass; obj: PJSObject): PJSObjectMap; cdecl;
  JSObjectMapOp = procedure(cx: PJSContext; map: PJSObjectMap); cdecl;
  JSLookupPropOp = function(cx: PJSContext; obj: PJSObject; id: jsid; var objp: PJSObject; var propp: PJSProperty): JSBool; cdecl;
  JSDefinePropOp = function(cx: PJSContext; obj: PJSObject; id: jsid; value: jsval; getter, setter: JSPropertyOp; attrs: uintN; var propp: PJSProperty): JSBool; cdecl;
  JSPropertyIdOp = function(cx: PJSContext; obj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl;
  JSAttributesOp = function(cx: PJSContext; obj: PJSObject; id: jsid; prop: PJSProperty; attrsp: puintN): JSBool; cdecl;
  JSCheckAccessIdOp = function(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode; vp: pjsval; attrsp: puintN): JSBool; cdecl;
  JSObjectOp = function(cx: PJSContext; obj: PJSObject): PJSObject; cdecl;
  JSPropertyRefOp = procedure(cx: PJSContext; obj: PJSObject; prop: PJSProperty); cdecl;
  JSSetObjectSlotOp = function(cx: PJSContext; obj: PJSObject; slot: uint32; pobj: PJSObject): JSBool; cdecl;
  JSGetRequiredSlotOp = function(cx: PJSContext; obj: PJSObject; slot: uint32): jsval; cdecl;
  JSSetRequiredSlotOp = procedure(cx: PJSContext; obj: PJSObject; slot: uint32; v: jsval); cdecl;
  JSNative = function(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
  JSGCCallback = function(cx: PJSContext; status: JSGCStatus): JSBool; cdecl;
  JSGCRootMapFun = function(rp: Pointer; name: PChar; data: Pointer): intN; cdecl;
  JSBranchCallback = function(cx: PJSContext; script: PJSScript): JSBool; cdecl;
  JSErrorReporter = procedure(cx: PJSContext; message: PChar; report: PJSErrorReport); cdecl;
  JSLocaleToUpperCase = function(cx: PJSContext; src: PJSString; rval: pjsval): JSBool; cdecl;
  JSLocaleToLowerCase = function(cx: PJSContext; src: PJSString; rval: pjsval): JSBool; cdecl;
  JSLocaleCompare = function(cx: PJSContext; src1, src2: PJSString; rval: pjsval): JSBool; cdecl;
  JSStringFinalizeOp = procedure(cx: PJSContext; str: PJSString); cdecl;
  JSHashEnumerator = function(he: PJSHashEntry; i: intN; arg: Pointer): intN; cdecl;
  JSHashComparator = function(v1, v2: Pointer): intN; cdecl;
  JSHashFunction = function(key: Pointer): JSHashNumber; cdecl;
  JSPrincipalsTranscoder = function(xdr: PJSXDRState; var principalsp: PJSPrincipals): JSBool; cdecl;

  JSAtomMap = record
    vector: PPJSAtom;
    length: jsatomid;
  end;

  JSScript = record
    code: pjsbytecode;
    length: uint32;
    main: pjsbytecode;
    version: JSVersion;
    atomMap: JSAtomMap;
    filename: PChar;
    lineno: uintN;
    depth: uintN;
    trynotes: Pointer;
    principals: Pointer;
    _object: Pointer;
  end;

  JSFunction = record
    nrefs: jsrefcount;
    obj: PJSObject;
    native: JSNative;
    script: PJSScript;
    nargs: uint16;
    extra: uint16;
    nvars: uint16;
    flags: uint8;
    spare: uint8;
    atom: PJSAtom;
    clasp: PJSClass;
  end;

  JSFunctionSpec = record
    name: PChar;
    call: JSNative;
    nargs: uint8;
    flags: uint8;
    extra: uint16;
  end;
  TJSFunctionSpecArray = array of JSFunctionSpec;

  JSProperty = record
    id: jsid;
  end;

  JSObjectMap = record
    nrefs: jsrefcount;
    ops: PJSObjectOps;
    nslots: uint32;
    freeslot: uint32;
  end;

  JSObjectOps = record
  (* Mandatory non-null members *)
    newObjectMap: JSNewObjectMapOp;
    destroyObjectMap: JSObjectMapOp;
    lookupProperty: JSLookupPropOp;
    defineProperty: JSDefinePropOp;
    getProperty: JSPropertyIdOp;
    setProperty: JSPropertyIdOp;
    getAttributes: JSAttributesOp;
    setAttributes: JSAttributesOp;
    deleteProperty: JSPropertyIdOp;
    defaultValue: JSConvertOp;
    enumerate: JSNewEnumerateOp;
    checkAccess: JSCheckAccessIdOp;

  (* Optionally non-null members *)
    thisObject: JSObjectOp;
    dropProperty: JSPropertyRefOp;
    call: JSNative;
    construct: JSNative;
    xdrObject: JSXDRObjectOp;
    hasInstance: JSHasInstanceOp;
    setProto: JSSetObjectSlotOp;
    setParent: JSSetObjectSlotOp;
    mark: JSMarkOp;
    clear: JSFinalizeOp;
    getRequiredSlot: JSGetRequiredSlotOp;
    setRequiredSlot: JSSetRequiredSlotOp;
  end;

  JSString = record
    length: size_t;
    chars: pjschar;
  end;

  JSIdArray = record
    length: jsint;
    vector: jsid;
  end;

  JSLocaleCallbacks = record
    localeToUpperCase: JSLocaleToUpperCase;
    localeToLowerCase: JSLocaleToLowerCase;
    localeCompare: JSLocaleCompare;
  end;

  JSExceptionState = record
    throwing: JSBool;
    exception: jsval;
  end;

  JSConstDoubleSpec = record
    dval: jsdouble;
    name: PChar;
    flags: uint8;
    spare: array[0..2] of uint8;
  end;

  JSPropertySpec = record
    name: PChar;
    tinyid: int8;
    flags: uint8;
    getter: JSPropertyOp;
    setter: JSPropertyOp;
  end;
  TJSPropertySpecArray = array of JSPropertySpec;

  JSPropertyDesc = record
    id: jsval;
    value: jsval;
    flags: uint8;
    spare: uint8;
    slot: uint16;
    alias: jsval;
  end;

  JSPropertyDescArray = record
    length: uint32;
    _array: PJSPropertyDesc;
  end;

  JSScopeProperty = record
    id: jsid;
    getter: JSPropertyOp;
    setter: JSPropertyOp;
    slot: uint32;
    attrs: uint8;
    flags: uint8;
    shortid: int16;
    parent: PJSScopeProperty;
    kids: PJSScopeProperty;
  end;

  JSClass = record
    name: PChar;
    flags: Cardinal;

  (* Mandatory non-null function pointer members. *)
    addProperty: JSPropertyOp;
    delProperty: JSPropertyOp;
    getProperty: JSPropertyOp;
    setProperty: JSPropertyOp;
    enumerate: JSEnumerateOp;
    resolve: JSResolveOp;
    convert: JSConvertOp;
    finalize: JSFinalizeOp;

  (* Optionally non-null members start here. *)
    getObjectOps: JSGetObjectOps;
    checkAccess: JSCheckAccessOp;
    call: JSNative;                (* Assign this if the object is callable (ie a function) *)
    construct: JSNative;          (* Constructor *)
    xdrObject: JSXDRObjectOp;
    hasInstance: JSHasInstanceOp;
    mark: JSMarkOp;
    spare: jsword;
  end;

  JSErrorReport = record
    filename: PChar;        // source file name, URL, etc., or null
    lineno: uintN;          // source line number
    linebuf: PChar;          // offending source line
    tokenptr: PChar;        // points to error token in linebuf (for caret positioning?)
    uclinebuf: pjschar;      // unicode line buffer
    uctokenptr: pjschar;    // unicode token pointers
    flags: uintN;
    errorNumber: uintN;      // see js.msg
    ucmessage: pjschar;      // default error message
    messageArgs: ppjschar;  // arguments for the error message
  end;

  JSErrorFormatString = record
    format: PChar;
    argCount: uintN;
  end;

// function  JS_ConvertArguments(cx: PJSContext; argc: uintN; argv: pjsval; format: PChar; : ...): JSBool; cdecl; external 'js3215.dll';
// function  JS_PushArguments(cx: PJSContext; var markp: Pointer; format: PChar; : ...): pjsval; cdecl; external 'js3215.dll';
// function  JS_ReportErrorFlagsAndNumber(cx: PJSContext; flags: uintN; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...): JSBool; cdecl; external 'js3215.dll';
// function  JS_ReportErrorFlagsAndNumberUC(cx: PJSContext; flags: uintN; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...): JSBool; cdecl; external 'js3215.dll';
// function  JS_ReportWarning(cx: PJSContext; format: PChar; : ...): JSBool; cdecl; external 'js3215.dll';
// procedure JS_ReportError(cx: PJSContext; format: PChar; : ...); cdecl; external LibName;
// procedure JS_ReportErrorNumber(cx: PJSContext; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...); cdecl; external LibName;
// procedure JS_ReportErrorNumberUC(cx: PJSContext; errorCallback: JSErrorCallback; userRef: Pointer; errorNumber: const uintN; : ...); cdecl; external LibName;
function JS_AddExternalStringFinalizer(finalizer: JSStringFinalizeOp): intN; cdecl; external LibName;
function JS_AddNamedRoot(cx: PJSContext; rp: Pointer; name: PChar): JSBool; cdecl; external LibName;
function JS_AddNamedRootRT(rt: PJSRuntime; rp: Pointer; name: PChar): JSBool; cdecl; external LibName;
function JS_AddRoot(cx: PJSContext; rp: Pointer): JSBool; cdecl; external LibName;
function JS_AliasElement(cx: PJSContext; obj: PJSObject; name: PChar; alias: jsint): JSBool; cdecl; external LibName;
function JS_AliasProperty(cx: PJSContext; obj: PJSObject; name: PChar; alias: PChar): JSBool; cdecl; external LibName;
function JS_BufferIsCompilableUnit(cx: PJSContext; obj: PJSObject; bytes: PChar; length: size_t): JSBool; cdecl; external LibName;
function JS_CallFunction(cx: PJSContext; obj: PJSObject; fun: PJSFunction; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external LibName;
function JS_CallFunctionName(cx: PJSContext; obj: PJSObject; name: PChar; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external LibName;
function JS_CallFunctionValue(cx: PJSContext; obj: PJSObject; fval: jsval; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external LibName;
function JS_CallUCFunctionName(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl; external LibName;
function JS_CheckAccess(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode; vp: pjsval; attrsp: puintN): JSBool; cdecl; external LibName;
function JS_CloneFunctionObject(cx: PJSContext; funobj: PJSObject; parent: PJSObject): PJSObject; cdecl; external LibName;
function JS_CompareStrings(str1: PJSString; str2: PJSString): intN; cdecl; external LibName;
function JS_CompileFile(cx: PJSContext; obj: PJSObject; filename: PChar): PJSScript; cdecl; external LibName;
function JS_CompileFileHandle(cx: PJSContext; obj: PJSObject; filename: PChar; fh: PFILE): PJSScript; cdecl; external LibName;
function JS_CompileFileHandleForPrincipals(cx: PJSContext; obj: PJSObject; filename: PChar; fh: PFILE; principals: PJSPrincipals): PJSScript; cdecl; external LibName;
function JS_CompileFunction(cx: PJSContext; obj: PJSObject; name: PChar; nargs: uintN; var argnames: PChar; bytes: PChar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileFunctionForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; name: PChar; nargs: uintN; var argnames: PChar; bytes: PChar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileScript(cx: PJSContext; obj: PJSObject; bytes: PChar; length: size_t; filename: PChar; lineno: uintN): PJSScript; cdecl; external LibName;
function JS_CompileScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; bytes: PChar; length: size_t; filename: PChar; lineno: uintN): PJSScript; cdecl; external LibName;
function JS_CompileUCFunction(cx: PJSContext; obj: PJSObject; name: PChar; nargs: uintN; var argnames: PChar; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileUCFunctionForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; name: PChar; nargs: uintN; var argnames: PChar; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileUCFunctionUC(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; nargs: uintN; var argnames: PChar; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileUCFunctionForPrincipalsUC(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; name: pjschar; namelen: size_t; nargs: uintN; var argnames: PChar; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSFunction; cdecl; external LibName;
function JS_CompileUCScript(cx: PJSContext; obj: PJSObject; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSScript; cdecl; external LibName;
function JS_CompileUCScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; chars: pjschar; length: size_t; filename: PChar; lineno: uintN): PJSScript; cdecl; external LibName;
function JS_ConcatStrings(cx: PJSContext; left: PJSString; right: PJSString): PJSString; cdecl; external LibName;
function JS_ConstructObject(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject): PJSObject; cdecl; external LibName;
function JS_ConstructObjectWithArguments(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject; argc: uintN; argv: pjsval): PJSObject; cdecl; external LibName;
function JS_ContextIterator(rt: PJSRuntime; var iterp: PJSContext): PJSContext; cdecl; external LibName;
function JS_ConvertStub(cx: PJSContext; obj: PJSObject; _type: JSType; vp: pjsval): JSBool; cdecl; external LibName;
function JS_ConvertValue(cx: PJSContext; v: jsval; _type: JSType; vp: pjsval): JSBool; cdecl; external LibName;
function JS_DecompileFunction(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString; cdecl; external LibName;
function JS_DecompileFunctionBody(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString; cdecl; external LibName;
function JS_DecompileScript(cx: PJSContext; script: PJSScript; name: PChar; indent: uintN): PJSString; cdecl; external LibName;
function JS_DefineConstDoubles(cx: PJSContext; obj: PJSObject; cds: PJSConstDoubleSpec): JSBool; cdecl; external LibName;
function JS_DefineElement(cx: PJSContext; obj: PJSObject; index: jsint; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external LibName;
function JS_DefineFunction(cx: PJSContext; obj: PJSObject; name: PChar; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction; cdecl; external LibName;
function JS_DefineFunctions(cx: PJSContext; obj: PJSObject; fs: PJSFunctionSpec): JSBool; cdecl; external LibName;
function JS_DefineObject(cx: PJSContext; obj: PJSObject; name: PChar; clasp: PJSClass; proto: PJSObject; attrs: uintN): PJSObject; cdecl; external LibName;
function JS_DefineProperties(cx: PJSContext; obj: PJSObject; ps: PJSPropertySpec): JSBool; cdecl; external LibName;
function JS_DefineProperty(cx: PJSContext; obj: PJSObject; name: PChar; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external LibName;
function JS_DefinePropertyWithTinyId(cx: PJSContext; obj: PJSObject; name: PChar; tinyid: int8; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external LibName;
function JS_DefineUCFunction(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction; cdecl; external LibName;
function JS_DefineUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external LibName;
function JS_DefineUCPropertyWithTinyId(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; tinyid: int8; value: jsval; getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool; cdecl; external LibName;
function JS_DeleteElement(cx: PJSContext; obj: PJSObject; index: jsint): JSBool; cdecl; external LibName;
function JS_DeleteElement2(cx: PJSContext; obj: PJSObject; index: jsint; rval: pjsval): JSBool; cdecl; external LibName;
function JS_DeleteProperty(cx: PJSContext; obj: PJSObject; name: PChar): JSBool; cdecl; external LibName;
function JS_DeleteProperty2(cx: PJSContext; obj: PJSObject; name: PChar; rval: pjsval): JSBool; cdecl; external LibName;
function JS_DeleteUCProperty2(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; rval: pjsval): JSBool; cdecl; external LibName;
function JS_Enumerate(cx: PJSContext; obj: PJSObject): PJSIdArray; cdecl; external LibName;
function JS_EnumerateStandardClasses(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external LibName;
function JS_EnumerateStub(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external LibName;
function JS_ErrorFromException(cx: PJSContext; v: jsval): PJSErrorReport; cdecl; external LibName;
function JS_EvaluateScript(cx: PJSContext; obj: PJSObject; bytes: PChar; length: uintN; filename: PChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external LibName;
function JS_EvaluateScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; bytes: PChar; length: uintN; filename: PChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external LibName;
function JS_EvaluateUCScript(cx: PJSContext; obj: PJSObject; chars: pjschar; length: uintN; filename: PChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external LibName;
function JS_EvaluateUCScriptForPrincipals(cx: PJSContext; obj: PJSObject; principals: PJSPrincipals; chars: pjschar; length: uintN; filename: PChar; lineno: uintN; rval: pjsval): JSBool; cdecl; external LibName;
function JS_ExecuteScript(cx: PJSContext; obj: PJSObject; script: PJSScript; rval: pjsval): JSBool; cdecl; external LibName;
function JS_ExecuteScriptPart(cx: PJSContext; obj: PJSObject; script: PJSScript; part: JSExecPart; rval: pjsval): JSBool; cdecl; external LibName;
function JS_GetArrayLength(cx: PJSContext; obj: PJSObject; var length: jsuint): JSBool; cdecl; external LibName;
function JS_GetClass(obj: PJSObject): PJSClass; cdecl; external LibName;
function JS_GetConstructor(cx: PJSContext; proto: PJSObject): PJSObject; cdecl; external LibName;
function JS_GetContextPrivate(cx: PJSContext): Pointer; cdecl; external LibName;
function JS_GetElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external LibName;
function JS_GetEmptyStringValue(cx: PJSContext): jsval; cdecl; external LibName;
function JS_GetExternalStringGCType(rt: PJSRuntime; str: PJSString): intN; cdecl; external LibName;
function JS_GetFunctionFlags(fun: PJSFunction): uintN; cdecl; external LibName;
function JS_GetFunctionId(fun: PJSFunction): PJSString; cdecl; external LibName;
function JS_GetFunctionName(fun: PJSFunction): PChar; cdecl; external LibName;
function JS_GetFunctionObject(fun: PJSFunction): PJSObject; cdecl; external LibName;
function JS_GetGlobalObject(cx: PJSContext): PJSObject; cdecl; external LibName;
function JS_GetImplementationVersion: PChar; cdecl; external LibName;
function JS_GetInstancePrivate(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): Pointer; cdecl; external LibName;
function JS_GetLocaleCallbacks(cx: PJSContext): PJSLocaleCallbacks; cdecl; external LibName;
function JS_GetNaNValue(cx: PJSContext): jsval; cdecl; external LibName;
function JS_GetNegativeInfinityValue(cx: PJSContext): jsval; cdecl; external LibName;
function JS_GetOptions(cx: PJSContext): uint32; cdecl; external LibName;
function JS_GetParent(cx: PJSContext; obj: PJSObject): PJSObject; cdecl; external LibName;
function JS_GetPendingException(cx: PJSContext; vp: pjsval): JSBool; cdecl; external LibName;
function JS_GetPositiveInfinityValue(cx: PJSContext): jsval; cdecl; external LibName;
function JS_GetPrivate(cx: PJSContext; obj: PJSObject): Pointer; cdecl; external LibName;
function JS_GetProperty(cx: PJSContext; obj: PJSObject; name: PChar; vp: pjsval): JSBool; cdecl; external LibName;
function JS_GetPropertyAttributes(cx: PJSContext; obj: PJSObject; name: PChar; attrsp: puintN; foundp: PJSBool): JSBool; cdecl; external LibName;
function JS_GetPrototype(cx: PJSContext; obj: PJSObject): PJSObject; cdecl; external LibName;
function JS_GetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32; vp: pjsval): JSBool; cdecl; external LibName;
function JS_GetRuntime(cx: PJSContext): PJSRuntime; cdecl; external LibName;
function JS_GetScopeChain(cx: PJSContext): PJSObject; cdecl; external LibName;
function JS_GetScriptObject(script: PJSScript): PJSObject; cdecl; external LibName;
function JS_GetStringBytes(str: PJSString): pchar; cdecl; external LibName;
function JS_GetStringChars(str: PJSString): pjschar; cdecl; external LibName;
function JS_GetStringLength(str: PJSString): size_t; cdecl; external LibName;
function JS_GetTypeName(cx: PJSContext; _type: JSType): PChar; cdecl; external LibName;
function JS_GetUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external LibName;
function JS_GetUCPropertyAttributes(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; attrsp: puintN; foundp: PJSBool): JSBool; cdecl; external LibName;
function JS_GetVersion(cx: PJSContext): JSVersion; cdecl; external LibName;
function JS_HasArrayLength(cx: PJSContext; obj: PJSObject; var length: jsuint): JSBool; cdecl; external LibName;
function JS_IdToValue(cx: PJSContext; id: jsid; vp: pjsval): JSBool; cdecl; external LibName;
function JS_Init(maxbytes: uint32): PJSRuntime; cdecl; external LibName;
function JS_InitClass(cx: PJSContext; obj: PJSObject; parent_proto: PJSObject; clasp: PJSClass; _constructor: JSNative; nargs: uintN; ps: PJSPropertySpec; fs: PJSFunctionSpec; static_ps: PJSPropertySpec; static_fs: PJSFunctionSpec): PJSObject; cdecl; external LibName;
function JS_InitStandardClasses(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external LibName;
function JS_InstanceOf(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): JSBool; cdecl; external LibName;
function JS_InternString(cx: PJSContext; s: PChar): PJSString; cdecl; external LibName;
function JS_InternUCString(cx: PJSContext; s: pjschar): PJSString; cdecl; external LibName;
function JS_InternUCStringN(cx: PJSContext; s: pjschar; length: size_t): PJSString; cdecl; external LibName;
function JS_IsAboutToBeFinalized(cx: PJSContext; thing: Pointer): JSBool; cdecl; external LibName;
function JS_IsArrayObject(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external LibName;
function JS_IsConstructing(cx: PJSContext): JSBool; cdecl; external LibName;
function JS_IsExceptionPending(cx: PJSContext): JSBool; cdecl; external LibName;
function JS_IsRunning(cx: PJSContext): JSBool; cdecl; external LibName;
function JS_LockGCThing(cx: PJSContext; thing: Pointer): JSBool; cdecl; external LibName;
function JS_LockGCThingRT(rt: PJSRuntime; thing: Pointer): JSBool; cdecl; external LibName;
function JS_LookupElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external LibName;
function JS_LookupProperty(cx: PJSContext; obj: PJSObject; name: PChar; vp: pjsval): JSBool; cdecl; external LibName;
function JS_LookupUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external LibName;
function JS_MakeStringImmutable(cx: PJSContext; str: PJSString): JSBool; cdecl; external LibName;
function JS_malloc(cx: PJSContext; nbytes: size_t): Pointer; cdecl; external LibName;
function JS_MapGCRoots(rt: PJSRuntime; map: JSGCRootMapFun; data: Pointer): uint32; cdecl; external LibName;
function JS_NewArrayObject(cx: PJSContext; length: jsint; vector: pjsval): PJSObject; cdecl; external LibName;
function JS_NewContext(rt: PJSRuntime; stackChunkSize: size_t): PJSContext; cdecl; external LibName;
function JS_NewDependentString(cx: PJSContext; str: PJSString; start: size_t; length: size_t): PJSString; cdecl; external LibName;
function JS_NewDouble(cx: PJSContext; d: jsdouble): pjsdouble; cdecl; external LibName;
function JS_NewDoubleValue(cx: PJSContext; d: jsdouble; rval: pjsval): JSBool; cdecl; external LibName;
function JS_NewExternalString(cx: PJSContext; chars: pjschar; length: size_t; _type: intN): PJSString; cdecl; external LibName;
function JS_NewFunction(cx: PJSContext; call: JSNative; nargs: uintN; flags: uintN; parent: PJSObject; name: PChar): PJSFunction; cdecl; external LibName;
function JS_NewGrowableString(cx: PJSContext; chars: pjschar; length: size_t): PJSString; cdecl; external LibName;
function JS_NewNumberValue(cx: PJSContext; d: jsdouble; rval: pjsval): JSBool; cdecl; external LibName;
function JS_NewObject(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject): PJSObject; cdecl; external LibName;
function JS_NewRegExpObject(cx: PJSContext; bytes: pchar; length: size_t; flags: uintN): PJSObject; cdecl; external LibName;
function JS_NewScriptObject(cx: PJSContext; script: PJSScript): PJSObject; cdecl; external LibName;
function JS_NewString(cx: PJSContext; bytes: pchar; length: size_t): PJSString; cdecl; external LibName;
function JS_NewStringCopyN(cx: PJSContext; s: PChar; n: size_t): PJSString; cdecl; external LibName;
function JS_NewStringCopyZ(cx: PJSContext; s: PChar): PJSString; cdecl; external LibName;
function JS_NewUCRegExpObject(cx: PJSContext; chars: pjschar; length: size_t; flags: uintN): PJSObject; cdecl; external LibName;
function JS_NewUCString(cx: PJSContext; chars: pjschar; length: size_t): PJSString; cdecl; external LibName;
function JS_NewUCStringCopyN(cx: PJSContext; s: pjschar; n: size_t): PJSString; cdecl; external LibName;
function JS_NewUCStringCopyZ(cx: PJSContext; s: pjschar): PJSString; cdecl; external LibName;
function JS_Now: int64; cdecl; external LibName;
function JS_ObjectIsFunction(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external LibName;
function JS_PropertyStub(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; external LibName;
function JS_realloc(cx: PJSContext; p: Pointer; nbytes: size_t): Pointer; cdecl; external LibName;
function JS_RemoveExternalStringFinalizer(finalizer: JSStringFinalizeOp): intN; cdecl; external LibName;
function JS_RemoveRoot(cx: PJSContext; rp: Pointer): JSBool; cdecl; external LibName;
function JS_RemoveRootRT(rt: PJSRuntime; rp: Pointer): JSBool; cdecl; external LibName;
function JS_ResolveStandardClass(cx: PJSContext; obj: PJSObject; id: jsval; resolved: PJSBool): JSBool; cdecl; external LibName;
function JS_ResolveStub(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl; external LibName;
function JS_SaveExceptionState(cx: PJSContext): PJSExceptionState; cdecl; external LibName;
function JS_SetArrayLength(cx: PJSContext; obj: PJSObject; length: jsuint): JSBool; cdecl; external LibName;
function JS_SetBranchCallback(cx: PJSContext; cb: JSBranchCallback): JSBranchCallback; cdecl; external LibName;
function JS_SetCheckObjectAccessCallback(rt: PJSRuntime; acb: JSCheckAccessOp): JSCheckAccessOp; cdecl; external LibName;
function JS_SetElement(cx: PJSContext; obj: PJSObject; index: jsint; vp: pjsval): JSBool; cdecl; external LibName;
function JS_SetErrorReporter(cx: PJSContext; er: JSErrorReporter): JSErrorReporter; cdecl; external LibName;
function JS_SetGCCallback(cx: PJSContext; cb: JSGCCallback): JSGCCallback; cdecl; external LibName;
function JS_SetGCCallbackRT(rt: PJSRuntime; cb: JSGCCallback): JSGCCallback; cdecl; external LibName;
function JS_SetOptions(cx: PJSContext; options: uint32): uint32; cdecl; external LibName;
function JS_SetParent(cx: PJSContext; obj: PJSObject; parent: PJSObject): JSBool; cdecl; external LibName;
function JS_SetPrincipalsTranscoder(rt: PJSRuntime; px: JSPrincipalsTranscoder): JSPrincipalsTranscoder; cdecl; external LibName;
function JS_SetPrivate(cx: PJSContext; obj: PJSObject; data: Pointer): JSBool; cdecl; external LibName;
function JS_SetProperty(cx: PJSContext; obj: PJSObject; name: PChar; vp: pjsval): JSBool; cdecl; external LibName;
function JS_SetPropertyAttributes(cx: PJSContext; obj: PJSObject; name: PChar; attrs: uintN; foundp: PJSBool): JSBool; cdecl; external LibName;
function JS_SetPrototype(cx: PJSContext; obj: PJSObject; proto: PJSObject): JSBool; cdecl; external LibName;
function JS_SetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32; v: jsval): JSBool; cdecl; external LibName;
function JS_SetUCProperty(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; vp: pjsval): JSBool; cdecl; external LibName;
function JS_SetUCPropertyAttributes(cx: PJSContext; obj: PJSObject; name: pjschar; namelen: size_t; attrs: uintN; foundp: PJSBool): JSBool; cdecl; external LibName;
function JS_SetVersion(cx: PJSContext; version: JSVersion): JSVersion; cdecl; external LibName;
function JS_strdup(cx: PJSContext; s: PChar): PChar; cdecl; external LibName;
function JS_StringToVersion(_string: PChar): JSVersion; cdecl; external LibName;
function JS_ToggleOptions(cx: PJSContext; options: uint32): uint32; cdecl; external LibName;
function JS_TypeOfValue(cx: PJSContext; v: jsval): JSType; cdecl; external LibName;
function JS_UCBufferIsCompilableUnit(cx: PJSContext; obj: PJSObject; chars: pjschar; length: size_t): JSBool; cdecl; external LibName;
function JS_UndependString(cx: PJSContext; str: PJSString): pjschar; cdecl; external LibName;
function JS_UnlockGCThing(cx: PJSContext; thing: Pointer): JSBool; cdecl; external LibName;
function JS_UnlockGCThingRT(rt: PJSRuntime; thing: Pointer): JSBool; cdecl; external LibName;
function JS_ValueToBoolean(cx: PJSContext; v: jsval; bp: PJSBool): JSBool; cdecl; external LibName;
function JS_ValueToConstructor(cx: PJSContext; v: jsval): PJSFunction; cdecl; external LibName;
function JS_ValueToECMAInt32(cx: PJSContext; v: jsval; ip: pint32): JSBool; cdecl; external LibName;
function JS_ValueToECMAUint32(cx: PJSContext; v: jsval; ip: puint32): JSBool; cdecl; external LibName;
function JS_ValueToFunction(cx: PJSContext; v: jsval): PJSFunction; cdecl; external LibName;
function JS_ValueToId(cx: PJSContext; v: jsval; idp: pjsid): JSBool; cdecl; external LibName;
function JS_ValueToInt32(cx: PJSContext; v: jsval; ip: pint32): JSBool; cdecl; external LibName;
function JS_ValueToNumber(cx: PJSContext; v: jsval; dp: pjsdouble): JSBool; cdecl; external LibName;
function JS_ValueToObject(cx: PJSContext; v: jsval; var objp: PJSObject): JSBool; cdecl; external LibName;
function JS_ValueToString(cx: PJSContext; v: jsval): PJSString; cdecl; external LibName;
function JS_ValueToUint16(cx: PJSContext; v: jsval; ip: puint16): JSBool; cdecl; external LibName;
function JS_VersionToString(version: JSVersion): PChar; cdecl; external LibName;
function JS_XDRBytes(xdr: PJSXDRState; bytes: PChar; len: uint32): JSBool; cdecl; external LibName;
function JS_XDRCString(xdr: PJSXDRState; var s: PChar): JSBool; cdecl; external LibName;
function JS_XDRCStringOrNull(xdr: PJSXDRState; var s: PChar): JSBool; cdecl; external LibName;
function JS_XDRDouble(xdr: PJSXDRState; var d: pjsdouble): JSBool; cdecl; external LibName;
function JS_XDRFindClassById(xdr: PJSXDRState; id: uint32): PJSClass; cdecl; external LibName;
function JS_XDRFindClassIdByName(xdr: PJSXDRState; name: PChar): JSBool; cdecl; external LibName;
function JS_XDRMemDataLeft(xdr: PJSXDRState): uint32; cdecl; external LibName;
function JS_XDRMemGetData(xdr: PJSXDRState; lp: puint32): Pointer; cdecl; external LibName;
function JS_XDRNewMem(cx: PJSContext; mode: JSXDRMode): PJSXDRState; cdecl; external LibName;
function JS_XDRRegisterClass(xdr: PJSXDRState; clasp: PJSClass; lp: uint32): JSBool; cdecl; external LibName;
function JS_XDRScript(xdr: PJSXDRState; var script: PJSScript): JSBool; cdecl; external LibName;
function JS_XDRString(xdr: PJSXDRState; var str: PJSString): JSBool; cdecl; external LibName;
function JS_XDRStringOrNull(xdr: PJSXDRState; var str: PJSString): JSBool; cdecl; external LibName;
function JS_XDRUint16(xdr: PJSXDRState; s: puint16): JSBool; cdecl; external LibName;
function JS_XDRUint32(xdr: PJSXDRState; lp: puint32): JSBool; cdecl; external LibName;
function JS_XDRUint8(xdr: PJSXDRState; b: puint8): JSBool; cdecl; external LibName;
function JS_XDRValue(xdr: PJSXDRState; vp: pjsval): JSBool; cdecl; external LibName;
procedure JS_ClearNewbornRoots(cx: PJSContext); cdecl; external LibName;
procedure JS_ClearPendingException(cx: PJSContext); cdecl; external LibName;
procedure JS_ClearRegExpRoots(cx: PJSContext); cdecl; external LibName;
procedure JS_ClearRegExpStatics(cx: PJSContext); cdecl; external LibName;
procedure JS_ClearScope(cx: PJSContext; obj: PJSObject); cdecl; external LibName;
procedure JS_DestroyContext(cx: PJSContext); cdecl; external LibName;
procedure JS_DestroyContextMaybeGC(cx: PJSContext); cdecl; external LibName;
procedure JS_DestroyContextNoGC(cx: PJSContext); cdecl; external LibName;
procedure JS_DestroyIdArray(cx: PJSContext; ida: PJSIdArray); cdecl; external LibName;
procedure JS_DestroyScript(cx: PJSContext; script: PJSScript); cdecl; external LibName;
procedure JS_DropExceptionState(cx: PJSContext; state: PJSExceptionState); cdecl; external LibName;
procedure JS_FinalizeStub(cx: PJSContext; obj: PJSObject); cdecl; external LibName;
procedure JS_Finish(rt: PJSRuntime); cdecl; external LibName;
procedure JS_free(cx: PJSContext; p: Pointer); cdecl; external LibName;
procedure JS_GC(cx: PJSContext); cdecl; external LibName;
procedure JS_Lock(rt: PJSRuntime); cdecl; external LibName;
procedure JS_MarkGCThing(cx: PJSContext; thing: Pointer; name: PChar; arg: Pointer); cdecl; external LibName;
procedure JS_MaybeGC(cx: PJSContext); cdecl; external LibName;
procedure JS_PopArguments(cx: PJSContext; mark: Pointer); cdecl; external LibName;
procedure JS_ReportOutOfMemory(cx: PJSContext); cdecl; external LibName;
procedure JS_RestoreExceptionState(cx: PJSContext; state: PJSExceptionState); cdecl; external LibName;
procedure JS_SetCallReturnValue2(cx: PJSContext; v: jsval); cdecl; external LibName;
procedure JS_SetContextPrivate(cx: PJSContext; data: Pointer); cdecl; external LibName;
procedure JS_SetGlobalObject(cx: PJSContext; obj: PJSObject); cdecl; external LibName;
procedure JS_SetLocaleCallbacks(cx: PJSContext; callbacks: PJSLocaleCallbacks); cdecl; external LibName;
procedure JS_SetPendingException(cx: PJSContext; v: jsval); cdecl; external LibName;
procedure JS_SetRegExpInput(cx: PJSContext; input: PJSString; multiline: JSBool); cdecl; external LibName;
procedure JS_ShutDown; cdecl; external LibName;
procedure JS_Unlock(rt: PJSRuntime); cdecl; external LibName;
procedure JS_XDRDestroy(xdr: PJSXDRState); cdecl; external LibName;
procedure JS_XDRInitBase(xdr: PJSXDRState; mode: JSXDRMode; cx: PJSContext); cdecl; external LibName;
procedure JS_XDRMemResetData(xdr: PJSXDRState); cdecl; external LibName;
procedure JS_XDRMemSetData(xdr: PJSXDRState; data: Pointer; len: uint32); cdecl; external LibName;

(* Conversion routines *)
function JSStringToString(str: PJSString): TBridgeString;
function JSStringToJSVal(str: PJSString): jsval;
function StringToJSString(cx: PJSContext; const str: TBridgeString): PJSString;
function StringToJSVal(cx: PJSContext; str: TBridgeString): jsval;
function JSObjectToJSVal(obj: PJSObject): jsval;
function DoubleToJSVal(cx: PJSContext; dbl: Double): jsval;
function IntToJSVal(val: Integer): jsval;
function BoolToJSVal(val: Boolean): jsval;
function JSValToDouble(cx: PJSContext; val: jsval): Double;
function JSValToObject(v: jsval): PJSObject;
function JSValToInt(val: jsval): Integer;
function JSValToJSString(val: jsval): PJSString;
function JSValToBoolean(val: jsval): Boolean;

(* Validation routines *)
function JSValIsObject(v: jsval): Boolean;
function JSValIsNumber(v: jsval): Boolean;
function JSValIsInt(v: jsval): Boolean;
function JSValIsDouble(v: jsval): Boolean;
function JSValIsString(v: jsval): Boolean;
function JSValIsBoolean(v: jsval): Boolean;
function JSValIsNull(v: jsval): Boolean;
function JSValIsVoid(v: jsval): Boolean;

function CreateAnsiString(const Text: String): PChar; overload;
function CreateAnsiString(const Text: WideString): PChar; overload;
function CreateWideString(const Text: String): PWideChar; overload;
function CreateWideString(const Text: WideString): PWideChar; overload;
procedure SetReservedSlots(Cls: JSClass; Reserve: Integer);

implementation

{$IFDEF LINUX}uses Math, SysUtils;{$ENDIF}

const
  JSCLASS_RESERVED_SLOTS_SHIFT  : Cardinal = 8;
  JSCLASS_RESERVED_SLOTS_MASK   : Cardinal = 255;

function JSStringToString(str: PJSString): TBridgeString;
begin
  Result := TBridgeString(JS_GetStringChars(str));
end;

function JSStringToJSVal(str: PJSString): jsval;
begin
  Result := jsval(str);
  if (not JSValIsString(Result)) then
    Result := Result or JSVAL_STRING;
end;

function StringToJSString(cx: PJSContext; const str: TBridgeString): PJSString;
begin
  {$IFNDEF JSUnicode}
  Result := JS_NewStringCopyN(cx, PBridgeChar(str), Length(str));
  {$ELSE}
  Result := JS_NewUCStringCopyN(cx, PBridgeChar(str), Length(str));
  {$ENDIF}
end;

function StringToJSVal(cx: PJSContext; str: TBridgeString): jsval;
var
  jsstr: PJSString;
begin
  {$IFNDEF JSUnicode}
  jsstr := JS_NewStringCopyN(cx, PBridgeChar(str), Length(str));
  {$ELSE}
  jsstr := JS_NewUCStringCopyN(cx, PBridgeChar(str), Length(str));
  {$ENDIF}
  Result := jsval(jsstr) or JSVAL_STRING;
end;

function JSObjectToJSVal(obj: PJSObject): jsval;
begin
  Result := jsval(obj);
end;

function DoubleToJSVal(cx: PJSContext; dbl: Double): jsval;
begin
  JS_NewNumberValue(cx, dbl, @Result);
end;

function IntToJSVal(val: Integer): jsval;
begin
  Result := (jsval(val) shl 1) or JSVAL_INT;
end;

function BoolToJSVal(val: Boolean): jsval;
var
  tmp: Integer;
begin
  if (val) then
    tmp := JSVAL_TRUE
  else
    tmp := JSVAL_FALSE;
  Result := (tmp shl JSVAL_TAGBITS) or JSVAL_BOOLEAN;
end;

function JSValToObject(v: jsval): PJSObject;
begin
  Result := PJSObject(v or JSVAL_OBJECT);
end;

function JSValToJSString(val: jsval): PJSString;
begin
  if (JSValIsString(val)) then
    val := val xor JSVAL_STRING;
  Result := PJSString(val);
end;

function JSValToDouble(cx: PJSContext; val: jsval): Double;
begin
  JS_ValueToNumber(cx,val,@Result);
end;

function JSValToInt(val: jsval): Integer;
begin
  Result := val shr 1;
  //shr doesn't handle signed types in the same way as C. If the source was
  //negative then merge in the missing 1 in position 31.
(*  if val < 0 then
    Result := Result or $80000000; *)
  if val < 0 then
    Result := Result or High(integer); 
end;

function JSValToBoolean(val: jsval): Boolean;
begin
  Result := (val shr JSVAL_TAGBITS = JSVAL_TRUE);
end;

function JSValIsObject(v: jsval): Boolean;
begin
  Result := (v and JSVAL_TAGMASK = JSVAL_OBJECT);
end;

function JSValIsNumber(v: jsval): Boolean;
begin
  Result := (JSValIsInt(v) or JSValIsDouble(v));
end;

function JSValIsInt(v: jsval): Boolean;
begin
  Result := (v and JSVAL_INT <> 0) and (v <> JSVAL_VOID);
end;

function JSValIsDouble(v: jsval): Boolean;
begin
  Result := (v and JSVAL_TAGMASK = JSVAL_DOUBLE);
end;

function JSValIsString(v: jsval): Boolean;
begin
  Result := (v and JSVAL_TAGMASK = JSVAL_STRING);
end;

function JSValIsBoolean(v: jsval): Boolean;
begin
  Result := (v and JSVAL_TAGMASK = JSVAL_BOOLEAN);
end;

function JSValIsNull(v: jsval): Boolean;
begin
  Result := (v = JSVAL_NULL);
end;

function JSValIsVoid(v: jsval): Boolean;
begin
  Result := (v = JSVAL_VOID);
end;

function CreateAnsiString(const Text: String): PChar;
var
  Size: Integer;
begin
  Size := Length(Text)+1;
  Result := StrMove(StrAlloc(Size), PChar(Text), Size);
end;

function CreateAnsiString(const Text: WideString): PChar; overload;
begin
  Result := PChar(WideCharToString(PWideChar(Text)));
end;

function CreateWideString(const Text: String): PWideChar; overload;
begin
  Result := StringToOleStr(Text);
end;

function CreateWideString(const Text: WideString): PWideChar; overload;
begin
  Result := PWideChar(Copy(Text, 1, Length(Text)));
end;

procedure SetReservedSlots(Cls: JSClass; Reserve: Integer);
begin
  Cls.flags := Cls.flags or ((Reserve and JSCLASS_RESERVED_SLOTS_MASK) shl JSCLASS_RESERVED_SLOTS_SHIFT);
end;

{$IFDEF LINUX}
initialization
  SetExceptionMask(GetExceptionMask+[exInvalidOp]);
{$ENDIF}

end.

