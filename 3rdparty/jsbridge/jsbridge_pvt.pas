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

unit jsbridge_pvt;

{$I jsconfig.inc}

interface

uses js15decl, SysUtils, jsintf;

function Bridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
function Bridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
procedure Bridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
function Bridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
function Bridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
function Bridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;

const
  bridge_class: JSClass = (name: 'CBridge'; flags: JSCLASS_HAS_PRIVATE; addProperty: Bridge_AddProperty;
    delProperty: Bridge_DeleteProperty; getProperty: Bridge_GetProperty; setProperty: Bridge_SetProperty;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
    finalize: Bridge_FinalizeOp);

  bridge_method_class: JSClass = (name: 'CBridgeMethod'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
    delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub;
    finalize: JS_FinalizeStub; call: Bridge_MethodCall);

implementation

uses jsbridge;

function GetDelphiObject(cx: PJSContext; obj: PJSObject): TJSBridge;
var
  data: PBridgeData;
begin
  data := PBridgeData(JS_GetPrivate(cx, obj));
  Result := data^.data;
end;

function GetParamName(cx: PJSContext; id: jsval): TBridgeString;
begin
  Result := JS_GetStringChars(JS_ValueToString(cx, id));
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var pch: TBridgeString); overload;
begin
  pch := JS_GetStringChars(JS_ValueToString(cx, vp^));
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var int: Integer); overload;
begin
  JS_ValueToInt32(cx, vp^, @int);
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var dbl: Double); overload;
begin
  JS_ValueToNumber(cx, vp^, @dbl);
end;

procedure GetParamValue(cx: PJSContext; vp: pjsval; var res: PJSObject); overload;
begin
  JS_ValueToObject(cx, vp^, res);
end;

function Bridge_AddProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_ConvertOp(cx: PJSContext; obj: PJSObject; typ: JSType; vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_DeleteProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
begin
 // Need to raise a JS error here
  Result := JS_FALSE;
end;

function Bridge_EnumerateOp(cx: PJSContext; obj: PJSObject): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

procedure Bridge_FinalizeOp(cx: PJSContext; obj: PJSObject); cdecl;
begin
end;

function Bridge_GetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TJSBridge;
begin
  (* Method calls also come here to get the jsval for object functions *)
  jsobj := GetDelphiObject(cx, obj);
  vp^ := jsobj.GetProperty(GetParamName(cx, id));

  Result := JS_TRUE;
end;

function Bridge_ResolveOp(cx: PJSContext; obj: PJSObject; id: jsval): JSBool; cdecl;
begin
  Result := JS_TRUE;
end;

function Bridge_SetProperty(cx: PJSContext; obj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl;
var
  jsobj: TJSBridge;
  str: TBridgeString;
  paramName: TBridgeString;
begin
  jsobj := GetDelphiObject(cx, obj);
  paramName := GetParamName(cx, id);
  GetParamValue(cx, vp, str);
  jsobj.SetProperty(paramName, str);

  Result := JS_TRUE;
end;

function Bridge_MethodCall(cx: PJSContext; obj: PJSObject; argc: uintN; argv: pjsval; rval: pjsval): JSBool; cdecl;
type
  TParm = record
    case typ: Integer of
      1: (int: Integer);
      2: (dbl: Double);
      3: (obj: Pointer);
      4: (bool: LongBool);
  end;
var
  bridge: PJSObject;
  meth: Pointer;
  jsobj: TJSBridge;
  i: Integer;
  arglist: array of Pointer;
  tmpv: pjsval;
  val: jsval;
	str: TBridgeString;
	parm: ^TParm;
	jscls: PJSClass;
	_edx: Pointer;
	_ecx: Pointer;
	data: PBridgeData;
begin
	tmpv := argv;
	Dec(tmpv, 2); // access argv[-2] to get the |this| value
	bridge := PJSObject(tmpv^);
	if (GetParamName(cx, tmpv^) <> '[object CBridgeMethod]') then
	begin
	  // Raise JS error here -- we don't have the right |this|
		Result := JS_FALSE;
		exit;
	end;

	SetLength(arglist, 0);
	for i := 1 to argc do
	begin
		val := argv^;
		New(parm);
		if (JSValIsInt(val)) then
		begin
			parm^.typ := 1;
			JS_ValueToInt32(cx, val, @parm^.int);
    end
    else if (JSValIsNumber(val)) then
    begin
      parm^.typ := 2;
      JS_ValueToNumber(cx, val, @parm^.dbl);
    end
		(*
		else if (JSValIsBoolean(val)) then
		begin
		 parm^.typ := 1;
		 JS_ValueToBoolean(cx,val,@jsb);
		 parm^.bool := (jsb = JS_TRUE);
		end
		*)
    else if (JSValIsString(val)) then
    begin
      str := JS_GetStringChars(JS_ValueToString(cx, val));
			parm^.typ := 3;
			parm^.obj := PBridgeChar(str);
		end
		else if (JSValIsObject(val)) then
		begin
			jscls := JS_GetClass(Pointer(val));
			if (jscls^.flags and JSCLASS_HAS_PRIVATE = JSCLASS_HAS_PRIVATE) then
			begin
				parm^.typ := 3;
				parm^.obj := JS_GetPrivate(cx, Pointer(val));
			end;

			if (jscls^.flags and JSCLASS_HAS_PRIVATE = 0) or (parm^.obj = nil) then
			begin
				str := JS_GetStringChars(JS_ValueToString(cx, val));
				parm^.typ := 3;
        parm^.obj := PBridgeChar(str);
      end;
    end;

    if (i = 1) then
    begin
      _edx := Pointer(parm^.int);
      Dec(argc);
    end
    else if (i = 2) then
    begin
      _ecx := Pointer(parm^.int);
      Dec(argc);
    end
    else
    begin
      SetLength(arglist, Length(arglist) + 1);
      arglist[Length(arglist) - 1] := Pointer(parm^.int);
    end;
    Dispose(parm);

    Inc(argv);
  end;

	data := JS_GetPrivate(cx, obj);
	jsobj := data^.data;

	meth := JS_GetPrivate(cx, bridge);
  asm
		mov esi,argc
		mov edx,arglist

		cmp argc,0
		je @docall

	@loop:
		mov eax,[edx]
		push eax
		add edx,$04
		dec esi
		jnz @loop

	@docall:
		mov 	eax,jsobj
		mov		edx,_edx
		mov		ecx,_ecx
		call	meth
  end;

  SetLength(arglist, 0);
  Result := JS_TRUE;
end;

end.

