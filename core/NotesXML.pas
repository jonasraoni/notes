//
//    NotesXML - rotinas usadas para ler e escrever arquivos
//                    de configuração do Notes.
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
  @abstract(NotesXML - rotinas usadas para ler e escrever arquivos de configuração do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
  As rotinas desta unit permitem ler/escrever tags no estilo XML, como usado no Notes.
  Estas rotinas não são capazes de ler e escrever em arquivos. Você deve usar
  as rotinas da unit @link(NotesFuncs) para isto. Todas as rotinas trabalham
  lendo ou gravando em uma string. No caso das rotinas que escrevem na string,
  você deve passar a string com o operador "@@" na frente. Exemplo: @@MinhaString.
  As rotinas de leitura possuem sempre um valor padrão (chamado DefaultValue)
  que será atribuído no caso de um erro acontecer. O código fonte de Notes está
  cheio de exemplos de como usar estas rotinas no caso de você ter alguma dúvida :)
*)
unit NotesXML;

interface

uses
  SysUtils, Classes, Windows, graphics, NotesUtils, LibXmlParser;

{ Funções para manipular XML simples, como se fosse um arquivo INI. }

// Adiciona os headers de um documento XML. Passe em @code(SourceStr)
// a string que contém o documento, em @code(RootTagName) passe
// o nome que você deseja que seja usado como a tag raiz.
procedure AddStandardXMLTags(var SourceStr: string; const RootTagName: string);// Adiciona o footer de um documento XML finalizando-o.
// Lê tag e retorna o valor como uma string
function ReadTagStr(const aSourceString, tagName, DefaultValue:string):String;
// Lê tag e retorna o valor como um integer
function ReadTagInt(const aSourceString, tagName : String; const DefaultValue:integer):Integer;
// Lê tag e retorna o valor como um booleano
function ReadTagBool(const aSourceString, tagName : String; const DefaultValue: Boolean):Boolean;
// Lê tag e retorna o valor como uma cor (TColor)
function ReadTagColor(const aSourceString, tagName : String; const DefaultValue:TColor):TColor;
// Adiciona uma nova tag em StringToWrite usando um valor do tipo string.
procedure WriteTagStr(StringToWrite: PString; const tagName, Value: string);
// Adiciona uma nova tag em StringToWrite usando um valor do tipo integer.
procedure WriteTagInt(StringToWrite: PString; const tagName: string; const Value: integer);
// Adiciona uma nova tag em StringToWrite usando um valor do tipo bollean.
procedure WriteTagBool(StringToWrite: PString; const tagName: string; const Value: boolean);
// Adiciona uma nova tag em StringToWrite usando um valor do tipo TColor.
procedure WriteTagColor(StringToWrite: PString; const tagName: string; const Value: TColor);

// Lê uma sessão (use para ler tags dentro de outras tags)
function ReadSection(const aSourceString, tagName: string): String;
// Escreve uma sessão (use para escrever tags dentro de tags)
procedure WriteSection(StringToWrite: PString; const tagName, Value: string);


implementation

uses
  FastStrings, NotesHTML;

function XML2TXT(const S : string): string;
begin
  Result := FastReplace(S,'&amp;','&', False);
  Result := FastReplace(Result,'&quot;','"', False);
  Result := FastReplace(Result,'&copy;','© ',False);
  Result := FastReplace(Result,'&reg;','®',False);
  Result := FastReplace(Result,'&lt;','<', False);
  Result := FastReplace(Result,'&gt;','>', False);
end;

function TXT2XML(const S : string): string;
begin
  Result := FastReplace(S,'&','&amp;', False);
  Result := FastReplace(Result,'"','&quot;', False);
  Result := FastReplace(Result,'©','&copy;',False);
  Result := FastReplace(Result,'®','&reg;',False);
  Result := FastReplace(Result,'<','&lt;', False);
  Result := FastReplace(Result,'>','&gt;', False);
end;


procedure AddStandardXMLTags(var SourceStr: string; const RootTagName: string);
begin
  SourceStr:= '<?xml version="1.0" encoding="ISO-8859-1"?>'+#13#13+'<'+ RootTagName +'>'+#13#13+ SourceStr +#13+ '</' + RootTagName + '>';
end;

function ReadTagStr(const aSourceString, tagName, DefaultValue:string):String;
var
  intEndPos, intInitPos: Integer;
  EndTag, InitTag: string;
begin
   initTag:='<'+tagName+'>';
   EndTag:='</'+tagName+'>';
   intInitPos:= FastPosNoCase(aSourceString,InitTag,length(aSourceString),length(inittag),1) + length(inittag);
   intEndPos:=  FastPosNoCase(aSourceString,EndTag,length(aSourceString),Length(EndTag),intInitPos);
   if (intInitPos>0) and (intEndPos>0) and (intEndPos-intInitPos>0) then begin
     SetLength(Result, intEndPos-intInitPos);
     FastCharMove(aSourceString[intInitPos],Result[1], intEndPos-intInitPos);
     Result:= XML2TXT(Result);
   end
   else
     Result:=DefaultValue;
End;

function ReadTagInt(const aSourceString, tagName : String; const DefaultValue:integer):Integer;
Var
  intEndPos, intInitPos: Integer;
  strTemp, EndTag, InitTag: string;
Begin

   if aSourcestring = EmptyStr then begin
     Result:= DefaultValue;
     Exit;
   end;
   initTag:='<'+tagName+'>';
   EndTag:='</'+tagName+'>';
   intInitPos:= FastPosNoCase(aSourceString,InitTag,length(aSourceString),length(inittag),1) + length(inittag);
   intEndPos:=  FastPosNoCase(aSourceString,EndTag,length(aSourceString),Length(EndTag),intInitPos);
   if (intInitPos>0) and (intEndPos>0) and (intEndPos-intInitPos>0) then begin
     SetLength(strTemp, intEndPos-intInitPos);
     FastCharMove(aSourceString[intInitPos],strTemp[1], intEndPos-intInitPos);
     if IsNumber(strTemp) then
       Result:=StrToInt(StrTemp)
     else
       Result:=DefaultValue;
   end
   else
     Result:=DefaultValue;
End;

function ReadTagBool(const aSourceString, tagName : String; const DefaultValue: Boolean):Boolean;
// lê os dois primeiros caracteres entre as tags procurando por 1 ou 0
// o primeiro caracter tem preferência!
var
  intInitPos: integer;  // holds the end of the initial tag
  initTag:String;
Begin
   if aSourcestring = EmptyStr then begin
     Result:= DefaultValue;
     Exit;
   end;
   initTag:='<'+tagName+'>';
   intInitPos:= FastPosNoCase(aSourceString,InitTag,length(aSourceString),length(inittag),1);
   if intInitPos>0 then
   begin
     intInitPos:= intInitPos  + length(inittag);
     if (aSourceString[intInitPos]='1') or (aSourceString[intInitPos]='0') then
     begin
       if aSourceString[intInitPos] = '1' then
         Result:=True
       else
         Result:=False;
     end
     else if (aSourceString[intInitPos+1]='1') or (aSourceString[intInitPos+1]='0') then begin
       if aSourceString[intInitPos+1] = '1' then
         Result:=True
       else
         Result:=False;
     end
     else
       Result:=DefaultValue;
   end
   else
     Result:=DefaultValue;
end;

function ReadTagColor(const aSourceString, tagName : String; const DefaultValue:TColor):TColor;
var
  I: integer;
begin
  I := ReadTagInt(aSourceString,tagName,-1);
  if I=-1 then
    Result := DefaultValue
  else
    Result := I;
end;

procedure WriteTagStr(StringToWrite: PString; const tagName, Value: string);
Begin
  StringToWrite^ := StringToWrite^+'<'+tagName+'>'+ TXT2XML( Value )+ '</'+tagName+'>'#13#10;
End;

procedure WriteTagInt(StringToWrite: PString; const tagName: string; const Value: integer);
Begin
  StringToWrite^:=StringToWrite^+'<'+tagName+'>'+intToStr(Value)+'</'+tagName+'>'#13#10;
End;

procedure WriteTagBool(StringToWrite: PString; const tagName: string; const Value: boolean);
var
  strTemp: string;
begin
  if Value then
    strTemp:='1'
  else
    strTemp:='0';
  StringToWrite^:=StringToWrite^+'<'+tagName+'>'+strTemp+'</'+tagName+'>'#13#10;
end;

procedure WriteTagColor(StringToWrite: PString; const tagName: string; const Value: TColor);
begin
  StringToWrite^:=StringToWrite^+'<'+tagName+'>'+IntToStr(integer(Value))+'</'+tagName+'>'#13#10;
end;

procedure WriteSection(StringToWrite: PString; const tagName, Value: string);
Begin
  StringToWrite^ := StringToWrite^+'<'+tagName+'>'+Value+'</'+tagName+'>'#13#10;
End;

function ReadSection(const aSourceString, tagName: string): String;
var
  intEndPos, intInitPos: Integer;
  EndTag, InitTag: string;
begin
   initTag:='<'+tagName+'>';
   EndTag:='</'+tagName+'>';
   intInitPos:= FastPosNoCase(aSourceString,InitTag,length(aSourceString),length(inittag),1) + length(inittag);
   intEndPos:=  FastPosNoCase(aSourceString,EndTag,length(aSourceString),Length(EndTag),intInitPos);
   if (intInitPos>0) and (intEndPos>0) and (intEndPos-intInitPos>0) then
   begin
     SetLength(Result, intEndPos-intInitPos);
     FastCharMove(aSourceString[intInitPos],Result[1], intEndPos-intInitPos);
   end
   else
     Result:= '';
End;


end.
