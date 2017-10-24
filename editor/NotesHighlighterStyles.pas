//
//    TNotesHighlighterStyles - Estilos da coloração de sintaxe do Notes
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

{
@abstract(NotesHighlighter - Styles for the Syntax Highlighter)
@author(Anderson Barbieri <notesnr@ig.com.br>) }
unit NotesHighlighterStyles;

interface

uses SysUtils, classes, contnrs, graphics, SynEditHighlighter, libXmlParser;

type
  // Guarda uma lista de estilos para coloração
  //  no formato do synedit - TSynHighlighterAttributes
  TNotesHighlighterStyles = class (TObjectList)
  private
    FFileName: string;
  public
    constructor Create;
    // Cria um novo TSynHighlighterAttributes com os parâmetros passados
    // e adiciona ele a lista de estilos
    procedure addStyle(Name: string; FG, BG: TColor; Font: TFontStyles);
    // Preenche uma TStringList com os nomes dos estilos existentes
    procedure listStylesNames(const AList: TStrings);
    // pega o estilo com o nome passado. Retorna nil se ele não existir
    function  getStyle(const Name: string): TSynHighlighterAttributes;
    // pega o estilo com o nome passado. Retorna o parâmetro "Def" se ele não existir
    function  getStyleDef(const Name: string; const Def: TSynHighlighterAttributes): TSynHighlighterAttributes;
    // Carrega os estilos do arquivo
    procedure Load;
    // Salva os estilos para o arquivo
    procedure Save;
    // Recarrega apenas as propriedades dos items, sem destruir a lista
    procedure Reload;
    // salva os estilos para uma string em XML
    function getStylesAsXML: string;
    // Associa a lista a um arquivo (usado em Load, Save e Reload)
    property StyleFile: string read FFileName write FFileName;
  end;

implementation

// Convert String to TFontStyles
function String2Fs(Style: string): TFontStyles;
begin
  Result := [];
  if Pos('B', Style) > 0 then
    Include( Result, fsBold );
  if Pos('I', Style) > 0 then
    Include( Result, fsItalic );
  if Pos('U', Style) > 0 then
    Include( Result, fsUnderline );
  if Pos('S', Style) > 0 then
    Include( Result, fsStrikeOut );
end;

// Convert TFontStyles to String
function Fs2String(Style: TFontStyles): string;
begin
  Result := '';
  if fsBold in Style then Result := Result + 'B';
  if fsItalic in Style then Result := Result + 'I';
  if fsUnderline in Style then Result := Result + 'U';
  if fsStrikeOut in Style then Result := Result + 'S';
end;


{ TNotesHighlighterStyles }

constructor TNotesHighlighterStyles.Create;
begin
  self.OwnsObjects:= true;
end;

function TNotesHighlighterStyles.getStyle(
  const Name: string): TSynHighlighterAttributes;
begin
  Result:= getStyleDef(Name, nil);
end;

function TNotesHighlighterStyles.getStyleDef(const Name: string;
  const Def: TSynHighlighterAttributes): TSynHighlighterAttributes;
Var
  I: integer;
begin
  Result := Def;
  for I:= 0 to self.Count -1 do
    if SameText(TSynHighlighterAttributes(self.Items[I]).Name, Name) then
    begin
      Result:= TSynHighlighterAttributes(self.Items[I]);
      Exit;
    end;
end;



procedure TNotesHighlighterStyles.addStyle(Name: string; FG, BG: TColor;
  Font: TFontStyles);
var
  atr: TSynHighlighterAttributes;
begin
  atr:= TSynHighlighterAttributes.Create(Name);
  atr.Foreground:= FG;
  atr.Background:= BG;
  atr.Style:= Font;
  self.Add(atr);
end;


procedure TNotesHighlighterStyles.listStylesNames(const AList: TStrings);
Var
  I: integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for I:= 0 to self.Count -1 do
      AList.Add(TSynHighlighterAttributes(self.Items[I]).Name);
  finally
    AList.EndUpdate;
  end;
end;

function TNotesHighlighterStyles.getStylesAsXML: string;
Var
  I: integer;
begin
  Result:= '<?xml version="1.0" encoding="ISO-8859-1"?>'#13#10#13#10'<NotesHighlighterStyles>'#13#10#13#10;

  for I:= 0 to self.Count -1 do
  begin
    Result:= Result + '  <style>'#13#10;
    Result:= Result + '    <name>'+ TSynHighlighterAttributes(self.Items[I]).Name + '</name>'#13#10;
    Result:= Result + '    <foreground>'+ IntToStr(TSynHighlighterAttributes(self.Items[I]).Foreground) + '</foreground>'#13#10;
    Result:= Result + '    <background>'+ IntToStr(TSynHighlighterAttributes(self.Items[I]).Background) + '</background>'#13#10;
    Result:= Result + '    <fontstyle>'+ Fs2String(TSynHighlighterAttributes(self.Items[I]).Style) + '</fontstyle>'#13#10;
    Result:= Result + '  </style>'#13#10#13#10;
  end;

  Result:= Result + '</NotesHighlighterStyles>';
end;

procedure TNotesHighlighterStyles.Load;
var
  xml: TXMLParser;
  n, fs: string;
  fg, bg: TColor;
begin
  if not FileExists(FFileName) then
    raise Exception.Create(ClassName + '.Load - "'+FFileName+'" does not exists.');

  Clear;

  fg:= clBlack;
  bg:= clWhite;
  xml:= TXMLParser.Create;
  try
    xml.LoadFromFile(FFileName);
    xml.StartScan;
    while xml.Scan do
    begin
      if SameText(xml.CurName, 'style') then
      begin
        // no início de cada estilo, limpamos. Ao fim de cada um,
        // criamos o novo estilo na classe
        if xml.CurPartType = ptStartTag then
        begin
          fg:= clBlack;
          bg:= clBlack;
          n:= '';
          fs:= '';
        end else
        if xml.CurPartType = ptEndtag then
        begin
          self.addStyle(n, fg, bg, String2Fs(fs));
        end;
      end else
      begin
        if (sameText(xml.CurName, 'name')) and (xml.CurPartType = ptContent) then
          n:= Copy(xml.CurContent, 1, length(xml.CurContent))
        else if (sameText(xml.CurName, 'fontstyle')) and (xml.CurPartType = ptContent) then
          fs:= Copy(xml.CurContent, 1, length(xml.CurContent))
        else if (sameText(xml.CurName, 'foreground')) and (xml.CurPartType = ptContent) then
          fg:= strToIntDef(xml.CurContent, clBlack)
        else if (sameText(xml.CurName, 'background')) and (xml.CurPartType = ptContent) then
          bg:= strToIntDef(xml.CurContent, clWhite);
      end;
    end;
  finally
    FreeAndNil(xml);
  end;
end;

procedure TNotesHighlighterStyles.Reload;
var
  xml: TXMLParser;
  n, fs: string;
  fg, bg: TColor;
  fstyle: TSynHighlighterAttributes;
begin
  if not FileExists(FFileName) then
    raise Exception.Create(self.ClassName + '.Reload - "'+FFileName+'" does not exists.');
  fg:= clBlack;
  bg:= clWhite;
  xml:= TXMLParser.Create;
  try
    xml.LoadFromFile(FFileName);
    xml.StartScan;
    while xml.Scan do
    begin
      if SameText(xml.CurName, 'style') then
      begin
        // no início de cada estilo, limpamos. Ao fim de cada um,
        // criamos o novo estilo na classe
        if xml.CurPartType = ptStartTag then
        begin
          fg:= clBlack;
          bg:= clBlack;
          n:= '';
          fs:= '';
        end else
        if xml.CurPartType = ptEndtag then
        begin
          // se achamos um estilo com o mesmo nome,
          // apenas mudamos as propriedades
          fstyle:= self.getStyle(n);
          if fstyle <> nil then
          begin
            fstyle.Foreground:= fg;
            fstyle.Background:= bg;
            fstyle.Style:= String2Fs(fs);
          end else
            self.addStyle(n, fg, bg, String2Fs(fs));
        end;
      end else
      begin
        if (sameText(xml.CurName, 'name')) and (xml.CurPartType = ptContent) then
          n:= Copy(xml.CurContent, 1, length(xml.CurContent))
        else if (sameText(xml.CurName, 'fontstyle')) and (xml.CurPartType = ptContent) then
          fs:= Copy(xml.CurContent, 1, length(xml.CurContent))
        else if (sameText(xml.CurName, 'foreground')) and (xml.CurPartType = ptContent) then
          fg:= strToIntDef(xml.CurContent, clBlack)
        else if (sameText(xml.CurName, 'background')) and (xml.CurPartType = ptContent) then
          bg:= strToIntDef(xml.CurContent, clWhite);
      end;
    end;
  finally
    FreeAndNil(xml);
  end;
end;

procedure TNotesHighlighterStyles.Save;
var
  S: string;
begin
  if not fileExists(FFileName) then
    raise Exception.Create(ClassName+'.Save - "'+FFileName+'" does not exists.');

  S:= self.getStylesAsXML;
  with TFileStream.Create(FFileName, fmOpenWrite) do
  begin
    try
      Write( Pointer(S)^ , length(S) );
    finally
      free;
    end;
  end;
end;

end.
