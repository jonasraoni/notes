//
//    NotesProject -  classes para manipular os projetos do Notes
//
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
  @abstract(NotesProject -  classes para manipular os projetos do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesProject;

interface

uses Windows, SysUtils, Classes, NotesEditorTab, NotesProfile, LibXMLParser;

type

  // Descreve um arquivo do projeto com suas opções
  TNotesProjFile = class(TObject)
  public
    FileName: string;
    Marks: TNotesMarksList;
    Col: integer;
    Line: integer;
    TopLine: integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TNotesProjFilesItems = array of TNotesProjFile;

  TNotesProjFiles = class(TObject)
  private
    fItems: TNotesProjFilesItems;
    fCount: integer;
    function Get(Index: Integer): TNotesProjFile;
    procedure Put(Index: Integer; const Value: TNotesProjFile);
  public
    destructor Destroy; override;
    // Chame add para adicionar um item a Lista
    // O método retornará um pointer para um objeto onde você
    // deverá preencher nome, marcadores, etc.
    function add: TNotesProjFile;
    // remove um item pelo nome do arquivo
    function remove(FileName: string): integer;
    // Retorna o número do item em que está o arquivo
    function indexOf(FileName: string): integer;
    // Deleta todos os items
    procedure Clear;
    // Permite manipular os items (arquivos, marcadores, etc.)
    property Items[Index: Integer]: TNotesProjFile read Get write Put; default;
    // Retorna o número de items atual
    property Count: integer read fCount;
  end;

  // Opções para uso de buildfiles
  TNotesProjBuildFile = record
    // Usar build file?
    use,
    // O Notes deve autogerar o build file?!!?!
    autogen: boolean;
    // Nome do build file a ser usado/gerado
    Name,
    // Tipo de buildfile (MAKE, ANT, NANT, etc.)
    BuildFileType: string;
  end;
  
  // Opções para publicação do projeto
  TNotesCmdPublish = record
    // diretório para o qual os arquivos devem ser publicados
    dir,
    // filtro para arquivos que devem ser publicados
    filters: string;
    // Sobrescrever arquivos na pasta de destino?!
    OverwriteFiles,
    // Compilar antes de executar
    CompileBefore: boolean;
  end;

  // Opções para executar o projeto
  TNotesCmdExecute = record
    Name: string;
    Cmd: string;
    Args: string;
    Dir: string;
    Output: TNotesRunOutputType;
    SaveBeforeRun,
    CompileBeforeRun: Boolean;
  end;
  
  // Tipo de projeto: script/compilado/website
  TNotesProjType = (ptScript, ptCompiled, ptWebSite);
  
  // Guarda todas as informações sobre um projeto do Notes
  TNotesProject = class(TObject)
  private
    fFileType: string;
    fAutoAddFiles: boolean;
    fAutoAddFilesFilter: string;
    fName: string;
    fFileName: string;
    fExecute: TNotesCmdExecute;
    fPublish: TNotesCmdPublish;
    fBuildFile: TNotesProjBuildFile;
    fFiles: TNotesProjFiles;
    fProjType: TNotesProjType;
    fCompile: TNotesRunItem;
    fLibs: TStringList;
    procedure readFileTag(const xml: TXMLParser);
    procedure readProjLibsTag(const xml: TXMLParser);
    procedure readBuildFileTag(const xml: TXMLParser);
    procedure readCompileTag(const xml: TXMLParser);
    procedure readPublishTag(const xml: TXMLParser);
    procedure readExecuteTag(const xml: TXMLParser);
    procedure setFileName(const Value: string);
  public
    // Carrega o projeto do arquivo descrito em "FileName"
    procedure Load;
    // Salva o projeto do arquivo descrito em "FileName"
    procedure Save;
    // Seta o arquivo do projeto. Se o
    property FileName: string read fFileName write setFileName;
    // Nome do projeto
    property Name: string read fName;
    // Tipo de projeto
    property ProjType: TNotesProjType read fProjType write fProjType;
    // Tipo de arquivo do projeto
    property FileType: string read fFileType write fFileType;
    // Se o Notes deve auto-adicionar arquivos que apareçam na pasta do projeto
    property AutoAddFiles: boolean read fAutoAddFiles write fAutoAddFiles;
    // Filtro para arquivos que devem ser adicionados
    property AutoAddFilesFilter: string read fAutoAddFilesFilter write fAutoAddFilesFilter;
    // Lista de arquivos do projeto
    property Files: TNotesProjFiles read fFiles;
    // Lista de bibliotecas usadas no projeto
    property ProjectLibs: TStringList read fLibs;
    // BuildFile usado para compilar o projeto
    property BuildFile: TNotesProjBuildFile read fBuildFile;
    // Comandos usados para compilar o projeto
    property CmdCompile: TNotesRunItem read fCompile write fCompile;
    // Comandos usados para publicar o projeto
    property CmdPublish: TNotesCmdPublish read fPublish write fPublish;
    // Comandos usados para executar o projeto
    property CmdExecute: TNotesCmdExecute read fExecute write fExecute;
  end;


implementation

{ TNotesProjFile }

constructor TNotesProjFile.Create;
begin
  FileName:= '';
  Marks:= TNotesMarksList.Create;
  Col:= 0;
  Line:= 0;
  TopLine:= 0;
end;

destructor TNotesProjFile.Destroy;
begin
  Marks.Free;
end;

{ TNotesProjFiles }

function TNotesProjFiles.add: TNotesProjFile;
begin
  inc(fCount);
  setLength(fItems, fCount);
  fItems[fCount-1]:= TNotesProjFile.Create;
  Result:= fItems[fCount-1];
end;

procedure TNotesProjFiles.Clear;
Var
  I: integer;
begin
  for I:= 0 to fCount -1 do
    fItems[I].Free;
  setLength(fItems, 0);
end;

destructor TNotesProjFiles.Destroy;
begin
  Clear;
end;

function TNotesProjFiles.Get(Index: Integer): TNotesProjFile;
begin
  Result:= fItems[Index];
end;

function TNotesProjFiles.indexOf(FileName: string): integer;
Var
  I: integer;
begin
  Result:= -1;

  for I:= 0 to fCount - 1 do
    if SameText(fItems[I].FileName, FileName) then
    begin
      Result:= I;
      Exit;
    end;
end;

procedure TNotesProjFiles.Put(Index: Integer; const Value: TNotesProjFile);
begin
  fItems[Index]:= Value;
end;

function TNotesProjFiles.remove(FileName: string): integer;
Var
  I: integer;
begin
  I:= indexOf(FileName);
  Result:= I;
  if I < -1 then
  begin
    // destruímos o item
    fItems[I].Free;
    // passamos o último item para o item destruído
    fItems[I]:= fItems[fCount-1];
    Dec(fCount);
    // deletamos o último item do array
    setLength(fItems, Count);
  end;
end;


{ TNotesProject }

procedure TNotesProject.Load;
Var
  xml: TXMLParser;
begin
  if not FileExists(fFileName) then
    raise Exception.Create('O arquivo do projeto passado não existe.');

  xml:= TXMLParser.Create;
  try
    xml.LoadFromFile(fFileName);
    xml.StartScan;
    while (xml.Scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'NotesProject') ) )do
    begin
      if xml.curPartType = ptStartTag then
      begin
        if xml.CurName = 'filetype' then
          self.fFileType:= xml.CurContent
        else if xml.CurName = 'projtype' then
          self.fProjType:= TNotesProjType(StrToIntDef(xml.curContent, 0))
        else if xml.CurName = 'autoaddfiles' then
        begin
          if SameText(xml.curContent, 'true') then
            self.fAutoAddFiles:= true
          else
            self.fAutoAddFiles:= false;
        end
        else if xml.CurName = 'autoaddfilesfilter' then
          self.fAutoAddFilesFilter:= xml.CurContent
        else if xml.CurName = 'file' then
          readFileTag(xml)
        else if xml.CurName = 'projectlibs' then
          readProjLibsTag(xml)
        else if xml.CurName = 'buildfile' then
          readBuildFileTag(xml)
        else if xml.Curname = 'compile' then
          readCompileTag(xml)
        else if xml.CurName = 'publish' then
          readPublishTag(xml)
        else if xml.CurName = 'execute' then
          readExecuteTag(xml);
      end;
    end;
  finally
    xml.Free;
  end;
end;


procedure TNotesProject.readBuildFileTag(const xml: TXMLParser);
begin
  while (xml.scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'buildfile') ) ) do
  begin
    if xml.curName = 'name' then
      self.fBuildFile.name:= xml.curContent
    else if  xml.curName = 'type' then
      self.fBuildFile.buildfiletype:= xml.curContent
    else if xml.CurName = 'autogen' then
      self.fBuildFile.autogen:= SameText(xml.CurContent, 'true')
    else if xml.CurName = 'use' then
      self.fBuildFile.use:= SameText(xml.CurContent, 'true');
  end;
end;

procedure TNotesProject.readCompileTag(const xml: TXMLParser);
begin
//
end;

procedure TNotesProject.readExecuteTag(const xml: TXMLParser);
begin
  while (xml.scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'execute') ) ) do
  begin
    if xml.curName = 'cmd' then
      self.fExecute.Cmd:= xml.curContent
    else if  xml.curName = 'args' then
      self.fExecute.Args:= xml.curContent
    else if  xml.curName = 'dir' then
      self.fExecute.Dir:= xml.curContent
    else if xml.CurName = 'output' then
      self.fExecute.Output:= TNotesRunOutputType(StrToIntDef(xml.CurContent, 0))
    else if xml.CurName = 'compilebefore' then
      self.fExecute.CompileBeforeRun:= SameText(xml.CurContent, 'true')
    else if xml.CurName = 'savebeforerun' then
      self.fExecute.SaveBeforeRun:= SameText(xml.CurContent, 'true');
  end;
end;

procedure TNotesProject.readFileTag(const xml: TXMLParser);
Var
  f: TNotesProjFile;
  S: string;
begin
  f:= self.fFiles.add;
  while (xml.scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'file') ) ) do
  begin
    if xml.CurName = 'filename' then
    begin
      S:= StringReplace(xml.CurContent, ExtractFilePath(fFileName), '', [rfReplaceAll]);
      S:= IncludeTrailingPathDelimiter(ExtractFilePath(fFileName)) + S;
      f.FileName:= StringReplace(S, '\\', '\', [rfReplaceAll]);
    end
    else if xml.CurName = 'position' then
    begin
      f.Col:= StrToIntDef(xml.CurAttr.Value('col'), 1);
      f.Line:= StrToIntDef(xml.CurAttr.Value('line'), 1);
      f.TopLine:= StrToIntDef(xml.CurAttr.Value('topline'), 1);
    end else
    if xml.CurName = 'marks' then
    begin
      While (xml.Scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'marks') ) ) do
        f.Marks.Add(StrToIntDef(xml.CurContent, 1));
    end;
  end;
end;

procedure TNotesProject.readProjLibsTag(const xml: TXMLParser);
begin
  while (xml.scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'execute') ) ) do
  begin
    if xml.CurName = 'lib' then
      fLibs.Add(xml.CurContent);
  end;
end;

procedure TNotesProject.readPublishTag(const xml: TXMLParser);
begin
  while (xml.scan) and ( not ( (xml.CurPartType = ptEndTag) and (xml.CurName = 'execute') ) ) do
  begin
    if  xml.curName = 'dir' then
      self.fPublish.dir:= xml.curContent
    else if  xml.curName = 'filters' then
      self.fPublish.filters:= xml.curContent
    else if xml.CurName = 'compilebefore' then
      self.fPublish.CompileBefore:= SameText(xml.CurContent, 'true');
  end;
end;

procedure TNotesProject.Save;
Var
  S: string;
  I: integer;
begin
  S:= '<?xml version="1.0" encoding="ISO-8859-1"?>'#13'<NotesProject format="1">'#13#13;

  S:= S + '  <filetype>'+ self.fFileType +'</filetype>'#13 +
  '  <projtype>'+ IntToStr(Ord(self.fProjType)) +'<projtype>'#13;

  if self.fAutoAddFiles then
    S:= S + '  <autoaddfiles>true</autoaddfiles>'#13
  else
    S:= S + '  <autoaddfiles>false</autoaddfiles>'#13;

  S:= S + '  <autoaddfilesfilter>'+ self.fAutoAddFilesFilter +'</autoaddfilesfilter>'#13#13+
  '  <files>'#13;

  // ARQUIVOS


  // END arquivos
  S:= S + '  </files>'#13#13;

  // BIBLIOTECAS
  S:= S + '  <projectlibs>'#13;
  for I:= 0 to fLibs.Count -1 do
    S:= S + '    <lib>'+fLibs.Strings[I]+'</lib>'#13;
  S:= S + '  </projectlibs>'#13#13;

  // BUILDFILE
  S:= S + '  <buildfile>'#13;
  S:= S + '     <name>'+self.fBuildFile.Name+'</name>'#13+
  '    <type>'+self.fBuildFile.BuildFileType+'<type>'#13;

  if self.fBuildFile.use then
    S:= S + '     <use>true</use>'#13
  else
    S:= S + '     <use>false</use>'#13;

  if self.fBuildFile.autogen then
    S:= S + '     <autogen>true</autogen>'#13
  else
    S:= S + '     <autogen>false</autogen>'#13;

  S:= S + '  </buildfile>'#13#13;


  // comandos PUBLISH
  S:= S+'    <publish>'#13;

  if self.fPublish.CompileBefore then
    S:= S + '      <compilebefore>true</compilebefore>'#13
  else
    S:= S + '      <compilebefore>false</compilebefore>'#13;

  S:= S+'    <publish>'#13#13;
  // comandos EXECUTE
  ///////////////////////////////////////

  S:= S + '  </commands>'#13#13;
  S:= S + '</NotesProject>';
end;

procedure TNotesProject.setFileName(const Value: string);
begin
  if FileExists(fFileName) then
    if not RenameFile(fFileName, Value) then
      raise Exception.Create('O arquivo do projeto não pode ser renomeado.');

  fFileName := Value;
end;

end.
