//
//    NotesFileTypeOptions - classes para gerenciar opções específicas dos FileTypes
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
  @abstract( NotesFileTypeOptions - classes para gerenciar opções específicas dos FileTypes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesFileTypeOptions;

interface

uses Windows, SysUtils, Classes, NotesEditorTab, NotesHighlighterStyles,
  Graphics, Contnrs;

type
  { Ação a ser tomada pelo notes quando uma quebra de linha for inserida
    @code(lbNone) - nehuma ação;
    @code(lbAutoIndent) - AutoIndentar;
    @code(lbSmartIndent) - SmartIndent.
  }
  TNotesLineBreakAction = (lbNone, lbAutoIndent, lbSmartIndent);

type
  { Classe usada pelo TNotesEditorTab para carregar
    opções específicas para filetypes. Veja @link(TNotesFileTypeOptions)
    para a classe usada no diálogo de configurações. }
  TNotesFileTypeOptionsLitle = class(TObject)
  private
    fTabToSpaces: boolean;
    fAutoCloseTags: boolean;
    fIndentSize: integer;
    fTabSize: integer;
    fLineComment: string;
    fMultilineCommentStart: string;
    fFileType: string;
    fScopeBegin: string;
    fMultilineCommentEnd: string;
    fScopeEnd: string;
    fMatchingBracketsColor: TColor;
    fEditorColor: TColor;
    fMarginColor: TColor;
    fScopeColor: TColor;
    fGutterColor: TColor;
    fGutterTextColor: TColor;
    fCurrentLineColor: TColor;
    fLineBreakAction: TNotesLineBreakAction;
    fErrorColor: TColor;
    fFileFilters: string;
  public
    // É preciso passar o FileType para que a classe possa
    // carregar as informações
    constructor Create(const FileType: string); virtual;
    // Seta as opções de um EditorTab conforme as opções atuais
    procedure setEditorTabOptions(const EditorTab: TNotesEditorTab);
    // Permite saber o FileType que a classe representa
    property FileType: string read fFileType;
    // Número de espaços usados para identar
    property IdentSize: integer read fIndentSize write fIndentSize;
    // Tamanho em espaços do caracter tab
    property TabSize: integer read fTabSize write fTabSize;
    // Se devem ser usados espaços no lugar de tabs
    property TabToSpaces: boolean read fTabToSpaces write fTabToSpaces;
    // Se tags HTML/XML devem ser fechadas
    property AutoCloseTags: boolean read fAutoCloseTags write fAutoCloseTags;
    // Ação a ser tomada pelo notes quando uma quebra de linha for inserida
    property LineBreakAction: TNotesLineBreakAction read fLineBreakAction write fLineBreakAction;
    // Comentário do tipo de arquivo que fecha ao fim da linha
    property LineComment: string read fLineComment write fLineComment;
    // Início de comentário
    property MultilineCommentStart: string read fMultilineCommentStart write fMultilineCommentStart;
    // Fim de comentário
    property MultilineCommentEnd: string read fMultilineCommentEnd write fMultilineCommentEnd;
    // Keywords que determinam o início de um escopo
    property ScopeBegin: string read fScopeBegin write fScopeBegin;
    // Keywords que determinam o final de um escopo
    property ScopeEnd: string read fScopeEnd write fScopeEnd;

    // Cor do fundo do Editor
    property EditorColor: TColor read fEditorColor write fEditorColor;
    // Cor da linha atual
    property CurrentLineColor: TColor read fCurrentLineColor write fCurrentLineColor;
    // Cor do indicador de escopo
    property ScopeColor: TColor read fScopeColor write fScopeColor;
    // Cor da marcação de braços pares
    property MatchingBracketsColor: TColor read fMatchingBracketsColor write fMatchingBracketsColor;
    // Cor do texto da gutter
    property GutterTextColor: TColor read fGutterTextColor write fGutterTextColor;
    // Cor da gutter
    property GutterColor: TColor read fGutterColor write fGutterColor;
    // Cor da margem direita
    property MarginColor: TColor read fMarginColor write fMarginColor;
    // cor para erros
    property ErrorsColor: TColor read fErrorColor write fErrorColor;
    // Extensões de arquivos
    property FileFilters: string read fFileFilters write fFileFilters;
  end;

  // Classe usada para carregar/salvar opções específicas para um tipo de arquivo
  TNotesFileTypeOptions = class(TNotesFileTypeOptionsLitle)
  private
    fStyles: TNotesHighlighterStyles;
  public
    constructor Create(const FileType: string); override;
    destructor Destroy; override;
    // Salva as opções
    procedure Save;
    // Estilos da coloração
    property Styles: TNotesHighlighterStyles read fStyles;
  end;

  // Classe para gerenciar uma lista de TNotesFileTypeOptions
  // Você não precisa carregar os objetos, eles serão carregados
  // automaticamente quando você precisar deles. Use saveall
  // para salvar todas as operações.
  TNotesFileTypeOptionsMan = class(TObjectList)
  private
    function FindFileType(const FileType: string): integer;
    function LoadFileType(const FileType: string): integer;
  public
    // pega o TNotesFileTypeOptions para o filetype passado. Carrega ele se
    //  ainda não estiver carregado
    function getFileTypeOptions(const FileType: string): TNotesFileTypeOptions;
    // Lista os estilos em uma TStrings do tipo de arquivo passado
    procedure ListStyles(const FileType: string; const List: TStrings);
    // Salva tudo que foi alterado
    procedure SaveAll;
    // Compila os vários arquivos para filtros de arquivo (chame após SaveAll)
    procedure CompileFileFilters;
  end;

implementation

uses NotesGlobals, NotesUtils, NotesXML;

{ TNotesFileTypeOptionsLitle }

constructor TNotesFileTypeOptionsLitle.Create(const FileType: string);
Var
  S: string;
begin
  if not nProfile.FileTypesMgr.FileTypeExists(FileType) then
    raise Exception.Create(ClassName + '.Create - The FileType "'+FileType+'" does not exists.');

  fFileType:= FileType;
  S:= '';

  if FileExists(addSlash(nProfile.Paths.FileTypesDir + FileType) + FileType + NOTES_FILETYPE_CONFIG_EXTENSION) then
    S:= FileToStr(addSlash(nProfile.Paths.FileTypesDir + FileType) + FileType + NOTES_FILETYPE_CONFIG_EXTENSION);
  // Mesmo que o arquivo não exista, lemos de S, pois assim setamos as opções padrões :)
  fTabToSpaces:= ReadTagBool(S, 'TabToSpaces', True);
  fAutoCloseTags:= ReadTagBool(S, 'AutoCloseTags', True);
  fIndentSize:= ReadTagInt(S, 'IndentSize', 4);
  fTabSize:= ReadTagInt(S, 'TabSize', 8);
  fLineComment:= ReadTagStr(S, 'LineComment', '');
  fMultilineCommentStart:= ReadTagStr(S, 'MultilineCommentStart', '');
  fMultilineCommentEnd:= ReadTagStr(S, 'MultilineCommentEnd', '');
  fScopeBegin:= ReadTagStr(S, 'ScopeBegin', '');
  fScopeEnd:= ReadTagStr(S, 'ScopeEnd', '');
  fLineBreakAction:= TNotesLineBreakAction(ReadTagInt(S, 'LineBreakAction', 1));
  // Cores!
  fMatchingBracketsColor:= ReadTagColor(S, 'MatchingBracketsColor', clSilver);
  fEditorColor:= ReadTagColor(S, 'EditorColor', clWhite);
  fMarginColor:= ReadTagColor(S, 'MarginColor', clBtnFace);
  fScopeColor:= ReadTagColor(S, 'ScopeColor', clLtGray);
  fGutterColor:= ReadTagColor(S, 'GutterColor', clBtnFace);
  fGutterTextColor:= ReadTagColor(S, 'GutterTextColor', clBtnText);
  fCurrentLineColor:= ReadTagColor(S, 'CurrentLineColor', clMoneyGreen);
  fErrorColor:= ReadTagColor(S, 'ErrorsColor', clRed);

  fFileFilters:= nProfile.FileTypesMgr.FileTypeExtensions[fFileType];
end;

procedure TNotesFileTypeOptionsLitle.setEditorTabOptions(
  const EditorTab: TNotesEditorTab);
begin
  if EditorTab = nil then Exit;
  EditorTab.IndentSize:= fIndentSize;
  EditorTab.TabSize:= fTabSize;
  EditorTab.TabAsSpace:= fTabToSpaces;
  EditorTab.LineComment:= fLineComment;
  EditorTab.MultilineCommentStart:= fMultilineCommentStart;
  Editortab.MultilineCommentEnd:= fMultilineCommentEnd;
  EditorTab.CloseHTMLTags:= fAutoCloseTags;
  EditorTab.Editor.Gutter.Color:= fGutterColor;
  EditorTab.Editor.Gutter.Font.Color:= fGutterTextColor;
  EditorTab.Editor.Gutter.AutoSize:= true;
  EditorTab.Editor.Gutter.Font.Name:= 'Courier New';
  EditorTab.Editor.RightEdgeColor:= fMarginColor;
  EditorTab.Editor.Color:= fEditorColor;
  EditorTab.EscopeIndicatorColor:= fScopeColor;
  EditorTab.highlightSymbolsColor:= fMatchingBracketsColor;
  EditorTab.highlightLineColor:= fCurrentLineColor;
  EditorTab.AutoIndent:= (fLineBreakAction = lbAutoIndent);
  EditorTAb.UseSmartIndent:= (fLineBreakAction = lbSmartIndent);
  EditorTab.EscopeStartTokens:= fScopeBegin;
  EditorTab.EscopeEndTokens:= fScopeEnd;
  EditorTab.ErrorsColor:= fErrorColor;
  // deixar por último
  EditorTab.highlightLine:= fCurrentLineColor <> fEditorColor;
end;


{ TNotesFileTypeOptions }

constructor TNotesFileTypeOptions.Create(const FileType: string);
begin
  inherited;
  fStyles:= TNotesHighlighterStyles.Create;
  fStyles.StyleFile:= addSlash(nProfile.Paths.FileTypesDir + FileType) + FileType + NOTES_FILETYPE_HIGHLIGHTERSTYLES_EXTENSION;
  fStyles.Load;
end;

destructor TNotesFileTypeOptions.Destroy;
begin
  fStyles.Free;
  inherited;
end;

procedure TNotesFileTypeOptions.Save;
Var
  S: string;
begin
  fStyles.Save;
  S:= '';
  WriteTagBool(@S, 'TabToSpaces', fTabToSpaces);
  WriteTagBool(@S, 'AutoCloseTags', fAutoCloseTags);
  WriteTagInt(@S, 'IndentSize', fIndentSize);
  WriteTagInt(@S, 'TabSize', fTabSize);
  WriteTagStr(@S, 'LineComment', fLineComment);
  WriteTagStr(@S, 'MultilineCommentStart', fMultilineCommentStart);
  WriteTagStr(@S, 'MultilineCommentEnd', fMultilineCommentEnd);
  WriteTagStr(@S, 'ScopeBegin', fScopeBegin);
  WriteTagStr(@S, 'ScopeEnd', fScopeEnd);
  WriteTagInt(@S, 'LineBreakAction', Ord(fLineBreakAction));
  // Cores!
  WriteTagColor(@S, 'MatchingBracketsColor', fMatchingBracketsColor);
  WriteTagColor(@S, 'EditorColor', fEditorColor);
  WriteTagColor(@S, 'MarginColor', fMarginColor);
  WriteTagColor(@S, 'ScopeColor', fScopeColor);
  WriteTagColor(@S, 'GutterColor', fGutterColor);
  WriteTagColor(@S, 'GutterTextColor', fGutterTextColor);
  WriteTagColor(@S, 'CurrentLineColor', fCurrentLineColor);
  WriteTagColor(@S, 'ErrorsColor', fErrorColor);

  AddStandardXMLTags(S, 'NotesFileTypeOptions');
  StrToFile(addSlash(nProfile.Paths.FileTypesDir + FileType) + FileType + NOTES_FILETYPE_CONFIG_EXTENSION, S);

  nProfile.FileTypesMgr.FileTypeExtensions[self.FileType]:= self.FileFilters;
end;


{ TNotesFileTypeOptionsMan }

procedure TNotesFileTypeOptionsMan.CompileFileFilters;
Var
  DlgFilters: string;
  FTFilters: string;
  ExtLangAssoc: string;
  a: TStringArray;
  I, J, Len: integer;
  FileTypes: PNotesFolderTree;
begin
  DlgFilters:='Todos os arquivos (*.*)|*.*';
  ExtLangAssoc:='';
  nProfile.FileTypesMgr.ListTypes( FileTypes );
  try

    for I := 0 to High( FileTypes^ ) do
      with FileTypes^[I] do
      begin
        // colocamos a extensões em um array
        a:= ExplodeStr(nProfile.FileTypesMgr.FileTypeExtensions[Path], ';');
        try
          Len:= length(a);
          if Len > 0 then
          begin
            FTFilters:='';
            // o último não tem ';'
            for J:= 0 to Len-2 do
              FTFilters:= FTFilters + '*.' + a[J] + ';';
            if Len-1 > -1 then
              FTFilters:= FTFilters + '*.' + a[Len-1];
            DlgFilters:= DlgFilters + '|' + Path + ' (' + FTFilters + ')|' + FTFilters;

            for J:= 0 to Len-1 do
              ExtLangAssoc:= ExtLangAssoc + a[J] + ':' + Path + ASCII_CRLF;
          end;
        finally
          setLength(a, 0);
        end;

      end;

      StrToFile(nProfile.Paths.AssociationsFile, ExtLangAssoc);
      StrToFile(nProfile.Paths.DlgFiltersFile, DlgFilters);

  finally
    FreeFolderTree( FileTypes );
  end;
end;

function TNotesFileTypeOptionsMan.FindFileType(
  const FileType: string): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I:= 0 to Count -1 do
    if SameText(FileType, (Items[I] As TNotesFileTypeOptions).FileType) then
    begin
      Result:= I;
      Exit;
    end;
end;

function TNotesFileTypeOptionsMan.getFileTypeOptions(
  const FileType: string): TNotesFileTypeOptions;
Var
  I: integer;
begin
  Result:= nil;
  I:= FindFileType(FileType);
  if I < 0 then
    I:= LoadFileType(FileType);
  if I < 0 then
    Exit;
  Result:= (Items[I] As TNotesFileTypeOptions);
end;

procedure TNotesFileTypeOptionsMan.ListStyles(const FileType: string;
  const List: TStrings);
begin
  getFileTypeOptions(FileType).Styles.listStylesNames(List);
end;

function TNotesFileTypeOptionsMan.LoadFileType(
  const FileType: string): integer;
begin
  Result:= Add( TNotesFileTypeOptions.Create(FileType) );
end;

procedure TNotesFileTypeOptionsMan.SaveAll;
Var
  I: integer;
begin
  for I:= 0 to Count -1 do
    (Items[I] As TNotesFileTypeOptions).Save;
end;

end.
