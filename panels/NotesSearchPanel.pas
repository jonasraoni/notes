//
//    NotesSearchPanel - painel de resultado da busca do Notes.
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
  @abstract(NotesSearchPanel - painel de resultado da busca do Notes.)
  @author(Josimar Silva <josimar_me@yahoo.com.br>)
*)
unit NotesSearchPanel;

interface

uses Classes, NotesPanels, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Graphics, NotesEditorTab, NotesGlobals, Menus, VirtualTrees,
  NotesEditorTabPosList, RegExpr, NotesSeeker, NotesTabsManager;

type
  // Tipo para o evento que ocorre quando um novo resultado é encontrado
  TGrepNewResultEvent= procedure(const etpos: TNotesEditorTabPosList) of object;

  // Lista de resultados
  TGrepList = class(TList)
    procedure Clear; override;
  end;

  // Classe de busca em arquivos
  TGrepThread = class(TThread)
  private
    FPath     : string;
    FText     : string;
    FFilter   : string;
    FOptions  : integer;
    FRegex    : TRegExpr;
    fSeeker   : TNotesSeeker;
    fOnResult: TGrepNewResultEvent;
    procedure ExamineFile(FileName: string);
    procedure DoNewResult;
  public
    procedure Execute; override;
    property Path: string read FPath write FPath;
    property Text: string read FText write FText;
    property Filter: string read FFilter write FFilter;
    property Options: integer read FOptions write FOptions;
    property OnResult: TGrepNewResultEvent read fOnResult write fOnResult;
  end;


type
  // Painel para busca em diretórios
  TNotesSearchPanel = class(TNotesPanel)
  private
    lvOutput : TVirtualStringTree;
    fPopup   : TPopupMenu;
    fGrep: TGrepThread;
    fRunning: boolean;
    fWildCards: boolean;
    fWholeWords: boolean;
    fRegex: boolean;
    fCaseSensitive: boolean;
    fSearchStr: string;
    fDir: string;
    fWhere: string;
    fRecursive: boolean;
    fFilter: string;

    procedure SearchOpenFile(const FileName: string; const edtab: TNotesEditorTab);
    procedure DoCopyLine(Sender: TObject);
    procedure DoCopyAll(Sender: TObject);
    procedure DoClear(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RunDirSearch;
    procedure RunAllOpenFilesSearch;
    procedure RunProjectSearch;
    procedure RunCurEditorSearch;
    procedure DoStartNewSearch;
    procedure HandleNewResult(const etpos: TNotesEditorTabPosList);
    procedure HandleTerminate(Sender: TObject);
  public
    constructor Create(const StartUpHost: TNotesPanelHost); override;
    destructor  Destroy; override;
    function    getPanelName: string; override;
    procedure   Resize; override;
    procedure   Initialize; override;
    procedure   ReadOptions(const S: string); override;
    procedure   WriteOptions(var  S: string); override;
  published
    // Roda uma das ações do painel:
    // * "search" - executa a busca
    // * "clear" - limpa o conteúdo do painel
    function PanelExec(const PanelAction, Params: string): string; override;
    // Diretório onde a busca será feita
    property DirName: string read fDir write fDir;
    // String a ser buscada
    property SearchStr: string read fSearchStr write fSearchStr;
    // Filtro para busca em diretórios
    property FileFilter: string read fFilter write fFilter;
    // Usar expressões regulares
    property Regexpr: boolean read fRegex write fRegex;
    // Usar coringas
    property WildCards: boolean read fWildCards write fWildCards;
    // Busca sensitiva a caixa
    property CaseSensitive: boolean read fCaseSensitive write fCaseSensitive;
    // Apenas palavras inteiras
    property WholeWords: boolean read fWholeWords write fWholeWords;
    // Onde deve ser feita a busca. Opções: 'dir', 'project', 'openfiles', 'editor'
    property Where: string read fWhere write fWhere;
    // Se a busca deve ser recursiva, isto é, se subdiretórios devem
    // ser pesquisados
    property Recursive: boolean read fRecursive write fRecursive;
    // Retorna true se uma busca está acontecendo
    property Running: boolean read fRunning;
  end;

var
  InfoItens: TNotesEditorTabPosList;
  FFileName : string;

implementation

Uses NotesUtils, Forms, Windows, SysUtils, NotesXML, Dialogs, Clipbrd;

const
  LV_FOLDER_ICO = 19;
  LV_FILE_ICO = 3;
  LV_INFO_ICO = 15;

function RemoveNonPrintableChars(const S: string): string;
begin
  Result:= StringReplace(S, ''#9, '    ', [rfReplaceAll]);
  Result:= StringReplace(Result, ''#10, ' ', [rfReplaceAll]);
  Result:= StringReplace(Result, ''#13, ' ', [rfReplaceAll]);
end;

constructor TNotesSearchPanel.Create(const StartUpHost: TNotesPanelHost);
var
  pi: TMenuItem;
begin
  inherited;

  fRunning:= false;
  fGrep:= nil;
  fWhere:= 'dir';

  lvOutput := TVirtualStringTree.Create(compsOwner);
  lvOutput.Images := NImages;

  With lvOutput do
  begin
    //* Internacionalizar usando getMsgTranslation() *
    Header.Columns.Add.Text := 'Arquivo';
    Header.Columns.Add.Text := 'Linha';
    Header.Columns.Add.Text := 'Coluna';

    Header.AutoSizeIndex := 0;
    Header.MainColumn := -1;
    Header.Options := [hoAutoResize, hoColumnResize, hoDrag, hoShowImages, hoVisible];
    TabOrder := 0;
    HintAnimation := hatNone;
    TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes];
    TreeOptions.SelectionOptions := [toFullRowSelect];

    OnDblClick := TreeDblClick;
    OnGetText  := TreeGetText;
    OnFreeNode := TreeFreeNode;
    OnGetImageIndex := TreeGetImageIndex;

    Parent:= self.Parent;
  end;

  fPopup:= TPopupMenu.Create(compsOwner);
  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyLine';
  pi.Caption:= '&Copiar linha';
  pi.OnClick:= DoCopyLine;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyAllLines';
  pi.Caption:= 'Copiar &todas as linhas';
  pi.OnClick:= DoCopyAll;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnClearAll';
  pi.Caption:= '&Limpar pesquisa';
  pi.OnClick:= DoClear;
  fPopup.Items.Add(pi);

  lvOutput.PopupMenu:= fPopup;
  lvOutput.NodeDataSize := Sizeof(TNotesEditorTabPos);
end;

destructor TNotesSearchPanel.Destroy;
begin
  inherited;
end;

procedure TNotesSearchPanel.DoClear(Sender: TObject);
begin
  lvOutput.Clear;
end;

procedure TNotesSearchPanel.DoCopyAll(Sender: TObject);
Var
  S    : string;
  Info : PTNotesEditorTabPos;
  Node : PVirtualNode;
  I    : integer;
begin
  S := '';
  lvoutput.ContentToClipboard(CF_TEXT, tstAll);
  Node := lvOutput.GetFirst;
  for I := 0 to lvOutput.TotalCount -1 do
  begin
    Info := lvOutput.GetNodeData(Node);

    if Node.ChildCount > 0 then
      S := S + Info.FileName + ' (' + IntToStr(Node.ChildCount) + ')'
    else
      S := S + Info.FileName + ',' + Info.Info + ',' + IntToStr(Info.Line) + ',' +  IntToStr(Info.Col);

    S := S + #13;
    Node := lvOutput.GetNext(Node);
  end;
  Clipboard.SetTextBuf(PChar(S));
end;

procedure TNotesSearchPanel.DoCopyLine(Sender: TObject);
Var
  S    : string;
  Info : PTNotesEditorTabPos;
  Node : PVirtualNode;
begin
  Node := lvOutput.GetFirstSelected;
  Info := lvOutput.GetNodeData(Node);

  if Assigned(node) then
  begin
    if Node.ChildCount > 0 then
      S := S + Info.FileName + ' (' + IntToStr(Node.ChildCount) + ')'
    else
      S := S + Info.FileName + ',' + Info.Info + ',' + IntToStr(Info.Line) + ',' +  IntToStr(Info.Col);

    Clipboard.SetTextBuf(PChar(S));
  end else
    setStatusMsg('Nenhuma linha foi selecionada para cópia !');
end;

procedure TNotesSearchPanel.TreeDblClick(Sender: TObject);
var
  Info : PTNotesEditorTabPos;
  Node : PVirtualNode;
  etp  : TNotesEditorTabPos;
begin
  Node := lvOutput.GetFirstSelected;
  Info := lvOutput.GetNodeData(Node);

  etp.Col      := Info.Col;
  etp.FileName := Info.FileName;
  etp.Line     := Info.Line;

  GoToEditorTabPos(etp, false);
end;

function TNotesSearchPanel.getPanelName: string;
begin
  Result:= 'Search';
end;

procedure TNotesSearchPanel.Initialize;
begin
  inherited;
end;

function TNotesSearchPanel.PanelExec(const PanelAction, Params: string): string;
begin
    // * "search" - executa a busca
    // * "clear" - limpa o conteúdo do painel
  if SameText(PanelAction, 'search') then
  begin
    if SameText(fWhere, 'dir') then
      RunDirSearch
    else if SameText(fWhere, 'openfiles') then
      RunAllOpenFilesSearch
    else if SameText(fWhere, 'project') then
      RunProjectSearch
    else
      RunCurEditorSearch;
  end else
  if SameText(PanelAction, 'clear') then
  begin
    DoClear(nil);
  end;
end;

procedure TNotesSearchPanel.ReadOptions(const S: string);
begin
  inherited;
end;

procedure TNotesSearchPanel.Resize;
begin
  inherited;
  lvOutput.Left:= 2;
  lvOutput.Top:= 2;
  lvOutput.Width:= lvOutput.Parent.Width - 4;
  lvOutput.Height:= lvOutput.Parent.Height - 4;
end;

procedure TNotesSearchPanel.HandleNewResult(const etpos: TNotesEditorTabPosList);
var
  I     : integer;
  FNode : PVirtualNode;
  LNode : PVirtualNode;
  VInfo : PTNotesEditorTabPos;
  Temp  : TNotesEditorTabPos;
begin
  lvOutput.BeginUpdate;
  try
    if etpos.Count = 0 then
      exit;

    FNode := lvOutput.AddChild(nil);
    VInfo  := lvOutput.GetNodeData(FNode);

    VInfo.FileName := etpos.Items[0].FileName;
    VInfo.Line     := -1;

    for I := 0 to etpos.Count - 1 do
    begin
      Temp := etpos.Items[I];

      LNode := lvOutput.AddChild(FNode);
      VInfo := lvOutput.GetNodeData(LNode);

      VInfo.Info      := Temp.Info;
      VInfo.FileName    := Temp.FileName;
      VInfo.Line        := Temp.Line;
      VInfo.Col         := Temp.Col;
    end;
  finally
    lvOutput.EndUpdate;
  end;
end;

procedure TNotesSearchPanel.WriteOptions(var S: string);
begin
  inherited;
end;

procedure TNotesSearchPanel.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Info : PTNotesEditorTabPos;
begin
  if TextType = ttNormal then
    begin
      Info := lvOutput.GetNodeData(Node);

      if Node.ChildCount > 0 then
      begin
          if Column = 0 then
            CellText := Info.Filename + ' (' + IntToStr(Node.ChildCount) + ')'
          else
            CellText := '';
      end else
      begin
        if Info.FileName <> '' then
        begin
          case Column of
            0: CellText := Info.Info;
            // 1: CellText := Info.Info;
            1: CellText := IntToStr(Info.Line);
            2: CellText := IntToStr(Info.Col);
          end;
        end else
        begin
          if Column = 0 then
            CellText:= Info.Info
          else
            CellText:= '';
        end;
      end;
    end
  else
    CellText := '';
end;

procedure TNotesSearchPanel.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Info : PTNotesEditorTabPos;
begin
  Info          := lvOutput.GetNodeData(Node);
  Info.Filename := ''
end;

procedure TGrepThread.Execute;
var
  I    : integer;
  Attr : integer;
  SR   : TSearchRec;
  Dirs : TStringList;
  Mask : TStringList;
  CurPath: string;
begin
  Attr  := $00000001;
  Dirs  := TStringList.Create;
  Mask  := SplitFilter(FFilter);
  FRegex:= nil;

  if Not Assigned(fSeeker) then
    fSeeker:= TNotesSeeker.Create;

  try
    Dirs.Add(FPath);

    if (FOptions and NOTES_SEARCH_SUBDIRS) <> 0 then
      Attr := Attr or faDirectory;

    if (FOptions and NOTES_SEARCH_REGEXP) <> 0 then
    begin
      FRegex := TRegExpr.Create;
      FRegex.Expression := FText;

      if (FOptions and NOTES_SEARCH_CASESENSITIVE) = 0 then
        FRegex.ModifierI := true;
    end;

    while (not Terminated) and (Dirs.Count > 0) do
    begin
      CurPath := AddSlash(Dirs[0]);
      Dirs.Delete(0);

      if FindFirst(CurPath + '*', Attr, SR) = 0 then
      begin
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
            if (SR.Attr and faDirectory) <> 0 then
              Dirs.Add(CurPath + SR.Name)
            else
              begin
                if Mask.Count = 0 then
                  ExamineFile(CurPath + SR.Name)
                else
                  for I := 0 to Mask.Count - 1 do
                    if WildcardCompare(Mask[I], SR.Name) then
                    begin
                      ExamineFile(CurPath + SR.Name);
                      Break;
                    end;
              end;
          end;
        if not Terminated and (InfoItens.Count > 0) then
        begin
          Synchronize(DoNewResult);
          InfoItens.Clear;
        end;
        until FindNext(SR) <> 0;
      end;

      FindClose(SR);
    end;
  finally
    if Assigned(FRegex) then
      FRegex.Free;
    if Assigned(FSeeker) then
      FreeAndNil(FSeeker);

    Mask.Free;
    Dirs.Free;
  end;
end;

procedure TGrepThread.DoNewResult;
begin
  If (Assigned(fOnResult)) and (InfoItens.Count > 0) then
    fOnResult(InfoItens);
end;

procedure TGrepThread.ExamineFile(FileName: String);
var
  Lin, Col : integer;
  TheText  : TStrings;

  procedure AddInfo(Txt: string);
  begin
    InfoItens.Add(Lin, Col, Filename, RemoveNonPrintableChars(Txt));
  end;

begin
  FFileName := FileName;
  setStatusMsg('Buscando em "' +  FFileName + '"...');

  TheText := nil;

  try
    if (FOptions and NOTES_SEARCH_REGEXP) <> 0 then
    begin
      TheText := TStringList.Create;
      TheText.LoadFromFile (FFileName);
      if FRegex.Exec(TheText.Text) then
      begin
        Col := FRegex.MatchPos[0];
        AddInfo(FRegex.Match[0]);
        while FRegex.ExecNext do
        begin
          Col := FRegex.MatchPos[0];
          AddInfo(FRegex.Match[0]);
        end;
      end;
    end
    else
    begin
      fseeker.Text:= FileToStr(filename);
      fSeeker.StartAt := 0;
      fseeker.SearchStr := FText;
      fseeker.EnableOptions((NotesSearchOptions and NOTES_SEARCH_CASESENSITIVE) <> 0,
                            (NotesSearchOptions and NOTES_SEARCH_WHOLEWORDS) <> 0,
                            true,
                            (NotesSearchOptions and NOTES_SEARCH_WILDCARDS) <> 0,
                            false);
      fseeker.StartSearch;
      while fSeeker.Search do
      begin;
        Lin := fseeker.CurLine + 1;
        Col := fseeker.CurCol + 1;
        AddInfo(fSeeker.Context);
      end;
    end;
  finally
    TheText.Free;
  end;
end;

procedure TGrepList.Clear;
begin
  InfoItens.Clear;
  inherited;
end;

procedure TNotesSearchPanel.RunDirSearch;
begin
  if fRunning then Exit;
  fRunning:= true;
  DoStartNewSearch;

  fGrep:= TGrepThread.Create(true);
  // Não precisamos nos preocupar com a liberação dos recursos!
  fGrep.FreeOnTerminate:= true;
  fGrep.Path:= fDir;
  fGrep.Text:= fSearchStr;
  fGrep.Filter:= fFilter;
  fGrep.OnResult:= HandleNewResult;
  fGrep.OnTerminate:= HandleTerminate;

  fGrep.Options:= 0;
  if fWholeWords then fGrep.Options := fGrep.Options or NOTES_SEARCH_WHOLEWORDS;
  if fWildCards then fGrep.Options := fGrep.Options or NOTES_SEARCH_WILDCARDS;
  if fCaseSensitive then fGrep.Options := fGrep.Options or NOTES_SEARCH_CASESENSITIVE;
  if fRegex then fGrep.Options := fGrep.Options or NOTES_SEARCH_REGEXP;
  if fRecursive then fGrep.Options := fGrep.Options or NOTES_SEARCH_SUBDIRS;

  NotesSearchOptions:= fGrep.Options;
  // Executa a busca
  fGrep.Resume;
end;

procedure TNotesSearchPanel.HandleTerminate(Sender: TObject);
begin
  fRunning:= false;
  setStatusMsg('Pesquisa completa!');
end;

procedure TNotesSearchPanel.RunAllOpenFilesSearch;
Var
  I: integer;
begin
  if fRunning then Exit;
  InfoItens.Clear;
  fRunning:= true;
  DoStartNewSearch;
  try
    for I:= 0 to tabs.EditorsTabCount -1 do
    begin
      if tabs.EditorsTab[I].FullPath <> '' then
        SearchOpenFile(tabs.EditorsTab[I].FullPath, tabs.EditorsTab[I])
      else
        SearchOpenFile(tabs.EditorsTab[I].Tab.Caption, tabs.EditorsTab[I]);

      HandleNewResult(InfoItens);
      InfoItens.Clear;
    end;
  finally
    fRunning:= false;
  end;
end;

procedure TNotesSearchPanel.RunProjectSearch;
begin
  if fRunning then Exit;
  fRunning:= true;
  //
  fRunning:= false;
end;

procedure TNotesSearchPanel.RunCurEditorSearch;
begin
  if (fRunning) or (ActiveEditorTab = nil) then Exit;
  InfoItens.Clear;
  fRunning:= true;
  DoStartNewSearch;
  try
    if ActiveEditorTab.FullPath <> '' then
      SearchOpenFile(ActiveEditorTab.FullPath, ActiveEditorTab)
    else
      SearchOpenFile(ActiveEditorTab.Tab.Caption, ActiveEditorTab);

    HandleNewResult(InfoItens);
    InfoItens.Clear;
  finally
    fRunning:= false;
  end;
end;

procedure TNotesSearchPanel.SearchOpenFile(const FileName: string; const edtab: TNotesEditorTab);
var
  Lin, Col : integer;
  seeker: TNotesSeeker;
  Regex: TRegExpr;

  procedure AddInfo(Txt: string);
  begin
    InfoItens.Add(Lin, Col, Filename, RemoveNonPrintableChars(Txt));
  end;

begin
  if edtab = nil then Exit;

  fFileName := FileName;
  if fRegex then
  begin
    Regex := TRegExpr.Create;
    try
      Regex.Expression := fSearchStr;

      if not fCaseSensitive then
        Regex.ModifierI := true;

      if Regex.Exec(edtab.Editor.Text) then
      begin
        Lin := 0;
        Col := Regex.MatchPos[0];
        AddInfo(Regex.Match[0]);
        while Regex.ExecNext do
        begin
          Application.ProcessMessages;
          Col := Regex.MatchPos[0];
          AddInfo(Regex.Match[0]);
        end;
      end;
    finally
      Regex.Free;
    end;
  end
  else
  begin

    Seeker:= TNotesSeeker.Create;
    try
      Seeker.Text:= edtab.Editor.Text;
      Seeker.StartAt := 0;
      seeker.SearchStr := fSearchStr;
      seeker.EnableOptions(fCaseSensitive, fWholeWords, true, fWildCards, false);

      seeker.StartSearch;
      while Seeker.Search do
      begin
        Application.ProcessMessages;
        Lin := seeker.CurLine + 1;
        Col := seeker.CurCol + 1;
        AddInfo(Seeker.Context);
      end;
    finally
      seeker.Free;
    end;
  end;
end;

procedure TNotesSearchPanel.DoStartNewSearch;
var
  FNode : PVirtualNode;
  VInfo : PTNotesEditorTabPos;
begin
  lvOutput.BeginUpdate;
  try
    FNode := lvOutput.AddChild(nil);
    VInfo  := lvOutput.GetNodeData(FNode);

    VInfo.FileName := '';
    VInfo.Info:= 'Resultados da busca por "' + fSearchStr + '"';
    VInfo.Line:= -1;
  finally
    lvOutput.EndUpdate;
  end;

  Show;
end;

procedure TNotesSearchPanel.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  VInfo : PTNotesEditorTabPos;
begin
  ImageIndex := -1;

  if (Column = 0) and (Kind in [ikNormal, ikSelected]) then
  begin
    if (Node.ChildCount > 0) then
      ImageIndex := LV_FOLDER_ICO
    else
    begin
      VInfo:= lvOutput.GetNodeData(Node);

      if VInfo.FileName <> '' then
        ImageIndex := LV_FILE_ICO
      else
        ImageIndex := LV_INFO_ICO
    end;
  end;
end;

initialization
  InfoItens := TNotesEditorTabPosList.Create;
  PanelReg.registerPanel(TNotesSearchPanel);

finalization
  InfoItens.Free;

end.
