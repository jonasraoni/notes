//
//    NotesOutputPanel - painel "saída" do Notes.
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
  @abstract(NotesOutputPanel - painel "saída" do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesOutputPanel;

interface

uses Classes, NotesPanels, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Graphics, NotesEditorTab, NotesGlobals, Menus, NotesProfile, DosCommand;

type

  TNotesCaptureTo = (ctOutput, ctEditor);

  TNotesOutputPanel = class(TNotesPanel)
  private
    fRunning: boolean;
    fDosCmd: TDosCommand;
    fValidFiles: string;
    fCaptureTo: TNotesCaptureTo;

    fAutoParse: boolean;
    fArgs: string;
    fRE: string;
    fDir: string;
    fFileToRun: string;
    fFilePos: string;
    fLinePos: string;
    fScanProblems: boolean;

    lbOutput: TListBox;
    fPopup: TPopupMenu;
    fpoAutoOpen: TMenuItem;
    fpoAutoGoToFirtError: TMenuItem;
    // UserOptions
    fAutoOpen: boolean;
    fAutoGoToFirstError: boolean;
    // menu
    procedure DoGoToError(Sender: TObject);
    procedure DoCopyLine(Sender: TObject);
    procedure DoCopyAll(Sender: TObject);
    procedure DoClear(Sender: TObject);
    procedure DoOptAutoOpen(Sender: TObject);
    procedure DoOptAutoGoToFirstError(Sender: TObject);
    procedure DoOnPopup(Sender: TObject);
    // DOSCOMMAND
    procedure NewOutputLine(Sender: TObject; NewLine: string; OutputType: TOutputType);
    procedure OutputEnd(Sender: TObject; const ExitCode: LongWord);
    function  getCaptureTo: string;
    procedure Run;
    procedure setCaptureTo(const Value: string);
    procedure scanOutputForProblems;
  public
    constructor Create(const StartUpHost: TNotesPanelHost); override;
    destructor  Destroy; override;
    procedure  Resize; override;
    procedure  Initialize; override;
    function   getPanelName: string; override;
    procedure  ReadOptions(const S: string); override;
    procedure  WriteOptions(var  S: string); override;
  published
    // Roda uma das ações do painel:
    // * "run" - executa o arquivo especificado em "FileToRun" com as configurações atuais
    // * "getoutput" - retorna o conteúdo do painel saída
    // * "add" - adiciona a string passada como parâmetro a saída
    // * "scanproblems" - faz o painel buscar erros (de compilação) na saída e
    //    reportar eles ao Notes. O painel faz isto automaticamente quando
    //    "AutoScanForProblems" está ativado.
    // * "clear" - limpa o conteúdo do painel
    // * "stop" - tenta parar o programa que esta sendo executado
    function  PanelExec(const PanelAction, Params: string): string; override;
    // Retorna "True" se o painel estiver executando algum programa.
    property  Running: boolean read fRunning;
    // Especifica para onde a saída do arquivo executado deve ser capturada.
    // Usando "outputwindow" captura no próprio painel de saída.
    // Se setado para "editor" captura no editor.
    property  CaptureTo: string read getCaptureTo write setCaptureTo;
    // Arquivo que o painel deve rodar e capturar a saída
    property  FileToRun: string read fFileToRun write fFileToRun;
    // Argumentos que serão passados ao arquivo especificado em "FileToRun"
    property  Arguments: string read fArgs write fArgs;
    // Permite setar o diretório de trabalho do sistema operacional antes
    // de executar o arquivo especificado em "FileToRun"
    property  WorkDir: string read fDir write fDir;
    // Indica se o parser automático de erros de compilação deve ser usado
    property  ErrorsAutoParse: boolean read fAutoParse write fAutoParse;
    // Expressão regular usada para interpretar a saída de um compilador
    // quando a opção "ErrorsAutoParse" está desligada.
    property  ErrorsRegexParser: string read fRE write fRE;
    // Posição em que a expressão regular retornará o nome do arquivo com erro
    property  ErrorsReFilePos: string read fFilePos write fFilePos;
    // Posição em que a expressão regular retornará a linha
    property  ErrorsReLinePos: string read fLinePos write fLinePos;
    // String como uma lista de arquivos em cada linha. Usada quando arquivos
    // com caminhos relativos e/ou incompletos são reportados como tendo erros
    // pelo compilador para completar o nome do arquivo. Em geral setado para
    // os arquivos abertoso no Notes e os arquivos do projeto atual.
    property  ErrorsValidFiles: string read fValidFiles write fValidFiles;
    // Se setado para true, busca por erros na saída e preenche a lista de
    // problemas (aquela que mostra os ícones de erro na gutter) com o que encontrar.
    property  AutoScanForProblems: boolean read fScanProblems write fScanProblems;
  end;

implementation

uses Windows, ShellApi, Clipbrd, NotesListEditor, NotesXML, SysUtils,
  NotesOutputParser, NotesUtils, FastStrings, NotesEditorTabPosList, Forms;

{ TNotesOutputPanel }

constructor TNotesOutputPanel.Create;
var
  pi: TMenuItem;
begin
  inherited;

  lbOutput:= TListBox.Create(compsOwner);
  lbOutput.OnDblClick:= DoGoToError;

  lbOutput.Parent:= parent;
  fPopup:= TPopupMenu.Create(compsOwner);
  lbOutput.Parent:= self.Parent;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnGotoLine';
  pi.Caption:= '&Ir para o erro';
  pi.OnClick:= DoGoToError;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyLine';
  pi.Caption:= '&Copiar mensagem';
  pi.OnClick:= DoCopyLine;
  fPopup.Items.Add(pi);

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnCopyAllLines';
  pi.Caption:= 'Copiar &todas as mensagens';
  pi.OnClick:= DoCopyAll;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnClearAll';
  pi.Caption:= '&Limpar mensagens';
  pi.OnClick:= DoClear;
  fPopup.Items.Add(pi);

  fPopup.Items.NewBottomLine;

  // submenu OPÇÕES
  pi:= TMenuItem.Create(compsOwner);
  pi.Name:= 'mnOptions';
  pi.Caption:= '&Opções';

  fpoAutoGoToFirtError:= TMenuItem.Create(compsOwner);
  fpoAutoGoToFirtError.Name:= 'mnOptionsGotoFirstError';
  fpoAutoGoToFirtError.Caption:= '&Ir para o primeiro erro';
  fpoAutoGoToFirtError.OnClick:= DoOptAutoGoToFirstError;
  pi.Add(fpoAutoGoToFirtError);

  fpoAutoOpen:= TMenuItem.Create(compsOwner);
  fpoAutoOpen.Name:= 'mnOptionsAutoOpen';
  fpoAutoOpen.Caption:= '&Abrir painel automaticamente';
  fpoAutoOpen.OnClick:= DoOptAutoOpen;
  pi.Add(fpoAutoOpen);

  fPopup.Items.Add(pi);

  fPopup.OnPopup:= DoOnPopup;

  lbOutput.PopupMenu:= fPopup;
end;

destructor TNotesOutputPanel.Destroy;
begin
  if Assigned(fDosCmd) then
  begin
    if fRunning then
    begin
      fDosCmd.Stop;
      // Damos tempo para o processo terminar
      Sleep(1000);
    end;
    fDosCmd.Free;
  end;
  inherited;
end;

procedure TNotesOutputPanel.Initialize;
begin
  inherited;
  //
end;

procedure TNotesOutputPanel.Resize;
begin
  inherited;
  lbOutput.Left:= 2;
  lbOutput.Top:= 2;
  lbOutput.Width:= lbOutput.Parent.Width - 4;
  lbOutput.Height:= lbOutput.Parent.Height - 4;
end;


procedure TNotesOutputPanel.ReadOptions(const S: string);
begin
  inherited;
  fAutoOpen:= ReadTagBool(S, 'AutoOpen', true);
  fAutoGoToFirstError:= ReadTagBool(S, 'AutoGoToFirstError', true);
end;


procedure TNotesOutputPanel.WriteOptions(var S: string);
begin
  inherited;
  WriteTagBool(@S, 'AutoOpen', fAutoOpen);
  WriteTagBool(@S, 'AutoGoToFirstError', fAutoGoToFirstError);
end;

// MENU

procedure TNotesOutputPanel.DoClear(Sender: TObject);
begin
  lbOutput.Clear;
end;

procedure TNotesOutputPanel.DoCopyAll(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(lbOutput.Items.text));
end;

procedure TNotesOutputPanel.DoCopyLine(Sender: TObject);
begin
  if lbOutput.ItemIndex > -1 then
    Clipboard.SetTextBuf(PChar(lbOutput.Items[lbOutput.itemIndex]));
end;

procedure TNotesOutputPanel.DoGoToError(Sender: TObject);
Var
  etp: TNotesEditorTabPos;
begin
  if lbOutPut.ItemIndex < 0 then Exit;
  // temos q resetar as propriedas - outro componente pode ter modificado tudo
  outputParser.Auto:= fAutoParse;
  outputParser.Regex:= fRE;
  outputParser.RELinePos:= fLinePos;
  outputParser.REFilePos:= fFilePos;
  outputParser.ValidFiles:= fValidFiles;
  outputParser.ErrorStr:= lbOutPut.Items.Strings[lbOutPut.ItemIndex];
  outputParser.Parse;

  etp.Line:= OutputParser.Line;
  etp.FileName:= OutputParser.FileName;

  GoToEditorTabPos(etp, true);
end;


procedure TNotesOutputPanel.DoOptAutoGoToFirstError(Sender: TObject);
begin
  fAutoGoToFirstError:= not fAutoGoToFirstError;
end;

procedure TNotesOutputPanel.DoOptAutoOpen(Sender: TObject);
begin
  fAutoOpen:= not fAutoOpen;
end;

procedure TNotesOutputPanel.DoOnPopup(Sender: TObject);
begin
  fpoAutoOpen.Checked:= fAutoOpen;
  fpoAutoGoToFirtError.Checked:= fAutogotofirstError;
end;

procedure TNotesOutputPanel.NewOutputLine(Sender: TObject; NewLine: string;
  OutputType: TOutputType);
begin
  if OutputType = otEntireLine then
  begin
    if fCaptureTo = ctOutput then
    begin
      lbOutput.Items.Add(StringReplace(NewLine, #9'', '    ', [rfReplaceAll]));
    end else
    begin
      if ActiveEditorTab <> nil then
        ActiveEditorTab.Editor.SelText:= NewLine + ASCII_CRLF;
    end;
  end;
end;

procedure TNotesOutputPanel.OutputEnd(Sender: TObject; const ExitCode: LongWord);
begin

  if fCaptureTo = ctOutput then
  begin
    lbOutput.Items.Add(' ');
    lbOutput.Items.Add('--');
    lbOutput.Items.Add( Format(getMsgTranslation('MsgProcessFinished', 'The process finished with exit code %s'), [intToStr(ExitCode)]) );

    if fScanProblems then
      scanOutputForProblems;
  end;

  if fCaptureTo = ctEditor then
  begin
    if ActiveEditorTab <> nil then
      ActiveEditorTab.Editor.EndUpdate;
  end;

  fRunning:= false;
end;

procedure TNotesOutputPanel.Run;
begin
  if fRunning then Exit;
  fRunning:= true;

  if Not Assigned(fDosCmd) then
  begin
    fDoscmd:= TDosCommand.Create(nil);
    fDosCmd.MaxTimeAfterLastOutput:= 70;
    fDosCmd.OnNewLine:= NewOutputLine;
    fDosCmd.OnTerminated:= OutputEnd;
  end;

  fDosCmd.CommandLine:= fFileToRun + ' ' + fArgs;

  if fCaptureTo = ctOutput then
  begin
    if fAutoOpen then
      Show;

    outputParser.Auto:= fAutoParse;
    outputParser.ValidFiles:= fValidFiles;
    outputParser.Regex:= fRE;
    outputParser.RELinePos:= fLinePos;
    outputParser.REFilePos:= fFilePos;

    lbOutput.Items.Add(Format(getMsgTranslation('MsgRunning', 'Running %s...'), [fDosCmd.CommandLine]));
    lbOutput.Items.Add(' ');
  end;

    if fDir <> '' then
      SetCurrentDir(fDir);
    fDosCmd.Execute;
end;

function TNotesOutputPanel.getPanelName: string;
begin
  Result:= 'Output';
end;

function TNotesOutputPanel.PanelExec(const PanelAction,
  Params: string): string;
begin
  Result:='';

  if SameText('Run', PanelAction) then
  begin
    Run;
  end else if SameText('getoutput', PanelAction) then
  begin
    Result:= lbOutput.Items.Text;
  end else if SameText('add', PanelAction) then
  begin
    lbOutput.Items.Add(Params);
  end else if SameText('scanproblems', PanelAction) then
  begin
    scanOutputForProblems;
  end else if SameText('clear', PanelAction) then
  begin
    lbOutput.Clear;
  end else if SameText('stop', PanelAction) then
  begin
    if (fRunning) and (assigned(fDosCmd)) then
      fDosCmd.Stop;
  end;
end;

function TNotesOutputPanel.getCaptureTo: string;
begin
  if fCaptureTo = ctOutput then
    Result:= 'outputwindow'
  else
    Result:= 'editor';
end;

procedure TNotesOutputPanel.setCaptureTo(const Value: string);
begin
  if SameText('editor', Value) then
    fCaptureTo:= ctEditor
  else
    fCaptureTo:= ctOutput;
end;

procedure TNotesOutputPanel.scanOutputForProblems;
Var
  I: integer;
begin
  ProblemsList.Clear;

  outputParser.Auto:= fAutoParse;
  outputParser.ValidFiles:= fValidFiles;
  outputParser.Regex:= fRE;
  outputParser.RELinePos:= fLinePos;
  outputParser.REFilePos:= fFilePos;

  // o Notes insere 4 linhas, então esperamos
  // q a saída tenha mais de 4 linhas
  if lboutput.Items.Count > 4 then
  begin
    for I:= 2 to lbOutput.Items.Count - 3 do
    begin

      outputParser.ErrorStr:= lbOutput.Items.Strings[I];
      outputParser.Parse;
      if (outputParser.FileName <> '') and (outputParser.Line > -1) then
        ProblemsList.Add(outPutParser.line, 0, outPutParser.FileName, outputParser.ErrorStr)
      else if (outputParser.Line > -1) and (outputParser.FileName = '') and (ActiveEditorTab <> nil) and (acTiveEditorTab.FullPath <> '') then
        ProblemsList.Add(outPutParser.line, 0, ActiveEditorTab.FullPath, outputParser.ErrorStr)
      else if (length(outputParser.ErrorStr) > 0 ) and( ProblemsList.Count > 0 ) then
        ProblemsList.AddInfo(ProblemsList.Count-1, #13 + outputParser.ErrorStr);

    end;

  end;

  if ActiveEditorTab <> nil then
    ActiveEditorTab.Editor.InvalidateGutter;

  if (ProblemsList.Count > 0) and (fAutoGotoFirstError) then
    ProblemsList.gotoNext;
end;

initialization
  PanelReg.registerPanel(TNotesOutputPanel);

end.
