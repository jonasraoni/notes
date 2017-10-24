//
//    NotesPanels - sistema de painéis do Notes.
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
  @abstract(NotesPanels - sistema de painéis do Notes.)
  @author(Anderson R. Barbieri <notesnr@ig.com.br>)
*)
unit NotesPanels;

interface

uses Classes, Windows, ComCtrls, NotesGlobals, SysUtils,
     ExtCtrls, Graphics, Controls, Forms, StdCtrls,
     Menus, Messages, NotesTranslation;

type

  // Componente estilo Splitter usado nos painéis do Notes
  TNotesSplitter = Class(TPanel)
  private
    fMouseOnButton: boolean;
    fDownPos: TPoint;
    fMoving: boolean;
    // fMouseMovesCount guarda o número de vezes que
    // a mensagem para mover o spliter foi recebida.
    // Como recebemos muitas mensagens, só processmos
    // ela depois de recebê-la quatro vezes. Com isso
    // temos menos flicker ao fazer o resize dos controles...
    fMouseMovesCount: integer;
    fNotCollapsedSize: integer;
    function GetCollapsed: Boolean;
    procedure setCollapsed(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function  IsMouseOverBtn(X, Y: integer): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    // Se o painel está aberto ou fechado
    property Collapsed: Boolean read getCollapsed write setCollapsed;
    property Align;
    property Color;
    property Cursor;
    property Visible;
    property Height;
    property Width;
  end;


Type
  TNotesPanelHostType = (htLeft, htRight, htBottom);

  TNotesPanelHost = class;
  TNotesPanelManager = class;

  // Classe de onde novos painéis devem derivar
  TNotesPanel = class(TPersistent)
  private
    fHost: TNotesPanelHost;
    fManager: TNotesPanelManager;
    fTranslate: TNotesTranslation;
    fTab: TTabSheet;
    fOwner: TComponent;
    function getParent: TWinControl;
  protected
    // Modificado para True depois que o painel foi inicializado
    fInitialized: boolean;
    // Retorna a tradução para uma mensagem. Você deve escolher um nome
    // para uma mensagem, por exemplo "PanelError". Então chame
    // getMsgTranslation('PanelError', 'Erro in myPanel') e ela
    // retornará a mensagem traduzida ou o default da mensagem se
    // uma tradução não for encontrada. ATENÇÃO: os controles são
    // traduzidos automagicamente pelo sistema de painéis, você
    // apenas precisa traduzir mensagens que serão usada me diálogos
    // como o messageBox, inputBox, etc.
    function getMsgTranslation(const MsgName, MsgDefault: string): string;
  public
    constructor Create(const StartUpHost: TNotesPanelHost); reintroduce; virtual;
    destructor  Destroy; override;
    // Chamado após a criação do painel para fazer tarefas como a tradução
    // dos controles. Vide também o procedimento Initialize.
    procedure AfterPanelCreate(const Manager: TNotesPanelManager); virtual;
    // Você deve escrever código para fazer o resize dos componentes do painel
    // Evite usar a propriedade Align para isto. O Notes chama o procedimento
    // Resize de forma inteligente, aumentando visivelmente a performance do painel.
    // Além disto muitas vezes a propriedade Align acaba não funcionando como deveria
    // devido a complexidade do sistema de painéis.
    procedure Resize; Virtual; Abstract;
    // Deve retornar o nome do painel. O nome de cada painel deve ser único, pois
    // ele será usado para identificar em todas as chamadas do TNotesPanelsManager
    function getPanelName: string;  Virtual; Abstract;
    // O Notes chama este procedimento para inciar o painel. É uma boa hora
    // para carregar informações lentas (exemplo: alimentar um treeview
    // com os arquivos de um diertório). Quando ele é chamado, o painel já
    // está visivel para o usuário.
    procedure Initialize; Virtual;
    // O painel pode ler as suas opções usando os procedimentos da unit NotesXML
    procedure ReadOptions(const S: string); virtual;
    // O painel pode escrever as suas opções usando os procedimentos da unit NotesXML
    procedure WriteOptions(var  S: string); virtual;
    // Mostra o painel
    procedure Show;
    // Esconde o painel
    procedure Hide;
    // Parent que deve ser usado pelos controles criados pelo painel
    property Parent: TWinControl read getParent;
    // Host atual do painel
    property Host: TNotesPanelHost read fHost write fHost;
    // Gerenciador dos painéis
    property Manager: TNotesPanelManager read fManager;
    // Componente que DEVE ser usado como Owner de todos os controles
    property CompsOwner: TComponent read fOwner;
  published
    // Maneira pela qual o Notes invoca ações do painel. Crie
    // propriedades no campo published se as ações dependem de vários
    // parâmetros. O gerenciador de painéis provê meio de setar as
    // propriedades publicadas de forma fácil :)
    function PanelExec(const PanelAction, Params: string): string; virtual; abstract;
  end;

  // Host dos painéis
  TNotesPanelHost = class
  private
    fManager: TNotesPanelManager;
    fName: string;
    fPopup: TPopupMenu;
    fHCaption: string;
    procedure DoPgsResize(Sender: TObject);
    procedure DoOnPopUp(Sender: TObject);
    procedure DoPopSetLocation(Sender: TObject);
    procedure DoPopSetTab(Sender: TObject);
  protected
    fHostType: TNotesPanelHostType;
    fPanel: TPanel;
    fPageCtrl: TPageControl;
    fSplitter: TNotesSplitter;
  public
    constructor Create(const Name: String; Panel: TPanel; HostType: TNotesPanelHostType);
    destructor Destroy; override;
    // checa a quantidade de painéis e, dependendo do número, fica invisível
    procedure CheckPanelsCount;
    // Recarrega o popup menu
    procedure ReloadPopupMenu;
    // Pagecontrol ao qual está associado
    property PageControl: TPageControl read fPageCtrl write fPageCtrl;
    // Gerenciador de painéis
    property Manager: TNotesPanelManager read fManager write fManager;
    // Nome traduzido do painel
    property HostCaption: string read fHCaption write fHCaption;
    // Nome
    property Name: string read fName write fName;
    // Splitter dos painéis
    property Splitter: TNotesSplitter read fSplitter write fSplitter;
  end;

  // Gerenciador de painéis
  TNotesPanelManager = class
  private
    fPanelsList: TList;
    fHostsList: TList;
    fUpdateCount: integer;
    fDefaultHost: string;
    procedure addPanel(APanel: TNotesPanel);
  public
    constructor Create;
    destructor Destroy; override;
    // Carrega as configurações
    procedure Load;
    // Salva as configurações
    procedure Save;
    // Chamado pelo Notes quando está inicando
    procedure Initialize;
    // Para de atualizar os painéis
    procedure BeginUpdate;
    // Volta a atualizar os painéis
    procedure EndUpdate;
    // Rrtorna true se estiver sendo atualizado
    function  Updating: boolean;
    // Faz o resize de todos os painéis
    procedure ResizePanels;
    // Faz o resize dos painéis do Host
    procedure ResizePanelsOfHost(AHost: TNotesPanelHost);
    // Adiciona um host. Adicione todos os Host antes de criar os painéis
    procedure addPanelHost(const Name: string; Panel: TPanel; HostType: TNotesPanelHostType);
    // Cria os painéis
    procedure createPanels;
    // Pega um painel pelo seu nome
    function  getPanelByName(const Name: string): TNotesPanel;
    function  getPanelByParent(PanelParent: TWinControl): TNotesPanel;
//    function  getPanelTabByName(const Name: string): TTabSheet;
    function  getHostByName(const Name: string): TNotesPanelHost;
    function  getHostByPgCtrl(PgCtrl: TPageControl): TNotesPanelHost ;
    procedure setPanelLocation(const PanelName: string; const HostName: string); overload;
    procedure setPanelLocation(NPanel: TNotesPanel; Host: TNotesPanelHost); overload;
    // Mostra o painel especificado
    procedure ShowPanel(const PanelName: string);
    // Esconde o painel passado
    procedure HidePanel(const PanelName: string);
    // Esconde todos os painéis
    procedure HideAllPanels;
    // Seta o host padrão
    property  DefaultHost: string read fDefaultHost write fDefaultHost;
    // Lista de hosts
    property  HostsList: TList read fHostsList;
  public
    // Executa uma ação
    function PanelExecute(const PanelName, PanelAction, Params: string): string;
    // Seta uma opção
    procedure setPanelProperty(const PanelName, PanelProperty, Value: string);
    // Pega o valor de uma oção
    function  getPanelProperty(const PanelName, PanelProperty: string): string;
  end;

type

  // Metaclasse de TNotesPanel
  TNotesPanelClass = class of TNotesPanel;

  // Registro que guarda as classes dos painéis
  TNotesPanelsRegistry = class(TObject)
  private
    fItems: array of TNotesPanelClass;
    fCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Registra um painel. Ex: registerPanel(TNotesOutputPanel)
    procedure registerPanel(PanelClass: TNotesPanelClass);
    // Retorna a TNotesPanelClass pedida
    function getPanelClass(index: integer): TNotesPanelClass;
    // Quantida de classes registradas
    property Count: integer read fCount;
  end;

Var
  PanelReg: TNotesPanelsRegistry;

implementation

uses NotesUtils, NotesXML, typInfo;

const
  // groupindex dos items do popup para os Hosts
  HP_HOST_GROUPINDEX = 20;
  // groupindex dos items do popup para as Tabs
  HP_TAB_GROUPINDEX = 30;

// constantes da splitter:
const
  // flecha voltada para cima
  ARROW_TO_TOP = 1;
  // flecha voltada para baixo
  ARROW_TO_BOTTOM = 2;
  // flecha voltada para esquerda
  ARROW_TO_LEFT = 3;
  // flecha voltada para a direita
  ARROW_TO_RIGHT = 4;

{ TNotesPanel }

procedure TNotesPanel.AfterPanelCreate(const Manager: TNotesPanelManager);
begin
  fManager:= Manager;
  fTranslate.TranslateComponent(compsOwner);
end;

constructor TNotesPanel.Create(const StartUpHost: TNotesPanelHost);
begin
  fOwner:= TComponent.Create(nil);
  fTranslate:= TNotesTranslation.Create;
  fTranslate.TranslationFile:= nProfile.getTranslationFileForModule(self.ClassName);
  fInitialized:= false;
  fTab:= TTabSheet.Create(CompsOwner);
  if not Assigned(StartUpHost) then
    raise Exception.Create(ClassName+ '.Create - pass a valid StartUpHost!');
  fTab.Parent:= StartUpHost.PageControl;
  fTab.PageControl:= StartUpHost.PageControl;;
  fHost:= StartUpHost;
  fTab.Name:= 'PanelTab';
  fTab.Caption:= getPanelName;
end;

destructor TNotesPanel.Destroy;
begin
  if assigned(fOwner) then
    FreeAndNil(FOwner);
  if Assigned(fTranslate) then
    fTranslate.Free;
end;

function TNotesPanel.getMsgTranslation(const MsgName,
  MsgDefault: string): string;
begin
  Result:= fTranslate.getMsgTranslation( MsgName, MsgDefault );
end;

function TNotesPanel.getParent: TWinControl;
begin
  Result:= fTab as TWinControl;
end;

procedure TNotesPanel.Hide;
begin
  fManager.HidePanel(getPanelName);
end;

procedure TNotesPanel.Initialize;
begin
  fInitialized:= true;
end;

procedure TNotesPanel.ReadOptions(const S: string);
begin
//
end;

procedure TNotesPanel.Show;
begin
  fManager.ShowPanel(getPanelName);
end;

procedure TNotesPanel.WriteOptions(var S: string);
begin
//
end;

{ TNotesPanelHost }

procedure TNotesPanelHost.CheckPanelsCount;
begin
  if fPageCtrl.PageCount = 0 then
  begin
    fSplitter.Collapsed:= true;
    fPageCtrl.Parent.Visible:= false;
  end else
  begin
    fPageCtrl.Parent.Visible:= true;
    if not Splitter.Collapsed then
      Manager.ResizePanelsOfHost(self);
  end;
end;

constructor TNotesPanelHost.Create(const Name: String; Panel: TPanel;
  HostType: TNotesPanelHostType);
begin
  fHostType:= HostType;
  fPanel:= Panel;
  fName:= Name;

  // Spliter
  fSplitter:= TNotesSplitter.Create(fPanel);
  fSplitter.Parent:= fPanel;

  Case fHostType of
    htLeft: fSplitter.Align:= alRight;
    htRight: fSplitter.Align:= alLeft;
    htBottom: fSplitter.Align:= alTop;
  end;

  // PageControl
  fPageCtrl:= TPageControl.Create(fPanel);
  fPageCtrl.ControlStyle:= fPageCtrl.ControlStyle + [csDisplayDragImage];
  fPageCtrl.Parent:= fPanel;
  fPageCtrl.Align:= alClient;

  fPageCtrl.OnResize:= DoPgsResize;
  fPopup:= TPopUpMenu.Create(fPanel);
  fPopup.OnPopUp:= DoOnPopUp;
  fPageCtrl.PopupMenu:= fPopup;
end;

destructor TNotesPanelHost.Destroy;
begin
//
end;

procedure TNotesPanelHost.DoOnPopUp(Sender: TObject);
var
  MousePt : TPoint;
  ClickedTabIndex : integer;
  I: integer;
begin
  // Ativamos a tab em que o usuário clicou
  GetCursorPos(MousePt);
  MousePt := fPageCtrl.ScreenToClient(MousePt);
  ClickedTabIndex := fPageCtrl.IndexOfTabAt(MousePT.X, MousePT.Y);
  if ClickedTabIndex > -1 then
    fPageCtrl.ActivePageIndex:= ClickedTabIndex;

  for I := 0 to fPopup.Items.Count - 1 do
  begin
    if fPopup.Items[I].GroupIndex = HP_TAB_GROUPINDEX then
    begin
      if fPopup.Items[I].Tag = fPageCtrl.ActivePageIndex then
        fPopup.Items[I].Checked:= true;
    end else
    if fPopup.Items[I].GroupIndex = HP_HOST_GROUPINDEX then
    begin
      if fPopup.Items[I].Caption = self.Name then
        fPopup.Items[I].Checked:= true;
    end;

  end;
end;

procedure TNotesPanelHost.DoPgsResize(Sender: TObject);
begin
  fManager.ResizePanelsOfHost(Self);
end;

procedure TNotesPanelHost.DoPopSetLocation(Sender: TObject);
Var
  apanel: TNotesPanel;
  ahost: TNotesPanelHost;
begin
  if fPageCtrl.PageCount = 0 then Exit;
  if not (Sender is TMenuItem) then Exit;
  apanel:= Manager.getPanelByParent(fPagectrl.ActivePage As TWinControl);
  ahost:= Manager.getHostByPgCtrl(TPageControl(Pointer((Sender As TMenuItem).tag)));
  if (ahost <> nil) and (apanel <> nil) then
    Manager.setPanelLocation(apanel, ahost);
end;

procedure TNotesPanelHost.DoPopSetTab(Sender: TObject);
begin
  if (Sender is TMenuItem) and ((Sender As TMenuItem).GroupIndex = HP_TAB_GROUPINDEX) then
    fPageCtrl.ActivePageIndex:= (Sender As TMenuItem).Tag;
end;

procedure TNotesPanelHost.ReloadPopupMenu;
Var
  pi: TMenuItem;
  I: integer;
begin
  fPopup.Items.AutoLineReduction:= maManual;
  fPopup.Items.AutoHotkeys:= maManual;

  // limpa os items do popup
  fPopup.items.Clear;

  // HOSTS
  for I := 0 to fManager.HostsList.Count - 1 do
  begin
    pi:= TMenuItem.Create(fPopup.Items);
    pi.Caption:= TNotesPanelHost(fManager.HostsList.Items[I]).Name;
    pi.Tag:= Integer(Pointer(TNotesPanelHost(fManager.HostsList.Items[I]).PageControl));
    pi.OnClick:= DoPopSetLocation;
    pi.GroupIndex:= HP_HOST_GROUPINDEX;
    pi.RadioItem:= true;
    fPopup.Items.Add(pi);
  end;
  // insere separador
  fPopup.Items.NewBottomLine;

  //TABS
  for I := 0 to fPageCtrl.PageCount - 1 do
  begin
    pi:= TMenuItem.Create(fPopup.Items);
    pi.Caption:= fPageCtrl.Pages[I].Caption;
    pi.Tag:= I;
    pi.OnClick:= DoPopSetTab;
    pi.GroupIndex:= HP_TAB_GROUPINDEX;
    pi.RadioItem:= true;
    fPopup.Items.Add(pi);
  end;

  fPopup.Items.AutoLineReduction:= maAutomatic;
  fPopup.Items.AutoHotkeys:= maAutomatic;
end;

{ TNotesPanelManager }

procedure TNotesPanelManager.addPanel(APanel: TNotesPanel);
begin
  fPanelsList.Add(Pointer(APanel));
  APanel.AfterPanelCreate(Self);
end;

procedure TNotesPanelManager.addPanelHost(const Name: string; Panel: TPanel;
  HostType: TNotesPanelHostType);
Var
  I: integer;
begin
  I:= fHostsList.Add(Pointer(TNotesPanelHost.Create(Name, Panel, HostType)));
  TNotesPanelHost(fHostsList.Items[I]).Manager:= self;
end;

procedure TNotesPanelManager.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

constructor TNotesPanelManager.Create;
begin
  fPanelsList:= TList.Create;
  fHostsList:= TList.Create;
  // O EndUpdate é chamado no final de Initialize
  BeginUpdate;
end;

destructor TNotesPanelManager.Destroy;
Var
  I: integer;
begin
  // Chamamos BeginUpdate para que nada tente se atualizar
  // durante a destruição.
  BeginUpdate;

  // setamos a página ativa para -1 para que possamos destruir todas as
  // tabs tranquilamente
  for I := 0 to fHostsList.Count -1 do
    TNotesPanelHost(fHostsList.Items[I]).PageControl.ActivePageIndex:= -1;

  for I:= 0 to fPanelsList.Count -1 do
    TNotesPanel(fPanelsList.Items[I]).Free;

  for I := 0 to fHostsList.Count -1 do
    TNotesPanelHost(fHostsList.Items[I]).Free;

  fHostsList.Free;
  fPanelsList.Free;
  inherited;
end;


procedure TNotesPanelManager.EndUpdate;
Var
  I: integer;
begin
  if fUpdateCount > 0 then
  begin
    Dec(fUpdateCount);
    if fUpdateCount = 0 then
    begin
      ResizePanels;
      for I := 0 to fHostsList.Count - 1 do
        TNotesPanelHost(fHostsList.Items[I]).ReloadPopupMenu;
    end;
  end else
    fUpdateCount:= 0;
end;

function TNotesPanelManager.getHostByName(
  const Name: string): TNotesPanelHost;
Var
  I: integer;
begin
  Result:= nil;

  for I := 0 to fHostsList.Count - 1 do
  begin
    if SameText(TNotesPanelHost(fHostsList.Items[I]).Name, Name) then
    begin
      Result:= TNotesPanelHost(fHostsList.Items[I]);
      Exit;
    end;
  end;
end;


function TNotesPanelManager.getHostByPgCtrl(
  PgCtrl: TPageControl): TNotesPanelHost;
Var
  I: integer;
begin
  Result:= nil;
  for I := 0 to fHostsList.Count - 1 do
    if TNotesPanelHost(fHostsList.Items[I]).PageControl = PgCtrl then
    begin
      Result:= TNotesPanelHost(fHostsList.Items[I]);
      Exit;
    end;
end;

function TNotesPanelManager.getPanelByName(
  const Name: string): TNotesPanel;
Var
  I: integer;
begin
  Result:= nil;
  for I := 0 to fPanelsList.Count - 1 do
  begin
    if SameText(TNotesPanel(fPanelsList.Items[I]).getPanelName, Name) then
    begin
      Result:= TNotesPanel(fPanelsList.Items[I]);
      Exit;
    end;
  end;
end;

procedure TNotesPanelManager.Initialize;
Var
  I: integer;
begin
//  for I := 0 to fPanelsList.Count - 1 do
//   for J:= 0 to TNotesPanel(fPanelsList.items[I]).CompsOwner.ComponentCount - 1 do
//     NotesMenu.InitComponent(TNotesPanel(fPanelsList.items[I]).CompsOwner.Components[J]);

  for I := 0 to fPanelsList.count - 1 do
    TNotesPanel(fPanelsList.Items[I]).Initialize;

  EndUpdate;
end;

procedure TNotesPanelManager.Load;
Var
  S, B: string;
  I: integer;
begin
  S:= FileToStr(NProfile.Paths.PanelsOptionsFile);
  // caregamos a sessão que contém o host de cada painel
  B:= ReadSection(S, 'PanelsHosts');
  // carregamos as localizações dos painés
  for I := 0 to fPanelsList.Count -1 do
  begin
    setPanelLocation(TNotesPanel(fPanelsList.Items[I]).getPanelName, ReadTagStr(B, TNotesPanel(fPanelsList.Items[I]).getPanelName, fDefaultHost));
  end;
  // carregamos a sessão que tem as opções da cada painel
  B:= ReadSection(S, 'PanelsOptions');
  // mandamos os painéis lerem suas opções
  for I := 0 to fPanelsList.Count -1 do
    TNotesPanel(fPanelsList.Items[I]).ReadOptions(ReadSection(B, TNotesPanel(fPanelsList.Items[I]).getPanelName));

  for I:= 0 to fHostsList.Count -1 do
    TNotesPanelHost(fHostsList.Items[I]).CheckPanelsCount;
end;

procedure TNotesPanelManager.ResizePanels;
Var
  I: integer;
begin
  if Updating then Exit;

  for I := 0 to fPanelsList.Count-1 do
    TNotesPanel(fPanelsList.Items[I]).Resize;
end;

procedure TNotesPanelManager.ResizePanelsOfHost(AHost: TNotesPanelHost);
Var
  I: integer;
begin
  if Updating then Exit;
  if AHost = nil then Exit;

  for I:= 0 to fPanelsList.Count -1 do
    if TNotesPanel(fPanelsList.Items[I]).Host = AHost then
      TNotesPanel(fPanelsList.Items[I]).Resize;
end;

procedure TNotesPanelManager.setPanelLocation(const PanelName,
  HostName: string);
begin
  setPanelLocation(getPanelByName(PanelName), getHostByName(HostName));
end;

procedure TNotesPanelManager.setPanelLocation(NPanel: TNotesPanel;
  Host: TNotesPanelHost);
var
  oldhost: TNotesPanelHost;
begin
  if (assigned(NPanel)) and (assigned(Host)) then
  begin
    //LockWindowUpdate(Application.MainForm.Handle);
    oldhost:= NPanel.Host;
    try
      oldhost.PageControl.ActivePageIndex:= -1;

      NPanel.Host:= Host;
      (NPanel.Parent As TTabSheet).Parent:= Host.PageControl As TWinControl;
      (NPanel.Parent As TTabSheet).PageControl:= Host.PageControl;
      Host.PageControl.ActivePageIndex:= pred( Host.PageControl.PageCount );
      Host.CheckPanelsCount;

      oldhost.PageControl.ActivePageIndex:= pred( oldhost.PageControl.PageCount );
      oldhost.CheckPanelsCount;
    finally
      //LockWindowUpdate(0);
      oldhost.ReloadPopupMenu;
      Host.ReloadPopupMenu;
    end;
  end;
end;

procedure TNotesPanelManager.Save;
Var
  S, B, C: string;
  I: integer;
begin

  for I := 0 to fPanelsList.Count -1 do
    WriteTagStr(@B, TNotesPanel(fPanelsList.Items[I]).getPanelName,
    TNotesPanel(fPanelsList.Items[I]).Host.Name);

  WriteSection(@S, 'PanelsHosts', B);
  B:= '';

  for I := 0 to fPanelsList.Count -1 do
  begin
    C:= '';
    TNotesPanel(fPanelsList.Items[I]).WriteOptions(C);
    WriteSection(@B, TNotesPanel(fPanelsList.Items[I]).getPanelName, C);
  end;

  WriteSection(@S, 'PanelsOptions', B);

  NotesXML.AddStandardXMLTags(S, 'NotesPanelsOptions');

  StrTofile(NProfile.Paths.PanelsOptionsFile, S);
end;


function TNotesPanelManager.Updating: boolean;
begin
  Result:= (fUpdateCount > 0);
end;

procedure TNotesPanelManager.HidePanel(const PanelName: string);
Var
  pa: TNotesPanel;
  h: TNotesPanelHost;
begin
  pa:= getPanelByName(PanelName);
  if pa <> nil then
  begin
    h:= pa.Host;
    if (assigned(H) = false) or (H.Name = '') then
      H:= getHostByPgCtrl(pa.fTab.PageControl);
    if H = nil then Exit;

    if not H.Splitter.Collapsed then
      H.Splitter.Collapsed:= true;
  end;
end;

procedure TNotesPanelManager.ShowPanel(const PanelName: string);
Var
  pa: TNotesPanel;
  h: TNotesPanelHost;
begin
  pa:= getPanelByName(PanelName);
  if pa <> nil then
  begin
    h:= pa.Host;
    if (assigned(H) = false) or (H.Name = '') then
      H:= getHostByPgCtrl(pa.fTab.PageControl);
    if H = nil then Exit;

    if H.Splitter.Collapsed then
      H.Splitter.Collapsed:= false;

    pa.fTab.PageControl.ActivePage:= pa.fTab;
  end;
end;

procedure TNotesPanelManager.HideAllPanels;
Var
  I: integer;
begin
  for I:= 0 to fHostsList.Count - 1 do
    if not TNotesPanelHost(fHostsList.Items[I]).Splitter.Collapsed then
      TNotesPanelHost(fHostsList.Items[I]).Splitter.Collapsed := true;
end;

procedure TNotesPanelManager.createPanels;
Var
  I: integer;
  H: TNotesPanelHost;
begin
  if fHostsList.Count = 0 then
    raise Exception.Create(ClassName + '.CreatePanels - create the hosts first!!');

  H:= getHostByName(DefaultHost);

  if H = nil then
    H:= TNotesPanelHost(fHostsList.Items[0]);

  // cria um painel para cada classe
  for I:= 0 to PanelReg.Count -1 do
    addPanel(PanelReg.getPanelClass(I).Create(H));

end;

function TNotesPanelManager.getPanelProperty(const PanelName,
  PanelProperty: string): string;
Var
  propInfo : PPropInfo;
  obj: TNotesPanel;
begin
  obj:= getPanelByName(PanelName);

  if obj = nil then
    raise Exception.Create(ClassName+'.getPanelProperty - panel "'+PanelName+'" does not exists.');

  propInfo := getPropInfo(obj, PanelProperty);

  if propInfo = nil then
    raise Exception.Create(ClassName + '.getPanelProperty - can not find property "' +PanelProperty+ '" in panel "'+PanelName+'".');

  try
    case propInfo.PropType^.Kind of
      tkString, tkLString, tkWString: Result:= getStrProp(obj, propInfo);
      tkInteger, tkChar, tkWChar: Result:= IntToStr(GetOrdProp(obj, propInfo));
      tkFloat: Result:= FloatToStr(GetFloatProp(obj, propInfo));
      tkSet: Result:= GetSetProp(obj, propInfo);
      tkVariant: Result:= GetVariantProp(obj, propInfo);
      // Bolleans
      tkEnumeration: Result:= GetEnumProp(obj, propInfo);
      tkInt64: Result:= IntToStr(GetInt64Prop(obj, propInfo));
    end;
  except
    raise Exception.Create(ClassName + '.getPanelProperty - could not get property "' +PanelProperty+ '" in panel "'+PanelName+'", sorry :(');
  end;
end;

function TNotesPanelManager.PanelExecute(const PanelName, PanelAction,
  Params: string): string;
Var
 pa: TNotesPanel;
begin
  Result:='';
  pa:= getPanelByName(PanelName);
  if pa = nil then
    raise Exception.Create(ClassName+'.PanelExecute - panel "'+PanelName+'" does not exists.');
  Result:= pa.PanelExec(PanelAction, Params);
end;

procedure TNotesPanelManager.setPanelProperty(const PanelName, PanelProperty,
  Value: string);
Var
  propInfo : PPropInfo;
  obj: TNotesPanel;

begin
  obj:= getPanelByName(PanelName);

  if obj = nil then
    raise Exception.Create(ClassName+'.setPanelProperty - panel "'+PanelName+'" does not exists.');

  propInfo := getPropInfo(obj, PanelProperty);

  if propInfo = nil then
    raise Exception.Create(ClassName + '.setPanelProperty - can not find property "' +PanelProperty+ '" in panel "'+PanelName+'".');

  try
    case propInfo.PropType^.Kind of
      tkString, tkLString, tkWString: setStrProp(obj, propInfo, Value);
      tkInteger, tkChar, tkWChar: setOrdProp(obj, propInfo, strToIntDef(Value, 0));
      tkFloat:  SetFloatProp(obj, propInfo, strToFloat(Value));
      tkSet: SetSetProp(obj, propInfo, Value);
      tkVariant: SetVariantProp(obj, propInfo, Value);
      // Bolleans
      tkEnumeration: SetEnumProp(obj, propInfo, Value);
      tkInt64:  SetInt64Prop(obj, propInfo, strToInt64(Value));
    end;
  except
    raise Exception.Create(ClassName + '.setPanelProperty - could not set property "' +PanelProperty+ '" in panel "'+PanelName+'" to "'+Value+'".');
  end;
end;

function TNotesPanelManager.getPanelByParent(
  PanelParent: TWinControl): TNotesPanel;
Var
  I: integer;
begin
  Result:= nil;
  for I:= 0 to fPanelsList.Count - 1 do
    if TNotesPanel(fPanelsList.Items[I]).Parent = PanelParent then
    begin
      Result:= TNotesPanel(fPanelsList.Items[I]);
      Exit;
    end;
end;


////////////////////////////////////////////////
//////
//////  SPLITTER
//////
/////////////////////////////////////////////////////////

procedure DrawArrow(const Orientation: byte; const Cn: TCanvas; const P: TPoint);
begin
  Case Orientation of
  ARROW_TO_TOP:
    begin
      // 1   -
      Cn.MoveTo(P.X + 3, P.Y);
      Cn.LineTo(P.X + 4, P.Y);
      // 3  ---
      Cn.MoveTo(P.X + 1, P.Y+1);
      Cn.LineTo(P.X + 5, P.Y+1);
      // 5 -----
      Cn.MoveTo(P.X, P.Y+2);
      Cn.LineTo(P.X + 6, P.Y+2);
    end;
  ARROW_TO_BOTTOM:
    begin
      // 5 -----
      Cn.MoveTo(P.X, P.Y);
      Cn.LineTo(P.X + 6, P.Y);
      // 3  ---
      Cn.MoveTo(P.X + 1, P.Y+1);
      Cn.LineTo(P.X + 5, P.Y+1);
      // 1   -
      Cn.MoveTo(P.X + 3, P.Y+2);
      Cn.LineTo(P.X + 4, P.Y+2);
    end;
  ARROW_TO_LEFT:
    begin
      // 1
      Cn.MoveTo(P.X, P.Y+3);
      Cn.LineTo(P.X, P.Y+4);
      // 3
      Cn.MoveTo(P.X+1, P.Y+1);
      Cn.LineTo(P.X+1, P.Y+5);
      // 5
      Cn.MoveTo(P.X+2, P.Y);
      Cn.LineTo(P.X+2, P.Y+6);
    end;
  ARROW_TO_RIGHT:
    begin
      //5
      Cn.MoveTo(P.X, P.Y);
      Cn.LineTo(P.X, P.Y+6);
      //3
      Cn.MoveTo(P.X+1, P.Y+1);
      Cn.LineTo(P.X+1, P.Y+5);
      //1
      Cn.MoveTo(P.X+2, P.Y+3);
      Cn.LineTo(P.X+2, P.Y+4);
    end;
  end;
end;

function MixColors(CL1, CL2: TColor): TColor;
Var
  rgb1, rgb2: longInt;
  r,g,b: integer;
begin
  rgb1:= ColorToRGB(CL1);
  rgb2:= ColorToRGB(CL2);
  r := (rgb1 and $000000FF)          + (rgb2 and $000000FF)          div 2;
  g := ((rgb1 and $0000FF00) shr 8)  + ((rgb2 and $0000FF00) shr 8)  div 2;
  b := ((rgb1 and $00FF0000) shr 16) + ((rgb2 and $00FF0000) shr 16) div 2;
  if r > 255 then  r:= 255;
  if g > 255 then  g:= 255;
  if b > 255 then  b:= 255;
  Result := RGB(r, g, b);
end;


{ TNotesSpliter }

function TNotesSplitter.GetCollapsed: Boolean;
begin
  Result:= false;
  if not Assigned(Parent) then Exit;

  if ( (align in [alLeft, alRight]) and
       (Parent.Width < Width + 2)   )
     or
     ( (align in [alTop, alBottom]) and
       (Parent.Height < Height + 2) ) then
    Result:= true;
end;

constructor TNotesSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Align:= alRight;
  Width:= 8;
  Height:= 8;
  BevelInner:= bvnone;
  BevelOuter:= bvnone;
  // por causa do XPMenu
  tag:= 999;
end;

procedure TNotesSplitter.Paint;
begin
  inherited;

  if not (align in [alLeft, alRight, alTop]) then
    Exit;

  Canvas.Pen.Color:= MixColors(clBlack, clBtnFace);
  Canvas.Brush.Color:= clBtnShadow;
  Canvas.Rectangle(0,0,Width,Height);

  if not fMouseOnButton then
  begin
    Canvas.Brush.Color:= clBtnFace;
    Canvas.Pen.Color:= clBtnShadow;
  end else
  begin
    Canvas.Brush.Color:= MixColors(clSilver, clBtnFace);
    Canvas.Pen.Color:= clBtnShadow;
  end;

  Case align of
    alLeft, alRight: Canvas.Rectangle(0,(Height div 2) -25, width ,(Height div 2) +25);
  else
    Canvas.Rectangle((Width div 2)- 25,0,(Width div 2) +25, height);
  end;

  // chamar DrawArrow

  if fMouseOnButton then
    Canvas.Pen.Color:= clBtnText
  else
    Canvas.Pen.Color:= MixColors(clBlack, clBtnFace);

  Case align of
    alLeft:
      if GetCollapsed then
        DrawArrow(ARROW_TO_LEFT, Canvas, Point(2, (Height div 2)-2))
      else
        DrawArrow(ARROW_TO_RIGHT, Canvas, Point(3, (Height div 2)-2));
    alRight:
      if GetCollapsed then
        DrawArrow(ARROW_TO_RIGHT, Canvas, Point(3, (Height div 2)-2))
      else
        DrawArrow(ARROW_TO_LEFT, Canvas, Point(2, (Height div 2)-2));
    alTop:
      if GetCollapsed then
        DrawArrow(ARROW_TO_TOP, Canvas, Point((Width div 2)-2, 2))
      else
        DrawArrow(ARROW_TO_BOTTOM, Canvas, Point((Width div 2)-2, 3));
  end;
end;

procedure TNotesSplitter.setCollapsed(const Value: Boolean);
begin
  if not assigned(Parent) then
    Exit;

  if Value then
  begin
    if align = alTop then
    begin
      fNotCollapsedSize:= Parent.Height;
      Constraints.MinHeight:= 8;
      Parent.Height:= 8;
    end else
    begin
      fNotCollapsedSize:= Parent.Width;
      Constraints.MinWidth:= 8;
      Parent.Width:= 8;
    end;
  end else
  begin
    if align = alTop then
    begin
      if fNotCollapsedSize > 70 then
        Parent.Height:= fNotCollapsedSize
      else
        Parent.Height:= 160;
    end else
    begin
      if fNotCollapsedSize > 80 then
        Parent.Width:= fNotCollapsedSize
      else
        Parent.Width:= 200;
    end;
  end;
  Parent.Invalidate;
end;

procedure TNotesSplitter.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    fDownPos:= Point(X, Y);
    fMoving:= true;
    fMouseMovesCount:= 0;

    if (Collapsed = true) and (IsMouseOverBtn(X,Y) = false) then
    begin
       Collapsed:= false;
       fMoving:= false;
    end;
  end;
  inherited;
end;

procedure TNotesSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewOnBtn: boolean;
begin
  if not Assigned(Parent) then
    Exit;
  ///////////////////////////////
   // efeitos do botão        //
  ///////////////////////////////
  NewOnBtn:= isMouseOverBtn(X, Y);

  if (NewOnBtn) or (Collapsed) then
  begin
    Cursor:= crArrow
  end else
  begin
    if align in [alLeft, alright] then
      Cursor:= crHSplit
    else
      Cursor:= crVSplit;
  end;

  if NewOnBtn <> fMouseOnButton then
  begin
    fMouseOnButton:= NewOnBtn;
    Paint;
  end;

  ////////////////////////////////
   // fazemos  o rezising      //
  ////////////////////////////////
  if fMoving = false then
    Exit;
  // Se o usuário diminuir muito o tamanho do controle,
  // então fechamos o controle para ele
  if ((align in [alLeft, alRight]) and (Parent.Width < 30)) or
    ((align = alTop) and (Parent.Height < 24)) then
  begin
    fMoving:= false;
    Collapsed:= true;
    Exit;
  end;

  // só movemos a cada 3 mensagens
  if fMouseMovesCount < 3 then
  begin
    inc(fMouseMovesCount);
    Exit;
  end else
    fMouseMovesCount:= 0;
  ///

  Case align of
    alLeft:
      if fDownPos.X <> X then
        Parent.Width:= Parent.Width + ( fDownPos.X - X);
    alRight:
      if fDownPos.X <> X then
        Parent.Width:= Parent.Width + ( fDownPos.X + X);
    alTop:
      if fDownPos.Y <> Y then
        Parent.Height:= Parent.Height + (fDownPos.Y - Y)
  end;

  inherited;
end;

procedure TNotesSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsMouseOverBtn(X, Y) then
    if IsMouseOverBtn(fDownPos.X, fDownPos.Y) then
      Collapsed:= not Collapsed;

  fMoving:= false;
  fMouseMovesCount:= 0;
  inherited;
end;

procedure TNotesSplitter.CMMouseEnter(var Message: TMessage);
begin
  // setamos o cursor correto indicando a
  // ação que a splitter fará
  if align in [alBottom, alTop] then
  begin
    if (Collapsed = false) and (fMouseOnButton = false) then
      Cursor:= crVSplit
    else
      Cursor:= crArrow;
  end else
  begin
    if (Collapsed = false) and (fMouseOnButton = false)  then
      Cursor:= crHSplit
    else
      Cursor:= crArrow;
  end;
end;

procedure TNotesSplitter.CMMouseLeave(var Message: TMessage);
begin
  if fMouseOnButton then
  begin
    fMouseOnButton:= false;
    Paint;
  end;
end;

function TNotesSplitter.IsMouseOverBtn(X, Y: integer): boolean;
begin
  Result:= false;
  if align in [alLeft, alRight] then
  begin
    if (Y > ((Height div 2) - 25)) and (Y < ((Height div 2) + 25)) then
      Result:= true;
  end else
  begin
    if (X > ((Width div 2) - 25)) and (X < ((Width div 2) + 25)) then
      Result:= true;
  end;
end;


{ TNotesPanelsRegistry }

constructor TNotesPanelsRegistry.Create;
begin
  fCount:= 0;
end;

destructor TNotesPanelsRegistry.Destroy;
begin
  fCount:= 0;
  setLength(fItems, 0);
  inherited;
end;

function TNotesPanelsRegistry.getPanelClass(index: integer): TNotesPanelClass;
begin
  result:= fItems[index];
end;

procedure TNotesPanelsRegistry.registerPanel(PanelClass: TNotesPanelClass);
begin
  inc(fCount);
  setLength(fItems, fCount);
  fItems[fCount-1]:= PanelClass;
end;

initialization
  PanelReg:= TNotesPanelsRegistry.Create;

finalization
  if Assigned(PanelReg) then
    FreeAndNil(PanelReg);

end.
