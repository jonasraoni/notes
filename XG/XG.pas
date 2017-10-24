unit XG;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, LibXmlParser, Graphics,
  strUtils, stdCtrls, ExtCtrls, XGUtils, typInfo, XGComps;

type
  TXGLoadEvent= procedure(Sender: TObject; const FileName: string; var CanLoad: boolean) of object;

  { Classe principal - faz a leitura do XML convertendo para componentes. }
  TXG = class(TComponent)
  private
    fFile: string;
    xml: TxmlParser;
    fBind: TComponent;
    XgOwner: IXgOwner;
    // Carrega a página de um arquivo XG
    procedure  loadFromFile(Value: string);
    procedure  ReadComp(const tagName: string);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property  src: string read fFile write loadFromFile;
  end;

  TXgFrame = class(TXgScrollBox, iXgOwner)
  private
    fChromePath: string;
    fBind: TComponent;
    fCurParent: TWinControl;
    fOnLoad: TXGLoadEvent;
    fTimedLoad: string;
    ftimer: TTimer;
    fXG: TXG;
    fFile: string;
    fCssList: TCSSStyleList;
    Updating: Boolean;
    fSkinName: string;
    fLocale: string;
    fExtraData: TXgExtraDataList;
    fOnOpenURI: TXGLoadEvent;
    procedure OnTimeToLoad(Sender: TObject);
    procedure setLocale(const Value: string);
    procedure setSkin(const Value: string);
    procedure sendMessageToContainers(msg: TXgContainerMessages);
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    // IInterface - implementado como stub
    function _AddRef: Integer; stdcall;
    // IInterface - implementado como stub
    function _Release: Integer; stdcall;
    function getBind: TComponent;
    function  getURIPath(const uri: string): string;
    procedure openURI(const uri: string);
    procedure loadFromFile(FileName: string);
    procedure setCurParent(Value: TWinControl);
    function  getCurParent: TWinControl;
    function getRootExtraData: TXgExtraDataList;
    function  getCssPropertys(Id, ClassName, Style, tagName: string): TCSSPropArray;
  published
    // Passe em @code(Bind) o objeto que contém a
    // implementação dos métodos usados no XG.
    property Bind: TComponent read fBind write fBind;
    // Determina a skin que deverá ser usada
    property Skin: string read fSkinName write setSkin;
    // Determina a linguagem as ser usada.
    property Locale: string read fLocale write setLocale;
    // Permite ao aplicativo saber quando uma nova página esta sendo carregada
    // e impedir o carregamento se preciso.
    property OnLoad: TXGLoadEvent read fOnLoad write fOnLoad;
    // Evento chamado ao abrir uma uri
    property OnOpenUri: TXGLoadEvent read fOnOpenURI write fOnOpenURI;
  end;

implementation

uses XGContainers, XGServices, XGTypeHandlers, shellApi;

{ TXG }

constructor TXG.Create(Owner: TComponent);
begin
  inherited;
  XGOwner := Owner as IXgOwner;
end;

destructor TXG.Destroy;
begin
  if Assigned(xml) then
    FreeAndNil(xml);
  inherited;
end;

procedure TXG.loadFromFile(Value: string);
begin
  if not Assigned(XML) then xml:= TxmlParser.Create;

  if not XML.LoadFromFile(Value) then
    raise Exception.Create('The XG file "'+Value+'" could not be found.');

  fFile:= Value;

  XML.StartScan;

  While XML.Scan do
  begin
    if XML.CurPartType in [ptStartTag, ptEmptyTag] then
    begin
      if XML.CurName <> 'xg' then
        ReadComp(xml.CurName);
    end else
    if (XML.CurPartType = ptEndTag) and (SameText(xml.CurName, CompsReg.getTagName(TComponentClass(XGOwner.getCurParent.ClassType)))) then
    begin
      XGOwner.setCurParent( XGOwner.getCurParent.Parent );
    end;
  end;

  FreeAndNil(XML);
end;

procedure TXG.ReadComp(const tagName: string);
Var
  Ctrl: TComponentClass;
  obj: TComponent;
  I: integer;
  N, V: string;
  tc: TComponentClass;
  sTagName: string;
  data: TXgExtraCompData;
begin
  Ctrl:= CompsReg.getCtrl(TagName);
  if Ctrl = nil then Exit;
  obj:= Ctrl.Create(Owner);
  // Não remova! Isto parece inútil mas é necessário
  // por que tagName vem associado a xml.CurName por causa da suprema
  // inteligência do compilador. Como usamos XML.Scan, xml.CurName muda
  // e tagName vai pro saco. O jeito é copiarmos ela para outra variável...
  sTagName:= Copy(tagName, 1, length(tagName));
  // Serviços e Componentes só podem ser criados dentro de um container
  if (isXGContainer(XGOwner.getCurParent) = false) and (isXGContainer(obj) = false) then
    Exit;

  if obj is TControl then
    (obj as TControl).Parent:= XGOwner.getCurParent;

  if isXGContainer(obj) then
  begin
    (obj As IXgContainer).initContainer(self, fBind, xml);
  end else
  if isXGService(obj) then
  begin
    (obj As iXgService).InitService(self, fBind, xml);
  end;

  for I:= 0 to xml.CurAttr.Count - 1 do
  begin
    // Sim, isso é necessário! Deixe o copy aí apesar de parecer idiotice...
    N:= Copy(xml.CurAttr.Name(I), 1, length(xml.CurAttr.Name(I)));
    V:= Copy(xml.CurAttr.Value(I), 1, length(xml.CurAttr.Value(I)));
    if N = 'flex' then
      data.Flex:= StrToInt(V)
    else if N = 'class' then
      data.CssClass:= V
    else if N = 'style' then
      data.CssStyle:= V
    else if N = 'persist' then
      data.Persist:= V
    else
      setProperty(obj, sTagName, N,  ParsePropertys(V, Owner, XGOwner.getBind));
  end;

  // Containers não podem ter atributos em tags separadas. Serviços e componentes podem.
  if (isXGContainer(obj) = false) and (xml.CurPartType <> ptEmptyTag) then
  begin
    While (xml.Scan) and  ((xml.CurPartType <> ptEndTag) and (SameText(xml.CurName, sTagName)=false) ) do
    begin
      if xml.CurPartType = ptStartTag then
      begin
        N:= Copy(xml.CurName, 1, length(XML.CurName));
        tc:= TypeHandlersReg.getCtrl(N);
        if tc = nil then
        begin
          if (xml.Scan) and (xml.CurPartType in [ptContent, ptCDATA]) then
          begin
            V:= Copy(xml.CurContent, 1 , length(xml.CurContent));
            if xml.curPartType = ptContent then
              V:= ParsePropertys(V, Owner, XGOwner.getBind);
            setProperty(obj, sTagName, N, V);
          end;
        end else
        begin
          // será liberado qdo o XG for destruído. É necessário deixar assim
          // para não dar violação de memória - ainda não pude entender por que.Self..
          (tc.Create(self) as IXgTypeHandler).ReadTags(N, xml, obj, XgOwner.getCurParent, Owner);
        end;
      end;
    end;
  end;

  data.Child:= obj;

 {*
  *  Os containers guardam dados extras para todos os
  *  seus componentes filhos. O XGOwner guarda os dados
  *  dos containers que estão na raiz...
  *}
  if isXgContainer( XGOwner.getCurParent) then
    (XGOwner.getCurParent As IXgContainer).addComponent(data)
  else if XGOwner.getCurParent = Owner then
    XGOwner.getRootExtraData.add(data);

  if obj is TControl then
  begin
    // * aplicar Estilo! *
     applyStyle(obj As TControl);
    // * aplicar localização *
     applyLocale(obj As TControl);
  end;

  if isXGContainer(obj) then
  begin
    (obj As ixgContainer).EndReadContainerPropertys;
    XGOwner.setCurParent( obj as TWinControl );
  end else
  if isXGService(obj) then
  begin
    (obj As iXgService).EndReadServicePropertys;
  end;
end;


{ TXgFrame }

function TXgFrame._AddRef: Integer;
begin
  Result:= -1;
end;

function TXgFrame._Release: Integer;
begin
  Result:= -1;
end;

function TXgFrame.getBind: TComponent;
begin
  Result:= fBind;
end;

procedure TXgFrame.loadFromFile(FileName: string);
Var
  b: boolean;
  I: integer;
  c: TComponent;
  s: string;
begin
  if Updating then Exit;

  fChromePath:= IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(application.ExeName)) + 'chrome');

  // Suporte para o evento OnLoad
  if (Assigned(fOnLoad)) and (fTimedLoad='') then
  begin
    b:= true;
    fOnLoad(self, FileName, b);
    if b = false then
      Exit;
  end;

  if self.ComponentCount > 0 then
  begin
    if Updating then Exit;
    fTimedLoad:= FileName;
    fTimer.Enabled:= true;
    Exit;
  end;

  Updating:= true;
  ffile:= FileName;

  if not Assigned(LastFont) then
  begin
    LastFont:= TFont.Create;
    LastFont.Assign(self.Font);
  end;

  fcurParent:= self;

  fXG:= TXG.Create(self);
  try
    s:= getURIPath('chrome://skin/') + ChangeFileExt(getUriFileName(FileName), '') + '.css';
    if S <> '' then
      fCssList.LoadFromFile(s);
    s:= getURIPath(FileName);
    if s <> '' then
      fXG.src:= s;
  finally
    // Os XG só servem para ler o XML. Após ler o documento, devem ser destruídos
    for I := ComponentCount - 1 downto 0 do
    begin
      if Components[I] is TXG then
      begin
        c:= Components[I];
        self.RemoveComponent(C);
        FreeAndNil(C);
      end
      else if isXgService(Components[I]) then
      begin
        b:= false;
        (Components[I] As IXGService).endReadPage(b);
        if b then
        begin
          c:= Components[I];
          self.RemoveComponent(C);
          FreeAndNil(C);
        end;
      end;
    end;

    sendMessageToContainers(cmXgLoaded);
    Updating:= false;
  end;

end;

procedure TXGFrame.OnTimeToLoad(Sender: TObject);
begin
  fTimer.Enabled:= false;
  if Updating then Exit;
  if fTimedLoad <> '' then
  begin
    try
      screen.Cursor:= crHourGlass;
      DestroyComponents;
      LoadFromFile(fTimedLoad);
    finally
      screen.Cursor:= crDefault;
    end;
  end;
  fTimedLoad:= '';
end;

constructor TXgFrame.Create(Owner: TComponent);
begin
  inherited;
  fTimer:= TTimer.Create(nil);
  fTimer.Enabled:= false;
  fTimer.Interval:= 100;
  fTimer.OnTimer:= OnTimeToLoad;
  fBind:= nil;
  Updating:= false;
  fCssList:= TCSSStyleList.Create;
  fExtraData:= TXgExtraDataList.Create;

  // containers
  CompsReg.add('canvas', TXgCanvasContainer);
  CompsReg.add('hbox', TXgHBox);
  CompsReg.add('vbox', TXgVBox);
  CompsReg.add('bbox', TXgBBox);
  CompsReg.add('box', TXgBox);
  // serviços

  // componentes
  CompsReg.add('label', TLabel);
  CompsReg.add('button', TButton);
  CompsReg.add('edit', TEdit);
  CompsReg.add('textarea', TXgMemo);
  CompsReg.add('checkbox', TCheckBox);
  CompsReg.add('radiobutton', TRadioButton);
  CompsReg.add('listbox', TListBox);
  CompsReg.add('combobox', TXgComboBox);
  CompsReg.add('hyperlink', TXgHyperLink);
  CompsReg.add('image', TXgImage);
  CompsReg.add('rect', TXgRect);
  CompsReg.add('ellipse', TXgEllipse);
  CompsReg.add('circle', TXgCircle);
  CompsReg.add('line', TXgLine);
  // TYPEHANDLERS
  TypeHandlersReg.add('items', TXgStrListTypeHandler);
end;

destructor TXgFrame.Destroy;
begin
  ftimer.Free;
  fCssList.Free;
  fExtraData.Free;
  inherited;
end;

function TXgFrame.getCurParent: TWinControl;
begin
  Result:= fCurParent;
end;

procedure TXgFrame.setCurParent(Value: TWinControl);
begin
  fCurParent:= Value;
end;

function TXgFrame.getCssPropertys(Id, ClassName, Style, tagName: string): TCSSPropArray;
begin
  Result:= fCssList.getCtrlPropertys(Id, ClassName, Style, tagName);
end;

function TXgFrame.getURIPath(const uri: string): string;
begin
  Result:= '';

  if fileExists(uri) then
  begin
    Result:= uri;
    Exit;
  end;

  if SameText(Copy(uri, 1, 15), 'chrome://locale') then
  begin
    if fLocale <> '' then
      Result:= StringReplace(uri, 'chrome://locale' , fChromePath + 'locale\' + fLocale, [rfIgnoreCase])
    else if DirectoryExists(fChromePath + 'locale\default') then
      Result:= StringReplace(uri, 'chrome://locale' , fChromePath + 'default', [rfIgnoreCase]);
  end else
  if SameText(Copy(uri, 1, 13), 'chrome://skin') then
  begin
    if fSkinName <> '' then
      Result:= StringReplace(uri, 'chrome://skin' , fChromePath + 'skin\' + fSkinName, [rfIgnoreCase])
    else if DirectoryExists(fChromePath + 'skin\default') then
      Result:= StringReplace(uri, 'chrome://skin' , fChromePath + 'skin\default', [rfIgnoreCase]);
  end else
    Result:= StringReplace(uri, 'chrome://' , fChromePath, [rfIgnoreCase]);

  Result:= StringReplace(Result, '/', '\', [rfReplaceAll]);
  Result:= StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

procedure TXgFrame.setLocale(const Value: string);
begin
  fLocale := Value;
end;

procedure TXgFrame.setSkin(const Value: string);
begin
  fSkinName := Value;
end;

procedure TXgFrame.sendMessageToContainers(msg: TXgContainerMessages);
Var
  I: integer;
begin
  for I:= 0 to ControlCount - 1 do
    if isXgContainer(Controls[I]) then
      (Controls[I] As IXgcontainer).XgMessage(msg);
end;

function TXgFrame.getRootExtraData: TXgExtraDataList;
begin
  Result:= fExtraData;
end;

procedure TXgFrame.openURI(const uri: string);
Var
  b: boolean;
begin
  b:= true;
  if Assigned(fOnOpenURI) then
    fOnOpenURI(self, uri, b);

  if not b then Exit;  

  if SameText('.xg', Copy(uri, length(uri) - 2, 3) ) then
    loadFromFile(uri)
  else if SameText('chrome://', copy(uri, 1, 9)) then
    ShellExecute(0, 'open', PChar(getUriPath(uri)), '', '', SW_SHOW)
  else
    ShellExecute(0, 'open', PChar(uri), '','',  SW_SHOW);
end;

end.
