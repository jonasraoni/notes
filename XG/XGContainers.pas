{
* adicionar getRootContainers em IXGOwner
* adicionar no XGFrame
* modificar a forma como os componentes são adicionados
* modificar a parte de CSS para funfar com as alterações
* modificar o Box container
}

unit XGContainers;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, LibXmlParser, Graphics,
  strUtils, ExtCtrls, XGUtils, XGComps, XG;

type
  { Interface que deve ser implementada por componentes
    que queiram funcionar como CONTAINERS em XG. }
  IXgContainer = interface(IInterface)
    ['{ED94F92A-35C9-45DE-964D-8D6588DFA0E4}']
    // Chamado assim que o XG se dá conta que se trata de um Container
    procedure initContainer(const XG: TXG; const Bind: TComponent;
      const xml: TXMlParser);
    // Chamado para cada componente que está sendo adicionado ao container.
    // Um container deve usar este método para alterar as propriedades do
    // componente recém-criado, tais como localização e tamanho.
    // NOTA: TODOS os componentes criados tem como owner o próprio XG, não
    // o container. Isto ocorre para facilitar o acesso por parte do componente
    // que possui a lógica do negócio e, também, por parte de scripts, etc.
    procedure addComponent(data: TXgExtraCompData);
    // Chamado assim que o XG termina de ler as propriedades do container e
    // está pronto para ler os seus componentes.
    procedure endReadContainerPropertys;
    // Retorna os dados extras do componente
    function getExtraData(obj: TComponent): TXgExtraCompData;
    // Usado pelo XG para enviar informações aos containers.
    procedure XgMessage(msg: TXgContainerMessages);
  end;

{
  TXgVBoxContainer

  TXgHBoxContainer

  TXgHSplitedContainer

  TXgVSplitedContainer

  TXgFlowContainer

  }

  { CANVAS - container mais básico que permite ser pintado
    e permite posição fixa dos componentes. }
  TXgCanvasContainer = class(TXgScrollBox, IXgContainer)
  protected
    fExtraDataList: TXgExtraDataList;
    fFlexCount: integer;
  public
    constructor Create(Owner: TComponent); override; 
    destructor Destroy; override; 
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure initContainer(const XG: TXG; const Bind: TComponent;
      const xml: TXMlParser);
    procedure addComponent(data: TXgExtraCompData); virtual;
    procedure endReadContainerPropertys; virtual;
    function getExtraData(obj: TComponent): TXgExtraCompData; dynamic;
    procedure XgMessage(msg: TXgContainerMessages); virtual;
  end;

  TXgBoxOrientation = (boHorizontal, boVertical);
  {
    Determina o alinhamento dos componentes de um container (geralmente um box)
    Da documentação do XUL: "
    # start: Child elements are aligned starting from the left or top edge of the box. If the box is larger than the total size of the children, the extra space is placed on the right or bottom side.
    # center: Extra space is split equally along each side of the child elements, resulting the children being placed in the center of the box.
    # end: Child elements are placed on the right or bottom edge of the box. If the box is larger than the total size of the children, the extra space is placed on the left or top side.
    # baseline: This value applies to horizontally oriented boxes only. It causes the child elements to be aligned so that their text labels are lined up.
    # stretch: The child elements are stretched to fit the size of the box. For a horizontal box, the children are stretched to be the height of the box. For a vertical box, the children are stretched to be the width of the box. If the size of the box changes, the children stretch to fit. Use the flex attribute to create elements that stretch in the opposite direction.
  "}
  TXgChildAlign = (caStart, caCenter, caEnd, caBaseline, caStretch);

  // Classe que vai servir de base para todos os
  // containers do tipo "BOX". Basicamente implementamos
  // tudo aqui e só criamos propriedades e algumas
  // outras coisas nos derivados para implementar
  // o Box, HBox, VBox e BBox.
  // Vide: http://www.xulplanet.com/references/elemref/ref_box.html
  TXgCustomBox = class(TXgCanvasContainer)
  private
    fPadding: integer;
    function getChildAlign: string;
    procedure setChildAlign(const Value: string);
  protected
    fOrient: TXgBoxOrientation;
    fIsXgLoading: boolean;
    fBoxAutoSize: boolean;
    fChildAlign: TXgChildAlign;
    procedure Resize; override;
    procedure DoLayout; dynamic;
  public
    constructor Create(Owner: TComponent); override;
    procedure endReadContainerPropertys; override;
    procedure XgMessage(msg: TXgContainerMessages); override;
  published
    property ChildAlign: string read getChildAlign write setChildAlign;
    property Padding: integer read fPadding write fPadding;
  end;

  TXgBox = class(TXgCustomBox)
  private
    function getOrient: string;
    procedure setOrient(const Value: string);
  published
    property Orient: string read getOrient write setOrient;
  end;

  TXgHBox = class(TXgCustomBox);

  TXgVBox = class(TXgCustomBox)
  public
    constructor Create(Owner: TComponent); override;
  end;

  TXgBBox = class(TXgCustomBox)
  public
    constructor Create(Owner: TComponent); override;
  end;

  { Boxes tem o seguinte comportamento:
    # por padrão eles autodeterminam o seu tamanho conforme
      o tamanho dos controles que está contendo
    # se o usuário setar width/heigth, então ele não autoajusta
      o tamanho, fica fixo. Se os controles ultrapassarem o seu
      tamanho, ele mostra uma scrollbar
  }

function isXgContainer(const obj: TComponent): boolean;

implementation

uses Variants, Math;

function isXgContainer(const obj: TComponent): boolean;
begin
  Result:= false;
  if obj.GetInterfaceEntry(StringToGUID('{ED94F92A-35C9-45DE-964D-8D6588DFA0E4}')) <> nil then
    Result:= true;
end;


{ TXgCanvasContainer }

constructor TXgCanvasContainer.Create(Owner: TComponent);
begin
  inherited;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  fExtraDataList:= TXgExtraDataList.Create;
end;

destructor TXgCanvasContainer.Destroy;
begin
  FreeAndnil(fExtraDataList);
  inherited;
end;

procedure TXgCanvasContainer.endReadContainerPropertys;
begin
//
end;

procedure TXgCanvasContainer.addComponent(data: TXgExtraCompData);
begin
  fExtraDataList.add(data);
  fFlexCount:= fFlexCount + data.Flex; 
end;

function TXgCanvasContainer._AddRef: Integer;
begin
  Result:= -1;
end;

function TXgCanvasContainer._Release: Integer;
begin
  Result:= -1;
end;

procedure TXgCanvasContainer.initContainer(const XG: TXG;
  const Bind: TComponent; const xml: TXMlParser);
begin
  //
end;

function TXgCanvasContainer.getExtraData(
  obj: TComponent): TXgExtraCompData;
begin
  Result:= fExtraDataList.get(obj);
end;


procedure TXgCanvasContainer.XgMessage(msg: TXgContainerMessages);
Var
  I: integer;
begin
  // repassa as mensagens para os childs
  for I:= ControlCount -1 downto 0 do
    if isXgContainer(Controls[I]) then
      (Controls[I] As IXgContainer).XgMessage(msg);
end;


{ TXgCustomBox }

constructor TXgCustomBox.Create(Owner: TComponent);
begin
  inherited;
  fIsXgLoading:= true;
  fOrient:= boHorizontal;
  Width:= 0;
  Height:= 0;
  fPadding:= 8;
end;

procedure TXgCustomBox.DoLayout;
Var
  I, totalW, totalH: integer;
  fLastPos: TPoint;
begin
  if fIsXgLoading then Exit;
  fLastPos.X:= 0;
  fLastPos.Y:= 0;

  totalW:= Width;
  totalH:= Height;

  // tamanho total dos controles
  for I:=0 to fExtraDataList.Count - 1 do
    if fExtraDataList.Items[I].Child is TControl then
    begin
      if fOrient = boHorizontal then
      begin
        totalW:= totalW + (fExtraDataList.Items[I].Child As TControl).Width;
        totalH:= MaX(totalH, (fExtraDataList.Items[I].Child As TControl).Height);
      end else
      begin
        totalW:= MaX(totalW , (fExtraDataList.Items[I].Child As TControl).Width);
        totalH:= totalH + (fExtraDataList.Items[I].Child As TControl).Height;
      end;
    end;

  if fBoxAutoSize then
    SetBounds(Left, top, totalW + fPadding + fPadding, totalH + fPadding + fPadding);

  // quando o tipo de alinhamento for "end"
  // os espaços que sobram devem ficar na esquerda/topo
  // ao invés de ficarem na direita/bottom
  if fChildAlign = caEnd then
  begin
    fLastPos.X:= Width - totalW + fPadding;
    fLastPos.Y:= height - totalH + fPadding;
  end;

  for I:=0 to fExtraDataList.Count - 1 do
  begin
    if fExtraDataList.Items[I].Child is TControl then
    begin
      Case fChildAlign of
        caStart, caEnd:
          begin
            if fOrient = boHorizontal then
              (fExtraDataList.Items[I].Child As TControl).setBounds(fLastPos.X + fPadding, 8, (fExtraDataList.Items[I].Child As TControl).Width, (fExtraDataList.Items[I].Child As TControl).Height)
            else
              (fExtraDataList.Items[I].Child As TControl).setBounds(fPadding, fLastPos.Y + fPadding, (fExtraDataList.Items[I].Child As TControl).Width, (fExtraDataList.Items[I].Child As TControl).Height);
          end;
        caStretch:
          begin
            if fOrient = boHorizontal then
              (fExtraDataList.Items[I].Child As TControl).setBounds(fLastPos.X + fPadding, fPadding, (fExtraDataList.Items[I].Child As TControl).Width, height-fPadding)
            else
              (fExtraDataList.Items[I].Child As TControl).setBounds(fPadding, fLastPos.Y + fPadding, width-fPadding, (fExtraDataList.Items[I].Child As TControl).Height);
          end;
        caBaseline:
          begin
            // Para usar baseline, a orientação deve ser necessariamente horizontal
            fOrient:= boHorizontal;
            (fExtraDataList.Items[I].Child As TControl).setBounds(fLastPos.X + fPadding, fPadding+ fPadding +Height-(fExtraDataList.Items[I].Child As TControl).Height, (fExtraDataList.Items[I].Child As TControl).Width, (fExtraDataList.Items[I].Child As TControl).Height)
          end;
        caCenter:
          begin
            if fOrient = boHorizontal then
              (fExtraDataList.Items[I].Child As TControl).setBounds(fLastPos.X + fPadding, (height -  (fExtraDataList.Items[I].Child As TControl).Height) div 2, (fExtraDataList.Items[I].Child As TControl).Width, (fExtraDataList.Items[I].Child As TControl).Height)
            else
              (fExtraDataList.Items[I].Child As TControl).setBounds( (width - (fExtraDataList.Items[I].Child As TControl).Width) div 2  , fLastPos.Y + fPadding, (fExtraDataList.Items[I].Child As TControl).Width, (fExtraDataList.Items[I].Child As TControl).Height);
          end;

      end;
      fLastPos.X:= (fExtraDataList.Items[I].Child As TControl).left + (fExtraDataList.Items[I].Child As TControl).width;
      fLastPos.Y:= (fExtraDataList.Items[I].Child As TControl).Top + (fExtraDataList.Items[I].Child As TControl).Height;
    end;
  end;

  if fBoxAutoSize then
    SetBounds(Left, top, fLastPos.X + fPadding, fLastPos.Y + fPadding);

end;

procedure TXgCustomBox.endReadContainerPropertys;
begin
  inherited;
  fBoxAutoSize:= (Width = 0) and (Height = 0);
end;

function TXgCustomBox.getChildAlign: string;
begin
  Result:= 'stretch';
  case fChildAlign of
    caStart: Result:= 'start';
    caCenter: Result:= 'center';
    caEnd: Result:= 'end';
    caBaseline: Result:= 'baseline';
  end;
end;

procedure TXgCustomBox.Resize;
begin
  inherited;
  //
end;

procedure TXgCustomBox.setChildAlign(const Value: string);
begin
  if SameText(Value, 'center') then
    fChildAlign:= caCenter
  else if SameText(Value, 'start') then
    fChildAlign:= caStart
  else if SameText(Value, 'end') then
    fChildAlign:= caEnd
  else if SameText(Value, 'baseline') then
    fChildAlign:= caBaseline
  else if SameText(Value, 'stretch') then
    fChildAlign:= caStretch;

  DoLayout;
end;

procedure TXgCustomBox.XgMessage(msg: TXgContainerMessages);
begin
  inherited;
  if msg = cmXgLoaded then
    fIsXgLoading:= false;
  if msg in [cmChildsChanged, cmXgLoaded] then
    DoLayout;
end;


{ TXgBox }

function TXgBox.getOrient: string;
begin
  if fOrient = boHorizontal then
    Result:= 'horizontal'
  else
    Result:='vertical';
end;

procedure TXgBox.setOrient(const Value: string);
begin
  if Sametext(value, 'horizontal') then
    fOrient:= boHorizontal
  else
    fOrient:= boVertical;
end;

{ TXgVBox }

constructor TXgVBox.Create(Owner: TComponent);
begin
  inherited;
  fOrient:= boVertical;
end;

{ TXgBBox }

constructor TXgBBox.Create(Owner: TComponent);
begin
  inherited;
  fChildAlign:= caBaseline;
end;

end.
