unit XGUtils;

interface

uses Windows, Classes, sysUtils, strUtils, Graphics, LibXMLParser, typInfo, Controls;

type
  // Representa um componente suportado
  TXGCompItem = record
    tag: string;
    ctrl: TComponentClass;
  end;

  TXGCompItems = array of TXGCompItem;

  // Lista de componentes suportados
  TXGCompsRegistry = class(TObject)
  private
    fItems: TXGCompItems;
    fCount: integer;
  public
    Destructor destroy; override;
    procedure add(item: TXGCompItem); overload;
    procedure add(const tagName: string;  const CtrlClass: TComponentClass); overload;
    function indexOf(const name: string): integer;
    function getTagName(c: TComponentClass): string;
    function getCtrl(const name: string): TComponentClass;
    procedure Clear;
  end;

  // propriedades "extras" armazenadas para cada componente
  // pelo container do componente. As propriedades permitem
  // que não precisemos modificar todos os componentes da VCL / LCL
  // para que eles possam usar estilos, persistência, etc.
  // Além disso esse modo de funcionar permite que qualquer
  // componente de terceiro "plugado" ao XG automaticamente
  // ganhe todos esses poderes sem precisar de nenhuma alteração :)
  TXgExtraCompData = record
    Child: TComponent;
    CssClass: string;
    CssStyle: string;
    Persist:  string;
    Flex: integer;
  end;

  TXgExtraDataList = class(TObject)
  public
    Items: array of TXgExtraCompData;
    Count: integer;
  public
    destructor destroy; override;
    procedure  add(data: TXgExtraCompData);
    function   indexOf(obj: TComponent): integer;
    function  get(obj: TComponent): TXgExtraCompData;
  end;

  // Classe usada para guardar dados na propriedade objeto de
  // listboxes, combos, etc. Use NewItemValue...
  TItemValue = class(TComponent)
  private
    fValue: string;
  published
    Property value: string read fValue write fValue;
  end;

type
  TStringArray = array of String;

type
  TCSSProperty = record
    // nome da propriedade
    Name,
    // valor da propriedade
    Value,
    // unidade em que está o valor
    TypeOfValue: string;
  end;

  // array de propriedades CSS
  TCSSPropArray = array of TCSSProperty;

  // Tipo de estilo.
  // csClass -> uma classe. Exemplo: " .meubotaum {..."
  // csId -> põe o estilo em um ID. Exemplo: " #minhaid {..."
  // csTag -> põe o estilo em todas as tags referencial pelo nome da tag. Ex: " listbox {..."
  TCSSStyleType = (csClass, csId, csTag);

  // Um estilo CSS. Corresponde a estrutura "nome {propriedade:valor;}" dos aruqivos CSS
  TCSSStyle = record
    Name: string;
    ClassType: TCSSStyleType;
    Propertys: TCSSPropArray;
  end;

  TCSSStyleArray = array of TCSSStyle;

  // Lista de estilos. Use LoadFromFile para carregar um arquivo CSS.
  TCSSStyleList = Class(TObject)
  private
    // Carrega o conteúdo do arquivo, remove os comentários, e retorna conteúdo e length
    function OpenAndStrip(const FileName: string; Var S: string; Var Len: integer): boolean;
    // Coloca todas as propriedades de FromArr em ToArr, sobrescrevendo
    // as propriedades de ToArr conforme necessário
    procedure MergePropertys(var FromArr, ToArr: TCSSPropArray);
    // retorna o nome do seletor no caso da string ter um
    function getSelectorName(Var S: string): string;
    // Adiciona o nome do selector ao nome de uma propriedade. Assim, p. ex., font-family vira hoverfont-family...
    procedure AddSelectorToPropsNames(var a: TCSSPropArray; const SelectorName: string);
  public
    // é mais rápido como variável e aqui nós precisamos de velocidade máxima :)

    // permite acessar cada estilo carregado
    Items: TCSSStyleArray;
    // permite acessar o número de estilos
    Count: integer;
  public
    destructor destroy; override;
    // carrega um arquivo CSS
    procedure LoadFromFile(const FileName: string);
    // Limpa a lista
    procedure Clear;
    // Pega o index do item
    function IndexOf(const Name: string; const ClassType: TCSSStyleType): integer;
    // Pega todas as propriedades para um controle
    // juntado as propriedades da tag, classe, id e estilo inline (exatamente nesta ordem)
    function getCtrlPropertys(Id, ClassName, Style, tagName: string): TCSSPropArray;
  end;

type

  // Mensagens enviadas para os containers
  // cmChildsChanged --> informa que os componentes que ele contém forma modificados
  // cmXgLoaded --> informa que o xg acabou de criar a interface carregando um arquivo xg
  TXgContainerMessages = (cmChildsChanged, cmXgLoaded);

    // Toda classe que for usar a classe XG precisa implementar esta interface
  IXgOwner = interface(IInterface)
    ['{01FB7AFE-C5E4-4597-9CD3-9BF9E1006E74}']
    // Retorna a classe que contém a implementação dos métodos
    function getBind: TComponent;
    // Deve retornar o arquivo atual
    function  getURIPath(const uri: string): string;
    procedure openURI(const uri: string);
    // Carrega uma GUI de um arquivo XG
    procedure loadFromFile(FileName: string);
    // Seta o parent atual
    procedure setCurParent(Value: TWinControl);
    // Pega o parent atual
    function getCurParent: TWinControl;
    // Retorna os dados extras dos containers que tem como parent o próprio XG
    function getRootExtraData: TXgExtraDataList;
    // Retorna as propriedades CSS para determinado objeto
    function getCssPropertys(Id, ClassName, Style, tagName: string): TCSSPropArray;
  end;

// Aplica as propriedades localizadas do objeto
procedure applyLocale(obj: TControl); forward;
// Aplica o estilo do objeto
procedure applyStyle(obj: TControl); forward;

// Faz o parsing de propriedades e converte elas para um array
// alguns nomes de propriedades DEVEM ser convertidas aqui quando
// elas não existirem nos componentes da VCL ou do XG.
// A única exeção são o nome de propriedades de fontes. A função
// que as lê e converte para TFont espera encontrar nomes tipo font-family, font-weigth, etc.
function parseCssPropertys(const S: string): TCSSPropArray ; forward;

// Cria um novo valor para guardar strings em items de combos e listboxes.
// Ele será destruído automagicamente quando o programa for finalizado :)
function NewItemValue(value: string): TObject; forward;

// Separa em uma string em multiplas partes toda vez que encontra o delimitador
function ExplodeStr(S: string; const Delimiter: Char): TStringArray; forward;


{ Retorna a cor que é pedida na string.
  Formatos suportados:
  - HTML, ex: #000000
  - RGB, ex: RGB(0,0,0)
  - "OS", para pegar cores do sistema operacional. ex: OS.btnFace
  }
function getColor(S: string): TColor; forward;
// retorna o nome do arquivo de uma uri. Ex, se o uri for http://teste/ola.htm ele retornará 'ola.htm'
function getUriFileName(const uri: string): string; forward;
// Retorna o método pedido em "Method"
function  getMethod(const Method: string; const CompsOwner: TComponent; const Bind: TObject): TMethod; forward;
// Retorna o valor de uma propriedade de acordo com uma string no formato objeto.propriedade
function  getStrAsProperty(const Prop: string; const CompsOwner: TComponent; const Bind: TObject): string; forward;
// Interpreta propriedades no formato {objeto.propriedade} dentro de uma string
function  parsePropertys(const Value: string; const CompsOwner: TComponent; const Bind: TObject): string; forward;
// Seta uma propriedade de um objeto
procedure setProperty(obj: TComponent; tagName, propName, propValue: string); forward;

// Reimplementação de PosEx para compatibilidade com Delphi 6.
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;

var
  CompsReg: TXgCompsRegistry;
  TypeHandlersReg: TXgCompsRegistry;
  LastFile: string;
  LastFont: TFont;
  ItemValueOwner: TComponent;

implementation

uses XGContainers, Forms, FastStrings;

{ Rotinas }

function hasJustAlpha(const S: string): boolean;
Var
  I, Len: integer;
begin
  Result:= false;
  Len:= length(S);
  if Len = 0 then Exit;
  for I:= 1 to Len do
    if not (S[I] in ['a'..'z', 'A'..'Z']) then Exit;
  Result:= true;
end;

function hasJustNumbers(const S: string): boolean;
Var
  I, Len: integer;
begin
  Result:= false;
  Len:= length(S);
  if Len = 0 then Exit;
  for I:= 1 to Len do
    if not (S[I] in ['0'..'9']) then Exit;
  Result:= true;
end;

function getColor(S: string): TColor;
Var
  Len, I1, I2: integer;
begin
  S:= StringReplace(S, #32, '', [rfReplaceAll]);
  S:= StringReplace(S, #9, '', [rfReplaceAll]);
  Len:= length(S);
  Result:= clNone;
  if SameText('none', S) then
  begin
    Result:= clNone;
  end else
  if (Len > 9) and (SameText('RGB(', Copy(S, 1, 4))) then
  begin
    S:= Copy(S, 5, Len);
    Len:= Len - 5;
    setLength(S, Len);
    // o código acima remove "RGB(" e ")"
    I1:= PosEx(',', S, 1);
    I2:= PosEx(',', S, I1+1);
    if (I1 < 2) or (I2 <= I1) then
      raise Exception.Create('"rgb(' +S + ')" is not a suported value for a color.')
    else
      Result:= RGB(StrToInt(Copy(S, 1, I1-1)), StrToInt(Copy(S, I1+1, I2-1-I1)),
        StrToInt(Copy(S, I2+1, Len))) ;
  end else
  if (Len = 7) and (S[1] = '#') then
  begin
    Result :=  RGB(StrToInt('$'+Copy(S, 2, 2)), StrToInt('$'+Copy(S, 4, 2)),
      StrToInt('$'+Copy(S, 6, 2))) ;
  end else
  if hasJustAlpha(S) then
  begin
    Result:= StringToColor('cl' + s);
  end else
  begin
    try
      if hasJustNumbers(S) then
        Result:= TColor(StrToInt(S));
    except
      raise Exception.Create('"' +S + '" is not a suported value for a color.');
    end;
  end;
  setLength(S, 0);
end;

{ TXGCompsRegistry }

destructor TXGCompsRegistry.destroy;
begin
  Clear;
  inherited;
end;

procedure TXGCompsRegistry.add(item: TXGCompItem);
begin
  inc(fCount);
  setLength(fItems, fCount);
  fItems[fCount-1]:= item;
end;

procedure TXGCompsRegistry.add(const tagName: string;  const CtrlClass: TComponentClass);
var
  NewItem: TXGCompItem;
begin
  NewItem.tag:= tagName;
  NewItem.ctrl:= ctrlClass;
  add(NewItem);
end;

function TXGCompsRegistry.indexOf(const name: string): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I:= 0 to fCount -1 do
  begin
    if SameText(fItems[I].tag, name) then
    begin
      Result:= I;
      Break;
    end;
  end;
end;

function TXGCompsRegistry.getCtrl(const name: string): TComponentClass;
Var
  I: integer;
begin
  Result:= nil;
  I:= indexOf(name);
  if I > -1 then
    Result:= fItems[I].ctrl;
end;

procedure TXGCompsRegistry.Clear;
begin
  fCount:= 0;
  setLength(fItems, fCount);
end;


function TXGCompsRegistry.getTagName(c: TComponentClass): string;
Var
  I: integer;
begin
  Result:= 'XG';
  for I:= 0 to fCount -1 do
  begin
    if fItems[I].ctrl = C then
    begin
      Result:= fItems[I].tag;
      Break;
    end;
  end;
end;

function ExplodeStr(S: string; const Delimiter: Char): TStringArray;
Var
  I, tStart, tEnd, tCount, Len: integer;
begin
  len:= length(S);
  tStart:= 1;
  tCount:= 0;

  if Len = 0 then Exit;

  if S[Len] = Delimiter then
  begin
    Dec(Len);
    setLength(S, Len);
  end;

  for I:= 1 to Len do
  begin
    if S[I] = Delimiter then
    begin
      tEnd:= I - 1;
      inc(tCount);
      setLength(Result, tCount);
      Result[tCount-1] := copy(S, tStart, tEnd - tStart +1);
      tStart:= tEnd + 2;
    end;
  end;
  if tStart <= Len then
  begin
    inc(tCount);
    setLength(Result, tCount);
    Result[tCount-1] := copy(S, tStart, Len);
  end;
end;

function getMethod(const Method: string; const CompsOwner: TComponent; const Bind: TObject): TMethod;
Var
  Len: integer;
  a: TStringArray;
  obj: TComponent;
  propInfo : PPropInfo;
begin
  Result.Code:= nil;
  Result.Data:= nil;
  a:= nil;
  if Pos(',', Method) <> 0 then
  begin
    a:= ExplodeStr(Method, '.');
    Len:= length(a);
    if Len = 2 then
    begin
      obj:= CompsOwner.FindComponent(a[0]);
      if obj <> nil then
      begin
        Result.Data:= Pointer(obj);
        propInfo := getPropInfo(obj, Trim(a[1]));
        if propInfo <> nil then
          if propInfo.PropType^.Kind = tkMethod then
            Result.Code:= obj.MethodAddress(a[1]);
      end;
    end;
  end else
  begin
    if Bind <> nil then
    begin
      Result.Code:= Bind.MethodAddress(Method);
      Result.Data:= Pointer(Bind);
    end;
  end;
  if a <> nil then
    setLength(a, 0);
end;


function  getStrAsProperty(const Prop: string; const CompsOwner: TComponent; const Bind: TObject): string;
Var
  Len: integer;
  a: TStringArray;
  obj: TComponent;
  propInfo : PPropInfo;
begin
  Result:= prop;
  // espera receber uma string no formato objeto.propriedade
  a:= ExplodeStr(prop, '.');
  Len:= length(a);
  if Len = 2 then
  begin
    obj:= CompsOwner.FindComponent(a[0]);
    if obj <> nil then
    begin
      propInfo := getPropInfo(obj, Trim(a[1]));

      if propInfo = nil then
        raise Exception.Create('Can not find "' +prop+ '".');

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
    end;
  end;
  setLength(a, 0);
end;


function parsePropertys(const Value: string; const CompsOwner: TComponent; const Bind: TObject): string;
Var
  I, len, open: integer;
  S, R: string;
  sValue: string;
begin
  len:= length(Value);
  Result:= Copy(Value, 1, len);
  sValue:= Result;
  open:= 0;

  for I:=1 to Len do
  begin
    case sValue[I] of
      '{': if open = 0 then
             open:= I
           else
             open:= 0;
      '}' : if open <> 0 then
            begin
              S:= Copy(sValue, Open+1, I-1-Open);
              R:= getStrAsProperty(S, CompsOwner, Bind);
              if R <> '' then
                Result:= StringReplace(Result, '{'+S+'}', R, [rfReplaceAll]);
            end;
       else
       // só permitir ._abcderfghijklmnopqrstuvxyz0123456789
       // tava dando problema usando in [..], então foi no braço mesmo, hehhe
       if ((ord(sValue[I]) < 48) and (ord(sValue[I]) <> 46)) or
          ((ord(sValue[I]) > 57) and (ord(sValue[I]) < 65)) or
          ((ord(sValue[I]) > 90) and (ord(sValue[I]) < 97) and (ord(sValue[I]) <> 95)) or
           (ord(sValue[I]) > 122) then
         open:= 0;
    end;
  end;
end;

procedure setProperty(obj: TComponent; tagName, propName, propValue: string);
var
  propInfo : PPropInfo;
  M: TMethod;
  S: string;
begin

  S:= AnsiUpperCase(propName);
  // HACK não há outra maneira de saber se a propriedade é uma cor?!
  if (Pos('COLOR', S) <> 0) or (S= 'FILL') or (S= 'STROKE') then
    propValue:= IntToStr(getColor(propValue));

  // Suporte a ID
  if SameText(propName, 'id') then
    propName:= 'name';

  propInfo := getPropInfo(obj, propName);

  if propInfo = nil then
    Exit;

  case propInfo.PropType^.Kind of
    tkString, tkLString, tkWString: setStrProp(obj, propInfo, propValue);
    tkInteger, tkChar, tkWChar: setOrdProp(obj, propInfo, strToIntDef(propValue, 0));
    tkFloat:  SetFloatProp(obj, propInfo, strToFloat(propValue));
    tkSet: SetSetProp(obj, propInfo, propValue);
    tkVariant: SetVariantProp(obj, propInfo, propValue);
    // Bolleans
    tkEnumeration: SetEnumProp(obj, propInfo, propValue);
    tkInt64:  SetInt64Prop(obj, propInfo, strToInt64(propValue));
    tkMethod:
      begin
        M:= getMethod(propValue, obj.Owner, (obj.Owner As IXgOwner).getBind);
        if (M.Code <> nil) and (M.Data <> nil) then
          setMethodProp(obj, propInfo, M);
      end;
    // TODO Como suportar arrays e classes?!
  end;
end;

procedure applyStyle(obj: TControl);
Var
  I: integer;
  xg: IXgOwner;
  props: TCSSPropArray;
  data: TXgExtracompData;
  tagName: string;
  fontp: TFont;
  hoverfont: TFont;
  Len: integer;
begin
  // Pegamos as propriedades
  xg:= obj.Owner As IXgOwner;
  // containers guardam os dados extras para si mesmos :)
  if isXgContainer(obj.Parent) then
    data:= (obj.parent As IXgContainer).getExtraData(obj)
  else if obj.Parent = obj.Owner then
    data:= xg.getRootExtraData.get(obj);

  if data.Child = nil then Exit;

  tagName:= CompsReg.getTagName( TComponentClass(obj.ClassType) );
  props:= xg.getCssPropertys(obj.Name, data.CssClass, data.CssStyle, tagName);

  { Defaults do sistema operacional }
  LastFont.Size:= 10;
  LastFont.Color:= clBlack;
  LastFont.Name:= 'Tahoma';
  LastFont.Style:= [];

  HoverFont:= TFont.Create;
  HoverFont.Assign(LastFont);

  Len:= length(props);
  if Len= 0 then Exit;

  // lemos e aplicamos as propriedades
  for I:= 0 to Len -1 do
  begin
    if props[I].Name = 'fontfamily' then
      LastFont.Name:= props[I].Value
    else
    if props[I].Name = 'fontweight' then
    begin
      if (props[I].Value = 'bold') or (props[I].Value = 'bolder') then
      begin
        LastFont.Style:= LastFont.Style + [fsbold];
      end;
    end else
    if props[I].Name = 'fontstyle' then
    begin
      if (props[I].Value = 'italic') then
      begin
        LastFont.Style:= LastFont.Style + [fsitalic];
      end;
    end else
    if props[I].Name = 'textdecoration' then
    begin
      if (props[I].Value = 'underline') then
      begin
        LastFont.Style:= LastFont.Style + [fsUnderline];
      end
      else if (props[I].Value = 'line-through') then
      begin
        LastFont.Style:= LastFont.Style + [fsStrikeOut];
      end;
    end else
    if props[I].Name = 'fontcolor' then
      LastFont.Color:= getColor(props[I].Value)
    else
    if props[I].Name = 'fontsize' then
      lastFont.Size := StrtoInt(props[I].Value)

    else if props[I].Name = 'hoverfontfamily' then
      hoverFont.Name:= props[I].Value
    else
    if props[I].Name = 'hoverfontweight' then
    begin
      if (props[I].Value = 'bold') or (props[I].Value = 'bolder') then
      begin
        hoverFont.Style:= LastFont.Style + [fsbold];
      end;
    end else
    if props[I].Name = 'hoverfontstyle' then
    begin
      if (props[I].Value = 'italic') then
      begin
        hoverFont.Style:= LastFont.Style + [fsitalic];
      end;
    end else
    if props[I].Name = 'hovertextdecoration' then
    begin
      if (props[I].Value = 'underline') then
      begin
        hoverFont.Style:= LastFont.Style + [fsUnderline];
      end
      else if (props[I].Value = 'linethrough') then
      begin
        hoverFont.Style:= LastFont.Style + [fsStrikeOut];
      end;
    end else
    if props[I].Name = 'hoverfontcolor' then
      hoverFont.Color:= getColor(props[I].Value)
    else
    if props[I].Name = 'hoverfontsize' then
      hoverFont.Size := StrtoInt(props[I].Value)
    else if (props[I].Name = 'minwidth' ) and (obj is TControl)then
      (obj As TControl).Constraints.MinWidth:= StrToIntDef(props[I].Value ,0)
    else if (props[I].Name = 'maxwidth') and (obj is TControl) then
      (obj As TControl).Constraints.MaxWidth:= StrToIntDef(props[I].Value ,0)
    else if (props[I].Name = 'minheight') and (obj is TControl) then
      (obj As TControl).Constraints.MinHeight:= StrToIntDef(props[I].Value ,0)
    else if (props[I].Name = 'maxheight') and (obj is TControl) then
      (obj As TControl).Constraints.MaxHeight:= StrToIntDef(props[I].Value ,0)
   else if (props[I].Name =  'width') and (obj is TControl) then
      (obj As TControl).Width:= StrToIntDef(props[I].Value ,0)
    else if (props[I].Name = 'height') and (obj is TControl) then
      (obj As TControl).Height:= StrToIntDef(props[I].Value ,0)
    else
      setProperty(obj, tagName, props[I].Name, props[I].Value);
  end;

  if IsPublishedProp(obj, 'Font') then
  begin
    fontp:= getObjectProp(obj, 'Font', TFont) As TFont;
    if fontp <> nil then
      fontp.Assign(LastFont);
  end;

  if IsPublishedProp(obj, 'HoverFont') then
  begin
    fontp:= getObjectProp(obj, 'HoverFont', TFont) As TFont;
    if fontp <> nil then
      fontp.Assign(HoverFont);
  end;

  HoverFont.Free;

  setLength(props, 0);
end;

procedure applyLocale(obj: TControl);
begin
  //
end;


function NewItemValue(value: string): TObject;
var
  iv: TItemValue;
begin
  iv:= TItemValue.Create(nil);
  iv.value:= value;
  Result:= TObject(iv);
end;

//
// C S S
//

function parseCssPropertys(const S: string): TCSSPropArray;
Var
  I, J, Len, Count: integer;
  a, b: TStringArray;
  N, V: string;
  props: TCSSPropArray;
  dots: integer;
begin
  if S = '' then Exit;
  Count:= 0;
  setLength(props, 0);
  // propriedades em CSS são separadas por ;
  a:= ExplodeStr(S, ';');
  Len:= length(a);
  if Len = 0 then Exit;

  for I:= 0 to Len-1 do
  begin
    // o nome da propriedade e o valor são separados por :
    dots:= AnsiPos(':', a[I]);

    if dots <> 0 then
    begin
      N:= Trim(copy(a[I], 1, dots-1));
      V:= Trim(copy(a[I], dots+1, length(a[I])-dots));
      // removemos as aspas que possam existir nos parâmentros
      V:= StringReplace(V, '"', '', [rfReplaceAll]);
      V:= Trim(StringReplace(V, ''+#39, '', [rfReplaceAll]));
      setLength(props, Count+1);
      props[Count].Name:= N;
      props[Count].Value:= V;
      inc(Count);

      if props[Count-1].Name = 'background-color' then
      begin
        props[Count-1].Name := 'color';
      end else
      if props[Count-1].Name = 'color' then
      begin
        // Color é transformado em fontcolor para não termos problemas
        // com a propriedade "color" de vários componentes da VCL
        props[Count-1].Name := 'fontcolor';
      end else
      if props[Count-1].Name = 'fontfamily' then
      begin
        // o nome das fontes pedidas são separados por ,
        b:= ExplodeStr(props[Count-1].Value, ',');
        for J:= 0 to High(b) do
        begin
          if screen.Fonts.IndexOf(Trim(b[J])) > -1 then
          begin
            props[Count-1].Value:= Trim(b[J]);
            Break;
          end;
        end;
        setLength(b, 0);
      end else
      begin
        // propriedades como Stroke-Width viram strokewidth
        props[Count-1].Name:= StringReplace(props[Count-1].Name, '-', '', [rfReplaceAll]);
      end;
      // Tipo
      J:= length(props[Count-1].Value);
      if J > 1 then
      begin
        if props[Count-1].Value[1] in ['0'..'9','-'] then
        begin
          // pegamos os dois últimos caracteres, onde devem estar a unidade
          V:= Copy(props[Count-1].Value, J-1, 2);
          if V[2] = '%' then
          begin
            props[Count-1].Value:= Copy(props[Count-1].Value, 1, J-1);
            props[Count-1].TypeOfValue:= '%';
          end else
          begin
            if (V = 'em') or (V = 'px') or (V = 'pt') or (V = 'in') then
            begin
              props[Count-1].Value:= Copy(props[Count-1].Value, 1, J-2);
              props[Count-1].TypeOfValue:= V;
            end;
          end;
        end else
        // Suporte a valores do tipo "url(http://algo.com)"
        if (SameText('url(', copy(props[Count-1].Value, 1,4)))
          and (props[Count-1].Value[J] = ')') then
        begin
          V:= copy(props[Count-1].Value, 5, J-5);
          props[Count-1].Value:= V;
          props[Count-1].TypeOfValue:= 'url';
        end;
      end;
    end;
    setLength(b, 0);
  end;
  setLength(a, 0);
  Result:= props;
end;


{ TCSSStyleList }

procedure TCSSStyleList.AddSelectorToPropsNames(var a: TCSSPropArray;
  const SelectorName: string);
Var
  I, Len: integer;
begin
  if SelectorName= '' then Exit;
  Len:= length(a);
  for i:=0 to Len-1 do
    a[I].Name:= SelectorName + a[I].Name;
end;

procedure TCSSStyleList.Clear;
begin
  Count:= 0;
  setLength(items, 0);
end;

destructor TCSSStyleList.destroy;
begin
  Clear;
  inherited;
end;

function TCSSStyleList.getCtrlPropertys(Id, ClassName, Style,
  tagName: string): TCSSPropArray;
Var
  I: integer;
  a: TCSSPropArray;
begin
  // propriedades da tag
  I:= IndexOf(tagName, csTag);
  if I > -1 then
    Result:= Copy(items[I].Propertys);
  // propriedades da classe
  I:= IndexOf(ClassName, csClass);
  if I > -1 then
    MergePropertys(Items[I].Propertys, Result);
  // propriedades da ID
  I:= IndexOf(Id, csId);
  if I > -1 then
    MergePropertys(Items[I].Propertys, Result);
  // propriedades do estilo inline
  if Style <> '' then
  begin
    a:= parseCssPropertys(Style);
    MergePropertys(a, Result);
    setLength(a, 0);
  end;
end;

function TCSSStyleList.getSelectorName(Var S: string): string;
Var
  I: integer;
begin
  Result:='';
  I:= pos(':', S);
  if I <> 0 then
  begin
    Result:= Copy(S, I+1, length(S));
    S:= Copy(S, 1, I-1);
  end;
end;

function TCSSStyleList.IndexOf(const Name: string;
  const ClassType: TCSSStyleType): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I:= 0 to Count -1 do
  begin
    if (Items[I].ClassType = ClassType) and (SameText(Items[I].Name, Name)) then
    begin
      Result:= I;
      Break;
    end;
  end;
end;

procedure TCSSStyleList.LoadFromFile(const FileName: string);
Var
  I, Len, NameStart: integer;
  cStart, cEnd: integer;
  props, nameprops: TCSSPropArray;
  CType: TCSSStyleType;
  Names: TStringArray;
  S: string;
begin
  Clear;
  if not OpenAndStrip(FileName, S, Len) then Exit;
  if Len = 0 then Exit;

  cStart:= Pos('{', S);
  While cStart <> 0 do
  begin
    cEnd:= PosEx('}', S, cStart);
    if cEnd <> 0 then
    begin
      // Precisamos achar onde está o nome
      for NameStart:= cStart downto 1 do
        if S[NameStart] in ['>', '<','}', ';','/', '\','+','!'] then
          Break;
      // pegamos os nomes. Formato esperado: button, .teste, #MeuBotaum { font-family: bla, bla;}
      Names:= ExplodeStr(Trim(Copy(S, NameStart+1, cStart-NameStart-1)), ',');
      Len:= length(Names);
      if Len > 0 then
      begin
        props:= parseCSSPropertys(copy(S, cStart+1, cEnd-cStart-1));
        for I:= 0 to Len-1 do
        begin
          Names[I]:= Trim(Names[I]);
          nameprops:= copy(props);
          AddSelectorToPropsNames(nameprops, getSelectorName(Names[I]));
          if Names[I] <> '' then
          begin
            case Names[I][1] of
              '#': CType:= csId;
              '.': CType:= csClass;
              else CType:= csTag;
            end;
            // removemos o primeiro caracter, se necessário
            if Ctype <> csTag then
              Names[I]:= Copy(Names[I], 2, 150);
            // guardamos o index
            NameStart:= indexOf(Names[I], CType);
            if NameStart = -1 then
            begin
              inc(Count);
              setLength(items, Count);
              items[Count-1].Name:= Names[I];
              items[Count-1].ClassType:= CType;
              items[Count-1].Propertys:= nameprops;
            end else
            begin
              // Se não retornou -1, temos que fazer o merde das propriedades
              // para dentro da classe anterior com o mesmo nome
              MergePropertys(nameprops, items[NameStart].Propertys);
            end;
          end;

          setLength(nameprops, 0);
        end;

        setLength(props, 0);
      end;

      setLength(Names, 0);
    end;

    cStart:= PosEx('{', S, cStart+1);
  end;


end;

procedure TCSSStyleList.MergePropertys(var FromArr,
  ToArr: TCSSPropArray);
Var
  FromLen, ToLen, I, J, Target: integer;
begin
  FromLen:= length(FromArr);
  if FromLen = 0 then Exit;
  ToLen:= length(ToArr);

  for I:= 0 to FromLen -1 do
  begin
    target := -1;
    for J:= 0 to ToLen - 1 do
    begin
      if SameText(FromArr[I].Name, toArr[J].Name) then
      begin
        target:= J;
        Break;
      end;
    end;
    if target = -1 then
    begin
      inc(ToLen);
      setLength(ToArr, ToLen);
      ToArr[toLen-1].Name:= FromArr[I].Name;
      ToArr[toLen-1].Value:= FromArr[I].Value;
      ToArr[toLen-1].TypeOfValue:= FromArr[I].TypeOfValue;
    end else
    begin
      // juntamos o atual com o target
      ToArr[target].Value:= FromArr[I].Value;
      ToArr[target].TypeOfValue:= FromArr[I].TypeOfValue;
    end;
  end;
end;

function TCSSStyleList.OpenAndStrip(const FileName: string; var S: string;
  var Len: integer): boolean;
Var
  cStart, cEnd: integer;
  FS: TFileStream;
begin
  Result:= false;

  if not FileExists( Filename ) then
    Exit;
  // Carregamos o arquivo em S
  FS := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
  try
    Len := FS.Size;
    SetLength( S, Len );
    FS.Read( S[1], Len );
  finally
    FS.Free;
  end;
  // Removemos os comentários
  cStart:= Pos('/*', S);
  While cStart <> 0 do
  begin
    cEnd:= PosEx('*/', S, cStart);
    if cEnd <> 0 then
      Delete(S, cStart, cEnd - cStart+2);
    cStart:= PosEx('/*', S, cStart);
  end;

  Len:= length(S);
  Result:= true;
end;

{ TXgExtraDataList }

procedure TXgExtraDataList.add(data: TXgExtraCompData);
begin
  inc(Count);
  setLength(items, count);
  items[count-1]:= data;
end;

destructor TXgExtraDataList.destroy;
begin
  setLength(items, 0);
  inherited;
end;

function TXgExtraDataList.get(obj: TComponent): TXgExtraCompData;
Var
  I: integer;
begin
  Result.Child:= nil;
  I:= indexOf(obj);
  if I > -1 then
  begin
    Result.Child:=    items[I].Child;
    Result.CssClass:= items[I].CssClass;
    Result.CssStyle:= items[I].CssStyle;
    Result.Persist:=  items[I].Persist;
    Result.Flex:=     items[I].Flex;
  end;
end;

function TXgExtraDataList.indexOf(obj: TComponent): integer;
Var
  I: integer;
begin
  Result:= -1;
  for I:= 0 to Count -1 do
  begin
    if Items[I].Child = obj then
    begin
      Result:= I;
      Break;
    end;
  end;
end;

function getUriFileName(const uri: string): string;
Var
  a: TStringArray;
  i: integer;
begin
  Result:= '';
  i:= Pos('/', uri);
  if i = length(uri) then Exit;

  if i <> 0 then
    a:= ExplodeStr(uri, '/')
  else
    a:= ExplodeStr(uri, '\');
  i:= length(a);

  if i > 0 then
    Result:= a[i-1];

  setLength(a, 0);
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
begin
  if Offset < 1 then
    Offset:= 1;
  Result:= FastPos(S, SubStr, length(S), length(SubStr), Offset);
  if Result < 1 then
  Result:= 0;
end;

initialization
  ItemValueOwner:= TComponent.Create(nil);
  CompsReg:= TXgCompsRegistry.Create;
  TypeHandlersReg:= TXgCompsRegistry.Create;

finalization
  if assigned(LastFont) then
    FreeAndNil(LastFont);
  FreeAndNil(ItemValueOwner);
  FreeAndNil(CompsReg);
  FreeAndNil(TypeHandlersReg);
end.
