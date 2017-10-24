unit XgCSSParser;

interface

uses Windows, SysUtils, Classes, XGUtils;

type
  TCSSProperty = record
    // nome da propriedade
    Name,
    // valor da propriedade
    Value,
    // unidade em que está o valor
    TypeOfValue: string;
  end;

  TCSSPropArray = array of TCSSProperty;

  TCSSClassType = (ctClass, ctId, ctTag);

  TCSSClass = record
    Name: string;
    ClassType: TCSSClassType;
    Propertys: TCSSPropArray;
  end;

  TCSSClassArray = array of TCSSClass;

  TCSSClassList = Class(TObject)
  private
    // Carrega o conteúdo do arquivo, remove os comentários, e retorna conteúdo e length
    function OpenAndStrip(const FileName: string; Var S: string; Var Len: integer): boolean;
    // Coloca todas as propriedades de FromArr em ToArr, sobrescrevendo
    // as propriedades de ToArr conforme necessário
    procedure MergePropertys(FromArr, ToArr: TCSSPropArray);
  public
    // é mais rápido como variável...
    Items: TCSSClassArray;
    Count: integer;
  public
    destructor destroy; override;
    // carrega um arquivo CSS
    procedure LoadFromFile(const FileName: string);
    // Limpa a lista
    procedure Clear;
    // Pega o index do item
    function IndexOf(const Name: string; const ClassType: TCSSClassType): integer;
    // Pega todas as propriedades para um controle
    // juntado as propriedades da tag, classe, id e estilo inline (exatamente nesta ordem)
    function getCtrlPropertys(Id, ClassName, Style, tagName: string): TCSSPropArray;
  end;

  // Faz o parsing de propriedades e converte elas para um array
  // alguns nomes de propriedades DEVEM ser convertidas aqui quando
  // elas não exiveverm nos componentes da VCL ou do XG.
  // A única exeção são o nome de propriedades de fontes. A função
  // que as lê e converte para TFont espera encontrar nomes tipo font-family, font-weigth, etc.
  function parseCssPropertys(const S: string): TCSSPropArray ; forward;



implementation

uses Forms;

function parseCssPropertys(const S: string): TCSSPropArray;
Var
  I, J, Len, Count: integer;
  a, b: TStringArray;
  N, V: string;
  props: TCSSPropArray;
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
    b:= ExplodeStr(a[I], ':');
    if length(b) = 2 then
    begin
      N:= Trim(b[0]);
      V:= Trim(b[1]);
      // removemos as aspas que possam existir nos parâmentros
      V:= StringReplace(V, '"', '', [rfReplaceAll]);
      V:= Trim(StringReplace(V, ''+#39, '', [rfReplaceAll]));
      setLength(props, Count+1);
      props[Count].Name:= N;
      props[Count].Value:= V;
      inc(Count);
      setLength(b, 0);

      if props[Count-1].Name = 'background-color' then
      begin
        props[Count-1].Name := 'backcolor';
      end else
      if props[Count-1].Name = 'color' then
      begin
        // Color é transformado em font-color para não termos problemas
        // com a propriedade "color" de vários componentes da VCL
        props[Count-1].Name := 'font-color';
      end else
      if props[Count-1].Name = 'font-family' then
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
          end;
        end;
      end;
    end;
    setLength(b, 0);
  end;
  setLength(a, 0);
  Result:= props;
end;


{ TCSSClassList }

procedure TCSSClassList.Clear;
begin
  Count:= 0;
  setLength(items, 0);
end;

destructor TCSSClassList.destroy;
begin
  Clear;
  inherited;
end;

function TCSSClassList.getCtrlPropertys(Id, ClassName, Style,
  tagName: string): TCSSPropArray;
Var
  I: integer;
  a: TCSSPropArray;
begin
  // propriedades da tag
  I:= IndexOf(tagName, ctTag);
  if I > -1 then
    Result:= Copy(items[I].Propertys);
  // propriedades da classe
  I:= IndexOf(ClassName, ctClass);
  if I > -1 then
    MergePropertys(Items[I].Propertys, Result);
  // propriedades da ID
  I:= IndexOf(Id, ctId);
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

function TCSSClassList.IndexOf(const Name: string;
  const ClassType: TCSSClassType): integer;
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

procedure TCSSClassList.LoadFromFile(const FileName: string);
Var
  I, Len, NameStart: integer;
  cStart, cEnd: integer;
  props: TCSSPropArray;
  CType: TCSSClassType;
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
          if Names[I] <> '' then
          begin
            case Names[I][1] of
              '#': CType:= ctId;
              '.': CType:= ctClass;
              else CType:= ctTag;
            end;
            // removemos o primeiro caracter, se necessário
            if Ctype <> ctTag then
              Names[I]:= Copy(Names[I], 2, 150);
            // guardamos o index
            NameStart:= indexOf(Names[I], CType);
            if NameStart = -1 then
            begin
              inc(Count);
              setLength(items, Count);
              items[Count-1].Name:= Names[I];
              items[Count-1].ClassType:= CType;
              items[Count-1].Propertys:= Copy(Props);
            end else
            begin
              // Se não retornou -1, temos que fazer o merde das propriedades
              // para dentro da classe anterior com o mesmo nome
              MergePropertys(Props, items[NameStart].Propertys);
            end;
          end;
        end;

        setLength(props, 0);
      end;

      setLength(Names, 0);
    end;

    cStart:= PosEx('{', S, cStart+1);
  end;


end;

procedure TCSSClassList.MergePropertys(FromArr,
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

function TCSSClassList.OpenAndStrip(const FileName: string; var S: string;
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

end.
