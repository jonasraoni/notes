unit XGTypeHandlers;

interface

uses Windows, SysUtils, Classes, LibXMLParser, XGUtils;

type
  // Todo typeHandler deve implementar esta interface
  // Lembre-se que logo após as tags serem lidas, o TypeHandler
  // será destruído.
  IXgTypeHandler = interface(Iinterface)
    ['{CB8C1B76-1734-4709-B879-684CE45BAAED}']
    // Repassa tags - o typehandler deve ler a tag atual e as tags filhas sozinho :)
    procedure ReadTags(tagName: string; const xml: TXMLParser; const Component, Container, Owner: TComponent);
  end;

  { Leitor para stringlists }
  TXgStrListTypeHandler = class(TComponent, IXgTypeHandler)
  private
    fComp: Tcomponent;
    fContainer: TComponent;
    fOwner: TComponent;
    function getStrList: TStrings;
  public
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure ReadTags(tagName: string; const xml: TXMLParser; const Component, Container, Owner: TComponent);
  end;

implementation

uses typInfo, StdCtrls;

{ TXgStrListTypeHandler }

function TXgStrListTypeHandler._AddRef: Integer;
begin
  Result:= -1;
end;

function TXgStrListTypeHandler._Release: Integer;
begin
  Result:= -1;
end;

procedure TXgStrListTypeHandler.ReadTags(tagName: string; const xml: TXMLParser; const Component, Container, Owner: TComponent);
Var
  SL: TStrings;
  ItemTagName: string;
  sTagName: string;
  T, V: string;
begin
  fComp:= Component;
  fContainer:= Container;
  fOwner:= Owner;
  // recebe Items, pega Item
  sTagName:= tagName;
  ItemTagName:= copy(tagName, 1, length(tagName)-1);
  SL:= getStrList;
  if SL = nil then
  begin
    // lê até o fim da tag e sai
    while xml.Scan do
      if (xml.CurPartType = ptEndTag) and (SameText(xml.CurName, stagName)) then
        Exit;
  end;

  sl.BeginUpdate;
  try
    while xml.Scan do
    begin
      if (xml.CurPartType = ptEndTag) and (SameText(xml.CurName, stagName)) then
        break;
      if (xml.CurPartType = ptStartTag) and (SameText(xml.CurName, ItemTagName)) then
      begin
        V:= Copy(xml.CurAttr.Value('value'), 1, High(integer));
        if V <> '' then
          V:= parsePropertys(V, fOwner, (fOwner as IXgOwner).getBind);
        if XMl.Scan then
        begin
          if xml.CurPartType = ptContent then
          begin
            T:= xml.CurContent;
            T:= Copy(T, 1, length(T));
            T:= parsePropertys(T, fOwner, (fOwner as IXgOwner).getBind);
            if V <> '' then
              sl.AddObject(T, NewItemValue(V))
            else
              sl.Add(T);
          end else
          if (xml.Scan) and (xml.CurPartType = ptCData) then
          begin
            T:= xml.CurContent;
            T:= Copy(T, 1, length(T));
            if V <> '' then
              sl.AddObject(T, NewItemValue(V))
            else
              sl.Add(T);
          end;
        end;
      end;
    end;
  finally
    sl.EndUpdate;
    if SL.Count > 0 then
    begin
      if Component is TComboBox then
      (Component As TComboBox).ItemIndex:= 0;
    end;
  end;
end;

function TXgStrListTypeHandler.getStrList: TStrings;
var
  propInfo : PPropInfo;
  o: TObject;
begin
  Result:= nil;
  propInfo:= getPropInfo(fComp, 'Items', [tkClass]);
  if propInfo = nil then
    propInfo:= getPropInfo(fComp, 'Lines', [tkClass]);
  if propInfo = nil then
    Exit;
  o:= GetObjectProp(fComp, propinfo);
  if o is TStrings then
    Result:= TStrings(o);
end;

end.
