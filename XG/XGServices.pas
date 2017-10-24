unit XGServices;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, LibXmlParser, Graphics,
  strUtils, ExtCtrls, XGUtils, XG;

type

  { Interface que todos os serviços devem implementar.  }
  IXgService = interface(IInterface)
  ['{6A8DD925-7060-4079-9B9C-10AFD1B4B74C}']
    // Chamado assim que o XG percebe que se trata de um serviço
    procedure InitService(const XG: TXG; const Bind: TComponent;
      const xml: TXMlParser);
    // Chamado quando o XG termina de ler as proprieades do serviço
    procedure endReadServicePropertys;
    // Chamado quando o XG termina de ler todo o arquivo XML
    procedure endReadPage(Var CanFree: boolean);
    // Permite ao serviço proibir que multiplas instâncias de si mesmo
    // sejam criadas. Se o serviço proibir múltiplas instâncias, ao invés
    // de criar uma nova instância, XG irá apenas chamar InitService novamente
    // e reler as propriedades da úninca instância já criada. Serviços que
    // possuem instâncias únicas ganham o nome de sua própria tag e podem assim
    // ser referenciados usando ela.
    function  allowMultipleInstances: boolean;
  end;


//function isXgService(const tagName: string): boolean; overload;
function isXgService(const obj: TComponent): boolean; //overload;

implementation

function isXgService(const obj: TComponent): boolean;
begin
  Result:= false;
//  if obj is TXgDrawingService then
//    Result:= true;
  if obj.GetInterfaceEntry(StringToGUID('{6A8DD925-7060-4079-9B9C-10AFD1B4B74C}')) <> nil then
    Result:= true;
end;




{
  TXgDrawingService - serviço de desenho :)

  TXgTranslationService - serviço de tradução
    cada string traduzida é uma var

  TXgHTTPService - baixa arquivos da internet

  TXgXMLReaderService

  TXgSimpleScriptService

  TXgFileReaderService

  TXgFileSearchService

}
end.