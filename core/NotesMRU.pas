//
//    NotesMRU - classes para lidar com listas e menus MRU
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
@abstract(NotesMRU - classes para lidar com listas e menus MRU.)
@author(Anderson R. Barbieri <notesnr@ig.com.br>)
@author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
@author(Kambiz <kambiz@delphiarea.com>)
Essa unit contém uma modificação do componente TMRUFiles
feito por Kambiz. O componente foi transformado em duas
classes permitindo tanto a manipulação de listas de itens
recentes (e neste caso a classe evita que itens duplicados
sejam armazenados) quanto menus. Estas classes são usadas
para guardar os arquivos recentes do Notes.
*)
unit NotesMRU;

interface

uses
  Windows, Classes, Menus, Dialogs;

type
{ TbMRUList é uma classe que armazena uma lista
de strings de forma a evitar que itens duplicados
sejam salvos. Use para manter itens recentes ou
algo do gênero.}
  TNotesMRU = class(TObject)
  private
    function GetItemsCount: Integer;
  protected
    FList: TStringList;
    FMaxItens: Byte;
    procedure SetMaxItens(MaxValue: Byte);
  public
    {Método construtor}
    constructor Create;
    {Método destrutor}
    destructor Destroy; override;
    { Adiciona um novo item (string) a lista. Caso já exista um item
      igual a ele armazenado o item antigo será deletado e o novo ficará
      no topo da lista.}
    procedure Add(const Item: string);
    { Remove um item da lista.}
    procedure Remove(const Item: string);
    { Carrega a lista de um arquivo.}
    function LoadFromFile(const AFilename: string ): Boolean;
    { Carrega a lista de uma string.}
    procedure LoadFromString(const AString: string);
    { Salva a lista para um arquivo.}
    procedure SaveToFile( const FileToSave: string );
    { Permite ler os items da lista. }
    property List: TStringList read FList;
    { Número máximo de itens da lista, de 0 a 255.}
    property MaxItens: Byte read FMaxItens write SetMaxItens;
    { Retorna o número de itens armazenados. Esta propriedade é read-only.}
    property ItemsCount: Integer read GetItemsCount;
  end;

{ Você deve criar um método do tipo TMRUClickEvent e especificá-lo no
 evento OnClick da classe @link(TNotesMRUMenu). Use FileName para saber qual
 o arquivo recente escolhido pelo usuário.}
  TNotesMRUClickEvent = procedure(Sender: TObject; const FileName: String) of object;

{ Classe que armazena uma lista de arquivos recentes
 e cria os itens do menu de arquivos recentes.}
  TNotesMRUMenu = class(TNotesMRU)
  private
    FMenuItem: TMenuItem;
    FOwner: TComponent;
    ItemsCount: Word;
    FOnClick: TNotesMRUClickEvent;
    FCaptionWPath: Boolean;
    procedure SetMenuItem(Value: TMenuItem);
    procedure MenuItemClicked(Sender: TObject);
    procedure ClearMenuItems;
  public
    constructor Create( const AOwner: TComponent = nil; const AMenuItem: TMenuItem = nil; const OnClickHandler: TNotesMRUClickEvent = nil; const ListFilename: string = ''; const AMaxItems: Integer = 15 );
    { Limpa os menus e todos os itens do histórico.}
    procedure Clear;
    { Você precisa passar um item de menu para esta propriedade.
     Abaixo dele aparecerão os itens mru criados pela classe.}
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    { Atualiza os menus criados pela classe. Você deve chamar este
     método sempre que adcionar, remover ou carregar itens. Você pode
     fazer várias modificações nos itens e só ao final chamar este método. }
    procedure ReloadMenus;
    { Permite configurar o método que será chamado quando o usuário clicar
     em um dos itens criados pela classe. Especifique um método antes de
     criar os menus. No caso dos menus já terem sido criados, chame o método
     @link(ReloadMenus).}
    property OnClick: TNotesMRUClickEvent read FOnClick write FOnClick;
    { Se true, os itens mostrarão o caminho completo dos arquivos. Se false
     a função da RTL ExtractFileName será usada e a caption dos itens mostrará
     apenas o nome do arquivo com extensão.}
    property CaptionWithPath: Boolean read FCaptionWPath write FCaptionWPath;
  end;

implementation

uses
  SysUtils, Forms, NotesConfig, NotesGlobals;


{ TNotesMRU }

constructor TNotesMRU.Create;
begin
  inherited;
  FList := TStringList.Create;
  FMaxItens := 20;
end;

destructor TNotesMRU.Destroy;
begin
  FList.Free;
  inherited;
end;

function TNotesMRU.GetItemsCount: Integer;
Begin
  Result := FList.Count;
End;

procedure TNotesMRU.Add(const Item: String);
var
  I: Integer;
begin
  if Item <> EmptyStr then
  begin
    List.BeginUpdate;
    Try
      for I := 0 to List.Count-1 do
      begin
        if CompareText(List[I], Item) = 0 then
        begin
          List.Delete(I);
          Break;
        end;
      end;
      while List.Count >= MaxItens do
      Begin
        List.Delete(MaxItens-1);
      End;
      List.Insert(0, Item);
    Finally
      List.EndUpdate;
    End;
  end;
End;


procedure TNotesMRU.Remove(const Item: String);
var
  I: integer;
begin
  if Item <> EmptyStr then
  Begin
    for I := 0 to List.Count-1 do
    begin
      if (CompareText(List[I], Item) = 0) then
      Begin
        List.Delete(I);
        Break;
      End;
    end;
  End;
end;

function TNotesMRU.LoadFromFile( const AFilename: string ): Boolean;
begin
  Result := False;
  if FileExists( AFilename ) then
  begin
    FList.LoadFromFile( AFilename );
    Result := True;
  end;
end;

procedure TNotesMRU.LoadFromString(const AString: string);
begin
  FList.Text := AString;
End;

procedure TNotesMRU.SaveToFile(const FileToSave: String);
Begin
  FList.SaveToFile( FileToSave );
End;

procedure TNotesMRU.SetMaxItens(MaxValue: Byte);
begin
  if MaxItens <> MaxValue then
  begin
    FMaxItens := MaxValue;
    List.BeginUpdate;
    try
      while List.Count >= MaxItens do
        List.Delete(MaxItens-1);
    finally
      List.EndUpdate;
    end;
  end;
end;

{ TNotesMRUMenu }

procedure TNotesMRUMenu.ClearMenuItems;
var
  Index, I: Integer;
begin
  if FMenuItem = nil then Exit;
  Index := FMenuItem.MenuIndex + 1;
  for I := 1 to ItemsCount do
    FMenuItem.Parent.Delete( Index );
  ItemsCount := 0;
end;

procedure TNotesMRUMenu.Clear;
Begin
  ClearMenuItems;
  FList.Clear;
end;

procedure TNotesMRUMenu.SetMenuItem(Value: TMenuItem);
begin
  if FMenuItem = Value then
    Exit;
  ClearMenuItems;
  FMenuItem := Value;
  ReloadMenus;
end;

procedure TNotesMRUMenu.ReloadMenus;
var
  I: Integer;
  Index: Integer;
  NewMenuItem: TMenuItem;
begin
  if not Assigned ( FMenuItem ) or not Assigned( FOwner ) then Exit;
  ClearMenuItems;
  Index := FMenuItem.MenuIndex + 1;
  for I := Pred( FList.Count ) downto 0 do begin
    NewMenuItem := TMenuItem.Create( FOwner );
    NewMenuItem.Tag := I;
    NewMenuItem.Hint := Format( 'Abre o arquivo %s', [ FList[I] ] );
    if FCaptionWPath then
      NewMenuItem.Caption := FList[I]
    else
      NewMenuItem.Caption := ExtractFileName( FList[I] );
    NewMenuItem.OnClick := MenuItemClicked;
    FMenuItem.Parent.Insert( Index, NewMenuItem );
    Inc( ItemsCount );
  end;
  if FList.Count > 0 then
  begin
    NewMenuItem := TMenuItem.Create( FOwner );
    NewMenuItem.Caption := '-';
    FMenuItem.Parent.Insert( Index, NewMenuItem );
    Inc( ItemsCount );
  end;
end;

procedure TNotesMRUMenu.MenuItemClicked(Sender: TObject);
begin
  if Assigned( FOnClick ) then
    FOnClick( Self, FList[ TMenuItem(Sender).Tag ] );
end;

constructor TNotesMRUMenu.Create( const AOwner: TComponent = nil; const AMenuItem: TMenuItem = nil; const OnClickHandler: TNotesMRUClickEvent = nil; const ListFilename: string = ''; const AMaxItems: Integer = 15 );
begin
  inherited Create;
  FOwner := AOwner;
  FMenuItem := AMenuItem;
  LoadFromFile( ListFilename );
  MaxItens := AMaxItems;
  FOnClick := OnClickHandler;
end;

end.
