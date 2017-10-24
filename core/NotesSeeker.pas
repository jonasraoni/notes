//
//    TNotesSeeker - classe de buscas do Notes.
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
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    **************************************************************
//    Revision #0
//      Version  : 1.0.0
//      Date     : 2003-11-30 22:00:00 GMT -3:00
//      Reviewer : Jonas Raoni Soares da Silva
//      Changes  : Criada a classe.
//    **************************************************************
//

(*
@abstract(NotesSeeker - classe de buscas do Notes.)
@author(Jonas Raoni Soares da Silva <jonasraoni@gmail.com>)
@created(30 Nov 2003)
*)
unit NotesSeeker;


interface

uses
 { Windows: dependente na função GetTickCount }
 Windows, SysUtils, Classes;

type

{
  @code(ENotesSeekerException) - 
    Notificar erros na classe TNotesSeeker de forma           
    profissional, facilitando a interceptação e/ou log de     
    erros                                                     
}
  ENotesSeekerException = class ( Exception )
  public
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
  end;

  {Opões de pesquisa: <BR>
   @code(nsHandleEOL) - se você precisar buscar por quebras de linhas, você precisa setar esta opção.<BR>
   @code(nsCaseSensitive) - diferenciar maiúsculas de minúsculas.<BR>
   @code(nsWholeWords) - retorna apenas palavras inteiras.<BR>
   @code(nsBackward) - busca de traz para frente. <BR>
   @code(nsHandleWildCard) - usa coringas * e ? na pesquisa.}
  TNotesSeekerOption = ( nsHandleEOL, nsCaseSensitive, nsWholeWords, nsBackward, nsHandleWildCard );
  { Set de @link(TNotesSeekerOption).}
  TNotesSeekerOptions = set of TNotesSeekerOption;

{
  @code(TNotesSeeker) -
    Permite fazer buscas em strings com várias opções
}
  TNotesSeeker = class(TObject)
  protected
    FTimeElapsed, FMatches, FStartAt,
    FCurCol, FCurLine, FMatchLen: Cardinal;

    FBufferEnd, FBuffer, FBufferBegin, FBufferBackup,
    FSearchStr, FEOL, FWildCardBegin, FWildCard, FWildCardEnd: PChar;

    FOptions: TNotesSeekerOptions;

    FContextRightLenght, FContextLeftLenght: Byte;

    FKeepText, FSelection: Boolean;

    function GetText: string;
    function GetReplacedText: string;
    function GetContext: string;
    function GetSearchStr: string;
    function GetCurCol: Cardinal;
    function GetRemainingText: string;
    function GetCurByte: Cardinal;
    function GetEOL: string;

    procedure SetOptions(const Value: TNotesSeekerOptions);
    procedure SetText( const Value: string);
    procedure SetSearchStr(const Value: string);
    procedure SetEOL(const Value: string);

    procedure FreeBuffer;
    procedure FreeEOL;
    procedure FreeSearchStr;

  public
    { Método construtor }
    constructor Create; virtual;
    { Método destruidor }
    destructor Destroy; override;

    { Seta se a pesquisa é em um texto selecionado }
    property Selection: boolean read FSelection write FSelection;
    { Quando HandleEOL fizer parte das opções, armazenará a linha onde a string procurada foi encontrada }
    property CurLine: Cardinal read FCurLine;
    { Armazenará a coluna onde a string procurada foi encontrada, se HandleEOL não estiver nas opções, armazenará a mesma coisa que a propriedade CurByte }
    property CurCol: Cardinal read GetCurCol;
    { Armanzena a posição ou byte "absoluto" onde a string foi encontrada }
    property CurByte: Cardinal read GetCurByte;
    { Especifica a posição/byte inicial onde a busca deverá começar }
    property StartAt: Cardinal read FStartAt write FStartAt;
    { Retorna o contexto onde a string procurada foi encontrada }
    property Context: string read GetContext;
    { Especifica a quantidade de caracteres que deverão fazer parte do contexto encontrado ao lado esquerdo da string procurada }
    property ContextLeftLenght: Byte read FContextLeftLenght write FContextLeftLenght;
    { Especifica a quantidade de caracteres que deverão fazer parte do contexto encontrado ao lado direito da string procurada }
    property ContextRightLenght: Byte read FContextRightLenght write FContextRightLenght;
    { Tempo em milisegundos utilizado na busca atual }
    property TimeElapsed: Cardinal read FTimeElapsed;
    { Armazena o número de strings que coincidiram com a busca até o presente momento }
    property Matches: Cardinal read FMatches;
    { Lenght da string encontrada. }
    property MatchLen: Cardinal read FMatchLen;
    { Permite alterar a sequência de caracteres que demarcam o fim de uma linha }
    property EOL: string read GetEOL write SetEOL;
    { Armazena as opções atualmente habilitadas para a busca, podendo ser alterada a qualquer momento }
    property Options: TNotesSeekerOptions read FOptions write SetOptions;
    { Termo a ser procurado no texto }
    property SearchStr: string read GetSearchStr write SetSearchStr;
    { Texto onde a busca será efetuada }
    property Text: string read GetText write SetText;
    { Texto restante ao término da busca }
    property RemainingText: string read GetRemainingText;
    { Especifica se a classe deverá manter uma cópia do texto setado inicialmente }
    property KeepText: Boolean read FKeepText write FKeepText;
    { Retorna o texto com os replaces, caso KeepText seja falso, essa propriedade se torna sinônimo da propriedade Text }
    property ReplacedText: string read GetReplacedText;

    { Prepara tudo para uma nova busca }
    procedure StartSearch;
    { Carrega o texto da busca a partir de um arquivo }
    procedure LoadFromFile( const AFilename: string );
    { Carrega o texto da busca a partir de um stream }
    procedure LoadFromStream( const AStream: TStream );
    { Carrega o texto da busca a partir de um buffer }
    procedure LoadFromBuffer( const ABuffer: PChar);
    { Efetua a substituição da string encontrada pela string contida em "S" }
    procedure Replace( const S: String );
    { Modo prático para setar as opções }
    procedure EnableOptions( const CaseSensitive: Boolean = true; const WholeWords: Boolean = false; const HandleEOL: Boolean = true; const HandleWildCard: Boolean = false; const Backward: Boolean = false );

    { Efetua a busca: se o termo procurado for encontrado, retorna true, caso contrário retorna false }
    function Search: Boolean;
  end;

  { Retorna o número de ocorrências de SubStr em Str percorrendo no máximo MaxLen caracteres de Str }
  function MatchCount( const Str: PChar; const SubStr: PChar; MaxLen: Integer ): Integer;
  { Compara Str1 e Str2 de trás pra frente, se as duas forem iguais retorna true, caso contrário false }
  function StrLRComp(const Str1Begin, Str1, Str2Begin, Str2: PChar; MaxLen: Integer): Boolean;
  { Compara Str1 e Str2 com case insensitive de trás pra frente, se as duas forem iguais retorna true, caso contrário false }
  function StrLIRComp(const Str1Begin, Str1, Str2Begin, Str2: PChar; MaxLen: Integer): Boolean; forward;
  { Adiciona MaxLen caracteres contidos em Source em Dest, setando o último caracter em nulo }
  function AddChars( const Dest, Source: PChar; MaxLen: Integer ): PChar;

const
  { Caracteres que definem delimitadores de palavra, usada quando a opção WholeWords está ativa }
  WhiteSpaces: set of Char = [' ',#9,#13,#10,'!','"','#','$','%','&','''','(',')','*','+','-','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~','.', ','];

implementation

function MatchCount;
var
  Pos: Integer;
begin
  Result:=0;
  Pos:= 0;
  while MaxLen>0 do begin
    if ( (Str+Pos)^ = SubStr^ ) and ( StrLComp( Str+Pos, SubStr, StrLen(SubStr) ) = 0 ) then begin
      Inc( Pos, StrLen(SubStr) );
      Inc( Result );
      Dec( MaxLen, StrLen(SubStr) );
      continue;
    end;
    Inc( Pos );
    Dec( MaxLen );
  end;
end;

function StrLRComp;
var
  Pos: Integer;
begin
  Pos := 0;
  Result:= true;
  while MaxLen > 0 do begin
    if not (( ( (Str1-Pos)^ <> Str1Begin ) and ( (Str2-Pos)^ <> Str2Begin ) ) or (Pos=0)) or ( (Str1-Pos)^ <> (Str2-Pos)^) then begin
      Result := False;
      Break;
    end;
    Inc( Pos );
    Dec( MaxLen );
  end;
end;

function StrLIRComp;
var
  Pos: Integer;
begin
  Pos := 0;
  Result:= true;
  while MaxLen > 0 do begin
    if not ( ( (Str1-Pos)^ <> Str1Begin ) and ( (Str2-Pos)^ <> Str2Begin ) ) or ( UpCase( (Str1-Pos)^ ) <> UpCase( (Str2-Pos)^ ) ) then begin
      Result := False;
      Break;
    end;
    Inc( Pos );
    Dec( MaxLen );
  end;
end;

function AddChars;
var
  Pos: Integer;
begin
  Pos := 0;
  while MaxLen>0 do begin
    (Dest+Pos)^ := (Source+Pos)^;
    Inc( Pos );
    Dec( MaxLen );
  end;
  Result := Dest+Pos;
  Result^ := #0;
end;


{ class : TNotesSeeker }

{ TNotesSeeker : protected }

function TNotesSeeker.GetText: string;
begin
  if FBufferBackup <> nil then
    Result := StrPas( FBufferBackup )
  else
    Result := ReplacedText;
end;

function TNotesSeeker.GetReplacedText: string;
begin
  Result := StrPas( FBufferBegin );
end;

function TNotesSeeker.GetContext: string;
var
  BeginAt, EndAt, TempBuffer: PChar;
  BufferPos, TempSize: Integer;
begin
  if nsBackward in FOptions then begin
    BufferPos := Integer( FBuffer+1 );
    BeginAt := Ptr( BufferPos-FContextLeftLenght );
    EndAt := FBuffer+FMatchLen+FContextRightLenght+1;
  end
  else begin
    BufferPos := Integer( FBuffer-FMatchLen );
    BeginAt := Ptr( BufferPos-FContextLeftLenght );
    EndAt := FBuffer+FContextRightLenght;
  end;
  if BeginAt > EndAt then
    raise ENotesSeekerException.CreateFmt('GetContext::Range Error "BeginAt(%d) > EndAt(%d)"', [Integer(BeginAt), Integer(EndAt)]);
  if BeginAt < FBufferBegin then
    BeginAt := FBufferBegin;
  if EndAt > FBufferEnd then
    EndAt := FBufferEnd;
  TempSize := (EndAt-BeginAt) + 1;
  GetMem( TempBuffer, TempSize );
  Result := StrPas( StrLCopy( TempBuffer, BeginAt, TempSize-1 ) );
  FreeMem( TempBuffer );
end;

function TNotesSeeker.GetSearchStr: string;
begin
  Result := StrPas( FSearchStr );
end;

function TNotesSeeker.GetCurCol: Cardinal;
begin
  if FCurCol-FMatchLen < 1 then
    Result := 0
  else
    Result := FCurCol - FMatchLen;
end;

function TNotesSeeker.GetRemainingText: string;
var
  Return: PChar;
begin
  if not ( nsBackward in FOptions ) then
    Result := StrPas( FBuffer )
  else if FBuffer-FBufferBegin+1 > 0 then begin
    GetMem( Return, FBuffer-FBufferBegin+1 );
    Result := StrPas( StrLCopy( Return, FBufferBegin, FBuffer-FBufferBegin+1 ) );
    FreeMem( Return );
  end;
end;

function TNotesSeeker.GetCurByte: Cardinal;
begin
{$WARNINGS OFF}
  if nsBackward in FOptions then
    Result := FBufferEnd-1 - FBuffer - FMatchLen
  else
  begin
    if not FSelection then
      Result := FBuffer - FMatchLen - FBufferBegin
    else
      Result := (FBuffer - FMatchLen - FBufferBegin) + FStartAt; 
  end;
{$WARNINGS ON}
end;

function TNotesSeeker.GetEOL: string;
begin
  Result := StrPas( FEOL );
end;

procedure TNotesSeeker.SetOptions(const Value: TNotesSeekerOptions);
begin
  if ( [nsHandleWildCard, nsWholeWords] <= Value ) then
    raise ENotesSeekerException.Create( 'SetOptions::Não é possível utilizar as opções WholeWords e WildCards em conjunto' );
  FOptions := Value;
end;

procedure TNotesSeeker.SetText( const Value: string);
begin
  LoadFromBuffer( PChar( Value ) );
end;

procedure TNotesSeeker.SetSearchStr(const Value: string);
begin
  FreeSearchStr;
  GetMem( FSearchStr, Length( Value )+1 );
  FWildCardBegin := StrCopy( FSearchStr, PChar( Value ) );
  FWildCard := FWildCardBegin;
  FWildCardEnd := StrEnd( FWildCardBegin );
end;

procedure TNotesSeeker.SetEOL(const Value: string);
begin
  FreeEOL;
  GetMem( FEOL, Length( Value )+1 );
  StrCopy( FEOL, PChar( Value ) );
end;


procedure TNotesSeeker.FreeBuffer;
begin
  if FBufferBegin <> nil then begin
    FBufferEnd := nil;
    FBuffer := nil;
    FreeMem( FBufferBegin );
  end;
  if FBufferBackup <> nil then begin
    FreeMem( FBufferBackup );
    FBufferBackup := nil;
  end;
end;

procedure TNotesSeeker.FreeEOL;
begin
  if FEOL <> nil then begin
    FreeMem( FEOL );
    FEOL := nil;
  end;
end;

procedure TNotesSeeker.FreeSearchStr;
begin
  if FSearchStr <> nil then begin
    FreeMem( FSearchStr );
    FSearchStr := nil;
    FWildCardBegin := nil;
    FWildCard := nil;
    FWildCardEnd := nil;
  end;
end;



{ TNotesSeeker : public }

constructor TNotesSeeker.Create;
begin
  EOL := #13#10;
  FContextLeftLenght := 10;
  FContextRightLenght := 20;
  FOptions := [nsHandleEOL, nsCaseSensitive];
end;

destructor TNotesSeeker.Destroy;
begin
  FreeBuffer;
  FreeSearchStr;
  FreeEOL;
  inherited Destroy;
end;

procedure TNotesSeeker.LoadFromBuffer( const ABuffer: PChar);
begin
  FreeBuffer;
  GetMem( FBufferBegin, StrLen( ABuffer )+1 );
  FBuffer := StrCopy( FBufferBegin, ABuffer );
  FBufferEnd :=  StrEnd( FBufferBegin );
  if FKeepText then begin
    GetMem( FBufferBackup, StrLen( FBufferBegin )+1 );
    StrCopy( FBufferBackup, FBufferBegin );
  end;
end;

procedure TNotesSeeker.LoadFromFile(const AFilename: string);
var
  FS: TFileStream;
begin
  if not FileExists( AFilename ) then
    raise ENotesSeekerException.CreateFmt( 'LoadFromFile::Arquivo "%s" não encontrado', [AFilename] );
  FS := TFileStream.Create( AFilename, fmOpenRead );
  try
    LoadFromStream( FS );
  finally
    FS.Free;
  end;
end;

procedure TNotesSeeker.LoadFromStream(const AStream: TStream);
var
  Size: Int64;
begin
  FreeBuffer;
  Size := AStream.Size;
  GetMem( FBuffer, Size+1 );
  Size := AStream.Read( FBuffer^, Size );
  ( FBuffer+Size )^ := #0;
  FBufferEnd := (FBuffer+Size);
  FBufferBegin := FBuffer;
  if FKeepText then
  begin
    GetMem( FBufferBackup, StrLen( FBufferBegin )+1 );
    StrCopy( FBufferBackup, FBufferBegin );
  end;
end;

procedure TNotesSeeker.StartSearch;
begin
  FCurLine := 0;
  FCurCol := 0;
  FMatches := 0;
  if FBufferBackup <> nil then begin
    FreeMem( FBufferBegin );
    GetMem( FBufferBegin, StrLen( FBufferBackup )+1 );
    FBuffer := StrCopy( FBufferBegin, FBufferBackup );
    FBufferEnd := FBuffer + StrLen( FBufferBackup );
  end;

  if StrLen( FSearchStr ) = 0 then
    raise ENotesSeekerException.Create('StartSearch::Propriedade SearchStr está vazia.');

  if nsBackward in FOptions then begin
    FBuffer := FBufferEnd-1;
    FWildCard := FWildCardEnd-1;
  end
  else begin
    FBuffer := FBufferBegin;
    FWildCard := FWildCardBegin;
  end;
end;

function TNotesSeeker.Search: Boolean;
var
  Init: Cardinal;
  Jump: Byte;
begin
  Jump := 1;
  Result := True;
  Init := GetTickCount;
  {$WARNINGS OFF}
  if not ( nsBackward in FOptions ) then
    while FBuffer <> FBufferEnd do begin
      if (FStartAt > FBuffer-FBufferBegin) and (not FSelection) then
      begin
        Inc( FBuffer );
        Inc( FCurCol );
        Continue;
      end
      else if ( nsHandleWildCard in FOptions ) then begin
        if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, StrLen( FEOL ) ) = 0 ) then begin
          Inc( FCurLine );
          FCurCol := 0;
          if ( FWildCard^ <> '*' ) then begin
            if FWildCard <> FWildCardBegin then begin
              FWildCard := FWildCardBegin;
              FMatchLen := 0;
              Jump := 1;
            end
          end
          else
            Inc( FMatchLen, StrLen( FEOL ) );
        end;
        if FWildCard = FWildCardBegin then
          FMatchLen := 0;
        if FWildCard^ = #0 then begin
          FWildCard := FWildCardBegin;
          Inc( FMatches );
          FTimeElapsed := GetTickCount - Init;
          Exit;
        end
        else if ( FWildCard+1 = FWildCardEnd ) and  ( (FWildCard+1)^ = '*' ) and ( FBuffer+1 = FBufferEnd ) then begin
          Inc( FBuffer );
          Inc( FMatchLen );
          Inc( FCurCol );
          Inc( FMatches );
          FTimeElapsed := GetTickCount - Init;
          Exit;
        end
        else if FWildCard^ = '?' then begin
          Inc( FBuffer );
          Inc( FCurCol );
          Inc( FMatchLen );
          Inc( FWildCard );
          if ( FBuffer = FBufferEnd ) and ( FWildCard = FWildCardEnd ) then begin
            Inc( FMatches );
            FTimeElapsed := GetTickCount - Init;
            Exit;
          end;
          Continue;
        end
        else if FWildCard^ = '*' then begin
          if (FWildCard+Jump)^ = '?' then begin
            Inc( FBuffer );
            Inc( FCurCol );
            Inc( FMatchLen );
            Inc( Jump );
            Continue;
          end
          else if (FWildCard+Jump)^ = #0 then begin
            Inc( FMatchLen, FBufferEnd-FBuffer );
            Inc( FCurCol, FBufferEnd-FBuffer );
            FBuffer := FBufferEnd;
            FWildCard := FWildCardBegin;
            Inc( FMatches );
            FTimeElapsed := GetTickCount - Init;
            Exit;
          end
          else if (FWildCard+Jump)^ = '*' then begin
            Inc( FWildCard, Jump );
            Jump := 1;
            Continue;
          end
          else if nsCaseSensitive in FOptions then begin
            if FBuffer^ = (FWildCard+Jump)^ then begin
              Inc( FWildCard, Jump );
              Jump := 1;
              Continue;
            end
          end
          else if UpCase( FBuffer^ ) = UpCase( (FWildCard+Jump)^ ) then begin
            Inc( FWildCard, Jump );
            Jump := 1;
            Continue;
          end;
          Inc( FMatchLen );
          Inc( FBuffer );
          Inc( FCurCol );
          Continue;
        end
        else
          if nsCaseSensitive in FOptions then begin
            if FBuffer^ = FWildCard^ then begin
              Inc( FBuffer );
              Inc( FMatchLen );
              Inc( FCurCol );
              Inc( FWildCard );
              Jump := 1;
              if FWildCard^ = #0 then begin
                FWildCard := FWildCardBegin;
                Inc( FMatches );
                FTimeElapsed := GetTickCount - Init;
                Exit;
              end
              else if not ( FWildCard^ in ['*','?'] ) and ( FBuffer^ <> FWildCard^ ) then begin
                FWildCard := FWildCardBegin;
                Continue;
              end;
              Continue;
            end;
          end
          else if UpCase( FBuffer^ ) = UpCase( FWildCard^ ) then begin
            Inc( FBuffer );
            Inc( FMatchLen );
            Inc( FCurCol );
            Inc( FWildCard );
            Jump := 1;
            if FWildCard^ = #0 then begin
              FWildCard := FWildCardBegin;
              Inc( FMatches );
              FTimeElapsed := GetTickCount - Init;
              Exit;
            end
            else if not ( FWildCard^ in ['*','?'] ) and ( UpCase( FBuffer^ ) <> UpCase( FWildCard^ ) ) then begin
              FWildCard := FWildCardBegin;
              Continue;
            end;
            Continue;
          end;
          if FWildCard <> FWildCardBegin then begin
            FWildCard := FWildCardBegin;
            FMatchLen := 0;
            Jump := 1;
          end;
      end
      else if ( nsCaseSensitive in FOptions ) then begin
        if ( FBuffer^ = FSearchStr^ ) and ( StrLComp( FBuffer, FSearchStr, StrLen(FSearchStr) ) = 0 ) then begin
          if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, StrLen( FEOL ) ) = 0 ) then begin
            Inc( FCurLine, MatchCount( FBuffer, FEOL, StrLen( FSearchStr ) ) );
            FCurCol := 0;
          end;
          if nsWholeWords in FOptions then begin
            if ( FBuffer > FBufferBegin ) and not ( (FBuffer-1)^ in WhiteSpaces ) then begin
              Inc( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
            if ( FBuffer+StrLen( FSearchStr ) < FBufferEnd ) and not ( (FBuffer + StrLen( FSearchStr ))^ in WhiteSpaces ) then begin
              Inc( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
          end;
          Inc( FBuffer, StrLen( FSearchStr ) );
          Inc( FCurCol, StrLen( FSearchStr ) );
          FMatchLen := StrLen( FSearchStr );
          Inc( FMatches );
          FTimeElapsed := GetTickCount - Init;
          Exit;
        end
        else if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, StrLen( FEOL ) ) = 0 ) then begin
          Inc( FBuffer, StrLen( FEOL ) );
          Inc( FCurLine );
          FCurCol := 0;
          Continue;
        end;
      end
      else if ( UpCase( FBuffer^ ) = UpCase( FSearchStr^ ) ) and ( StrLIComp( FBuffer, FSearchStr, StrLen( FSearchStr ) ) = 0 ) then begin
        if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, StrLen( FEOL ) ) = 0 ) then begin
          Inc( FCurLine, MatchCount( FBuffer, FEOL, StrLen( FSearchStr ) ) );
          FCurCol := 0;
        end;
        if nsWholeWords in FOptions then begin
            if ( FBuffer > FBufferBegin ) and not ( (FBuffer-1)^ in WhiteSpaces ) then begin
              Inc( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
            if ( FBuffer+StrLen( FSearchStr ) < FBufferEnd ) and not ( (FBuffer + StrLen( FSearchStr ))^ in WhiteSpaces ) then begin
              Inc( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
        end;
        Inc( FBuffer , StrLen( FSearchStr ) );
        Inc( FCurCol, StrLen( FSearchStr ) );
        FMatchLen := StrLen( FSearchStr );
        Inc( FMatches );
        FTimeElapsed := GetTickCount - Init;
        Exit;
      end
      else if ( nsHandleEOL in FOptions ) and ( FBuffer^ = FEOL^ ) and ( StrLComp( FBuffer, FEOL, StrLen( FEOL ) ) = 0 ) then begin
        Inc( FBuffer, StrLen( FEOL ) );
        Inc( FCurLine );
        FCurCol := 0;
        Continue;
      end;
      Inc( FBuffer );
      Inc( FCurCol );
      Inc( FMatchLen );
    end
  else
    while FBuffer <> FBufferBegin-1 do begin
      if (FStartAt > FBufferEnd-1 - FBuffer ) then begin
        Dec( FBuffer );
        Inc( FCurCol );
        Continue;
      end
      else if ( nsHandleWildCard in FOptions ) then begin
        if ( nsHandleEOL in FOptions ) and StrLRComp( FBufferBegin, FBuffer, FEOL, FEOL+StrLen( FEOL )-1, StrLen( FEOL ) ) then begin
          Inc( FCurLine );
          FCurCol := 0;
          if ( FWildCard^ <> '*' ) then begin
            if FWildCard <> FWildCardEnd-1 then begin
              FWildCard := FWildCardEnd-1;
              FMatchLen := 0;
              Jump := 1;
            end
          end
          else
            Inc( FMatchLen, StrLen( FEOL ) );
        end;
        if FWildCard = FWildCardEnd-1 then
          FMatchLen := 0;
        if FWildCard = FWildCardBegin-1 then begin
          FWildCard := FWildCardEnd-1;
          Inc( FMatches );
          FTimeElapsed := GetTickCount - Init;
          Exit;
        end
        else if ( FWildCard-1 = FWildCardBegin-1 )  and ( (FWildCard-1)^ = '*' ) and ( FBuffer-1 = FBufferBegin-1 ) then begin
          Dec( FBuffer );
          Inc( FMatchLen );
          Inc( FCurCol );
          Inc( FMatches );
          FTimeElapsed := GetTickCount - Init;
          Exit;
        end
        else if FWildCard^ = '?' then begin
          Dec( FBuffer );
          Inc( FCurCol );
          Inc( FMatchLen );
          Dec( FWildCard );
          if ( FBuffer = FBufferBegin-1 ) and ( FWildCard = FWildCardBegin-1 ) then begin
            Inc( FMatches );
            FTimeElapsed := GetTickCount - Init;
            Exit;
          end;
          Continue;
        end
        else if FWildCard^ = '*' then begin
          if (FWildCard-Jump)^ = '?' then begin
            Dec( FBuffer );
            Inc( FCurCol );
            Inc( FMatchLen );
            Inc( Jump );
            Continue;
          end
          else if FWildCard-Jump = FWildCardBegin-1 then begin
            Inc( FMatchLen, FBufferEnd-FBuffer );
            Inc( FCurCol, FBufferEnd-FBuffer );
            FBuffer := FBufferEnd-1;
            FWildCard := FWildCardEnd-1;
            Inc( FMatches );
            FTimeElapsed := GetTickCount - Init;
            Exit;
          end
          else if (FWildCard-Jump)^ = '*' then begin
            Dec( FWildCard, Jump );
            Jump := 1;
            Continue;
          end
          else if nsCaseSensitive in FOptions then begin
            if FBuffer^ = (FWildCard-Jump)^ then begin
              Dec( FWildCard, Jump );
              Jump := 1;
              Continue;
            end
          end
          else if UpCase( FBuffer^ ) = UpCase( (FWildCard-Jump)^ ) then begin
            Dec( FWildCard, Jump );
            Jump := 1;
            Continue;
          end;
          Inc( FMatchLen );
          Dec( FBuffer );
          Inc( FCurCol );
          Continue;
        end
        else
          if nsCaseSensitive in FOptions then begin
            if FBuffer^ = FWildCard^ then begin
              Dec( FBuffer );
              Inc( FMatchLen );
              Inc( FCurCol );
              Dec( FWildCard );
              Jump := 1;
              if FWildCard = FWildCardBegin-1 then begin
                FWildCard := FWildCardEnd-1;
                Inc( FMatches );
                FTimeElapsed := GetTickCount - Init;
                Exit;
              end
              else if not ( FWildCard^ in ['*','?'] ) and ( FBuffer^ <> FWildCard^ ) then begin
                FWildCard := FWildCardEnd-1;
                Continue;
              end;
              Continue;
            end;
          end
          else if UpCase( FBuffer^ ) = UpCase( FWildCard^ ) then begin
            Dec( FBuffer );
            Dec( FWildCard );
            Inc( FMatchLen );
            Inc( FCurCol );
            Jump := 1;
            if FWildCard = FWildCardBegin-1 then begin
              FWildCard := FWildCardEnd-1;
              Inc( FMatches );
              FTimeElapsed := GetTickCount - Init;
              Exit;
            end
            else if not ( FWildCard^ in ['*','?'] ) and ( UpCase( FBuffer^ ) <> UpCase( FWildCard^ ) ) then begin
              FWildCard := FWildCardEnd-1;
              Continue;
            end;
            Continue;
          end;
          if FWildCard <> FWildCardEnd-1 then begin
            FWildCard := FWildCardEnd-1;
            FMatchLen := 0;
            Jump := 1;
          end;
      end
      else if ( nsCaseSensitive in FOptions ) then begin
        if StrLRComp( FBufferBegin, FBuffer, FSearchStr, FSearchStr+StrLen(FSearchStr)-1, StrLen(FSearchStr) ) then begin
          if ( nsHandleEOL in FOptions ) and StrLRComp( FBufferBegin, FBuffer, FEOL, FEOL+StrLen( FEOL )-1, StrLen( FEOL ) ) then begin
            Inc( FCurLine, MatchCount( FBuffer-StrLen( FSearchStr ), FEOL, StrLen( FSearchStr ) ) );
            FCurCol := 0;
          end;
          if nsWholeWords in FOptions then begin
            if ( FBuffer < FBufferEnd-1 ) and not ( (FBuffer+1)^ in WhiteSpaces ) then begin
              Dec( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
            if ( FBuffer - StrLen( FSearchStr ) > FBufferBegin ) and not ( (FBuffer - StrLen( FSearchStr ))^ in WhiteSpaces ) then begin
              Dec( FBuffer );
              Inc( FCurCol );
              Continue;
            end;
          end;
          Dec( FBuffer, StrLen( FSearchStr ) );
          Inc( FCurCol, StrLen( FSearchStr ) );
          FMatchLen := StrLen( FSearchStr );
          FTimeElapsed := GetTickCount - Init;
          Inc( FMatches );
          Exit;
        end
        else if ( nsHandleEOL in FOptions ) and StrLRComp( FBufferBegin, FBuffer, FEOL, FEOL+StrLen( FEOL )-1, StrLen( FEOL ) ) then begin
          Dec( FBuffer, StrLen( FEOL ) );
          Inc( FCurLine );
          FCurCol := 0;
          Continue;
        end;
      end
      else if StrLIRComp( FBufferBegin, FBuffer, FSearchStr, FSearchStr+StrLen(FSearchStr)-1, StrLen(FSearchStr) ) then begin
        if ( nsHandleEOL in FOptions ) and StrLRComp( FBufferBegin, FBuffer, FEOL, FEOL+StrLen( FEOL )-1, StrLen( FEOL ) ) then begin
          Inc( FCurLine, MatchCount( FBuffer-StrLen( FSearchStr ), FEOL, StrLen( FSearchStr ) ) );
          FCurCol := 0;
        end;
        if nsWholeWords in FOptions then begin
          if ( FBuffer < FBufferEnd-1 ) and not ( (FBuffer+1)^ in WhiteSpaces ) then begin
            Dec( FBuffer );
            Inc( FCurCol );
            Continue;
          end;
          if ( FBuffer - StrLen( FSearchStr ) > FBufferBegin ) and not ( (FBuffer - StrLen( FSearchStr ))^ in WhiteSpaces ) then begin
            Dec( FBuffer );
            Inc( FCurCol );
            Continue;
          end;
        end;
        Dec( FBuffer, StrLen( FSearchStr ) );
        Inc( FCurCol, StrLen( FSearchStr ) );
        FTimeElapsed := GetTickCount - Init;
        FMatchLen := StrLen( FSearchStr );
        Inc( FMatches );
        Exit;
      end
      else if ( nsHandleEOL in FOptions ) and StrLRComp( FBufferBegin, FBuffer, FEOL, FEOL+StrLen( FEOL )-1, StrLen( FEOL ) ) then begin
        Dec( FBuffer, StrLen( FEOL ) );
        Inc( FCurLine );
        FCurCol := 0;
        Continue;
      end;
      Dec( FBuffer );
      Inc( FCurCol );
      Inc( FMatchLen );
    end;
  FTimeElapsed := GetTickCount - Init;
  Result := False;
  {$WARNINGS ON}
end;

procedure TNotesSeeker.Replace(const S: String);
var
  TempBuff: PChar;
  BufferOffset: Integer;
begin
  if not ( nsBackward in FOptions ) then begin
    BufferOffset := FBuffer-FMatchLen - FBufferBegin + Length( S );
    GetMem( TempBuff, ( FBuffer-FMatchLen - FBufferBegin) + Length(S) + ( FBufferEnd - FBuffer ) + 1 );
    FBufferEnd := AddChars( AddChars( AddChars( TempBuff, FBufferBegin, FBuffer-FMatchLen - FBufferBegin ), PChar( S ), Length( S ) ), FBuffer, FBufferEnd - FBuffer );
    FreeMem( FBufferBegin );
    FBufferBegin := TempBuff;
    FBuffer := FBufferBegin + BufferOffset;
//    TempBuff := nil;
  end
  else begin
    BufferOffset := FBufferEnd - (FBuffer+FMatchLen) + Length( S );
    if FBuffer < FBufferBegin then
      GetMem( TempBuff, FBufferEnd - (FBuffer+FMatchLen) + Length( S ) + ( FBuffer - FBufferBegin ) + 1 )
    else
      GetMem( TempBuff, FBufferEnd - (FBuffer+FMatchLen) + Length( S ) + ( FBuffer - FBufferBegin ) + 1 );
    FBufferEnd := AddChars( AddChars( AddChars( TempBuff, FBufferBegin, FBuffer-FBufferBegin+1 ), PChar( S ), Length( S ) ), FBuffer+FMatchLen+1, FBufferEnd-1 - (FBuffer+FMatchLen) );
    FreeMem( FBufferBegin );
    FBufferBegin := TempBuff;
    FBuffer := FBufferEnd - BufferOffset;
//    TempBuff := nil;
  end;
end;


{ ENotesSeekerException }
{ ENotesSeekerException : public }

constructor ENotesSeekerException.Create(const Msg: string);
begin
  inherited Create( 'TNotesSeeker.'+Msg );
end;

constructor ENotesSeekerException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt( 'TNotesSeeker.'+Msg, Args );
end;

procedure TNotesSeeker.EnableOptions(const CaseSensitive, WholeWords, HandleEOL, HandleWildCard, Backward: Boolean);
  var Opcoes: TNotesSeekerOptions;
begin
  if CaseSensitive then Include( Opcoes, nsCaseSensitive ) else Exclude( Opcoes, nsCaseSensitive );
  if HandleEOL then Include( Opcoes, nsHandleEOL ) else Exclude( Opcoes, nsHandleEOL );
  if Backward then Include( Opcoes, nsBackward ) else Exclude( Opcoes, nsBackward );
  if HandleWildCard then Include( Opcoes, nsHandleWildCard ) else Exclude( Opcoes, nsHandleWildCard );
  if WholeWords then Include( Opcoes, nsWholeWords ) else Exclude( Opcoes, nsWholeWords );
  SetOptions( Opcoes );
end;

{function StrLComp(const Str1, Str2: PChar; MaxLen: Integer): Integer;
var
  Pos1, Pos2: Integer;
begin
  Pos1 := 0;
  Pos2 := 0;
  Result:= 0;
  while MaxLen > 0 do begin
    if (Str1+Pos1)^ <> #0 then begin
      if (Str2+Pos2)^ <> #0 then begin
        Inc( Result, Ord( (Str1+Pos1)^ ) - Ord( (Str2+Pos2)^ ) );
        Inc( Pos1 );
        Inc( Pos2 );
      end
      else begin
        Inc( Result, Ord( (Str1+Pos1)^ ) );
        Inc( Pos1 );
      end
    end
    else if (Str2+Pos2)^ <> #0 then begin
      Inc( Result, -Ord( (Str2+Pos2)^ ) );
      Inc( Pos2 );
    end
    else
      Exit;
    Dec( MaxLen );
  end;
end;

function StrLRComp(const Str1Begin, Str1, Str2Begin, Str2: PChar; MaxLen: Integer): Integer;
var
  Pos1, Pos2: Integer;
begin
  Pos1 := 0;
  Pos2 := 0;
  Result:= 0;
  while MaxLen > 0 do begin
    if (Str1-Pos1)^ <> Str1Begin then begin
      if (Str2-Pos2)^ <> Str2Begin then begin
        Inc( Result, Ord( (Str1-Pos1)^ ) - Ord( (Str2-Pos2)^ ) );
        Inc( Pos1 );
        Inc( Pos2 );
      end
      else begin
        Inc( Result, Ord( (Str1-Pos1)^ ) );
        Inc( Pos1 );
      end
    end
    else if (Str2-Pos2)^ <> Str2Begin then begin
      Inc( Result, -Ord( (Str2-Pos2)^ ) );
      Inc( Pos2 );
    end
    else
      Exit;
    Dec( MaxLen );
  end;
end;}

end.

