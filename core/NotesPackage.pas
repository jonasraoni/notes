unit NotesPackage;

{
==================================
| Estrutura do arquivo:
==================================
---------------------------------
| assinatura: 'NPK'
---------------------------------
| versão (integer)
---------------------------------
| número de arquivos (integer)
---------------------------------
| infos gerais (TNotesPackageInfo)
---------------------------------
|
| (Repete para cada arquivo:)
|
|   |------------------------
|   | info (TFileHeader)
|   |------------------------
|   | conteúdo do arquivo
|   |------------------------
---------------------------------
}


interface

uses
  Classes, SysUtils, zlibex;

const
  NPK_SIGNATURE = 'NPK';
  NPK_SIGNATURELEN = 3;
  NPK_VER: integer = 1;

type
  PFileHeader = ^TFileHeader;
  TFileHeader = record
    Filename: string[255];
    Date: integer;
    OriginalSize: integer;
    Size: integer;
    Crc: cardinal;
  end;

  { tipos de pacotes:
      * npUnknow - desconhecido
      * npLibrary - arquivos que serão importados para a biblioteca do notes
      * npFileTemplates - arquivos que serão importados como templates
      * npProject - arquivos que podem ser tanto um template de projeto quanto
          um backup de um projeto
      * npFileType - adiciona um novo tipo de arquivo ao Notes
      * npProgramdirFiles - adiciona arquivos ao diretório do programa
      * npConfigDirFiles - adiciona arquivos ao diretório de configuração do Notes
      * npProfileDirFiles - adiciona arquivos ao diretório do profile atual
  }
  TNotesPackageType = (npUnknow, npLibrary, npFileTemplates, npProject,
    npFileType, npProgramDirFiles, npConfigDirFiles, npProfileDirFiles);

  // Informações sobre o pacote
  TNotesPackageInfo = record
    PackageType: TNotesPackageType;
    FileType: string[100];
    Author: string[255];
    Description: string[255];
    Url: string[255];
    Email: string[255];
  end;

  TNotesPackageWriter = class(TObject)
  protected
    FReady: Boolean;
    FOutputFile: File;
    // Diretório base
    fBaseDir: string;
    FFilesCount: integer;
  public
    {  Cria um novo pacote (NPK)
       @code(AFilename) - Nome do arquivo do pacote a ser criado (ex: c:\meupacote.npk)
       @code(ABaseDir) - Diretório base dos arquivos que serão colocados no pacote. Os nomes dos arquivos serão gravados como relativos a este diretório.
       @code(PackageInfo) - Informações a serem gravadas no pacote
       @code(OverwriteOnExists) - se o arquivo "AFileName" já existir, ele pode ser sobrescrito?!   }
    constructor Create( const AFilename, ABaseDir: string; PackageInfo: TNotesPackageInfo; const OverwriteOnExists: Boolean = False);
    destructor Destroy; override;

    procedure AddFile( const AFilename: string);
  end;

  TNotesPackageReader = class(TObject)
  protected
    fInfo: TNotesPackageInfo;
    FReady: Boolean;
    FCurHeader: TFileHeader;
    FDataPosition, FNextHeaderStart: integer;
    FInputFile: File;
    FFilesCount: integer;
    function CheckEOF( const BytesNeeded: integer ): Boolean;

  public
    constructor Create( const AFilename: string );
    destructor Destroy; override;

    function GetNextFile: Boolean;
    function CurFileContent: string;
    function ReadFile( Data: PChar; const ChunkSize: integer ): integer;

    property CurrentFile: TFileHeader read FCurHeader;
    property Info: TNotesPackageInfo read fInfo;
    property FilesCount: integer read FFilesCount;
  end;

implementation


{ TNotesPackageWriter }

procedure TNotesPackageWriter.AddFile(const AFilename: string);
var
  InputFile: File of byte;
  BytesRead, BytesWritten: Integer;
  CurPos, HeaderPos, TotalWritten: integer;
  Buffer: array[1..10*1024] of Char;
  FileHeader: TFileHeader;
  Compressor: TZCompressionBlock;
begin
  if (FReady) and (FileExists( AFilename )) then
  begin

    with FileHeader do
    begin
      FileHeader.Filename:= StringReplace(AFileName, fBaseDir, '', [rfIgnoreCase]);
      Size := 0;
      OriginalSize := 0;
      Date := FileAge(AFileName);
      Crc:= 0;
    end;

    HeaderPos := FilePos( FOutputFile );
    BlockWrite( FOutputFile, FileHeader, SizeOf( TFileHeader ) );

    AssignFile( InputFile, AFileName );
    {$I-}
    Reset( InputFile );
    {$I+}
    if IOResult <> 0 then
      raise Exception.Create( ClassName + '.AddFile: the file "'+AFileName+'" could not be readed.' );

    FileHeader.OriginalSize := FileSize( InputFile );
    TotalWritten := 0;
    Compressor := TZCompressionBlock.Create( @FOutputFile );
    repeat
      BlockRead( InputFile, Buffer, SizeOf( Buffer ), BytesRead );
      BytesWritten := Compressor.Write( Buffer, BytesRead );
      Inc( TotalWritten, BytesWritten );
    until ( BytesRead = 0 ) or ( BytesRead <> BytesWritten );
    CloseFile( InputFile );
    Compressor.Free;

    CurPos := FilePos( FOutputFile );
    Seek( FOutputFile, HeaderPos );
    FileHeader.OriginalSize := TotalWritten;
    FileHeader.Size := CurPos - ( HeaderPos + SizeOf( TFileHeader ) );
    BlockWrite( FOutputFile, FileHeader, SizeOf( TFileHeader ) );
    Seek( FOutputFile, CurPos );

    inc(FFilesCount);
  end;
end;

constructor TNotesPackageWriter.Create( const AFilename, ABaseDir: string; PackageInfo: TNotesPackageInfo; const OverwriteOnExists: Boolean = False);
begin
  if FileExists( AFilename ) then
  begin
    if OverwriteOnExists then
      DeleteFile( AFilename )
    else
      Exit;
  end;

  FBaseDir:= ABaseDir;
  FFilesCount:= 0;

  Assign( FOutputFile, AFilename );
  {$I-}
  Rewrite( FOutputFile, 1 );
  {$I+}
  if IOResult <> 0 then
    raise Exception.Create( ClassName + '.Create: the file could not be acessed.' )
  else
  begin
    FReady := True;
    BlockWrite( FOutputFile, NPK_SIGNATURE, NPK_SIGNATURELEN );
    BlockWrite( FOutputFile, NPK_VER, sizeOf(integer));
    BlockWrite( FOutputFile, FFilesCount, sizeOf(integer));
    BlockWrite( FOutputFile, PackageInfo, SizeOf(TNotesPackageInfo) );
  end;
end;

destructor TNotesPackageWriter.Destroy;
begin
  if FReady then
  begin
    // seta o número de arquivos
    Seek(FOutputFile, NPK_SIGNATURELEN + sizeOf(integer));
    BlockWrite(FOutputFile, FFilesCount, sizeOf(integer));
    CloseFile( FOutputFile );
  end;
  inherited;
end;

{ TNotesPackageReader }

constructor TNotesPackageReader.Create(const AFilename: string);
var
  Signature: array[ 0..NPK_SIGNATURELEN-1 ] of Char;
  Vers: integer;
begin
  if not FileExists( AFilename ) then
    raise Exception.Create( ClassName + '.Create: file not found.' );

  Assign( FInputFile, AFilename );
  {$I-}
  Reset( FInputFile, 1 );
  {$I+}
  if IOResult <> 0 then
    raise Exception.Create( ClassName + '.Create: the file could not be opened.' )
  else
  begin
    BlockRead( FInputFile, Signature, NPK_SIGNATURELEN );
    if Signature <> NPK_SIGNATURE then
      raise Exception.Create( ClassName + '.Create: file not found.' );

    BlockRead( FInputFile, Vers, sizeOf(integer));
    if Vers <> NPK_VER then
      raise Exception.Create(ClassName + '.Create: this is not a suported NPK version.');

    // lê número de arquivos
    BlockRead( FInputFile, FFilesCount, sizeOf(integer));
    // Lê as informações do pacote
    BlockRead( FInputFile, fInfo, sizeOf(TNotesPackageInfo));

    FReady := True;
    FNextHeaderStart := FilePos( FInputFile );
  end;
end;

destructor TNotesPackageReader.Destroy;
begin
  if FReady then
    CloseFile( FInputFile );
  inherited;
end;

function TNotesPackageReader.GetNextFile: Boolean;
begin
  Seek( FInputFile, FNextHeaderStart );
  Result := not CheckEOF( SizeOf( TFileHeader ) );
  if Result then
  begin
    BlockRead( FInputFile, FCurHeader, SizeOf( TFileHeader ) );
    FNextHeaderStart := integer(FilePos( FInputFile )) + FCurHeader.Size;
    FDataPosition := 0;
  end;
end;

function TNotesPackageReader.ReadFile( Data: PChar; const ChunkSize: integer ): integer;
var
  Decompressor: TZDecompressionBlock;
begin
  Result := ChunkSize;

  if Result <> 0 then
  begin
    Decompressor := TZDecompressionBlock.Create( @FInputFile );
    Result := Decompressor.Read( Data^, Result );
    Decompressor.Free;
    Inc( FDataPosition, Result );
  end;
end;

function TNotesPackageReader.CheckEOF(const BytesNeeded: integer): Boolean;
begin
  Result := FilePos( FInputFile ) + BytesNeeded >= FileSize( FInputFile );
end;

function TNotesPackageReader.CurFileContent: string;
var
  a: PChar;
begin
  getMem(a, FCurHeader.OriginalSize);

  self.ReadFile(a, FCurHeader.OriginalSize);

  result:= a;
  setLength(Result, FCurHeader.OriginalSize);

  FreeMem(a);
end;

end.
