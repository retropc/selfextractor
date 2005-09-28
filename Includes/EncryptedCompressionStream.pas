unit EncryptedCompressionStream;

interface

uses
  Classes, DCPCrypt2, Shared, ComCtrls;

type
  TEncryptedCompressionStream = class(TStream)
  private
    FCompressor: TStream;
    FCipherType: TCipherTypes;
    FCompressionType: TCompressionTypes;
    FFileProgressBar: TProgressBar;
    FFilesProgressBar: TProgressBar;
    FOutput: TStream;
    FCryptoStream: TMemoryStream;
  protected
    property Compressor: TStream read FCompressor;
    property FileProgressBar: TProgressBar read FFileProgressBar;
    property FilesProgressBar: TProgressBar read FFilesProgressBar;
    property Output: TStream read FOutput;
    property CryptoStream: TMemoryStream read FCryptoStream;
    procedure BytesProcessed(ABytesProcessed: cardinal);
    procedure UpdateProgress(Sender: TObject);
  public
    constructor Create(const AOutput: TStream; const ACipherType: TCipherTypes; const ACompressionType: TCompressionTypes; const ALevel: byte; const AFileProgressBar: TProgressBar = nil; const AFilesProgressBar: TProgressBar = nil; const APassword: string = '');
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
  published
    property CompressionType: TCompressionTypes read FCompressionType;
    property CipherType: TCipherTypes read FCipherType;
  end;

  TCryptoWriteStream = class(TMemoryStream)
  private
    FCrypto: TDCP_BlockCipher;
    FCipherType: TCipherTypes;
    FOutput: TStream;
    FStartPosition: int64;
    FVirtualMode: boolean;
  protected
    property Crypto: TDCP_BlockCipher read FCrypto;
    property StartPosition: int64 read FStartPosition;
    property VirtualMode: boolean read FVirtualMode default true;
  published
    property CipherType: TCipherTypes read FCipherType;
    property Output: TStream read FOutput;
  public
    constructor Create(const AOutput: TStream; const ACipherType: TCipherTypes; const APassword: string = '');
    destructor Destroy; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

implementation

uses
  LZMA, Compress, BZip2, SysUtils;

{ TEncryptedCompressionStream }

constructor TEncryptedCompressionStream.Create(const AOutput: TStream;
  const ACipherType: TCipherTypes; const ACompressionType: TCompressionTypes;
  const ALevel: byte; const AFileProgressBar: TProgressBar = nil;
  const AFilesProgressBar: TProgressBar = nil; const APassword: string = '');
var
  pCompressProc: TCompressorProgressProc;
begin
  FOutput := AOutput;

  FCompressionType := ACompressionType;
  if APassword = '' then
    FCipherType := cptNone
  else
    FCipherType := ACipherType;

  FFileProgressBar := AFileProgressBar;
  FFilesProgressBar := AFilesProgressBar;

  if CipherType <> cptNone then
    FCryptoStream := TCryptoWriteStream.Create(Output, CipherType, APassword)
  else
    FCryptoStream := TMemoryStream(Output);

  case ACompressionType of
    cmpBZ2:
    begin
      FCompressor := TBZCompressionStream.Create(TBlockSize100k(ALevel), CryptoStream);
      if Assigned(AFileProgressBar) then
        TBZCompressionStream(FCompressor).OnProgress := UpdateProgress;
    end;
    cmpLZMA:
    begin
      if Assigned(AFileProgressBar) then
        pCompressProc := BytesProcessed
      else
        pCompressProc := nil;
      FCompressor := TCompressedBlockWriter.Create(CryptoStream, TLZMACompressor, ALevel, pCompressProc);
    end;
  end;
end;

procedure TEncryptedCompressionStream.UpdateProgress(Sender: TObject);
begin
  FileProgressBar.Position := TBZCompressionStream(Sender).Position - FileProgressBar.Tag;
  FilesProgressBar.Position := TBZCompressionStream(Sender).Position - FilesProgressBar.Tag;
end;

procedure TEncryptedCompressionStream.BytesProcessed(ABytesProcessed: cardinal);
begin
  FileProgressBar.Position := FileProgressBar.Position + integer(ABytesProcessed);
end;

destructor TEncryptedCompressionStream.Destroy;
begin
  if CompressionType = cmpLZMA then
    TCompressedBlockWriter(Compressor).Finish;

  if (CompressionType <> cmpNone) then
    Compressor.Free;

  if CipherType <> cptNone then
    CryptoStream.Free;

  inherited;
end;

function TEncryptedCompressionStream.Read(var ABuffer;
  ACount: Integer): Longint;
begin
  raise Exception.Create('Unable to read TEncryptedCompressionStream!');
end;

function TEncryptedCompressionStream.Seek(AOffset: Integer;
  AOrigin: Word): Longint;
begin
  raise Exception.Create('Unable to seek TEncryptedCompressionStream!');
end;

function TEncryptedCompressionStream.Write(const ABuffer;
  ACount: Integer): Longint;
begin
  case CompressionType of
    cmpNone: FCryptoStream.Write(ABuffer, ACount);
    cmpBZ2, cmpLZMA: Compressor.Write(ABuffer, ACount);
  end;
  Result := ACount;
end;

constructor TCryptoWriteStream.Create(const AOutput: TStream;
  const ACipherType: TCipherTypes; const APassword: string);
begin
  FCipherType := ACipherType;
  FOutput := AOutput;
  FVirtualMode := true;

  FStartPosition := Output.Position;
  if not InitCrypto(FCrypto, CipherType) then
    raise Exception.Create('Unable to init crypto!');
  if not KeyCrypto(Crypto, APassword) then
    raise Exception.Create('Unable to load key!');
end;

destructor TCryptoWriteStream.Destroy;
begin
  Crypto.Free;
  inherited;
end;

function TCryptoWriteStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if not VirtualMode then
    Result := inherited Seek(Offset, Origin)
  else
    Result := Output.Seek(Offset, Origin);
end;

function TCryptoWriteStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  FVirtualMode := false;
  try
    Result := inherited Write(ABuffer, ACount);
    Position := 0;
    if integer(Crypto.EncryptStream(Self, Output, Result)) <> ACount then
      raise Exception.Create('TCryptoWriteStream encryption error!');
    Result := ACount;
  finally
    Size := 0;
    FVirtualMode := true;
  end;
end;

end.

