unit DecryptCompressedStream;

interface

uses
  Classes, DCPCrypt2, Shared;
  
type
  TCryptoReadStream = class(TMemoryStream)
  private
    FCrypto: TDCP_BlockCipher;
    FCipherType: TCipherTypes;
    FInput: TStream;
    FSize: int64;
    FRead: int64;
  protected
    property Crypto: TDCP_BlockCipher read FCrypto;
  published
    property CipherType: TCipherTypes read FCipherType;
    property Input: TStream read FInput;
    property DataRead: int64 read FRead;
    function GetSize: int64; override;
  public
    constructor Create(const AInput: TStream; const ACipherType: TCipherTypes; const ADataLeft: int64; const APassword: string = '');
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
  end;

  TDecryptCompressedStream = class(TStream)
  private
    FInput: TStream;
    FDecompressor: TStream;
    FCipherType: TCipherTypes;
    FCryptoStream: TMemoryStream;
    FCompressionType: TCompressionTypes;
  protected
    property Decompressor: TStream read FDecompressor;
    property CryptoStream: TMemoryStream read FCryptoStream;
    property Input: TStream read FInput;
  public
    constructor Create(const AInput: TStream; const ACipherType: TCipherTypes; const ACompressionType: TCompressionTypes; const ADataLeft: int64 = 0; const APassword: string = '');
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
  published
    property CompressionType: TCompressionTypes read FCompressionType;
    property CipherType: TCipherTypes read FCipherType;
  end;

implementation

uses
  BZip2, Compress, SysUtils, LZMA;

{ TCryptoReadStream }

constructor TCryptoReadStream.Create(const AInput: TStream;
  const ACipherType: TCipherTypes; const ADataLeft: int64; const APassword: string);
begin
  FCipherType := ACipherType;
  FInput := AInput;

  FSize := ADataLeft;
  FRead := 0;

  if CipherType <> cptNone then
  begin
    if not InitCrypto(FCrypto, CipherType) then
      raise Exception.Create('Unable to init crypto!');
    if not KeyCrypto(Crypto, APassword) then
      raise Exception.Create('Unable to load key!');
  end;
end;

destructor TCryptoReadStream.Destroy;
begin
  Crypto.Free;
  inherited;
end;

function TCryptoReadStream.GetSize: int64;
begin
  Result := FSize;
end;

function TCryptoReadStream.Read(var ABuffer; ACount: Integer): Longint;
begin
  try
    if ACount > FSize - DataRead then
      ACount := FSize - DataRead;
    inc(FRead, ACount);
    ACount := Crypto.DecryptStream(Input, Self, ACount);
    Position := 0;
    Result := inherited Read(ABuffer, ACount);
  finally
    Size := 0;
  end;
end;

function TDecryptCompressedStream.Seek(AOffset: Integer;
  AOrigin: Word): Longint;
begin
  raise Exception.Create('Unable to seek TDecryptCompressionStream!');
end;

function TDecryptCompressedStream.Write(const ABuffer;
  ACount: Integer): Longint;
begin
  raise Exception.Create('Unable to write TDecryptCompressionStream!');
end;

{ TDecryptCompressedStream }

constructor TDecryptCompressedStream.Create(const AInput: TStream;
  const ACipherType: TCipherTypes; const ACompressionType: TCompressionTypes;
  const ADataLeft: int64; const APassword: string);
begin
  FInput := AInput;

  FCompressionType := ACompressionType;
  FCipherType := ACipherType;

  if ACipherType <> cptNone then
    FCryptoStream := TCryptoReadStream.Create(AInput, ACipherType, ADataLeft, APassword)
  else
    FCryptoStream := TCryptoReadStream(Input);

  case ACompressionType of
    cmpBZ2: FDecompressor := TBZDecompressionStream.Create(CryptoStream);
    cmpLZMA: FDecompressor := TCompressedBlockReader.Create(CryptoStream, TLZMADecompressor, ADataLeft);
  end;
end;

destructor TDecryptCompressedStream.Destroy;
begin
  if (CompressionType = cmpBZ2) or (CompressionType = cmpLZMA) then
    Decompressor.Free;

  if CipherType <> cptNone then
    CryptoStream.Free;

  inherited;
end;

function TDecryptCompressedStream.Read(var ABuffer;
  ACount: Integer): Longint;
begin
  if CompressionType = cmpNone then
    Result := CryptoStream.Read(ABuffer, ACount)
  else
    Result := Decompressor.Read(ABuffer, ACount);
end;

end.
