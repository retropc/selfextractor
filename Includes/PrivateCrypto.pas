unit PrivateCrypto;

interface

uses
  FGInt;

type
  THashedFile = class
  private
    FFileHash: string;
    FNewItem: boolean;
    FAuthorised: boolean;
  public
    constructor Create(const AFileHash: string; const AAuthorised: boolean); overload;
    constructor Create(const AFileName: string); overload;
  published
    property FileHash: string read FFileHash;
    property Authorised: boolean read FAuthorised write FAuthorised;
    property NewItem: boolean read FNewItem write FNewItem;
  end;

  TPrivateKey = class
  private
    P: TFGInt;
    Q: TFGInt;
    G: TFGInt;
    X: TFGInt;
  public
    constructor Create(const AFilename: string; const AKey: string);
    destructor Destroy; override;
    procedure Sign(const AData: string; out AR, AAS: string);
  end;

implementation

uses
  FGIntDSA, SysUtils, Windows, Classes, DCPrijndael, DCPcrypt2,
  DCPSHA256, Shared;

{ TPrivateKey }

constructor TPrivateKey.Create(const AFilename, AKey: string);
var
  ifFile: TFileStream;
  ofMemoryParser: TMemoryStream;
  iMagic: word;
  strBuf: string;
begin
  ofMemoryParser := TMemoryStream.Create;
  try
    ifFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      with TDCP_rijndael.Create(nil) do
        try
          InitStr(AKey, TDCP_SHA256);
          DecryptStream(ifFile, ofMemoryParser, ifFile.Size);
        finally
          Free;
        end;
    finally
      ifFile.Free;
    end;
    with ofMemoryParser do
    begin
      Position := 0;
      ReadBuffer(iMagic, sizeof(iMagic));
      if iMagic <> ord('A') shl 4 + ord('H') then
          raise Exception.Create('Invalid key!');

      ReadBuffer(iMagic, sizeof(iMagic));
      setlength(strBuf, iMagic);
      ReadBuffer(strBuf[1], iMagic);
      Base10StringToFGInt(strBuf, P);
      ReadBuffer(iMagic, sizeof(iMagic));
      setlength(strBuf, iMagic);
      ReadBuffer(strBuf[1], iMagic);
      Base10StringToFGInt(strBuf, Q);
      ReadBuffer(iMagic, sizeof(iMagic));
      setlength(strBuf, iMagic);
      ReadBuffer(strBuf[1], iMagic);
      Base10StringToFGInt(strBuf, G);
      ReadBuffer(iMagic, sizeof(iMagic));
      setlength(strBuf, iMagic);
      ReadBuffer(strBuf[1], iMagic);
      Base10StringToFGInt(strBuf, X);

    end;
  finally
    ofMemoryParser.Free;
  end;
end;

destructor TPrivateKey.Destroy;
begin
  FGIntDestroy(P);
  FGIntDestroy(Q);
  FGIntDestroy(X);
  FGIntDestroy(G);
  inherited;
end;

{ TPrivateKey }

procedure TPrivateKey.Sign(const AData: string; out AR, AAS: string);
var
  iRand1: cardinal;
  iRand2: cardinal;
  iRandom: cardinal;
  fgsK: TFGInt;
begin
  iRand1 := Random(High(cardinal)) + Random(High(cardinal)) + 1;
  iRand2 := Random(High(cardinal)) + Random(High(cardinal)) + 1;
  iRandom := iRand1 * iRand2;

  Base10StringToFGInt(inttostr(iRandom), fgsK);

  DSASign(P, Q, G, X, fgsK, AData, AR, AAS);
end;

{ THashedFile }

constructor THashedFile.Create(const AFileHash: string; const AAuthorised: boolean);
begin
  FFileHash := AFileHash;
  FAuthorised := AAuthorised;
  FNewItem := false;
end;

constructor THashedFile.Create(const AFilename: string);
begin
  FFilehash := SHAFile(AFilename);
  FAuthorised := true;
  FNewItem := true;
end;

function StrToHex(const AInput: string): string;
var
  iCount: integer;
begin
  Result := '';
  for iCount := 1 to length(AInput) do
    Result := Result + IntToHex(ord(AInput[iCount]), 2);
end;

end.
