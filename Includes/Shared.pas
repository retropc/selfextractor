unit Shared;

interface

uses
  DCPCrypt2, Classes, Messages, SysUtils;

const
  VERSION = 9;
  CR = #13;
  LF = #10;
  CRLF = CR + LF;

type
  EDSAException = class(Exception);
  
  TSHA256Digest = array[0..7] of DWORD;

  TCipherTypes = (cptBlowfish, cptTwofish, cptAES, cptNone);
  TCompressionTypes = (cmpNone, cmpBZ2, cmpLZMA);
  TExecuteBeforeWait = procedure();
  

  TChecksumFileStream = class(TFileStream)
  private
    FChecksum: integer;
  public
    procedure UpdateChecksum(const ABuffer; const ABufferLength: integer);
    procedure WriteBufferChecksum(const ABuffer; const ABufferLength: integer);
    procedure ReadBufferChecksum(var ABuffer; const ABufferLength: integer);
    property Checksum: integer read FChecksum;
    constructor Create(const FileName: string; Mode: Word); reintroduce; overload;
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal); reintroduce; overload;
  end;

var
  pgWriteBuffer: TFileStream;

function CalculateChecksum(const APassword: string): integer;
function InitCrypto(out ACryptoObject: TDCP_BlockCipher; const ACipherType: TCipherTypes = cptBlowfish): boolean;
function KeyCrypto(const ACryptoObject: TDCP_BlockCipher; const APassword: string): boolean;
function DigestToString(const AInput: TSHA256Digest): string;
function StrToHex(const AInput: string; const AAdd: string = ''): string;
function HexToStr(const AInput: string): string;
function SHAFile(const AFilename: string): string;
function ExecuteFollower(const AFilename, AParamaters, ADirectory: string; const AExecuteBeforeWait: TExecuteBeforeWait = nil): boolean;

implementation

uses
  DCPBlowfish, DCPSHA256, DCPTwofish, DCPrijndael, Windows;

function CalculateChecksum(const APassword: string): integer;
var
  cCount: integer;
begin
  if length(APassword) = 0 then
    Result := 0
  else
  begin
    Result := 1;
    for cCount := 1 to length(APassword) do
      Result := Result * (((ord(APassword[cCount]) + cCount - 1) mod 256) + 1);
  end;
end;

function InitCrypto(out ACryptoObject: TDCP_BlockCipher;
  const ACipherType: TCipherTypes): boolean;
begin
  Result := true;
  case ACipherType of
    cptBlowfish: ACryptoObject := TDCP_Blowfish.Create(nil);
    cptTwofish: ACryptoObject := TDCP_Twofish.Create(nil);
    cptAES: ACryptoObject := TDCP_Rijndael.Create(nil);
    else
      Result := false;
  end;
end;

function KeyCrypto(const ACryptoObject: TDCP_BlockCipher; const APassword: string): boolean;
begin
  with ACryptoObject do
  begin
    InitStr(APassword, TDCP_sha256);
    if Initialized then
       CipherMode := cmCBC;
    Result := Initialized;
  end;
end;

{ TChecksumFileStream }

constructor TChecksumFileStream.Create(const FileName: string; Mode: Word);
begin
  inherited;
  FChecksum := 1;
end;

constructor TChecksumFileStream.Create(const FileName: string; Mode: Word;
  Rights: Cardinal);
begin
  inherited;
  FChecksum := 1;
end;

procedure TChecksumFileStream.ReadBufferChecksum(var ABuffer;
  const ABufferLength: integer);
begin
  ReadBuffer(ABuffer, ABufferLength);
  UpdateChecksum(ABuffer, ABufferLength);
end;

procedure TChecksumFileStream.UpdateChecksum(const ABuffer;
  const ABufferLength: integer);
var
  pBuffer: PByte;
  iCount: integer;
begin
  pBuffer := @ABuffer;
  for iCount := 0 to ABufferLength - 1 do
  begin
    FChecksum := FChecksum * ((pBuffer^ + iCount + Position) mod 256 + 1);
    inc(pBuffer);
  end;
end;

procedure TChecksumFileStream.WriteBufferChecksum(const ABuffer;
  const ABufferLength: integer);
begin
  WriteBuffer(ABuffer, ABufferLength);
  UpdateChecksum(ABuffer, ABufferLength);
end;

function DigestToString(const AInput: TSHA256Digest): string;
begin
  setlength(Result, sizeof(AInput));
  CopyMemory(@Result[1], @AInput, length(Result));
end;

function StrToHex(const AInput: string; const AAdd: string = ''): string;
var
  iCount: integer;
begin
  Result := '';
  for iCount := 1 to length(AInput) do
    Result := Result + AAdd + IntToHex(ord(AInput[iCount]), 2);
end;

function HexToStr(const AInput: string): string;
const
  AHexValues: string = '0123456789ABCDEF';
var
  iCount: integer;
begin
  Result := '';
  for iCount := 1 to length(AInput) div 2 do
    Result := Result + chr((Pos(Copy(AInput, (iCount - 1) * 2 + 1, 1), AHexValues) - 1) shl 4 + Pos(Copy(AInput, (iCount - 1) * 2 + 2, 1), AHexValues) - 1);
end;

function SHADigestFile(const AFilename: string): TSHA256Digest;
var
  pHashObject: TDCP_sha256;
  ifStream: TFileStream;
begin
  pHashObject := TDCP_sha256.Create(nil);
  try
    pHashObject.Init;
    ifStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      pHashObject.UpdateStream(ifStream, ifStream.Size);
      pHashObject.Final(Result);
    finally
      ifStream.Free;
    end;
  finally
    pHashObject.Free;
  end;
end;

function SHAFile(const AFilename: string): string;
begin
  Result := StrToHex(DigestToString(SHADigestFile(AFilename)));
end;

function ExecuteFollower(const AFilename, AParamaters, ADirectory: string; const AExecuteBeforeWait: TExecuteBeforeWait = nil): boolean;
var
  eStartupInfo: TStartupInfo;
  eProcessInfo: TProcessInformation;
  sCurrentDir: string;
  sExec: string;
  bOK: boolean;
begin
  Result := false;
  ZeroMemory(@eStartupInfo, sizeof(eStartupInfo));
  eStartupInfo.cb := sizeof(eStartupInfo);
  sExec := ADirectory + '\' + AFilename;
  sCurrentDir := GetCurrentDir;
  SetCurrentDir(ADirectory);
  try
    if AParamaters = '' then
      bOK := CreateProcess(nil, PAnsiChar(sExec), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, eStartupInfo, eProcessInfo)
    else
      bOK := CreateProcess(PAnsiChar(sExec), PAnsiChar(sExec + ' ' + AParamaters), nil, nil, False, NORMAL_PRIORITY_CLASS, nil, nil, eStartupInfo, eProcessInfo);
    if not bOK then
      exit;
    try
      if Assigned(AExecuteBeforeWait) then
        AExecuteBeforeWait;
      WaitForSingleObject(eProcessInfo.hProcess, INFINITE);
      Sleep(500);
    finally
      CloseHandle(eProcessInfo.hProcess);
      CloseHandle(eProcessInfo.hThread);
    end;
    Result := true;
  finally
    SetCurrentDir(PAnsiChar(sCurrentDir));
  end;
end;

end.
