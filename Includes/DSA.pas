unit DSA;

interface

uses
  Classes, Shared, Windows;

type
  TErrorMessageProc = procedure(const AMessage: string; const AIcon: integer = MB_ICONWARNING);

function VerifyHashFile(const AHashFilename, AP, AQ, AG, AY: string; const AInputFileStream: TStream): boolean; overload;
function VerifyHashFile(const AHashFilename, AP, AQ, AG, AY, AFilename: string): boolean; overload;
procedure GetOurHash(const AFilestream: TStream; out ADigest: TSHA256Digest); overload;

implementation

uses
  DCPSHA256, SysUtils, FGInt, FGIntDSA;

procedure GetOurHash(const AFilestream: TStream; out ADigest: TSHA256Digest);
var
  iPosition: integer;
begin
  iPosition := AFileStream.Position;
  try
    AFileStream.Position := 0;
    with TDCP_SHA256.Create(nil) do
      try
        Init;
        UpdateStream(AFileStream, AFileStream.Size);
        Final(ADigest);
      finally
        Free;
      end;
  finally
    AFileStream.Position := iPosition;
  end;
end;

function VerifyDSA(const AP, AQ, AG, AY, AM, AR, AAS: string): boolean;
var
  fSuccess: boolean;
  P: TFGInt;
  Q: TFGInt;
  G: TFGInt;
  Y: TFGInt;
begin
  Base10StringToFGInt(AP, P);
  Base10StringToFGInt(AQ, Q);
  Base10StringToFGInt(AG, G);
  Base10StringToFGInt(AY, Y);
  DSAVerify(P, Q, G, Y, AM, AR, AAS, fSuccess);
  Result := fSuccess;
end;

function VerifySignature(const AP, AQ, AG, AY, AR, AAS: string; const ADigest: TSHA256Digest): boolean;
begin
  Result := VerifyDSA(AP, AQ, AG, AY, DigestToString(ADigest), AR, AAS);
end;

function VerifyHashFile(const AHashFilename, AP, AQ, AG, AY, AFilename: string): boolean;
var
  ifFileStream: TFileStream;
begin
  ifFileStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    VerifyHashFile(AHashFilename, AP, AQ, AG, AY, ifFileStream);
  finally
    ifFileStream.Free;
  end;
end;

function VerifyHashFile(const AHashFilename, AP, AQ, AG, AY: string; const AInputFileStream: TStream): boolean;
var
  ifInput: TextFile;
  strR: string;
  strS: string;
  strOurHash: string;
  strHOurHash: string;
  pCrypto: TDCP_SHA256;
  strLine: string;
  strAuthorised: string;
  fFound: boolean;
  shasFileDigest: TSHA256Digest;
  shasHashFileDigest: TSHA256Digest;
begin
  Result := false;
  GetOurHash(AInputFileStream, shasFileDigest);
  strOurHash := DigestToString(shasFileDigest);
  strHOurHash := StrToHex(strOurHash);
  try
    AssignFile(ifInput, AHashFilename);
    Reset(ifInput);
    try
      fFound := false;
      pCrypto := TDCP_SHA256.Create(nil);
      try
        pCrypto.Init;
        while not EOF(ifInput) do
        begin
          ReadLn(ifInput, strLine);
          if strLine = '' then
            break;
          ReadLn(ifInput, strAuthorised);
          if strLine = strHOurHash then
            fFound := true;
          pCrypto.UpdateStr(strLine + CRLF + strAuthorised + CRLF);
        end;
        pCrypto.Final(shasHashFileDigest);
        if EOF(ifInput) then
          exit;
        ReadLn(ifInput, strR);
        if EOF(ifInput) then
          exit;
        ReadLn(ifInput, strS);
        if not fFound then
          raise Exception.Create('This file has not been authorised, check for corruption (and for viruses).');
        if strAuthorised <> 'True' then
          raise Exception.Create('Access denied, this file has been deauthorised.');
        if not VerifySignature(AP, AQ, AG, AY, HexToStr(strR), HexToStr(strS), shasHashFileDigest) then
          raise Exception.Create('DSA signature verification failure!');
        Result := true;
      finally
        pCrypto.Free;
      end;
    finally
      CloseFile(ifInput);
    end;
  except
    raise;
  end;
end;

end.

