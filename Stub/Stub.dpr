program Stub;
     
{$R 'Stub.res' 'Stub.rc'}

uses
  Classes,
  Windows,
  SysUtils,
  DCPCrypt2,
  DCPBlowfish,
  BZip2,
  LZMA,
  Compress,
  Messages,
  Commctrl,
  DCPSHA256,
  DSA,
  DecryptCompressedStream,
  Shared,
  PasswordDialog in 'PasswordDialog.pas',
  TemporaryDirectory;

var
  oFileStream: TFileStream;
  oEncryptedCompressedStream: TDecryptCompressedStream;
  sCaption: string = 'Self extractor';
  iPasswordValue: integer;
  sExecutable: string = '';
  sParamaters: string = '';
  sExtractIfFull: string = '';
  cFileCount: cardinal;
  cExecutableStartPosition: cardinal;
  oFilesCreated: TStringList;
  oDirectoriesCreated: TStringList;
  eCryptoType: TCipherTypes;
  eCompressionType: TCompressionTypes;
  cTotalSize: cardinal;
  cDataSize: cardinal;
  sExtractionDir: string;
  
{
procedure Clearup;
var
  cCount: cardinal;
begin
  if oFilesCreated.Count > 0 then
  begin
    for cCount := oFilesCreated.Count - 1 downto 0 do
      DeleteFile(PAnsiChar(oFilesCreated.Strings[cCount]));
    oFilesCreated.Clear;
  end;
  if oDirectoriesCreated.Count > 0 then
  begin
    for cCount := oDirectoriesCreated.Count - 1 downto 0 do
      RemoveDirectory(PAnsiChar(oDirectoriesCreated.Strings[cCount]));
    oDirectoriesCreated.Clear;
  end;
end;
}

procedure DelDir(const ADirName: string);
var
  ssrSearchRec: TSearchRec;
  iGotOne: integer;
begin
  iGotOne := FindFirst(ADirName + '\*.*', faAnyFile, ssrSearchRec);
  while iGotOne = 0 do
  begin
    if ((ssrSearchRec.Attr and faDirectory) = 0) then
      DeleteFile(ADirName + '\' + ssrSearchRec.Name)
    else if (ssrSearchRec.Name <> '.') and (ssrSearchRec.Name <> '..') then
      DelDir(ADirName + '\' + ssrSearchRec.Name);
    iGotOne := FindNext(ssrSearchRec);
  end;
  FindClose(ssrSearchRec);
  RemoveDir(ADirName);
end;

procedure Clearup;
begin
  DelDir(sExtractionDir);
end;

function GetCurrentDir: string;
begin
  setlength(Result, GetCurrentDirectory(1, nil));
  GetCurrentDirectory(length(Result), PAnsiChar(Result));
  setlength(Result, length(Result) - 1);
end;

function ConvertToUNCPath(const AMappedDrive: string): string;
var
  sRemoteString: string;
  sDrive: string;
  cStringLength: cardinal;
begin
  Result := '';
  sDrive := Copy(AMappedDrive, 1, 2);
  cStringLength := 0;
  if WNetGetConnection(PAnsiChar(sDrive), @sRemoteString[1], cStringLength) <> ERROR_MORE_DATA then
    exit;
  setlength(sRemoteString, cStringLength);
  if cardinal(length(sRemoteString)) <> cStringLength then
    exit;
  if WNetGetConnection(PAnsiChar(sDrive), @sRemoteString[1], cStringLength) <> NO_ERROR then
    exit;
  setlength(sRemoteString, cStringLength - 1);
  Result := sRemoteString;
end;

function GetLongPathName(const APathName: string): string;
var
  sDrive: string;
  sPath: string;
  eSearchRec: TSearchRec;
begin
  sDrive := ExtractFileDrive(APathName);
  sPath := Copy(APathName, Length(sDrive) + 1, Length(APathName));
  if (sPath = '') or (sPath = '\') then
  begin
    Result := APathName;
    if Result[Length(Result)] = '\' then
      Delete(Result, Length(Result), 1);
  end
  else
  begin
    sPath := GetLongPathName(ExtractFileDir(APathName));
    if FindFirst(APathName, faAnyFile, eSearchRec) = 0 then
    begin
{$WARNINGS OFF}
      Result := sPath + '\' + eSearchRec.FindData.cFileName;
{$WARNINGS ON}
      FindClose(eSearchRec);
    end
    else
      Result := sPath + '\' + ExtractFileName(APathName);
  end;
end;

function GetRealExecutableName: string;
begin
  Result := GetLongPathName(ExpandUNCFileName(ParamStr(0)));
end;

procedure CreateDirectory(const ADirectory: string);
begin
  if not DirectoryExists(ADirectory) then
  begin
    ForceDirectories(ADirectory);
    oDirectoriesCreated.Add(ADirectory);
  end;
end;

procedure GetOurHash(out ADigest: TSHA256Digest);
var
  iPosition: integer;
begin
  iPosition := oFileStream.Position;
  try
    oFileStream.Position := 0;
    with TDCP_SHA256.Create(nil) do
      try
        Init;
        UpdateStream(oFileStream, oFileStream.Size);
        Final(ADigest);
      finally
        Free;
      end;
  finally
    oFileStream.Position := iPosition;
  end;
end;

function ExtractRemainingValues: TExtractReturn;
var
  wForceUNCPathLength: word;
  sForceUNCPath: string;
  wExecutableLength: word;
  wParamaterLength: word;
  wExtractIfFullLength: word;
  wHashLength: word;
  wKeyLength: word;
  sPKey: string;
  sQKey: string;
  sGKey: string;
  sYKey: string;
  sHashPath: string;
begin
  with oEncryptedCompressedStream do
  begin
    ReadBuffer(wForceUNCPathLength, sizeof(wForceUNCPathLength));
    setlength(sForceUNCPath, wForceUNCPathLength);
    if wForceUNCPathLength <> 0 then
    begin
      ReadBuffer(sForceUNCPath[1], wForceUNCPathLength);
      if CompareText(sForceUNCPath, GetRealExecutableName) <> 0 then
      begin
        Result := ecrPathError;
        exit;
      end;
    end
    else
      sForceUNCPath := '';

    ReadBuffer(wExecutableLength, sizeof(wExecutableLength));
    setlength(sExecutable, wExecutableLength);
    ReadBuffer(sExecutable[1], wExecutableLength);

    ReadBuffer(wParamaterLength, sizeof(wParamaterLength));
    setlength(sParamaters, wParamaterLength);

    if wParamaterLength <> 0 then
      ReadBuffer(sParamaters[1], wParamaterLength)
    else if ParamCount > 0 then
    begin
      sParamaters := StringReplace(GetCommandLine, ParamStr(0), '', [rfReplaceAll]);
      while Copy(sParamaters, 1, 1) = ' ' do
        sParamaters := Copy(sParamaters, 2, MaxInt);
    end;

    ReadBuffer(wExtractIfFullLength, sizeof(wExtractIfFullLength));
    setlength(sExtractIfFull, wExtractIfFullLength);
    if wExtractIfFullLength <> 0 then
      ReadBuffer(sExtractIfFull[1], wExtractIfFullLength)
    else
      sExtractIfFull := '';

    ReadBuffer(wHashLength, sizeof(wHashLength));
    setlength(sHashPath, wHashLength);
    ReadBuffer(sHashPath[1], wHashLength);
    if wHashLength <> 0 then
    begin
      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      setlength(sPKey, wKeyLength);
      ReadBuffer(sPKey[1], wKeyLength);

      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      setlength(sQKey, wKeyLength);
      ReadBuffer(sQKey[1], wKeyLength);

      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      setlength(sGKey, wKeyLength);
      ReadBuffer(sGKey[1], wKeyLength);

      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      setlength(sYKey, wKeyLength);
      ReadBuffer(sYKey[1], wKeyLength);
      try
        VerifyHashFile(sHashPath, sPKey, sQKey, sGKey, sYKey, oFileStream);
      except
        on E:Exception do
        begin
          ErrorMessage(E.Message, MB_ICONERROR);
          Result := ecrDealt;
          exit;
        end;
      end;
    end
    else
    begin
      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      ReadBuffer(wKeyLength, sizeof(wKeyLength));
      ReadBuffer(wKeyLength, sizeof(wKeyLength));
    end;
    
    ReadBuffer(cFileCount, sizeof(cFileCount));
    if cFileCount = 0 then
    begin
      Result := ecrInvalidFileCount;
      exit;
    end;
    Result := ecrOK;
  end;
end;

function Extraction(const APassword, ADestination: string): TExtractReturn;
var
  wFilenameSize: word;
  sFilename: string;
  sOutputFilename: string;
  oOutput: TFileStream;
  cFileSize: cardinal;
  pProgress: HWND;
begin
  if iPasswordValue <> CalculateChecksum(APassword) then
  begin
    Result := ecrInvalidPassword;
    exit;
  end;

  oFileStream.Position := cExecutableStartPosition;

  oEncryptedCompressedStream := TDecryptCompressedStream.Create(oFileStream, eCryptoType, eCompressionType, cDataSize, APassword);
  with oEncryptedCompressedStream do
    try
      Result := ExtractRemainingValues;
      if Result <> ecrOK then
        exit;

      HideForm;

      Result := ecrError;
      sExtractionDir := ADestination;
      CreateDirectory(ADestination);

      try
        pProgress := CreateProgressDialog(cFileCount);
        try
          while(cFileCount > 0) do
          begin
            SendMessage(pProgress, PBM_STEPIT, 0, 0);
            SendMessage(pProgress, WM_PAINT, 0, 0);
            Read(wFilenameSize, sizeof(word));
            setlength(sFilename, wFilenameSize);
            Read(sFilename[1], wFilenameSize);
            Read(cFileSize, sizeof(cFileSize));
            sOutputFilename := ADestination + '\' + sFilename;
            CreateDirectory(ExtractFilePath(sOutputFilename));
            oOutput := TFileStream.Create(sOutputFilename, fmCreate or fmShareExclusive);
            try
              oFilesCreated.Add(sOutputFilename);
              if cFileSize <> 0 then
                oOutput.CopyFrom(oEncryptedCompressedStream, cFileSize);
            finally
              oOutput.Free;
            end;
            dec(cFileCount);
          end;
          Result := ecrOK;
        finally
          DestroyWindow(pProgress);
        end;
      finally
        if Result <> ecrOK then
          Clearup;
      end;
    finally
      Free;
    end;
end;

function CheckDiskSpace(const ADrive: string): TExtractReturn;
var
  iDiskSpaceFree: int64;
begin
  Result := ecrDiskSpaceError;

  iDiskSpaceFree := DiskFree(ord(Lowercase(Copy(ADrive, 1, 1))[1]) - ord('a') + 1);
  if iDiskSpaceFree = -1 then
    exit;

  if iDiskSpaceFree < cTotalSize then
  begin
    Result := ecrInsufficientDiskSpace;
    exit;
  end
  else
    Result := ecrOK;
end;

function ExtractFiles(const APassword: string): TExtractReturn;
var
  sDirectory: string;
begin
  sDirectory := GetTempDirectory;
  if sDirectory = '' then
  begin
    Result := ecrTempDirectory;
    exit;
  end;

  Result := CheckDiskSpace(Copy(sDirectory, 1, 3));
  if Result <> ecrOK then
  begin
    if (sExtractIfFull <> '') and DirectoryExists(sExtractIfFull) then
    begin
      sDirectory := GetTempDirectory(sExtractIfFull);
      if sDirectory = '' then
      begin
        Result := ecrTempDirectory;
        exit;
      end;
      Result := CheckDiskSpace(Copy(sDirectory, 1, 3));
      if Result <> ecrOK then
        exit;
    end
    else
      exit;
  end;
  Result := Extraction(APassword, sDirectory);
  if Result = ecrOK then
  begin
    try
      ExecuteFollower(sExecutable, sParamaters, sDirectory, HideForm);
    finally
      Clearup;
      Terminate;
    end;
  end;
end;

function PrepareStreamValues: TExtractReturn;
var
  aMagicDigits: array[0..1] of char;
  bCaptionLength: byte;
  bVersion: byte;
begin
  Result := ecrError;
  oFileStream := TChecksumFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyWrite);
//  oFileStream := TFileStream.Create('N:\MyDocuments\SE\Verify.exe', fmOpenRead or fmShareDenyWrite);
  with oFileStream do
    try
      Position := Size - (sizeof(cExecutableStartPosition) + sizeof(aMagicDigits) + sizeof(cTotalSize));
      cDataSize := Position;
      ReadBuffer(cTotalSize, sizeof(cTotalSize));
      ReadBuffer(cExecutableStartPosition, sizeof(cExecutableStartPosition));
      ReadBuffer(aMagicDigits, 2);
      if (aMagicDigits[0] <> 'B') or (aMagicDigits[1] <> 'Z') then
      begin
        Result := ecrInvalidFormat;
        exit;
      end;
      Position := cExecutableStartPosition;
      ReadBuffer(bVersion, sizeof(bVersion));
      if bVersion <> VERSION then
      begin
        Result := ecrInvalidVersion;
        exit;
      end;
      ReadBuffer(eCryptoType, sizeof(eCryptoType));
      ReadBuffer(eCompressionType, sizeof(eCompressionType));

      ReadBuffer(iPasswordValue, sizeof(iPasswordValue));

      ReadBuffer(bCaptionLength, sizeof(bCaptionLength));
      setlength(sCaption, bCaptionLength);
      ReadBuffer(sCaption[1], bCaptionLength);

      cExecutableStartPosition := Position;
      cDataSize := cDataSize - Position;
      oFilesCreated := TStringList.Create;
      try
        oDirectoriesCreated := TStringList.Create;
        try
          Result := ecrOK;
        finally
          if Result <> ecrOK then
            oDirectoriesCreated.Free;
        end;
      finally
        if Result <> ecrOK then
          oFilesCreated.Free;
      end;
    finally
      if Result <> ecrOK then
        oFileStream.Free;
    end;
end;

begin
  if not HandleError(PrepareStreamValues) then
    exit;
  try
    if iPasswordValue <> 0 then
      CreatePasswordDialog(@ExtractFiles, sCaption)
    else
      HandleError(ExtractFiles(''));
  finally
    oFileStream.Free;
    oDirectoriesCreated.Free;
    oFilesCreated.Free;
  end;
end.

