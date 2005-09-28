unit BuilderFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan, ComCtrls;
  
type
  TBuilderForm = class(TForm)
    btnBuild: TButton;
    SaveDialog: TSaveDialog;
    XP: TXPManifest;
    prgProgress: TProgressBar;
    prgFiles: TProgressBar;
    pcOptions: TPageControl;
    tsFiles: TTabSheet;
    memFiles: TMemo;
    tsShell: TTabSheet;
    edtExecutable: TLabeledEdit;
    edtParams: TLabeledEdit;
    tsOptions: TTabSheet;
    edtCaption: TLabeledEdit;
    tsProtection: TTabSheet;
    edtUNC: TLabeledEdit;
    edtPassword: TLabeledEdit;
    rgCipher: TRadioGroup;
    edtExtractHere: TLabeledEdit;
    lblCompression: TLabel;
    tbCompressionLevel: TTrackBar;
    edtRootDirectory: TLabeledEdit;
    btnRecurse: TButton;
    tsLoadSave: TTabSheet;
    btnLoadOptions: TButton;
    btnSaveOptions: TButton;
    OpenSettings: TOpenDialog;
    SaveSettings: TSaveDialog;
    rgCompressionType: TRadioGroup;
    edtHashProtection: TLabeledEdit;
    btnLoadKey: TButton;
    OpenKeyDialog: TOpenDialog;
    edtP: TEdit;
    edtQ: TEdit;
    edtG: TEdit;
    edtY: TEdit;
    procedure btnBuildClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure edtExecutableChange(Sender: TObject);
    procedure btnLoadOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveOptionsClick(Sender: TObject);
    procedure memFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRecurseClick(Sender: TObject);
    procedure rgCompressionTypeClick(Sender: TObject);
    procedure btnLoadKeyClick(Sender: TObject);
  end;
  TBuildReturn = (burOK, burError, burCreateFailed, burStubWriteFailed,
                  burInvalidFileCount, burBufferAllocError, burInputFileError,
                  burCompressionStreamAllocationError,
                  burMemoryStorageAllocError, burCryptoAllocError,
                  burCryptoKeyError, burCryptoUnknownType,
                  burLZMADLLLoadingFailed, burLZMADLLInitFailed);

var
  BuilderForm: TBuilderForm;

implementation

{$R *.dfm}

//{$DEFINE DEMO}

uses
  Shared, IniFiles, BrowseDialog, EncryptedCompressionStream, LZMA, Clipbrd,
  DCPSHA256;
  
{
  Old file format
  ===============

  Stub executable
  Version number                        Byte                    |
  Crypto type                           Byte                    |
  Compression type                      Byte                    |
  Checksum                              Integer, signed         |
  Permitted UNC path length             Word, unsigned  --      |
  UNC path                              String          --      |
  Caption length                        Byte                    |
  Caption                               String                  | Checksummed
  Executable to run afterwards length   Word, unsigned  --      |    here
  Executable to run afterwards          String          --      |
  Paramaters length                     Word, unsigned          |
  Paramaters                            String                  |
  Extract here if drive full length     Word unsigned           |
  Extract here if drive full            String                  |
  File count                            Integer, unsigned       |
  Header checksum                       Integer, signed
  File #n filename length               Word, unsigned
  File #n filename                      String
  File #n size                          Integer, unsigned
  File #n                               String (crypted)
  Total extracted file size             Integer, unsigned
  Size of original executable           Integer, unsigned
  Magic 'BZ'                            2 chars

  New file format
  ===============

  Stub executable
  Version number                        Byte
  Crypto type                           Byte
  Compression type                      Byte
  Checksum                              Integer, signed
  Caption length                        Byte
  Caption                               String

  Permitted UNC path length             Word, unsigned  --      X
  UNC path                              String          --      X
  Executable to run afterwards length   Word, unsigned  --      X
  Executable to run afterwards          String          --      X
  Paramaters length                     Word, unsigned          X
  Paramaters                            String                  X
  Extract here if drive full length     Word unsigned           X
  Extract here if drive full            String                  X
  Hash protection file length           Word, unsigned          X
  Hash protection file                  String                  X
  P key length                          Word, unsigned          X
  P key                                 String                  X
  Q key length                          Word, unsigned          X
  Q key                                 String                  X
  G key length                          Word, unsigned          X
  G key                                 String                  X
  Y key length                          Word, unsigned          X
  Y key                                 String                  X
  File count                            Integer, unsigned       X  crypted 'ere


  File #n filename length               Word, unsigned          X
  File #n filename                      String                  X
  File #n size                          Integer, unsigned       X

  Then the filenames
  File #n                               String (crypted)        X

  Total extracted file size             Integer, unsigned       X
  Size of original executable           Integer, unsigned       X
  Magic 'BZ'                            2 chars                 X
}

function WriteStub(const AFile: TFileStream): integer;
var
  hHandle: THandle;
  hRes: THandle;
  pPointer: Pointer;
  iLength: longint;
begin
  Result := 0;
  hRes := FindResource(hInstance, 'STUB', 'STUB');
  if hRes = 0 then
    exit;
  iLength := SizeofResource(hInstance, hRes);
  if iLength = 0 then
    exit;
  hHandle := LoadResource(hInstance, hRes);
  if hHandle = 0 then
    exit;
  try
    pPointer := LockResource(hHandle);
    try
      AFile.Write(pPointer^, iLength);
      Result := iLength;
    finally
      UnlockResource(hHandle);
    end;
  finally
    FreeResource(hHandle);
  end;
end;

function BadIntegerTextBox(const AInput: TEdit): boolean;
var
  iCount: integer;
  strData: string;
begin
  strData := AInput.Text;
  if strData = '' then
  begin
    Result := true;
    exit;
  end;
  Result := false;
  for iCount := 1 to length(strData) do
    if not (strData[iCount] in ['0'..'9']) and not (strData[iCount] in ['A'..'F']) then
    begin
      Result := true;
      break;
    end;
end;

function BuildExtractor(const ASaveTo, ACaption, APassword, ARunAfter,
  ARootDirectory, AParams, AForceUNCPath, ATempFull: string; const AFiles: TStrings;
  const AFileProgress, AFilesProgress: TProgressBar;
  const AHashFile: string; const AP, AQ, AG, AY: string;
  const ALevel: integer = 8;
  const ACipherType: TCipherTypes = cptBlowFish; const ACompressionType: TCompressionTypes = cmpLZMA
  ): TBuildReturn;
var
  oOutput: TFileStream;
  oInput: TFileStream;
  cStubSize: cardinal;
  iChecksum: integer;
  cFileCount: cardinal;
  cLoopCount: cardinal;
  bCaptionLength: byte;
  wExecutableLength: word;
  sFilename: string;
  wFilenameLength: word;
  wParamLength: word;
  cFileSize: cardinal;
  cStart: cardinal;
  bVersion: byte;
  cTotalFileSize: cardinal;
  cAllFilesSize: cardinal;
  wForceUNCPathLength: word;
  wTempFullLength: word;
  iModule: THandle;
  wHashLength: word;
  wKeyLength: word;
  sKey: string;
begin
  AFileProgress.Position := 0;
  AFilesProgress.Position := 0;

  if AFiles.Count = 0 then
  begin
    Result := burInvalidFileCount;
    exit;
  end;

  Result := burError;
  iModule := 0;

  if ACompressionType = cmpLZMA then
  begin
    iModule := LoadLibrary('lzma.dll');
    if iModule = 0 then
    begin
      Result := burLZMADLLLoadingFailed;
      exit;
    end;
    if not LZMAInitCompressFunctions(iModule) then
    begin
      Result := burLZMADLLInitFailed;
      exit;
    end;
  end;

  try
    oOutput := TFileStream.Create(ASaveTo, fmCreate or fmShareExclusive);
    try
      with oOutput do
      begin
        cStubSize := WriteStub(oOutput);
        if cStubsize = 0 then
        begin
          Result := burStubWriteFailed;
          exit;                                                                       
        end;

        if APassword <> '' then
          iChecksum := CalculateChecksum(APassword)
        else
          iChecksum := 0;

        bVersion := VERSION;

{------- NOT CRYPTED ----------------------------------------------------------}
        WriteBuffer(bVersion, sizeof(bVersion));
        WriteBuffer(ACipherType, sizeof(ACipherType));
        WriteBuffer(ACompressionType, sizeof(ACompressionType));
        WriteBuffer(iChecksum, sizeof(iChecksum));

        bCaptionLength := length(ACaption);
        WriteBuffer(bCaptionLength, sizeof(bCaptionLength));
        WriteBuffer(ACaption[1], bCaptionLength);

{------- CRYPTED --------------------------------------------------------------}
        cTotalFileSize := oOutput.Position;
        with TEncryptedCompressionStream.Create(oOutput, ACipherType, ACompressionType, ALevel, AFileProgress, AFilesProgress, APassword) do
          try
            wForceUNCPathLength := length(AForceUNCPath);
            WriteBuffer(wForceUNCPathLength, sizeof(wForceUNCPathLength));
            WriteBuffer(AForceUNCPath[1], wForceUNCPathLength);

            wExecutableLength := length(ARunAfter);
            WriteBuffer(wExecutableLength, sizeof(wExecutableLength));
            WriteBuffer(ARunAfter[1], wExecutableLength);

            wParamLength := length(AParams);
            WriteBuffer(wParamLength, sizeof(wParamLength));
            WriteBuffer(AParams[1], wParamLength);

            wTempFullLength := length(ATempFull);
            WriteBuffer(wTempFullLength, sizeof(wTempFullLength));
            WriteBuffer(ATempFull[1], wTempFullLength);

            wHashLength := length(AHashFile);
            WriteBuffer(wHashLength, sizeof(wHashLength));
            if wHashLength = 0 then
            begin
              wKeyLength := 0;
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
            end
            else
            begin
              WriteBuffer(AHashFile[1], wHashLength);
              sKey := AP;
              wKeyLength := length(sKey);
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(sKey[1], wKeyLength);
              sKey := AQ;
              wKeyLength := length(sKey);
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(sKey[1], wKeyLength);
              sKey := AG;
              wKeyLength := length(sKey);
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(sKey[1], wKeyLength);
              sKey := AY;
              wKeyLength := length(sKey);
              WriteBuffer(wKeyLength, sizeof(wKeyLength));
              WriteBuffer(sKey[1], wKeyLength);
            end;
            
            cFileCount := AFiles.Count;
            WriteBuffer(cFileCount, sizeof(cFileCount));

            if ACompressionType = cmpNone then
            begin
              AFileProgress.Max := 1;
              AFileProgress.Position := 1;
            end
            else if ACompressionType = cmpLZMA then
            begin
              AFileProgress.Position := 0;
            end;

            cAllFilesSize := 0;
            for cLoopCount := 0 to cFileCount - 1 do //would just decrease till 0
            begin
               oInput := TFileStream.Create(ARootDirectory + AFiles[cLoopCount], fmOpenRead or fmShareDenyWrite);
              try
                cAllFilesSize := cAllFilesSize + oInput.Size;
              finally
                oInput.Free;
              end;
            end;
            AFilesProgress.Max := cAllFilesSize + oOutput.Position - cTotalFileSize;

            cTotalFileSize := 0;

            for cLoopCount := 0 to cFileCount - 1 do //would just decrease till 0
            begin                                    //but order matters
              sFilename := AFiles[cLoopCount];
              wFilenameLength := length(sFilename);
              WriteBuffer(wFilenameLength, sizeof(wFilenameLength));
              WriteBuffer(sFilename[1], wFilenameLength);

              AFilesProgress.Max := AFilesProgress.Max + sizeof(wFilenameLength) + wFilenameLength;
              cStart := AFilesProgress.Position;
              oInput := TFileStream.Create(ARootDirectory + sFileName, fmOpenRead or fmShareDenyWrite);
              try
                cFileSize := oInput.Size;
                if ACompressionType <> cmpNone then
                begin
                  AFileProgress.Max := cFileSize;
                  AFileProgress.Position := 0;
                  AFileProgress.Tag := cTotalFileSize;
                end;
                inc(cTotalFileSize, cFileSize);
                WriteBuffer(cFileSize, sizeof(cFileSize));
                CopyFrom(oInput, 0);

                AFileProgress.Max := AFileProgress.Position;
              finally
                oInput.Free;
              end;
              AFilesProgress.Position := cStart + cFileSize;
            end;
            if ACompressionType = cmpLZMA then
            begin
              AFileProgress.Position := 0;
              AFileProgress.Max := AFilesProgress.Max;
            end
            else
              AFileProgress.Position := AFileProgress.Max;
          finally
            Free;
          end;
        AFileProgress.Position := AFileProgress.Max;
        AFilesProgress.Max := AFilesProgress.Position;
        WriteBuffer(cTotalFileSize, sizeof(cTotalFileSize));
        WriteBuffer(cStubSize, sizeof(cStubSize));
        WriteBuffer('BZ', 2);

        Result := burOK;
      end;
    finally
      oOutput.Free;
      if Result <> burOK then
        DeleteFile(ASaveTo);
    end;
  finally
    if iModule <> 0 then
      FreeLibrary(iModule);
  end;
end;

function GetFileHash(const AFilename: string): string;
var
  ifFileStream: TFileStream;
  shasDigest: TSHA256Digest;
begin
  ifFileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    with TDCP_SHA256.Create(nil) do
      try
        Init;
        UpdateStream(ifFileStream, ifFileStream.Size);
        Final(shasDigest);
        Result := DigestToString(shasDigest);
      finally
        Free;
      end;
  finally
    ifFileStream.Free;
  end;
end;

procedure TBuilderForm.btnBuildClick(Sender: TObject);
var
  eRetCode: TBuildReturn;
  cLineCount: cardinal;
  oLines: TStringList;
  sWarning: string;
  scurCursor: TCursor;
  eCipherType: TCipherTypes;
  iSubtractWhat: integer;
begin
  Application.ProcessMessages;

  if not SaveDialog.Execute then
    exit;

  Application.ProcessMessages;
  TButton(Sender).Enabled := false;
  try
    scurCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      oLines := TStringList.Create;
      try
        for cLineCount := 0 to memFiles.Lines.Count - 1 do
          if memFiles.Lines[cLineCount] <> '' then
            oLines.Add(memFiles.Lines[cLineCount]);
        if edtPassword.Text = '' then
          eCipherType := cptNone
        else
          eCipherType := TCipherTypes(rgCipher.ItemIndex);
        if rgCompressionType.ItemIndex = 2 then
          iSubtractWhat := 0
        else
          iSubtractWhat := 1;
        eRetCode := BuildExtractor(SaveDialog.Filename, edtCaption.Text,
                                   edtPassword.Text, edtExecutable.Text,
                                   edtRootDirectory.Text + '\', edtParams.Text,
                                   edtUNC.Text, edtExtractHere.Text, oLines,
                                   prgProgress, prgFiles,
                                   edtHashProtection.Text, edtP.Text, edtQ.Text,
                                   edtG.Text, edtY.Text,
                                   tbCompressionLevel.Position - iSubtractWhat,
                                   eCipherType,
                                   TCompressionTypes(rgCompressionType.ItemIndex));
      finally
        FreeAndNil(oLines);
      end;
    finally
      Screen.Cursor := scurCursor;
    end;
    sWarning := '';
    case eRetCode of
      burOK: MessageDlg('Self extractor created!', mtInformation, [mbOK], 0);
      burCreateFailed: sWarning := 'Unable to create filestream!';
      burStubWriteFailed: sWarning := 'Stub write failure!';
      burMemoryStorageAllocError: sWarning := 'Memory stream allocation error!';
      burInvalidFileCount: sWarning := 'No files selected!';
      burBufferAllocError: sWarning := 'Buffer allocation error!';
      burInputFileError: sWarning := 'Input file error!';
      burCompressionStreamAllocationError: sWarning := 'Compression stream allocation error!';
      burCryptoAllocError: sWarning := 'Crypto allocation error!';
      burCryptoKeyError: sWarning := 'Error initialising crypto key!';
      burCryptoUnknownType: sWarning := 'Unknown crypto type!';
      burLZMADLLLoadingFailed: sWarning := 'Unable to load LZMA compression DLL!';
      burLZMADLLInitFailed: sWarning := 'Unable to init LZMA compression DLL!';

      else
        sWarning := 'Unknown error!';
    end;
    if sWarning <> '' then
      MessageDlg(sWarning, mtWarning, [mbOK], 0);
  finally
    TButton(Sender).Enabled := true;
  end;
end;

procedure TBuilderForm.btnBrowseClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    edtExecutable.Text := SaveDialog.Filename;
end;

function CountLines(const ALines: TStrings): cardinal;
var
  cLineCount: cardinal;
begin
  Result := 0;
  with ALines do
    if Count > 0 then
      for cLineCount := 0 to Count - 1 do
        if Strings[cLineCount] <> '' then
          inc(Result);
end;

procedure TBuilderForm.edtExecutableChange(Sender: TObject);
begin
  btnBuild.Enabled := (CountLines(memFiles.Lines) <> 0) and (edtRootDirectory.Text <> '')
                      and (edtExecutable.Text <> '');
  if (edtPassword.Text <> '') and (edtCaption.Text = '') then
    btnBuild.Enabled := false;

  if edtHashProtection.Text <> '' then
    if BadIntegerTextBox(edtP) or BadIntegerTextBox(edtQ) or
       BadIntegerTextBox(edtQ) or BadIntegerTextBox(edtY) then
      btnBuild.Enabled := false;

  rgCipher.Enabled := edtPassword.Text <> '';

end;

procedure TBuilderForm.btnLoadOptionsClick(Sender: TObject);
var
  iCount: integer;
  iCount2: integer;
  iCountS: integer;
begin
  OpenSettings.FileName := '';
  if not OpenSettings.Execute then
    exit;

  with TMemIniFile.Create(OpenSettings.Filename) do
    try
      for iCountS := 0 to pcOptions.PageCount - 1 do
        with pcOptions.Pages[iCountS] do
          for iCount := 0 to ControlCount - 1 do
          begin
            if Controls[iCount].Name = '' then
              continue;
            if Controls[iCount] is TEdit then
              TEdit(Controls[iCount]).Text := ReadString(Controls[iCount].ClassName, Controls[iCount].Name, '')
            else if Controls[iCount] is TLabeledEdit then
              TLabeledEdit(Controls[iCount]).Text := ReadString(Controls[iCount].ClassName, Controls[iCount].Name, '')
            else if Controls[iCount] is TMemo then
            begin
              with TMemo(Controls[iCount]) do
              begin
                Clear;
                for iCount2 := 1 to ReadInteger(ClassName, Name + ' 0', 0) do
                  Lines.Add(ReadString(ClassName, Name + ' ' + inttostr(iCount2), ''));
              end;
            end
            else if Controls[iCount] is TRadioGroup then
            begin
              TRadioGroup(Controls[iCount]).ItemIndex := ReadInteger(Controls[iCount].ClassName, Controls[iCount].Name, 0);
              if Controls[iCount].Name = 'rgCompressionType' then
              begin
                rgCompressionType.OnClick(rgCompressionType);
                tbCompressionLevel.Position := ReadInteger('TTrackBar', 'tbCompressionLevel', 9)
              end;
            end;
          end;
      edtExecutableChange(nil);
    finally
      Free;
    end;
end;

procedure TBuilderForm.FormCreate(Sender: TObject);
begin
  pcOptions.ActivePage := tsFiles;
  rgCompressionType.OnClick(rgCompressionType);
{$IFDEF DEMO}
  Caption := Caption + ' - demo version, limited to 4 files';
{$ENDIF}
end;

procedure TBuilderForm.btnSaveOptionsClick(Sender: TObject);
var
  iCount: integer;
  iCount2: integer;
  iCountS: integer;
begin
  if not SaveSettings.Execute then
    exit;

  if FileExists(SaveSettings.Filename) then
    DeleteFile(SaveSettings.Filename);

  with TMemIniFile.Create(SaveSettings.Filename) do
    try
      for iCountS := 0 to pcOptions.PageCount - 1 do
        with pcOptions.Pages[iCountS] do
          for iCount := 0 to ControlCount - 1 do
          begin
            if Controls[iCount].Name = '' then
              continue;
            if Controls[iCount] is TEdit then
              WriteString(Controls[iCount].ClassName, Controls[iCount].Name, TEdit(Controls[iCount]).Text)
            else if Controls[iCount] is TLabeledEdit then
              WriteString(Controls[iCount].ClassName, Controls[iCount].Name, TLabeledEdit(Controls[iCount]).Text)
            else if Controls[iCount] is TMemo then
            begin
              with TMemo(Controls[iCount]) do
              begin
                WriteInteger(ClassName, Name + ' 0', Lines.Count);
                for iCount2 := 0 to Lines.Count - 1 do
                  WriteString(ClassName, Name + ' ' + inttostr(iCount2 + 1), Lines[iCount2]);
              end;
            end
            else if Controls[iCount] is TTrackbar then
              WriteInteger(Controls[iCount].ClassName, Controls[iCount].Name, TTrackbar(Controls[iCount]).Position)
            else if Controls[iCount] is TRadioGroup then
              WriteInteger(Controls[iCount].ClassName, Controls[iCount].Name, TRadioGroup(Controls[iCount]).ItemIndex);
          end;
      UpdateFile;
    finally
      Free;
    end;
end;

procedure TBuilderForm.memFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Lowercase(Chr(Key)) = 'a') then
    memFiles.SelectAll;
end;

function RightSlash(const AInput: string): string;
begin
  Result := AInput;
  if length(Result) = 0 then
    exit;
  if Result[length(AInput)] <> '\' then
    Result := Result + '\';
end;

procedure RecurseAndAdd(const AChopOff, ADirName: string; const AFiles: TMemo);
var
  ssrSearchRec: TSearchRec;
  strDirName: string;
  iGotOne: integer;
  pFiles: TStringList;
begin
  strDirName := StringReplace(ADirName, AChopOff, '', [rfIgnoreCase]);
  strDirName := RightSlash(strDirName);
  iGotOne := FindFirst(ADirName + '*.*', faAnyFile, ssrSearchRec);
  pFiles := TStringList.Create;
  try
    while iGotOne = 0 do
    begin
      if ((ssrSearchRec.Attr and faDirectory) = 0) then
        pFiles.Add(strDirName + ssrSearchRec.Name)
      else if (ssrSearchRec.Name <> '.') and (ssrSearchRec.Name <> '..') then
        RecurseAndAdd(AChopOff, ADirName + ssrSearchRec.Name + '\', AFiles);
      iGotOne := FindNext(ssrSearchRec);
    end;
    for iGotOne := 0 to pFiles.Count - 1 do
      AFiles.Lines.Add(pFiles[iGotOne]);
  finally
    pFiles.Free;
  end;
  FindClose(ssrSearchRec);
end;

procedure TBuilderForm.btnRecurseClick(Sender: TObject);
var
  strResult: string;
  scurCursor: TCursor;
begin
  if not DoBrowseDialog(Handle, 'Recurse', strResult) then
    exit;
  edtRootDirectory.Text := strResult;
  strResult := RightSlash(strResult);
  memFiles.Clear;

  scurCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    RecurseAndAdd(strResult, strResult, memFiles);
  finally
    Screen.Cursor := scurCursor;
  end;
end;

procedure TBuilderForm.rgCompressionTypeClick(Sender: TObject);
begin
  tbCompressionLevel.Enabled := TRadioGroup(Sender).ItemIndex <> 0;
  lblCompression.Enabled := tbCompressionLevel.Enabled;
  if TRadioGroup(Sender).ItemIndex = 2 then
    tbCompressionLevel.Max := 4
  else if TRadioGroup(Sender).ItemIndex = 1 then
    tbCompressionLevel.Max := 9;
end;

procedure TBuilderForm.btnLoadKeyClick(Sender: TObject);
var
  ifKeyFile: TextFile;
  strBuffer: string;
begin
  OpenKeyDialog.FileName := '';
  if not OpenKeyDialog.Execute then
    exit;
    
  AssignFile(ifKeyFile, OpenKeyDialog.Filename);
  Reset(ifKeyFile);
  try
    ReadLn(ifKeyFile, strBuffer);
    edtP.Text := strBuffer;
    ReadLn(ifKeyFile, strBuffer);
    edtQ.Text := strBuffer;
    ReadLn(ifKeyFile, strBuffer);
    edtG.Text := strBuffer;
    ReadLn(ifKeyFile, strBuffer);
    edtY.Text := strBuffer;
    edtExecutableChange(nil);
  finally
    CloseFile(ifKeyFile);
  end;
end;

end.
