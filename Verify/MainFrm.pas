unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan;

const
//  HASH_FILENAME = '\\tyr\students\c\cp45617\publish\authorised.shk';
{$IF Not Declared(HASH_FILENAME)}
  ENC_HASH_FILENAME = #$5D#$58#$7D#$69#$6B#$78#$71#$65#$57#$62#$7E#$6D#$68#$7B#$79#$76#$5B#$49#$50#$2C#$27#$38#$3D#$3B#$52#$62#$6D#$42#$46#$6E#$66#$4D#$54#$7D#$76#$6F#$6E#$4D#$63#$6B#$57#$7C#$74#$27#$77#$69#$6B;
{$IFEND}
  P = '278886557862714850774838454008578887574861032537892369354596235187294784979097598707460832270947317643324702144116872186523832471898430812371271207073371319550847444845903816626699948990560683906800719923028' + '00347025578559151976617159963951332238520117786587600338248130181507959522267135837777622181638965689214813';
  Q = '181715612582909903536533399173568140748555676750233683';
  G = '1272661536851704884935081605573725785585832038689297542120550177149676174782926241759789848906257494233728286135295871273090223031700651767431622053842206247467446374502004607056532903443036632' + '2536471211144391031402334744667092870080831977007979507447610373387269506629479885994833351522940354409019152060393084056';
  Y = '244683202711504223973489651794769138836336343256840390640599372220422833044066249435254683975361846847630937670231120833288198958208532766935791267142338498784300193310577220711196585' + '02290815307223894500933524790745328543075675643073104331946828092090224921612540059634783439159320696165720939723408569870628354195';

type
  TMainForm = class(TForm)
    btnShortcut: TButton;
    btnVerifyRun: TButton;
    btnTyr: TButton;
    btnHTTP: TButton;
    cbxLocation: TComboBox;
    lblLocation: TLabel;
    XPM: TXPManifest;
    btnDelete: TButton;
    btnBrowse: TButton;
    OpenDialog: TOpenDialog;
    procedure btnHTTPClick(Sender: TObject);
    procedure btnTyrClick(Sender: TObject);
    procedure btnShortcutClick(Sender: TObject);
    procedure cbxLocationChange(Sender: TObject);
    procedure btnVerifyRunClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    function Verify(const AURL: string; const AKeepFile: boolean; out AFilename: string; out ADeletable: boolean): boolean;
  protected
    procedure WMDROPFILES(var Msg: TMessage); message WM_DROPFILES;
  public
    procedure SaveOutput(const AAddItem: boolean = true);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Shared, DSA, IniFiles, ShlObj, ShellAPI, ActiveX,
{$IF Declared(HASH_FILENAME)}
  ClipBrd,
{$IFEND}
  TemporaryDirectory, URLMon;

{$R *.dfm}

function CryptHashLocation(const AInput: string): string;
var
  iCount: integer;
begin
  Result := '';
  for iCount := 1 to length(AInput) do
    Result := Result + chr(ord(AInput[iCount]) XOR (iCount * iCount MOD 47));
end;

function DownloadFile(const AFrom: string; const ATo: string): boolean; overload;
begin
  Result := URLDownloadToFile(nil, PAnsiChar(AFrom), PAnsiChar(ATo), 0, nil) = 0;
end;

function GetRealTemp(): string;
begin
  Result := GetTempDirectory();
  if Result = '' then
    if DirectoryExists('T:\') then
      Result := GetTempDirectory('T:\')
end;

function RightSlash(const AInput: string): string;
begin
  Result := AInput;
  if (AInput <> '') and (AInput[length(AInput)] <> '\') then
    Result := Result + '\';
end;

function DownloadFile(const AFrom: string): string; overload;
var
  strFilename: string;
begin
  Result := '';
  strFilename := GetRealTemp + '.exe';
  if strFilename = '' then
    raise Exception.Create('Could not create temporary file!');
  if DownloadFile(AFrom, strFilename) then
    Result := strFilename
  else
    DeleteFile(strFilename);
end;

function GetSpecialFolderPath(const AFolder: Integer): string;
var
  pIDL: PItemIDList;
  strBuffer: array[0..MAX_PATH] of Char;
begin
  if SHGetSpecialFolderLocation(0, AFolder, PIDL) <> NOERROR then
    Result := ''
  else if not SHGetPathFromIDList(PIDL, strBuffer) then
    Result := ''
  else
    Result := strBuffer;
end;

procedure CreateLink(const AObjectPath, ALinkPath, ADescription, AParamaters, AIcon: string);
var
  piShellLink: IShellLink;
  piPersistFile: IPersistFile;
  rgcWidePath: array[0..259] of WideChar;
begin
  CoInitialize(nil);
  if Failed(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, piShellLink)) then
    raise Exception.Create('Unable to create an IShellLink instance');
  try
    with piShellLink do
    begin
      SetPath(PChar(AObjectPath));
      SetArguments(PChar(AParamaters));
      SetDescription(PChar(ADescription));
      if AIcon <> '' then
        SetIconLocation(PChar(AIcon), 0);
    end;
    
    if Failed(piShellLink.QueryInterface(IPersistFile, piPersistFile)) then
      raise Exception.Create('Unable to create an IPersistFile instance');
    MultiByteToWideChar(CP_ACP, 0, PChar(ALinkPath), -1, rgcWidePath, 259);

    if Failed(piPersistFile.Save(rgcWidePath, true)) then
      raise Exception.Create('Unable to save link');
  finally
    CoUninitialize;
  end;
end;

procedure TMainForm.btnHTTPClick(Sender: TObject);
begin
  with cbxLocation do
  begin
    Text := 'http://';
    SetFocus;
    SelStart := length(Text);
    OnChange(nil);
  end;
end;

procedure TMainForm.btnTyrClick(Sender: TObject);
begin
  with cbxLocation do
  begin
    Text := '\\tyr\students\';
    SetFocus;
    SelStart := length(Text);
  end;
end;

function IsHTTP(const AInput: string): boolean;
begin
  Result := Lowercase(Copy(AInput, 1, 7)) = 'http://';
end;

function ObtainFile(const AInput: string; out ADeletable: boolean): string;
begin
  if IsHTTP(AInput) then
  begin
    ADeletable := true;
    Result := DownloadFile(AInput);
  end
  else
  begin
    Result := AInput;
    ADeletable := false;
  end;
end;

procedure ErrorMessageProc(const AMessage: string; const AIcon: integer = MB_ICONWARNING);
begin
  if AIcon = MB_ICONERROR then
    MessageDlg(AMessage, mtError, [mbOK], 0)
  else
    MessageDlg(AMessage, mtWarning, [mbOK], 0);
end;

function TMainForm.Verify(const AURL: string; const AKeepFile: boolean; out AFilename: string; out ADeletable: boolean): boolean;
var
  strLocation: string;
begin
  Result := false;
  AFilename := ObtainFile(AURL, ADeletable);
  if AFilename = '' then
    raise Exception.Create('Could not download file!');
  try
    try
{$IF Declared(HASH_FILENAME)}
      strLocation := HASH_FILENAME;
{$ELSE}
      strLocation := CryptHashLocation(ENC_HASH_FILENAME);
{$IFEND}
      VerifyHashFile(strLocation, P, Q, G, Y, AFilename);
      Result := true;
    except
      on E:EInOutError do MessageDlg('Unable to open hash file!', mtError, [mbOK], 0);
      on E:Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  finally
    if not AKeepFile and ADeletable then
      DeleteFile(AFilename);
  end;
end;

function ExtractHTTPFileName(const AInput: string): string;
var
  iLastPos: integer;
  iCount: integer;
begin
  if IsHTTP(AInput) then
  begin
    iLastPos := 0;
    for iCount := 1 to length(AInput) do
    begin
      if AInput[iCount] = '?' then
      begin
        Result := Copy(AInput, iLastPos + 1, iCount - iLastPos - 1);
        exit;
      end;
      if AInput[iCount] = '/' then
        iLastPos := iCount;
    end;
    Result := Copy(AInput, iLastPos + 1, MaxInt);
  end
  else
    Result := ExtractFilename(AInput);
end;

procedure TMainForm.btnShortcutClick(Sender: TObject);
var
  strIcon: string;
begin
  if MessageDlg('A shortcut to verify and run the executable will be placed on your desktop, continue?', mtConfirmation, [mbYes, mbNo], 0) = ID_NO then
    exit;
  if not IsHTTP(cbxLocation.Text) then
    strIcon := cbxLocation.Text
  else
    strIcon := '';
  CreateLink(ParamStr(0), GetSpecialFolderPath(CSIDL_DESKTOP) + '\' + ChangeFileExt(ExtractHTTPFilename(cbxLocation.Text), '.lnk'), ExtractHTTPFileName(cbxLocation.Text), '"' + cbxLocation.Text + '"', strIcon);
end;

procedure TMainForm.cbxLocationChange(Sender: TObject);
begin
  btnShortcut.Enabled := cbxLocation.Text <> '';
  btnVerifyRun.Enabled := btnShortcut.Enabled;
  btnDelete.Enabled := cbxLocation.ItemIndex <> -1;
end;

procedure TMainForm.SaveOutput(const AAddItem: boolean = true);
var
  iCount: integer;
begin
  if AAddItem then
  begin
    for iCount := 0 to cbxLocation.Items.Count - 1 do
      if AnsiCompareText(cbxLocation.Items[iCount], cbxLocation.Text) = 0 then
        exit;
    cbxLocation.Items.Add(cbxLocation.Text);
    cbxLocation.ItemIndex := cbxLocation.Items.Count - 1;
    cbxLocation.OnChange(nil);
  end;
  cbxLocation.Items.SaveToFile(GetSpecialFolderPath(CSIDL_PERSONAL) + '\vlist.txt');
end;

procedure TMainForm.btnVerifyRunClick(Sender: TObject);
var
  fDeletable: boolean;
  strOutput: string;
begin
  fDeletable := false;
  try
    if not Verify(cbxLocation.Text, true, strOutput, fDeletable) then
      exit;
    ExecuteFollower(ExtractFilename(strOutput), '', ExtractFileDir(strOutput));
    SaveOutput;
  finally
    if fDeletable then
      DeleteFile(strOutput);
  end;
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
var
  strText: string;
begin
  if cbxLocation.ItemIndex = -1 then
    beep
  else
  begin
    strText := cbxLocation.Text;
    cbxLocation.DeleteSelected;
    cbxLocation.Text := strText;
    cbxLocation.OnChange(nil);
    SaveOutput(false);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  strFilename: string;
begin
  AutoSize := false;
  if ParamCount = 1 then
  begin
    cbxLocation.Text := ParamStr(1);
    try
      btnVerifyRun.Click;
      exit;
    finally
      Application.Terminate;
    end;
  end;
  strFilename := GetSpecialFolderPath(CSIDL_PERSONAL) + '\vlist.txt';
  if FileExists(strFilename) then
    cbxLocation.Items.LoadFromFile(strFilename);
{$IF Declared(HASH_FILENAME)}
  Clipboard.SetTextBuf(PAnsiChar('  ENC_HASH_FILENAME = ' + StrToHex(CryptHashLocation(HASH_FILENAME), '#$') + ';'));
{$IFEND}
  DragAcceptFiles(Handle, true);
end;

procedure TMainForm.btnBrowseClick(Sender: TObject);
begin
  OpenDialog.Filename := '';
  if OpenDialog.Execute then
  begin
    cbxLocation.Text := OpenDialog.Filename;
    cbxLocation.OnChange(nil);
  end;
end;

procedure TMainForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  NewHeight := Height;
end;

{$WARNINGS OFF}
procedure TMainForm.WMDROPFILES(var Msg: TMessage);
var
  iCount: integer;
  iFiles: integer;
  iSize: integer;
  pzFilename: PChar;
begin
  inherited;
  iFiles := DragQueryFile(Msg.WParam, $FFFFFFFF, pzFilename, 255);
  try
    for iCount := 0 to iFiles - 1 do
    begin
      iSize := DragQueryFile(Msg.WParam, iCount, nil, 0) + 1;
      pzFilename := StrAlloc(iSize);
      DragQueryFile(Msg.WParam, iCount, pzFilename, iSize);
      cbxLocation.Text := StrPas(pzFilename);
      cbxLocation.OnChange(nil);
      StrDispose(pzFilename);
    end;
  finally
    DragFinish(Msg.WParam);
  end;
end;
{$WARNINGS ON}

end.
