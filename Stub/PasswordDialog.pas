unit PasswordDialog;

interface

uses
  Windows;

type
  TExtractReturn = (ecrOK, ecrError, ecrExecutableOpenError, ecrInvalidFormat,
                    ecrInvalidPassword, ecrBufferAllocationError,
                    ecrMinorBufferAllocationError, ecrInvalidFileCount,
                    ecrFileBufferAllocationError, ecrCleanupAllocationError,
                    ecrOutputStreamCreationError, ecrExecuteError,
                    ecrTempDirectory, ecrDecompressionStreamError,
                    ecrMemoryStorageAllocationError, ecrInvalidVersion,
                    ecrCryptoUnknownType, ecrCryptoAllocError,
                    ecrCryptoKeyError, ecrInsufficientDiskSpace,
                    ecrDiskSpaceError, ecrPathError, ecrInvalidChecksum,
                    ecrUnknownCompressionType, ecrDealt);

  PExtractFunction = ^TExtractFunction;
  TExtractFunction = function(const APassword: string): TExtractReturn;

procedure CreatePasswordDialog(const AExtractFiles: PExtractFunction; const ACaption: string = 'Self extractor');
function CreateProgressDialog(const AMax: integer; const APosition: integer = 0): HWND;
function HandleError(const AError: TExtractReturn): boolean;
procedure HideForm;
procedure Terminate;
procedure EnableButton(const AEnable: boolean = true; const ALatter: boolean = true);
procedure ErrorMessage(const AMessage: string; const AIcon: integer = MB_ICONWARNING);

implementation

uses
  Messages, XPMan, SysUtils, CommCtrl;

var
  eWinClass: TWndClassA;
  prgProgress: HWND;
  edtPassword: HWND;
  btnOK, btnCancel: HWND;
  hHandle: HWND;
  pExtract: PExtractFunction;
  bExitLoop: boolean;
  hLastFocused: HWND;

procedure InitCommonControls; external comctl32 name 'InitCommonControls';

procedure ErrorMessage(const AMessage: string; const AIcon: integer = MB_ICONWARNING);
begin
  MessageBox(hHandle, PAnsiChar(AMessage), 'Error', AIcon);
end;

function HandleError(const AError: TExtractReturn): boolean;
begin
  Result := false;
  case AError of
    ecrOK: Result := true;
    ecrExecutableOpenError: ErrorMessage('Cannot open executable!', MB_ICONERROR);
    ecrInvalidFormat: ErrorMessage('Invalid executable format!', MB_ICONERROR);
    ecrBufferAllocationError: ErrorMessage('Buffer allocation error!', MB_ICONERROR);
    ecrInvalidFileCount: ErrorMessage('Invalid file count!', MB_ICONERROR);
    ecrCleanupAllocationError: ErrorMessage('Could not allocate cleanup objects!', MB_ICONERROR);
    ecrInvalidChecksum: ErrorMessage('Invalid checksum!', MB_ICONERROR);

    ecrInvalidPassword:
    begin
      ErrorMessage('Incorrect password!');
      SetWindowText(edtPassword, nil);
      SetFocus(edtPassword);
    end;
    ecrMinorBufferAllocationError: ErrorMessage('Buffer allocation error!');
    ecrFileBufferAllocationError: ErrorMessage('File buffer allocation error');
    ecrOutputStreamCreationError: ErrorMessage('Output stream creation error!');
    ecrExecuteError: ErrorMessage('Execution error!');
    ecrTempDirectory: ErrorMessage('Unable to create temporary directory!');
    ecrDecompressionStreamError: ErrorMessage('Unable to create decompression stream!');
    ecrMemoryStorageAllocationError: ErrorMessage('Memory storage allocation error!');
    ecrInvalidVersion: ErrorMessage('Invalid data version!');
    ecrInsufficientDiskSpace: ErrorMessage('Insufficient disk space!');
    ecrDiskSpaceError: ErrorMessage('Unable to read free disk space!');
    ecrPathError: ErrorMessage('Self extractor is not permitted to run from this location!');

    ecrCryptoUnknownType: ErrorMessage('Unknown crypto type!', MB_ICONERROR);

    ecrCryptoAllocError: ErrorMessage('Crypto allocation error!');
    ecrCryptoKeyError: ErrorMessage('Crypto key initialisation error!');

    ecrUnknownCompressionType: ErrorMessage('Unknown compression type!', MB_ICONERROR);
    ecrDealt: exit;
    
    else
      ErrorMessage('Unknown error!');
  end;
end;

procedure EnableButton(const AEnable: boolean = true; const ALatter: boolean = true);
begin
  SendMessage(edtPassword, EM_SETREADONLY, integer(not AEnable), 0);
  SendMessage(edtPassword, WM_PAINT, 0, 0);
  if ALatter then
    EnableWindow(btnOK, AEnable);
end;

procedure Terminate;
begin
  PostMessage(hHandle, WM_QUIT, 0, 0);
end;

procedure HideForm;
begin
  ShowWindow(hHandle, SW_HIDE);
end;

procedure GetUserInput;
var
  iTextLength: integer;
  sText: string;
begin
  iTextLength := GetWindowTextLength(edtPassword);
  if iTextLength > 0 then
  begin
    Setlength(sText, iTextLength);
    if length(sText) <> iTextLength then
      exit;
    try
      GetWindowText(edtPassword, @sText[1], iTextLength + 1);
      EnableButton(false);
      HandleError(TExtractFunction(pExtract)(sText));
      EnableButton(true, GetWindowTextLength(edtPassword) <> 0);
    finally
      setlength(sText, 0);
    end;
  end
  else
    MessageBeep(0);
end;

function RegisterFont(const AFontName: string): THandle;
begin
  Result := CreateFont(-11, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH or FF_DONTCARE, PAnsiChar(AFontName));
end;

function WindowProc(AHWnd, AMsg, AWParam, ALParam: integer): integer; stdcall;
var
  hGotFocus: HWND;
begin
  Result := DefWindowProc(AHWnd, AMsg, AWParam, ALParam);

  case AMsg of
    WM_ACTIVATE:
      case LOWORD(AWParam) of
        WA_ACTIVE, WA_CLICKACTIVE:
          if GetFocus = HWND(hHandle) then
            SetFocus(hLastFocused);
        WA_INACTIVE:
        begin
          hGotFocus := GetFocus;
          if (hGotFocus <> 0) and
             (hGotFocus <> hHandle) then
            hLastFocused := GetFocus;
          end;
        end;
    WM_COMMAND:
      if ALParam = integer(btnOK) then
      begin
        GetUserInput;
        SetFocus(edtPassword);
      end
      else if ALParam = integer(btnCancel) then
        PostMessage(hHandle, WM_QUIT, 0, 0)
      else if ALParam = integer(edtPassword) then
      begin
        case HIWORD(AWParam) of
          EN_CHANGE: EnableWindow(btnOK, GetWindowTextLength(ALParam) <> 0);
        end;
      end;
    WM_DESTROY: bExitLoop := true;
  end;
end;

procedure CreatePasswordDialog(const AExtractFiles: PExtractFunction;
  const ACaption: string);
var
  hFont: THandle;
  hPasswordFont: THandle;
  cPasswordChar: char;
  eRect: TRect;
  eStartRect: TRect;
  lblPassword: THandle;
  eMsg: TMsg;
begin
  bExitLoop := false;
  pExtract := AExtractFiles;
  cPasswordChar := '*';

  eWinClass.hInstance := hInstance;
  with eWinClass do
  begin
    Style := CS_CLASSDC or CS_PARENTDC;
    lpfnWndProc := @WindowProc;
    hbrBackground := COLOR_BTNFACE + 1;
    lpszClassname := 'BZ_SELFEXTRACTOR';
    hCursor := LoadCursor(0, IDC_ARROW);
    hIcon :=  LoadIcon(hInstance, 'MAINICON');
  end;
  RegisterClass(eWinClass);

  eStartRect.Right := 325;
  eStartRect.Bottom := 43 + GetSystemMetrics(SM_CYCAPTION);
  with eRect do
  begin
    if SystemParametersInfo(SPI_GETWORKAREA, 0, @eRect, 0) then
    begin
      Top := Bottom - Top;
      Left := Right - Left;
      Right := eStartRect.Right;
      Bottom := eStartRect.Bottom;
      Top := (Top - Bottom) div 2;
      Left := (Left - Right) div 2;
    end
    else
    begin
      Right := eStartRect.Right;
      Bottom := eStartRect.Bottom;
      Top := 0;
      Left := 0;
    end;

    hHandle := CreateWindowEx(WS_EX_WINDOWEDGE, 'BZ_SELFEXTRACTOR', PAnsiChar(ACaption), WS_VISIBLE or WS_CAPTION or WS_SYSMENU, Left, Top, Right, Bottom, 0, 0, hInstance, nil);
  end;

  btnOK := CreateWindow('BUTTON', '&OK', WS_VISIBLE or WS_CHILD or BS_TEXT or BS_DEFPUSHBUTTON or WS_TABSTOP, 236, 6, 75, 24, hHandle, IDOK, hInstance, nil);
  btnCancel := CreateWindow('BUTTON', '&Cancel', WS_CHILD, 0, 0, 0, 0, hHandle, IDCANCEL, hInstance, nil);
  lblPassword := CreateWindow('STATIC', '', WS_VISIBLE or WS_CHILD or SS_LEFT, 8, 11, 80, 13, hHandle, 0, hInstance, nil);
  edtPassword := CreateWindowEx(WS_EX_CLIENTEDGE, 'EDIT', '', WS_CHILD or WS_VISIBLE or ES_PASSWORD or WS_TABSTOP or ES_AUTOHSCROLL, 95, 8, 131, 21, hHandle, 0, hInstance, nil);

  hFont := RegisterFont('Tahoma');
  if hFont = 0 then
    hFont := RegisterFont('MS Sans Serif');

  hPasswordFont := RegisterFont('Wingdings');
  if hPasswordFont = 0 then
    hPasswordFont := hFont
  else
    cPasswordChar := 'l';

  if hFont <> 0 then
  begin
    SendMessage(btnOK, WM_SETFONT, hFont, 0);
    SendMessage(lblPassword, WM_SETFONT, hFont, 0);
    SendMessage(edtPassword, WM_SETFONT, hPasswordFont, 0);
    SendMessage(edtPassword, EM_SETPASSWORDCHAR, ord(cPasswordChar), 0);
  end;

  SetWindowText(lblPassword, 'Enter &password:');
  SetFocus(edtPassword);
  hLastFocused := edtPassword;
  SendMessage(hHandle, DM_SETDEFID, IDOK, 0);
  EnableWindow(btnOK, false);

  UpdateWindow(hHandle);

  while(not bExitLoop and GetMessage(eMsg, hHandle, 0, 0)) do
    if not IsDialogMessage(hHandle, eMsg) then
    begin
      TranslateMessage(eMsg);
      DispatchMessage(eMsg);
    end;

end;

function CreateProgressDialog(const AMax: integer; const APosition: integer = 0): HWND;
var
  eRect: TRect;
  eStartRect: TRect;
//  eMsg: TMsg;
begin

  eStartRect.Right := 325;
  eStartRect.Bottom := 15;
  with eRect do
  begin
    if SystemParametersInfo(SPI_GETWORKAREA, 0, @eRect, 0) then
    begin
      Top := Bottom - Top;
      Left := Right - Left;
      Right := eStartRect.Right;
      Bottom := eStartRect.Bottom;
      Top := (Top - Bottom) div 2;
      Left := (Left - Right) div 2;
    end
    else
    begin
      Right := eStartRect.Right;
      Bottom := eStartRect.Bottom;
      Top := 0;
      Left := 0;
    end;

    prgProgress := CreateWindowEx(0, 'msctls_progress32', 'Extracting...', WS_VISIBLE or WS_POPUP, Left, Top, Right, Bottom, 0, 0, hInstance, nil);
  end;

  UpdateWindow(prgProgress);
  SendMessage(prgProgress, PBM_SETRANGE, 0, MakeLParam(0, AMax));
  SendMessage(prgProgress, PBM_SETSTEP, APosition, 0);
  SendMessage(prgProgress, PBM_STEPIT, 0, 0);  
  SendMessage(prgProgress, PBM_SETSTEP, 1, 0);
  SendMessage(prgProgress, WM_PAINT, 0, 0);
  Result := prgProgress;

{  while(not bExitLoop and GetMessage(eMsg, prgProgress, 0, 0)) do
    if not IsDialogMessage(hHandle, eMsg) then
    begin
      TranslateMessage(eMsg);
      DispatchMessage(eMsg);
    end;
 }
end;

initialization
  InitCommonControls;

end.
