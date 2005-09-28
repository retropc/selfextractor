unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, Contnrs, PrivateCrypto, XPMan;

const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;
  
type
  TMainForm = class(TForm)
    lvHashes: TListView;
    pmItems: TPopupMenu;
    mnuAdd: TMenuItem;
    mnuDelete: TMenuItem;
    OpenDialog: TOpenDialog;
    mnuSave: TMenuItem;
    XPM: TXPManifest;
    OpenHash: TOpenDialog;
    OpenPrivateKey: TOpenDialog;
    procedure pmItemsPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuAddClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lvHashesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure mnuSaveClick(Sender: TObject);
  private
    FHashesList: TObjectList;
    FDirty: boolean;
    FLoaded: boolean;
    procedure CommenceSave;
  protected
    procedure WMDROPFILES(var Msg: TMessage); message WM_DROPFILES;
  public
    procedure LoadFromFile(const AFilename: string; out AHashesList: TObjectList);
    procedure SaveToFile(const AFilename: string; const AHashesList: TObjectList; const APrivateKey: TPrivateKey);
    procedure AddToListView(const AHash: THashedFile);
    procedure SaveChanges(out ACanClose: boolean);
    function GetPrivateKey(const ALocation: string): TPrivateKey;
  published
    property Hashes: TObjectList read FHashesList;
    property Dirty: boolean read FDirty;
    property FormLoaded: boolean read FLoaded;
  end;

var
  MainForm: TMainForm;

implementation

uses
  EnterPPFrm, DCPSHA256, DCPcrypt2, Shared, ShellAPI;

{$R *.dfm}

{$WARNINGS OFF}
procedure TMainForm.WMDROPFILES(var Msg: TMessage);
var
  iCount: integer;
  iFiles: integer;
  iSize: integer;
  pzFilename: PChar;
  strFilename: string;
  pHash: THashedFile;
begin
  inherited;
  iFiles := DragQueryFile(Msg.WParam, $FFFFFFFF, pzFilename, 255);
  try
    for iCount := 0 to iFiles - 1 do
    begin
      iSize := DragQueryFile(Msg.WParam, iCount, nil, 0) + 1;
      pzFilename := StrAlloc(iSize);
      DragQueryFile(Msg.WParam, iCount, pzFilename, iSize);
      strFilename := StrPas(pzFilename);
      if FileExists(strFilename) and (Lowercase(ExtractFileExt(strFilename)) = '.exe') then
      begin
        pHash := THashedFile.Create(strFilename);
        Hashes.Add(pHash);
        AddToListView(pHash);
      end;
      StrDispose(pzFilename);
    end;
  finally
    DragFinish(Msg.WParam);
  end;
end;
{$WARNINGS ON}

procedure TMainForm.pmItemsPopup(Sender: TObject);
begin
  mnuDelete.Enabled := Assigned(lvHashes.Selected);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  try
    if (not OpenHash.Execute) or (OpenHash.Filename = '') or (not OpenPrivateKey.Execute) or (OpenPrivateKey.FileName = '') then
    begin
      Application.Terminate;
      exit;
    end;
    LoadFromFile(OpenHash.Filename, FHashesList);
  except
    on EInOutError do
      MessageDlg('Unable to open hash list!', mtWarning, [mbOK], 0)
    else
      raise;
  end;
  FLoaded := true;
  DragAcceptFiles(Handle, true);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Hashes) then
    FreeAndNil(FHashesList);
end;

procedure TMainForm.LoadFromFile(const AFilename: string; out AHashesList: TObjectList);
var
  ifFile: TextFile;
  strHash: string;
  fAuthorised: boolean;
  strAuthorised: string;
  pHash: THashedFile;
begin
  AHashesList := TObjectList.Create;
  AHashesList.OwnsObjects := true;
  if not FileExists(AFilename) then
    exit;
  AssignFile(ifFile, AFilename);
  Reset(ifFile);
  try
    while not EOF(ifFile) do
    begin
      ReadLn(ifFile, strHash);
      if strHash = '' then
        break;
      ReadLn(ifFile, strAuthorised);

      if TryStrToBool(strAuthorised, fAuthorised) then
      begin
        pHash := THashedFile.Create(strHash, fAuthorised);
        AHashesList.Add(pHash);
        AddToListView(pHash);
      end;
    end;
  finally
    CloseFile(ifFile);
  end;
end;

procedure TMainForm.SaveToFile(const AFilename: string; const AHashesList: TObjectList; const APrivateKey: TPrivateKey);
var
  iCount: integer;
  pHash: THashedFile;
  ofFile: TextFile;
  strData: string;
  strA: string;
  strS: string;
  shasDigest: array[0..7] of DWord;
begin
  AssignFile(ofFile, AFilename);
  Rewrite(ofFile);
  try
    with TDCP_SHA256.Create(nil) do
      try
        Init;
        for iCount := 0 to AHashesList.Count - 1 do
        begin
          pHash := AHashesList[iCount] as THashedFile;
          WriteLn(ofFile, pHash.FileHash);
          WriteLn(ofFile, BoolToStr(pHash.Authorised, true));
          UpdateStr(pHash.FileHash + CRLF + BoolToStr(pHash.Authorised, true) + CRLF);
        end;
        Final(shasDigest);
        setlength(strData, sizeof(shasDigest));
        CopyMemory(@strData[1], @shasDigest, length(strData));
        APrivateKey.Sign(strData, strA, strS);
        WriteLn(ofFile);
        WriteLn(ofFile, StrToHex(strA));
        WriteLn(ofFile, StrToHex(strS));
      finally
        Free;
      end;
    for iCount := 0 to AHashesList.Count - 1 do
      THashedFile(AHashesList[iCount]).NewItem := false;
  finally
    CloseFile(ofFile);
  end;
end;

procedure TMainForm.mnuAddClick(Sender: TObject);
var
  pHash: THashedFile;
begin
  OpenDialog.FileName := '';
  if not OpenDialog.Execute then
    exit;
  pHash := THashedFile.Create(OpenDialog.Filename);
  Hashes.Add(pHash);
  AddToListView(pHash);
end;

procedure TMainForm.mnuDeleteClick(Sender: TObject);
var
  iIndex: integer;
begin
  if not Assigned(lvHashes.Selected) or not Assigned(lvHashes.Selected.Data) then
  begin
    Beep;
    exit;
  end;
  iIndex := Hashes.IndexOf(lvHashes.Selected.Data);
  if iIndex = -1 then
  begin
    Beep;
    exit;
  end;
  if not THashedFile(Hashes[iIndex]).NewItem then
    FDirty := true;

  Hashes.Delete(iIndex);
  lvHashes.DeleteSelected;
end;

procedure TMainForm.AddToListView(const AHash: THashedFile);
begin
  with lvHashes.Items.Add do
  begin
    Data := AHash;
    Caption := AHash.FileHash;
    Checked := AHash.Authorised;
  end;
end;

procedure TMainForm.CommenceSave;
var
  pKey: TPrivateKey;
begin
  pKey := GetPrivateKey(OpenPrivateKey.Filename);
  try
    SaveToFile(OpenHash.Filename, Hashes, pKey);
    FDirty := false;
  finally
    pKey.Free;
  end;
end;

procedure TMainForm.SaveChanges(out ACanClose: boolean);
var
  mrsResult: integer;
begin
  mrsResult := MessageDlg('Save changes?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case mrsResult of
    ID_YES: CommenceSave;
    ID_CANCEL: ACanClose := false;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  iCount: integer;
begin
  if Dirty then
    SaveChanges(CanClose)
  else
    for iCount := 0 to Hashes.Count - 1 do
      if THashedFile(Hashes[iCount]).NewItem then
      begin
        SaveChanges(CanClose);
        break;
      end;
end;

function TMainForm.GetPrivateKey(const ALocation: string): TPrivateKey;
begin
  Result := TPrivateKey.Create(OpenPrivateKey.Filename, TEnterPPForm.Create(Self).ShowModal);
end;

procedure TMainForm.lvHashesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if FormLoaded and (Change = ctState) and Assigned(Item) and Assigned(Item.Data) then
  begin
    THashedFile(Item.Data).Authorised := Item.Checked;
    FDirty := true;
  end;
end;

procedure TMainForm.mnuSaveClick(Sender: TObject);
begin
  CommenceSave;
end;

end.
