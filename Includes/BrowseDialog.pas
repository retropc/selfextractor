unit BrowseDialog;

interface

function DoBrowseDialog(const AHandle: Integer; const ACaption: string; var AFolder: string): boolean;

implementation

uses
  Windows, ShellAPI, ShlObj, SysUtils;
  
{$WARNINGS OFF}
function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: LPARAM; lpData: LPARAM): integer; stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  BrowseCallbackProc := 0;
end;

function DoBrowseDialog(const AHandle: Integer; const ACaption: string; var AFolder: string): boolean;
const
  BIF_STATUSTEXT           = $0004;
  BIF_NEWDIALOGSTYLE       = $0040;
  BIF_RETURNONLYFSDIRS     = $0080;
  BIF_SHAREABLE            = $0100;
  BIF_USENEWUI             = BIF_EDITBOX or BIF_NEWDIALOGSTYLE;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  JtemIDList: PItemIDList;
  Path: PAnsiChar;
begin
  Result := False;

  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(AHandle, CSIDL_DRIVES, JtemIDList);
  with BrowseInfo do
  begin
    hwndOwner := GetActiveWindow;
    pidlRoot := JtemIDList;
    SHGetSpecialFolderLocation(hwndOwner, CSIDL_DRIVES, JtemIDList);

    pszDisplayName := StrAlloc(MAX_PATH);

    lpszTitle := PChar(ACaption);
    ulFlags := ulFlags or BIF_USENEWUI or BIF_RETURNONLYFSDIRS;
    lpfn := @BrowseCallbackProc;
    lParam := LongInt(PChar(AFolder));
  end;

  ItemIDList := SHBrowseForFolder(BrowseInfo);

  if Assigned(ItemIDList) and SHGetPathFromIDList(ItemIDList, Path) then
  begin
    AFolder := Path;
    Result := True;
  end;
end;
{$WARNINGS ON}

end.
