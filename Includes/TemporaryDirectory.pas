unit TemporaryDirectory;

interface

function GetTempDirectory(ARoot: string = ''): string;

implementation

uses
  Windows;

function GetWindowsTempDir: string;
begin
  setlength(Result, MAX_PATH);
  setlength(Result, GetTempPath(MAX_PATH, PAnsiChar(Result)));
end;

function GetTempDirectory(ARoot: string = ''): string;
var
  aTempFileName: array [0..MAX_PATH-1] of char;
begin
  if ARoot = '' then
    ARoot := GetWindowsTempDir;
  if GetTempFileName(PAnsiChar(ARoot), 'BZO', 0, aTempFileName) <> 0 then
  begin
    Result := aTempFileName;
    DeleteFile(PAnsiChar(Result));
  end
  else
    Result := '';
end;

end.
 