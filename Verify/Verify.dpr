program Verify;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  Shared,
  DSA;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Verify signature';
  Application.ShowMainForm := ParamCount = 0;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
