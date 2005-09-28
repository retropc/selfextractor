program AuthoriseHashes;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  EnterPPFrm in 'EnterPPFrm.pas' {EnterPPForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Authorise hashes';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEnterPPForm, EnterPPForm);
  Application.Run;
end.
