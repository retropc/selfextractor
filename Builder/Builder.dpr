program Builder;

{$R 'Stub.res' 'Stub.rc'}

{$R Builder.RES}

uses
  Forms,
  BuilderFrm in 'BuilderFrm.pas' {BuilderForm};


begin
  Application.Initialize;
  Application.Title := 'Create self extractor';
  Application.CreateForm(TBuilderForm, BuilderForm);
  Application.Run;
end.
