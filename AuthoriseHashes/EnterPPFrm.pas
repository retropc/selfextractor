unit EnterPPFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TEnterPPForm = class(TForm)
    edtPP: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    function ShowModal: string; reintroduce;
  end;

var
  EnterPPForm: TEnterPPForm;

implementation

{$R *.dfm}

function TEnterPPForm.ShowModal: string;
begin
  inherited ShowModal;
  Result := edtPP.Text;
end;

procedure TEnterPPForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TEnterPPForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
