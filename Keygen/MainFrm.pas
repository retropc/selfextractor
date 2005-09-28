unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, shellapi,
  Dialogs, StdCtrls, FGIntPrimeGeneration, Math, fgint, fgintdsa, XPMan,
  ExtCtrls;

type
  TMainForm = class(TForm)
    btnGenSave: TButton;
    SavePubDialog: TSaveDialog;
    SavePrivDialog: TSaveDialog;
    XPM: TXPManifest;
    edtPP: TLabeledEdit;
    procedure btnGenSaveClick(Sender: TObject);
  private
    procedure GenerateKeys(out AP, AQ, AG, AX, AY: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  DCPrijndael, DCPSHA256, BinaryChecksumSign;

{$R *.dfm}

procedure TMainForm.GenerateKeys(out AP, AQ, AG, AX, AY: string);
var
  P: TFGInt;
  Q: TFGInt;
  G: TFGInt;
  X: TFGInt;
  Y: TFGInt;
  Temp1: TFGInt;
  Temp2: TFGInt;
  One: TFGInt;
begin
  Q := FGIntLargeRandom(160);
  PrimeSearch(Q);
  P := FGIntLargeRandom(1024);
  DSAPrimeSearch(Q, P);

  FGIntToBase10String(P, AP);
  FGIntToBase10String(Q, AQ);

  FGIntCopy(P, Temp1);
  Temp1.Number[1] := Temp1.Number[1] - 1;
  FGIntDiv(Temp1, Q, Temp2);
  FGIntDestroy(Temp1);
  FGIntDestroy(Q);
  G := FGIntLargeRandom(1024);
  X := FGIntLargeRandom(1024);
  Base10StringToFGInt('1', One);

  repeat
      FGIntRandom1(G, Temp1);
      FGIntCopy(Temp1, G);
      FGIntModExp(G, Temp2, P, Temp1);
      FGIntCopy(Temp1, G);
  until FGIntCompareAbs(One, G) <> Eq;

  FGIntDestroy(Temp1);
  FGIntDestroy(One);
  FGIntDestroy(Temp2);

// x is your secret key, random
// k is a random number, the same k must
// not be used twice for a signature
  X := FGIntLargeRandom(160);
  FGIntModExp(G, X, P, Y);
  FGIntDestroy(P);

  FGIntToBase10String(G, AG);
  FGIntDestroy(G);
  FGIntToBase10String(X, AX);
  FGIntDestroy(X);
  FGIntToBase10String(Y, AY);
  FGIntDestroy(Y);

end;


procedure TMainForm.btnGenSaveClick(Sender: TObject);
var
  ifPub: TextFile;
  ofKey: TFileStream;
  ofMemory: TMemoryStream;
  ofCrypto: TDCP_rijndael;
  iMagic: word;
  strP: string;
  strQ: string;
  strG: string;
  strX: string;
  strY: string;
begin
  if not SavePrivDialog.Execute or not SavePubDialog.Execute then
    exit;

  GenerateKeys(strP, strQ, strG, strX, strY);

  if FileExists(SavePrivDialog.Filename) then
    DeleteFile(SavePrivDialog.Filename);
  if FileExists(SavePubDialog.Filename) then
    DeleteFile(SavePubDialog.Filename);

  AssignFile(ifPub, SavePubDialog.Filename);
  Rewrite(ifPub);
  try
    WriteLn(ifPub, strP);
    WriteLn(ifPub, strQ);
    WriteLn(ifPub, strG);
    WriteLn(ifPub, strY);
  finally
    CloseFile(ifPub);
  end;
  
  ofMemory := TMemoryStream.Create;
  try
    iMagic := ord('A') shl 4 + ord('H');
    ofMemory.Write(iMagic, sizeof(word));
    iMagic := length(strP);
    ofMemory.Write(iMagic, sizeof(word));
    ofMemory.Write(strP[1], iMagic);
    iMagic := length(strQ);
    ofMemory.Write(iMagic, sizeof(word));
    ofMemory.Write(strQ[1], iMagic);
    iMagic := length(strG);
    ofMemory.Write(iMagic, sizeof(word));
    ofMemory.Write(strG[1], iMagic);
    iMagic := length(strX);
    ofMemory.Write(iMagic, sizeof(word));
    ofMemory.Write(strX[1], iMagic);
    ofMemory.Position := 0;
    ofKey := TFileStream.Create(SavePrivDialog.Filename, fmCreate or fmShareExclusive);
    try
      ofCrypto := TDCP_rijndael.Create(nil);
      try
        ofCrypto.InitStr(edtPP.text, TDCP_SHA256);
        ofCrypto.EncryptStream(ofMemory, ofKey, ofMemory.Size);
      finally
        ofCrypto.Free;
      end;
    finally
      ofKey.Free;
    end;
  finally
    ofMemory.Free;
  end;
  MessageDlg('Done!', mtInformation, [mbOK], 0);
end;

end.
