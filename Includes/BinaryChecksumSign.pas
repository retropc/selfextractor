unit BinaryChecksumSign;

interface

uses
  FGInt;

function FGIntLargeRandom(const ASize: word): TFGInt;

type
  TPrivateKey = record
    P, Q, G, X: TFGInt;
  end;

var
  pgPrivateKey: TPrivateKey;

implementation

uses
  Classes, SysUtils, FGIntDSA;

function ApproxLog2b(const AValue: TFGInt): double;
var
  iLoop: integer;
  eCurrent: TFGInt;
  eOne: TFGInt;
  ePower: TFGInt;
  eOutput: TFGInt;
  eTempStorage: TFGInt;
begin
  Result := -1;
  Base10StringToFGInt('0', eCurrent);
  Base10StringToFGInt('2', ePower);
  Base10StringToFGInt('1', eOne);
  try
    for iLoop := 0 to 2048 do
    begin
      FGIntExp(ePower, eCurrent, eOutput);
      if FGIntCompareAbs(eOutput, AValue) = lt then
      begin
        Result := iLoop - 1;
        exit;
      end;
      FGIntAdd(eCurrent, eOne, eTempStorage);
      FGIntCopy(eTempStorage, eCurrent);
    end;
  finally
    FGIntDestroy(eCurrent);
    FGIntDestroy(ePower);
    FGIntDestroy(eOne);
    FGIntDestroy(eOutput);
    FGIntDestroy(eTempStorage);
  end;
end;

function FGIntLargeRandom(const ASize: word): TFGInt;
var
  eCurrent: TFGInt;
begin
  Randomize;
  Base10StringToFGInt('1', result);
  repeat
    FGIntMulByInt(result, eCurrent, Random(High(integer)));
    FGIntCopy(eCurrent, Result);
  until ApproxLog2b(Result) > ASize;
  FGIntDestroy(eCurrent);
end;

end.
