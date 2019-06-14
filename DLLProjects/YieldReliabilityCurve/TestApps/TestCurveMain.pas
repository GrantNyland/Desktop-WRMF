unit TestCurveMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
const OPNAME = 'TForm1.FormCreate';
begin
  StringGrid1.Cells[1, 1] := '1';
  StringGrid1.Cells[2, 1] := '1';
  StringGrid1.Cells[1, 2] := '2';
  StringGrid1.Cells[2, 2] := '1.5';
  StringGrid1.Cells[1, 3] := '3';
  StringGrid1.Cells[2, 3] := '3';
  StringGrid1.Cells[1, 4] := '4';
  StringGrid1.Cells[2, 4] := '5';
end;

function Trinomial(AX: double; ACurveCoefficients: array of double): double;
const OPNAME = 'TestCurveMain.Trinomial';
begin
  Result :=
    ACurveCoefficients[0] +
    ACurveCoefficients[1] * AX +
    ACurveCoefficients[2] * AX * AX +
    ACurveCoefficients[3] * AX * AX * AX;
end;

function CalculateCurveCoefficients(
  ACurveType: integer;
  ACoefficientCount: integer;
  ACurveCoefficients: PDouble;
  AYXPairCount: integer;
  AYXPairs: PDouble
): boolean; stdcall; external 'CurveCalculator.dll';
const OPNAME = 'TestCurveMain.CalculateCurveCoefficients';
procedure TForm1.Button1Click(Sender: TObject);
const OPNAME = 'TForm1.Button1Click';
var
  LIndex: integer;
  LCurveCoefficients: array[0..3] of double;
  LPoints: array[0..7] of double;
begin
  for LIndex := 0 to 3 do
  begin
    LPoints[LIndex * 2] := StrToFloat(StringGrid1.Cells[2, LIndex + 1]);
    LPoints[LIndex * 2 + 1] := StrToFloat(StringGrid1.Cells[1, LIndex + 1]);
  end;
  CalculateCurveCoefficients(RadioGroup1.ItemIndex, 4, @LCurveCoefficients[0], 4, @LPoints[0]);
  StringGrid1.Cells[3, 1] := FloatToStr(Trinomial(LPoints[1], LCurveCoefficients));
  StringGrid1.Cells[3, 2] := FloatToStr(Trinomial(LPoints[3], LCurveCoefficients));
  StringGrid1.Cells[3, 3] := FloatToStr(Trinomial(LPoints[5], LCurveCoefficients));
  StringGrid1.Cells[3, 4] := FloatToStr(Trinomial(LPoints[7], LCurveCoefficients));
end;

end.
