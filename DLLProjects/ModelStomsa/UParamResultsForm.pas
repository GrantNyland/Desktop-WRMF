unit UParamResultsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ComCtrls, VCL.StdCtrls,
  UStomsaData, UStomsaGlobalData, UAbstractObject;

type
  TfmPARAMResults = class(TFrame)
    edtPARAMFile: TRichEdit;
  protected
    FAppModules: TAppModules;
    procedure FormRefresh;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    procedure AfterConstruction; override;
  end;

var
  fmPARAMResults: TfmPARAMResults;

implementation

uses
  UDataModule,
  UStomsaDLLManager,
  UStomsaModelManager,
  UErrorHandlingOperations;


var
  Dispersion : array500_500F;
  CrossRawData : array500_1000F;

{$R *.DFM}
constructor TfmPARAMResults.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmPARAMResults.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmPARAMResults.AfterConstruction;
const OPNAME = 'TfmPARAMResults.AfterConstruction';
var
  RecordCount : integer;
  Lengths,StartYr : array500I;
  Phi1, Phi2, Theta1, Theta2 : array500F;
  Counter, Loop : Integer;

  procedure LoadRecord(Position : integer);
  const OPNAME = 'UParamResultsForm.LoadRecord';
  //NOTE: Normalised is an array with position 1 = 1900 (Year)
  var
    loop : integer;

    function Trunc5(TheValue : Double) : double;
    const OPNAME = 'UParamResultsForm.Trunc5';
    begin
      Result := 0.00;
      try
        TheValue := TheValue * 100000;
        Result := Round(TheValue) / 100000;
      except on E: Exception do HandleError(E, OPNAME); end;
    end;

  begin
    try
      with fmData.DataStorage.CurrentRecord do
      begin
        Lengths[Position] := RecordLength;
        StartYr[Position] := StartYear - 1900;
        Phi1[Position] := Trunc5(Phi[1,UserTimeSeriesModel]);
        Phi2[Position] := Trunc5(Phi[2,UserTimeSeriesModel]);
        Theta1[Position] := Trunc5(Theta[1,UserTimeSeriesModel]);
        Theta2[Position] := Trunc5(Theta[2,UserTimeSeriesModel]);
        for loop := 1 to RecordLength do
          CrossRawData[Position,loop] := 0.0;
        for loop := 1 to RecordLength do
          CrossRawData[Position,loop+StartYr[Position]-1] := AnnualRawIncData[Loop];

        //ensure that the record is flagged as not containing generated flows now,
        //any previously generated flows are now defunct due to the new PARAM file
        StatisticsCalculated := False;
        //DSR fmData.DataStorage.UpdateCurrentRecord;
      end;//with fmData
    except on E: Exception do HandleError(E, OPNAME); end;
  end;
begin
  inherited;
  try
    if fmData.DataStorage.ParamDataHasChanged then
    begin
      if NOT(fmData.DataStorage.PARAMFileCreated) then
      begin
        RecordCount := fmData.DataStorage.IncFileCount;

        for loop := 1 to 500 do
        begin
          Lengths[loop] := 0;
          StartYr[loop] := 0;
          Phi1[loop] := 0.0;
          Phi2[loop] := 0.0;
          Theta1[loop] := 0.0;
          Theta2[loop] := 0.0;
        end;

        Counter := 0;
        if fmData.DataStorage.First then
        repeat
          inc(Counter);
          LoadRecord(Counter);
        until NOT(fmData.DataStorage.Next);

        if RecordCount > 0 then
        try
          with fmData.DataStorage.CrossResults do
          begin

            // Call the DLL.
            TStomsaModelManager(FAppModules.Model).DLLManager.CROSS(
                                   @RecordCount,
                                   @Lengths[1],
                                   @StartYr[1],
                                   @Phi1[1],
                                   @Phi2[1],
                                   @Theta1[1],
                                   @Theta2[1],
                                   @CrossRawData[1,1],
                                   @Dispersion[1,1],
                                   @Eigen_G0[1],
                                   @Eigen_H1[1],
                                   @Eigen_H0[1],
                                   @Root_G0[1,1],
                                   @Root_H0[1,1],
                                   @Root_H1[1,1],
                                   @Coef_Z[1,1],
                                   @Coef_A[1,1],
                                   @BB[1,1],
                                   @Differences[1,1]);

//            UStomsaGlobalData.Cross(RecordCount,Lengths,StartYr,Phi1,Phi2,Theta1,Theta2,CrossRawData,Dispersion,
//                                    Eigen_G0,Eigen_H1,Eigen_H0,Root_G0,Root_H0,Root_H1,Coef_Z,Coef_A,BB,Differences);
          end;
          fmData.DataStorage.PARAMFileCreated := true;
        except
          on EDivByZero do self.Caption := 'Unable to complete call to CROSS - No PARAM file created';
        end;
      end;
      edtPARAMFile.Clear;
      edtPARAMFile.Lines.AddStrings(fmData.DataStorage.GeneratePARAMFile);
      edtPARAMFile.SelStart := 0;
      fmData.DataStorage.ParamDataHasChanged := false;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmPARAMResults.FormRefresh;
const OPNAME = 'TfmPARAMResults.FormRefresh';
begin
  try
    edtPARAMFile.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
