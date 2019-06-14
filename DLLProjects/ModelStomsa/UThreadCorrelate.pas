unit UThreadCorrelate;

interface

uses
  Classes, UAbstractObject, UStomsaData, UStomsaGlobalData;

type
  TCorrelate = class(TThread)
    NRR           : arrayT1I;
    NYRS          : arrayT2I;
    ISTRY         : arrayT2I;
    NSYRI         : arrayT1I;
    XH1,XH2       : array13_400F; //Y2K
    FLOWD1,FLOWD2 : array101_400_13F; //Y2K
  private
    { Private declarations }
  protected
    FAppModules: TAppModules;
    procedure Execute; override;
  public
    constructor Create(AAppModules: TAppModules);
  end;

implementation

uses
  SysUtils,
  UDataModule,
  VCL.Graphics,
  UCorrelateForm,
  UStomsaDLLManager,
  UStomsaModelManager,
  UErrorHandlingOperations;

{ TCorrelate }

constructor TCorrelate.Create(AAppModules: TAppModules);
const OPNAME = 'TCorrelate.Create';
begin
  try
    FAppModules := AAppModules;
    inherited Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TCorrelate.Execute;
const OPNAME = 'TCorrelate.Execute';
var
  FirstLoop,
  SecondLoop : Integer;
  GaugeNames : Array[1..2] of string;

  procedure GetCor(Position : Integer);
  const OPNAME = 'UThreadCorrelate.GetCor';
  var
    Loop, Loop2,
    Loop3         : Integer;
    Sum : double;
  begin
    try
      with fmData.DataStorage.CurrentRecord do
      begin
        GaugeNames[Position] := FileName;
        if Position = 1 then
        begin
          NRR[1] := 41; //Number of sequences to process
          NYRS[1] := RecordLength;//Number of years
          ISTRY[1] := StartYear;
          NSYRI[1] := GeneratedYearCount;
          for Loop := 1 to 100 do
            for Loop2 := 1 to 13 do
              XH1[Loop2,Loop] := 0.0;

          for Loop := 1 to RecordLength do//Load monthly historical data
          begin
            for Loop2 := 1 to 12 do
              XH1[Loop2,Loop] := MonthlyRawIncData[Loop,Loop2];
            XH1[13,Loop] := AnnualRawIncData[Loop];
          end;

          for Loop := 1 to 101 do
            for Loop2 := 1 to 100 do
              for Loop3 := 1 to 13 do
                FlowD1[Loop,Loop2,Loop3] := 0.0;

          for Loop := 1 to SequenceCount do
            for Loop2 := 1 to GeneratedYearCount do
            begin
              Sum := 0;
              for Loop3 := 1 to 12 do
              begin
                FlowD1[Loop,Loop2,Loop3] := GeneratedFlows[Loop,Loop2,Loop3];
                Sum := Sum + GeneratedFlows[Loop,Loop2,Loop3];
              end;
                FlowD1[Loop,Loop2,13] := Sum;
            end;
        end//if Position = 1
        else
        begin
          NYRS[2] := RecordLength;//Number of years
          ISTRY[2] := StartYear;

          for Loop := 1 to 100 do
            for Loop2 := 1 to 13 do
              XH2[Loop2,Loop] := 0.0;

          for Loop := 1 to RecordLength do//Load monthly historical data
          begin
            for Loop2 := 1 to 12 do
              XH2[Loop2,Loop] := MonthlyRawIncData[Loop,Loop2];
            XH2[13,Loop] := AnnualRawIncData[Loop];
          end;

          for Loop := 1 to 101 do
            for Loop2 := 1 to 100 do
              for Loop3 := 1 to 13 do
                FlowD2[Loop,Loop2,Loop3] := 0.0;

          for Loop := 1 to SequenceCount do
            for Loop2 := 1 to GeneratedYearCount do
            begin
              Sum := 0;
              for Loop3 := 1 to 12 do
              begin
                FlowD2[Loop,Loop2,Loop3] := GeneratedFlows[Loop,Loop2,Loop3];
                Sum := Sum + GeneratedFlows[Loop,Loop2,Loop3];
              end;
              FlowD2[Loop,Loop2,13] := Sum;
            end;
        end;
      end;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

  procedure PerformCorrelation;
  const OPNAME = 'UThreadCorrelate.PerformCorrelation';
  var
    Loop, Loop2   : Integer;
    tempRSHO,                     //Historical correlation coefficients
    tempRMEANO      : Array13F;   //Mean of simulated, 12 months plus annual
    tempPO          : Array7_13F; //Distribution of correlation coefficients, 12 months plus annual
  begin
    try
      fmCorrelate.stbCorrelate.Panels[0].Text := 'Correlating ' + GaugeNames[1] + ' to ' + GaugeNames[2];
      TStomsaModelManager(FAppModules.Model).DLLManager.MTHRNK(
                 @NRR[1],
                 @NSYRI[1],
                 @NYRS[1],
                 @ISTRY[1],
                 @XH1[1,1],
                 @XH2[1,1],
                 @FLOWD1[1,1,1],
                 @FLOWD2[1,1,1],
                 @tempRSHO[1],
                 @tempRMEANO[1],
                 @tempPO[1,1]);
      with fmData.DataStorage.CurrentRecord.FlowCorrelationData do
      begin
        AddFlowCorrelationNode(GaugeNames[2]);
        with CurrentData do
        begin
          for Loop := 1 to 13 do
          begin
            RSHO[Loop] := tempRSHO[Loop];
            RMEANO[Loop] := tempRMEANO[Loop];
          end;
          for Loop2 := 1 to 7 do
            for Loop := 1 to 13 do
              PO[Loop2,Loop] := tempPO[Loop2,Loop];
        end;
        //SaveCurrentData;
      end;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;

begin
  try
    FreeOnTerminate := true;
    fmCorrelate.grdCompare.Enabled := False;// <-- Prevent the user from selecting additional gauges
    fmCorrelate.stbCorrelate.Panels[0].Text := 'Preparing data for generation';
    //Create the object to handle the data
    //TheProgress := TProgress.Create(false);

    //  Get the two records to correlate with each other
    //compare each gauge on vertical with each gauge on horizontal
    //have a loop to select gauges on vert and then compare them to gauges on the horiz
    //need to add subnodes to the first gauge to hold the comparison data

    for FirstLoop := 1 to (fmCorrelate.grdCompare.RowCount - 1) do
    begin
      //load up the first gauge in case
      fmData.DataStorage.GotoIndex(FirstLoop);
      GetCor(1);

      for SecondLoop := 1 to (fmCorrelate.grdCompare.ColCount - 1) do
      begin
        //If this cell is marked for processing then create a sub node,
        //load the data into the arrays and process the comparison
        if fmCorrelate.grdCompare.GetColour(SecondLoop,FirstLoop) = clBlack then
        begin
          fmData.DataStorage.GotoIndex(SecondLoop);
          GetCor(2);
          fmData.DataStorage.GotoIndex(FirstLoop);
          PerformCorrelation;
          fmCorrelate.grdCompare.SetColour(SecondLoop,FirstLoop,clNavy);
          fmCorrelate.grdCompare.Refresh;
        end;
      end;//for SecondLoop
    end;//for FirstLoop

    fmCorrelate.stbCorrelate.Panels[0].Text := 'Correlation Complete';
    fmCorrelate.grdCompare.Enabled := True;
    //TheProgress.Terminate;
    Terminate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
