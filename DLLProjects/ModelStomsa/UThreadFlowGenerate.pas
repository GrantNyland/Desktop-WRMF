unit UThreadFlowGenerate;

interface

uses
  Classes,
  UAbstractObject;

type
  TFlowGenerate = class(TThread)
  private
    FGenerateLength: Integer;
    FAppModules: TAppModules;
    procedure GetCurrentRecord;
  protected
    procedure Execute; override;
  public
    constructor Create(GenerateLength: Integer; AAppModules: TAppModules);
  end;

implementation

uses
  SysUtils,
  Windows,
  UStomsaData,
  UStomsaGlobalData,
  UDataModule,
  UThreadProgress,
  UStomsaDLLManager,
  UStomsaModelManager,
  UErrorHandlingOperations;

var
  TheCurrentRecord : TIncData;
  TheProgress      : TProgress;

{ TFlowGenerate }

constructor TFlowGenerate.Create(GenerateLength: Integer; AAppModules: TAppModules);
const OPNAME = 'TFlowGenerate.Create';
begin
  try
    FGenerateLength := GenerateLength;
    FAppModules := AAppModules;
    inherited Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowGenerate.GetCurrentRecord;
const OPNAME = 'TFlowGenerate.GetCurrentRecord';
begin
  try
    TheCurrentRecord := fmData.DataStorage.CurrentRecord;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TFlowGenerate.Execute;
const OPNAME = 'TFlowGenerate.Execute';
var
  NR, NGG, KeyGaugeCounter: Integer;
  NSYRD, IFLAGD, NFGD, NGPD: Integer;
  ITYPED, NZFD, NYRSD, ISTRYD: array200I;
  NKEYGD: Integer;
  IKEYGD: array200I;
  NGSTORD: array10I;
  FlowGenData: TFlowGenData;
  GaugeNumber, GenerateCount: Integer;

  procedure LoadCurrentData;
  const OPNAME = 'UThreadFlowGenerate.LoadCurrentData';
  //Load data for all items marked as applicable
  var
    Loop1, Loop2 : Integer;
  begin
    try
      Synchronize(GetCurrentRecord);
      inc(GaugeNumber);
      with TheCurrentRecord, FlowGenData do
      begin
        ITYPED[GaugeNumber] := Itype; // Marginal distribution type
        NZFD[GaugeNumber] := ZeroCount;
        NYRSD[GaugeNumber] := RecordLength;
        ISTRYD[GaugeNumber] := StartYear - 1900;

        //Identify the gauge as a key gauge
        if KeyGauge then
        begin
          Inc(KeyGaugeCounter);
          IKEYGD[KeyGaugeCounter] := GaugeNumber;
        end;

        //this needs to identify the gauge as included in the list for generation
        if GenerateFlows then
        begin
          inc(GenerateCount);
          NGSTORD[GenerateCount] := GaugeNumber;
        end;

        //Load historical monthly data
        for Loop1 := 1 to RecordLength do
        begin
          for Loop2 := 1 to 12 do
            XSDEL[GaugeNumber,Loop1,Loop2] := MonthlyRawIncData[Loop1,Loop2];
        end;

        PHID[GaugeNumber,1] := Phi[1,UserTimeSeriesModel];
        PHID[GaugeNumber,2] := Phi[2,UserTimeSeriesModel];
        THETAD[GaugeNumber,1] := Theta[1,UserTimeSeriesModel];
        THETAD[GaugeNumber,2] := Theta[2,UserTimeSeriesModel];
        PZEROD[GaugeNumber] := PZero;
        GAMMAD[GaugeNumber] := Gamma[UserCurve];
        DELTAD[GaugeNumber] := Delta[UserCurve];
        XLAMD[GaugeNumber] := xlamda[UserCurve];
        XID[GaugeNumber] := xx[UserCurve];
        ZMEAND[GaugeNumber] := Mean[1];
        ZSTDEVD[GaugeNumber] := StdDev[1];
        AA1D[GaugeNumber] := Historical[1,UserTimeSeriesModel];
        AA2D[GaugeNumber] := Historical[1,1];
        ZZ1D[GaugeNumber] := Historical[2,UserTimeSeriesModel];
        ZZ2D[GaugeNumber] := Historical[2,1]
      end;
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE LoadCurrentData

  procedure LoadCrossCorrelation;
  const OPNAME = 'UThreadFlowGenerate.LoadCrossCorrelation';
  //Load the cross correlation results from CROSS for use in the flow generation
  var
    Loop1, Loop2 : Integer;
  begin
    try
      with FlowGenData do
      begin
        for Loop1 := 1 to fmData.DataStorage.IncFileCount do
        begin
          for Loop2 := 1 to fmData.DataStorage.IncFileCount do
          begin
            BD[Loop1,Loop2] := fmData.DataStorage.CrossResults.Root_G0[Loop1,Loop2];
            B0D[Loop1,Loop2] := fmData.DataStorage.CrossResults.Root_H0[Loop1,Loop2];
            B1D[Loop1,Loop2] := fmData.DataStorage.CrossResults.Root_H1[Loop1,Loop2];
            AD[Loop1,Loop2] := fmData.DataStorage.CrossResults.Coef_Z[Loop1,Loop2];
            CD[Loop1,Loop2] := fmData.DataStorage.CrossResults.Coef_A[Loop1,Loop2];
          end;//for Loop2
        end;//for Loop1
      end;//with FlowGenData
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE LoadCrossCorrelation

  procedure StoreFlows;
  const OPNAME = 'UThreadFlowGenerate.StoreFlows';
  //Copy the flow generation results from the temp object to the final storage
  var
    FlowCounter : Integer;
  begin
    try
      FlowCounter := 0;
      if fmData.DataStorage.First then
      repeat
        with fmData.DataStorage.CurrentRecord do
        begin
          if GenerateFlows then
          begin
              inc(FlowCounter);

              GeneratedFlows     := FlowGenData.StochasticFlows[FlowCounter];
              SequenceCount      := 500;
              GeneratedYearCount := FGenerateLength;
              FlowsGenerated     := true;
              GenerateFlows      := False;
              //DSR fmData.DataStorage.UpdateCurrentRecord;
          end;//if GenerateFlows
        end;//with fmData
      until NOT(fmData.DataStorage.Next) or (FlowCounter = 10);
    except on E: Exception do HandleError(E, OPNAME); end;
  end;//PROCEDURE StoreFlows

begin
  try
    FreeOnTerminate := True;
    TheProgress := TProgress.Create(false);

    FlowGenData := TFlowGenData.Create;

    // Initialise the return variables.
    NR := 500;
    NGG := 10;
    ZeroMemory(@FlowGenData.StochasticFlows[1,1,1,1], NGG * NR * 100 * 12 * 4);

    with fmData.DataStorage do
    begin

      //Load plain integer Values
      NSYRD := FGenerateLength;
      IFLAGD := 0;//Rigorous method, 1 = bootstrap
      NKEYGD := KeyGaugeCount;//KeyGaugeCount;

      //Load marginal and time series data
      KeyGaugeCounter := 0;
      GaugeNumber := 0;
      GenerateCount := 0;

      //Load all gauge data into the arrays
      if fmData.DataStorage.First then
      repeat
        LoadCurrentData;
      until NOT(fmData.DataStorage.Next);

      NFGD := IncFileCount;//Number of gauges in PARAM.DAT
      NGPD := GenerateCount;//Number of gauges to process

      //Load data from Cross-Correlation
      LoadCrossCorrelation;
    end;

    with FlowGenData do
    begin

      // Call the DLL.
      TStomsaModelManager(FAppModules.Model).DLLManager.FLOWGEN(
                              NR,
                              NGG,
                              NSYRD,
                              IFLAGD,
                              NKEYGD,
                              NFGD,
                              NGPD,
                              @NGSTORD[1],
                              @ITYPED[1],
                              @NZFD[1],
                              @NYRSD[1],
                              @ISTRYD[1],
                              @IKEYGD[1],
                              @XSDEL[1,1,1],
                              @PHID[1,1],
                              @THETAD[1,1],
                              @PZEROD[1],
                              @GAMMAD[1],
                              @DELTAD[1],
                              @XLAMD[1],
                              @XID[1],
                              @ZMEAND[1],
                              @ZSTDEVD[1],
                              @BD[1,1],
                              @AA1D[1],
                              @AA2D[1],
                              @ZZ1D[1],
                              @ZZ2D[1],
                              @B0D[1,1],
                              @B1D[1,1],
                              @AD[1,1],
                              @CD[1,1],
                              @StochasticFlows[1,1,1,1]);

//      FLOWGEN(NSYRD, IFLAGD, NKEYGD, NFGD, NGPD, NGSTORD, ITYPED, NZFD, NYRSD, ISTRYD,
//              IKEYGD, XSDEL, PHID, THETAD, PZEROD, GAMMAD, DELTAD, XLAMD, XID, ZMEAND,
//              ZSTDEVD, BD, AA1D, AA2D, ZZ1D, ZZ2D, B0D, B1D, AD, CD,
//              StochasticFlows[1],StochasticFlows[2],StochasticFlows[3],StochasticFlows[4],
//              StochasticFlows[5],StochasticFlows[6],StochasticFlows[7],StochasticFlows[8],
//              StochasticFlows[9],StochasticFlows[10]);

      StoreFlows;
    end;
    FlowGenData.Destroy;
    TheProgress.Terminate;
    Terminate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
