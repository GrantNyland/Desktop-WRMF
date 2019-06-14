unit UMarginalForm;

//1. Still need the ability to enter user defined parameters
//2. Appear to be showing incorrect stats in gauge box; need average, std. dev and cv

interface

uses

  // Delphi
  System.Classes, Vcl.Imaging.jpeg, Vcl.ExtCtrls, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.Grids, Vcl.ComCtrls,

  // DWS
  UMarkStringGrid,
  UAbstractObject;

type
  TfmMarginal = class(TFrame)
    stbMarginal: TStatusBar;
    grdGauges: TMarkStringGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    grdParameters: TMarkStringGrid;
    Splitter2: TSplitter;
    pnlCorrelateMenu: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    procedure actSelectGaugeExecute(Sender: TObject);
    procedure grdParametersClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    procedure AddMarginalEntry;
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmMarginal: TfmMarginal;

implementation

uses

  // Delphi
  SysUtils,
  Windows,
  Vcl.Graphics,

  // DWS
  UDataModule,
  UStomsaData,
  UStomsaDLLManager,
  UStomsaGlobalData,
  UStomsaStatistics,
  UStomsaModelManager,
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmMarginal.Create(AOwner: TComponent; AAppModules: TAppModules);
const OPNAME = 'TfmMarginal.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmMarginal.AfterConstruction;
const OPNAME = 'TfmMarginal.AfterConstruction';
begin
  inherited;
  try
    // Form OnCreate
    with grdGauges do
    begin
      Cells[0,0] := 'File';
      Cells[1,0] := 'Start Year';
      Cells[2,0] := 'End Year';
      Cells[3,0] := 'Length';
      Cells[4,0] := 'No. Zeros';
      Cells[5,0] := 'Default Curve';
    end;
    with grdParameters do
    begin
      Cells[0,1] := 'Gamma';
      Cells[0,2] := 'Delta';
      Cells[0,3] := 'Xlamda';
      Cells[0,4] := 'Xx';
      Cells[0,5] := 'Criterion';

      Cells[1,0] := 'LN3';
      Cells[2,0] := 'LN2';
      Cells[3,0] := 'SB4';
      Cells[4,0] := 'SB3';
    end;

    // Form OnActivate
    if fmData.DataStorage.MarginalDataHasChanged then
    begin
      //load up all the relevant stuff from the DataStorage object
      grdGauges.RowCount := 1;
      //need to check for changes to the list and respond to this
      if fmData.DataStorage.First then
      begin
        AddMarginalEntry;
        while fmData.DataStorage.Next do
          AddMarginalEntry;

        grdGauges.Row := 1;
        actSelectGaugeExecute(self);
      end;
      stbMarginal.Panels[0].Text := 'Complete';
      fmData.DataStorage.MarginalDataHasChanged := false;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TfmMarginal.AddMarginalEntry;
const OPNAME = 'TfmMarginal.AddMarginalEntry';
var
  xv, xin : double;
  loop,loop2    : integer;
  Ga,De,Xl,Xxx,
  Aj            : Array1F;
  NormFlow      : array1000F;
  ErrorOccured  : Boolean;
begin
  try
    with grdGauges,fmData.DataStorage.CurrentRecord do
    begin
      RowCount := RowCount + 1;

      if NOT(MarginalFitted) then
      begin
        stbMarginal.Panels[0].Text := 'Fitting ' + Directory + FileName;
        self.Refresh;

        //  FITMARG fits the marginal distributions.
        //   It is important to note that the curve fitting is only carried out on the non-zero
        //   values with the X-axis values set at i/(N-NZF+1) for i := 1 to (N - NZF) where;
        //   N := the number of years in the file and,
        //   NZF := the number of zero annual values.}
        TStomsaModelManager(FAppModules.Model).DLLManager.FITMARG(
                @RecordLength,
                @StartYear,
                @ZeroCount,
                @DefaultCurve,
                @AnnualRawIncData[1],
                @NaturalFlows[1],
                @NormFlow[1],
                @Gamma[1],
                @Delta[1],
                @Xlamda[1],
                @Xx[1],
                @Criterion[1],
                @Average[1],
                @StdDeviation[1]);
        MarginalFitted := true;

        // Determine probability of zero
        if ZeroCount = 0 then
          PZero := 0.0
        else
          PZero := ZeroCount/RecordLength;

        for loop := 1 to RecordLength do
        begin
          xv := loop / (RecordLength + 1);
          if loop > ZeroCount then   //PVR
          begin
            xin := (xv - PZero)/(1.0 - PZero);
            SNVR[1] := UStomsaStatistics.XNorm(xin);
            Standardised[Loop] := SNVR[1];
            for loop2 := 1 to 4 do
            begin
              Ga[1] := Gamma[loop2];
              De[1] := Delta[loop2];
              Xl[1] := Xlamda[loop2];
              Xxx[1] := Xx[loop2];
              itype := 1;
              if loop2 > 2 then
                itype := 3;
              Fitted[loop2-1,loop-1] := UStomsaStatistics.AJV(itype,ErrorOccured,Snvr,Ga,De,Xl,Xxx);
            end;
          end;
        end;

        for loop := 1 to (RecordLength - ZeroCount) do
        begin
          Aj[1]  := NaturalFlows[loop];
          for loop2 := 1 to 4 do
          begin
            Ga[1]  := Gamma[loop2];
            De[1]  := Delta[loop2];
            Xl[1]  := Xlamda[loop2];
            Xxx[1] := Xx[loop2];
            itype := 1;
            if loop2 > 2 then
              itype := 3;
            NormalisedFlows[loop2-1,loop-1] := UStomsaStatistics.SNV(itype,ErrorOccured,Aj,Ga,De,Xl,Xxx);
          end;
        end;

        UserCurve := DefaultCurve;

        // Set Itype
        itype := 1;
        if UserCurve > 2 then
          itype := 3;

        fmData.DataStorage.CurrentRecord.MarginalFitted := True;
        //fmData.DataStorage.UpdateCurrentRecord;
      end;
      Cells[0,RowCount-1] := ChangeFileExt(FileName,'');
      Cells[1,RowCount-1] := IntToStr(StartYear);
      Cells[2,RowCount-1] := IntToStr(EndYear);
      Cells[3,RowCount-1] := IntToStr(RecordLength);
      Cells[4,RowCount-1] := IntToStr(ZeroCount);
      Cells[5,RowCount-1] := IntToStr(DefaultCurve);
      FixedRows := 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmMarginal.actSelectGaugeExecute(Sender: TObject);
const OPNAME = 'TfmMarginal.actSelectGaugeExecute';
var
  loop : integer;
begin
  try
    if fmData.DataStorage.GotoIndex(grdGauges.Row) then
    begin
      with fmData.DataStorage.CurrentRecord,grdParameters do
      begin
        //Fill the parameter grid and highlight the default one
        for loop := 1 to 4 do
        begin
          Cells[loop,1] := FloatToStrF(Gamma[loop],ffExponent,7,2);
          Cells[loop,2] := FloatToStrF(Delta[loop],ffExponent,7,2);
          Cells[loop,3] := FloatToStrF(Xlamda[loop],ffExponent,7,2);
          Cells[loop,4] := FloatToStrF(Xx[loop],ffExponent,7,2);
          Cells[loop,5] := FloatToStrF(Criterion[loop],ffExponent,7,2);

          if loop = DefaultCurve then
            SetColour(Loop,0,clRed)
          else if Loop = UserCurve then
            SetColour(Loop,0,clBlue)
          else
            SetColour(Loop,0,FixedColor);

          grdParameters.Refresh;
        end;//for loop

        Row := 1;
        Col := UserCurve;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmMarginal.grdParametersClick(Sender: TObject);
const OPNAME = 'TfmMarginal.grdParametersClick';
var
  Loop : Integer;
begin
  try
    with fmData.DataStorage.CurrentRecord,grdParameters do
    begin
      if UserCurve <> Col then
      begin
        UserCurve := Col;
        //highlight the column
        for Loop := 1 to 4 do
          if Loop = DefaultCurve then
            SetColour(Loop,0,clRed)
          else if Loop = Col then
            SetColour(Loop,0,clBlue)
          else
            SetColour(Loop,0,FixedColor);

        grdParameters.Refresh;
        // Set Itype
        itype := 1;
        if UserCurve > 2 then
          itype := 3;

        TimeSeriesFitted := False;
        //DSR TimeSeriesFitted := False;
        fmData.DataStorage.PARAMFileCreated := False;
        //DSR fmData.DataStorage.UpdateCurrentRecord;
        fmData.DataStorage.CurrentRecord.TimeSeriesFitted := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
