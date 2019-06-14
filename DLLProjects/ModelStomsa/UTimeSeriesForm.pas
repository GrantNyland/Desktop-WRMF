unit UTimeSeriesForm;

//Still need the following:
//1. Ability to change the phi and theta parameters for a gauge
//2. Calculate and save the gauge ranks

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ComCtrls, VCL.StdCtrls, VCL.ExtCtrls, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, VCL.Grids,
  UMarkStringGrid, UExplodePanel, UStomsaData, UStomsaGlobalData, VCL.ActnList, VCL.OleCtnrs,
  VCL.Imaging.jpeg,  UAbstractObject;

type
  TfmTimeSeries = class(TFrame)
    stbTimeSeries: TStatusBar;
    Panel1: TPanel;
    lstGauges: TListBox;
    Panel2: TPanel;
    Label1: TLabel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    grdTimeSeries: TMarkStringGrid;
    Splitter2: TSplitter;
    pnlCorrelateMenu: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    procedure actSelectGaugeExecute(Sender: TObject);
    procedure grdTimeSeriesClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    procedure AddTimeSeriesEntry;
    procedure SetRowColor(TheRow : Integer; TheColour : TColor);
  public
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
  end;

var
  fmTimeSeries: TfmTimeSeries;

implementation

uses
  System.UITypes,
  UDataModule,
  UStomsaStatistics,
  UGraphUnit,
  UStomsaDLLManager,
  UStomsaModelManager,
  UErrorHandlingOperations;

var
  LastGauge : Integer;

{$R *.DFM}

constructor TfmTimeSeries.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmTimeSeries.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmTimeSeries.AfterConstruction;
const OPNAME = 'TfmTimeSeries.AfterConstruction';
var
  Loop1, Loop2 : Integer;
begin
  inherited;
  try
    // Form OnCreate
    with grdTimeSeries do
    begin
      Cells[0,0] := 'Phi';
      Cells[1,0] := 'Theta';
      Cells[2,0] := 'Phi(1)';
      Cells[3,0] := 'Phi(2)';
      Cells[4,0] := 'Theta(1)';
      Cells[5,0] := 'Theta(2)';
      Cells[6,0] := 'AIC - Criterion';
      Cells[7,0] := 'Yield - Criterion';
      Cells[8,0] := 'Combined Criterion';
      Cells[9,0] := 'Rank';

      for Loop1 := 0 to 2 do
      begin
        for Loop2 := 0 to 2 do
        begin
          Cells[0,Loop2+1+Loop1*3] := IntToStr(Loop1);
          Cells[1,Loop2+1+Loop1*3] := IntToStr(Loop2);
        end;
      end;

    end;
    LastGauge := -1;

    //need to check for changes to the list and respond to this
    // Form OnActivate
    if fmData.DataStorage.TimeSeriesDataHasChanged = true then
    begin
      self.Refresh;
      LastGauge := lstGauges.ItemIndex;
      lstGauges.Clear;
      if fmData.DataStorage.First then
      begin
        AddTimeSeriesEntry;
        while fmData.DataStorage.Next do
          AddTimeSeriesEntry;
      end;
      if LastGauge >= 0 then
        lstGauges.ItemIndex := LastGauge
      else
        lstGauges.ItemIndex := 0;
      actSelectGaugeExecute(self);
      fmData.DataStorage.TimeSeriesDataHasChanged := false;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmTimeSeries.AddTimeSeriesEntry;
const OPNAME = 'TfmTimeSeries.AddTimeSeriesEntry';
var
  NNP, NY, I_type: Integer;
  NRM: array1000F;
  Ga, De, Xl, Xi: array1F;
  NN,IT,IER: arrayT9I;
  FFF,FF: array9F;
  Loop, Loop2, InsertPoint, Remainder : Integer;
  AJV: array1F;
  ErrorOccured : Boolean;
  RankArray: array[1..2,1..9] of Double;
begin
  try
    with fmData.DataStorage.CurrentRecord do
    begin
      lstGauges.Items.Add(ChangeFileExt(FileName,''));
      if MarginalFitted then
      begin
        //Only fit if not already done AND there has been a marginal fitting
        if NOT(TimeSeriesFitted) then
        begin
          Remainder := fmData.DataStorage.MarginalFittedCount - fmData.DataStorage.TimeSeriesFittedCount;

          stbTimeSeries.Panels[0].Text := 'Fitting Time Series ' + FileName +
                                          ' - Still to process:' + IntToStr(Remainder);
          stbTimeSeries.Refresh;

          Ga[1] := Gamma[UserCurve];
          De[1] := Delta[UserCurve];
          Xl[1] := Xlamda[UserCurve];
          Xi[1] := Xx[UserCurve];
          I_type := Itype;
          NNP := RecordLength - ZeroCount;
          NY := RecordLength;
          NLAG[1] := 0;
          NFRD[1] := 0;

          for Loop := 1 to RecordLength do
          begin
            Ajv[1] := AnnualRawIncData[loop];
            NRM[Loop] := UStomsaStatistics.SNV(IType,ErrorOccured,Ajv,Ga,De,Xl,Xi);
          end;

          TimeSeriesFitted := false;
          try

            TStomsaModelManager(FAppModules.Model).DLLManager.MDSTATS(
                                      @NY,
                                      @NNP,
                                      @M1,
                                      @M2,
                                      @AnnualRawIncData[1],
                                      @NaturalFlows[1],
                                      @NRM[1],
                                      @Mean[1],
                                      @StdDev[1],
                                      @Skew[1],
                                      @Excess[1],
                                      @NormalisedMean[1],
                                      @NormalisedStdDev[1],
                                      @NormalisedSkew[1],
                                      @NormalisedExcess[1],
                                      @Skew95[1],
                                      @Excess95[1],
                                      @ChiSquare[1],
                                      @ExceedanceXX1[1],
                                      @NormalisedChiSquare[1],
                                      @ExceedanceXX2[1]);

            TStomsaModelManager(FAppModules.Model).DLLManager.SERCOR(
                                      @NNP,
                                      @NY,
                                      @NFRD[1],
                                      @NLAG[1],
                                      @I_type,
                                      @NRM[1],
                                      @AnnualRawIncData[1],
                                      @Ga[1],
                                      @De[1],
                                      @Xl[1],
                                      @Xi[1],
                                      @NCORR[1,1],
                                      @NPORT[1],
                                      @NCONF[1],
                                      @NEXCD[1],
                                      @PCORR[1,1,1],
                                      @PPORT[1],
                                      @PFRD[1],
                                      @PLAG[1],
                                      @PCONF[1],
                                      @PEXCD[1],
                                      @NN[1],
                                      @IT[1],
                                      @IER[1],
                                      @FFF[1],
                                      @FF[1],
                                      @Phi[1,1],
                                      @Theta[1,1],
                                      @AIC[1],
                                      @DIST[1],
                                      @CRIT[1],
                                      @QI[1],
                                      @CRNX[1],
                                      @CRJX[1],
                                      @CRSX[1],
                                      @CRP[1],
                                      @Historical[1,1]);

            // Determine the default time series model from the CRIT parameter
            DefaultTimeSeriesModel := 0;

            // Initialise the sorting array
            for Loop := 1 to 9 do
            begin
              RankArray[1,Loop] := 0;
              RankArray[2,Loop] := 1.7E308; // Maximum possible value for double
            end;

            for Loop := 1 to 9 do
            begin
              InsertPoint := 1;
              for Loop2 := 1 to Loop do
              begin
                InsertPoint := Loop2;
                if CRIT[Loop] < RankArray[2,Loop2] then
                  Break;
              end;
              //move all stuff higher than InsertPoint up by one
              for Loop2 := Loop downto (InsertPoint + 1) do
              begin
                RankArray[1,Loop2] := RankArray[1,Loop2-1];
                RankArray[2,Loop2] := RankArray[2,Loop2-1];
              end;
              RankArray[1,InsertPoint] := Loop;
              RankArray[2,InsertPoint] := CRIT[Loop];
            end;

            for Loop := 1 to 9 do
              Rank[Loop] := Trunc(RankArray[1,Loop]);

            DefaultTimeSeriesModel := Trunc(RankArray[1,1]);

            UserTimeSeriesModel := DefaultTimeSeriesModel;
            TimeSeriesFitted := true;
            //DSR TimeSeriesFitted := true;
            //DSR fmData.DataStorage.UpdateCurrentRecord;
            fmData.DataStorage.CurrentRecord.TimeSeriesFitted := true;
          except
            //this will be handled internally by the DELPHI routine
            on EExternal do
              MessageDlg('SERCOR Failed on ' + FileName,mtError,[mbOk],0);
          end;

        end;//if NOT(TimeSeriesFitted)
        stbTimeSeries.Panels[0].Text := 'Time Series Fitting Complete';
        stbTimeSeries.Refresh;
      end;//if MarginalFitted
    end;//with fmData
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmTimeSeries.SetRowColor(TheRow : Integer; TheColour : TColor);
const OPNAME = 'TfmTimeSeries.SetRowColor';
var
  ColumnLoop : Integer;
begin
  try
    if TheColour = grdTimeSeries.FixedColor then
      for ColumnLoop := 0 to 1 do
        grdTimeSeries.ResetCellColor(ColumnLoop,TheRow)
    else
      for ColumnLoop := 0 to 1 do
        grdTimeSeries.SetColour(ColumnLoop,TheRow,TheColour)
  except on E: Exception do HandleError(E, OPNAME); end;
end;//PROCEDURE SetRowColour

procedure TfmTimeSeries.actSelectGaugeExecute(Sender: TObject);
const OPNAME = 'TfmTimeSeries.actSelectGaugeExecute';
var
  Loop, Loop2 : integer;
begin
  try
    if fmData.DataStorage.GotoIndex(lstGauges.ItemIndex + 1) then
    begin
      with fmData.DataStorage.CurrentRecord do
      begin
        for Loop := 1 to 9 do
        begin
          grdTimeSeries.Cells[2,Loop] := FloatToStrF(phi[1,Loop],ffFixed,15,4);
          grdTimeSeries.Cells[3,Loop] := FloatToStrF(phi[2,Loop],ffFixed,15,4);
          grdTimeSeries.Cells[4,Loop] := FloatToStrF(Theta[1,Loop],ffFixed,15,4);
          grdTimeSeries.Cells[5,Loop] := FloatToStrF(Theta[2,Loop],ffFixed,15,4);
          grdTimeSeries.Cells[6,Loop] := FloatToStrF(AIC[Loop],ffFixed,15,4);
          grdTimeSeries.Cells[7,Loop] := FloatToStrF(DIST[Loop],ffFixed,15,4);
          grdTimeSeries.Cells[8,Loop] := FloatToStrF(CRIT[Loop],ffFixed,15,4);
          for Loop2 := 1 to 9 do
          begin
            if Rank[Loop2] = Loop then
            begin
              grdTimeSeries.Cells[9,Loop] := IntToStr(Loop2);
              break;
            end;
          end;

          if Loop = DefaultTimeSeriesModel then
            SetRowColor(Loop,clRed)
          else if Loop = UserTimeSeriesModel then
            SetRowColor(Loop,clBlue)
          else
            SetRowColor(Loop,grdTimeSeries.FixedColor);
        end;
        grdTimeSeries.Row := UserTimeSeriesModel;
        grdTimeSeries.Refresh;
      end;//with fmData
    end;//if fmData
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmTimeSeries.grdTimeSeriesClick(Sender: TObject);
const OPNAME = 'TfmTimeSeries.grdTimeSeriesClick';
var
  Loop : Integer;
begin
  try
    with fmData.DataStorage.CurrentRecord do
      UserTimeSeriesModel := grdTimeSeries.Row;
    //DSR fmData.DataStorage.UpdateCurrentRecord;

    for Loop := 1 to 9 do
      if Loop = fmData.DataStorage.CurrentRecord.DefaultTimeSeriesModel then
        SetRowColor(Loop,clRed)
      else if Loop = fmData.DataStorage.CurrentRecord.UserTimeSeriesModel then
        SetRowColor(Loop,clBlue)
      else
        SetRowColor(Loop,grdTimeSeries.FixedColor);
    grdTimeSeries.Refresh;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
