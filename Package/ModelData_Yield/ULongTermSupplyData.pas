unit ULongTermSupplyData;

interface
uses
  VCL.Controls,
  Contnrs,
  Classes,
  VCL.Graphics,
  VCL.Forms,
  VCL.Dialogs,
  UAbstractObject,
  VoaimsCom_TLB,
  UYieldModelDataGUIForm;

type

  TCompliance = array of double;
  TLongTermSupply = class(TAbstractAppObject)
  protected
    FChannelNumber : integer;
    FSupply : TCompliance;
    FTargetDraft : double;
    FPrioritySplit : TCompliance;
    FDemandSplit : TCompliance;
    FActualDemand : double;
    FTotalDemandSplit : double;
    FCriteriaStatus : boolean;
    FIdentifier : integer;
    FNoOfSeqFullDemObtained : integer;
    FNoOfSeqDemNotObtained : integer;
    FBreakPointRI : double;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetBreakPointRI : double;
  public
    FValueList : TStringList;
    FStackedValues : TStringList;
    function Initialise: Boolean; override;
    property Identifier : integer read FIdentifier;
    property ChannelNumber : integer read FChannelNumber write FChannelNumber;
    property Supply : TCompliance read FSupply write FSupply;
    property TargetDraft : double read FTargetDraft write FTargetDraft;
    property CriteriaStatus : boolean read FCriteriaStatus write FCriteriaStatus;
    property PrioritySplit : TCompliance read FPrioritySplit write FPrioritySplit;
    property DemandSplit : TCompliance read FDemandSplit write FDemandSplit;
    property ActualDemand : double read FActualDemand write FActualDemand;
    property TotalDemandSplit : double read FTotalDemandSplit write FTotalDemandSplit;
    property NoOfSeqFullDemObtained : integer read FNoOfSeqFullDemObtained;
    property NoOfSeqDemNotObtained : integer read FNoOfSeqDemNotObtained;
    property BreakPointRI : double read GetBreakPointRI;

end;

  TLongTermSupplyData = class(TAbstractAppObject)
  protected
    FLongTermSupply : TStringList;
    FComplianceArray : TCompliance;
    FComplianceAssurance : TCompliance;
    FSelectedChannels : string;
    FSelectedRIArray : TIntegerArray;
    FRIArray : TIntegerArray;
    FDefaultRIArray : TIntegerArray;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function InArray(AValue : integer; AArray : TIntegerArray) : boolean;
    function CalculateXPointStochastic (APointYears : integer;APlaneYears : integer): double;
    function LoadRecurrenceIntervals: boolean;
    procedure ClearCompliance;
    procedure CalculateCompliance(ALongTermSupply : TLongTermSupply);
    procedure PopulateCompliance(ALongTermSupply : TLongTermSupply);
  public
    FPrioritySplitStr : TStringList;
    FSplits : TStringList;
    FDemandSplitstr : TStringList;

    function SetSupplyCriteriaStatus(ALongTermSupply : TLongTermSupply;AYValue, AXValue : double) : boolean;

    function AddLongTermSupply(AChannelNumber : integer) : TLongTermSupply;
    function GetLongTermSupplyByChannel(AChannelNumber : integer;AIdentifier : integer) : TLongTermSupply;
    function CreateLongTermSupply(AChannelNumber : integer): TLongTermSupply;
    function LongTermSupplyCount : integer;
    function Initialise: Boolean; override;
    property SelectedChannels : string read FSelectedChannels write FSelectedChannels;
    property ComplianceAssurance : TCompliance read FComplianceAssurance write FComplianceAssurance;

end;

implementation
uses
  SysUtils,
  VCLTee.Chart,
  Windows,
  UConstants,
  UDataSetType,
  VCLTee.TeeShape,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, VCL.CheckLst;

{ TLongTermSupplyData }

function TLongTermSupplyData.AddLongTermSupply(AChannelNumber : integer) : TLongTermSupply;
const OPNAME = 'TLongTermSupplyData.AddLongTermSupply';
var
  LLongTermSupply : TLongTermSupply;
begin
  Result := nil;
  try
    LLongTermSupply := TLongTermSupply.Create(FAppModules);
    LLongTermSupply.ChannelNumber := AChannelNumber;
    FLongTermSupply.AddObject(IntToStr(LLongTermSupply.ChannelNumber),LLongTermSupply);
    Result := LLongTermSupply;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongTermSupplyData.CalculateCompliance(ALongTermSupply : TLongTermSupply);
const OPNAME = 'TLongTermSupplyData.CalculateCompliance';
var
  LYieldModelData : IYieldModelData;
  LConfig         : IWaterDemandConfiguration;
  LChannel        : IGeneralFlowChannel;
  LRICount        : integer;
  LCategoryCount  : integer;
  LRequired       : double;
  LRIIndex        : integer;
  LCategoryIndex  : integer;
  LCategory       : IWaterDemandCategory;
  LTotal          : double;
begin
  try
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LConfig := LYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
    LChannel := LYieldModelData.NetworkElementData.ChannelList.ChannelByChannelNumber[ALongTermSupply.ChannelNumber];
    if (LChannel <> nil) and (LConfig <> nil) and (ALongTermSupply <> nil)then
    begin
      LRICount := LConfig.RiskCriteriaCount;
      LCategoryCount := LConfig.DemandCategoryCount;
      SetLength(FComplianceArray, LRICount + 1);
      SetLength(ALongTermSupply.FDemandSplit, LRICount + 1);
      SetLength(ALongTermSupply.FPrioritySplit, LRICount + 1);
      for LRIIndex := LRICount downto 1 do
      begin
        LRequired := ALongTermSupply.FActualDemand;
        FComplianceArray[LRIIndex] := 0;
        LTotal := 0.0;
        for LCategoryIndex := 1 to LCategoryCount do
        begin
          LCategory := LConfig.DemandCategoryByID[LCategoryIndex];
          LTotal    := LTotal + (LCategory.DemandPortionByIndex[LRIIndex]);
        end;
        FComplianceArray[LRIIndex] := LTotal * LRequired;
        ALongTermSupply.FDemandSplit[LRIIndex] := FComplianceArray[LRIIndex];
        ALongTermSupply.FPrioritySplit[LRIIndex] := LCategory.DemandPortionByIndex[LRIIndex];
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongTermSupplyData.CreateMemberObjects;
const OPNAME = 'TLongTermSupplyData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLongTermSupply := TStringList.Create;
    FPrioritySplitStr := TStringList.Create;
    FDemandSplitstr := TStringList.Create;
    FSplits := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongTermSupplyData.DestroyMemberObjects;
const OPNAME = 'TLongTermSupplyData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FLongTermSupply);
    FreeAndNil(FPrioritySplitStr);
    FreeAndNil(FDemandSplitstr);
    FreeAndNil(FSplits);
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupplyData.GetLongTermSupplyByChannel(AChannelNumber : integer;AIdentifier : integer): TLongTermSupply;
const OPNAME = 'TLongTermSupplyData.GetLongTermSupplyByChannel';
var
  LLongTermSupply : TLongTermSupply;
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FLongTermSupply.Count-1 do
    begin
      LLongTermSupply := TLongTermSupply(FLongTermSupply.Objects[LIndex]);
      if (LLongTermSupply <> nil) and
        (LLongTermSupply.ChannelNumber = AChannelNumber) and
        (LLongTermSupply.Identifier = AIdentifier) then
      begin
        Result := LLongTermSupply;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TLongTermSupplyData.ClearCompliance;
const OPNAME = 'TLongTermSupplyData.ClearCompliance';
begin
  try
    SetLength(FComplianceArray,0);
    SetLength(FComplianceAssurance,0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupplyData.LoadRecurrenceIntervals: boolean;
const OPNAME = 'TLongTermSupplyData.LoadRecurrenceIntervals';
var
  LIndex      : integer;
  LConfig     : IWaterDemandConfiguration;
  LRICount    : integer;
  LRIVal      : integer;
  LRIStr      : string;
  LRIList     : TStringList;
  LCheckIndex : integer;
begin
  Result := False;
  try
    LConfig  := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.WaterDemandConfiguration;
    LRICount := LConfig.RiskCriteriaCount;
    LRIList  := TStringList.Create;
    LRIList.Sorted := TRUE;
    try
      LRIList.CommaText := '0005,0010,0020,0050,0100,0200,0500';
      for LIndex := 1 to LRICount do
      begin
        LRIVal := Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]);
        LRIStr := Format('%4.4d', [LRIVal]);
        if (LRIList.IndexOf(LRIStr) < 0) then
          LRIList.Add(LRIStr);
      end;
      SetLength(FRIArray, LRIList.Count);
      SetLength(FSelectedRIArray, LRIList.Count);
      SetLength(FDefaultRIArray, LRIList.Count);
      for LIndex := 1 to LRIList.Count do
      begin
        LRIVal := StrToInt(LRIList.Strings[LIndex - 1]);
        FRIArray[LIndex - 1]        := LRIVal;
        FDefaultRIArray[LIndex - 1] := LRIVal;
        FSelectedRIArray[LIndex - 1]:= 0;
      end;
      for LIndex := 1 to LRICount do
      begin
        LRIVal := Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]);
        LRIStr := Format('%4.4d', [LRIVal]);
        LCheckIndex := LRIList.IndexOf(LRIStr);
        if (LCheckIndex >= 0) then
          FSelectedRIArray[LCheckIndex] := LRIVal;
      end;
    finally
      FreeAndNil(LRIList);
    end;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TLongTermSupplyData.InArray(AValue : integer; AArray : TIntegerArray) : boolean;
const OPNAME = 'TLongTermSupplyData.InArray';
var
  LIndex : integer;
begin
  Result := False;
  try
    for LIndex := Low(AArray) to High(AArray) do
      if (AValue = AArray[LIndex]) then
      begin
        Result := True;
        Break;
      end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TLongTermSupplyData.CalculateXPointStochastic (APointYears : integer;APlaneYears : integer): double;
const OPNAME = 'TLongTermSupplyData.CalculateXPointStochastic';
begin
  Result := -1;
  try
    if (APlaneYears>0) then
    begin
      Result := 1 - (1 - Power(1 - (1/APlaneYears),APointYears));
      Result := Result * 100;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongTermSupplyData.PopulateCompliance(ALongTermSupply : TLongTermSupply);
const OPNAME = 'TLongTermSupplyData.PopulateCompliance';
var
  LYieldModelData : IYieldModelData;
  LConfig         : IWaterDemandConfiguration;
  LIndex          : integer;
  LYearsCount     : integer;
  LXCentre        : double;
  LRInterval      : integer;
  LMaxYValue      : double;
  LCumYValue      : double;
  LCriteriaStatus : boolean;
begin
  try
    ClearCompliance;
    FPrioritySplitStr.Clear;
    FDemandSplitstr.Clear;
    LYieldModelData := FAppModules.Model.ModelData as IYieldModelData;
    LConfig         := lYieldModelData.NetworkFeaturesData.WaterDemandConfiguration;
    LYearsCount     := lYieldModelData.RunConfigurationData.YearsInAnalysis;
    CalculateCompliance(ALongTermSupply);
    LCumYValue := 0.0;
    LCriteriaStatus := True;
    for LIndex := Low(FSelectedRIArray) to High(FSelectedRIArray) do
    begin
      if FSelectedRIArray[LIndex] > 0 then
      begin
        FPrioritySplitStr.Add(Format('Priority Split %d', [FSelectedRIArray[LIndex]]));
        FDemandSplitstr.Add(Format('Demand Split 1 in %d', [FSelectedRIArray[LIndex]]));
      end;
    end;
    SetLength(FComplianceAssurance,High(FComplianceArray)+1);
    for LIndex := High(FComplianceArray) downto Low(FComplianceArray) do
    begin
      if (FComplianceArray[LIndex] > 0) then
      begin
        LMaxYValue := FComplianceArray[LIndex];
        LCumYValue := LCumYValue + LMaxYValue;
        LRInterval := Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]);
        if (InArray(LRInterval, FSelectedRIArray)) then
        begin
          LXCentre := CalculateXPointStochastic(LYearsCount, Trunc(LConfig.RecurrenceIntervalByIndex[LIndex]));
          CalculateXPointStochastic(LYearsCount, FRIArray[LIndex]);
          FComplianceAssurance[LIndex] := LXCentre;
          FSplits.Add(IntToStr(Trunc(LConfig.RecurrenceIntervalByIndex[LIndex])));
          if LCriteriaStatus then
            LCriteriaStatus := SetSupplyCriteriaStatus(ALongTermSupply,LCumYValue,Trunc(LXCentre));
          ALongTermSupply.FStackedValues.Add({IntToStr(Trunc(LXCentre))+'='+ }FloatToStr(LCumYValue));
        end;
      end;
    end;
    ALongTermSupply.CriteriaStatus := LCriteriaStatus;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupplyData.SetSupplyCriteriaStatus(ALongTermSupply : TLongTermSupply;AYValue, AXValue : double) : boolean;
const OPNAME = 'TLongTermSupplyData.SetSupplyCriteriaStatus';
var
  LIndex : integer;
  LYValue : double;
  LXValue : double;
begin
  Result := True;
  try
    if ALongTermSupply <> nil then
    begin
      for LIndex := 0 to ALongTermSupply.FValueList.Count-1 do
      begin
        if Trim(ALongTermSupply.FValueList[LIndex]) <> '' then
        begin
          LXValue := StrToFloat(ALongTermSupply.FValueList.Names[LIndex]);
          if LXValue >= AXValue then
          begin
            LYValue := StrToFloat(ALongTermSupply.FValueList.Values[FloatToStr(LXValue)]);
            if AYValue > LYValue then
            begin
              Result := False;
              Break;
            end
            else
              Break
          end;
        end;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TLongTermSupplyData.Initialise: Boolean;
const OPNAME = 'TLongTermSupplyData.Initialise';
var
  LSelectedChannels : TStringList;
  LDataStringList : TStringList;
  LActualDemand : double;
  LChannelNumber,
  LIndex : integer;
  LError : string;
  LCount : integer;
  LLongTermSupply : TLongTermSupply;
  LValues : TStringList;
  LValueIndex : integer;
  LXValue : double;
  LOldCursor      : TCursor;
  LChannel  : IGeneralFlowChannel;
  LCanPopulateCompliance : boolean;
begin
  Result := False;
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    FLongTermSupply.Clear;
    LSelectedChannels := TStringList.Create;
    LDataStringList := TStringList.Create;
    LValues := TStringList.Create;
    try
      if Trim(FSelectedChannels) <> '' then
      begin
        LoadRecurrenceIntervals;
        LSelectedChannels.CommaText := FSelectedChannels;
        for LIndex := 0 to LSelectedChannels.Count-1 do
        begin
          LDataStringList.Clear;
          LChannelNumber := StrToInt(LSelectedChannels[LIndex]);
          LCanPopulateCompliance := True;
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNumber];
          if (LChannel <> nil) then
          begin
            if (LChannel.MinMaxFlowConstraint <> nil) then
            begin
              if (LChannel.WaterDemandFeature = nil) then
                LCanPopulateCompliance := False
              else
              if (LChannel.SpecifiedDemandFeature <> nil) then
              begin
                if (LChannel.WaterDemandFeature = nil) then
                  LCanPopulateCompliance := False;
              end;
            end;

            if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetLongTermSupplyData(
               btAnualFirmSelectedYieldDemands,LChannelNumber,LDataStringList,LError,LActualDemand) then
            begin

              for LCount := 0  to LDataStringList.Count-1 do
              begin
                LLongTermSupply := AddLongTermSupply(LChannelNumber);
                if not LLongTermSupply.Initialise then Exit;
                LLongTermSupply.FIdentifier := LCount;
                LValues.Clear;
                LValues.CommaText := LDataStringList[LCount];
                if Trim(LValues[0]) <> '' then
                begin
                  LLongTermSupply.FActualDemand := StrToFloat(LValues[0]);
                  LValues.Delete(0);
                end;

                SetLength(LLongTermSupply.FSupply,LValues.Count);
                for LValueIndex := 0 to LValues.Count-1 do
                begin
                  if (LValueIndex <= LValues.Count-1) then
                  begin
                    LLongTermSupply.Supply[LValueIndex] := StrToFloat(LValues[LValueIndex]);
                    if LLongTermSupply.Supply[LValueIndex] = 0 then
                      LLongTermSupply.FNoOfSeqFullDemObtained := LLongTermSupply.FNoOfSeqFullDemObtained+1
                    else
                    if LLongTermSupply.Supply[LValueIndex] > 0 then
                      LLongTermSupply.FNoOfSeqDemNotObtained := LLongTermSupply.FNoOfSeqDemNotObtained+1;
                    LXValue := (LValueIndex)/(LValues.Count) * 100;
                    LLongTermSupply.FValueList.Add(InttoStr(Trunc(LXValue))+'='+FloatToStr(LLongTermSupply.Supply[LValueIndex]));
                  end;
                end;
                if LCanPopulateCompliance then
                  PopulateCompliance(LLongTermSupply);
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LSelectedChannels);
      FreeAndNil(LDataStringList);
      FreeAndNil(LValues);
    end;
    Result := True;
    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupplyData.LongTermSupplyCount: integer;
const OPNAME = 'TLongTermSupply.CreateMemberObjects';
begin
  Result := 0;
  try
    Result := FLongTermSupply.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupplyData.CreateLongTermSupply(AChannelNumber: integer): TLongTermSupply;
const OPNAME = 'TLongTermSupply.CreateMemberObjects';
var
  LError : string;
  LDataStringList : TStringList;
  LActualDemand : double;
  LValueIndex,
  LCount : integer;
  LValues : TStringList;
  LXValue : double;
  LChannel  : IGeneralFlowChannel;
  LCanPopulateCompliance : boolean;
begin
  Result := nil;
  try
    LCanPopulateCompliance := True;
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                 NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
    if (LChannel <> nil) then
    begin
      if (LChannel.MinMaxFlowConstraint <> nil) then
      begin
        if (LChannel.WaterDemandFeature = nil) then
        begin
          ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg2'));
          LCanPopulateCompliance := False;
        end
        else
        if (LChannel.SpecifiedDemandFeature <> nil) then
        begin
          if (LChannel.WaterDemandFeature = nil) then
          begin
            ShowMessage(FAppModules.Language.GetString('Message.OutputDistributionCurveValidatorMsg3'));
            LCanPopulateCompliance := False;
          end;
        end;
      end;
      if LoadRecurrenceIntervals then
      begin
        LDataStringList := TStringList.Create;
        LValues := TStringList.Create;
        try
          if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetLongTermSupplyData(
            btAnualFirmSelectedYieldDemands,AChannelNumber,LDataStringList,LError,LActualDemand) then
          for LCount := 0  to LDataStringList.Count-1 do
          begin
            Result := AddLongTermSupply(AChannelNumber);
            if not Result.Initialise then Exit;
            Result.FIdentifier := LCount;
            LValues.Clear;
            LValues.CommaText := LDataStringList[LCount];

            if Trim(LValues[0]) <> '' then
            begin
              Result.FActualDemand := StrToFloat(LValues[0]);
              LValues.Delete(0);
            end;

            SetLength(Result.FSupply,LValues.Count);
            for LValueIndex := 0 to LValues.Count-1 do
            begin
              if (LValueIndex <= LValues.Count-1) then
              begin
                Result.Supply[LValueIndex] := StrToFloat(LValues[LValueIndex]);
                if Result.Supply[LValueIndex] = 0 then
                  Result.FNoOfSeqFullDemObtained := Result.FNoOfSeqFullDemObtained+1
                else
                if Result.Supply[LValueIndex] > 0 then
                  Result.FNoOfSeqDemNotObtained := Result.FNoOfSeqDemNotObtained+1;
                LXValue := (LValueIndex)/(LValues.Count) * 100;
                Result.FValueList.Add(InttoStr(Trunc(LXValue))+'='+FloatToStr(Result.Supply[LValueIndex]));
              end;
            end;
            if LCanPopulateCompliance then
              PopulateCompliance(Result);
          end;
        finally
          FreeAndNil(LDataStringList);
          FreeAndNil(LValues);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TLongTermSupply }

procedure TLongTermSupply.CreateMemberObjects;
const OPNAME = 'TLongTermSupply.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FValueList := TStringList.Create;
    FStackedValues := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLongTermSupply.DestroyMemberObjects;
const OPNAME = 'TLongTermSupply.DestroyMemberObjects';
begin
  try
    FreeAndNil(FValueList);
    FreeAndNil(FStackedValues);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupply.Initialise: Boolean;
const OPNAME = 'TLongTermSupply.Initialise';
begin
  Result := False;
  try
    FNoOfSeqFullDemObtained := 0;
    FNoOfSeqDemNotObtained := 0;
    FBreakPointRI := 0;
    FCriteriaStatus := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLongTermSupply.GetBreakPointRI : double;
const OPNAME = 'TLongTermSupply.Initialise';
var
  LIndex : integer;
  LYValue : double;
begin
  Result := 0.0;
  try
    for LIndex := 0 to FValueList.Count-1 do
    begin
      LYValue := StrToFloat(FValueList.Values[FValueList.Names[LIndex]]);
      if LYValue = 0 then Continue;
      if LYValue > 0 then
      begin
        if (LIndex-1 <= FValueList.Count-1) and (LIndex-1>=0) then
        begin
          Result := StrToFloat(FValueList.Names[LIndex-1]);
          Break;
        end
        else
          Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
