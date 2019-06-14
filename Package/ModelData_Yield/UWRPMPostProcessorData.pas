unit UWRPMPostProcessorData;

interface
uses
  classes,
  sysutils,
  contnrs,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractDataObject,
  UAbstractFileNamesObject,
  UWRPMPltFileManager,
  UWRPMResFileManager,
  UWRPMSysFileManager,
  UWRPMPmpFileManager,
  UWRPMOutputSettings,
  UConstants;

type
  TWRPMPostProcessorData = class(TAbstractAppDataObject)
  protected
    //FDemandSupply : boolean;
    //FElementType : TNetworkElementType;
    FPopulating         : boolean;
    FPltFileManager     : TWRPMPltFileManager;
    FResFileManager     : TWRPMResFileManager;
    FSysFileManager     : TWRPMSysFileManager;
    FPmpFileManager     : TWRPMPmpFileManager;
    FWRPMOutputSettings : TWRPMOutputSettings;
    FElementData        : TStringList;
    FElementNumber      : integer;
    FElementName        : string;
    FElementType        : TNetworkElementType;
    FDecisionMonthName  : string;
    FDataType           : TOutputDataType;
    FOutputTimeStep     : TOutputTimeStep;
    FResFileName        : string;
    FSysFileName        : string;

    function GetOutputDataFile(AFilePreFix: string): TAbstractModelFileName;
    function CalculateAnnualChannelFlowData(ASourceData,ADestData : TStrings) : boolean;
    function CalculateMonthlyCummlativeChannelFlowData(ASourceData,ADestData : TStrings; AStartYear,AEndYear: integer) : boolean;
    function CalculateAnnualCummlativeChannelFlowData(ASourceData,ADestData : TStrings; AStartYear,AEndYear: integer) : boolean;
    function GetElementName(AElementNumber: integer; AElementType: TNetworkElementType): string;
    //______________________________________________________________________________________________________________//
    function GetDecisionMonthIndex(ADecisionMonth : integer): integer;
    function GetDecisionMonthData(AContainer : TStrings;ADecisionMonthIndex : integer): boolean;
    function GetAverageData(AContainer : TStrings): boolean;
    function AddDecisionData(AContainer : TStrings): string;
    procedure DoBoxPlot(AData : TStrings;AMonthly : boolean;ASupply : boolean);
    function CaptureAnnualData(AData : TStrings): boolean;
   // procedure SetNetworkElementType(AElementType: TNetworkElementType);
    procedure SetPopulating(APopulating: boolean);
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure Reset;override;
    function GetCurtailmentsData(AElementType : TNetworkElementType;AContainer : TStrings): boolean;
    function GetCurtailmentsBoxPlotData(AElementType : TNetworkElementType;ASubSystemIndex : integer; AContainer : TStrings): boolean;
    function GetDemandSupplyData(AElementType : TNetworkElementType;AContainer : TStrings): boolean;
    function GetDemandSupplyBoxPlotData(AElementType : TNetworkElementType;ASupportChannelNo : integer; AContainer : TStrings): boolean;

    function GetReservoirPltData(AElementType : TNetworkElementType;AIdentifier : integer;var AElement : string;AData: TStrings): boolean;
    function GetReservoirBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData: TStrings): boolean;
    function GetSubSystemPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
    function GetSubSystemBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
    function GetSupplyPltData(AElementType : TNetworkElementType;AIdentifier : integer;ASupplyData: TStrings): boolean;
    function GetSupplyBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;ASupplyData: TStrings): boolean;
    function GetChannelPmpData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
    function GetChannelPmpBoxPlotData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
    //________________________________Start New Functions___________________________________________________________
    function Get_PlotFileHeaderData: TWRPMPltFileHeaderData;
    function GetPltFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName : string; AElementType: TNetworkElementType): boolean;
    function GetResFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName : string; AElementType: TNetworkElementType): boolean;
    function GetInterBasinSupportFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName,AResFileName : string; AElementType: TNetworkElementType): boolean;
    function GetSysFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName,ASysFileName : string; AElementType: TNetworkElementType): boolean;
    function GetPmpFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName : string; AElementType: TNetworkElementType): boolean;
    function GetSubSystemSysFileName(ASubSystemName : string): string;
    function GetOutputPlotFileName : string;
    function GetOutputPumpFileName : string;

    //________________________________End New Functions_____________________________________________________________
    //property ElementType : TNetworkElementType read FElementType write SetNetworkElementType;
    property Populating : boolean read FPopulating write SetPopulating;
    property WRPMOutputSettings : TWRPMOutputSettings read FWRPMOutputSettings;

  end;

implementation
uses
  Math,
  UFileNames,
  //UWRPMPltFileAgent,
  //UWRPMSysFileAgent,
  //UWRPMResFileAgent,
  //UWRPMPmpFileAgent,
  UPlanningModelDataObject,
  UErrorHandlingOperations,
  UYieldModelDataObject,
  URunConfigurationData,
  UOutputData, DateUtils;

{ TWRPMPostProcessorData }


procedure TWRPMPostProcessorData.CreateMemberObjects;
const OPNAME = 'TWRPMPostProcessorData.CreateMemberObjects';
begin
  try
    FPltFileManager     := TWRPMPltFileManager.Create(FAppModules);
    FResFileManager     := TWRPMResFileManager.Create(FAppModules);
    FSysFileManager     := TWRPMSysFileManager.Create(FAppModules);
    FPmpFileManager     := TWRPMPmpFileManager.Create(FAppModules);
    FWRPMOutputSettings := TWRPMOutputSettings.Create(FAppModules);
    FElementData        := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPostProcessorData.DestroyMemberObjects;
const OPNAME = 'TWRPMPostProcessorData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FPltFileManager);
    FreeAndNil(FResFileManager);
    FreeAndNil(FSysFileManager);
    FreeAndNil(FPmpFileManager);
    FreeAndNil(FWRPMOutputSettings);
    FreeAndNil(FElementData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPostProcessorData.Reset;
const OPNAME = 'TWRPMPostProcessorData.Reset';
begin
  inherited;
  try
    FElementNumber      := NullInteger;
    FElementType        := votNone;
    FDataType           := btNone;
    FElementData.Clear;
    FPltFileManager.Initialise;
    FResFileManager.Initialise;
    FSysFileManager.Initialise;
    FPmpFileManager.Initialise;
    FWRPMOutputSettings.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPostProcessorData.SetPopulating(APopulating: boolean);
const OPNAME = 'TWRPMPostProcessorData.SetPopulating';
begin
  try
    //FElementNumber      := NullInteger;
    //FElementType        := votNone;
    //FDataType           := btNone;
    FPopulating         := APopulating;
    //FElementData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



{procedure TWRPMPostProcessorData.SetNetworkElementType(AElementType: TNetworkElementType);
const OPNAME = 'TWRPMPostProcessorData.SetNetworkElementType';
begin
  try
    FElementType := AElementType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TWRPMPostProcessorData.GetCurtailmentsData(AElementType : TNetworkElementType;AContainer: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetCurtailmentsData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMSysAgent;
  LSeqNo     : integer;}
begin
  Result := False;
  try
    {LFileAgent := TFileWRPMSysAgent.Create(FAppModules);
    try
      LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
      LFileName := GetOutputDataFile('SYS.OUT');
      Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AContainer,LSeqNo);
    finally
      LFileAgent.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetCurtailmentsBoxPlotData(AElementType : TNetworkElementType;ASubSystemIndex : integer; AContainer: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetCurtailmentsBoxPlotData';
{var
  LFileName      : TAbstractModelFileName;
  LFileAgent     : TFileWRPMSysAgent;
  LSortedValues  : TStringList;
  LAnnualValues  : TStringList;
  LLineValues    : TStringList;
  LTempValues    : TStringList;
  LSubSystemValues     : TStringList;
  LPositionArray : array[0..14] of integer;
  LRowCount      : integer;
  LIndex         : integer;
  LDecisionMonthIndex : integer;
  LDecisionMonth      : integer;
  LYear               : string;}
begin
  Result := False;
  try
    {AContainer.Clear;
    LFileAgent     := TFileWRPMSysAgent.Create(FAppModules);
    LTempValues    := TStringList.Create;
    LSortedValues  := TStringList.Create;
    LAnnualValues  := TStringList.Create;
    LLineValues    := TStringList.Create;
    LSubSystemValues := TStringList.Create;
    try
      LFileName := GetOutputDataFile('SYS.OUT');
      Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LTempValues,0);
      if Result and (LTempValues.Count > 1) then
      begin
        FDemandSupply := False;
        // Avarage or select data for a decision month
        LDecisionMonth      := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.DecisionMonth;
        LDecisionMonthIndex := GetDecisionMonthIndex(LDecisionMonth);
        if(LDecisionMonthIndex <= 0) then
          GetAverageData(LTempValues)
        else
          GetDecisionMonthData(LTempValues,LDecisionMonthIndex);

        // Get Valued for a selected subsystem
        if(LTempValues.Count > 0) then
        begin
          LSubSystemValues.Sorted := True;
          LSubSystemValues.Duplicates := dupAccept;

          LSortedValues.Sorted := True;
          LSortedValues.Duplicates := dupAccept;
          LSortedValues.Assign(LTempValues);
          LTempValues.Clear;

          LRowCount := 0;
          while (LRowCount < LSortedValues.Count) do
          begin
            LLineValues.CommaText  := LSortedValues[LRowCount];
            LYear := LLineValues[0];
            LAnnualValues.Clear;
            while (LYear = LLineValues[0]) do
            begin
              LAnnualValues.Add(LSortedValues[LRowCount]);
              LRowCount := LRowCount + 1;
              if(LRowCount >= LSortedValues.Count) then Break;
              LLineValues.CommaText  := LSortedValues[LRowCount];
            end;

            // get the position
            for LIndex := 0 to High(LPositionArray) do
            begin
              LPositionArray[LIndex] := Ceil((LAnnualValues.Count-1) * Exceedence[LIndex]);
            end;

            LSubSystemValues.Clear;
            if (ASubSystemIndex>-1) then
            begin
              for LIndex := 0 to LAnnualValues.Count-1 do
              begin
                LLineValues.CommaText  := LAnnualValues[LIndex];
                if((ASubSystemIndex + 1) < LLineValues.Count) then
                begin
                  LSubSystemValues.Add(LLineValues[ASubSystemIndex + 1]);
                end;
              end;
            end
            else
            if (ASubSystemIndex=-1) then
            begin
              for LIndex := 0 to LAnnualValues.Count-1 do
              begin
                LLineValues.CommaText  := LAnnualValues[LIndex];
                if(LLineValues.Count-4>0) then
                begin
                  LSubSystemValues.Add(LLineValues[LLineValues.Count-4]);
                end;
              end;
            end;

            LTempValues.Clear;
            LTempValues.Add(LYear);
            for LIndex := 0 to High(LPositionArray) do
            begin
              if(LPositionArray[LIndex] >= 0) and (LPositionArray[LIndex] < LSubSystemValues.Count) then
                LTempValues.Add(FormatFloat('##0.000',StrToFloat(LSubSystemValues[LPositionArray[LIndex]])));
            end;
            AContainer.Add(LTempValues.CommaText);
          end;
        end;
        if(AContainer.Count > 0) then
        begin
          LTempValues.Clear;
          LTempValues.Add('YEAR');
          for LIndex := 0 to High(LPositionArray) do
          begin
            LTempValues.Add(FormatFloat('##0.000',Exceedence[LIndex])+'%')
          end;
          AContainer.Insert(0,LTempValues.CommaText);
        end;
      end;
    finally
      LFileAgent.Free;
      LTempValues.Free;
      LSortedValues.Free;
      LAnnualValues.Free;
      LLineValues.Free;
      LSubSystemValues.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetDemandSupplyBoxPlotData(AElementType : TNetworkElementType;ASupportChannelNo : integer; AContainer: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetDemandSupplyBoxPlotData';
{var
  LFileName      : TAbstractModelFileName;
  LFileAgent     : TFileWRPMResAgent;
  LSortedValues  : TStringList;
  LAnnualValues  : TStringList;
  LLineValues    : TStringList;
  LTempValues    : TStringList;
  LSupportChannelValues     : TStringList;
  LPositionArray : array[0..14] of integer;
  LRowCount      : integer;
  LIndex         : integer;
  LDecisionMonthIndex : integer;
  LDecisionMonth      : integer;
  LYear               : string;
  LChannelIndex       : integer;
  LSupportChannels  : TStringList;}
begin
  Result := False;
  try
    {AContainer.Clear;
    LFileAgent     := TFileWRPMResAgent.Create(FAppModules);
    LTempValues    := TStringList.Create;
    LSortedValues  := TStringList.Create;
    LAnnualValues  := TStringList.Create;
    LLineValues    := TStringList.Create;
    LSupportChannelValues := TStringList.Create;
    LSupportChannels  := TStringList.Create;
    try
      LFileName := GetOutputDataFile('RES.OUT');
      Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LTempValues,0);
      if Result and (LTempValues.Count > 1) then
      begin
        FDemandSupply := True;
        LChannelIndex := -1;
        LSupportChannels.CommaText := LTempValues[0];
        if (ASupportChannelNo>0) and (LSupportChannels.Count > 0) then
          LChannelIndex := LSupportChannels.IndexOf(IntToStr(ASupportChannelNo));

        // Avarage or select data for a decision month
        LDecisionMonth      := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.DecisionMonth;
        LDecisionMonthIndex := GetDecisionMonthIndex(LDecisionMonth);
        if(LDecisionMonthIndex <= 0) then
          GetAverageData(LTempValues)
        else
          GetDecisionMonthData(LTempValues,LDecisionMonthIndex);
        // Get Valued for a selected subsystem
        if(LTempValues.Count > 0) then
        begin
          LSupportChannelValues.Sorted := True;
          LSupportChannelValues.Duplicates := dupAccept;

          LSortedValues.Sorted := True;
          LSortedValues.Duplicates := dupAccept;
          LSortedValues.Assign(LTempValues);
          LTempValues.Clear;

          LRowCount := 0;
          while (LRowCount < LSortedValues.Count) do
          begin
            LLineValues.CommaText  := LSortedValues[LRowCount];
            LYear := LLineValues[0];
            LAnnualValues.Clear;
            while (LYear = LLineValues[0]) do
            begin
              LAnnualValues.Add(LSortedValues[LRowCount]);
              LRowCount := LRowCount + 1;
              if(LRowCount >= LSortedValues.Count) then Break;
              LLineValues.CommaText  := LSortedValues[LRowCount];
            end;
            // get the position
            for LIndex := 0 to High(LPositionArray) do
            begin
              LPositionArray[LIndex] := Ceil((LAnnualValues.Count-1) * Exceedence[LIndex]);
            end;
            LSupportChannelValues.Clear;
            if (ASupportChannelNo = -1) then
            begin
              for LIndex := 0 to LAnnualValues.Count-1 do
              begin
                LLineValues.CommaText  := LAnnualValues[LIndex];
                if(2 < LLineValues.Count) then
                begin
                  LSupportChannelValues.Add(LLineValues[2]);
                end;
              end;
            end
            else

            if (ASupportChannelNo = 0) then
            begin
              //use supply values
              for LIndex := 0 to LAnnualValues.Count-1 do
              begin
                LLineValues.CommaText  := LAnnualValues[LIndex];
                if(3 < LLineValues.Count) then
                begin
                  LSupportChannelValues.Add(LLineValues[3]);
                end;
              end;
            end
            else
            begin
              if (LChannelIndex > 0) then
              begin
                for LIndex := 0 to LAnnualValues.Count-1 do
                begin
                  LLineValues.CommaText  := LAnnualValues[LIndex];
                  if((LChannelIndex) < LLineValues.Count) then
                    LSupportChannelValues.Add(LLineValues[LChannelIndex]);
                end;
              end;
            end;

            LTempValues.Clear;
            LTempValues.Add(LYear);
            for LIndex := 0 to High(LPositionArray) do
            begin
              if(LPositionArray[LIndex] >= 0) and (LPositionArray[LIndex] < LSupportChannelValues.Count) then
                LTempValues.Add(FormatFloat('##0.000',StrToFloat(LSupportChannelValues[LPositionArray[LIndex]])));
            end;
            AContainer.Add(LTempValues.CommaText);
          end;
        end;

        if(AContainer.Count > 0) then
        begin
          LTempValues.Clear;
          LTempValues.Add('YEAR');
          for LIndex := 0 to High(LPositionArray) do
          begin
            LTempValues.Add(FormatFloat('##0.000',Exceedence[LIndex])+'%')
          end;
          AContainer.Insert(0,LTempValues.CommaText);
        end;
      end;
    finally
      LFileAgent.Free;
      LTempValues.Free;
      LSortedValues.Free;
      LAnnualValues.Free;
      LLineValues.Free;
      LSupportChannelValues.Free;
      LSupportChannels.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetDemandSupplyData(AElementType : TNetworkElementType;AContainer: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetDemandSupplyData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMResAgent;
  LSeqNo     : integer;}
begin
  Result := False;
  try
    {LFileAgent := TFileWRPMResAgent.Create(FAppModules);
    try
      LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
      LFileName := GetOutputDataFile('RES.OUT');
      Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AContainer,LSeqNo);
    finally
      LFileAgent.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetSubSystemPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetSubSystemPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LSeqNo     : integer;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LSubSystemData     : ISubSystem;
  LAllocList : IAllocationDefinitionsList;
  LAlloc : IAllocationDefinition;
  LIndex : integer;
  LLineData : TStringList;
  LElement : string;}
begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElement := '';
      LElementIndex := -1;
      try
        LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,LSeqNo,votReviewSubSystems);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LAllocList := TPlanningModelDataObject(FAppModules.Model.ModelData).AllocationDefinitionsList;
            LLineData.CommaText := LElementContainer[0];
            if LAllocList <> nil then
            begin
              LSubSystemData := nil;
              for LIndex := 0 to LAllocList.AllocationDefinitionCount-1 do
              begin
                LAlloc := LAllocList.AllocationDefinitionByIndex[LIndex];
                if LAlloc <> nil then
                begin
                  LSubSystemData := LAlloc.SubSystemByID[AIdentifier];
                  if LSubSystemData <> nil then
                    Break;
                end;
              end;
              if (LSubSystemData <> nil) then
              begin
                LElement := LSubSystemData.Name;
                for LIndex := 0 to LLineData.Count-1 do
                begin
                  if (UpperCase(Trim(Copy(LElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                  begin
                    LElementIndex := LIndex;
                    Break;
                  end;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AData,LSeqNo,votReviewSubSystems,LElementIndex,pltDetails);
          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetSubSystemBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetSubSystemBoxPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LSubSystemData     : ISubSystem;
  LAllocList : IAllocationDefinitionsList;
  LAlloc : IAllocationDefinition;
  LIndex : integer;
  LLineData : TStringList;
  LElement : string;}
begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElement := '';
      LElementIndex := -1;
      try
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,0,votReviewSubSystems);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LAllocList := TPlanningModelDataObject(FAppModules.Model.ModelData).AllocationDefinitionsList;
            LLineData.CommaText := LElementContainer[0];
            if LAllocList <> nil then
            begin
              LSubSystemData := nil;
              for LIndex := 0 to LAllocList.AllocationDefinitionCount-1 do
              begin
                LAlloc := LAllocList.AllocationDefinitionByIndex[LIndex];
                if LAlloc <> nil then
                begin
                  LSubSystemData := LAlloc.SubSystemByID[AIdentifier];
                  if LSubSystemData <> nil then
                    Break;
                end;
              end;
              if (LSubSystemData <> nil) then
              begin
                LElement := LSubSystemData.Name;
                for LIndex := 0 to LLineData.Count-1 do
                begin
                  if (UpperCase(Trim(Copy(LElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                  begin
                    LElementIndex := LIndex;
                    Break;
                  end;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AData,0,votReviewSubSystems,LElementIndex,pltDetails);
            if(AData.Count>0) then
              DoBoxPlot(AData,True,False);
          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TWRPMPostProcessorData.GetReservoirPltData(AElementType : TNetworkElementType;AIdentifier : integer;var AElement : string;AData: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetReservoirPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LSeqNo     : integer;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LReservoirData     : IReservoirData;
  LReservoirDataList : IReservoirDataList;
  LIndex : integer;
  LLineData : TStringList; }
begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      AElement := '';
      LElementIndex := -1;
      try
        LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,LSeqNo,etReservoir);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LReservoirDataList := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
            LReservoirData := LReservoirDataList.ReservoirOrNodeByIdentifier[AIdentifier];
            if (LReservoirData <> nil) then
            begin
              LLineData.CommaText := LElementContainer[1];
              AElement := LReservoirData.ReservoirConfigurationData.ReservoirName;
              for LIndex := 0 to LLineData.Count-1 do
              begin
                if (UpperCase(Trim(Copy(AElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                begin
                  LElementIndex := LIndex;
                  Break;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AData,LSeqNo,etReservoir,LElementIndex,pltDetails);
          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetReservoirBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;AData: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetReservoirBoxPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LReservoirData     : IReservoirData;
  LReservoirDataList : IReservoirDataList;
  LIndex : integer;
  LLineData : TStringList;
  LElement : string;}
begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElementIndex := -1;
      try
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,0,etReservoir);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LReservoirDataList := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
            LReservoirData := LReservoirDataList.ReservoirOrNodeByIdentifier[AIdentifier];
            if (LReservoirData <> nil) then
            begin
              LLineData.CommaText := LElementContainer[1];
              LElement := LReservoirData.ReservoirConfigurationData.ReservoirName;
              for LIndex := 0 to LLineData.Count-1 do
              begin
                if (UpperCase(Trim(Copy(LElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                begin
                  LElementIndex := LIndex;
                  Break;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AData,0,etReservoir,LElementIndex,pltDetails);
            if(AData.Count>0) then
              DoBoxPlot(AData,True,False);

          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TWRPMPostProcessorData.GetSupplyPltData(AElementType : TNetworkElementType;AIdentifier : integer;ASupplyData: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetSupplyPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LSeqNo     : integer;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LCannel :IGeneralFlowChannel;
  LFeature : IMasterControlFeature;
  LIndex : integer;
  LLineData : TStringList;
  LElement : string;}
begin
  Result := False;
  try
    {if (ASupplyData <> nil)  then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElement := '';
      LElementIndex := -1;
      try
        LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,LSeqNo,etSupply);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LCannel := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AIdentifier];
            if LCannel <> nil then
              LFeature := LCannel.MasterControlFeature;
            if (LFeature <> nil) then
            begin
              LLineData.CommaText := LElementContainer[2];
              LElement := LFeature.Channel.ChannelName;
              for LIndex := 0 to LLineData.Count-1 do
              begin
                if (UpperCase(Trim(Copy(LElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                begin
                  LElementIndex := LIndex;
                  Break;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,ASupplyData,LSeqNo,etSupply,LElementIndex,pltDetails);
          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetSupplyBoxPltData(AElementType : TNetworkElementType;AIdentifier : integer;ASupplyData: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetSupplyBoxPltData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPlotAgent;
  LElementContainer,
  LSummaryContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LCannel :IGeneralFlowChannel;
  LFeature : IMasterControlFeature;
  LIndex : integer;
  LLineData : TStringList;
  LElement : string;
  LTimeStep : TOutputTimeStep;}
begin
  Result := False;
  try
    {if (ASupplyData <> nil)then
    begin
      LFileAgent := TFileWRPMPlotAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LSummaryContainer:= TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElement := '';
      LElementIndex := -1;
      try
        LFileName := GetOutputDataFile('PLT.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,0,etSupply);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LCannel := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AIdentifier];
            if LCannel <> nil then
              LFeature := LCannel.MasterControlFeature;
            if (LFeature <> nil) then
            begin
              LLineData.CommaText := LElementContainer[2];
              LElement := LFeature.Channel.ChannelName;
              for LIndex := 0 to LLineData.Count-1 do
              begin
                if (UpperCase(Trim(Copy(LElement,1,30))) = UpperCase(Trim(LLineData[LIndex]))) then
                begin
                  LElementIndex := LIndex;
                  Break;
                end;
              end;
            end;
            if LElementIndex >=0 then
            begin
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,ASupplyData,0,etSupply,LElementIndex,pltDetails);
              if(ASupplyData.Count>0) then
              begin
                LTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
                if (LTimeStep = otsAnnual) then
                begin
                  if CaptureAnnualData(ASupplyData) then
                    DoBoxPlot(ASupplyData,False,True);
                end
                else
                  DoBoxPlot(ASupplyData,True,True);
              end;
            end;
          end;
        end;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LSummaryContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetOutputDataFile(AFilePreFix : string): TAbstractModelFileName;
const OPNAME = 'TWRPMPostProcessorData.GetOutputDataFile';
{var
  LIndex : integer;
  LFileName : string;
  LFileNames : TModelFileNames;
  LFileNameObject : TFileNamesList;}
begin
  Result := nil;
  try
    {LFileNames := TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
    LFileNameObject := LFileNames.CastOutputFileNames;
    for LIndex := 0 to LFileNameObject.FilesCount-1 do
    begin
      LFileName := UpperCase(ExtractFileName(LFileNameObject.FileNameObject[LIndex].FileName));
      if (Pos(AFilePreFix,LFileName) > 0) then
      begin
        Result := LFileNameObject.FileNameObject[LIndex];
        Break;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetDecisionMonthIndex(ADecisionMonth: integer): integer;
const OPNAME = 'TWRPMPostProcessorData.GetDecisionMonthIndex';
{var
  LIndex : integer;
  LConfigurationData: IRunConfigurationData;}
begin
  Result := 0;
  try
    {LConfigurationData := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    for LIndex := 1 to LConfigurationData.NrOfDecisionMonths do
    begin
      if(ADecisionMonth = LConfigurationData.DecisionMonthByIndex[LIndex]) then
      begin
        Result := LIndex;
        Break;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetAverageData(AContainer: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetAverageData';
{var
  LRowCount : integer;
  LIndex : integer;
  LConfigurationData: IRunConfigurationData;
  LAnnualValues : TStringList;
  LNewValues    : TStringList;
  LYearValues   : TStringList;
  LYear         : string;}
begin
  Result := false;
  try
    {LAnnualValues := TStringList.Create;
    LNewValues    := TStringList.Create;
    LYearValues   := TStringList.Create;
    try
      LConfigurationData := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LYearValues.CommaText := AContainer[0];
      LYear := LYearValues[0];
      LRowCount := 1;
      while (LRowCount < AContainer.Count) and (LConfigurationData.NrOfDecisionMonths <> 0) do
      begin
        LAnnualValues.Clear;
        LYearValues.CommaText := AContainer[LRowCount];
        LYear := LYearValues[0];
        LIndex := 1;
        while (LYear = LYearValues[0]) and (LIndex <= LConfigurationData.NrOfDecisionMonths) do
        begin
          LAnnualValues.Add(AContainer.Strings[LRowCount]);
          LIndex := LIndex +1;
          LRowCount := LRowCount + 1;
          if(LRowCount >= AContainer.Count) then Break;
          LYearValues.CommaText := AContainer[LRowCount];
        end;
        LNewValues.Add(AddDecisionData(LAnnualValues));
      end;
      AContainer.Assign(LNewValues);
      Result := True;
    finally
      LAnnualValues.Free;
      LNewValues.Free;
      LYearValues.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetDecisionMonthData(AContainer: TStrings; ADecisionMonthIndex: integer): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetDecisionMonthData';
{var
  LIndex : integer;
  LConfigurationData: IRunConfigurationData;
  LNewValues    : TStringList;
  LDemandSupply : TStringList;}
begin
  Result := false;
  try
    {LNewValues    := TStringList.Create;
    LDemandSupply := TStringList.Create;
    try
      LConfigurationData := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
      LIndex := ADecisionMonthIndex;
      if(LIndex < 1) then LIndex := 1;
      while (LIndex < AContainer.Count) do
      begin
        if FDemandSupply then
        begin
          LDemandSupply.CommaText := AContainer.Strings[LIndex];
          if (LDemandSupply[3] < LDemandSupply[1]) then
            LDemandSupply[3] := LDemandSupply[1];
          LNewValues.Add(LDemandSupply.CommaText);
        end
        else
          LNewValues.Add(AContainer.Strings[LIndex]);
        LIndex  := LIndex + LConfigurationData.NrOfDecisionMonths;
      end;
      AContainer.Assign(LNewValues);
      Result := True;
    finally
      LNewValues.Free;
      LDemandSupply.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TWRPMPostProcessorData.AddDecisionData(AContainer: TStrings): string;
const OPNAME = 'TWRPMPostProcessorData.AddDecisionData';
{var
  LLineValue   : TStringList;
  LValues      : TStringList;
  LTotalValue  : TStringList;
  LIndex       : integer;
  LCount       : integer;
  LYear        : string;
  LCounter     : integer;}
begin
  Result := '';
  try
    {LValues      := TStringList.Create;
    LTotalValue  := TStringList.Create;
    LLineValue   := TStringList.Create;
    try
      LIndex := 0;
      while LIndex < AContainer.Count do
      begin
        LValues.CommaText := AContainer[LIndex];
        LYear := LValues[0];
        if FDemandSupply then
          LCounter := LValues.Count-1
        else
          LCounter := LValues.Count-2;
        for LCount := 1 to LCounter do
        begin
          if (FDemandSupply) and (LCount = 2) and (LValues[LCount] < LValues[1]) then
            LLineValue.Add(FormatFloat('0000.00000',StrToFloat(LValues[1])))
          else
            LLineValue.Add(FormatFloat('0000.00000',StrToFloat(LValues[LCount])));
        end;
        if LIndex = 0 then
          LTotalValue.AddStrings(LLineValue)
        else
        begin
          for LCount := 0 to LLineValue.Count-1 do
            LTotalValue[LCount] := FormatFloat('0000.00000',STrToFloat(LTotalValue[LCount])+STrToFloat(LLineValue[LCount]));
        end;
        LLineValue.Clear;
        LIndex := LIndex + 1;
      end;
      for LCount := 0 to LTotalValue.Count-1 do
        LTotalValue[LCount] := FormatFloat('0000.00000',StrToFloat(LTotalValue[LCount])/AContainer.Count);
      LTotalValue.Insert(0,LYear);
      LTotalValue.Add('AVG');
      Result := LTotalValue.CommaText;
    finally
      LValues.Free;
      LTotalValue.Free;
      LLineValue.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TWRPMPostProcessorData.GetPltDataByIndex(AElementIndex : integer;ADetailContainer,AData: TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetPltDataByIndex';
var
  LCount : integer;
  LLineData : TStringList;
  LOutData : TStringList;
  LDataIndex : integer;
begin
  Result := False;
  try
    LLineData := TStringList.Create;
    LOutData := TStringList.Create;
    try
      LDataIndex := 0;
      while LDataIndex<ADetailContainer.Count do
      begin
        LLineData.CommaText := ADetailContainer[LDataIndex];
        for LCount := 0 to LLineData.Count-1 do
        begin
          LOutData.CommaText := LLineData[LCount];
          if AElementIndex<LOutData.Count then
            AData.Add(LOutData[AElementIndex]);
        end;
        LDataIndex := LDataIndex + 1;
      end;
      Result := True;
    finally
      FreeAndNil(LLineData);
      FreeAndNil(LOutData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}

function TWRPMPostProcessorData.CaptureAnnualData(AData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.CaptureAnnualData';
{CDays : array[1..12] of double = (31.0,30.0,31.0,31.0,30.0,31.0,30.0,31.0,31.0,28.25,31.0,30.0);
var
  LIndex : integer;
  LModelData : TPlanningModelDataObject;
  LTimePeriods : integer;
  LCount : integer;
  LTempValues : TStringList;
  LAnnualValues : TStringList;
  LLineNo : integer;
  LMonthValue : double;
  LDaysInMonth : double;
  LStartMonth : integer;
  LCurrentValue : double;
  LValue : double;
  LSeqNo : integer;
  LSeq : integer;}
begin
  Result := False;
  try
    {if Assigned(AData) then
    begin
      LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
      LTimePeriods := LModelData.GetRunConfigurationData.Get_PeriodsInAnalysis;
      LSeqNo := LModelData.GetRunConfigurationData.Get_NumberOfSequencesInAnalysis;
      LTempValues := TStringList.Create;
      LAnnualValues := TStringList.Create;
      try
        LStartMonth := LModelData.RunConfigurationData.StartMonthNumber-5;
        if(LStartMonth < 0) then
          LStartMonth := 0;
        LLineNo := 0;
        for LSeq := 1 to LSeqNo do
        begin
          LMonthValue := 0;
          LTempValues.Clear;
          for LCount := 1 to Trunc(LTimePeriods/12) do
            LTempValues.Add(FormatFloat('00000.0000',LMonthValue));
          LCount := 0;
          while LLineNo<AData.Count do
          begin
            LValue := 0;
            for LIndex := 1 to 12 do
            begin
              LStartMonth := LStartMonth+1;
              if(LStartMonth > 12) then
                LStartMonth := 1;
              LDaysInMonth := CDays[LStartMonth];
              if LLineNo<AData.Count then
              begin
                LCurrentValue := StrToFloat(AData[LLineNo]);
                LValue := LValue+LCurrentValue*LDaysInMonth*24*36/10000;
                LTempValues[LCount] := FormatFloat('00000.0000',LValue);
                inc(LLineNo);
              end
              else Break;
            end;
            LAnnualValues.Add(LTempValues[LCount]);
            LCount := LCount+1;
            if LCount>=Trunc(LTimePeriods/12) then
              Break;
          end;
        end;
        AData.Clear;
        AData.Assign(LAnnualValues);
        Result := True;
      finally
        FreeAndNil(LTempValues);
        FreeAndNil(LAnnualValues);
      end;
    end;
    Result := True; }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRPMPostProcessorData.DoBoxPlot(AData : TStrings;AMonthly : boolean;ASupply : boolean);
const OPNAME = 'TWRPMPostProcessorData.DoBoxPlot';
{var
  LIndex : integer;
  LModelData : TPlanningModelDataObject;
  LPositionArray : array[1..13] of integer;
  LNoSeq : integer;
  LCount : integer;
  LNoOfYearsToSkip : integer;
  LLineNo,
  LStartYear,
  LTimePeriods : integer;
  LAnnualValues : TStringList;
  LTempValues : TStringList;
  LSortedValues : TStringList;
  LCumValues : TStringList;
  LLineData : TStringList;
  LValue : double;
  LTimeStepperLabel : string;
  LCondencedPlot : boolean;
  LPlotOptions : TOutputPlotOptions;
  LCumIndex : integer; }
begin
  try
    {LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
    LTimeStepperLabel := FAppModules.Model.ModelName + '_TimeStepper';
    LAnnualValues := TStringList.Create;
    LTempValues := TStringList.Create;
    LSortedValues := TStringList.Create;
    LLineData := TStringList.Create;
    LCumValues := TStringList.Create;
    try
      if (AData.Count > 0) then
      begin
        LTimePeriods := LModelData.GetRunConfigurationData.Get_PeriodsInAnalysis;
        LStartYear := LModelData.GetRunConfigurationData.Get_StartYearOther;
        LNoSeq := LModelData.GetRunConfigurationData.Get_NumberOfSequencesInAnalysis;
        LNoOfYearsToSkip := FAppModules.ViewIni.ReadInteger(LTimeStepperLabel,'YearsToSkip',0);
        LPlotOptions := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.PlotOption;

        LCondencedPlot := (LPlotOptions = poCondenced);
        if AMonthly then
        begin
          if (not (LCondencedPlot) and (ASupply)) or not (ASupply) then
          begin
            LCount := LNoSeq-1;
            for LIndex := 1 to LTimePeriods do
              LAnnualValues.Add(FormatFloat('####0.0000',LStartYear+LIndex/12.0));
            if LCount > 0 then
              for LIndex := 1 to High(LPositionArray) do
              begin
                case ElementType of
                votReviewMonthlyChannelResult:
                  LPositionArray[LIndex] := Ceil((LCount)*LChannelExceedence[LIndex]);
                else
                  LPositionArray[LIndex] := Ceil((LCount)*LRExceedence[LIndex]);
                end;
              end;
          end
          else
          if (LCondencedPlot) and (ASupply) then
          begin
            LCount := LNoSeq*Trunc(LTimePeriods/12)-LNoOfYearsToSkip-1;
            for LIndex := 1 to 12 do
              LAnnualValues.Add(FormatFloat('####0.0000',LIndex));
            if LCount > 0 then
              for LIndex := 1 to High(LPositionArray) do
              begin
                case ElementType of
                votReviewMonthlyChannelResult:
                  LPositionArray[LIndex] := Ceil((LCount)*LChannelExceedence[LIndex]);
                else
                  LPositionArray[LIndex] := Ceil((LCount)*LRExceedence[LIndex]);
                end;
              end;
          end;
          for LCount := 0 to LAnnualValues.Count-1 do
          begin
            LLineNo := LCount;
            LTempValues.Clear;

            while LLineNo<=AData.Count-1 do
            begin
              if LLineNo<=AData.Count-1 then
              begin
                LValue := StrToFloat(AData[LLineNo]);
                LTempValues.Add(FormatFloat('00000.0000',LValue));
              end;
              LLineNo:=LLineNo+LAnnualValues.Count;
            end;

            LSortedValues.Sorted := True;
            LSortedValues.Duplicates := dupAccept;
            LSortedValues.Clear;
            LSortedValues.Assign(LTempValues);
            LTempValues.Clear;
            LTempValues.Add(LAnnualValues[LCount]);
            for LIndex := 1 to High(LPositionArray) do
            begin
              if(LPositionArray[LIndex] >= 0) and (LPositionArray[LIndex]<LSortedValues.Count) then
              begin
                LValue := StrToFloat(LSortedValues[LPositionArray[LIndex]]);
                LTempValues.Add(FormatFloat('####0.0000',LValue));
              end;
            end;
            LLineData.Add(LTempValues.CommaText);
          end;
        end
        else
        if not AMonthly then
        begin
          LCount := LNoSeq-1;
          for LIndex := 0 to Trunc(LTimePeriods/12)-1 do
            LAnnualValues.Add(FormatFloat('####0.0000',LStartYear+LIndex));
          if LCount > 0 then
            for LIndex := 1 to High(LPositionArray) do
            begin
              case ElementType of
              votReviewMonthlyChannelResult:
                LPositionArray[LIndex] := Ceil((LCount)*LChannelExceedence[LIndex]);
              else
                LPositionArray[LIndex] := Ceil((LCount)*LRExceedence[LIndex]);
              end;
            end;

          for LCount := 0 to LAnnualValues.Count-1 do
          begin
            LLineNo := LCount;
            LTempValues.Clear;

            while LLineNo<=AData.Count-1 do
            begin
              if LLineNo<=AData.Count-1 then
              begin
                LValue := StrToFloat(AData[LLineNo]);
                LTempValues.Add(FormatFloat('00000.0000',LValue));
              end;
              LLineNo:=LLineNo+(Trunc(LTimePeriods/12));
            end;

            if (LPlotOptions = poCumulative) and not (ASupply) then
            begin
              if LCumValues.Count=0 then
                LCumValues.AddStrings(LTempValues)
              else
              begin
                if LCumValues.Count>0 then
                begin
                  for LCumIndex := 0 to LCumValues.Count-1 do
                  begin
                    LCumValues[LCumIndex] := FormatFloat('00000.0000',StrToFloat(LCumValues[LCumIndex])+
                                             StrToFloat(LTempValues[LCumIndex]));
                  end;
                end;
              end;
              LSortedValues.Sorted := True;
              LSortedValues.Duplicates := dupAccept;
              LSortedValues.Clear;
              LSortedValues.Assign(LCumValues);
            end
            else
            begin
              LSortedValues.Sorted := True;
              LSortedValues.Duplicates := dupAccept;
              LSortedValues.Clear;
              LSortedValues.Assign(LTempValues);
            end;
            LTempValues.Clear;
            LTempValues.Add(LAnnualValues[LCount]);
            for LIndex := 1 to High(LPositionArray) do
            begin
              if(LPositionArray[LIndex] >= 0) and (LPositionArray[LIndex]<LSortedValues.Count) then
              begin
                LValue := StrToFloat(LSortedValues[LPositionArray[LIndex]]);
                LTempValues.Add(FormatFloat('####0.0000',LValue));
              end;
            end;
            LLineData.Add(LTempValues.CommaText);
          end;
        end;
      end;
      AData.Clear;
      AData.Assign(LLineData);
      if(AData.Count > 0) then
      begin
        LTempValues.Clear;
        LTempValues.Add('YEAR');
        for LIndex := 1 to High(LPositionArray) do
        begin
          case ElementType of
          votReviewMonthlyChannelResult:
          LTempValues.Add(FormatFloat('##0.000',LChannelExceedence[LIndex])+'%')
          else
            LTempValues.Add(FormatFloat('##0.000',LRExceedence[LIndex])+'%')
          end;
        end;
        AData.Insert(0,LTempValues.CommaText);
      end;
    finally
      FreeAndNil(LTempValues);
      FreeAndNil(LAnnualValues);
      FreeAndNil(LSortedValues);
      FreeandNil(LLineData);
      FreeAndNil(LCumValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetChannelPmpData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetChannelPmpData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPmpAgent;
  LSeqNo     : integer;
  LElementContainer,
  LDetailContainer : TStringList;
  LElementIndex : integer;
  LChannel     : IGeneralFlowChannel;
  LIndex : integer;
  LLineData : TStringList;
  LElement : integer;
  LPos     : integer;
  LLeftChar,LRightChar,
  LChannelNumber  : string; }

begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPmpAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LDetailContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElementIndex := -1;
      try
        LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
        LFileName := GetOutputDataFile('PMP.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,LSeqNo);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AIdentifier];
            if (LChannel <> nil) then
            begin
              LLineData.CommaText := LElementContainer[0];
              LElement := LChannel.ChannelNumber;

              for LIndex := 0 to LLineData.Count-1 do
              begin
                LPos := Pos(IntToStr(LElement),LLineData[LIndex]);
                if (LPos = 1) then
                  LLeftChar := Copy(LLineData[LIndex],LPos-1,0)
                else
                  LLeftChar := Copy(LLineData[LIndex],LPos-1,1);
                LRightChar := Copy(LLineData[LIndex],Length(IntToStr(LElement))+1,1);
                if(LPos > 0) and ((LLeftChar = '-') or (LLeftChar = ' ') or (LLeftChar = '')) and
                  ((LRightChar = '-')or(LRightChar = ' ') or (LRightChar = '')) then
                begin
                  LChannelNumber := Trim(Copy(LLineData[LIndex],LPos,Length(IntToStr(LElement))));
                  if(LElement = StrToIntDef(LChannelNumber,0)) then
                  begin
                    LElementIndex := LIndex;
                    Break;
                  end;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LDetailContainer,LSeqNo,LElementIndex,rtDetails);
            if LDetailContainer.Count>0 then
            begin
              AData.Clear;
              AData.Assign(LDetailContainer);
            end;
          end;
        end;
        Result := True;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LDetailContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TWRPMPostProcessorData.GetChannelPmpBoxPlotData(AElementType : TNetworkElementType;AIdentifier : integer;AData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetChannelPmpBoxPlotData';
{var
  LFileName  : TAbstractModelFileName;
  LFileAgent : TFileWRPMPmpAgent;
  LSeqNo     : integer;
  LElementContainer : TStringList;
  LElementIndex : integer;
  LChannel     : IGeneralFlowChannel;
  LIndex : integer;
  LLineData : TStringList;
  LElement : integer;
  LTimeStep : TOutputTimeStep;
  LPos      : integer;
  LLeftChar,LRightChar,
  LChannelNumber : string;}
begin
  Result := False;
  try
    {if AData <> nil then
    begin
      LFileAgent := TFileWRPMPmpAgent.Create(FAppModules);
      LElementContainer := TStringList.Create;
      LLineData := TStringList.Create;
      LElementIndex := -1;
      try
        LSeqNo := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Sequence;
        LTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
        LFileName := GetOutputDataFile('PMP.OUT');
        if (AIdentifier >= 0) and (LFileName <> nil) then
        begin
          Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,LElementContainer,LSeqNo);
          if (Result) and (LElementContainer.Count>0) then
          begin
            LChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AIdentifier];
            if (LChannel <> nil) then
            begin
              LLineData.CommaText := LElementContainer[0];
              LElement := LChannel.ChannelNumber;
              for LIndex := 0 to LLineData.Count-1 do
              begin
                LPos := Pos(IntToStr(LElement),LLineData[LIndex]);
                if (LPos = 1) then
                  LLeftChar := Copy(LLineData[LIndex],LPos-1,0)
                else
                  LLeftChar := Copy(LLineData[LIndex],LPos-1,1);
                LRightChar := Copy(LLineData[LIndex],Length(IntToStr(LElement))+1,1);
                if(LPos > 0) and ((LLeftChar = '-') or (LLeftChar = ' ') or (LLeftChar = '')) and
                  ((LRightChar = '-')or(LRightChar = ' ') or (LRightChar = '')) then
                begin
                  LChannelNumber := Trim(Copy(LLineData[LIndex],LPos,Length(IntToStr(LElement))));
                  if(LElement = StrToIntDef(LChannelNumber,0)) then
                  begin
                    LElementIndex := LIndex;
                    Break;
                  end;
                end;
              end;
            end;
            if LElementIndex >=0 then
              Result := LFileAgent.ReadDataSequenceDataFromFile(LFileName,AData,0,LElementIndex,rtDetails);
            if AData.Count>0 then
            begin
              if LTimeStep = otsMonthly then
                DoBoxPlot(AData,True,False)
              else
              begin
              if CaptureAnnualData(AData) then
                DoBoxPlot(AData,False,False);
              end;
            end;
          end;
        end;
        Result := True;
      finally
        LFileAgent.Free;
        LElementContainer.Free;
        LLineData.Free;
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;
//________________________________Start New Functions___________________________________________________________

function TWRPMPostProcessorData.GetPltFileElementData(AContainer: TStrings; AElementNumber: integer;
          AElementName : string; AElementType: TNetworkElementType): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetPltFileElementData';
var
  LDataType       : TOutputDataType;
  LOutputTimeStep : TOutputTimeStep;
  LElementName    : string;
begin
  Result := False;
  try
    LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
    LDataType       := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.ValueType;


    if(Populating) and (AElementNumber = FElementNumber) and (AElementName = FElementName) and
      (AElementType = FElementType) and (LDataType = FDataType) then
    begin
      if(AElementType = votMasterControl) and (LOutputTimeStep = otsAnnual) then
        CalculateAnnualChannelFlowData(FElementData,AContainer)
      else
        AContainer.Assign(FElementData);
      Result := True;
    end
    else
    begin
      if(AElementName =  '') then
        LElementName  := GetElementName(AElementNumber,AElementType)
      else
        LElementName  := AElementName;

      FWRPMOutputSettings.StartYear    := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
      {FWRPMOutputSettings.StartMonth   := 12 - FAppModules.StudyArea.CalendarStartMonth;
      if(FWRPMOutputSettings.StartMonth < 1) or (FWRPMOutputSettings.StartMonth > 12) then
        FWRPMOutputSettings.StartMonth   := 1;}
      FWRPMOutputSettings.StartMonth   := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber;
      {if(FWRPMOutputSettings.StartMonth > 1) then
        FWRPMOutputSettings.StartMonth := 14 - FWRPMOutputSettings.StartMonth;}
      FWRPMOutputSettings.ElementID    := AElementNumber;
      FWRPMOutputSettings.ElementType  := AElementType;
      FWRPMOutputSettings.DataType     := LDataType;
      FWRPMOutputSettings.OutputTimeStep  := LOutputTimeStep;
      FWRPMOutputSettings.ElementName  := LElementName;
      FWRPMOutputSettings.PlotFileName := GetOutputPlotFileName;
      if FPltFileManager.ReadElementData(FWRPMOutputSettings,FElementData)then
      begin
        if(AElementType = votMasterControl) and (LOutputTimeStep = otsAnnual) then
          CalculateAnnualChannelFlowData(FElementData,AContainer)
        else
          AContainer.Assign(FElementData);
        FElementNumber  := AElementNumber;
        FElementName    := LElementName;
        FElementType    := AElementType;
        FDataType       := LDataType;
        FOutputTimeStep := LOutputTimeStep;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetResFileElementData(AContainer: TStrings; AElementNumber: integer;
         AElementName: string; AElementType: TNetworkElementType): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetResFileElementData';
var
  LDataType       : TOutputDataType;
  LOutputTimeStep : TOutputTimeStep;
  LFileName    : string;
begin
  Result := False;
  try
    LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
    LDataType       := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.ValueType;


    if(Populating) and (AElementNumber = FElementNumber) and (AElementName = FElementName) and
      (AElementType = FElementType) and (LDataType = FDataType) and (FOutputTimeStep = LOutputTimeStep) then
    begin
      AContainer.Assign(FElementData);
      Result := True;
    end
    else
    begin
      LFileName := IncludeTrailingPathDelimiter(TPlanningModelDataObject(FAppModules.Model.ModelData).Get_DataFilePaths.OutputFilePath)+ AElementName;
      if FResFileManager.ReadElementData(FElementData,LFileName)then
      begin
        FElementNumber  := AElementNumber;
        FElementName    := AElementName;
        FElementType    := AElementType;
        FDataType       := LDataType;
        FOutputTimeStep := LOutputTimeStep;
        AContainer.Assign(FElementData);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetInterBasinSupportFileElementData(  AContainer: TStrings; AElementNumber: integer;
         AElementName, AResFileName: string; AElementType: TNetworkElementType): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetInterBasinSupportFileElementData';
var
  LDataType       : TOutputDataType;
  LFileName    : string;
begin
  Result := False;
  try
    LDataType := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.ValueType;

    if(Populating) and (AElementNumber = FElementNumber) and (AElementName = FElementName) and
      (AElementType = FElementType) and (LDataType = FDataType) and (FResFileName = AResFileName) then
    begin
      AContainer.Assign(FElementData);
      Result := True;
    end
    else
    begin
      LFileName := IncludeTrailingPathDelimiter(TPlanningModelDataObject(FAppModules.Model.ModelData).Get_DataFilePaths.OutputFilePath)+ AResFileName;
      if FResFileManager.ReadInterBasinSupportChannelData(FElementData,LFileName,AElementNumber)then
      begin
        FElementNumber  := AElementNumber;
        FElementName    := AElementName;
        FElementType    := AElementType;
        FDataType       := LDataType;
        FResFileName    := AResFileName;
        AContainer.Assign(FElementData);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetSysFileElementData(AContainer : TStrings; AElementNumber : integer; AElementName,
         ASysFileName : string; AElementType: TNetworkElementType): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetInterBasinSupportFileElementData';
var
  LFileName           : string;
  LDecisionMonthName  : string;
  LDecisionMonthIndex : integer;
begin
  Result := False;
  try
    //You cannot select average if you have one decision month
    if(TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.NrOfDecisionMonths = 1) then
      LDecisionMonthIndex := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.DecisionMonthByIndex[1]
    else
      LDecisionMonthIndex := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.DecisionMonth;

    LDecisionMonthName  := '';
    if(LDecisionMonthIndex > 0) then
      LDecisionMonthName  := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LDecisionMonthIndex];

    if(Populating) and (AElementNumber = FElementNumber) and (AElementName = FElementName) and
      (AElementType = FElementType) and (FDecisionMonthName = LDecisionMonthName) and (FSysFileName = ASysFileName) then
    begin
      AContainer.Assign(FElementData);
      Result := True;
    end
    else
    begin
      LFileName := IncludeTrailingPathDelimiter(TPlanningModelDataObject(FAppModules.Model.ModelData).Get_DataFilePaths.OutputFilePath)+ ASysFileName;
      if FSysFileManager.ReadElementData(FElementData,LFileName,AElementName,LDecisionMonthName)then
      begin
        FElementNumber     := AElementNumber;
        FElementName       := AElementName;
        FElementType       := AElementType;
        FSysFileName       := ASysFileName;
        FDecisionMonthName := LDecisionMonthName;
        AContainer.Assign(FElementData);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetPmpFileElementData(AContainer: TStrings;AElementNumber: integer; AElementName: string;
         AElementType: TNetworkElementType): boolean;
const OPNAME = 'TWRPMPostProcessorData.GetPmpFileElementData';
var
  LStartYear,
  LMonthCount     : integer;
  LElementName    : string;
  LOutputTimeStep : TOutputTimeStep;
begin
  Result := False;
  try
    LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
    LStartYear      := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
    LMonthCount     := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.PeriodsInAnalysis;
    if(Populating) and (AElementNumber = FElementNumber) and (AElementName = FElementName) and
      (AElementType = FElementType)  then
    begin
      if(LOutputTimeStep = otsAnnual) then
        CalculateAnnualChannelFlowData(FElementData,AContainer)
      else if(LOutputTimeStep = otsMonthlyCumulative) then
        CalculateMonthlyCummlativeChannelFlowData(FElementData,AContainer,LStartYear,LStartYear+(LMonthCount div 12))
      else if(LOutputTimeStep = otsAnnualCumulative) then
        CalculateAnnualCummlativeChannelFlowData(FElementData,AContainer,LStartYear,LStartYear+(LMonthCount div 12))
      else
        AContainer.Assign(FElementData);
      Result := True;
    end
    else
    begin
      if(AElementName =  '') then
        LElementName  := GetElementName(AElementNumber,AElementType)
      else
        LElementName  := AElementName;
      FWRPMOutputSettings.StartYear    := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartYearOther;
      {FWRPMOutputSettings.StartMonth   := 12 - FAppModules.StudyArea.CalendarStartMonth;
      if(FWRPMOutputSettings.StartMonth < 1) or (FWRPMOutputSettings.StartMonth > 12) then
        FWRPMOutputSettings.StartMonth   := 1;}
      FWRPMOutputSettings.StartMonth   := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber;
      {if(FWRPMOutputSettings.StartMonth > 1) then
        FWRPMOutputSettings.StartMonth := 14 - FWRPMOutputSettings.StartMonth;}

      FWRPMOutputSettings.ElementID    := AElementNumber;
      FWRPMOutputSettings.ElementType  := AElementType;
      FWRPMOutputSettings.ElementName  := LElementName;
      FWRPMOutputSettings.PumpFileName := GetOutputPumpFileName;
      if FPmpFileManager.ReadElementData(FWRPMOutputSettings,FElementData)then
      begin
        if(LOutputTimeStep = otsAnnual) then
          CalculateAnnualChannelFlowData(FElementData,AContainer)
        else if(LOutputTimeStep = otsMonthlyCumulative) then
          CalculateMonthlyCummlativeChannelFlowData(FElementData,AContainer,LStartYear,LStartYear+(LMonthCount div 12))
        else if(LOutputTimeStep = otsAnnualCumulative) then
          CalculateAnnualCummlativeChannelFlowData(FElementData,AContainer,LStartYear,LStartYear+(LMonthCount div 12))
        else
          AContainer.Assign(FElementData);
        FElementNumber  := AElementNumber;
        FElementName    := AElementName;
        FElementType    := AElementType;
        FOutputTimeStep := LOutputTimeStep;
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.CalculateAnnualChannelFlowData(ASourceData,ADestData : TStrings): boolean;
const OPNAME = 'TWRPMPostProcessorData.CalculateAnnualChannelFlowData';
var
  LValue       : double;
  LIndex       : integer;
  LMonthIndex  : integer;
  LMonth       : integer;
  LDate        : TDateTime;
  LYearTotal   : double;
  LCurrentYear : integer;
  LYearData    : TStringList;
  LLineData    : TStringList;

begin
  Result := False;
  try
    LYearData    := TStringList.Create;
    LLineData    := TStringList.Create;
    try
      LYearTotal       := 0.0;
      LCurrentYear     := 0;
      LIndex           := 0;
      ADestData.Clear;

      while True do
      begin
        if(LIndex >= ASourceData.Count) then Break;
        for LMonthIndex := 1 to 12 do
        begin
          if(LIndex >= ASourceData.Count) then Break;
          LLineData.CommaText := ASourceData[LIndex];
          LDate               := StrToDate(LLineData[1]);
          LValue              := StrToFloat(LLineData[2]);
          LMonth              := MonthOf(LDate);
          LValue              := LValue * DaysPerMonth[LMonth] * 24.0 * 36.0 /10000.0;  //DATA(ISEQ,MON)*DAY(MP)*24*36/10000
          LYearTotal          := LYearTotal + LValue;
          if(LCurrentYear = 0) then
          begin
            LCurrentYear := YearOf(LDate);
          end;
          LIndex := LIndex + 1;
        end;

        LYearData.Clear;
        LDate := EncodeDate(LCurrentYear,12,01);
        LYearData.Add('0');
        LYearData.Add(DateToStr(LDate));
        LYearData.Add(FormatFloat('0.0000',LYearTotal));
        ADestData.Add(LYearData.CommaText);

        LYearTotal       := 0.0;
        LCurrentYear     := 0;
      end;
      Result := True;
    finally
      LYearData.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.CalculateMonthlyCummlativeChannelFlowData(ASourceData, ADestData: TStrings;
         AStartYear,AEndYear: integer): boolean;
const OPNAME = 'TWRPMPostProcessorData.CalculateMonthlyCummlativeChannelFlowData';
var
  LIndex        : integer;
  LMonth        : integer;
  LMonthIndex   : integer;
  LSequence    : integer;
  LValue       : double;
  LDate        : TDateTime;
  LCummlativeTotal    : double;
  LMonthData    : TStringList;
  LLineData    : TStringList;
begin
  Result := False;
  try
    ADestData.Clear;
    LMonthData    := TStringList.Create;
    LLineData    := TStringList.Create;
    try
      LIndex              := 0;
      while True do
      begin
        if(LIndex >= ASourceData.Count) then Break;
        LCummlativeTotal := 0.0;
        for LMonthIndex := 1 to 12 do
        begin
          if(LIndex >= ASourceData.Count) then Break;
          LLineData.CommaText := ASourceData[LIndex];
          LSequence           := StrToInt(LLineData[0]);
          LDate               := StrToDate(LLineData[1]);
          LValue              := StrToFloat(LLineData[2]);
          LMonth              := MonthOf(LDate);
          LValue              := LValue * DaysPerMonth[LMonth] * 24.0 * 36.0 /10000.0;  //DATA(ISEQ,MON)*DAY(MP)*24*36/10000
          LCummlativeTotal    := LCummlativeTotal + LValue;

          LMonthData.Clear;
          LMonthData.Add(IntToStr(LSequence));
          LMonthData.Add(DateToStr(LDate));
          LMonthData.Add(FormatFloat('0.0000',LCummlativeTotal));
          ADestData.Add(LMonthData.CommaText);

          LIndex := LIndex + 1;
        end;
      end;
      Result := True;
    finally
      LMonthData.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.CalculateAnnualCummlativeChannelFlowData(ASourceData, ADestData: TStrings;
         AStartYear,AEndYear: integer): boolean;
const OPNAME = 'TWRPMPostProcessorData.CalculateAnnualChannelFlowData';
var
  LIndex        : integer;
  LMonth        : integer;
  LMonthIndex   : integer;
  LCurrentYear : integer;
  LSequence    : integer;
  LValue       : double;
  LDate        : TDateTime;
  LCurrentYearTotal   : double;
  LCummlativeTotal    : double;
  LYearData    : TStringList;
  LLineData    : TStringList;
begin
  Result := False;
  try
    ADestData.Clear;
    LYearData    := TStringList.Create;
    LLineData    := TStringList.Create;
    try
      LIndex              := 0;
      LSequence           := 1;
      LCurrentYearTotal   := 0.0;
      LCummlativeTotal    := 0.0;
      LCurrentYear        := AStartYear;
      while True do
      begin
        if(LIndex >= ASourceData.Count) then Break;
        for LMonthIndex := 1 to 12 do
        begin
          if(LIndex >= ASourceData.Count) then Break;
          LLineData.CommaText := ASourceData[LIndex];
          LDate               := StrToDate(LLineData[1]);
          LValue              := StrToFloat(LLineData[2]);
          LMonth              := MonthOf(LDate);
          LValue              := LValue * DaysPerMonth[LMonth] * 24.0 * 36.0 /10000.0;  //DATA(ISEQ,MON)*DAY(MP)*24*36/10000
          LCurrentYearTotal   := LCurrentYearTotal + LValue;
          LIndex := LIndex + 1;
        end;

        LCummlativeTotal := LCummlativeTotal + LCurrentYearTotal;
        LCurrentYearTotal   := 0.0;

        LYearData.Clear;
        LDate := EncodeDate(LCurrentYear,12,01);
        LYearData.Add(IntToStr(LSequence));
        LYearData.Add(DateToStr(LDate));
        LYearData.Add(FormatFloat('0.0000',LCummlativeTotal));
        ADestData.Add(LYearData.CommaText);

        LCurrentYear     := LCurrentYear + 1;
        if(LCurrentYear  >= AEndYear) then
        begin
          LSequence       := LSequence + 1;
          LCurrentYear    := AStartYear;
          LCummlativeTotal    := 0.0;
        end;
      end;
      Result := True;
    finally
      LYearData.Free;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.Get_PlotFileHeaderData: TWRPMPltFileHeaderData;
const OPNAME = 'TWRPMPostProcessorData.Get_PlotFileHeaderData';
begin
  Result := nil;
  try
    Result := FPltFileManager.HeaderData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWRPMPostProcessorData.GetOutputPlotFileName: string;
const OPNAME = 'TWRPMPostProcessorData.GetOutputPlotFileName';
var
  LFileNames : TModelFileNames;
  LFileNameObject : TAbstractModelFileName;
begin
  Result := '';
  try
    LFileNames := TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
    LFileNameObject := LFileNames.GetPlotOutFile;
    if(LFileNameObject <> nil) then
      Result := LFileNameObject.FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetOutputPumpFileName: string;
const OPNAME = 'TWRPMPostProcessorData.GetOutputPumpFileName';
var
  LFileNames : TModelFileNames;
  LFileNameObject : TAbstractModelFileName;
begin
  Result := '';
  try
    LFileNames := TPlanningModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject;
    LFileNameObject := LFileNames.GetPumpOutFile;
    if(LFileNameObject <> nil) then
      Result := LFileNameObject.FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetSubSystemSysFileName(ASubSystemName: string): string;
const OPNAME = 'TWRPMPostProcessorData.GetSubSystemSysFileName';
var
  LIndex : integer;
  LIndex2 : integer;
  LSubSystem : ISubSystem;
  LFileName  : string;
  LAllocationDefinition : IAllocationDefinition;
begin
  Result := '';
  try
    if(ASubSystemName <> '') then
    begin
      for LIndex := 0 to TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.AllocationDefinitionCount-1 do
      begin
        LAllocationDefinition := TPlanningModelDataObject(FAppModules.Model.ModelData).CastAllocationDefinitionsList.AllocationDefinitionByIndex[LIndex];
        for LIndex2 := 0 to LAllocationDefinition.NrOfSubSystems-1 do
        begin
          LSubSystem := LAllocationDefinition.SubSystemByIndex[LIndex2];
          if(LSubSystem <> nil) then
          begin
            if(Trim(LSubSystem.Name) = Trim(ASubSystemName)) then
            begin
              //LFileName := IncludeTrailingPathDelimiter(TPlanningModelDataObject(FAppModules.Model.ModelData).Get_DataFilePaths.OutputFilePath);
              LFileName := LFileName + TPlanningModelDataObject(FAppModules.Model.ModelData).Get_DataFilePaths.DataFilePrefix;
              LFileName := LFileName + 'sys';
              LFileName := LFileName + FormatFloat('00',LIndex+1);
              LFileName := LFileName + '.out';
              Result    := LFileName;
              Exit;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRPMPostProcessorData.GetElementName(AElementNumber: integer; AElementType: TNetworkElementType): string;
const OPNAME = 'TWRPMPostProcessorData.GetElementName';
var
  LChannel : IGeneralFlowChannel;
  LReservoir : IReservoirData;
begin
  Result := '';
  try
    if(AElementType = votMasterControl) then
    begin
      LChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AElementNumber];
      if(LChannel <> nil) then
        Result := LChannel.ChannelName;
    end
    else if(AElementType = votReviewDamStorage) then
    begin
      LReservoir := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[AElementNumber];
      if(LReservoir <> nil) then
        Result := LReservoir.ReservoirConfigurationData.ReservoirName;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//________________________________End New Functions_____________________________________________________________

end.
