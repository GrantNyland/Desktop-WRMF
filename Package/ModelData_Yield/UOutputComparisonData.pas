//
//
//  UNIT      : Contains TOutputComparisonData Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 19/06/2007
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UOutputComparisonData;

interface
uses
  Classes,
  Contnrs,
  UAbstractObject,
  //UOutputComparisonLoadAgent,
  UOutputData,
  VoaimsCom_TLB;
type
  TOutputComparisonData = class(TAbstractAppObject)
  protected
    FReservoirsList      : TStringList;
    FChannelList         : TStringList;
    FOutputData          : TOutputData;
    FOutputFileName      : string;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_ChannelCount: integer;
    function Get_ChannelList: string;
    function Get_ReservoirCount: integer;
    function Get_ReservoirList: string;
    procedure Set_ChannelList(const Value: string);
    procedure Set_ReservoirList(const Value: string);
    function Get_OutputData: TOutputData;
  public
    function Initialise: Boolean; override;
    property ReservoirCount : integer read Get_ReservoirCount;
    property ChannelCount : integer read Get_ChannelCount;
    property ReservoirList : string read Get_ReservoirList write Set_ReservoirList;
    property ChannelList : string read Get_ChannelList write Set_ChannelList;
    property OutputData : TOutputData read FOutputData;
    property OutputFileName : string read FOutputFileName write FOutputFileName;
  end;

  TOutputComparisonList = class(TAbstractAppObject)
  protected
    FReservoirGrandAverageA : TStringList;
    FReservoirGrandAverageB : TStringList;
    FChannelGrandAverageA : TStringList;
    FChannelGrandAverageB : TStringList;
    FOutputComparisonData : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GrandAverageVolume(AData : TStrings) : double;

  public
    function GenerateReservoirGrandAverage : boolean;
    function GenerateChannelGrandAverage : boolean;
    function AddOutputComparisonDataFromFile(AFileName : string) : TOutputComparisonData;
    function DeleteOutputComparisonDataByFileName(AFileName : string) : boolean;
    function GetOutputComparisonDataByFileName(AFileName : string) : TOutputComparisonData;
    function GetOutputComparisonDataByIndex(AIndex : integer) : TOutputComparisonData;
    function GetOutputComparisonDataCount : integer;
    function GetReservoirsVolumeDifference(AElement : integer): Double;
    function GetChannelAvrgFlowDifference(AElement : integer): Double;
    function Initialise: Boolean; override;

end;

implementation
uses
  System.Types,
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  UConstants,
  UDataSetType,
  UOutputDataLoadAgent,
  UErrorHandlingOperations,
  UYieldModelDataObject;

{ TOutputComparisonData }

function TOutputComparisonData._AddRef: Integer;
const OPNAME = 'TOutputComparisonData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputComparisonData._Release: Integer;
const OPNAME = 'TOutputComparisonData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputComparisonData.CreateMemberObjects;
const OPNAME = 'TOutputComparisonData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FReservoirsList      := TStringList.Create;
    FChannelList         := TStringList.Create;
    FOutputData          := TOutputData.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonData.DestroyMemberObjects;
const OPNAME = 'TOutputComparisonData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirsList);
    FreeAndNil(FChannelList);
    FreeAndNil(FOutputData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TOutputComparisonData.Get_ChannelCount: integer;
const OPNAME = 'TOutputComparisonData.Get_ChannelCount';
begin
  Result := 0;
  try
    Result := FChannelList.Count;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TOutputComparisonData.Get_ChannelList: string;
const OPNAME = 'TOutputComparisonData.Get_ChannelList';
begin
  Result := '';
  try
    Result := FChannelList.CommaText;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TOutputComparisonData.Get_ReservoirCount: integer;
const OPNAME = 'TOutputComparisonData.Get_ReservoirCount';
begin
  Result := 0;
  try
    Result := FReservoirsList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonData.Get_ReservoirList: string;
const OPNAME = 'TOutputComparisonData.Get_ReservoirList';
begin
  Result := '';
  try
    Result := FReservoirsList.CommaText;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

procedure TOutputComparisonData.Set_ChannelList(const Value: string);
const OPNAME = 'TOutputComparisonData.Set_ChannelList';
begin
  try
    FChannelList.CommaText := Value;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonData.Set_ReservoirList(const Value: string);
const OPNAME = 'TOutputComparisonData.Set_ReservoirList';
begin
  try
    FReservoirsList.CommaText := Value;
  except on E: Exception do HandleError(E,OPNAME) end;
end;


function TOutputComparisonData.Get_OutputData: TOutputData;
const OPNAME = 'TOutputComparisonData.Get_OutputData';
begin
  Result := nil;
  try
    Result := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonData.Initialise: Boolean;
const OPNAME = 'TOutputComparisonData.Initialise';
begin
  Result := inherited Initialise;
  try
    FOutputData.Initialise;
    FOutputData.CastSummaryOutputData.DataSources.AddSource(sodsSumFile);
    FOutputData.CastSummaryOutputData.DataSources.CurrentSource := sodsSumFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TOutputComparisonList }

function TOutputComparisonList.AddOutputComparisonDataFromFile(AFileName: string): TOutputComparisonData;
const OPNAME = 'TOutputComparisonList.AddOutputComparisonDataFromFile';
begin
  Result := nil;
  try
    if GetOutputComparisonDataCount > 1 then
      Exit;
    Result := GetOutputComparisonDataByFileName(AFileName);
    if Result = nil then
    begin
      Result := TOutputComparisonData.Create(FAppModules);
      Result.OutputFileName := AFileName;
      FOutputComparisonData.Add(Result);
    end
    else
      MessageDlg(Format('%s file is already used',[AFileName]), mtInformation,[mbOk],0);

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonList.CreateMemberObjects;
const OPNAME = 'TOutputComparisonList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FOutputComparisonData := TObjectList.Create;
    FReservoirGrandAverageA := TStringList.Create;
    FReservoirGrandAverageA.Sorted := True;
    FReservoirGrandAverageA.Duplicates := dupAccept;

    FReservoirGrandAverageB := TStringList.Create;
    FReservoirGrandAverageB.Sorted := True;
    FReservoirGrandAverageB.Duplicates := dupAccept;

    FChannelGrandAverageA := TStringList.Create;
    FChannelGrandAverageB := TStringList.Create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonList.DeleteOutputComparisonDataByFileName(AFileName: string): boolean;
const OPNAME = 'TOutputComparisonList.DeleteOutputComparisonDataByFileName';
var
  LOutputComparisonData : TOutputComparisonData;
begin
  Result := False;
  try
    LOutputComparisonData := GetOutputComparisonDataByFileName(AFileName);
    if LOutputComparisonData <> nil then
      FOutputComparisonData.Remove(LOutputComparisonData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonList.DestroyMemberObjects;
const OPNAME = 'TOutputComparisonList.DestroyMemberObjects';
begin
  try
    FreeAndNil(FOutputComparisonData);
    FreeAndNil(FReservoirGrandAverageA);
    FreeAndNil(FReservoirGrandAverageB);
    FreeAndNil(FChannelGrandAverageA);
    FreeAndNil(FChannelGrandAverageB);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonList.GetReservoirsVolumeDifference(AElement : integer): double;
const OPNAME = 'TOutputComparisonList.GetReservoirsVolumeDifference';
var
  LIndex,
  LElementA,
  LElement : integer;
  LValueA, LValueB : double;
  LGrandAverage : TStringList;
begin
  Result := 0.0;
  try
    if FReservoirGrandAverageA.Count > 0 then
    begin
      LGrandAverage := TStringList.Create;
      try
        LGrandAverage.CommaText := FReservoirGrandAverageB.CommaText;
        for LIndex := 0 to LGrandAverage.Count-1 do
        begin
          if Pos('=',LGrandAverage[LIndex]) > 1 then
          begin
            LElementA := FReservoirGrandAverageA.IndexOfName(LGrandAverage.Names[LIndex]);
            if (LElementA >=0) then
            begin
              LElement := StrToInt(LGrandAverage.Names[LIndex]);
              if (AElement = LElement)  then
              begin
                LValueA := StrToFloat(FReservoirGrandAverageA.Values[IntToStr(LElement)]);
                LValueB := StrToFloat(LGrandAverage.Values[IntToStr(LElement)]);
                Result := LValueA-LValueB;
                Break;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(LGrandAverage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonList.GetChannelAvrgFlowDifference(AElement : integer): Double;
const OPNAME = 'TOutputComparisonList.GetChannelAvrgFlowDifference';
var
  LIndex,
  LElementA,
  LElement : integer;
  LValueA, LValueB : double;
  LGrandAverage : TStringList;
begin
  Result := 0.0;
  try
    if FReservoirGrandAverageA.Count > 0 then
    begin
      LGrandAverage := TStringList.Create;
      try
        LGrandAverage.CommaText := FChannelGrandAverageB.CommaText;
        for LIndex := 0 to LGrandAverage.Count-1 do
        begin
          if Pos('=',LGrandAverage[LIndex]) > 1 then
          begin
            LElementA := FChannelGrandAverageA.IndexOfName(LGrandAverage.Names[LIndex]);
            if (LElementA >=0) then
            begin
              LElement := StrToInt(LGrandAverage.Names[LIndex]);
              if (AElement = LElement)  then
              begin
                LValueA := StrToFloat(FChannelGrandAverageA.Values[IntToStr(LElement)]);
                LValueB := StrToFloat(LGrandAverage.Values[IntToStr(LElement)]);
                Result := LValueA-LValueB;
                Break;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(LGrandAverage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputComparisonList.GrandAverageVolume(AData : TStrings) : double;
const OPNAME = 'TOutputComparisonList.GrandAverageVolume';
var
  LIndex,
  LCount                : integer;
  LMonthlyValue         : double;
  LMonthlyValues        : TStringList;
  LAvarages             : array[0..13] of double;
  LGrandAvarage         : double;
begin
  Result := 0.0;
  try
    if (AData.Count > 0) then
    begin
      for LIndex := Low(LAvarages) to High(LAvarages) do
        LAvarages[LIndex] := 0.0;
      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do
        begin
          LMonthlyValues.CommaText := AData.Strings[LIndex];
          for LCount := 1 to 12 do
          begin
            LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
            LAvarages[LCount] := LAvarages[LCount] + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);
          end;
        end;
        LGrandAvarage := 0.00;
        for LIndex := Low(LAvarages)+1 to High(LAvarages) do
        begin
          LAvarages[LIndex] := LAvarages[LIndex] / AData.Count;
          LGrandAvarage := LGrandAvarage+ LAvarages[LIndex];
        end;
         Result := StrToFloat(FormatFloat('######0.000',(LGrandAvarage / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear)));
      finally
        LMonthlyValues.Free;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputComparisonList.GetOutputComparisonDataByFileName(AFileName: string): TOutputComparisonData;
const OPNAME = 'TOutputComparisonList.GetOutputComparisonDataByFileName';
var
  LIndex : integer;
begin
  Result := nil;   
  try
    for LIndex := 0 to FOutputComparisonData.Count-1 do
    begin
      if UpperCase(TOutputComparisonData(FOutputComparisonData.Items[LIndex]).OutputFileName) = UpperCase(AFileName) then
      begin
        Result := TOutputComparisonData(FOutputComparisonData.Items[LIndex]);
        Break;
      end;
    end;
  except on E: Exception do HandleError(E,OPNAME) end;
end;

function TOutputComparisonList.GetOutputComparisonDataByIndex(AIndex: integer): TOutputComparisonData;
const OPNAME = 'TOutputComparisonList.GetOutputComparisonDataByIndex';
begin
  Result := nil;
  try
    if (AIndex >= 0) and (AIndex <=FOutputComparisonData.Count-1) then
      Result := TOutputComparisonData(FOutputComparisonData.Items[AIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonList.GetOutputComparisonDataCount: integer;
const OPNAME = 'TOutputComparisonList.GetOutputComparisonDataCount';
begin
  Result := 0;
  try
    Result := FOutputComparisonData.Count;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonList.Initialise: Boolean;
const OPNAME = 'TOutputComparisonList.Initialise';
begin
  Result := False;
  try
    FOutputComparisonData.Clear;
    FReservoirGrandAverageA.Clear;
    FReservoirGrandAverageB.Clear;
    FChannelGrandAverageA.Clear;
    FChannelGrandAverageB.Clear;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TOutputComparisonList.GenerateReservoirGrandAverage: boolean;
const OPNAME = 'TOutputComparisonList.GenerateReservoirGrandAverage';
var
  LIndex : integer;
  LOutputComparisonData : TOutputComparisonData;
  LDataContainer,
  LReservoirs : TStringList;
  LResIndex : integer;
  LElement : integer;
  LErrors : string;
  LReservoirList : IReservoirDataList;
  LReservoirData : IReservoirData;
  LReservoir     : IReservoirConfigurationData;
begin
  Result := False;
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList;
    LDataContainer := TStringList.Create;
    LReservoirs := TStringList.Create;
    try
      FReservoirGrandAverageA.Clear;
      FReservoirGrandAverageB.Clear;
      for LIndex := 0 to FOutputComparisonData.Count-1 do
      begin
        LOutputComparisonData := GetOutputComparisonDataByIndex(LIndex);
        LReservoirs.Clear;
        if LOutputComparisonData <> nil then
        begin
          if  LReservoirList <> nil then
          begin
            for LResIndex := 0 to LReservoirList.ReservoirCount - 1 do
            begin
              LReservoirData := LReservoirList.ReservoirByIndex[LResIndex];
              if LReservoirData <> nil then
              begin
                LReservoir     := LReservoirData.ReservoirConfigurationData;
                if LReservoir <> nil then
                  LReservoirs.Add(IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
              end;
            end;
          end;

          for LResIndex := 0 to LReservoirs.Count-1 do
          begin
            LElement := StrToInt(LReservoirs[LResIndex]);
            if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
              btMonthEndReservoirVolume,LElement,LErrors) then
            begin
              if (LDataContainer.Count > 0) and (LIndex = 0) then
                FReservoirGrandAverageA.Add(IntToStr(LElement)+'='+FloatToStr(GrandAverageVolume(LDataContainer)));
              if (LDataContainer.Count > 0) and (LIndex = 1) then
                FReservoirGrandAverageB.Add(IntToStr(LElement)+'='+FloatToStr(GrandAverageVolume(LDataContainer)));
            end;
          end;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LDataContainer);
      FreeAndNil(LReservoirs);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonList.GenerateChannelGrandAverage : boolean;
const OPNAME = 'TOutputComparisonList.GenerateChannelGrandAverage';
var
  LIndex : integer;
  LOutputComparisonData : TOutputComparisonData;
  LDataContainer,
  LChannels : TStringList;
  LResIndex : integer;
  LElement : integer;
  LErrors : string;
  LChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;

begin
  Result := False;
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    LDataContainer := TStringList.Create;
    LChannels := TStringList.Create;
    try
      FChannelGrandAverageA.Clear;
      FChannelGrandAverageA.Clear;
      for LIndex := 0 to FOutputComparisonData.Count-1 do
      begin
        LOutputComparisonData := GetOutputComparisonDataByIndex(LIndex);
        LChannels.Clear;
        if LOutputComparisonData <> nil then
        begin
          if  LChannelList <> nil then
          begin

            for LResIndex := 0 to LChannelList.ChannelCount - 1 do
            begin
              LChannel := LChannelList.ChannelByIndex[LResIndex];
              if (LChannel <> nil) then
              begin
                LChannels.Add(IntToStr(LChannel.ChannelNumber));
              end;
            end;
          end;

          for LResIndex := 0 to LChannels.Count-1 do
          begin
            LElement := StrToInt(LChannels[LResIndex]);
            if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
              btMonthlyAverageChannelFlow,LElement,LErrors) then
            begin
              if (LDataContainer.Count > 0) and (LIndex = 0) then
                FChannelGrandAverageA.Add(IntToStr(LElement)+'='+FloatToStr(GrandAverageVolume(LDataContainer)));
              if (LDataContainer.Count > 0) and (LIndex = 1) then
                FChannelGrandAverageB.Add(IntToStr(LElement)+'='+FloatToStr(GrandAverageVolume(LDataContainer)));
            end;
          end;
        end;
      end;
      Result := True;
    finally
      FreeAndNil(LDataContainer);
      FreeAndNil(LChannels);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
