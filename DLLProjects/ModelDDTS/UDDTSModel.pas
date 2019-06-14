//
//
//  UNIT      : Contains TDDTSModel Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 26/08/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSModel;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  VoaimsCom_TLB,
  UDataFileObjects,
  UAbstractObject,
  UAbstractFileNamesObject,
  UDDTSOutputCSVFileAgent,
  UDDTSDailyDataObject,
  UDDTSData,
  UDDTSDataObject;
type
  TDDTSModel = class(TAbstractAppObject)
  protected
    FFSLVolume : double;
    FDSLVolume : double;
    FStartingStorageVolume : double;
    FStartingStorageArea : double;

    FConvertedRunoff : double;
    FConvertedOtherInflow : double;
    FConvertedRainfall : double;
    FConvertedEvaporation : double;
    FConvertedIncreamentalRunoff : double;
    FConvertedEWR : double;

    FTotalAvailable : array of double;
    FEvaporation  : array of double;
    FEvapSupply : double;

    FTotIn : double;

    FEWRSupplyFromDsRunoff : double;
    FEWRSupplyFromDamSpill : double;
    FEWRSupplyFromRelTarget : array of double;
    FEDamSpill : array of double;

    FRelSupply : double;
    FDsReqSupply : double;

    FStorageElevation : array of double;
    FStorageVolume : array of double;
    FAbsTarget : double;
    FAbsSupply : double;

    FRelForDsReqTarget : double;
    FRelForDsReqSupply : double;
    FConcertedDsReqScale : double;
    FConvertedAbsTargetDraftScale : double;

    FStorageSurfaceArea : array of double;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function DoComputeModel(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): Boolean;
    procedure WriteOutputDailyData(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion);

    function GetConvertedRunoff(ARunoff: double) : double;
    function GetConvertedOtherInflow(AOtherInflow: double) : double;
    function GetConvertedRainfall(AIndex : integer;ARainfall: double) : double;
    function GetConvertedEvaporation(AIndex : integer; AEvaporation: double) : double;
    function GetConvertedIncreamentalRunoff(AIncreamentalRunoff: double) : double;
    function GetConvertedEWR(AEWR: double) : double;

    function GetTotIn : double;
    function GetTotalAvailable(AIndex : integer) : double;


    function GetRelSupply(AIndex : integer) : double;
    function GetEvapSupply(AIndex : integer) : double;

    function GetStorageVolume(AIndex : integer) : double;
    function GetAbsTarget : double;
    function GetAbsSupply(AIndex: integer) : double;

    function GetRelForDsReqTarget(AIndex : integer) : double;
    function GetRelForDsReqSupply(AIndex : integer) : double;
    function GetConcertedDsReqScale : double;
    function GetConvertedAbsTargetDraftScale: double;

    function GetEWRSupplyFromDsRunoff : double;
    function GetEWRSupplyFromDamSpill(AIndex : integer) : double;
    function GetEWRSupplyFromRelTarget(AIndex : integer) : double;
    function GetDamSpill(AIndex : integer) : double;
  public
    function Initialise : boolean; override;
    function RunModel(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): Boolean;
  end;

implementation

uses
  SysConst,
  DateUtils,
  UUtilities,
  UConstants,
  UReservoirData,Math,
  UErrorHandlingOperations;

const
  CmillConversion = 0.0864;
  CDivConversion = 1000;
  CDsReqAbsTDConversion = 31.5576;
  CDsReqConversion = 52.385616;

{ TDDTSModel }

procedure TDDTSModel.CreateMemberObjects;
const OPNAME = 'TDDTSModel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSModel.DestroyMemberObjects;
const OPNAME = 'TDDTSModel.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModel.DoComputeModel(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TDDTSModel.DoComputeModel';
var
  LOutputDailyFileData  : TDDTSOutputDataList;
  LOutputData  : TDDTSOutputData;
  LIndex: Integer;
  LCombinedDailyDataObject : TCombinedDailyDataObject;
  LStop: boolean;
  LMessage : string;
  LReservoir : IReservoirData;
begin
  Result := False;
  try
    if ADataFileObjects <> nil then
    begin

      LMessage := 'Model has Started Running';
      AProgressFunction(LMessage,ptNone,LStop);
      Initialise;
      LReservoir := TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.CastReservoirList.ReservoirByIndex[0];
      LOutputDailyFileData := TDDTSDataObject(FAppModules.Model.ModelData).DDTSOutputDataList;
      LOutputDailyFileData.Initialise;
      ADataFileObjects.FOutputDataObject.Clear;
      if LReservoir <> nil then
      begin
        FStartingStorageVolume := LReservoir.GetReservoirVolumeByElevation(LReservoir.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1]);
        FStartingStorageArea := LReservoir.GetReservoirSurfaceAreaByElevation(LReservoir.ReservoirZoneElevationsData.InitialLevelsData.InitialLevelsByIndex[1]);
        FDSLVolume := LReservoir.GetReservoirVolumeByElevation(LReservoir.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
        FFSLVolume := LReservoir.GetReservoirVolumeByElevation(LReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation);
      end;
      SetLength(FTotalAvailable,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FEvaporation,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FEWRSupplyFromRelTarget,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FStorageVolume,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FStorageElevation,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FStorageSurfaceArea,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);
      SetLength(FEDamSpill,ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount);


      try

        for LIndex := 0 to ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataCount-1 do
        begin
          LCombinedDailyDataObject := ADataFileObjects.FDDTSDailyDataObject.CombinedDailyDataObjectByIndex[LIndex];
          if (LCombinedDailyDataObject <> nil) then
          begin
            FConvertedRunoff := GetConvertedRunoff(LCombinedDailyDataObject.RunoffValue);
            FConvertedOtherInflow := GetConvertedOtherInflow(LCombinedDailyDataObject.OtherInflowValue);
            FConvertedRainfall := GetConvertedRainfall(LIndex,LCombinedDailyDataObject.RainfallValue);
            FEvaporation[LIndex] := GetConvertedEvaporation(LIndex,LCombinedDailyDataObject.EvaporationValue);
            FConvertedIncreamentalRunoff := GetConvertedIncreamentalRunoff(LCombinedDailyDataObject.IncreamentalRunoffValue);
            FConvertedEWR := GetConvertedEWR(LCombinedDailyDataObject.EWRValue);
            FTotIn := GetTotIn;
            FTotalAvailable[LIndex] := GetTotalAvailable(LIndex);
            FEvapSupply := GetEvapSupply(LIndex);

            FEWRSupplyFromDsRunoff := GetEWRSupplyFromDsRunoff;


            FRelSupply := GetRelSupply(LIndex);

            FRelForDsReqTarget := GetRelForDsReqTarget(LIndex);
            FRelForDsReqSupply := GetRelForDsReqSupply(LIndex);

            FAbsTarget := GetAbsTarget;
            FAbsSupply := GetAbsSupply(LIndex);



            FStorageVolume[LIndex] := GetStorageVolume(LIndex);
            FStorageElevation[LIndex] :=  LReservoir.GetReservoirElevationByVolume(FStorageVolume[LIndex]);
            FStorageSurfaceArea[LIndex] :=  LReservoir.GetReservoirSurfaceAreaByElevation(FStorageElevation[LIndex]);
            FEDamSpill[LIndex] := GetDamSpill(LIndex);

            FEWRSupplyFromDamSpill := GetEWRSupplyFromDamSpill(LIndex);
            FEWRSupplyFromRelTarget[LIndex] := GetEWRSupplyFromRelTarget(LIndex);


            LOutputData := LOutputDailyFileData.AddOutputData;
            LOutputData.ColumnA := LCombinedDailyDataObject.DailyDate;
            LOutputData.ColumnC := LCombinedDailyDataObject.RunoffValue;
            LOutputData.ColumnD := FConvertedRunoff;
            LOutputData.ColumnE := 0;
            LOutputData.ColumnF := FConvertedOtherInflow;
            LOutputData.ColumnG := LCombinedDailyDataObject.RainfallValue;
            LOutputData.ColumnH := FConvertedRainfall;
            LOutputData.ColumnI := FTotIn;
            LOutputData.ColumnJ := FTotalAvailable[LIndex];
            LOutputData.ColumnL := LCombinedDailyDataObject.EvaporationValue;
            LOutputData.ColumnM := FEvaporation[LIndex];
            LOutputData.ColumnN := FEvapSupply;
            LOutputData.ColumnO := FEvapSupply-FEvaporation[LIndex];
            LOutputData.ColumnP := LCombinedDailyDataObject.IncreamentalRunoffValue;
            LOutputData.ColumnQ := FConvertedIncreamentalRunoff;
            LOutputData.ColumnS := LCombinedDailyDataObject.EWRValue;
            LOutputData.ColumnT := FConvertedEWR;
            LOutputData.ColumnU := FEWRSupplyFromDsRunoff;
            LOutputData.ColumnV := FEWRSupplyFromDamSpill;
            LOutputData.ColumnW := FEWRSupplyFromRelTarget[LIndex];
            LOutputData.ColumnX := FRelSupply;
            if LIndex>0 then
              LOutputData.ColumnY := FRelSupply-FEWRSupplyFromRelTarget[LIndex-1];
            LOutputData.ColumnZ := FEWRSupplyFromDsRunoff/CmillConversion;
            LOutputData.ColumnAA := FEWRSupplyFromDamSpill/CmillConversion;
            LOutputData.ColumnAB := FRelSupply/CmillConversion;
            LOutputData.ColumnAC := FRelForDsReqTarget;
            LOutputData.ColumnAD := FRelForDsReqSupply;
            LOutputData.ColumnAE := FRelForDsReqSupply-FRelForDsReqTarget;
            LOutputData.ColumnAF := FAbsTarget;
            LOutputData.ColumnAG := FAbsSupply;
            LOutputData.ColumnAH := FAbsSupply-FAbsTarget;

            LOutputData.ColumnAI := FStorageElevation[LIndex];

            LOutputData.ColumnAJ := FStorageVolume[LIndex];

            LOutputData.ColumnAK := FStorageSurfaceArea[LIndex];

            LOutputData.ColumnAL := FEDamSpill[LIndex];
            ADataFileObjects.FOutputDataObject.Add(LOutputData.SaveToCSVString);

            
          end;

        end;
        WriteOutputDailyData(ADataFileObjects,AProgressFunction);
      finally
        Finalize(FTotalAvailable);
        Finalize(FEvaporation);
        Finalize(FEWRSupplyFromRelTarget);
        Finalize(FStorageVolume);
        Finalize(FStorageSurfaceArea);
        Finalize(FStorageElevation);
      end;
      Result := True;
      LMessage := 'Model Finnished Successfully';
      AProgressFunction(LMessage,ptNone,LStop);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetAbsSupply(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetAbsTarget';
begin
  Result := NullFloat;
  try
    Result := Max(Min(FAbsTarget,FTotalAvailable[AIndex]-FEvapSupply-FDSLVolume),0)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetAbsTarget: double;
const OPNAME = 'TDDTSModel.GetAbsTarget';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := (LDDTSDetailData.TargetDraft/CDsReqAbsTDConversion)*CmillConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConcertedDsReqScale: double;
const OPNAME = 'TDDTSModel.GetConvertedRainfall';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := LDDTSDetailData.DSRequiments/CDsReqAbsTDConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedAbsTargetDraftScale: double;
const OPNAME = 'TDDTSModel.GetConvertedRainfall';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := LDDTSDetailData.TargetDraft/CDsReqAbsTDConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedEvaporation(AIndex : integer; AEvaporation: double): double;
const OPNAME = 'TDDTSModel.GetConvertedRainfall';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result := AEvaporation*FStartingStorageArea/CDivConversion
    else
      Result := AEvaporation*FStorageSurfaceArea[AIndex-1]/CDivConversion;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedEWR(AEWR: double): double;
const OPNAME = 'TDDTSModel.GetConvertedEWR';
begin
  Result := NullFloat;
  try
    Result := AEWR*CmillConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedIncreamentalRunoff(AIncreamentalRunoff: double): double;
const OPNAME = 'TDDTSModel.GetConvertedIncreamentalRunoff';
begin
  Result := NullFloat;
  try
    Result := AIncreamentalRunoff*CmillConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedOtherInflow(AOtherInflow: double): double;
const OPNAME = 'TDDTSModel.GetConvertedOtherInflow';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := (LDDTSDetailData.OtherInflowScale/100)*AOtherInflow*CmillConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedRainfall(AIndex : integer;ARainfall: double): double;
const OPNAME = 'TDDTSModel.GetConvertedRainfall';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result := ARainfall*FStartingStorageArea/CDivConversion
    else
      Result := ARainfall*FStorageSurfaceArea[AIndex-1]/CDivConversion

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetConvertedRunoff(ARunoff: double): double;
const OPNAME = 'TDDTSModel.GetConvertedRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := (LDDTSDetailData.RunoffScale/100)*ARunoff*CmillConversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetDamSpill(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetDamSpill';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result :=  (FFSLVolume+FConvertedRunoff+FConvertedOtherInflow+
                     FConvertedRainfall-FEvapSupply-FRelSupply-FRelForDsReqSupply-FAbsSupply)- FStorageVolume[AIndex]
    else
    if FStorageVolume[AIndex-1] <> NullFloat then
      Result :=  (FStorageVolume[AIndex-1]+FConvertedRunoff+FConvertedOtherInflow+
                     FConvertedRainfall-FEvapSupply-FRelSupply-FRelForDsReqSupply-FAbsSupply)- FStorageVolume[AIndex];

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetEvapSupply(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetEvapSupply';
begin
  Result := NullFloat;
  try
    Result := Min(FEvaporation[AIndex],FTotalAvailable[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetEWRSupplyFromDamSpill(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetEWRSupplyFromDamSpill';
begin
  Result := NullFloat;
  try
    Result := Min(FEDamSpill[AIndex],FConvertedEWR-FEWRSupplyFromDsRunoff);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetEWRSupplyFromDsRunoff: double;
const OPNAME = 'TDDTSModel.GetEWRSupplyFromDsRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
      Result := Min((LDDTSDetailData.EWRScale/100)*FConvertedIncreamentalRunoff,FConvertedEWR);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetEWRSupplyFromRelTarget(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetEWRSupplyFromRelTarget';
begin
  Result := NullFloat;
  try
    Result := FConvertedEWR-FEWRSupplyFromDsRunoff-FEWRSupplyFromDamSpill;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetRelForDsReqSupply(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetRelForDsReqSupply';
begin
  Result := NullFloat;
  try
    Result := Max(Min(FRelForDsReqTarget,FTotalAvailable[AIndex]-FEvapSupply-FDSLVolume),0)
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSModel.GetRelForDsReqTarget(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetEWRSupplyFromDsRunoff';
var
  LDDTSDetailData : TDDTSDetailData;
begin
  Result := NullFloat;
  try
    LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIndex(0);
    if LDDTSDetailData <> nil then
    begin
      if AIndex = 0 then
        Result := Max((GetConcertedDsReqScale*CmillConversion)-((LDDTSDetailData.DSPercRelease/100)*FConvertedIncreamentalRunoff+(LDDTSDetailData.EWRPercRelease/100)*FRelSupply),0)
      else
        Result := Max((GetConcertedDsReqScale*CmillConversion)-((LDDTSDetailData.DSPercRelease/100)*FConvertedIncreamentalRunoff+(FEDamSpill[AIndex-1]*LDDTSDetailData.SpillPercRelease/100)+(LDDTSDetailData.EWRPercRelease/100)*FRelSupply),0)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetRelSupply(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetRelSupply';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result := Max(Min(FConvertedEWR-FEWRSupplyFromDsRunoff,FTotalAvailable[AIndex]-FEvapSupply-FDSLVolume),0)
    else
      Result := Max(Min(FEWRSupplyFromRelTarget[AIndex-1],FTotalAvailable[AIndex]-FEvapSupply-FDSLVolume),0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetStorageVolume(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetStorageVolume';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result :=  Min(FStartingStorageVolume+FConvertedRunoff+FConvertedOtherInflow+
                     FConvertedRainfall-FEvapSupply-FRelSupply-FRelForDsReqSupply-FAbsSupply,FFSLVolume)
    else
    if FStorageVolume[AIndex-1] <> NullFloat then
      Result :=  Min(FStorageVolume[AIndex-1]+FConvertedRunoff+FConvertedOtherInflow+
                     FConvertedRainfall-FEvapSupply-FRelSupply-FRelForDsReqSupply-FAbsSupply,FFSLVolume)

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.GetTotalAvailable(AIndex: integer): double;
const OPNAME = 'TDDTSModel.GetTotIn';
begin
  Result := NullFloat;
  try
    if AIndex = 0 then
      Result := FStartingStorageVolume + FConvertedRunoff + FConvertedOtherInflow + FConvertedRainfall
    else
    if FTotalAvailable[AIndex-1] <> NullFloat then
      Result := FTotalAvailable[AIndex-1] + FConvertedRunoff + FConvertedOtherInflow + FConvertedRainfall;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSModel.GetTotIn: double;
const OPNAME = 'TDDTSModel.GetTotIn';
begin
  Result := NullFloat;
  try
    Result := FConvertedRunoff+FConvertedOtherInflow+FConvertedRainfall;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModel.Initialise: boolean;
const OPNAME = 'TDDTSModel.Initialise';
begin
  Result := False;
  try
    FFSLVolume := NullFloat;
    FStartingStorageVolume := NullFloat;
    FStartingStorageArea := NullFloat;
    FConvertedRunoff := NullFloat;
    FConvertedOtherInflow := NullFloat;
    FConvertedRainfall := NullFloat;
    FConvertedEvaporation := NullFloat;
    FConvertedIncreamentalRunoff := NullFloat;
    FConvertedEWR := NullFloat;


    FRelSupply := NullFloat;
    FDsReqSupply := NullFloat;
    FEvapSupply := NullFloat;
    FTotIn := NullFloat;

    FAbsTarget := NullFloat;
    FAbsSupply := NullFloat;

    FRelForDsReqTarget := NullFloat;
    FRelForDsReqSupply := NullFloat;
    FConcertedDsReqScale := NullFloat;
    FConvertedAbsTargetDraftScale := NullFloat;

    FEWRSupplyFromDsRunoff := NullFloat;
    FEWRSupplyFromDamSpill := NullFloat;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;

end;



function TDDTSModel.RunModel(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion): Boolean;
const OPNAME = 'TDDTSModel.RunModel';
begin
  Result := False;
  try
    Result := DoComputeModel(ADataFileObjects,AProgressFunction);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSModel.WriteOutputDailyData(ADataFileObjects: TDataFileObjects; AProgressFunction: TProgressUpdateFuntion);
const OPNAME = 'TDDTSModel.WriteOutputDailyData';
var
  LFileAgent : TDDTSOutputCSVFileAgent;
  LAModelFileName : TAbstractModelFileName;
begin
  try
    LAModelFileName :=  TDDTSDataObject(FAppModules.Model.ModelData).CastFileNamesObject.OutputFileNames.FileNameObject[0];
    LFileAgent := TDDTSOutputCSVFileAgent.Create(FAppModules);
    try
      LFileAgent.WriteModelDataToFile(LAModelFileName,ADataFileObjects,AProgressFunction);
    finally
      LFileAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
