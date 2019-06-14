//
//
//  UNIT      : Contains TDDTSDataObject Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 07/04/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSDataObject;

interface
uses
  Classes,
  SysUtils,
  Contnrs,
  UAbstractModelData,
  UViewModelDataObject,
  UFilesLineTypeObject,
  UAbstractFileNamesObject,
  UFileNames,
  UDDTSData,
  UAbstractObject;


type
  TDDTSDataObject = class(TAbstractModelData)
  protected
    FDDTSDamDataList : TDDTSDamDataList;
    FDDTSOutputDataList : TDDTSOutputDataList;
    FFileNamesObject : TModelFileNames;
    FFilesLineTypes  : TFilesLineTypes;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetDDTSControlViewDataItems(AViewID : string;AItemsList : TViewModelDataItemsList): boolean;
    function GetFilesLineTypes: TAbstractFilesLineTypes; override;
    function GetFileNamesObject: TAbstractModelFileNameList; override;
  public
    procedure Reset;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function LoadModelData : boolean;
    function ClearModelData : boolean;

    function Initialise: boolean; override;
    function GetViewDataItems(AViewId : string; AItemsList : TViewModelDataItemsList; var AHandled : boolean): boolean; override;
    property CastFileNamesObject : TModelFileNames read FFileNamesObject;
    property DDTSDamDataList  : TDDTSDamDataList read FDDTSDamDataList;
    property DDTSOutputDataList  : TDDTSOutputDataList read FDDTSOutputDataList;

end;
implementation
uses
  VCL.Controls,
  VCL.Dialogs,
  UConstants,
  UDDTSDamDataLoadAgent,
  UErrorHandlingOperations;
{ TDDTSDataObject }

function TDDTSDataObject._AddRef: Integer;
const OPNAME = 'TDDTSDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDataObject._Release: Integer;
const OPNAME = 'TDDTSDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDataObject.ClearModelData: boolean;
const OPNAME = 'TDDTSDataObject.ClearModelData';
var
  LIdentifier : integer;
  LDataLoadAgent : TDDTSDamDataLoadAgent;
begin
  Result := False;
  try
    if(FDDTSDamDataList.CastReservoirList.CastReservoirByIndex[0] <> nil) then
    begin
      LIdentifier := FDDTSDamDataList.CastReservoirList.CastReservoirByIndex[0].ReservoirConfigurationData.ReservoirIdentifier;
      FDDTSDamDataList.CastReservoirList.DeleteReservoir(LIdentifier);
    end;

    if(FDDTSDamDataList.GetDDTSDetailDataByIndex(0) <> nil) then
    begin
      LIdentifier    := FDDTSDamDataList.GetDDTSDetailDataByIndex(0).Identifier;
      LDataLoadAgent := TDDTSDamDataLoadAgent.Create(FAppModules);
      try
        if LDataLoadAgent.DeleteDamDailyData(LIdentifier) then
           FDDTSDamDataList.RemoveDDTSDetailData(LIdentifier);
        FDDTSOutputDataList.Initialise;
      finally
        LDataLoadAgent.Free;
      end;
    end;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDataObject.CreateMemberObjects;
const OPNAME = 'TDDTSDataObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FFileNamesObject    := TModelFileNames.Create(FAppModules);
    FDDTSOutputDataList := TDDTSOutputDataList.Create(FAppModules);
    FFilesLineTypes     := TFilesLineTypes.Create;
    FDDTSDamDataList    := TDDTSDamDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDataObject.DestroyMemberObjects;
const OPNAME = 'TDDTSDataObject.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FFileNamesObject);
    FreeAndNil(FDDTSOutputDataList);
    FreeAndNil(FFilesLineTypes);
    FreeandNil(FDDTSDamDataList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDataObject.GetViewDataItems(AViewId: string;
  AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TDDTSDataObject.GetViewDataItems';
var
  LUpperViewId: string;
begin
  Result := False;
  try
    if (not AHandled) then
    begin
      if (Trim(AViewId) <> '') and Assigned(AItemsList) then
      begin
        LUpperViewId := UpperCase(Trim(AViewId));
        if (Pos('DDTSDAMDATA',LUpperViewId) = 1) then
          AHandled := GetDDTSControlViewDataItems(AViewId,AItemsList);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDataObject.GetDDTSControlViewDataItems(AViewID: string; AItemsList: TViewModelDataItemsList): boolean;
const OPNAME = 'TDDTSDataObject.GetDDTSControlViewDataItems';
Var
  LIndex             : integer;
  LViewModelDataItem : TViewModelDataItem;
  LDDTSDetailData    : TDDTSDetailData;
Begin
  Result := False;
  try
    AItemsList.Reset;
    AViewID := UpperCase(Trim(AViewID));
    if (Trim(AViewID) <> '') and Assigned(AItemsList) then
    begin
      for LIndex := 0 to FDDTSDamDataList.GetDDTSDetailDataCount - 1 do
      begin
        LDDTSDetailData := FDDTSDamDataList.GetDDTSDetailDataByIndex(LIndex);
        if Assigned(LDDTSDetailData) then
        begin
          LViewModelDataItem := AItemsList.AddViewModelDataItem;
          if Assigned(LViewModelDataItem) then
          begin
            LViewModelDataItem.Caption     := Trim(LDDTSDetailData.DamDescription);
            LViewModelDataItem.Weighting   := LDDTSDetailData.Identifier;
            LViewModelDataItem.ParamNames  :=  'Identifier' ;
            LViewModelDataItem.ParamValues := IntToStr(LDDTSDetailData.Identifier);
            LViewModelDataItem.DataType    := 'DDTSDAMDATA';
          end;
        end;
      end;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDataObject.Initialise: boolean;
const OPNAME = 'TDDTSDataObject.Initialise';
begin
  Result := False;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDataObject.Reset;
const OPNAME = 'TDDTSDataObject.Reset';
begin
  try
   FDDTSDamDataList.Initialise;
   FDDTSOutputDataList.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDataObject.LoadModelData: boolean;
const OPNAME = 'TDDTSDataObject.LoadModelData';
var
  LLoadAgent : TDDTSDamDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TDDTSDamDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.LoadDDTSDamDataLoadAgent(FDDTSDamDataList);
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDataObject.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDDTSDataObject.Validate';
begin
  Result := False;
  try
    Result := FDDTSDamDataList.Validate(AErrors,AContext);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDataObject.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TDDTSDataObject.GetFileNamesObject';
begin
  Result := nil;
  try
    Result := FFileNamesObject;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSDataObject.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TDDTSDataObject.GetFilesLineTypes';
begin
  Result := nil;
  try
    Result := FFilesLineTypes;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
