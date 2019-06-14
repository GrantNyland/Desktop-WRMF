//
//
//  UNIT      : Contains TNetworkElementData Class
//  AUTHOR    : Dziedzi Ramulondi(Arivia)
//  DATE      : 27/08/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UNetworkElementData;

interface

uses
  Classes,

  //  DWAF VCL
  UAbstractObject,
  VoaimsCom_TLB,
  UReservoirData,
  UReservoirAreaGroup,
  UReservoirPenaltyStructureData,
  UChannelData;
type

  //_______________________NetworkElementData______________________________________________________________
  TNetworkElementData = class(TAbstractAppObject,INetworkElementData)
  protected
    FReservoirList: TReservoirDataList;
    FReservoirPenaltyStructureList: TReservoirPenaltyStructureList;

    FChannelList            : TChannelList;
    FChannelPenaltyList     : TChannelPenaltyList;
    FReservoirAreaGroupList : TReservoirAreaGroupList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function CreateReservoirsZone(AZoneLevel: integer; ACurrentReservoirNumber: integer = 0):IReservoirPenaltyZoneData ; safecall;
    function DeleteReservoirsZone(AZoneLevel: integer): boolean ; safecall;

    function Get_ReservoirList: IReservoirDataList; safecall;
    function Get_ReservoirPenaltyStructureList: IReservoirPenaltyList; safecall;
    function Get_ChannelList: IChannelList; safecall;
    function Get_ChannelPenaltyList : IChannelPenaltyList; safecall;
    function Get_ReservoirAreaGroupList: IReservoirAreaGroupList; safecall;
    function Validate (var AErrors : WideString; const AContext    : WideString='') : WordBool; safecall;

    property ReservoirList: IReservoirDataList read Get_ReservoirList;
    property ReservoirPenaltyStructureList: IReservoirPenaltyList read Get_ReservoirPenaltyStructureList;
    property ChannelList: IChannelList read Get_ChannelList;
    property ChannelPenaltyList: IChannelPenaltyList read Get_ChannelPenaltyList;
    property ReservoirAreaGroupList: IReservoirAreaGroupList read Get_ReservoirAreaGroupList;

    property CastChannelList                   : TChannelList                   read FChannelList;
    property CastChannelPenaltyList            : TChannelPenaltyList            read FChannelPenaltyList;
    property CastReservoirList                 : TReservoirDataList             read FReservoirList;
    property CastReservoirAreaGroupList        : TReservoirAreaGroupList        read FReservoirAreaGroupList;
    property CastReservoirPenaltyStructureList : TReservoirPenaltyStructureList read FReservoirPenaltyStructureList;
  end;

implementation

{ TNetworkElementData }

uses
  SysUtils,
  System.UITypes,
  VCL.Dialogs,
  VCL.Controls,
  UReservoirZoneDataSQLAgent,
  UErrorHandlingOperations;

procedure TNetworkElementData.CreateMemberObjects;
const OPNAME = 'TNetworkElementData.CreateMemberObjects';
begin
  inherited;
  try
    FReservoirList                 := TReservoirDataList.Create(FAppModules);
    FReservoirPenaltyStructureList := TReservoirPenaltyStructureList.Create(FAppModules);
    FChannelList                   := TChannelList.Create(FAppModules);
    FChannelPenaltyList            := TChannelPenaltyList.Create(FAppModules);
    FReservoirAreaGroupList        := TReservoirAreaGroupList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkElementData.DestroyMemberObjects;
const OPNAME = 'TNetworkElementData.DestroyMemberObjects';
begin
  try
    FreeAndNil(FReservoirList);
    FreeAndNil(FReservoirPenaltyStructureList);

    FreeAndNil(FChannelList);
    FreeAndNil(FChannelPenaltyList);

    FreeAndNil(FReservoirAreaGroupList);
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited;
end;

function TNetworkElementData._AddRef: Integer;
const OPNAME = 'TNetworkElementData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkElementData._Release: Integer;
const OPNAME = 'TNetworkElementData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkElementData.Initialise: boolean;
const OPNAME = 'TNetworkElementData.Initialise';
begin
  Result := False;
  try
    Result := FReservoirList.Initialise and
              FReservoirPenaltyStructureList.Initialise and
              FChannelList.Initialise and
              FChannelPenaltyList.Initialise and
              FReservoirAreaGroupList.Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkElementData.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FReservoirList.LanguageHasChanged and
              FReservoirPenaltyStructureList.LanguageHasChanged and
              FChannelList.LanguageHasChanged and
              FChannelPenaltyList.LanguageHasChanged and
              FReservoirAreaGroupList.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.SaveState: boolean;
const OPNAME = 'TNetworkElementData.SaveState';
begin
  Result := False;
  try
    Result := FReservoirList.SaveState and
              FReservoirPenaltyStructureList.SaveState and
              FChannelList.SaveState and
              FChannelPenaltyList.SaveState and
              FReservoirAreaGroupList.SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TNetworkElementData.StudyDataHasChanged';
begin
  Result := False;
  try
    Result := FReservoirList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) and
              FReservoirPenaltyStructureList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) and
              FChannelList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) and
              FChannelPenaltyList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue) and
              FReservoirAreaGroupList.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.StudyHasChanged: boolean;
const OPNAME = 'TNetworkElementData.StudyHasChanged';
begin
  Result := False;
  try
    Result := FReservoirList.StudyHasChanged and
              FReservoirPenaltyStructureList.StudyHasChanged and
              FChannelList.StudyHasChanged and
              FChannelPenaltyList.StudyHasChanged and
              FReservoirAreaGroupList.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.Get_ChannelList: IChannelList;
const OPNAME = 'TNetworkElementData.Get_ChannelList';
begin
  Result := nil;
  try
    Result := FChannelList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.Get_ChannelPenaltyList: IChannelPenaltyList;
const OPNAME = 'TNetworkElementData.Get_ChannelPenaltyList';
begin
  Result := nil;
  try
    Result := FChannelPenaltyList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.Get_ReservoirList: IReservoirDataList;
const OPNAME = 'TNetworkElementData.Get_ReservoirList';
begin
  Result := nil;
  try
    Result := FReservoirList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.Get_ReservoirPenaltyStructureList: IReservoirPenaltyList;
const OPNAME = 'TNetworkElementData.Get_ReservoirPenaltyStructureList';
begin
  Result := nil;
  try
    Result := FReservoirPenaltyStructureList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.CreateReservoirsZone(AZoneLevel,
         ACurrentReservoirNumber: integer): IReservoirPenaltyZoneData;
const OPNAME = 'TNetworkElementData.CreateReservoirsZone';
var
  LReservoirZoneDataSQLAgent:TReservoirZoneDataSQLAgent;
  LZoneLevel: integer;
  lFieldProperty : TAbstractFieldProperty;
begin
  Result := nil;
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('StorageZoneCount');
    if (FReservoirPenaltyStructureList.PenaltyZoneCount >= StrToInt(lFieldProperty.FieldMaximumValue)) then
      raise Exception.Create('No more than' + lFieldProperty.FieldMaximumValue +  'zones can be added.');

    if(FReservoirList.ReservoirCount > 0) then
    if(MessageDlg(FAppModules.Language.GetString('Message.NetworkElementDataCreateReservoirsZone'),
                  mtWarning,mbOKCancel,0) = mrCancel) then
      Exit;

    LZoneLevel := AZoneLevel;
    if(LZoneLevel < 1) then
      LZoneLevel := 1;

    if(FReservoirPenaltyStructureList.PenaltyZoneCount <= 0) then
      LZoneLevel := 1
    else
    if(LZoneLevel > (FReservoirPenaltyStructureList.PenaltyZoneCount-2)) then
      LZoneLevel := FReservoirPenaltyStructureList.PenaltyZoneCount-2;

    if (FReservoirPenaltyStructureList.PenaltyZoneCount > 0) then
    begin
      if(MessageDlg(FAppModules.Language.GetString('Message.AddBelowSelectedZone'),
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        LZoneLevel := LZoneLevel +1;
    end;

    if (LZoneLevel < 1) or ((FReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount > 0) and
    (LZoneLevel > FReservoirPenaltyStructureList.ReservoirPenaltyCounts.StorageZoneCount-2))then
    begin
      if(MessageDlg(FAppModules.Language.GetString('Message.CanOnlyAddAboveSelectedZone'),
                 mtInformation, [mbOK, mbCancel], 0) = mrCancel) then
        Exit
      else
        LZoneLevel := LZoneLevel -1;
    end;

    LReservoirZoneDataSQLAgent := TReservoirZoneDataSQLAgent.Create(FAppModules);
    try
      Result := LReservoirZoneDataSQLAgent.CreateReservoirsZone(FReservoirList,
                FReservoirPenaltyStructureList,LZoneLevel,ACurrentReservoirNumber);
      if Assigned(Result) then
        FAppModules.Model.StudyDataHasChanged(sdccAdd,'ReservoirZoneName','',IntToStr(AZoneLevel));
    finally
      LReservoirZoneDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.DeleteReservoirsZone(AZoneLevel: integer): boolean;
const OPNAME = 'TNetworkElementData.DeleteReservoirsZone';
var
  LReservoirZoneDataSQLAgent:TReservoirZoneDataSQLAgent;
begin
  Result := False;
  try
    if (AZoneLevel = 0) then
      raise Exception.Create('FSL cannot be deleted.');
    if (AZoneLevel = FReservoirPenaltyStructureList.PenaltyZoneCount-1) then
      raise Exception.Create('DSL cannot be deleted.');
    if (AZoneLevel = FReservoirPenaltyStructureList.PenaltyZoneCount) then
      raise Exception.Create('BOT cannot be deleted.');

    if(FReservoirList.ReservoirCount > 0) then
    if(MessageDlg(FAppModules.Language.GetString('Message.NetworkElementDataDeleteReservoirsZone'),
                  mtWarning,mbOKCancel,0) = mrCancel) then
      Exit;

    LReservoirZoneDataSQLAgent := TReservoirZoneDataSQLAgent.Create(FAppModules);
    try
      Result := LReservoirZoneDataSQLAgent.DeleteReservoirsZone(FReservoirList,
                FReservoirPenaltyStructureList,AZoneLevel);
      if Result then
        FAppModules.Model.StudyDataHasChanged(sdccDelete,'ReservoirZoneName',IntToStr(AZoneLevel),'');
    finally
      LReservoirZoneDataSQLAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkElementData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TNetworkElementData.Validate';
begin
  Result := True;
  try
    if not FReservoirList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FReservoirPenaltyStructureList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FChannelList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
    if not FChannelPenaltyList.Validate(AErrors,AContext) then
      Result := False;
    if not FReservoirAreaGroupList.Validate(AErrors,AContext) then
      Result := False;
    if(not Result) and FAppModules.GlobalData.StopOnFirstErr then
      Exit;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkElementData.Get_ReservoirAreaGroupList: IReservoirAreaGroupList;
const OPNAME = 'TNetworkElementData.Get_ReservoirAreaGroupList';
begin
  Result := nil;
  try
    Result := FReservoirAreaGroupList;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
