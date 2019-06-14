unit UDamSedimentationDataObject;

interface
uses
  Classes,
  SysUtils,
  Contnrs,
  UAbstractObject,
  UAbstractModelData,
  UViewModelDataObject,
  UAbstractFileNamesObject;
type
  TDamSedimentationDataObject = class(TAbstractAppObject)
  protected
    FDamIdentifier      : integer;
    FDamName            : WideString;
    FDamCode            : WideString;
    FSurveyFileName     : WideString;
    FXCoord             : double;
    FYCoord             : double;
    FChanged            : boolean;
    FDataSource         : WideString;

    FCapacity            : TStringList;
    FReductionInCapacity : TStringList;
    FIncreaseInArea      : TStringList;
    FInitialArea         : TStringList;

    FIncreaseInArea_Gamma : double;
    FDecayInCapacity_Beta : double;
    FDecayOverTime_Alpha : double;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_DamIdentifier      : integer; safecall;
    function Get_DamName            : WideString; safecall;
    function Get_DamCode            : WideString; safecall;
    function Get_SurveyFileName     : WideString; safecall;

    function Get_DataSource          : WideString;
    function Get_XCoord: double;
    function Get_YCoord: double;
    function Get_CapacitySurveyCount : integer;
    function Get_OriginalCapacityByIndex(aIndex : integer) : double;
    function Get_Alpha: double;
    function Get_AreaSurveyCount: integer;
    function Get_Beta: double;
    function Get_Gamma: double;
    function Get_IncreaseInAreaByIndex(aIndex: integer): double;
    function Get_InitialAreaByIndex(aIndex: integer): double;
    function Get_ReductionInCapacityByIndex(aIndex: integer): double;

    procedure Set_IncreaseInAreaByIndex(aIndex: integer; const Value: double);
    procedure Set_InitialAreaByIndex(aIndex: integer; const Value: double);
    procedure Set_ReductionInCapacityByIndex(aIndex: integer; const Value: double);

    procedure Set_DamName(const AValue : WideString);
    procedure Set_DamCode(const AValue : WideString);
    procedure Set_SurveyFileName(const AValue : WideString);
    procedure Set_DataSource(const AValue : WideString);
    procedure Set_XCoord(AValue : double);
    procedure Set_YCoord(AValue : double);
    procedure Set_OriginalCapacityByIndex(aIndex : integer; aValue : double);

  public
    function Populate(ADamIdentifier: integer; ADamName, ADamCode, ASurveyFileName, ADataSource : widestring;AXCoord, AYCoord: double): boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    property DamIdentifier       : integer                                       read Get_DamIdentifier;
    property DamName             : WideString                                    read Get_DamName             write Set_DamName;
    property DamCode             : WideString                                    read Get_DamCode             write Set_DamCode;
    property DataSource          : WideString                                    read Get_DataSource          write Set_DataSource;
    property XCoord              : double                                        read Get_XCoord              write Set_XCoord;
    property YCoord              : double                                        read Get_YCoord              write Set_YCoord;
    property Changed             : boolean                                       read FChanged                write FChanged;
    property SurveyFileName      : WideString                                    read FSurveyFileName         write FSurveyFileName;
    property CapacitySurveyCount                          : integer              read Get_CapacitySurveyCount;
    property OriginalCapacityByIndex[aIndex : integer]    : double               read Get_OriginalCapacityByIndex write Set_OriginalCapacityByIndex;
    property ReductionInCapacityByIndex[aIndex : integer] : double               read Get_ReductionInCapacityByIndex write Set_ReductionInCapacityByIndex;

    property AreaSurveyCount                              : integer              read Get_AreaSurveyCount;
    property InitialAreaByIndex[aIndex : integer] : double                       read Get_InitialAreaByIndex write Set_InitialAreaByIndex;
    property IncreaseInAreaByIndex[aIndex : integer] : double                    read Get_IncreaseInAreaByIndex write Set_IncreaseInAreaByIndex;

    // Decision Variables...
    property DecayOverTime_Alpha                 : double                        read Get_Alpha;
    property ReductionInCapacity_Beta            : double                        read Get_Beta;
    property IncreaseInArea_Gamma                : double                        read Get_Gamma;

  end;

  TDamSedimentationDataList = class(TAbstractAppObject)
  protected
    FDamSedimentationDataList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_DamSedimentationDataByIdentifier(ADamID: integer): TDamSedimentationDataObject;
    function Get_DamSedimentationDataByIndex( AIndex: integer): TDamSedimentationDataObject;
    function Get_DamSedimentationDataCount: integer;
    function MaxDamIdentifier: integer;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    function NewDamSedimentation:TDamSedimentationDataObject;
    function CreateDamSedimentation(ASurveyFileName: string):TDamSedimentationDataObject;
    function DeleteDamSedimentation(ADamID: integer):boolean;
    function SaveDamSedimentation(ADamID: integer):boolean;
    property DamSedimentationDataByIdentifier[ASiteID: integer] : TDamSedimentationDataObject  read Get_DamSedimentationDataByIdentifier;
    property DamSedimentationDataByIndex[AIndex: integer] : TDamSedimentationDataObject  read Get_DamSedimentationDataByIndex;
    property DamSedimentationDataCount : integer  read Get_DamSedimentationDataCount;

  end;

  TDamSedimentationModelData = class(TAbstractModelData)
  protected
    FDamSedimentationDataList  : TDamSedimentationDataList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFilesLineTypes: TAbstractFilesLineTypes; override;
    function GetFileNamesObject: TAbstractModelFileNameList; override;
  public
    function LoadData : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean; override;
    property FilesLineTypes: TAbstractFilesLineTypes read GetFilesLineTypes;
    property FileNamesObject: TAbstractModelFileNameList read GetFileNamesObject;
    property DamSedimentationDataList  : TDamSedimentationDataList read FDamSedimentationDataList;
  end;
implementation

uses
  VCL.Controls,
  UConstants,
  UUtilities,
  //UIFRDataSQLAgent,
  //UIFRDataLoadAgent,
  USedimentationDataLoadAgent,
  UErrorHandlingOperations;

{ TDamSedimentationDataObject }
procedure TDamSedimentationDataObject.CreateMemberObjects;
const OPNAME = 'TDamSedimentationDataObject.CreateMemberObjects';
begin
  inherited;
  try
    FCapacity            := TStringList.create;
    FInitialArea         := TStringList.create;
    FIncreaseInArea      := TStringList.create;
    FReductionInCapacity := TStringList.create;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDamSedimentationDataObject.DestroyMemberObjects;
const OPNAME = 'TDamSedimentationDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FCapacity);
    FreeAndNil(FInitialArea);
    FreeAndNil(FIncreaseInArea);
    FreeAndNil(FReductionInCapacity);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDamSedimentationDataObject._AddRef: Integer;
const OPNAME = 'TDamSedimentationDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject._Release: Integer;
const OPNAME = 'TDamSedimentationDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Validate(var AErrors: WideString;
                        const AContext: WideString): WordBool;
const OPNAME = 'TDamSedimentationDataObject.Validate';
var
 // lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorList := TStringList.Create;
    try

    finally
      lErrorList.Free;
      lErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDamSedimentationDataObject.Initialise: boolean;
const OPNAME = 'TDamSedimentationDataObject.Initialise';
begin
  Result := False;
  try
    FDamIdentifier      := 0;
    FDamName            := '';
    FDamCode           := '';
    FSurveyFileName         := '';
    FXCoord              := 0.0;
    FYCoord              := 0.0;
    FInitialArea.Clear;
    FIncreaseInArea.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDamSedimentationDataObject.Get_DamIdentifier: integer;
const OPNAME = 'TDamSedimentationDataObject.Get_DamIdentifier';
begin
  Result := NullInteger;
  try
    Result    := FDamIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_DataSource: WideString;
const OPNAME = 'TDamSedimentationDataObject.Get_DataSource';
begin
  Result := '';
  try
    Result    := FDataSource;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDamSedimentationDataObject.Get_Gamma: double;
const OPNAME = 'TDamSedimentationDataObject.Get_Gamma';
begin
  Result := 0.0;
  try
    Result    := FIncreaseInArea_Gamma;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_IncreaseInAreaByIndex(aIndex: integer): double;
const OPNAME = 'TDamSedimentationDataObject.Get_IncreaseInAreaByIndex';
begin
  Result := 0.0;
  try
    if (aIndex >= 0) and (aIndex <= FIncreaseInArea.Count-1) then
      if trim(FIncreaseInArea[aIndex]) <> '' then
        Result    := strToFloat(formatFloat('##000.00', strToFloat(FIncreaseInArea[aIndex])));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_InitialAreaByIndex(aIndex: integer): double;
const OPNAME = 'TDamSedimentationDataObject.Get_InitialAreaByIndex';
begin
  Result := 0.0;
  try
    if (aIndex >= 0) and (aIndex <= FInitialArea.Count-1) then
      if trim(FInitialArea[aIndex]) <> '' then
        Result    := strToFloat(formatFloat('##000.00', strToFloat(FInitialArea[aIndex])));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_OriginalCapacityByIndex(aIndex: integer): double;
const OPNAME = 'TDamSedimentationDataObject.Get_ReductionInCapacityByIndex';
begin
  Result := 0.0;
  try
    if (aIndex >= 0) and (aIndex <= FCapacity.Count-1) then
      if trim(FCapacity[aIndex]) <> '' then
        Result    := strToFloat(formatFloat('##000.00', strToFloat(FCapacity[aIndex])));
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDamSedimentationDataObject.Get_ReductionInCapacityByIndex(aIndex: integer): double;
const OPNAME = 'TDamSedimentationDataObject.Get_ReductionInCapacityByIndex';
begin
  Result := 0.0;
  try
    if (aIndex >= 0) and (aIndex <= FReductionInCapacity.Count-1) then
      if trim(FReductionInCapacity[aIndex]) <> '' then
        Result    := strToFloat(formatFloat('##000.00', strToFloat(FReductionInCapacity[aIndex])));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_SurveyFileName: WideString;
const OPNAME = 'TDamSedimentationDataObject.Get_SurveyFileName';
begin
  Result := '';
  try
    Result    := FSurveyFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_Alpha: double;
const OPNAME = 'TDamSedimentationDataObject.Get_Alpha';
begin
  Result := 0.00;
  try
    Result    := FDecayOverTime_Alpha;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_AreaSurveyCount: integer;
const OPNAME = 'TDamSedimentationDataObject.Get_Alpha';
begin
  Result := 0;
  try
    Result    := FInitialArea.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_Beta: double;
const OPNAME = 'TDamSedimentationDataObject.Get_Alpha';
begin
  Result := 0.00;
  try
    Result    := FDecayInCapacity_Beta;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_CapacitySurveyCount: integer;
const OPNAME = 'TDamSedimentationDataObject.Get_DamCode';
begin
  Result := 0;
  try
    Result := FCapacity.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_DamCode: WideString;
const OPNAME = 'TDamSedimentationDataObject.Get_DamCode';
begin
  Result := '';
  try
    Result    := FDamCode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_DamName: WideString;
const OPNAME = 'TDamSedimentationDataObject.Get_DamName';
begin
  Result := '';
  try
    Result    := FDamName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_XCoord: double;
const OPNAME = 'TIFRSiteDataObject.Get_XCoord';
begin
  Result := NullFloat;
  try
    Result    := FXCoord;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataObject.Get_YCoord: double;
const OPNAME = 'TIFRSiteDataObject.Get_YCoord';
begin
  Result := NullFloat;
  try
    Result    := FYCoord;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataObject.Set_DataSource(const AValue: WideString);
const OPNAME = 'TDamSedimentationDataObject.Set_DataSource';
var
  LOldValue : String;
begin
  try
    if(AValue <> FDataSource) then
    begin
      LOldValue   := FDataSource;
      FDataSource := AValue;
      FChanged    := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamSedimentationSource',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDamSedimentationDataObject.Set_IncreaseInAreaByIndex(aIndex: integer;
  const Value: double);
begin

end;

procedure TDamSedimentationDataObject.Set_InitialAreaByIndex(aIndex: integer;
  const Value: double);
begin

end;

procedure TDamSedimentationDataObject.Set_OriginalCapacityByIndex(
  aIndex: integer; aValue: double);
begin

end;

procedure TDamSedimentationDataObject.Set_ReductionInCapacityByIndex(
  aIndex: integer; const Value: double);
begin

end;

procedure TDamSedimentationDataObject.Set_SurveyFileName(const AValue: WideString);
const OPNAME = 'TDamSedimentationDataObject.Set_SurveyFileName';
var
  LOldValue : String;
begin
  try
    if(AValue <> FSurveyFileName) then
    begin
      LOldValue    := FSurveyFileName;
      FSurveyFileName := AValue;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamSedimentationSurveyFileName',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDamSedimentationDataObject.Set_DamCode(const AValue: WideString);
const OPNAME = 'TDamSedimentationDataObject.Set_DamCode';
var
  LOldValue : String;
begin
  try
    if(AValue <> FDamCode) then
    begin
      LOldValue        := FDamCode;
      FDamCode := AValue;
      FChanged         := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamCode',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataObject.Set_DamName(const AValue: WideString);
const OPNAME = 'TDamSedimentationDataObject.Set_DamName';
var
  LOldValue : String;
begin
  try
    if(AValue <> FDamName) then
    begin
      LOldValue := FDamName;
      FDamName := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamSediName',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataObject.Set_XCoord(AValue: double);
const OPNAME = 'TDamSedimentationDataObject.Set_XCoord';
var
  LOldValue : String;
begin
  try
    if(AValue <> FXCoord) then
    begin
      LOldValue := FloatToStr(FXCoord);
      FXCoord   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamXCoord',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataObject.Set_YCoord(AValue: double);
const OPNAME = 'TDamSedimentationDataObject.Set_YCoord';
var
  LOldValue : String;
begin
  try
    if(AValue <> FYCoord) then
    begin
      LOldValue := FloatToStr(FYCoord);
      FYCoord   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'DamYCoord',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDamSedimentationDataObject.Populate(ADamIdentifier: integer; ADamName, ADamCode, ASurveyFileName, ADataSource: widestring;AXCoord, AYCoord: double): boolean;
const OPNAME = 'TDamSedimentationDataObject.Populate';
begin
  Result := False;
  try
    FDamIdentifier      := ADamIdentifier;
    FDamName            := ADamName;
    FDamCode            := ADamCode;
    FSurveyFileName     := ASurveyFileName;
    FDataSource          := ADataSource;
    FXCoord              := AXCoord;
    FYCoord              := AYCoord;
    FChanged             := False;
    Result               := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDamSedimentationDataList }

function TDamSedimentationDataList._AddRef: Integer;
const OPNAME = 'TDamSedimentationDataList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList._Release: Integer;
const OPNAME = 'TDamSedimentationDataList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataList.CreateMemberObjects;
const OPNAME = 'TDamSedimentationDataList.CreateMemberObjects';
begin
  inherited;
  try
    FDamSedimentationDataList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationDataList.DestroyMemberObjects;
const OPNAME = 'TDamSedimentationDataList.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDamSedimentationDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.NewDamSedimentation: TDamSedimentationDataObject;
const OPNAME = 'TDamSedimentationDataList.NewDamSedimentation';
var
  LNewDamSedimentationData:TDamSedimentationDataObject;
begin
  Result := nil;
  try
    LNewDamSedimentationData := TDamSedimentationDataObject.Create(FAppModules);
    LNewDamSedimentationData.Initialise;
    FDamSedimentationDataList.Add(LNewDamSedimentationData);
    Result := LNewDamSedimentationData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.CreateDamSedimentation(ASurveyFileName: string): TDamSedimentationDataObject;
const OPNAME = 'TDamSedimentationDataList.CreateDamSedimentation';
begin
  Result := nil;
  try
    Result := NewDamSedimentation;
    Result.FDamIdentifier := MaxDamIdentifier+1;
    Result.FDamName       := 'Dam Sedimentation '+ IntToStr(Result.FDamIdentifier);
    Result.FSurveyFileName    := ASurveyFileName;
    Result.Changed         := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.Get_DamSedimentationDataByIdentifier(ADamID: integer): TDamSedimentationDataObject;
const OPNAME = 'TDamSedimentationDataList.Get_DamSedimentationDataByIdentifier';
var
  LIndex : integer;
  LDamSedimentationData:TDamSedimentationDataObject;
begin
  Result := nil;
  try
    for LIndex := 0 to FDamSedimentationDataList.Count-1  do
    begin
      LDamSedimentationData := TDamSedimentationDataObject(FDamSedimentationDataList[LIndex]);
      if(LDamSedimentationData.DamIdentifier = ADamID) then
      begin
        Result := LDamSedimentationData;
        Exit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.Get_DamSedimentationDataByIndex(AIndex: integer): TDamSedimentationDataObject;
const OPNAME = 'TDamSedimentationDataList.Get_DamSedimentationDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDamSedimentationDataList.Count) then
      Result  := TDamSedimentationDataObject(FDamSedimentationDataList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.Get_DamSedimentationDataCount: integer;
const OPNAME = 'TDamSedimentationDataList.Get_DamSedimentationDataCount';
begin
  Result := 0;
  try
    Result :=  FDamSedimentationDataList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.Initialise: boolean;
const OPNAME = 'TDamSedimentationDataList.Initialise';
begin
  Result := False;
  try
    FDamSedimentationDataList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TDamSedimentationDataList.Validate';
var
  LIndex : integer;
  LIFRSiteData:TDamSedimentationDataObject;
begin
  Result := False;
  try
    for LIndex := 0 to FDamSedimentationDataList.Count-1  do
    begin
      LIFRSiteData := TDamSedimentationDataObject(FDamSedimentationDataList[LIndex]);
      LIFRSiteData.Validate(AErrors,AContext);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.MaxDamIdentifier: integer;
const OPNAME = 'TDamSedimentationDataList.MaxDamIdentifier';
var
  LIndex : integer;
  LDamSedimentationData:TDamSedimentationDataObject;
begin
  Result := 0;
  try
    for LIndex := 0 to FDamSedimentationDataList.Count-1  do
    begin
      LDamSedimentationData := DamSedimentationDataByIndex[LIndex];
      if(LDamSedimentationData.DamIdentifier > Result) then
        Result := LDamSedimentationData.DamIdentifier;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.DeleteDamSedimentation(ADamID: integer): boolean;
const OPNAME = 'TDamSedimentationDataList.DeleteDamSedimentation';
var
  LDamSedimentationData:TDamSedimentationDataObject;
  //LSQLAgent:TSedimentationDataSQLAgent;
begin
  Result := False;
  try
    LDamSedimentationData := DamSedimentationDataByIdentifier[ADamID];
    if(LDamSedimentationData <> nil) then
    begin
     { LSQLAgent := TSedimentationDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.DeleteDamSedimentationData(LIFRSiteData);
        if Result then
         FDamSedimentationDataList.Remove(LDamSedimentationData);
      finally
        LSQLAgent.Free;
      end;  }
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationDataList.SaveDamSedimentation(ADamID: integer): boolean;
const OPNAME = 'TDamSedimentationDataList.SaveDamSedimentation';
var
  LDamSedimentationData:TDamSedimentationDataObject;
 // LSQLAgent:TSedimentationDataSQLAgent;
begin
  Result := False;
  try
    LDamSedimentationData := DamSedimentationDataByIdentifier[ADamID];
    if(LDamSedimentationData <> nil) then
    begin
      {LSQLAgent := TSedimentationDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.SaveIFRData(LIFRSiteData);
        if Result then
          LDamSedimentationData.Changed := False;
      finally
        LSQLAgent.Free;
      end;  }
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDamSedimentationModelData }

procedure TDamSedimentationModelData.CreateMemberObjects;
const OPNAME = 'TDamSedimentationModelData.CreateMemberObjects';
begin
  inherited;
  try
    FDamSedimentationDataList  := TDamSedimentationDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDamSedimentationModelData.DestroyMemberObjects;
const OPNAME = 'TDamSedimentationModelData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDamSedimentationDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationModelData.Initialise: boolean;
const OPNAME = 'TDamSedimentationModelData.Initialise';
begin
  Result := False;
  try
    Result := FDamSedimentationDataList.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationModelData.LoadData: boolean;
const OPNAME = 'TDamSedimentationModelData.LoadData';
var
  LLoadAgent : TSedimentationDataLoadAgent;
begin
  Result := False;
  try
    LLoadAgent := TSedimentationDataLoadAgent.Create(FAppModules);
    try
      Result := LLoadAgent.LoadDataFromDB(FDamSedimentationDataList);
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDamSedimentationModelData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TDamSedimentationModelData.Validate';
begin
  Result := False;
  try
    Result := FDamSedimentationDataList.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationModelData.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TDamSedimentationModelData.GetFileNamesObject';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationModelData.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TDamSedimentationModelData.GetFilesLineTypes';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDamSedimentationModelData.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TDamSedimentationModelData.GetViewDataItems';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
