//
//
//  UNIT      : Contains TIFRSiteDataObject Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UIFRDataObject;

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
  TIFRSiteDataObject = class(TAbstractAppObject)
  protected
    FSiteIdentifier      : integer;
    FSiteName            : WideString;
    FSiteDescription     : WideString;
    FCSVFileName         : WideString;
    FQuaternaryCatchment : WideString;
    FRiverName           : WideString;
    FAssociatedEMC       : WideString;
    FLevelOfDetail       : WideString;
    FLevelOfConfidence   : WideString;
    FDataSource          : WideString;
    FXCoord              : double;
    FYCoord              : double;
    FExceedencePerc      : TExceedencePercentagesArray;
    FRequiredFlows       : TIFRArray;
    FChanged             : boolean;
    FMonthNames          : TMonthNamesArray;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function Get_SiteIdentifier      : integer; safecall;
    function Get_SiteName            : WideString; safecall;
    function Get_SiteDescription     : WideString; safecall;
    function Get_CSVFileName         : WideString; safecall;
    function Get_QuaternaryCatchment : WideString; safecall;
    function Get_RiverName           : WideString; safecall;
    function Get_AssociatedEMC       : WideString; safecall;
    function Get_LevelOfDetail       : WideString; safecall;
    function Get_LevelOfConfidence   : WideString; safecall;
    function Get_DataSource          : WideString; safecall;
    function Get_XCoord              : double; safecall;
    function Get_YCoord              : double; safecall;
    function Get_ExceedencePercArray : TExceedencePercentagesArray;
    function Get_RequiredFlowsArray  : TIFRArray;
    function Get_MonthNamesCommaText : WideString;
    function Get_ExceedencePercByIndex(AIndex: integer): double;
    function Get_RequiredFlowsByIndex(ARow,ACol: integer): double;


    procedure Set_SiteName(const AValue : WideString);
    procedure Set_SiteDescription(const AValue : WideString);
    procedure Set_CSVFileName(const AValue : WideString);
    procedure Set_QuaternaryCatchment(const AValue : WideString);
    procedure Set_RiverName(const AValue : WideString);
    procedure Set_AssociatedEMC(const AValue : WideString);
    procedure Set_LevelOfDetail(const AValue : WideString);
    procedure Set_LevelOfConfidence(const AValue : WideString);
    procedure Set_DataSource(const AValue : WideString);
    procedure Set_XCoord(AValue : double);
    procedure Set_YCoord(AValue : double);
    procedure Set_ExceedencePercByIndex(AIndex: integer;AValue : double);
    procedure Set_RequiredFlowsByIndex(ARow,ACol: integer;AValue : double);
    function ValidateInflows (AErrorMessages : TStrings;
                              AErrorColumns  : TStringList): WordBool;
  public
    function Populate(ASiteIdentifier: integer; ASiteName, ASiteDescription, ACSVFileName, AQuaternaryCatchment,
             ARiverName, AAssociatedEMC, ALevelOfDetail, ALevelOfConfidence, ADataSource,AMonthNamesCommaText:WideString;
             AXCoord, AYCoord: double): boolean;
    function PopulateArraysWithDataset(ADataSet : TAbstractModelDataset): boolean;
    function PopulateArrays(AExceedencePerc : TExceedencePercentagesArray; ARequiredFlows : TIFRArray): boolean;
    function PopulateMonthNamesCommaText(AMonthNamesCSV:string): boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    property SiteIdentifier      : integer                     read Get_SiteIdentifier;
    property SiteName            : WideString                  read Get_SiteName            write Set_SiteName;
    property SiteDescription     : WideString                  read Get_SiteDescription     write Set_SiteDescription;
    property CSVFileName         : WideString                  read Get_CSVFileName         write Set_CSVFileName;
    property QuaternaryCatchment : WideString                  read Get_QuaternaryCatchment write Set_QuaternaryCatchment;
    property RiverName           : WideString                  read Get_RiverName           write Set_RiverName;
    property AssociatedEMC       : WideString                  read Get_AssociatedEMC       write Set_AssociatedEMC;
    property LevelOfDetail       : WideString                  read Get_LevelOfDetail       write Set_LevelOfDetail;
    property LevelOfConfidence   : WideString                  read Get_LevelOfConfidence   write Set_LevelOfConfidence;
    property DataSource          : WideString                  read Get_DataSource          write Set_DataSource;
    property XCoord              : double                      read Get_XCoord              write Set_XCoord;
    property YCoord              : double                      read Get_YCoord              write Set_YCoord;
    property MonthNames          : TMonthNamesArray            read FMonthNames;
    property MonthNamesCommaText : WideString                  read Get_MonthNamesCommaText;
    property Changed             : boolean                     read FChanged                  write FChanged;
    property ExceedencePercByIndex[AIndex: integer] : double   read Get_ExceedencePercByIndex write Set_ExceedencePercByIndex;
    property RequiredFlowsByIndex[ARow,ACol: integer] : double   read Get_RequiredFlowsByIndex write Set_RequiredFlowsByIndex;
    property ExceedencePercentageArray: TExceedencePercentagesArray read Get_ExceedencePercArray;
    property RequiredFlowsArray       : TIFRArray                   read Get_RequiredFlowsArray;
  end;

  TIFRSiteDataList = class(TAbstractAppObject)
  protected
    FIFRSiteDataList  : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_IFRSiteDataByIdentifier(ASiteID: integer): TIFRSiteDataObject;
    function Get_IFRSiteDataByIndex( AIndex: integer): TIFRSiteDataObject;
    function Get_IFRSiteDataCount: integer;
    function MaxSiteIdentifier: integer;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    function NewIFRSite:TIFRSiteDataObject;
    function CreateIFRSite(ACSVFileName: string):TIFRSiteDataObject;
    function DeleteIFRSite(ASiteID: integer):boolean;
    function SaveIFRSite(ASiteID: integer):boolean;
    property IFRSiteDataByIdentifier[ASiteID: integer] : TIFRSiteDataObject  read Get_IFRSiteDataByIdentifier;
    property IFRSiteDataByIndex[AIndex: integer] : TIFRSiteDataObject  read Get_IFRSiteDataByIndex;
    property IFRSiteDataCount : integer  read Get_IFRSiteDataCount;

  end;

  TIFRModelData = class(TAbstractModelData)
  protected
    FIFRSiteDataList  : TIFRSiteDataList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFilesLineTypes: TAbstractFilesLineTypes; override;
    function GetFileNamesObject: TAbstractModelFileNameList; override;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function Initialise: boolean; override;
    function GetViewDataItems(AViewId: string; AItemsList:TViewModelDataItemsList; var AHandled:boolean): boolean; override;
    property FilesLineTypes: TAbstractFilesLineTypes read GetFilesLineTypes;
    property FileNamesObject: TAbstractModelFileNameList read GetFileNamesObject;
    property IFRSiteDataList  : TIFRSiteDataList read FIFRSiteDataList;
  end;
implementation

uses
  System.Types,
  VCL.Controls,
  UConstants,
  UUtilities,
  UIFRDataSQLAgent,
//  UIFRDataLoadAgent,
  UErrorHandlingOperations;

{ TIFRSiteDataObject }
procedure TIFRSiteDataObject.CreateMemberObjects;
const OPNAME = 'TIFRSiteDataObject.CreateMemberObjects';
begin
  inherited;
  try
    SetLength(FMonthNames,13);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRSiteDataObject.DestroyMemberObjects;
const OPNAME = 'TIFRSiteDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    Finalize(FMonthNames);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRSiteDataObject._AddRef: Integer;
const OPNAME = 'TIFRSiteDataObject._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject._Release: Integer;
const OPNAME = 'TIFRSiteDataObject._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Validate(var AErrors: WideString;
                        const AContext: WideString): WordBool;
const OPNAME = 'TIFRSiteDataObject.Validate';
var
  lStopOnFirstError : Boolean;
  lErrorCols        : TStringList;
  lErrorList        : TStringList;
begin
  Result := FALSE;
  try
    lErrorCols := TStringList.Create;
    lErrorList := TStringList.Create;
    try
      if (AContext = 'RequiredInflows') then
      begin
        Result := ValidateInflows(lErrorList,lErrorCols);
        if (NOT Result) then
        begin
          if (lErrorCols.Count = 0) then
            AErrors := AErrors + lErrorList.Text
          else
            AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                 CTStringsSeparator + lErrorCols.Text+ CTStringsSeparator;
          lErrorList.Clear;
          lErrorCols.Clear;
        end;
      end
      else
      begin
        Result := TRUE;
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;

        if (Result OR (NOT lStopOnFirstError)) then
        begin
          if (NOT ValidateInflows(lErrorList,lErrorCols)) then
          begin
            Result := FALSE;
            if (lErrorCols.Count = 0) then
              AErrors := AErrors + lErrorList.Text
            else
              AErrors := AErrors + CTStringsSeparator + lErrorList.Text +
                                   CTStringsSeparator + lErrorCols.Text + CTStringsSeparator;
            lErrorList.Clear;
            lErrorCols.Clear;
          end;
        end;
          Result := TRUE;
      end;
      AErrors := AErrors + lErrorList.Text;
    finally
      lErrorList.Free;
      lErrorCols.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteDataObject.Initialise: boolean;
const OPNAME = 'TIFRSiteDataObject.Initialise';
begin
  Result := False;
  try
    FSiteIdentifier      := 0;
    FSiteName            := '';
    FSiteDescription     := '';
    FCSVFileName         := '';
    FQuaternaryCatchment := '';
    FRiverName           := '';
    FAssociatedEMC       := '';
    FLevelOfDetail       := '';
    FLevelOfConfidence   := '';
    FDataSource          := '';
    FXCoord              := 0.0;
    FYCoord              := 0.0;
    FChanged             := False;
    Finalize(FExceedencePerc);
    Finalize(FRequiredFlows);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRSiteDataObject.Get_SiteIdentifier: integer;
const OPNAME = 'TIFRSiteDataObject.Get_SiteIdentifier';
begin
  Result := NullInteger;
  try
    Result    := FSiteIdentifier;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_AssociatedEMC: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_AssociatedEMC';
begin
  Result := '';
  try
    Result    := FAssociatedEMC;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_CSVFileName: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_CSVFileName';
begin
  Result := '';
  try
    Result    := FCSVFileName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_DataSource: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_DataSource';
begin
  Result := '';
  try
    Result    := FDataSource;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_LevelOfConfidence: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_LevelOfConfidence';
begin
  Result := '';
  try
    Result    := FLevelOfConfidence;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_LevelOfDetail: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_LevelOfDetail';
begin
  Result := '';
  try
    Result    := FLevelOfDetail;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_QuaternaryCatchment: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_QuaternaryCatchment';
begin
  Result := '';
  try
    Result    := FQuaternaryCatchment;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_RiverName: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_RiverName';
begin
  Result := '';
  try
    Result    := FRiverName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_SiteDescription: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_SiteDescription';
begin
  Result := '';
  try
    Result    := FSiteDescription;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_SiteName: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_SiteName';
begin
  Result := '';
  try
    Result    := FSiteName;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_XCoord: double;
const OPNAME = 'TIFRSiteDataObject.Get_XCoord';
begin
  Result := NullFloat;
  try
    Result    := FXCoord;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_YCoord: double;
const OPNAME = 'TIFRSiteDataObject.Get_YCoord';
begin
  Result := NullFloat;
  try
    Result    := FYCoord;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_ExceedencePercArray: TExceedencePercentagesArray;
const OPNAME = 'TIFRSiteDataObject.Get_ExceedencePercArray';
begin
  Result := nil;
  try
    Result    := FExceedencePerc;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_RequiredFlowsArray: TIFRArray;
const OPNAME = 'TIFRSiteDataObject.Get_RequiredFlowsArray';
begin
  Result := nil;
  try
    Result    := FRequiredFlows;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_ExceedencePercByIndex( AIndex: integer): double;
const OPNAME = 'TIFRSiteDataObject.Get_ExceedencePercByIndex';
begin
  Result := NullFloat;
  try
    if (AIndex >= Low(FExceedencePerc)) and (AIndex <= High(FExceedencePerc)) then
    begin
      Result := FExceedencePerc[AIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_RequiredFlowsByIndex(ARow, ACol: integer): double;
const OPNAME = 'TIFRSiteDataObject.Get_RequiredFlowsByIndex';
begin
  Result := NullFloat;
  try
    if (ARow >= Low(FExceedencePerc)) and (ARow <= High(FExceedencePerc)) and
       (ACol >= 0) and (ARow <= 11) then
    begin
      Result := FRequiredFlows[ARow, ACol];
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_AssociatedEMC(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_AssociatedEMC';
var
  LOldValue : String;
begin
  try
    if(AValue <> FAssociatedEMC) then
    begin
      LOldValue      := FAssociatedEMC;
      FAssociatedEMC := AValue;
      FChanged       := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteAssociatedEMC',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_CSVFileName(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_CSVFileName';
var
  LOldValue : String;
begin
  try
    if(AValue <> FCSVFileName) then
    begin
      LOldValue    := FCSVFileName;
      FCSVFileName := AValue;
      FChanged     := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteCSVFileName',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_DataSource(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_DataSource';
var
  LOldValue : String;
begin
  try
    if(AValue <> FDataSource) then
    begin
      LOldValue   := FDataSource;
      FDataSource := AValue;
      FChanged    := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteSource',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_LevelOfConfidence(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_LevelOfConfidence';
var
  LOldValue : String;
begin
  try
    if(AValue <> FLevelOfConfidence) then
    begin
      LOldValue          := FLevelOfConfidence;
      FLevelOfConfidence := AValue;
      FChanged           := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteLevelConfidence',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_LevelOfDetail(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_LevelOfDetail';
var
  LOldValue : String;
begin
  try
    if(AValue <> FLevelOfDetail) then
    begin
      LOldValue      := FLevelOfDetail;
      FLevelOfDetail := AValue;
      FChanged       := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteLevelDetail',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_QuaternaryCatchment(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_QuaternaryCatchment';
var
  LOldValue : String;
begin
  try
    if(AValue <> FQuaternaryCatchment) then
    begin
      LOldValue            := FQuaternaryCatchment;
      FQuaternaryCatchment := AValue;
      FChanged             := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteQuaternaryCatchment',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_RiverName(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_RiverName';
var
  LOldValue : String;
begin
  try
    if(AValue <> FRiverName) then
    begin
      LOldValue  := FRiverName;
      FRiverName := AValue;
      FChanged   := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteRiverName',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_SiteDescription(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_SiteDescription';
var
  LOldValue : String;
begin
  try
    if(AValue <> FSiteDescription) then
    begin
      LOldValue        := FSiteDescription;
      FSiteDescription := AValue;
      FChanged         := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteDescr',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_SiteName(const AValue: WideString);
const OPNAME = 'TIFRSiteDataObject.Set_SiteName';
var
  LOldValue : String;
begin
  try
    if(AValue <> FSiteName) then
    begin
      LOldValue := FSiteName;
      FSiteName := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteName',LOldValue,AValue);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_XCoord(AValue: double);
const OPNAME = 'TIFRSiteDataObject.Set_XCoord';
var
  LOldValue : String;
begin
  try
    if(AValue <> FXCoord) then
    begin
      LOldValue := FloatToStr(FXCoord);
      FXCoord   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteXCoord',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_YCoord(AValue: double);
const OPNAME = 'TIFRSiteDataObject.Set_YCoord';
var
  LOldValue : String;
begin
  try
    if(AValue <> FYCoord) then
    begin
      LOldValue := FloatToStr(FYCoord);
      FYCoord   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteYCoord',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_ExceedencePercByIndex(AIndex: integer; AValue: double);
const OPNAME = 'TIFRSiteDataObject.Set_ExceedencePercByIndex';
var
  LOldValue : String;
begin
  try
    if (AIndex >= Low(FExceedencePerc)) and (AIndex <= High(FExceedencePerc)) and (AValue <> FExceedencePerc[AIndex]) then
    begin
      LOldValue := FloatToStr(FExceedencePerc[AIndex]);
      FExceedencePerc[AIndex]   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteExceedence',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataObject.Set_RequiredFlowsByIndex(ARow, ACol: integer; AValue: double);
const OPNAME = 'TIFRSiteDataObject.Set_RequiredFlowsByIndex';
var
  LOldValue : String;
begin
  try
    if (ARow >= Low(FRequiredFlows)) and (ARow <= High(FRequiredFlows)) and
       (ACol >= 0) and (ARow <= 11) and  (AValue <> FRequiredFlows[ARow, ACol]) then
    begin
      LOldValue := FloatToStr(FRequiredFlows[ARow, ACol]);
      FRequiredFlows[ARow, ACol]   := AValue;
      FChanged  := True;
      FAppModules.Model.StudyDataHasChanged(sdccEdit,'IFRSiteFlow',LOldValue,FloatToStr(AValue));
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Populate(ASiteIdentifier: integer; ASiteName, ASiteDescription, ACSVFileName,
         AQuaternaryCatchment, ARiverName, AAssociatedEMC, ALevelOfDetail, ALevelOfConfidence,ADataSource,
         AMonthNamesCommaText: WideString; AXCoord, AYCoord: double): boolean;
const OPNAME = 'TIFRSiteDataObject.Populate';
var
  LMonthNames : TStringList;
  LIndex      : integer;
begin
  Result := False;
  try
    FSiteIdentifier      := ASiteIdentifier;
    FSiteName            := ASiteName;
    FSiteDescription     := ASiteDescription;
    FCSVFileName         := ACSVFileName;
    FQuaternaryCatchment := AQuaternaryCatchment;
    FRiverName           := ARiverName;
    FAssociatedEMC       := AAssociatedEMC;
    FLevelOfDetail       := ALevelOfDetail;
    FLevelOfConfidence   := ALevelOfConfidence;
    FDataSource          := ADataSource;
    FXCoord              := AXCoord;
    FYCoord              := AYCoord;
    FChanged             := False;

    LMonthNames          := TStringList.Create;
    try
      LMonthNames.CommaText := AMonthNamesCommaText;
      for LIndex := 0 to LMonthNames.Count-1 do
      begin
        if(LIndex <= High(FMonthNames)) then
          FMonthNames[LIndex] := LMonthNames[LIndex];
      end;
    finally
      LMonthNames.Free;
    end;
    Result               := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.PopulateMonthNamesCommaText(AMonthNamesCSV: string): boolean;
const OPNAME = 'TIFRSiteDataObject.PopulateMonthNamesCommaText';
var
  LMonthNames : TStringList;
  LMonthNamesCommaText,
  LMonth      : string;
  LIndex      : integer;
begin
  Result := False;
  try
    LMonthNames          := TStringList.Create;
    try
      LMonthNamesCommaText := AMonthNamesCSV;
      while (LMonthNamesCommaText <> '') do
      begin
        LMonth  := ExtractDelemetedFirstSubstring(',',LMonthNamesCommaText);
        if(LMonth = '') then
        begin
          LMonth := LMonthNamesCommaText;
          LMonthNamesCommaText := '';
        end;
        LMonthNames.Add(LMonth);
      end;

      for LIndex := 0 to LMonthNames.Count-1 do
      begin
        if(LIndex <= High(FMonthNames)) then
          FMonthNames[LIndex] := LMonthNames[LIndex];
      end;
      Result := True;
    finally
      LMonthNames.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.PopulateArraysWithDataset(ADataSet: TAbstractModelDataset): boolean;
const OPNAME = 'TIFRSiteDataObject.PopulateArraysWithDataset';
var
  LRow,
  LCol: integer;
  LFieldName: string;
  LValue : double;
begin
  Result := False;
  try
    Finalize(FExceedencePerc);
    Finalize(FRequiredFlows);
    if(ADataSet <> nil) and not (ADataSet.DataSet.Eof and ADataSet.DataSet.Bof) then
    begin
      ADataSet.DataSet.First;
      LRow := 0;
      while not ADataSet.DataSet.Eof do
      begin
        LRow := LRow + 1;
        ADataSet.DataSet.Next;
      end;
      SetLength(FExceedencePerc, LRow);
      SetLength(FRequiredFlows, LRow,12);

      ADataSet.DataSet.First;
      LRow := 0;
      while not ADataSet.DataSet.Eof do
      begin
        LValue := ADataSet.DataSet.FieldByName('ExceedProbability').AsFloat;
        FExceedencePerc[LRow] := LValue;

        for LCol := 1 to 12 do
        begin
          LFieldName := Format('%s%2.2d',['Flow',LCol]);
          LValue := ADataSet.DataSet.FieldByName(LFieldName).AsFloat;
          FRequiredFlows[LRow,LCol-1] := LValue;
        end;

        ADataSet.DataSet.Next;
        LRow := LRow + 1;
      end;
      Result               := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.PopulateArrays(AExceedencePerc: TExceedencePercentagesArray; ARequiredFlows: TIFRArray): boolean;
const OPNAME = 'TIFRSiteDataObject.PopulateArrays';
var
  LRow,
  LCol: integer;
begin
  Result := False;
  try
    Finalize(FExceedencePerc);
    Finalize(FRequiredFlows);

    SetLength(FExceedencePerc, Length(AExceedencePerc));
    SetLength(FRequiredFlows, Length(AExceedencePerc),12);

    for LRow := Low(FExceedencePerc) to High(FExceedencePerc) do
    begin
      FExceedencePerc[LRow] := AExceedencePerc[LRow];
      for LCol := 1 to 12 do
      begin
        FRequiredFlows[LRow,LCol-1] := ARequiredFlows[LRow,LCol-1];
      end;
    end;
    Result               := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataObject.Get_MonthNamesCommaText: WideString;
const OPNAME = 'TIFRSiteDataObject.Get_MonthNamesCommaText';
var
  LMonthNames : TStringList;
  LIndex      : integer;
begin
  Result := '';
  try
    LMonthNames          := TStringList.Create;
    try
      for LIndex := Low(FMonthNames) to High(FMonthNames) do
      begin
        LMonthNames.Add(FMonthNames[LIndex]);
      end;
      Result  := LMonthNames.CommaText;
    finally
      LMonthNames.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;

end;

function TIFRSiteDataObject.ValidateInflows(AErrorMessages: TStrings;
                                            AErrorColumns: TStringList): WordBool;
const OPNAME = 'TIFRSiteDataObject.ValidateInflows';
var
  lIndex            : integer;
  lMonth            : integer;
  lStopOnFirstError : Boolean;
  lMessage          : string;
  lValid            : boolean;
  lResult           : boolean;
  lAscending        : Boolean;
  LNrOfFileLines    : integer;
begin
  Result := FALSE;
  try
    lResult := TRUE;
    lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
    LNrOfFileLines := Length(ExceedencePercentageArray);
    for lIndex := 0 to LNrOfFileLines - 1 do
    begin
      for lMonth := 1 to 12 do
      begin
        lMessage := '';
        lValid := FAppModules.FieldProperties.ValidateFieldProperty
                    ('IFRVariables', FloatToStr(FRequiredFlows[lIndex, lMonth-1]),
                    lMessage, lIndex+1, lMonth);
        if (NOT lValid) then
        begin
          lResult := FALSE;
          AErrorMessages.Add('ERROR:' +lMessage);
          if (AErrorColumns.IndexOf(IntToStr(lMonth )) < 0 ) then
            AErrorColumns.Add(IntToStr(lMonth));
          if (lStopOnFirstError) then
            Break;
        end;
      end;
    end;

    if (lResult OR (NOT lStopOnFirstError)) then
    begin
      lAscending := TRUE;
      lMonth := 1;
      while (lMonth <= 12) do
      begin
        lValid := TRUE;
        lIndex := 1;
        while (lValid AND (lIndex < LNrOfFileLines)) do
        begin
          if (FRequiredFlows[lIndex-1, lMonth-1] <> NullFloat) and
             (FRequiredFlows[lIndex-1, lMonth-1] > FRequiredFlows[lIndex, lMonth-1]) then
          begin
            AErrorColumns.Add(IntToStr(lMonth));
            lValid     := FALSE;
            lAscending := FALSE;
          end
          else
            lIndex := lIndex + 1;
        end;
        lMonth := lMonth + 1;
      end;
      if (NOT lAscending) then
      begin
        lResult := FALSE;
        lMessage := FAppModules.Language.GetString('ContextValidation.IFRNotInAscendingOrder');
        AErrorMessages.Add('ERROR:' + lMessage);
      end;
    end;
    Result := lResult;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TIFRSiteDataList }

function TIFRSiteDataList._AddRef: Integer;
const OPNAME = 'TIFRSiteDataList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList._Release: Integer;
const OPNAME = 'TIFRSiteDataList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataList.CreateMemberObjects;
const OPNAME = 'TIFRSiteDataList.CreateMemberObjects';
begin
  inherited;
  try
    FIFRSiteDataList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRSiteDataList.DestroyMemberObjects;
const OPNAME = 'TIFRSiteDataList.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FIFRSiteDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.NewIFRSite: TIFRSiteDataObject;
const OPNAME = 'TIFRSiteDataList.NewIFRSite';
var
  LIFRSiteData:TIFRSiteDataObject;
begin
  Result := nil;
  try
    LIFRSiteData := TIFRSiteDataObject.Create(FAppModules);
    LIFRSiteData.Initialise;
    FIFRSiteDataList.Add(LIFRSiteData);
    Result := LIFRSiteData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.CreateIFRSite(ACSVFileName: string): TIFRSiteDataObject;
const OPNAME = 'TIFRSiteDataList.CreateIFRSite';
begin
  Result := nil;
  try
    Result := NewIFRSite;
    Result.FSiteIdentifier := MaxSiteIdentifier+1;
    Result.FSiteName       := 'IFR Site '+ IntToStr(Result.FSiteIdentifier);
    Result.FCSVFileName    := ACSVFileName;
    Result.Changed         := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.Get_IFRSiteDataByIdentifier(ASiteID: integer): TIFRSiteDataObject;
const OPNAME = 'TIFRSiteDataList.Get_IFRSiteDataByIdentifier';
var
  LIndex : integer;
  LIFRSiteData:TIFRSiteDataObject;
begin
  Result := nil;
  try
    for LIndex := 0 to FIFRSiteDataList.Count-1  do
    begin
      LIFRSiteData := TIFRSiteDataObject(FIFRSiteDataList[LIndex]);
      if(LIFRSiteData.SiteIdentifier = ASiteID) then
      begin
        Result := LIFRSiteData;
        Exit;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.Get_IFRSiteDataByIndex(AIndex: integer): TIFRSiteDataObject;
const OPNAME = 'TIFRSiteDataList.Get_IFRSiteDataByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FIFRSiteDataList.Count) then
      Result  := TIFRSiteDataObject(FIFRSiteDataList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.Get_IFRSiteDataCount: integer;
const OPNAME = 'TIFRSiteDataList.Get_IFRSiteDataCount';
begin
  Result := 0;
  try
    Result :=  FIFRSiteDataList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.Initialise: boolean;
const OPNAME = 'TIFRSiteDataList.Initialise';
begin
  Result := False;
  try
    FIFRSiteDataList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TIFRSiteDataList.Validate';
var
  LIndex : integer;
  LIFRSiteData:TIFRSiteDataObject;
begin
  Result := False;
  try
    for LIndex := 0 to FIFRSiteDataList.Count-1  do
    begin
      LIFRSiteData := TIFRSiteDataObject(FIFRSiteDataList[LIndex]);
      LIFRSiteData.Validate(AErrors,AContext);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.MaxSiteIdentifier: integer;
const OPNAME = 'TIFRSiteDataList.MaxSiteIdentifier';
var
  LIndex : integer;
  LIFRSiteData:TIFRSiteDataObject;
begin
  Result := 0;
  try
    for LIndex := 0 to FIFRSiteDataList.Count-1  do
    begin
      LIFRSiteData := IFRSiteDataByIndex[LIndex];
      if(LIFRSiteData.SiteIdentifier > Result) then
        Result := LIFRSiteData.SiteIdentifier;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.DeleteIFRSite(ASiteID: integer): boolean;
const OPNAME = 'TIFRSiteDataList.DeleteIFRSite';
var
  LIFRSiteData:TIFRSiteDataObject;
  LSQLAgent:TIFRDataSQLAgent;
begin
  Result := False;
  try
    LIFRSiteData := IFRSiteDataByIdentifier[ASiteID];
    if(LIFRSiteData <> nil) then
    begin
      LSQLAgent := TIFRDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.DeleteIFRData(LIFRSiteData);
        if Result then
         FIFRSiteDataList.Remove(LIFRSiteData);
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRSiteDataList.SaveIFRSite(ASiteID: integer): boolean;
const OPNAME = 'TIFRSiteDataList.SaveIFRSite';
var
  LIFRSiteData:TIFRSiteDataObject;
  LSQLAgent:TIFRDataSQLAgent;
begin
  Result := False;
  try
    LIFRSiteData := IFRSiteDataByIdentifier[ASiteID];
    if(LIFRSiteData <> nil) then
    begin
      LSQLAgent := TIFRDataSQLAgent.Create(FAppModules);
      try
        Result := LSQLAgent.SaveIFRData(LIFRSiteData);
        if Result then
          LIFRSiteData.Changed := False;
      finally
        LSQLAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TIFRModelData }

procedure TIFRModelData.CreateMemberObjects;
const OPNAME = 'TIFRModelData.CreateMemberObjects';
begin
  inherited;
  try
    FIFRSiteDataList  := TIFRSiteDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRModelData.DestroyMemberObjects;
const OPNAME = 'TIFRModelData.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FIFRSiteDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelData.Initialise: boolean;
const OPNAME = 'TIFRModelData.Initialise';
begin
  Result := False;
  try
    Result := FIFRSiteDataList.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TIFRModelData.Validate';
begin
  Result := False;
  try
    Result := FIFRSiteDataList.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelData.GetFileNamesObject: TAbstractModelFileNameList;
const OPNAME = 'TIFRModelData.GetFileNamesObject';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelData.GetFilesLineTypes: TAbstractFilesLineTypes;
const OPNAME = 'TIFRModelData.GetFilesLineTypes';
begin
  Result := nil;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelData.GetViewDataItems(AViewId: string; AItemsList: TViewModelDataItemsList; var AHandled: boolean): boolean;
const OPNAME = 'TIFRModelData.GetViewDataItems';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
