
//
//
//  UNIT      : Contains TDDTSInputData Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 07/04/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDDTSData;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  VCL.Controls,
  VoaimsCom_TLB,
  UReservoirData,
  UAbstractObject;

type
  TDDTSInputData = class(TAbstractAppObject)
  protected
    FIdentifier  : integer;
    FDailyDate : TDate;
    FRunoff : double;
    FOtherInflow : double;
    FRainfall : double;
    FEvaporation : double;
    FIncreamentalRunoff : double;
    FEWR : double;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_DailyDate: TDate;
    function Get_Runoff: double;
    function Get_OtherInflow : double;
    function Get_Rainfall : double;
    function Get_Evaporation : double;
    function Get_IncreamentalRunoff : double;
    function Get_EWR : double;

    procedure Set_Runoff(const AValue: double);
    procedure Set_DailyDate(const Value: TDate);
    procedure Set_OtherInflow(const Value: double);
    procedure Set_Rainfall(const Value: double);
    procedure Set_Evaporation(const Value: double);
    procedure Set_IncreamentalRunoff(const Value: double);
    procedure Set_EWR(const Value: double);
  public
    function Populate(AIdentifier : integer;
                     ADailyDate : TDate;
                     ARunoff : double;
                     AOtherInflow : double;
                     ARainfall : double;
                     AEvaporation : double;
                     AIncreamentalRunoff : double;
                     AEWR : double ) : boolean;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;

    function Initialise: Boolean; override;
    property Identifier : integer read FIdentifier;
    property DailyDate : TDate read Get_DailyDate;
    property Runoff : double   read Get_Runoff;
    property OtherInflow : double read Get_OtherInflow ;
    property Rainfall : double read Get_Rainfall;
    property Evaporation : double read Get_Evaporation;
    property IncreamentalRunoff : double read Get_IncreamentalRunoff;
    property EWR : double read Get_EWR;
  end;

  TDDTSOutputData = class(TAbstractAppObject)
  protected
    FRowIndex : integer;
    FIdentifier : integer;
    FColumnA  : TDateTime;
    FColumnB  : Double;
    FColumnC  : Double;
    FColumnD  : Double;
    FColumnE  : Double;
    FColumnF  : Double;
    FColumnG  : Double;
    FColumnH  : Double;
    FColumnI  : Double;
    FColumnJ  : Double;
    FColumnK  : Double;
    FColumnL  : Double;
    FColumnM  : Double;
    FColumnN  : Double;
    FColumnO  : Double;
    FColumnP  : Double;
    FColumnQ  : Double;
    FColumnR  : Double;
    FColumnS  : Double;
    FColumnT  : Double;
    FColumnU  : Double;
    FColumnV  : Double;
    FColumnW  : Double;
    FColumnX  : Double;
    FColumnY  : Double;
    FColumnZ  : Double;
    FColumnAA : Double;
    FColumnAB : Double;
    FColumnAC : Double;
    FColumnAD : Double;
    FColumnAE : Double;
    FColumnAF : Double;
    FColumnAG : Double;
    FColumnAH : Double;
    FColumnAI : Double;
    FColumnAJ : Double;
    FColumnAK : Double;
    FColumnAL : Double;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function Initialise: Boolean; override;
    function SaveToCSVString: string;
    function LoadFromSVString(ACSVString: string):boolean;

    property RowIndex   : integer   read FRowIndex   write  FRowIndex;
    property Identifier : integer   read FIdentifier write  FIdentifier;
    property ColumnA  : TDateTime read FColumnA  write  FColumnA;
    property ColumnB  : Double    read FColumnB  write  FColumnB;
    property ColumnC  : Double    read FColumnC  write  FColumnC;
    property ColumnD  : Double    read FColumnD  write  FColumnD;
    property ColumnE  : Double    read FColumnE  write  FColumnE;
    property ColumnF  : Double    read FColumnF  write  FColumnF;
    property ColumnG  : Double    read FColumnG  write  FColumnG;
    property ColumnH  : Double    read FColumnH  write  FColumnH;
    property ColumnI  : Double    read FColumnI  write  FColumnI;
    property ColumnJ  : Double    read FColumnJ  write  FColumnJ;
    property ColumnK  : Double    read FColumnK  write  FColumnK;
    property ColumnL  : Double    read FColumnL  write  FColumnL;
    property ColumnM  : Double    read FColumnM  write  FColumnM;
    property ColumnN  : Double    read FColumnN  write  FColumnN;
    property ColumnO  : Double    read FColumnO  write  FColumnO;
    property ColumnP  : Double    read FColumnP  write  FColumnP;
    property ColumnQ  : Double    read FColumnQ  write  FColumnQ;
    property ColumnR  : Double    read FColumnR  write  FColumnR;
    property ColumnS  : Double    read FColumnS  write  FColumnS;
    property ColumnT  : Double    read FColumnT  write  FColumnT;
    property ColumnU  : Double    read FColumnU  write  FColumnU;
    property ColumnV  : Double    read FColumnV  write  FColumnV;
    property ColumnW  : Double    read FColumnW  write  FColumnW;
    property ColumnX  : Double    read FColumnX  write  FColumnX;
    property ColumnY  : Double    read FColumnY  write  FColumnY;
    property ColumnZ  : Double    read FColumnZ  write  FColumnZ;
    property ColumnAA : Double    read FColumnAA write  FColumnAA;
    property ColumnAB : Double    read FColumnAB write  FColumnAB;
    property ColumnAC : Double    read FColumnAC write  FColumnAC;
    property ColumnAD : Double    read FColumnAD write  FColumnAD;
    property ColumnAE : Double    read FColumnAE write  FColumnAE;
    property ColumnAF : Double    read FColumnAF write  FColumnAF;
    property ColumnAG : Double    read FColumnAG write  FColumnAG;
    property ColumnAH : Double    read FColumnAH write  FColumnAH;
    property ColumnAI : Double    read FColumnAI write  FColumnAI;
    property ColumnAJ : Double    read FColumnAJ write  FColumnAJ;
    property ColumnAK : Double    read FColumnAK write  FColumnAK;
    property ColumnAL : Double    read FColumnAL write  FColumnAL;
  end;

  TDDTSOutputDataList = class(TAbstractAppObject)
  protected
    FDataList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_Count : integer;
    function Get_OutputDataByIndex(AIndex : integer) : TDDTSOutputData;
  public
    function AddOutputData : TDDTSOutputData;
    function Initialise: Boolean; override;
    property OutputDataByIndex[AIndex : integer] : TDDTSOutputData read Get_OutputDataByIndex;
    property Count : integer read Get_Count;
  end;

  TDDTSDetailData = class(TAbstractAppObject)
  protected
    FDamDescription : string;
    FIdentifier : integer;
    FRunoffScale : double;
    FOtherInflowScale : double;
    FEWRScale : double;
    FTargetDraft : double;
    FDSRequiments: double;

    FImportHeading : string;

    FMinRunoff : double;
    FMaxRunoff : double;
    FMinOtherInflow : double;
    FMaxOtherInflow : double;
    FMinRainfall : double;
    FMaxRainfall : double;
    FMinEvaporation : double;
    FMaxEvaporation : double;
    FMinIncreamentalRunoff : double;
    FMaxIncreamentalRunoff : double;
    FMinEWR : double;
    FMaxEWR : double;

    FDSPercRelease : double;
    FSpillPercRelease : double;
    FEWRPercRelease : double;

    FDDTSInputDataObject : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function Get_RunoffScale : double;
    procedure Set_RunoffScale(AValue : double);

    function Get_OtherInflowScale : double;
    procedure Set_OtherInflowScale(AValue : double);

    function Get_EWRScale : double;
    procedure Set_EWRScale(AValue : double);

    function Get_TargetDraft : double;
    procedure Set_TargetDraft(AValue : double);

    function Get_DSRequiments : double;
    procedure Set_DSRequiments(AValue : double);

    function Get_DSPercRelease : double;
    procedure Set_DSPercRelease(AValue : double);

    function Get_SpillPercRelease : double;
    procedure Set_SpillPercRelease(AValue : double);

    function Get_EWRPercRelease : double;
    procedure Set_EWRPercRelease(AValue : double);
    function Get_ImportHeading : string;
    procedure Set_ImportHeading(AValue : string);

    function Get_MinRunoff : double;
    procedure Set_MinRunoff(AValue : double);
    function Get_MaxRunoff : double;
    procedure Set_MaxRunoff(AValue : double);
    function Get_MinOtherInflow : double;
    procedure Set_MinOtherInflow(AValue : double);
    function Get_MaxOtherInflow : double;
    procedure Set_MaxOtherInflow(AValue : double);
    function Get_MinRainfall : double;
    procedure Set_MinRainfall(AValue : double);
    function Get_MaxRainfall : double;
    procedure Set_MaxRainfall(AValue : double);
    function Get_MinEvaporation : double;
    procedure Set_MinEvaporation(AValue : double);
    function Get_MaxEvaporation : double;
    procedure Set_MaxEvaporation(AValue : double);
    function Get_MinIncreamentalRunoff : double;
    procedure Set_MinIncreamentalRunoff(AValue : double);
    function Get_MaxIncreamentalRunoff : double;
    procedure Set_MaxIncreamentalRunoff(AValue : double);
    function Get_MinEWR : double;
    procedure Set_MinEWR(AValue : double);
    function Get_MaxEWR : double;
    procedure Set_MaxEWR(AValue : double);


    function ValidateRunoffScale(AErrorMessages: TStrings): boolean;
    function ValidateOtherInflowScale(AErrorMessages: TStrings): boolean;
    function ValidateEWRScale(AErrorMessages: TStrings): boolean;
    function ValidateTargetDraft(AErrorMessages: TStrings): boolean;
    function ValidateDSRequiments(AErrorMessages: TStrings): boolean;

    function ValidateDSPercRelease(AErrorMessages: TStrings): boolean;
    function ValidateSpillPercRelease(AErrorMessages: TStrings): boolean;
    function ValidateEWRPercRelease(AErrorMessages: TStrings): boolean;

    function ValidateMinRunoff(AErrorMessages: TStrings): boolean;
    function ValidateMaxRunoff(AErrorMessages: TStrings): boolean;
    function ValidateMinOtherInflow (AErrorMessages: TStrings): boolean;
    function ValidateMaxOtherInflow(AErrorMessages: TStrings): boolean;
    function ValidateMinRainfall(AErrorMessages: TStrings): boolean;
    function ValidateMaxRainfall(AErrorMessages: TStrings): boolean;
    function ValidateMinEvaporation(AErrorMessages: TStrings): boolean;
    function ValidateMaxEvaporation(AErrorMessages: TStrings): boolean;
    function ValidateMinIncreamentalRunoff(AErrorMessages: TStrings): boolean;
    function ValidateMaxIncreamentalRunoff(AErrorMessages: TStrings): boolean;
    function ValidateMinEWR(AErrorMessages: TStrings): boolean;
    function ValidateMaxEWR(AErrorMessages: TStrings): boolean;


  public
    function Populate(ADamDescription : string;
                      AIdentifier : integer;
                      ARunoffScale : double;
                      AOtherInflowScale : double;
                      AEWRScale : double;
                      ATargetDraft : double;
                      ADSRequiments: double;
                      ADSPercRelease : double;
                      ASpillPercRelease : double;
                      AEWRPercRelease : double; AImportHeadlines : string):boolean;
    function PopulateMinMax(
                      AMinRunoff : double;
                      AMaxRunoff : double;
                      AMinOtherInflow : double;
                      AMaxOtherInflow : double;
                      AMinRainfall : double;
                      AMaxRainfall : double;
                      AMinEvaporation : double;
                      AMaxEvaporation : double;
                      AMinIncreamentalRunoff : double;
                      AMaxIncreamentalRunoff : double;
                      AMinEWR : double;
                      AMaxEWR : double):boolean;

    function Validate(var AErrors: WideString; const AContext: WideString): WordBool;
    function GetDDTSInputDataByDate(ADate : TDate): TDDTSInputData;
    function AddDDTSInputData: TDDTSInputData;
    function GetDDTSInputDataByIndex(AIndex : integer): TDDTSInputData;
    function GetDDTSInputDataByIdentifier(AIdentifier : integer): TDDTSInputData;
    function GetDDTSInputDataCount : integer;
    function Initialise: Boolean; override;
    property Identifier : integer read FIdentifier write FIdentifier;
    property DamDescription : string read FDamDescription write FDamDescription;
    property RunoffScale : double read Get_RunoffScale write Set_RunoffScale;
    property OtherInflowScale : double read Get_OtherInflowScale write Set_OtherInflowScale;
    property EWRScale : double read Get_EWRScale write Set_EWRScale;
    property TargetDraft : double read Get_TargetDraft write Set_TargetDraft;
    property DSRequiments: double read Get_DSRequiments write Set_DSRequiments;

    property DSPercRelease: double read Get_DSPercRelease write Set_DSPercRelease;
    property SpillPercRelease: double read Get_SpillPercRelease write Set_SpillPercRelease;
    property EWRPercRelease: double read Get_EWRPercRelease write Set_EWRPercRelease;

    property ImportHeading : string read Get_ImportHeading write Set_ImportHeading;

    property MinRunoff : double read Get_MinRunoff write Set_MinRunoff;
    property MaxRunoff: double read Get_MaxRunoff write Set_MaxRunoff;
    property MinOtherInflow : double read Get_MinOtherInflow write Set_MinOtherInflow;
    property MaxOtherInflow : double read Get_MaxOtherInflow write Set_MaxOtherInflow;
    property MinRainfall : double read Get_MinRainfall write Set_MinRainfall;
    property MaxRainfall : double read Get_MaxRainfall write Set_MaxRainfall;
    property MinEvaporation : double read Get_MinEvaporation write Set_MinEvaporation;
    property MaxEvaporation : double read Get_MaxEvaporation write Set_MaxEvaporation;
    property MinIncreamentalRunoff : double read Get_MinIncreamentalRunoff write Set_MinIncreamentalRunoff;
    property MaxIncreamentalRunoff : double read Get_MaxIncreamentalRunoff write Set_MaxIncreamentalRunoff;
    property MinEWR : double read Get_MinEWR write Set_MinEWR ;
    property MaxEWR : double read Get_MaxEWR write Set_MaxEWR;


  end;

  TDDTSDamDataList = class(TAbstractAppObject)
  protected
    FDDTSDamDetailDataObject : TObjectList;
    FReservoirList: TReservoirDataList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Get_ReservoirDataList : TReservoirDataList;
  public
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool;
    function AddDDTSDetailData : TDDTSDetailData;
    function CreateDDTSDetailData(ADamName : string): TDDTSDetailData;
    function RemoveDDTSDetailData(AIdentifier : integer): boolean;
    function GetDDTSDetailDataByIndex(AIndex : integer): TDDTSDetailData;
    function GetDDTSDetailDataByIdentifier(AIdentifier : integer): TDDTSDetailData;
    function GetDDTSDetailDataCount : integer;
    function Initialise: Boolean; override;
    property CastReservoirList : TReservoirDataList read Get_ReservoirDataList;

  end;

implementation

uses UConstants,
     UDDTSDamDataLoadAgent,
     UDDTSDamDataSQLAgent,
     UErrorHandlingOperations;
{ TDDTSDamDataList }

function TDDTSDamDataList.AddDDTSDetailData: TDDTSDetailData;
const OPNAME = 'TDDTSDamDataList.AddDDTSDetailData';
begin
  Result := nil;
  try
    Result := TDDTSDetailData.Create(FAppModules);
    FDDTSDamDetailDataObject.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.CreateDDTSDetailData(ADamName : string): TDDTSDetailData;
const OPNAME = 'TDDTSDamDataList.CreateDDTSDetailData';
var
  LReservoirData : TReservoirData;
  LDDTSDetailData : TDDTSDetailData;
  LLoadAgent : TDDTSDamDataLoadAgent;
begin
  Result := nil;
  try
    LLoadAgent := TDDTSDamDataLoadAgent.Create(FAppModules);
    try
      LReservoirData := LLoadAgent.CreateReservoirData(Self);
      if LReservoirData <> nil then
      begin
        LDDTSDetailData := AddDDTSDetailData;
        if LDDTSDetailData <> nil then
        begin

          if LLoadAgent.CreateDamDailyData(LReservoirData.ReservoirConfigurationData.RecordIdentifier) then
            LReservoirData.ReservoirConfigurationData.ReservoirName := ADamName;
        end;
      end;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSDamDataList.CreateMemberObjects;
const OPNAME = 'TDDTSDamDataList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDDTSDamDetailDataObject := TObjectList.Create(False);
    FReservoirList := TReservoirDataList.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDamDataList.DestroyMemberObjects;
const OPNAME = 'TDDTSDamDataList.DestroyMemberObjects';
begin
 try
   inherited DestroyMemberObjects;
 except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.GetDDTSDetailDataByIdentifier(AIdentifier: integer): TDDTSDetailData;
const OPNAME = 'TDDTSDamDataList.GetDDTSDetailDataByIdentifier';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FDDTSDamDetailDataObject.Count - 1 do
    begin
      if (TDDTSDetailData(FDDTSDamDetailDataObject.Items[LIndex]).FIdentifier = AIdentifier) then
      begin
        Result := TDDTSDetailData(FDDTSDamDetailDataObject.Items[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.GetDDTSDetailDataByIndex(AIndex: integer): TDDTSDetailData;
const OPNAME = 'TDDTSDamDataList.GetDDTSDetailDataByIndex';
begin
  Result := nil;
  try
   if(AIndex >= 0) and (AIndex < FDDTSDamDetailDataObject.Count) then
      Result := TDDTSDetailData(FDDTSDamDetailDataObject.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.GetDDTSDetailDataCount: integer;
const OPNAME = 'TDDTSDamDataList.GetDDTSDetailDataByIdentifier';
begin
  Result := 0;
  try
    Result := FDDTSDamDetailDataObject.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.Get_ReservoirDataList: TReservoirDataList;
const OPNAME = 'TDDTSDamDataList.Get_ReservoirDataList';
begin
  Result := nil;
  try
    Result := FReservoirList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.Initialise: Boolean;
const OPNAME = 'TDDTSDamDataList.Initialise';
begin
  Result := False;
  try
    FDDTSDamDetailDataObject.Clear;
    FReservoirList.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.RemoveDDTSDetailData(AIdentifier: integer): boolean;
const OPNAME = 'TDDTSDamDataList.CreateDDTSDetailData';
begin
  Result := False;
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList.Validate(var AErrors: WideString;
  const AContext: WideString): WordBool;
const OPNAME = 'TDDTSDamDataList.Validate';
var
  LReservoirData : IReservoirData;
begin
  Result := False;
  try
    LReservoirData := CastReservoirList.ReservoirByIndex[0];
    if LReservoirData <> nil then
      Result    := LReservoirData.Validate(AErrors,AContext) and
                 GetDDTSDetailDataByIndex(0).Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDamDataList._AddRef: Integer;
const OPNAME = 'TDDTSDetailData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
function TDDTSDamDataList._Release: Integer;
const OPNAME = 'TDDTSDetailData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDDTSDetailData }

function TDDTSDetailData.AddDDTSInputData: TDDTSInputData;
const OPNAME = 'TDDTSDetailData.AddDDTSInputData';
begin
  Result := nil;
  try
    Result := TDDTSInputData.Create(FAppModules);
    FDDTSInputDataObject.Add(Result);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.CreateMemberObjects;
const OPNAME = 'TDDTSDetailData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FDDTSInputDataObject := TObjectList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.DestroyMemberObjects;
const OPNAME = 'TDDTSDetailData.CreateMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FDDTSInputDataObject);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.GetDDTSInputDataByDate(ADate: TDate): TDDTSInputData;
const OPNAME = 'TDDTSDetailData.GetDDTSInputDataByDate';
begin
  Result := nil;
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.GetDDTSInputDataByIdentifier(AIdentifier: integer): TDDTSInputData;
const OPNAME = 'TDDTSDetailData.GetDDTSInputDataByIdentifier';
begin
  Result := nil;
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.GetDDTSInputDataByIndex(AIndex: integer): TDDTSInputData;
const OPNAME = 'TDDTSDetailData.GetDDTSInputDataByIdentifier';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FDDTSInputDataObject.Count) then
      Result := TDDTSInputData(FDDTSInputDataObject[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.GetDDTSInputDataCount: integer;
const OPNAME = 'TDDTSDetailData.GetDDTSInputDataCount';
begin
  Result := 0;
  try
    Result := FDDTSInputDataObject.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_DSPercRelease: double;
const OPNAME = 'TDDTSDetailData.Get_DSPercRelease';
begin
  Result := 0;
  try
    Result := FDSPercRelease;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_DSRequiments: double;
const OPNAME = 'TDDTSDetailData.Get_DSRequiments';
begin
  Result := 0;
  try
    Result := FDSRequiments;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_EWRPercRelease: double;
const OPNAME = 'TDDTSDetailData.Get_EWRPercRelease';
begin
  Result := 0;
  try
    Result := FEWRPercRelease;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_EWRScale: double;
const OPNAME = 'TDDTSDetailData.Get_EWRScale';
begin
  Result := 0;
  try
    Result := FEWRScale;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Get_OtherInflowScale: double;
const OPNAME = 'TDDTSDetailData.Get_OtherInflowScale';
begin
  Result := 0;
  try
    Result := FOtherInflowScale;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_RunoffScale: double;
const OPNAME = 'TDDTSDetailData.Get_RunoffScale';
begin
  Result := 0;
  try
    Result := FRunoffScale;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_SpillPercRelease: double;
const OPNAME = 'TDDTSDetailData.Get_SpillPercRelease';
begin
  Result := 0;
  try
    Result := FSpillPercRelease;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_TargetDraft: double;
const OPNAME = 'TDDTSDetailData.Get_TargetDraft';
begin
  Result := 0;
  try
    Result := FTargetDraft;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_ImportHeading: string;
const OPNAME = 'TDDTSDetailData.Get_ImportHeading';
begin
  Result := ' ';
  try
    Result := FImportHeading;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MinEvaporation: double;
const OPNAME = 'TDDTSDetailData.Get_MinEvaporation';
begin
  Result := 0;
  try
    Result := FMinEvaporation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MinEWR: double;
const OPNAME = 'TDDTSDetailData.Get_MinEWR';
begin
  Result := 0;
  try
    Result := FMinEWR;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Get_MinIncreamentalRunoff: double;
const OPNAME = 'TDDTSDetailData.Get_MinIncreamentalRunoff';
begin
  Result := 0;
  try
    Result := FMinIncreamentalRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Get_MinOtherInflow: double;
const OPNAME = 'TDDTSDetailData.Get_MinOtherInflow';
begin
  Result := 0;
  try
    Result := FMinOtherInflow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MinRainfall: double;
const OPNAME = 'TDDTSDetailData.Get_MinRainfall';
begin
  Result := 0;
  try
    Result := FMinRainfall;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MinRunoff: double;
const OPNAME = 'TDDTSDetailData.Get_MinRunoff';
begin
  Result := 0;
  try
    Result := FMinRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Get_MaxEvaporation: double;
const OPNAME = 'TDDTSDetailData.Get_MaxEvaporation';
begin
  Result := 0;
  try
    Result := FMaxEvaporation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MaxEWR: double;
const OPNAME = 'TDDTSDetailData.Get_MaxEWR';
begin
  Result := 0;
  try
    Result := FMaxEWR;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MaxIncreamentalRunoff: double;
const OPNAME = 'TDDTSDetailData.Get_MaxIncreamentalRunoff';
begin
  Result := 0;
  try
    Result := FMaxIncreamentalRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Get_MaxOtherInflow: double;
const OPNAME = 'TDDTSDetailData.Get_MaxOtherInflow';
begin
  Result := 0;
  try
    Result := FMaxOtherInflow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MaxRainfall: double;
const OPNAME = 'TDDTSDetailData.Get_MaxRainfall';
begin
  Result := 0;
  try
    Result := FMaxRainfall;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Get_MaxRunoff: double;
const OPNAME = 'TDDTSDetailData.Get_MaxRunoff';
begin
  Result := 0;
  try
    Result := FMaxRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.Initialise: Boolean;
const OPNAME = 'TDDTSDetailData.Get_TargetDraft';
begin
  Result := False;
  try
    FDamDescription := '';
    FIdentifier := 0;
    FRunoffScale := 0;
    FOtherInflowScale := 0;
    FEWRScale := 0;
    FTargetDraft := 0;
    FDSRequiments:= 0;
    FImportHeading := 'N';
    FDDTSInputDataObject.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Populate(ADamDescription: string; AIdentifier : integer;ARunoffScale,
  AOtherInflowScale, AEWRScale, ATargetDraft, ADSRequiments, ADSPercRelease,ASpillPercRelease,AEWRPercRelease: double;AImportHeadlines : string): boolean;
const OPNAME = 'TDDTSDetailData.Populate';
begin
  Result := False;
  try
    FDamDescription := ADamDescription;
    FIdentifier := AIdentifier;
    FRunoffScale := ARunoffScale;
    FOtherInflowScale := AOtherInflowScale;
    FEWRScale := AEWRScale;
    FTargetDraft := ATargetDraft;
    FDSRequiments:= ADSRequiments;
    FDSPercRelease := ADSPercRelease;
    FSpillPercRelease := ASpillPercRelease;
    FEWRPercRelease := AEWRPercRelease;
    FImportHeading := AImportHeadlines;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.PopulateMinMax(AMinRunoff, AMaxRunoff, AMinOtherInflow,
  AMaxOtherInflow, AMinRainfall, AMaxRainfall, AMinEvaporation, AMaxEvaporation,
  AMinIncreamentalRunoff, AMaxIncreamentalRunoff, AMinEWR,
  AMaxEWR: double): boolean;
const OPNAME = 'TDDTSDetailData.PopulateMinMax';
begin
  Result := False;
  try
    FMinRunoff := AMinRunoff;
    FMaxRunoff := AMaxRunoff;
    FMinOtherInflow := AMinOtherInflow;
    FMaxOtherInflow := AMaxOtherInflow;
    FMinRainfall := AMinRainfall;
    FMaxRainfall := AMaxRainfall;
    FMinEvaporation := AMinEvaporation;
    FMaxEvaporation := AMaxEvaporation;
    FMinIncreamentalRunoff := AMinIncreamentalRunoff;
    FMaxIncreamentalRunoff := AMaxIncreamentalRunoff;
    FMinEWR := AMinEWR;
    FMaxEWR := AMaxEWR;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSDetailData.Set_DSPercRelease(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_DSPercRelease';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DSPercRelease', FloatToStr(AValue), FloatToStr(FDSPercRelease), LContextData) then
          begin
            LOldValue := FDSPercRelease;
            FDSPercRelease := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DSPercRelease',FloatToStr(LOldValue),FloatToStr(FDSPercRelease));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_DSRequiments(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_DSRequiments';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'DSRequirment', FloatToStr(AValue), FloatToStr(FDSRequiments), LContextData) then
          begin
            LOldValue := FDSRequiments;
            FDSRequiments := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'DSRequirment',FloatToStr(LOldValue),FloatToStr(FDSRequiments));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_EWRPercRelease(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_EWRPercRelease';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'EWRPercRelease', FloatToStr(AValue), FloatToStr(FEWRPercRelease), LContextData) then
          begin
            LOldValue := FEWRPercRelease;
            FEWRPercRelease := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EWRPercRelease',FloatToStr(LOldValue),FloatToStr(FEWRPercRelease));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_EWRScale(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_EWRScale';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'EWRScaleFactor', FloatToStr(AValue), FloatToStr(FEWRScale), LContextData) then
          begin
            LOldValue := FEWRScale;
            FEWRScale := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'EWRScaleFactor',FloatToStr(LOldValue),FloatToStr(FEWRScale));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_OtherInflowScale(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_OtherInflowScale';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'OtherInflowScaleFactor', FloatToStr(AValue), FloatToStr(FOtherInflowScale), LContextData) then
          begin
            LOldValue := FOtherInflowScale;
            FOtherInflowScale := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'OtherInflowScaleFactor',FloatToStr(LOldValue),FloatToStr(FOtherInflowScale));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_RunoffScale(AValue: double);
const OPNAME = 'TDDTSDetailData.Get_RunoffScale';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'RunoffScaleFactor', FloatToStr(AValue), FloatToStr(FRunoffScale), LContextData) then
          begin
            LOldValue := FRunoffScale;
            FRunoffScale := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'RunoffScaleFactor',FloatToStr(LOldValue),FloatToStr(FRunoffScale));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_SpillPercRelease(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_SpillPercRelease';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'SpillPercRelease', FloatToStr(AValue), FloatToStr(FSpillPercRelease), LContextData) then
          begin
            LOldValue := FSpillPercRelease;
            FSpillPercRelease := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'SpillPercRelease',FloatToStr(LOldValue),FloatToStr(FSpillPercRelease));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_TargetDraft(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_TargetDraft';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'TargetDraft', FloatToStr(AValue), FloatToStr(FTargetDraft), LContextData) then
          begin
            LOldValue := FTargetDraft;
            FTargetDraft := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'TargetDraft',FloatToStr(LOldValue),FloatToStr(FTargetDraft));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.Validate(var AErrors: WideString;const AContext: WideString): WordBool;
const OPNAME = 'TReservoirData.Validate';
var
  LMessages         : TStringList;
  lErrorCols        : TStringList;
  LStopOnFirstError : boolean;
begin
  Result := True;
  try
    LMessages  := TStringList.Create;
    lErrorCols := TStringList.Create;
    try
      if(AContext = 'RunoffScaleFactor') then
        Result := ValidateRunoffScale(LMessages)
      else
      if(AContext = 'OtherInflowScaleFactor') then
        Result := ValidateOtherInflowScale(LMessages)
      else
      if(AContext = 'EWRScaleFactor') then
        Result := ValidateEWRScale(LMessages)
      else
      if(AContext = 'TargetDraft') then
        Result := ValidateTargetDraft(LMessages)
      else
      if(AContext = 'DSRequirment') then
        Result := ValidateDSRequiments(LMessages)
      else
      if(AContext = 'DSPercRelease') then
        Result := ValidateDSPercRelease(LMessages)
      else
      if(AContext = 'SpillPercRelease') then
        Result := ValidateSpillPercRelease(LMessages)
      else
      if(AContext = 'EWRPercRelease') then
        Result := ValidateEWRPercRelease(LMessages)
      else
      if(AContext = 'MinRunoff') then
        Result := ValidateMinRunoff(LMessages)
      else
      if(AContext = 'MaxRunoff') then
        Result := ValidateMaxRunoff(LMessages)
      else
      if(AContext = 'MinOtherInflow') then
        Result := ValidateMinOtherInflow(LMessages)
      else
      if(AContext = 'MaxOtherInflow') then
        Result := ValidateMaxOtherInflow(LMessages)
      else
      if(AContext = 'MinRainfall') then
        Result := ValidateMinRainfall(LMessages)
      else
      if(AContext = 'MaxRainfall') then
        Result := ValidateMaxRainfall(LMessages)
      else
      if(AContext = 'MinEvaporation') then
        Result := ValidateMinEvaporation(LMessages)
      else
      if(AContext = 'MaxEvaporation') then
        Result := ValidateMaxEvaporation(LMessages)
      else
      if(AContext = 'MinIncreamentalRunoff') then
        Result := ValidateMinIncreamentalRunoff(LMessages)
      else
      if(AContext = 'MaxIncreamentalRunoff') then
        Result := ValidateMaxIncreamentalRunoff(LMessages)
      else
      if(AContext = 'MinEWR') then
        Result := ValidateMinEWR(LMessages)
      else
      if(AContext = 'MaxEWR') then
        Result := ValidateMaxEWR(LMessages)
      else
      begin
        lStopOnFirstError := FAppModules.GlobalData.StopOnFirstErr;
        Result := False;
        if (not Result) and lStopOnFirstError then
          Exit;
        if not ValidateRunoffScale(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateOtherInflowScale(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateEWRScale(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateTargetDraft(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateDSRequiments(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateDSPercRelease(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateSpillPercRelease(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateEWRPercRelease(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinRunoff(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxRunoff(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinOtherInflow(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxRunoff(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinRainfall(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxRainfall(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinEvaporation(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxEvaporation(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinIncreamentalRunoff(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxIncreamentalRunoff(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMinEWR(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
        if not ValidateMaxEWR(LMessages) then
           Result := False;
        if (not Result) and lStopOnFirstError then
        begin
          AErrors := AErrors + LMessages.Text;
          Exit;
        end;
      end;
      AErrors := AErrors + LMessages.Text;
    finally
      LMessages.Free;
      lErrorCols.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateDSPercRelease(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateDSRequiments';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DSPercRelease',
            FloatToStr(FDSPercRelease), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FDSPercRelease)+ ':'+lMessage)
    else
    if (FDSPercRelease <= 0) then
      AErrorMessages.Add('ERROR:D/s percentage release must be greater than zero')
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateDSRequiments(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateDSRequiments';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('DSRequirment',
            FloatToStr(FDSRequiments), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FDSRequiments)+ ':'+lMessage)
    else
    if (FDSRequiments <= 0) then
      AErrorMessages.Add('ERROR:D/s Requirment must be greater than zero')
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateEWRPercRelease(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateEWRPercRelease';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('EWRPercRelease',
            FloatToStr(FEWRPercRelease), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FEWRPercRelease)+ ':'+lMessage)
    else
    if (FEWRPercRelease <= 0) then
      AErrorMessages.Add('ERROR:EWR Percentage Release must be greater than zero')
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateEWRScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateEWRScale';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('EWRScaleFactor',
            FloatToStr(FEWRScale), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FEWRScale)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxEvaporation(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxEvaporation';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxEvaporation',
            FloatToStr(FMaxEvaporation), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxEvaporation)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxEWR(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxEWR';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxEWR',
            FloatToStr(FMaxEWR), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxEWR)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxIncreamentalRunoff(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxIncreamentalRunoff';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxIncreamentalRunoff',
            FloatToStr(FMaxIncreamentalRunoff), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxIncreamentalRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxOtherInflow(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxOtherInflow';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxOtherInflow',
            FloatToStr(FMaxOtherInflow), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxOtherInflow)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxRainfall(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxRainfall';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxRainfall',
            FloatToStr(FMaxRainfall), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxRainfall)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMaxRunoff(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMaxRunoff';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MaxRunoff',
            FloatToStr(FMaxRunoff), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMaxRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMinEvaporation(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinEvaporation';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinEvaporation',
            FloatToStr(FMinEvaporation), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinEvaporation)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMinEWR(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinEWR';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinEWR',
            FloatToStr(FMinEWR), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinEWR)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMinIncreamentalRunoff(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinIncreamentalRunoff';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinIncreamentalRunoff',
            FloatToStr(FMinIncreamentalRunoff), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinIncreamentalRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSDetailData.ValidateMinOtherInflow(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinOtherInflow';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinOtherInflow',
            FloatToStr(FMinOtherInflow), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinOtherInflow)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMinRainfall(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinRainfall';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinRainfall',
            FloatToStr(FMinRainfall), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinRainfall)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateMinRunoff(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateMinRunoff';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('MinRunoff',
            FloatToStr(FMinRunoff), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FMinRunoff)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateOtherInflowScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateOtherInflowScale';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('OtherInflowScaleFactor',
            FloatToStr(FOtherInflowScale), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FOtherInflowScale)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateRunoffScale(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateRunoffScale';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('RunoffScaleFactor',
            FloatToStr(FRunoffScale), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FRunoffScale)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateSpillPercRelease(
  AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateDSRequiments';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('SpillPercRelease',
            FloatToStr(FSpillPercRelease), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FSpillPercRelease)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData.ValidateTargetDraft(AErrorMessages: TStrings): boolean;
const OPNAME = 'TDDTSDetailData.ValidateTargetDraft';
var
  LMessage : string;
begin
  Result := False;
  try
    lMessage := '';
    if (not FAppModules.FieldProperties.ValidateFieldProperty('TargetDraft',
            FloatToStr(FTargetDraft), LMessage)) then
      AErrorMessages.Add('ERROR:' +FloatToStr(FTargetDraft)+ ':'+lMessage)
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData._AddRef: Integer;
const OPNAME = 'TDDTSDetailData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSDetailData._Release: Integer;
const OPNAME = 'TDDTSDetailData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSDetailData.Set_ImportHeading(AValue: string);
const OPNAME = 'TDDTSDetailData.Set_ImportHeading';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : string;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'ImportHeadlines', AValue, FImportHeading, LContextData) then
          begin
            LOldValue := FImportHeading;
            FImportHeading := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'ImportHeadlines',LOldValue,FImportHeading);
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinEvaporation(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinEvaporation';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinEvaporation', FloatToStr(AValue), FloatToStr(FMinEvaporation), LContextData) then
          begin
            LOldValue := FMinEvaporation;
            FMinEvaporation := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinEvaporation',FloatToStr(LOldValue),FloatToStr(FMinEvaporation));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinEWR(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinEWR';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinEWR', FloatToStr(AValue), FloatToStr(FMinEWR), LContextData) then
          begin
            LOldValue := FMinEWR;
            FMinEWR := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinEWR',FloatToStr(LOldValue),FloatToStr(FMinEWR));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinIncreamentalRunoff(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinIncreamentalRunoff';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinIncreamentalRunoff', FloatToStr(AValue), FloatToStr(FMinIncreamentalRunoff), LContextData) then
          begin
            LOldValue := FMinIncreamentalRunoff;
            FMinIncreamentalRunoff := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinIncreamentalRunoff',FloatToStr(LOldValue),FloatToStr(FMinIncreamentalRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinOtherInflow(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinOtherInflow';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinOtherInflow', FloatToStr(AValue), FloatToStr(FMinOtherInflow), LContextData) then
          begin
            LOldValue := FMinOtherInflow;
            FMinOtherInflow := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinOtherInflow',FloatToStr(LOldValue),FloatToStr(FMinOtherInflow));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinRainfall(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinRainfall';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinRainfall', FloatToStr(AValue), FloatToStr(FMinRainfall), LContextData) then
          begin
            LOldValue := FMinRainfall;
            FMinRainfall := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinRainfall',FloatToStr(LOldValue),FloatToStr(FMinRainfall));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MinRunoff(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MinRunoff';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MinRunoff', FloatToStr(AValue), FloatToStr(FMinRunoff), LContextData) then
          begin
            LOldValue := FMinRunoff;
            FMinRunoff := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MinRunoff',FloatToStr(LOldValue),FloatToStr(FMinRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MaxEvaporation(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxEvaporation';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxEvaporation', FloatToStr(AValue), FloatToStr(FMaxEvaporation), LContextData) then
          begin
            LOldValue := FMaxEvaporation;
            FMaxEvaporation := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxEvaporation',FloatToStr(LOldValue),FloatToStr(FMaxEvaporation));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSDetailData.Set_MaxEWR(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxEWR';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxEWR', FloatToStr(AValue), FloatToStr(FMaxEWR), LContextData) then
          begin
            LOldValue := FMaxEWR;
            FMaxEWR := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxEWR',FloatToStr(LOldValue),FloatToStr(FMaxEWR));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MaxIncreamentalRunoff(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxIncreamentalRunoff';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxIncreamentalRunoff', FloatToStr(AValue), FloatToStr(FMaxIncreamentalRunoff), LContextData) then
          begin
            LOldValue := FMaxIncreamentalRunoff;
            FMaxIncreamentalRunoff := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxIncreamentalRunoff',FloatToStr(LOldValue),FloatToStr(FMaxIncreamentalRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MaxOtherInflow(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxOtherInflow';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxOtherInflow', FloatToStr(AValue), FloatToStr(FMaxOtherInflow), LContextData) then
          begin
            LOldValue := FMaxOtherInflow;
            FMaxOtherInflow := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxOtherInflow',FloatToStr(LOldValue),FloatToStr(FMaxOtherInflow));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MaxRainfall(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxRainfall';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxRainfall', FloatToStr(AValue), FloatToStr(FMaxRainfall), LContextData) then
          begin
            LOldValue := FMaxRainfall;
            FMaxRainfall := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxRainfall',FloatToStr(LOldValue),FloatToStr(FMaxRainfall));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSDetailData.Set_MaxRunoff(AValue: double);
const OPNAME = 'TDDTSDetailData.Set_MaxRunoff';
var
  LLoadAgent   : TDDTSDamDataSQLAgent;
  LContextData : TStringList;
  LOldValue    : double;
begin
  try
    LLoadAgent := TDDTSDamDataSQLAgent.Create(FAppModules);
    begin
      try
        LContextData := TStringList.Create;
        try
          LLoadAgent.LoadContextData(LContextData,FIdentifier);
          if FAppModules.FieldProperties.UpdateFieldValue(
             'MaxRunoff', FloatToStr(AValue), FloatToStr(FMaxRunoff), LContextData) then
          begin
            LOldValue := FMaxRunoff;
            FMaxRunoff := AValue;
            FAppModules.Model.StudyDataHasChanged(sdccEdit,'MaxRunoff',FloatToStr(LOldValue),FloatToStr(FMaxRunoff));
          end;
        finally
          LContextData.Free;
        end;
      finally
        LLoadAgent.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDDTSInputData }

procedure TDDTSInputData.CreateMemberObjects;
const OPNAME = 'TDDTSInputData.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSInputData.DestroyMemberObjects;
const OPNAME = 'TDDTSInputData.CreateMemberObjects';
begin
  try
    inherited DestroyMemberObjects;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSInputData.Get_DailyDate: TDate;
const OPNAME = 'TDDTSDetailData.Get_DailyDate';
begin
  Result := 0;
  try
    Result := FDailyDate;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_Evaporation: double;
const OPNAME = 'TDDTSDetailData.Get_Evaporation';
begin
  Result := 0;
  try
    Result := FEvaporation;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_EWR: double;
const OPNAME = 'TDDTSDetailData.Get_EWR';
begin
  Result := 0;
  try
    Result := FEWR;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_IncreamentalRunoff: double;
const OPNAME = 'TDDTSDetailData.Get_IncreamentalRunoff';
begin
  Result := 0;
  try
    Result := FIncreamentalRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_OtherInflow: double;
const OPNAME = 'TDDTSDetailData.Get_OtherInflow';
begin
  Result := 0;
  try
    Result := FOtherInflow;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_Rainfall: double;
const OPNAME = 'TDDTSDetailData.Get_Rainfall';
begin
  Result := 0;
  try
    Result := FRainfall;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Get_Runoff: double;
const OPNAME = 'TDDTSDetailData.Get_Runoff';
begin
  Result := 0;
  try
    Result := FRunoff;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSInputData.Initialise: Boolean;
const OPNAME = 'TDDTSInputData.Initialise';
begin
  Result := False;
  try

    FIdentifier  := 0;
    FDailyDate:= 0;
    FRunoff:= 0;
    FOtherInflow:= 0;
    FRainfall:= 0;
    FEvaporation := 0;
    FIncreamentalRunoff:= 0;
    FEWR:= 0;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData.Populate(AIdentifier: integer; ADailyDate: TDate;
  ARunoff, AOtherInflow, ARainfall, AEvaporation, AIncreamentalRunoff,
  AEWR: double): boolean;
const OPNAME = 'TDDTSInputData.Populate';
begin
  Result := False;
  try

    FIdentifier  := AIdentifier;
    FDailyDate:= ADailyDate;
    FRunoff:= ARunoff;
    FOtherInflow:= AOtherInflow;
    FRainfall:= ARainfall;
    FEvaporation := AEvaporation;
    FIncreamentalRunoff:= AIncreamentalRunoff;
    FEWR:= AEWR;
    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSInputData.Set_DailyDate(const Value: TDate);
begin

end;

procedure TDDTSInputData.Set_Evaporation(const Value: double);
begin

end;

procedure TDDTSInputData.Set_EWR(const Value: double);
begin

end;

procedure TDDTSInputData.Set_IncreamentalRunoff(const Value: double);
begin

end;

procedure TDDTSInputData.Set_OtherInflow(const Value: double);
begin

end;

procedure TDDTSInputData.Set_Rainfall(const Value: double);
begin

end;

procedure TDDTSInputData.Set_Runoff(const AValue: double);
begin

end;

function TDDTSInputData.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
begin

end;

function TDDTSInputData._AddRef: Integer;
const OPNAME = 'TDDTSDetailData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputData._Release: Integer;
const OPNAME = 'TDDTSDetailData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


{ TDDTSOutputData }

function TDDTSOutputData.Initialise: Boolean;
const OPNAME = 'TDDTSOutputData.Initialise';
begin
  Result := False;
  try
    FRowIndex   := NullInteger;
    FIdentifier := NullInteger;
    FColumnA  := NullDateTime;
    FColumnB  := NullFloat;
    FColumnC  := NullFloat;
    FColumnD  := NullFloat;
    FColumnE  := NullFloat;
    FColumnF  := NullFloat;
    FColumnG  := NullFloat;
    FColumnH  := NullFloat;
    FColumnI  := NullFloat;
    FColumnJ  := NullFloat;
    FColumnK  := NullFloat;
    FColumnL  := NullFloat;
    FColumnM  := NullFloat;
    FColumnN  := NullFloat;
    FColumnO  := NullFloat;
    FColumnP  := NullFloat;
    FColumnQ  := NullFloat;
    FColumnR  := NullFloat;
    FColumnS  := NullFloat;
    FColumnT  := NullFloat;
    FColumnU  := NullFloat;
    FColumnV  := NullFloat;
    FColumnW  := NullFloat;
    FColumnX  := NullFloat;
    FColumnY  := NullFloat;
    FColumnZ  := NullFloat;
    FColumnAA := NullFloat;
    FColumnAB := NullFloat;
    FColumnAC := NullFloat;
    FColumnAD := NullFloat;
    FColumnAE := NullFloat;
    FColumnAF := NullFloat;
    FColumnAG := NullFloat;
    FColumnAH := NullFloat;
    FColumnAI := NullFloat;
    FColumnAJ := NullFloat;
    FColumnAK := NullFloat;
    FColumnAL := NullFloat;
    Result    := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputData.LoadFromSVString(ACSVString: string): boolean;
const OPNAME = 'TDDTSDetailData.LoadFromSVString';
var
  LLineData  : TStringlist;
begin
  Result := False;
  try
    LLineData  := TStringlist.Create;
    try
      LLineData.CommaText := Trim(ACSVString);
      if(LLineData.Count > 00) then FColumnA  := StrToDateDef(LLineData[00],0.0);
      if(LLineData.Count > 01) then FColumnB  := StrToFloatDef(LLineData[01],0.0);
      if(LLineData.Count > 02) then FColumnC  := StrToFloatDef(LLineData[02],0.0);
      if(LLineData.Count > 03) then FColumnD  := StrToFloatDef(LLineData[03],0.0);
      if(LLineData.Count > 04) then FColumnE  := StrToFloatDef(LLineData[04],0.0);
      if(LLineData.Count > 05) then FColumnF  := StrToFloatDef(LLineData[05],0.0);
      if(LLineData.Count > 06) then FColumnG  := StrToFloatDef(LLineData[06],0.0);
      if(LLineData.Count > 07) then FColumnH  := StrToFloatDef(LLineData[07],0.0);
      if(LLineData.Count > 08) then FColumnI  := StrToFloatDef(LLineData[08],0.0);
      if(LLineData.Count > 09) then FColumnJ  := StrToFloatDef(LLineData[09],0.0);
      if(LLineData.Count > 10) then FColumnK  := StrToFloatDef(LLineData[10],0.0);
      if(LLineData.Count > 11) then FColumnL  := StrToFloatDef(LLineData[11],0.0);
      if(LLineData.Count > 12) then FColumnM  := StrToFloatDef(LLineData[12],0.0);
      if(LLineData.Count > 13) then FColumnN  := StrToFloatDef(LLineData[13],0.0);
      if(LLineData.Count > 14) then FColumnO  := StrToFloatDef(LLineData[14],0.0);
      if(LLineData.Count > 15) then FColumnP  := StrToFloatDef(LLineData[15],0.0);
      if(LLineData.Count > 16) then FColumnQ  := StrToFloatDef(LLineData[16],0.0);
      if(LLineData.Count > 17) then FColumnR  := StrToFloatDef(LLineData[17],0.0);
      if(LLineData.Count > 18) then FColumnS  := StrToFloatDef(LLineData[18],0.0);
      if(LLineData.Count > 19) then FColumnT  := StrToFloatDef(LLineData[19],0.0);
      if(LLineData.Count > 20) then FColumnU  := StrToFloatDef(LLineData[20],0.0);
      if(LLineData.Count > 21) then FColumnV  := StrToFloatDef(LLineData[21],0.0);
      if(LLineData.Count > 22) then FColumnW  := StrToFloatDef(LLineData[22],0.0);
      if(LLineData.Count > 23) then FColumnX  := StrToFloatDef(LLineData[23],0.0);
      if(LLineData.Count > 24) then FColumnY  := StrToFloatDef(LLineData[24],0.0);
      if(LLineData.Count > 25) then FColumnZ  := StrToFloatDef(LLineData[25],0.0);
      if(LLineData.Count > 26) then FColumnAA := StrToFloatDef(LLineData[26],0.0);
      if(LLineData.Count > 27) then FColumnAB := StrToFloatDef(LLineData[27],0.0);
      if(LLineData.Count > 28) then FColumnAC := StrToFloatDef(LLineData[28],0.0);
      if(LLineData.Count > 29) then FColumnAD := StrToFloatDef(LLineData[29],0.0);
      if(LLineData.Count > 30) then FColumnAE := StrToFloatDef(LLineData[30],0.0);
      if(LLineData.Count > 31) then FColumnAF := StrToFloatDef(LLineData[31],0.0);
      if(LLineData.Count > 32) then FColumnAG := StrToFloatDef(LLineData[32],0.0);
      if(LLineData.Count > 33) then FColumnAH := StrToFloatDef(LLineData[33],0.0);
      if(LLineData.Count > 34) then FColumnAI := StrToFloatDef(LLineData[34],0.0);
      if(LLineData.Count > 35) then FColumnAJ := StrToFloatDef(LLineData[35],0.0);
      if(LLineData.Count > 36) then FColumnAK := StrToFloatDef(LLineData[36],0.0);
      if(LLineData.Count > 37) then FColumnAL := StrToFloatDef(LLineData[37],0.0);
    finally
      LLineData.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputData.SaveToCSVString: string;
const OPNAME = 'TDDTSDetailData._AddRef';
var
  LLineData  : TStringlist;
begin
  Result := '';
  try
    LLineData  := TStringlist.Create;
    try
      LLineData.Add(FormatDateTime('yyyy/mm/dd',FColumnA));
      if FColumnB = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnB));
      if FColumnC = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnC));
      if FColumnD = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnD));
      if FColumnE = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnE));
      if FColumnF = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnF));
      if FColumnG = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnG));
      if FColumnH = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnH));
      if FColumnI = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnI));
      if FColumnJ = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnJ));
      if FColumnK = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnK));
      if FColumnL = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnL));
      if FColumnM = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnM));
      if FColumnN = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnN));
      if FColumnO = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnO));
      if FColumnP = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnP));
      if FColumnQ = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnQ));
      if FColumnR = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnR));
      if FColumnS = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnS));
      if FColumnT = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnT));
      if FColumnU = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnU));
      if FColumnV = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnV));
      if FColumnW = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnW));
      if FColumnX = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnX));
      if FColumnY = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnY));
      if FColumnZ = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnZ));
      if FColumnAA = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAA));
      if FColumnAB = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAB));
      if FColumnAC = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAC));
      if FColumnAD = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAD));
      if FColumnAE = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAE));
      if FColumnAF = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAF));
      if FColumnAG = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAG));
      if FColumnAH = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAH));
      if FColumnAI = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAI));
      if FColumnAJ = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAJ));
      if FColumnAK = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAK));
      if FColumnAL = NullFloat then
        LLineData.Add('')
      else
        LLineData.Add(FormatFloat('##0.000',FColumnAL));
      Result    := LLineData.CommaText;
    finally
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputData._AddRef: Integer;
const OPNAME = 'TDDTSDetailData._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputData._Release: Integer;
const OPNAME = 'TDDTSDetailData._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TDDTSOutputDataList }

function TDDTSOutputDataList._AddRef: Integer;
const OPNAME = 'TDDTSOutputDataList._AddRef';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputDataList._Release: Integer;
const OPNAME = 'TDDTSOutputDataList._Release';
begin
  Result := 0;
  try
    FRefCount := 1;
    Result    := FRefCount;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSOutputDataList.CreateMemberObjects;
const OPNAME = 'TDDTSOutputDataList.CreateMemberObjects';
begin
  inherited;
  try
    FDataList := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSOutputDataList.DestroyMemberObjects;
const OPNAME = 'TDDTSOutputDataList.CreateMemberObjects';
begin
  try
    FreeAndNil(FDataList);
  except on E: Exception do HandleError(E, OPNAME); end;
  inherited;
end;

function TDDTSOutputDataList.AddOutputData: TDDTSOutputData;
const OPNAME = 'TDDTSOutputDataList.AddOutputData';
begin
  Result := nil;
  try
    Result := TDDTSOutputData.Create(FAppModules);
    FDataList.Add(Result);
    Result.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputDataList.Get_Count: integer;
const OPNAME = 'TDDTSOutputDataList.Get_Count';
begin
  Result := 0;
  try
    Result := FDataList.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputDataList.Get_OutputDataByIndex(AIndex: integer): TDDTSOutputData;
const OPNAME = 'TDDTSOutputDataList.Get_OutputDataByIndex';
begin
  Result := nil;
  try
  if(AIndex >= 0) and (AIndex <  FDataList.Count) then
    Result := TDDTSOutputData(FDataList[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSOutputDataList.Initialise: Boolean;
const OPNAME = 'TDDTSOutputDataList.Initialise';
begin
  Result := False;
  try
    FDataList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{
    FDailyDate          : TDate;
    FRunoff             : double;
    FOtherInflow        : double;
    FRainfall           : double;
    FTotalInflow        : double;
    FGrossAvailable     : double;
    FEvaporation        : double;
    FEvaporationSupply  : double;
    FEvaporationDeficits: double;
    FIncreamentalRunoff : double;
    FEWR                : double;
    FEWRSupplyFromIncreamentalRunoff : double;
    FSpills             : double;
    FSpills             : double;
    FSpills             : double;
}

end.
