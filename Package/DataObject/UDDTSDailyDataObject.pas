//
//
//  UNIT      : Contains TPathsObject Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 05/08/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDDTSDailyDataObject;

interface

uses
  System.Classes,
  System.sysutils,
  System.contnrs,
  Math,
  //  DWAF VCL
  UAbstractObject,
  UConstants;

type


  TDailyDataObject = class(TAbstractObject)
  protected
    FFileData: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function ReadLineValues(ALineIndex: integer; var ADate : TDateTime; var AValue : double): boolean;
    function ReadDailyValue(ADate: TDateTime;var AStartPoint : integer; var AValue: double): boolean;
    function Get_StartDate : TDateTime;
    function Get_EndDate : TDateTime;
    function Get_ValuesCount : integer;
  public
    procedure Reset;
    function ReadFile(AFileName : string): boolean;
    function SaveToFile(AFileName : string): boolean;
    function ReadDailyValues(AIndex: integer; var ADate : TDateTime; var AValue : double): boolean;
    function Initialise: boolean;override;
    function Populated: boolean;
    property Count  : integer read Get_ValuesCount;
    property StartDate    : TDateTime read Get_StartDate;
    property EndDate      : TDateTime read Get_EndDate;
  end;

  TCombinedDailyDataObject = class(TAbstractObject)
  protected
    FDailyDate               : TDateTime;
    FRunoffValue             : double;
    FOtherInflowValue        : double;
    FIncreamentalRunoffValue : double;
    FEWRValue                : double;
    FRainfallValue           : double;
    FEvaporationValue        : double;
  public
    procedure Reset;
    function Initialise: boolean;override;
    property DailyDate               : TDateTime read  FDailyDate               write  FDailyDate;
    property RunoffValue             : double    read  FRunoffValue             write FRunoffValue;
    property OtherInflowValue        : double    read  FOtherInflowValue        write FOtherInflowValue;
    property IncreamentalRunoffValue : double    read  FIncreamentalRunoffValue write FIncreamentalRunoffValue;
    property EWRValue                : double    read  FEWRValue                write FEWRValue;
    property RainfallValue           : double    read  FRainfallValue           write FRainfallValue;
    property EvaporationValue        : double    read  FEvaporationValue        write FEvaporationValue;
  end;

  TDDTSDailyDataObject = class(TAbstractObject)
  protected
    FRunoffFileData : TDailyDataObject;
    FOtherInflowFileData : TDailyDataObject;
    FIncreamentalRunoffFileData : TDailyDataObject;
    FEWRFileData : TDailyDataObject;
    FRainfallFileData : TDailyDataObject;
    FEvaporationFileData : TDailyDataObject;
    FCombinedDailyDataList : TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function Get_CombinedDailyDataObjectByIndex(AIndex : integer) : TCombinedDailyDataObject;
    function Get_CombinedDailyDataCount : integer;
  public
    procedure Reset;
    function Initialise: boolean;override;
    function StartDate : TDateTime;
    function EndDate   : TDateTime;
    function AddCombinedDailyDataObject : TCombinedDailyDataObject;
    function GenerateCombinedDailyData: boolean;
    function SplitCombinedDailyData: boolean;
    function ValidateDDTSFileData(AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
    property RunoffFileData              : TDailyDataObject read FRunoffFileData            ;
    property OtherInflowFileData         : TDailyDataObject read FOtherInflowFileData       ;
    property IncreamentalRunoffFileData  : TDailyDataObject read FIncreamentalRunoffFileData;
    property EWRFileData                 : TDailyDataObject read FEWRFileData               ;
    property RainfallFileData            : TDailyDataObject read FRainfallFileData          ;
    property EvaporationFileData         : TDailyDataObject read FEvaporationFileData       ;
    property CombinedDailyDataCount      : integer          read Get_CombinedDailyDataCount;
    property CombinedDailyDataObjectByIndex[AIndex : integer] : TCombinedDailyDataObject read Get_CombinedDailyDataObjectByIndex;
  end;


implementation


uses UErrorHandlingOperations;

{TPathsObject}

procedure TDailyDataObject.CreateMemberObjects;
const OPNAME = 'TDailyDataObject.CreateMemberObjects';
Begin
  try
    FFileData               :=  TStringList.Create;
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDataObject.DestroyMemberObjects;
const OPNAME = 'TDailyDataObject.DestroyMemberObjects';
Begin
  try
    FFileData.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.ReadFile(AFileName: string): boolean;
const OPNAME = 'TPathsObject.ReadFile';
var
  LIndex : integer;
Begin
  Result := False;
  try
    FFileData.Clear;
    if FileExists(AFileName) then
    begin
      FFileData.LoadFromFile(AFileName);
      LIndex := FFileData.Count-1;
      while (LIndex >= 0) do
      begin
        if(Trim(FFileData[LIndex]) = '') then
          FFileData.Delete(LIndex);
        LIndex := LIndex - 1;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.SaveToFile(AFileName: string): boolean;
const OPNAME = 'TPathsObject.SaveToFile';
Begin
  Result := False;
  try
    DeleteFile(AFileName);
    FFileData.SaveToFile(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDataObject.Reset;
const OPNAME = 'TPathsObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDailyDataObject.Initialise: boolean;
const OPNAME = 'TPathsObject.Initialise';
Begin
  Result := False;
  try
    //Data and initialised.
    FFileData.Clear;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.Populated: boolean;
const OPNAME = 'TDailyDataObject.Populated';
Begin
  Result := False;
  try
    Result := (FFileData.Count > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.ReadLineValues(ALineIndex: integer; var ADate: TDateTime; var AValue: double): boolean;
const OPNAME = 'TDailyDataObject.ReadLineValues';
var
  LLineData    : TStringList;
  LReadString  : string;
  LDate        : TDateTime;
  LReadReal    : Double;
  LErrorCode   : Integer;
Begin
  Result := False;
  try
    ADate  := NullFloat;
    AValue := NullFloat;
    if(ALineIndex < FFileData.Count) then
    begin
      LLineData := TStringList.Create;
      try
        LLineData.CommaText := FFileData[ALineIndex];
        if(LLineData.Count >= 2) then
        begin
          LReadString  := Trim(LLineData[0]);
          LDate := StrToDateDef(LReadString,NullDateTime);
          if(LDate <> NullDateTime) then
          begin
            LReadString  := Trim(LLineData[1]);
            LReadReal    := 0.0;
            if(LReadString <> '') then
              Val(LReadString,LReadReal,LErrorCode);
            if(LErrorCode = 0) then
            begin
              ADate  := LDate;
              AValue := LReadReal;
              Result := True;
            end;
          end;
        end;
      finally
        LLineData.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.ReadDailyValue(ADate: TDateTime;var AStartPoint : integer; var AValue: double): boolean;
const OPNAME = 'TDailyDataObject.ReadDailyValues';
var
  LIndex       : integer;
  LDate        : TDateTime;
  LReadReal    : Double;
begin
  Result := False;
  try
    for LIndex := AStartPoint to FFileData.Count-1 do
    begin
      AStartPoint := LIndex;
      if ReadLineValues(LIndex,LDate,LReadReal) then
      begin
        if(LDate > ADate) then Exit;
        if(LDate = ADate) then
        begin
          AValue :=  LReadReal;
          Result := True;
          Exit;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.ReadDailyValues(AIndex: integer; var ADate: TDateTime; var AValue: double): boolean;
const OPNAME = 'TDailyDataObject.ReadDailyValues';
var
  LDate        : TDateTime;
  LReadReal    : Double;
begin
  Result := False;
  try
    Result := ReadLineValues(AIndex,LDate,LReadReal);
    if Result  then
    begin
      ADate := LDate;
      AValue := LReadReal;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.Get_EndDate: TDateTime;
const OPNAME = 'TDailyDataObject.Get_EndDate';
var
  LDate        : TDateTime;
  LReadReal    : Double;
begin
  Result := 0.0;
  try
    if Populated  then
    begin
      if ReadLineValues(FFileData.Count-1,LDate,LReadReal) then
      begin
        Result := LDate;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.Get_StartDate: TDateTime;
const OPNAME = 'TDailyDataObject.Get_StartDate';
var
  LDate        : TDateTime;
  LReadReal    : Double;
begin
  Result := 0.0;
  try
    if Populated then
    begin
      if ReadLineValues(0,LDate,LReadReal) then
      begin
        Result := LDate;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDataObject.Get_ValuesCount: integer;
const OPNAME = 'TDailyDataObject.Get_ValuesCount';
Begin
  Result := 0;
  try
    Result := FFileData.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TDDTSDailyDataObject }

function TDDTSDailyDataObject.AddCombinedDailyDataObject: TCombinedDailyDataObject;
const OPNAME = 'TDDTSDailyDataObject.AddCombinedDailyDataObject';
begin
  Result := nil;
  try
    Result := TCombinedDailyDataObject.Create;
    Result.Reset;
    FCombinedDailyDataList.Add(Result);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDailyDataObject.CreateMemberObjects;
const OPNAME = 'TDDTSDailyDataObject.CreateMemberObjects';
begin
  inherited;
  try
    FRunoffFileData             := TDailyDataObject.Create;;
    FOtherInflowFileData        := TDailyDataObject.Create;;
    FIncreamentalRunoffFileData := TDailyDataObject.Create;;
    FEWRFileData                := TDailyDataObject.Create;;
    FRainfallFileData           := TDailyDataObject.Create;;
    FEvaporationFileData        := TDailyDataObject.Create;;
    FCombinedDailyDataList      := TObjectList.Create(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDailyDataObject.DestroyMemberObjects;
const OPNAME = 'TDDTSDailyDataObject.DestroyMemberObjects';
begin
  inherited;
  try
    FRunoffFileData.Free;
    FOtherInflowFileData.Free;
    FIncreamentalRunoffFileData.Free;
    FEWRFileData.Free;
    FRainfallFileData.Free;
    FEvaporationFileData.Free;
    FCombinedDailyDataList.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.GenerateCombinedDailyData: boolean;
const OPNAME = 'TDDTSDailyDataObject.GenerateCombinedDailyData';
var
  LIndex1,
  LIndex2,
  LIndex3,
  LIndex4,
  LIndex5,
  LIndex6 : integer;

  LStatrDate,
  LEndDate  : TDateTime;
  LValue    : double;
  LCombinedDailyDataObject : TCombinedDailyDataObject;
begin
  Result := False;
  try
    FCombinedDailyDataList.Clear;
    LStatrDate := StartDate;
    if(LStatrDate = NullDateTime) then
      raise Exception.Create('There is/are a input daily data file/s with no data. All file must have the same period data.');

    LEndDate   := EndDate;

    LIndex1 := 0;
    LIndex2 := 0;
    LIndex3 := 0;
    LIndex4 := 0;
    LIndex5 := 0;
    LIndex6 := 0;
    while (LStatrDate <= LEndDate) do
    begin
      LCombinedDailyDataObject := AddCombinedDailyDataObject;
      LValue                                           := NullFloat;
      LCombinedDailyDataObject.DailyDate               := LStatrDate;

      LCombinedDailyDataObject.RunoffValue             := NullFloat;
      LCombinedDailyDataObject.OtherInflowValue        := NullFloat;
      LCombinedDailyDataObject.IncreamentalRunoffValue := NullFloat;
      LCombinedDailyDataObject.EWRValue                := NullFloat;
      LCombinedDailyDataObject.RainfallValue           := NullFloat;
      LCombinedDailyDataObject.EvaporationValue        := NullFloat;

      if FRunoffFileData.ReadDailyValue(LStatrDate,LIndex1,LValue) then
        LCombinedDailyDataObject.RunoffValue             := LValue;
      if FOtherInflowFileData.ReadDailyValue(LStatrDate,LIndex2,LValue) then
        LCombinedDailyDataObject.OtherInflowValue        := LValue;
      if FIncreamentalRunoffFileData.ReadDailyValue(LStatrDate,LIndex3,LValue) then
        LCombinedDailyDataObject.IncreamentalRunoffValue := LValue;
      if FEWRFileData.ReadDailyValue(LStatrDate,LIndex4,LValue) then
        LCombinedDailyDataObject.EWRValue                := LValue;
      if FRainfallFileData.ReadDailyValue(LStatrDate,LIndex5,LValue) then
        LCombinedDailyDataObject.RainfallValue           := LValue;
      if FEvaporationFileData.ReadDailyValue(LStatrDate,LIndex6,LValue) then
        LCombinedDailyDataObject.EvaporationValue        := LValue;

      LStatrDate := LStatrDate + 1.0;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.SplitCombinedDailyData: boolean;
const OPNAME = 'TDDTSDailyDataObject.Get_CombinedDailyDataObjectByIndex';
var
  LIndex : integer;
  LDailyDataObject : TCombinedDailyDataObject;
begin
  Result := False;
  try
    FRunoffFileData.Initialise;
    FOtherInflowFileData.Initialise;
    FIncreamentalRunoffFileData.Initialise;
    FEWRFileData.Initialise;
    FRainfallFileData.Initialise;
    FEvaporationFileData.Initialise;
    for LIndex := 0 to FCombinedDailyDataList.Count-1 do
    begin
      LDailyDataObject := TCombinedDailyDataObject(FCombinedDailyDataList.Items[LIndex]);
      if(LDailyDataObject.RunoffValue = NullInteger) then
        FRunoffFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FRunoffFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.RunoffValue));

      if(LDailyDataObject.OtherInflowValue = NullInteger) then
        FOtherInflowFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FOtherInflowFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.OtherInflowValue));

      if(LDailyDataObject.IncreamentalRunoffValue = NullInteger) then
        FIncreamentalRunoffFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FIncreamentalRunoffFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.IncreamentalRunoffValue));

      if(LDailyDataObject.EWRValue = NullInteger) then
        FEWRFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FEWRFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.EWRValue));

      if(LDailyDataObject.RainfallValue = NullInteger) then
        FRainfallFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FRainfallFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.RainfallValue));

      if(LDailyDataObject.EvaporationValue = NullInteger) then
        FEvaporationFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+',')
      else
        FEvaporationFileData.FFileData.Add(DateToStr(LDailyDataObject.DailyDate)+','+FormatFloat('##0.000',LDailyDataObject.EvaporationValue));
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.Get_CombinedDailyDataCount: integer;
const OPNAME = 'TDailyDataObject.Get_CombinedDailyDataCount';
Begin
  Result := 0;
  try
    Result := FCombinedDailyDataList.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.Get_CombinedDailyDataObjectByIndex(AIndex: integer): TCombinedDailyDataObject;
const OPNAME = 'TDDTSDailyDataObject.Get_CombinedDailyDataObjectByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FCombinedDailyDataList.Count) then
      Result := TCombinedDailyDataObject(FCombinedDailyDataList.Items[AIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.Initialise: boolean;
const OPNAME = 'TDDTSDailyDataObject.Initialise';
begin
  Result := False;
  try
    FRunoffFileData.Initialise;
    FOtherInflowFileData.Initialise;
    FIncreamentalRunoffFileData.Initialise;
    FEWRFileData.Initialise;
    FRainfallFileData.Initialise;
    FEvaporationFileData.Initialise;
    FCombinedDailyDataList.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDailyDataObject.Reset;
const OPNAME = 'TDDTSDailyDataObject.Reset';
begin
  try
     Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.StartDate: TDateTime;
const OPNAME = 'TDDTSDailyDataObject.EndDate';
begin
  Result := NullInteger;
  try
     Result := MinValue([FRunoffFileData.StartDate,FOtherInflowFileData.StartDate,FIncreamentalRunoffFileData.StartDate,
                         FEWRFileData.StartDate,FRainfallFileData.StartDate,FEvaporationFileData.StartDate]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDailyDataObject.EndDate: TDateTime;
const OPNAME = 'TDDTSDailyDataObject.EndDate';
begin
  Result := NullInteger;
  try
     Result := MaxValue([FRunoffFileData.EndDate,FOtherInflowFileData.EndDate,FIncreamentalRunoffFileData.EndDate,
                         FEWRFileData.EndDate,FRainfallFileData.EndDate,FEvaporationFileData.EndDate]);
  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TDDTSDailyDataObject.ValidateDDTSFileData(AAppModules: TAppModules; AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDDTSDailyDataObject.ValidateDDTSFileData';
var
  LRunoffDate             : TDateTime;
  LOtherInflowDate        : TDateTime;
  LIncreamentalRunoffDate : TDateTime;
  LEWRDate                : TDateTime;
  LRainfallDate           : TDateTime;
  LEvaporationDate        : TDateTime;
  LStop                   : boolean;
  LValue                  : double;
  LMaxIndex               : integer;
begin
  Result := False;
  try
    LStop := False;
    if(FRunoffFileData.Count =  0) then
      AProgressUpdateFuntion('Runoff File Data is empty. Please select a file with data.',ptError,LStop);
    if(FOtherInflowFileData.Count =  0) then
      AProgressUpdateFuntion('Other Inflow File Data is empty. Please select a file with data.',ptError,LStop);
    if(FIncreamentalRunoffFileData.Count =  0) then
      AProgressUpdateFuntion('Increamental Runoff File Data is empty. Please select a file with data.',ptError,LStop);
    if(FEWRFileData.Count =  0) then
      AProgressUpdateFuntion('EWR File Data is empty. Please select a file with data.',ptError,LStop);
    if(FRainfallFileData.Count =  0) then
      AProgressUpdateFuntion('FRainfall File Data is empty. Please select a file with data.',ptError,LStop);
    if(FEvaporationFileData.Count =  0) then
      AProgressUpdateFuntion('FEvaporation File Data is empty. Please select a file with data.',ptError,LStop);

    if LStop  then Exit;

    LRunoffDate             := NullDateTime;
    LOtherInflowDate        := NullDateTime;
    LIncreamentalRunoffDate := NullDateTime;
    LEWRDate                := NullDateTime;
    LRainfallDate           := NullDateTime;
    LEvaporationDate        := NullDateTime;

    FRunoffFileData.ReadLineValues(0,LRunoffDate,LValue);
    FOtherInflowFileData.ReadLineValues(0,LOtherInflowDate,LValue);
    FIncreamentalRunoffFileData.ReadLineValues(0,LIncreamentalRunoffDate,LValue);
    FEWRFileData.ReadLineValues(0,LEWRDate,LValue);
    FRainfallFileData.ReadLineValues(0,LRainfallDate,LValue);
    FEvaporationFileData.ReadLineValues(0,LEvaporationDate,LValue);

    if(MinValue([LRunoffDate,LOtherInflowDate,LIncreamentalRunoffDate,LEWRDate,LRainfallDate,LEvaporationDate]) <>
       MaxValue([LRunoffDate,LOtherInflowDate,LIncreamentalRunoffDate,LEWRDate,LRainfallDate,LEvaporationDate])) then
      AProgressUpdateFuntion('Start date is not the same in all input daily data files.',ptError,LStop);

    LRunoffDate             := NullDateTime;
    LOtherInflowDate        := NullDateTime;
    LIncreamentalRunoffDate := NullDateTime;
    LEWRDate                := NullDateTime;
    LRainfallDate           := NullDateTime;
    LEvaporationDate        := NullDateTime;

    LMaxIndex := FRunoffFileData.Count-1;
    FRunoffFileData.ReadLineValues(LMaxIndex,LRunoffDate,LValue);
    LMaxIndex := FOtherInflowFileData.Count-1;
    FOtherInflowFileData.ReadLineValues(LMaxIndex,LOtherInflowDate,LValue);
    LMaxIndex := FIncreamentalRunoffFileData.Count-1;
    FIncreamentalRunoffFileData.ReadLineValues(LMaxIndex,LIncreamentalRunoffDate,LValue);
    LMaxIndex := FEWRFileData.Count-1;
    FEWRFileData.ReadLineValues(LMaxIndex,LEWRDate,LValue);
    LMaxIndex := FRainfallFileData.Count-1;
    FRainfallFileData.ReadLineValues(LMaxIndex,LRainfallDate,LValue);
    LMaxIndex := FEvaporationFileData.Count-1;
    FEvaporationFileData.ReadLineValues(LMaxIndex,LEvaporationDate,LValue);

    if(MinValue([LRunoffDate,LOtherInflowDate,LIncreamentalRunoffDate,LEWRDate,LRainfallDate,LEvaporationDate]) <>
       MaxValue([LRunoffDate,LOtherInflowDate,LIncreamentalRunoffDate,LEWRDate,LRainfallDate,LEvaporationDate])) then
      AProgressUpdateFuntion('End date is not the same in all input daily data files.',ptError,LStop);

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TCombinedDailyDataObject }

function TCombinedDailyDataObject.Initialise: boolean;
const OPNAME = 'TCombinedDailyDataObject.Initialise';
begin
  Result := False;
  try
    FDailyDate               := NullDateTime;
    FRunoffValue             := NullFloat;
    FOtherInflowValue        := NullFloat;
    FIncreamentalRunoffValue := NullFloat;
    FEWRValue                := NullFloat;
    FRainfallValue           := NullFloat;
    FEvaporationValue        := NullFloat;
    Result                   := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCombinedDailyDataObject.Reset;
const OPNAME = 'TCombinedDailyDataObject.Reset';
begin
  try
    Initialise;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
