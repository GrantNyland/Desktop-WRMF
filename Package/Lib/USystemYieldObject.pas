//
//
//  UNIT      : Contains THistoricSystemYieldDataObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/02/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit USystemYieldObject;

interface
uses
  Contnrs,
  UYieldModelDataObject,
  URunConfigurationData,
  UAbstractFileNamesObject,
  UFileNames,
  USumOutFileManager,
  USumOutDataObjects,
  UAbstractDataObject,
  UAbstractObject;
type

  TMaxLength = 0..9;
  TDeficitPropotion = array [ TMaxLength ] of double;
  TSystemYieldType = ( ytHistoric, ytStochastic );
  TAbstractHistoricSystemYield = class ( TAbstractAppDataObject )
  protected
    function GetDeficitPropotion : TDeficitPropotion; virtual; abstract;
  public
    function LoadDataFromFile ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean; virtual; abstract;
    function LoadDataFromDB ( aFileBlocks : TObjectList; const aPrimaryKeyProperty : array of string ) : boolean; virtual; abstract;
    function SaveDataToDB ( const aPrimaryKeyProperty : array of string ) : boolean; virtual; abstract;
  end;

  THistoricSystemYieldDataObject = class ( TAbstractHistoricSystemYield )
  protected
    FLoadedFromFile,
    FLoadedFromDB : boolean;
    FErrorMsg : string;
    FTargetDraft : array [ TMaxLength ] of double;
    FDeficitPropotion : TDeficitPropotion;
    FSystemYield : array [ TMaxLength ] of double;
    FSystemYieldCounter : integer;

    function GetSystemYieldCounter : integer;
    function GetDeficitPropotion : TDeficitPropotion; override;
    function GetTargetDraft ( aIndex : integer ) : double;
    procedure CalculateSystemYield ( aIndex : integer );
    function GetSystemYield ( aIndex : integer ) : double;
    procedure SetSystemYield ( aIndex: integer; const aValue: double );
    procedure SetTargetDraft ( aIndex : integer; const aValue : double );
    function LoadDeficitFromBlocks ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean;
  public
    procedure Reset; override;
    function LoadDataFromFile ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean; override;
    function LoadDataFromDB ( aFileBlocks : TObjectList; const aPrimaryKeyProperty : array of string ) : boolean; override;
    function SaveDataToDB ( const aPrimaryKeyProperty : array of string ) : boolean; override;
    property DeficitPropotion : TDeficitPropotion read FDeficitPropotion write FDeficitPropotion;
    property ErrorMsg : string read FErrorMsg write FErrorMsg;
    property TargetDraft [ index : integer ] : double read GetTargetDraft write SetTargetDraft;
    property HistoricSystemYield [ index : integer ] : double read GetSystemYield write SetSystemYield;
    property HistoricSystemYieldCounter : integer read GetSystemYieldCounter;
  end;

  TAbstractSystemYieldObject = class ( TAbstractAppDataObject )
  public
    { Historic }
    function GetTargetYieldByIndex ( aIndex : integer ) : double; virtual; abstract;
    function GetDeficitPropotionByIndex ( aIndex : integer ) : double; virtual;abstract;
    function GetSystemYieldByIndex ( aIndex : integer ) : double; virtual; abstract;
    function ReadDeficitPropotionDataFromFile ( AFileNames : TModelFileNames ) : boolean; virtual; abstract;
    function ReadDeficitDataFromDB ( aFileBlocks : TObjectList ): boolean; virtual; abstract;
    function GetHistoricSystemYieldCounter : integer; virtual; abstract;
    { Stochastic }
    function GetPeriodLength : integer; virtual; abstract;
    function GetNumberOfSeqAnalysed : integer; virtual; abstract;
    function GetRecurrenceIntavalByIndex ( aIndex : integer ) : double; virtual; abstract;
    function GetReliabilityOfSupplyByIndex ( aIndex : integer ) : double; virtual; abstract;
    function GetNoOfFaiureSeqByIndex ( aIndex : integer ) : double; virtual; abstract;
    function GetTargetDraftByIndex ( aIndex : integer ) : double; virtual; abstract;
  end;
  TPeriodLength = class;
  TStochasticSystemYieldDataObject = class;
  TSystemYieldObject = class ( TAbstractSystemYieldObject )
  protected
    FSumOutFileManager : TSumOutFileManager;
    FHistoricSystemYieldDataObject : THistoricSystemYieldDataObject;
    FStochasticSystemYieldDataObject : TStochasticSystemYieldDataObject;
    FSystemYieldType : TSystemYieldType;
    FCounter : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure LoadDataObject; virtual;
    function SaveToDB : boolean; virtual;
    function GetErrorMsg : string;
    procedure SetNumberOfSeqAnalysed ( const aValue : integer );
    function GetPriodLengthDataByIndex ( aIndex : integer ): TPeriodLength;
  public
    function Initialise : Boolean; override;
    procedure Reset; override;

    function GetSystemYieldByIndex ( aIndex : integer ) : double; override;
    function GetTargetYieldByIndex ( aIndex : integer ) : double; override;
    function GetDeficitPropotionByIndex ( aIndex : integer ) : double; override;
    function ReadDeficitPropotionDataFromFile ( aFileNames : TModelFileNames ) : boolean; override;
    function GetHistoricSystemYieldCounter : integer; override;

    function GetPeriodLength : integer; override;
    function GetNumberOfSeqAnalysed : integer; override;
    function GetRecurrenceIntavalByIndex ( aIndex : integer ) : double; override;
    function GetReliabilityOfSupplyByIndex ( aIndex : integer ) : double; override;
    function GetNoOfFaiureSeqByIndex ( aIndex : integer ) : double; override;
    function GetTargetDraftByIndex ( aIndex : integer ) : double; override;

    function ReadStochasticDataFromFile ( aFileNames : TModelFileNames ) : boolean;
    function ReadDeficitDataFromDB ( aFileBlocks : TObjectList ) : boolean; override;
    property HistoricSystemYieldCounter : integer read GetHistoricSystemYieldCounter;
    property SystemYieldType : TSystemYieldType read FSystemYieldType write FSystemYieldType;
    property ErrorMsg : string read GetErrorMsg;
    property NumberOfSeqAnalysed : integer read GetNumberOfSeqAnalysed write SetNumberOfSeqAnalysed;
    property PriodLengthData [ index : integer ] : TPeriodLength read GetPriodLengthDataByIndex;
  end;

    TStochasticSystemYieldDataObject = class ( TAbstractAppDataObject )
    protected
      FNoOfSeqAnalysed : integer;
      FPriodLengthList : TObjectList;
      FPriodLengthData : array [ TMaxLength ] of TPeriodLength;
      FPeriodLength : array [ TMaxLength ] of integer;
      FRecurrenceIntaval : array [ TMaxLength ] of double;
      FReliabilityOfSupply : array [ TMaxLength ] of double;
      FNoOfFaiureSeq : array [ TMaxLength ] of double;
      FTargetDraft : array [ TMaxLength ] of double;
      FErrorMsg : string;
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
      function GetNoOfFaiureSeq ( aIndex : integer ): double; virtual;
      function GetNumberOfSeqAnalysed : integer; virtual;
      function GetRecurrenceIntaval ( aIndex : integer ): double; virtual;
      function GetReliabilityOfSupply ( aIndex : integer ): double; virtual;
      function GetTargetDraft ( aIndex : integer ): double; virtual;
      function GetPeriodLength ( aIndex : integer) : integer; virtual;
      function GetPriodLengthData ( aIndex : integer ) : TPeriodLength;
      procedure SetNoOfFaiureSeq ( aIndex : integer; const aValue : double ); virtual;
      procedure SetNumberOfSeqAnalysed ( const aValue : integer ); virtual;
      procedure SetRecurrenceIntaval ( aIndex : integer; const aValue : double ); virtual;
      procedure SetReliabilityOfSupply ( aIndex : integer; const aValue : double ); virtual;
      procedure SetTargetDraft ( aIndex : integer; const aValue : double ); virtual;
      function LoadNoOfFuilureSeqFromFile ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean; virtual;
      function LoadRecurrenceIntervalFromFile ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean; virtual;
      procedure SetPeriodLength ( aIndex : integer; const aValue : integer ); virtual;

    public
      procedure Reset; override;
      function Initialise: Boolean; override;
      procedure CalculateReliabilityOfSupply ( aIndex : integer ); virtual;
      function LoadDataFromFile ( aFileBlocks, aDeficitBlock : TObjectList; const aIndexProperty : array of integer ) : boolean; virtual;

      property PeriodLength [ index : integer ] : integer read GetPeriodLength write SetPeriodLength;
      property NoOfSeqAnalysed : integer read GetNumberOfSeqAnalysed write SetNumberOfSeqAnalysed;
      property RecurrenceIntaval [ index : integer ] : double read GetRecurrenceIntaval write SetRecurrenceIntaval;
      property ReliabilityOfSupply [ index : integer ] : double read GetReliabilityOfSupply write SetReliabilityOfSupply;
      property NoOfFaiureSeq [ index : integer ] : double read GetNoOfFaiureSeq write SetNoOfFaiureSeq;
      property TargetDraft [ index : integer ] : double read GetTargetDraft write SetTargetDraft;
      property PriodLengthData [ index : integer ] : TPeriodLength read GetPriodLengthData;
      property ErrorMsg : string read FErrorMsg write FErrorMsg;

    end;

    TPeriodLength = class ( TStochasticSystemYieldDataObject )
    protected
      FPeriodCount : integer;
      FRecordCount : integer;
      procedure CreateMemberObjects; override;
      procedure DestroyMemberObjects; override;
      function GetPeriodCount: integer;
      procedure SetPeriodCount ( const aValue : integer );
    public
      FPeriod : integer;
      function Initialise : Boolean; override;
      property PeriodCount : integer read GetPeriodCount write SetPeriodCount;
      property RecordCounter : integer read FRecordCount write FRecordCount;
end;

implementation
uses
  SysUtils,
  UDataSetType,
  UConstants,
  UErrorHandlingOperations, Classes;

{ TSystemYeildObject }

procedure TSystemYieldObject.CreateMemberObjects;
const OPNAME = 'TSystemYieldObject.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSumOutFileManager := TSumOutFileManager.Create ( FAppModules );
    FHistoricSystemYieldDataObject := THistoricSystemYieldDataObject.Create ( FAppModules );
    FStochasticSystemYieldDataObject := TStochasticSystemYieldDataObject.Create ( FAppModules );
    
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldObject.DestroyMemberObjects;
const OPNAME = 'TSystemYieldObject.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FreeAndNil ( FSumOutFileManager );
    FreeAndNil ( FHistoricSystemYieldDataObject );
//    FreeAndNil ( FStochasticSystemYieldDataObject );

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetDeficitPropotionByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetDeficitPropotionByIndex';
begin
  Result := 0.0;
  try
    Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).DeficitPropotion [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetErrorMsg : string;
const OPNAME = 'TSystemYieldObject.GetErrorMsg';
begin
  Result := '';
  try
    case FSystemYieldType of
      ytHistoric : Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).ErrorMsg;
      ytStochastic : Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).ErrorMsg;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetHistoricSystemYieldCounter : integer;
const OPNAME = 'TSystemYieldObject.GetHistoricSystemYieldCounter';
begin
  Result := 0;
  try
    Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).HistoricSystemYieldCounter;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetSystemYieldByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetSystemYieldByIndex';
begin
  Result := 0.0;
  try
    Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).HistoricSystemYield [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetTargetYieldByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetTargetYieldByIndex';
begin
  Result := NullFloat;
  try
    Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).TargetDraft [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.Initialise: Boolean;
const OPNAME = 'TSystemYieldObject.Initialise';
begin
  Result := False;
  try
//     Reset;
     LoadDataObject;
     Result := True;
//     Result := FStochasticSystemYieldDataObject.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldObject.LoadDataObject;
const OPNAME = 'TSystemYieldObject.LoadDataObject';
var
  LFileBlocks : TObjectList;
begin
  try
    case FSystemYieldType of
    ytHistoric :
    begin
      if not (ReadDeficitPropotionDataFromFile(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject)) then
      try
        LFileBlocks := TObjectList.Create;
        if not (ReadDeficitDataFromDB(LFileBlocks)) then
          FHistoricSystemYieldDataObject.ErrorMsg := 'NoDataFound'
      finally
        FreeAndNil(LFileBlocks)
      end;
    end;
    ytStochastic : ReadStochasticDataFromFile(TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject) ;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.ReadDeficitDataFromDB ( aFileBlocks : TObjectList ) : boolean;
const OPNAME = 'TSystemYieldObject.ReadDeficitDataFromDB';
var
  lPrimaryKeys : array [ 0..4 ] of string;
begin
  Result := False;
  try
    FHistoricSystemYieldDataObject.ErrorMsg := '';
    lPrimaryKeys [ 0 ] := FAppModules.StudyArea.ModelCode;
    lPrimaryKeys [ 1 ] := FAppModules.StudyArea.StudyAreaCode;
    lPrimaryKeys [ 2 ] := FAppModules.StudyArea.SubAreaCode;
    lPrimaryKeys [ 3 ] := FAppModules.StudyArea.ScenarioCode;

     Result := FSumOutFileManager.GetDeficitsBlocksFromDB ( aFileBlocks );
     if ( aFileBlocks.Count = 0) then
       FHistoricSystemYieldDataObject.ErrorMsg := 'CaptionHistoric'
     else
     begin
       Result := Result and THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).LoadDataFromDB ( aFileBlocks, [ ] );
       Result :=  FHistoricSystemYieldDataObject.LoadDataFromDB ( aFileBlocks, lPrimaryKeys );
     end;
      if ( FHistoricSystemYieldDataObject.HistoricSystemYieldCounter = 0 ) then
        FHistoricSystemYieldDataObject.ErrorMsg := 'CaptionNoDbData'
      //else

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.ReadDeficitPropotionDataFromFile ( aFileNames : TModelFileNames ) : boolean;
const OPNAME = 'TSystemYieldObject.ReadDeficitPropotionDataFromFile';
var
  lFileNameObject : TAbstractModelFileName;
  lDeficitBlocks : TObjectList;
  LNoOfFaiureSeqBlocks : TObjectList;
begin
  Result := False;
  try
    FHistoricSystemYieldDataObject.ErrorMsg := '';
    lFileNameObject := aFileNames.GetSumOutFile;
    if not Assigned(lFileNameObject) then
      FHistoricSystemYieldDataObject.ErrorMsg := 'CaptionNoOutputFile'
    else
    begin
      if not lFileNameObject.FileFound then
        FHistoricSystemYieldDataObject.ErrorMsg := 'CaptionNoOutputFile'
      else
      begin
        lDeficitBlocks := TObjectList.Create(True);
        LNoOfFaiureSeqBlocks := TObjectList.Create(True);
        try
          Result := FSumOutFileManager.GetDeficitsBlocksFromFile(lFileNameObject, LDeficitBlocks);
          FSumOutFileManager.GetRecurrenceIntervalBlocksFromFile(LFileNameObject, LNoOfFaiureSeqBlocks);
          FSumOutFileManager.GetNoOfFailureSeqBlocksFromFile(LFileNameObject, LNoOfFaiureSeqBlocks);
          if ((FSumOutFileManager.RunType <> rtHistoric) and (TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType = 'S'))
            or (LNoOfFaiureSeqBlocks.Count>0) then
          begin
            Result := True;
            Exit;
          end;
          if (lDeficitBlocks.Count = 0) then
            FHistoricSystemYieldDataObject.ErrorMsg := 'CaptionHistoric'
          else
          begin
            Result := Result and THistoricSystemYieldDataObject(FHistoricSystemYieldDataObject).LoadDataFromFile(lDeficitBlocks,[]);
            if Result then
              SaveToDB;
          end;
        finally
          lDeficitBlocks.Free;
          LNoOfFaiureSeqBlocks.Free;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldObject.Reset;
const OPNAME = 'TSystemYieldObject.Reset';
begin
  FCounter := 0;
end;

function TSystemYieldObject.SaveToDB : boolean;
const OPNAME = 'TSystemYieldObject.SaveToDB';
var
  lPrimaryKeys : array [ 0..5 ] of string;
begin
  Result := False;
  try
    FHistoricSystemYieldDataObject.ErrorMsg := '';
    lPrimaryKeys [ 0 ] := FAppModules.StudyArea.ModelCode;
    lPrimaryKeys [ 1 ] := FAppModules.StudyArea.StudyAreaCode;
    lPrimaryKeys [ 2 ] := FAppModules.StudyArea.SubAreaCode;
    lPrimaryKeys [ 3 ] := FAppModules.StudyArea.ScenarioCode;
    lPrimaryKeys [ 4 ] := '0';
    Result := THistoricSystemYieldDataObject ( FHistoricSystemYieldDataObject ).SaveDataToDB ( lPrimaryKeys );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ THistoricSystemYieldDataObject }

procedure THistoricSystemYieldDataObject.CalculateSystemYield ( aIndex : integer );
const OPNAME = 'THistoricSystemYieldDataObject.CalculateSystemYield';
begin
  try
    HistoricSystemYield [ aIndex ] := ( TargetDraft [ aIndex ] * ( 1 - DeficitPropotion [ aIndex ] ) );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.GetSystemYield ( aIndex : integer ) : double;
const OPNAME = 'THistoricSystemYieldDataObject.GetSystemYield';
begin
  Result := 0.0;
  try
    Result := FSystemYield [ aIndex ] 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.GetTargetDraft ( aIndex: integer ): double;
const OPNAME = 'THistoricSystemYieldDataObject.GetTargetDraft';
begin
  Result := 0.0;
  try
    Result := FTargetDraft [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.LoadDataFromDB(aFileBlocks : TObjectList;
                                                        const aPrimaryKeyProperty : array of string ) : boolean;
const OPNAME = 'THistoricSystemYieldDataObject.LoadDataFromDB';
var
  LIndex             : integer;
begin
  Result := False;
  FSystemYieldCounter := 0;
  FLoadedFromFile     := False;
  FLoadedFromDB       := True;
  try
    for lIndex := 0 to aFileBlocks.Count - 1 do
      Result := LoadDeficitFromBlocks(aFileBlocks,[lIndex])
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.LoadDataFromFile(aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean;
const OPNAME = 'THistoricSystemYieldDataObject.LoadDataFromFile';
var
  lIndex : integer;
begin
  Result := False;
  FLoadedFromFile     := True;
  FLoadedFromDB       := False;
  try
    for lIndex := 0 to aFileBlocks.Count - 1 do
      Result := LoadDeficitFromBlocks ( aFileBlocks, [ lIndex ] )

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.LoadDeficitFromBlocks ( aFileBlocks : TObjectList; const aIndexProperty: array of integer ) : boolean;
const OPNAME = 'THistoricSystemYieldDataObject.LoadDeficitFromBlocks';
var
  lBlockValues : TSumOutAnualSummaryBlockValues;
  lcount,
  lIndex,
  lIndex2 : integer;
begin
  Result := False;
  try
    lIndex := AIndexProperty[0];
    if ( lIndex >= 0 ) and ( lIndex <= aFileBlocks.Count ) then
    begin
      if ( aFileBlocks [ lIndex ] is TSumOutAnualSummaryBlockValues ) then
      begin
        lBlockValues := TSumOutAnualSummaryBlockValues ( aFileBlocks [ lIndex ] );
        for lCount := Low ( lBlockValues.FValues ) to High ( lBlockValues.FValues ) do
        begin
          if not ( lBlockValues.FValues[ lCount ].FInitalised ) then
            Break;
          FSystemYieldCounter := 0;
          for lIndex2 := 0 to 9 do
          begin
            FTargetDraft [ lIndex2 ] := lBlockValues.FValues [ lIndex2 ].FData;
            FDeficitPropotion [ lIndex2 ] := lBlockValues.FAnualValues.AnualSummaryValuesLine [ lIndex ].
                                             AnualSummaryValuesLine [ lIndex2 ].FData;
            CalculateSystemYield ( lIndex2 );
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THistoricSystemYieldDataObject.Reset;
const OPNAME = 'THistoricSystemYieldDataObject.Reset';
var
  lCount: integer;
begin
  inherited Reset;
  try
    for lCount := 0 to 9 do
      FDeficitPropotion [ lCount ] := 0.0;
    FLoadedFromFile     := False;
    FLoadedFromDB       := False;
    FSystemYieldCounter := 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure THistoricSystemYieldDataObject.SetSystemYield ( aIndex : integer; const aValue : double );
const OPNAME = 'THistoricSystemYieldDataObject.SetSystemYield';
begin
  try
    if not ( aValue = 0 ) then
    begin
      FSystemYieldCounter := FSystemYieldCounter + 1;
      FSystemYield [ aIndex ] := aValue;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure THistoricSystemYieldDataObject.SetTargetDraft ( aIndex: integer; const aValue : double );
const OPNAME = 'THistoricSystemYieldDataObject.SetTargetDraft';
begin
  try
    FTargetDraft [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function THistoricSystemYieldDataObject.GetDeficitPropotion : TDeficitPropotion;
const OPNAME = 'THistoricSystemYieldDataObject.GetDeficitPropotion';
begin
  try
    Result := FDeficitPropotion;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.SaveDataToDB ( const aPrimaryKeyProperty : array of string ) : boolean;
const OPNAME = 'THistoricSystemYieldDataObject.SaveDataToDB';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function THistoricSystemYieldDataObject.GetSystemYieldCounter : integer;
const OPNAME = 'THistoricSystemYieldDataObject.GetSystemYieldCounter';
begin
  Result := 0;
  try
    Result := FSystemYieldCounter;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TStochasticSystemYieldDataObject }

procedure TStochasticSystemYieldDataObject.CalculateReliabilityOfSupply(aIndex : integer);
const OPNAME = 'TStochasticSystemYieldDataObject.CalculateReliabilityOfSupply';
var
  lReliabilityOfSupply,
  lRisckOfFailure : double;
begin
  try
    if (FNoOfSeqAnalysed <= FNoOfFaiureSeq[aIndex]) then
      FReliabilityOfSupply[aIndex] := 0
    else
    begin
      lRisckOfFailure := (FNoOfFaiureSeq[aIndex] + 1)/FNoOfSeqAnalysed;
      lReliabilityOfSupply := 1 - lRisckOfFailure;
      FReliabilityOfSupply[aIndex] := lReliabilityOfSupply * 100;
    end
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.ReadStochasticDataFromFile ( aFileNames : TModelFileNames ) : boolean;
const OPNAME = 'TSystemYieldObject.ReadStochasticDataFromFile';
var
  lFileNameObject : TAbstractModelFileName;
  lDeficitBlock,
  lNoOfFaiureSeqBlocks : TObjectList;
begin
  Result := False;
  try
    FStochasticSystemYieldDataObject.ErrorMsg := '';
    lFileNameObject := aFileNames.GetSumOutFile;
    if not Assigned(lFileNameObject) then
      FStochasticSystemYieldDataObject.ErrorMsg := 'CaptionNoOutputFile'
    else
    begin
      if not lFileNameObject.FileFound then
        FStochasticSystemYieldDataObject.ErrorMsg := 'CaptionNoOutputFile'
      else
      begin
        lNoOfFaiureSeqBlocks := TObjectList.Create(True);
        lDeficitBlock := TObjectList.Create(True);
        try
          Result := FSumOutFileManager.GetNoOfFailureSeqBlocksFromFile(lFileNameObject, lNoOfFaiureSeqBlocks);
          if (FSumOutFileManager.RunType <> rtStochastic) and (FSumOutFileManager.RunType <> rtUnKnown) then
          begin
            Result := True;
            Exit;
          end;
          Result := Result and FSumOutFileManager.GetRecurrenceIntervalBlocksFromFile(lFileNameObject, lNoOfFaiureSeqBlocks);
          Result := Result and FSumOutFileManager.GetDeficitsBlocksFromFile(lFileNameObject,lDeficitBlock);
          if (lNoOfFaiureSeqBlocks.Count = 0) then
            FStochasticSystemYieldDataObject.ErrorMsg := 'CaptionStochastic'
          else
          begin
            Result := Result and TStochasticSystemYieldDataObject(FStochasticSystemYieldDataObject).LoadDataFromFile(lNoOfFaiureSeqBlocks, lDeficitBlock,[]);
          end;
        finally
          lNoOfFaiureSeqBlocks.Free;
          lDeficitBlock.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStochasticSystemYieldDataObject.CreateMemberObjects;
const OPNAME = 'TStochasticSystemYieldDataObject.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.DestroyMemberObjects;
const OPNAME = 'TStochasticSystemYieldDataObject.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetNoOfFaiureSeq ( aIndex : integer ): double;
const OPNAME = 'TStochasticSystemYieldDataObject.GetNoOfFaiureSeq';
begin
  Result := 0;
  try
    Result := FNoOfFaiureSeq [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetNumberOfSeqAnalysed : integer;
const OPNAME = 'TStochasticSystemYieldDataObject.GetNumberOfSeqAnalysed';
begin
  Result := 0;
  try
    Result := FNoOfSeqAnalysed;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetPeriodLength ( aIndex : integer ): integer;
const OPNAME = 'TStochasticSystemYieldDataObject.GetPeriodLength';
begin
  Result := 0;
  try
    Result := FPeriodLength [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetPriodLengthData ( aIndex : integer ) : TPeriodLength;
const OPNAME = 'TStochasticSystemYieldDataObject.GetPriodLengthData';
begin
  Result := nil;
  try
    Result := FPriodLengthData [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetRecurrenceIntaval ( aIndex : integer ) : double;
const OPNAME = 'TStochasticSystemYieldDataObject.GetRecurrenceIntaval';
begin
  Result := 0.00;
  try
    Result := FRecurrenceIntaval [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetReliabilityOfSupply ( aIndex : integer ) : double;
const OPNAME = 'TStochasticSystemYieldDataObject.GetReliabilityOfSupply';
begin
  Result := 0.00;
  try
    Result := FReliabilityOfSupply [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.GetTargetDraft ( aIndex : integer ) : double;
const OPNAME = 'TStochasticSystemYieldDataObject.GetTargetDraft';
begin
  Result := 0.00;
  try
    Result := FTargetDraft [ aIndex ];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.Initialise : Boolean;
const OPNAME = 'TStochasticSystemYieldDataObject.Initialise';
var
  lIndex : integer;
begin
  Result := False;
  try
    for lIndex := 0 to 9 do
      FPriodLengthData [ lIndex ] := TPeriodLength.Create ( FAppModules );
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.LoadDataFromFile ( aFileBlocks, aDeficitBlock : TObjectList; const aIndexProperty : array of integer ): boolean;
const OPNAME = 'TStochasticSystemYieldDataObject.LoadDataFromFile';
var
  lRecurrenceBlock,
  lBlockValues : TSumOutRecurrenceIntervalBlockValues;
  lDeficitBlock : TSumOutAnualSummaryBlockValues;
  lcount,
  lCount2,
  lRecordCounter,
  lPeriodCount,
  lTDIndex,
  lIndex : integer;
  procedure LoadDataForPeriodLenght;
  const OPNAME = 'LoadDataForPeriodLenght';
  var
    dIndex : integer;
    dRecord : boolean;
  begin
    NoOfSeqAnalysed := lDeficitBlock.FAnualValues.Count;
    dRecord := False;
    for dIndex := 0 to 9 do
    begin
      FPriodLengthData [ dIndex ].FPeriod := lBlockValues.FPeriodLength [ dIndex ].FData;
      FPriodLengthData [ dIndex ].FTargetDraft [ lCount2 ] := lBlockValues.FTargetDraft [ lTDIndex ].FData;
      if FPriodLengthData [ dIndex ].FPeriod = 0 then
        break;
      if FPriodLengthData [ dIndex ].FTargetDraft [ lCount2 ] = 0 then
        break;
      dRecord := True;
      FPriodLengthData [ dIndex ].PeriodCount := lPeriodCount;
      FPriodLengthData [ dIndex ].FNoOfFaiureSeq [ lCount2 ] := lBlockValues.FAnualValues.AnualSummaryValuesLine [ lCount2 ].AnualSummaryValuesLine [ dIndex ].FData;
      FPriodLengthData [ dIndex ].FRecurrenceIntaval [ lCount2 ] := lRecurrenceBlock.FAnualValues.AnualSummaryValuesLine [ lCount2 ].AnualSummaryValuesLine [ dIndex ].FData;
      FPriodLengthData [ dIndex ].CalculateReliabilityOfSupply ( lCount2 );
    end;
    if dRecord then
      lRecordCounter := lRecordCounter + 1;
  end;
begin
  Result := False;
  try
    if ( aFileBlocks.Count > 0 ) then
    begin
      for lIndex := 0 to aFileBlocks.Count - 1 do
      begin
        if lIndex = ( aFileBlocks.Count - 1 ) then
        begin
          Result := True;
          Exit;
        end;
        if ( aFileBlocks [ lIndex ] is TSumOutRecurrenceIntervalBlockValues ) and
           ( aDeficitBlock [ lIndex ] is TSumOutAnualSummaryBlockValues ) then
        begin
          lBlockValues := TSumOutRecurrenceIntervalBlockValues ( aFileBlocks [ lIndex ] );
          lRecurrenceBlock := TSumOutRecurrenceIntervalBlockValues ( aFileBlocks [ lIndex + 1 ] );
          lDeficitBlock := TSumOutAnualSummaryBlockValues ( aDeficitBlock [ lIndex ] );
          if not ( lBlockValues.FPeriodLength [ lIndex ].FInitalised ) and
             not ( lRecurrenceBlock.FValues [ lIndex + 1 ].FInitalised ) and
             not ( lDeficitBlock.FValues [ lIndex ].FInitalised ) then
            Break;
          lPeriodCount := 0;
          for lCount := 0 to 9 do
          begin
            FPeriodLength [ lCount ] := lBlockValues.FPeriodLength [ lCount ].FData;
            if lBlockValues.FPeriodLength [ lCount ].FData <> 0 then
              lPeriodCount := lPeriodCount + 1;
          end;
          lTDIndex := 3;
          lRecordCounter := 0;
          for lCount2 := 0 to 9 do
          begin
            LoadDataForPeriodLenght;
            lTDIndex := lTDIndex + 1;
          end;
          for lCount2 := 0 to lPeriodCount - 1 do
            FPriodLengthData [ lCount2 ].RecordCounter := lRecordCounter;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.LoadNoOfFuilureSeqFromFile ( aFileBlocks : TObjectList; const aIndexProperty : array of integer ) : boolean;
const OPNAME = 'TStochasticSystemYieldDataObject.LoadNoOfFuilureSeqFromFile';
{
var
  lBlockValues : TSumOutRecurrenceIntervalBlockValues;
  lcount,
  lIndex,
  lIndex2 : integer;
}
begin
  Result := False;
  try
{    lIndex := AIndexProperty[0];
    if ( lIndex >= 0 ) and ( lIndex <= aFileBlocks.Count ) then
    begin
      if ( aFileBlocks [ lIndex ] is TSumOutRecurrenceIntervalBlockValues ) then
      begin
        lBlockValues := TSumOutRecurrenceIntervalBlockValues ( aFileBlocks [ lIndex ] );
        for lCount := High ( lBlockValues.FPeriodLength ) to Low ( lBlockValues.FPeriodLength ) do
        begin
          if not ( lBlockValues.FPeriodLength [ lCount ].FInitalised ) then
            Break;
          for lIndex2 := 0 to 9 do
          begin
            FPeriodLength [ lIndex2 ] := lBlockValues.FPeriodLength [ lIndex2 ].FData
//            FTargetDraft [ lIndex2 ] := lBlockValues.FTargetDraft [ lIndex2 ].FData;

//            FNoOfFaiureSeq [ lIndex2 ] := lBlockValues.FNumberOfFailureSeqValues [ lIndex ].FData;
          end;

 
        end;
      end;
    end;
    Result := True;
}
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStochasticSystemYieldDataObject.LoadRecurrenceIntervalFromFile ( aFileBlocks: TObjectList;
                                                                           const aIndexProperty : array of integer ) : boolean;
const OPNAME = 'TStochasticSystemYieldDataObject.LoadRecurrenceIntervalFromFile';
begin
  Result := False;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.Reset;
const OPNAME = 'TStochasticSystemYieldDataObject.Reset';
var
  lIndex : integer;
begin
  inherited Reset;
  try
//    FNumberOfSeqAnalysed := 0;
    for lIndex := 0 to 9 do
    begin
      FPeriodLength [ lIndex ] := 0;
      FRecurrenceIntaval [ lIndex ] := 0;
      FReliabilityOfSupply [ lIndex ] := 0;
      FNoOfFaiureSeq [ lIndex ] := 0;
      FTargetDraft [ lIndex ] := 0;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.SetNoOfFaiureSeq ( aIndex : integer; const aValue : double );
const OPNAME = 'TStochasticSystemYieldDataObject.SetNoOfFaiureSeq';
begin
  try
    FNoOfFaiureSeq [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.SetNumberOfSeqAnalysed ( const aValue : integer );
const OPNAME = 'TStochasticSystemYieldDataObject.SetNumberOfSeqAnalysed';
var
  lIndex : integer;
begin
  try
    FNoOfSeqAnalysed := aValue;
    for lIndex := 0 to 9 do
      FPriodLengthData [ lIndex ].FNoOfSeqAnalysed := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.SetPeriodLength ( aIndex : integer; const aValue : integer );
const OPNAME = 'TStochasticSystemYieldDataObject.SetPeriodLength';
begin
  try
    FPeriodLength [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetNoOfFaiureSeqByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetNoOfFaiureSeqByIndex';
begin
  Result := 0;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).GetNoOfFaiureSeq ( aIndex );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetNumberOfSeqAnalysed : integer;
const OPNAME = 'TSystemYieldObject.GetNumberOfSeqAnalysed';
begin
  Result := 0;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).NoOfSeqAnalysed;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetPeriodLength : integer;
const OPNAME = 'TSystemYieldObject.GetPeriodLength';
begin
  Result := 0;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetRecurrenceIntavalByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetRecurrenceIntavalByIndex';
begin
  Result := 0.00;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).GetRecurrenceIntaval ( aIndex );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetReliabilityOfSupplyByIndex ( aIndex : integer ) : double;
const OPNAME = 'TSystemYieldObject.GetReliabilityOfSupplyByIndex';
begin
  Result := 0;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).GetReliabilityOfSupply ( aIndex );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSystemYieldObject.GetTargetDraftByIndex ( aIndex : integer) : double;
const OPNAME = 'TSystemYieldObject.GetTargetDraftByIndex';
begin
  Result := 0.00;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).GetTargetDraft ( aIndex );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TStochasticSystemYieldDataObject.SetRecurrenceIntaval ( aIndex : integer; const aValue : double );
const OPNAME = 'TStochasticSystemYieldDataObject.SetRecurrenceIntaval';
begin
  try
    FRecurrenceIntaval [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.SetReliabilityOfSupply ( aIndex : integer; const aValue : double );
const OPNAME = 'TStochasticSystemYieldDataObject.SetReliabilityOfSupply';
begin
  try
    FReliabilityOfSupply [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStochasticSystemYieldDataObject.SetTargetDraft ( aIndex : integer; const aValue : double );
const OPNAME = 'TStochasticSystemYieldDataObject.SetTargetDraft';
begin
  try
    FTargetDraft [ aIndex ] := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{ TPeriodLength }

procedure TPeriodLength.CreateMemberObjects;
const OPNAME = 'TPeriodLength.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPeriodLength.DestroyMemberObjects;
const OPNAME = 'TPeriodLength.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPeriodLength.GetPeriodCount : integer;
const OPNAME = 'TPeriodLength.GetPeriodCount';
begin
  Result := 0;
  try
    Result := FPeriodCount;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TPeriodLength.Initialise : Boolean;
const OPNAME = 'TPeriodLength.Initialise';
var
  lIndex : integer;
begin
  Result := False;
  try
    FPeriod := 0;
    for lIndex := 0 to 9 do
    begin
      RecurrenceIntaval [ lIndex ] := 0.00;
      ReliabilityOfSupply [ lIndex ] := 0.00;
      TargetDraft [ lIndex ] := 0.00;
      NoOfFaiureSeq [ lIndex ] := 0.00;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TSystemYieldObject.SetNumberOfSeqAnalysed ( const aValue : integer );
const OPNAME = 'TSystemYieldObject.SetNumberOfSeqAnalysed';
begin
  try
    TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).NoOfSeqAnalysed := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TSystemYieldObject.GetPriodLengthDataByIndex ( aIndex : integer) : TPeriodLength;
const OPNAME = 'TSystemYieldObject.GetPriodLengthDataByIndex';
begin
  Result := nil;
  try
    Result := TStochasticSystemYieldDataObject ( FStochasticSystemYieldDataObject ).PriodLengthData [ aIndex ]; 
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TPeriodLength.SetPeriodCount ( const aValue : integer );
const OPNAME = 'TPeriodLength.SetPeriodCount';
begin
  try
  FPeriodCount := aValue;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
