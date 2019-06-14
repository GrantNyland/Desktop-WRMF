unit UStudyObjects;

interface

uses
  Classes,
  Vcl.ComCtrls,
  UDWADBComponents,
  UAbstractObject,
  USystemModelLinkClasses;

type
  TModelDataObject = class;
  TSubAreaDataObject = class;
  TScenarioDataObject = class;
  TNodeLevel = (nlStudy, nlModel, nlSubArea, nlScenario);
  TFormActionState = (fasNone, fasEdit, fasDelete, fasAdd);

  TStudyDocumentList = class(TAbstractStudyDocumentList)
  protected
    function GetDocumentDetail(AContext: string): TStudyDocumentDetail; override;
  public
    procedure Clear; override;
    procedure AssignFrom(AStudyDocumentList: TStudyDocumentList);
    procedure AddDocument(ACategoryKey, AIdentifierKey, AFilename, ABookMark: string; APageNumber: integer);
    function IsDocumentSet(AContext: string): boolean; override;
    property DocumentDetail[AContext: string]: TStudyDocumentDetail read GetDocumentDetail;
  end;

  TStudySelectionNodeData = class(TObject)
  protected
    FNodeLevel: TNodeLevel;
    FNode: TTreeNode;
    FItemIndex: integer;
    FDocumentDetail: TStudyDocumentList;
    FEditable: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property NodeLevel: TNodeLevel read FNodeLevel write FNodeLevel;
    property Node: TTreeNode read FNode write FNode;
    property ItemIndex: integer read FItemIndex write FItemIndex;
    property DocumentDetail: TStudyDocumentList read FDocumentDetail;
    property Editable: Boolean  read FEditable write FEditable;
  end;

  TStudyDataObject = class(TStudySelectionNodeData)
  protected
    FStudy: string;
    FStudyDate: TDateTime;
    FStudyNumber: string;
    FClient: string;
    FConsultant: string;
    FStudyLabel: string;
    FStudyDescr: string;
    FStudyShapeFileName : string;
    FModel: TList;
    function GetModelCount: integer;
    function GetModel(AIndex: integer): TModelDataObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddModel(AModel: TModelDataObject);
    procedure AssignFrom(ADataObject:TStudyDataObject);
    procedure SetStudyShapeFileName ( aSelectedShapeFile : string );
    property Study: string read FStudy write FStudy;
    property StudyDate: TDateTime read FStudyDate write FStudyDate;
    property StudyNumber: string read FStudyNumber write FStudyNumber;
    property Client: string read FClient write FClient;
    property Consultant: string read FConsultant write FConsultant;
    property StudyLabel: string read FStudyLabel write FStudyLabel;
    property StudyDescr: string read FStudyDescr write FStudyDescr;
    property StudyShapeFileName : string read FStudyShapeFileName write FStudyShapeFileName;
    property ModelCount: integer read GetModelCount;
    property Model[AIndex: integer]: TModelDataObject read GetModel;
    property ModelList : TList read FModel;
  end;

  TModelDataObject = class(TStudySelectionNodeData)
  protected
    FStudy: TStudyDataObject;
    FModel: string;
    FSubModel: string;
    FModelLabel: string;
    FModelDescr: string;
    FSubArea: TList;
    function GetSubAreaCount: integer;
    function GetSubArea(AIndex: integer): TSubAreaDataObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSubArea(ASubArea: TSubAreaDataObject);
    procedure AssignFrom(ADataObject:TModelDataObject);
    property Study: TStudyDataObject read FStudy write FStudy;
    property Model: string read FModel write FModel;
    property SubModel: string read FSubModel write FSubModel;
    property ModelLabel: string read FModelLabel write FModelLabel;
    property ModelDescr: string read FModelDescr write FModelDescr;
    property SubAreaCount: integer read GetSubAreaCount;
    property SubArea[AIndex: integer]: TSubAreaDataObject read GetSubArea;
    property SubAreaList : TList read FSubArea;
  end;

  TSubAreaDataObject = class(TStudySelectionNodeData)
  protected
    FModel                : TModelDataObject;
    FSubArea              : string;
    FSubAreaLabel         : string;
    FSubAreaDescr         : string;
    FSubAreaShapeFileName : string;
    FTopLeftCoord         : double;
    FTopRightCoord        : double;
    FBottomLeftCoord      : double;
    FBottomRightCoord     : double;
    FScenario: TList;
    function GetScenarioCount: integer;
    function GetScenario(AIndex: integer): TScenarioDataObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddScenario(AScenario: TScenarioDataObject);
    procedure AssignFrom(ADataObject:TSubAreaDataObject);
    property Model: TModelDataObject read FModel write FModel;
    property SubArea: string read FSubArea write FSubArea;
    property SubAreaLabel: string read FSubAreaLabel write FSubAreaLabel;
    property SubAreaDescr: string read FSubAreaDescr write FSubAreaDescr;
    property SubAreaShapeFileName : string read FSubAreaShapeFileName write FSubAreaShapeFileName;
    property TopLeftCoord         : double read FTopLeftCoord     write FTopLeftCoord;
    property TopRightCoord        : double read FTopRightCoord    write FTopRightCoord;
    property BottomLeftCoord      : double read FBottomLeftCoord  write FBottomLeftCoord;
    property BottomRightCoord     : double read FBottomRightCoord write FBottomRightCoord;

    property ScenarioCount: integer read GetScenarioCount;
    property Scenario[AIndex: integer]: TScenarioDataObject read GetScenario;
    property ScenarioList : TList read FScenario;
  end;

  TScenarioDataObject = class(TStudySelectionNodeData)
  protected
    FSubArea: TSubAreaDataObject;
    FScenario: string;
    FScenarioLabel: string;
    FScenarioDescr: string;
    FDataFilesPrefix: string;
    FDataFilesPath: string;
    FFilesLoaded: boolean;
    FCalenderStartMonth: integer;
    FVersion: string;
    FDataImported: boolean;
  public
    constructor Create;
    procedure AssignFrom(ADataObject:TScenarioDataObject);
    property SubArea: TSubAreaDataObject read FSubArea write FSubArea;
    property Scenario: string read FScenario write FScenario;
    property ScenarioLabel: string read FScenarioLabel write FScenarioLabel;
    property ScenarioDescr: string read FScenarioDescr write FScenarioDescr;
    property DataFilesPrefix: string read FDataFilesPrefix write FDataFilesPrefix;
    property DataFilesPath: string read FDataFilesPath write FDataFilesPath;
    property FilesLoaded: boolean  read FFilesLoaded write FFilesLoaded;
    property CalenderStartMonth: integer  read FCalenderStartMonth write FCalenderStartMonth;
    property Version: string read FVersion write FVersion;
    property DataImported : boolean read FDataImported write FDataImported;
  end;

implementation

uses
  SysUtils;

{ TStudyDocumentList }

function TStudyDocumentList.GetDocumentDetail(AContext: string): TStudyDocumentDetail;
const OPNAME = 'TStudyDocumentList.GetDocumentDetail';
var LIndex: integer;
begin
  Result := nil;
  LIndex := IndexOf(AContext);
  if (LIndex > -1) then
    Result := TStudyDocumentDetail(Objects[LIndex]);
end;

function TStudyDocumentList.IsDocumentSet(AContext: string): boolean;
const OPNAME = 'TStudyDocumentList.IsDocumentSet';
begin
  Result := (IndexOf(AContext) > -1);
end;

procedure TStudyDocumentList.Clear;
const OPNAME = 'TStudyDocumentList.Clear';
var LIndex: integer;
begin
  for LIndex := 0 to Count - 1 do
  begin
    Objects[LIndex].Free;
    Objects[LIndex] := nil;
  end;
  inherited Clear;
end;

procedure TStudyDocumentList.AddDocument(ACategoryKey, AIdentifierKey, AFilename, ABookMark: string; APageNumber: integer);
const OPNAME = 'TStudyDocumentList.AddDocument';
var LDocumentDetail: TStudyDocumentDetail;
begin
  LDocumentDetail := TStudyDocumentDetail.Create;
  LDocumentDetail.CategoryKey   := ACategoryKey;
  LDocumentDetail.IdentifierKey := AIdentifierKey;
  LDocumentDetail.Filename      := AFilename;
  LDocumentDetail.BookMark      := ABookMark;
  LDocumentDetail.PageNumber    := APageNumber;
  AddObject(ABookMark, LDocumentDetail);
end;

procedure TStudyDocumentList.AssignFrom(AStudyDocumentList: TStudyDocumentList);
const OPNAME = 'TStudyDocumentList.AssignFrom';
var
  LIndex: integer;
  LDocumentDetail: TStudyDocumentDetail;
begin
  Clear;
  if Assigned(AStudyDocumentList) then
  begin
    for LIndex := 0 to AStudyDocumentList.Count - 1 do
    begin
      LDocumentDetail := TStudyDocumentDetail.Create;
      LDocumentDetail.AssignFrom(AStudyDocumentList.Objects[LIndex]);
      AddObject(AStudyDocumentList[LIndex], LDocumentDetail);
    end;
  end;
end;

{ TStudySelectionNodeData }

constructor TStudySelectionNodeData.Create;
begin
  inherited;
  FNodeLevel := nlStudy;
  FEditable  := True;
  FNode      := nil;
  FItemIndex := -1;
  FDocumentDetail := TStudyDocumentList.Create;
end;

destructor TStudySelectionNodeData.Destroy;
begin
  FreeAndNil(FDocumentDetail);
  inherited;
end;

{ TStudyDataObject }

constructor TStudyDataObject.Create;
begin
  inherited;
  FEditable := True;
  FNodeLevel := nlStudy;
  FModel := TList.Create;
end;

destructor TStudyDataObject.Destroy;
const OPNAME = 'TStudyDataObject.Destroy';
begin
  FreeAndNil(FModel);

  inherited;
end;

procedure TStudyDataObject.AddModel(AModel: TModelDataObject);
const OPNAME = 'TStudyDataObject.AddModel';
begin
  FModel.Add(AModel);
end;

function TStudyDataObject.GetModelCount: integer;
const OPNAME = 'TStudyDataObject.GetModelCount';
begin
  Result := FModel.Count;
end;

function TStudyDataObject.GetModel(AIndex: integer): TModelDataObject;
const OPNAME = 'TStudyDataObject.GetModel';
begin
  Result := TModelDataObject(FModel[AIndex]);
end;

procedure TStudyDataObject.AssignFrom(ADataObject: TStudyDataObject);
const OPNAME = 'TStudyDataObject.AssignFrom';
var
  LIndex: integer;
  LModel: TModelDataObject;
begin
  if Assigned(ADataObject) then
  begin
    FStudy        := ADataObject.FStudy;
    FStudyDate    := ADataObject.FStudyDate;
    FStudyNumber  := ADataObject.FStudyNumber;
    FClient       := ADataObject.FClient;
    FConsultant   := ADataObject.FConsultant;
    FStudyLabel   := ADataObject.FStudyLabel;
    FStudyDescr   := ADataObject.FStudyDescr;
    FModel.Clear;

    FStudyShapeFileName   := ADataObject.FStudyShapeFileName;

    for LIndex := 0 to ADataObject.FModel.Count -1 do
    begin
      LModel := TModelDataObject.Create;
      LModel.AssignFrom(TModelDataObject(ADataObject.FModel[LIndex]));
      LModel.FStudy := Self;
      FModel.Add(LModel);
    end;
  end;
end;

procedure TStudyDataObject.SetStudyShapeFileName ( aSelectedShapeFile : string );
const OPNAME = 'TStudyDataObject.SetStudyShapeFileName';
begin

end;

{ TModelDataObject }

constructor TModelDataObject.Create;
begin
  inherited;
  FEditable := True;
  FNodeLevel := nlModel;
  FSubArea := TList.Create;
end;

destructor TModelDataObject.Destroy;
begin
  FreeAndNil(FSubArea);
  inherited;
end;

procedure TModelDataObject.AddSubArea(ASubArea: TSubAreaDataObject);
const OPNAME = 'TModelDataObject.AddSubArea';
begin
  FSubArea.Add(ASubArea);
end;

function TModelDataObject.GetSubAreaCount: integer;
const OPNAME = 'TModelDataObject.GetSubAreaCount';
begin
  Result := FSubArea.Count;
end;

function TModelDataObject.GetSubArea(AIndex: integer): TSubAreaDataObject;
const OPNAME = 'TModelDataObject.GetSubArea';
begin
  Result := TSubAreaDataObject(FSubArea[AIndex]);
end;

procedure TModelDataObject.AssignFrom(ADataObject: TModelDataObject);
const OPNAME = 'TModelDataObject.AssignFrom';
var
  LIndex: integer;
  LSubArea: TSubAreaDataObject;
begin
  if Assigned(ADataObject) then
  begin
    FStudy      := ADataObject.FStudy;
    FModel      := ADataObject.FModel;
    FSubModel   := ADataObject.FSubModel;
    FModelLabel := ADataObject.FModelLabel;
    FModelDescr := ADataObject.FModelDescr;
    
    for LIndex := 0 to ADataObject.FSubArea.Count -1 do
    begin
      LSubArea := TSubAreaDataObject.Create;
      LSubArea.AssignFrom(TSubAreaDataObject(ADataObject.FSubArea[LIndex]));
      LSubArea.FModel := Self;
      FSubArea.Add(LSubArea);
    end;
  end;
end;

{ TSubAreaDataObject }

constructor TSubAreaDataObject.Create;
begin
  inherited;
  FEditable := True;
  FNodeLevel := nlSubArea;
  FScenario := TList.Create;
end;

destructor TSubAreaDataObject.Destroy;
begin
  FreeAndNil(FScenario);
  inherited;
end;

procedure TSubAreaDataObject.AddScenario(AScenario: TScenarioDataObject);
const OPNAME = 'TSubAreaDataObject.AddScenario';
begin
  FScenario.Add(AScenario);
end;

function TSubAreaDataObject.GetScenarioCount: integer;
const OPNAME = 'TSubAreaDataObject.GetScenarioCount';
begin
  Result := FScenario.Count;
end;

function TSubAreaDataObject.GetScenario(AIndex: integer): TScenarioDataObject;
const OPNAME = 'TSubAreaDataObject.GetScenario';
begin
  Result := TScenarioDataObject(FScenario[AIndex]);
end;

procedure TSubAreaDataObject.AssignFrom(ADataObject: TSubAreaDataObject);
const OPNAME = 'TSubAreaDataObject.AssignFrom';
var
  LIndex: integer;
  LScenario: TScenarioDataObject;
begin
  if Assigned(ADataObject) then
  begin
    FModel                := ADataObject.FModel;
    FSubArea              := ADataObject.FSubArea;
    FSubAreaLabel         := ADataObject.FSubAreaLabel;
    FSubAreaDescr         := ADataObject.FSubAreaDescr;
    FSubAreaShapeFileName := ADataObject.FSubAreaShapeFileName;
    FTopLeftCoord         := ADataObject.FTopLeftCoord;
    FTopRightCoord        := ADataObject.FTopLeftCoord;
    FBottomLeftCoord      := ADataObject.FBottomLeftCoord;
    FBottomRightCoord     := ADataObject.FBottomRightCoord;

    for LIndex := 0 to ADataObject.FScenario.Count -1 do
    begin
      LScenario := TScenarioDataObject.Create;
      LScenario.AssignFrom(TScenarioDataObject(ADataObject.FScenario[LIndex]));
      LScenario.FSubArea := Self;
      FScenario.Add(LScenario);
    end;
  end;
end;

{ TScenarioDataObject }

procedure TScenarioDataObject.AssignFrom(ADataObject: TScenarioDataObject);
const OPNAME = 'TScenarioDataObject.AssignFrom';
begin
  if Assigned(ADataObject) then
  begin
    FSubArea            := ADataObject.FSubArea;
    FScenario           := ADataObject.FScenario;
    FScenarioLabel      := ADataObject.FScenarioLabel;
    FScenarioDescr      := ADataObject.FScenarioDescr;
    FDataFilesPrefix    := ADataObject.FDataFilesPrefix;
    FDataFilesPath      := ADataObject.FDataFilesPath;
    FFilesLoaded        := ADataObject.FFilesLoaded;
    FCalenderStartMonth := ADataObject.FCalenderStartMonth;
    FVersion            := ADataObject.FVersion;
    FDataImported       := ADataObject.FDataImported;
  end;
end;

constructor TScenarioDataObject.Create;
begin
  inherited;
  FNodeLevel := nlScenario;
  FEditable := True;
end;

end.
