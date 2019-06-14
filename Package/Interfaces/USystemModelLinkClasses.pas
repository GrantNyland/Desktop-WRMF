//
//
//  UNIT      : Contains link classes used by the yield model.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/13
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit USystemModelLinkClasses;

interface

uses
  Messages,
  UStringListOfStringLists;

const
  UM_LoadModelManager = WM_User + 100;
  UM_NewStudyDetails  = WM_User + 101;

type
  TStudyLabels = class(TObject)
  protected
    FStudyLabel    : string;
    FModelLabel    : string;
    FSubAreaLabel  : string;
    FScenarioLabel : string;
  public
    property StudyLabel: string read FStudyLabel write FStudyLabel;
    property ModelLabel: string read FModelLabel write FModelLabel;
    property SubAreaLabel: string read FSubAreaLabel write FSubAreaLabel;
    property ScenarioLabel: string read FScenarioLabel write FScenarioLabel;
  end;

  TAbstractSystemModelLinkClass = class(TObject)
  public
    constructor Create;
    procedure Reset; virtual; abstract;
    procedure AssignFrom(AObject: TObject); virtual; abstract;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{ TAbstractSystemModelLinkClass }

constructor TAbstractSystemModelLinkClass.Create;
const OPNAME = 'TAbstractSystemModelLinkClass.Create';
begin
  inherited Create;
  try
    Reset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
