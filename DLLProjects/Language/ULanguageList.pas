//
//  UNIT      : Contains TLanguage Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/07
//  COPYRIGHT : Copyright © 2002 DWAF
//
unit ULanguageList;

interface

uses
  Classes,
  ULanguage,
  UAbstractObject;

type
  TLanguageList = class(TAbstractAppObject)
  protected
    FLanguages: TStringList;
    FCurrentLanguage: TLanguage;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetLanguage(ALangID: string): TLanguage;
    function LoadEnglish: boolean;
  public
    procedure Clear;
    function Count: integer;
    function Initialise: boolean; override;
    function SetCurrentLanguage(ALanguageID: string): boolean;
    property Language[ALangID: string]: TLanguage read GetLanguage; default;
    property CurrentLanguage: TLanguage read FCurrentLanguage;
  end;

implementation

uses
  SysUtils,
  ULanguageText_ENG_Buttons,
  ULanguageText_ENG_Fields,
  ULanguageText_ENG_FileAgentMessages,
  ULanguageText_ENG_General,
  ULanguageText_ENG_HelpAbout,
  ULanguageText_ENG_Menus,
  ULanguageText_ENG_ModelAgentMessages,
  ULanguageText_ENG_StudySelection,
  ULanguageText_ENG_ViewData,
  ULanguageText_ENG_Validation,
  ULanguageText_ENG_Rainfall,
  ULanguageText_ENG_Planning,
  ULanguageText_ENG_BSP,
  ULanguageText_ENG_BSP_Other,
  ULanguageText_ENG_VNV,
  UErrorHandlingOperations;

procedure TLanguageList.CreateMemberObjects;
const OPNAME = 'TLanguageList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FLanguages := TStringList.Create;
    FLanguages.Sorted := True;
    FLanguages.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLanguageList.DestroyMemberObjects;
const OPNAME = 'TLanguageList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FLanguages);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TLanguageList.Clear;
const OPNAME = 'TLanguageList.Clear';
var LIndex: integer;
begin
  try
    for LIndex := 0 to FLanguages.Count - 1 do
    begin
      FLanguages.Objects[LIndex].Free;
      FLanguages.Objects[LIndex] := nil;
    end;
    FLanguages.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageList.Count;
const OPNAME = 'TLanguageList.Count';
begin
  Result := 0;
  try
    Result := FLanguages.Count;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageList.GetLanguage(ALangID: string): TLanguage;
const OPNAME = 'TLanguageList.GetLanguage';
var LIndex: integer;
begin
  Result := nil;
  try
    LIndex := FLanguages.IndexOf(ALangID);
    if (LIndex >= 0) then
      Result := TLanguage(FLanguages.Objects[LIndex]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageList.SetCurrentLanguage(ALanguageID: string): boolean;
const OPNAME = 'TLanguageList.SetCurrentLanguage';
begin
  Result := False;
  try
    if (not Assigned(Language[ALanguageID])) then
    begin
      raise Exception.CreateFmt('Unknown language [%s]', [ALanguageID]);
    end else begin
      FCurrentLanguage := Language[ALanguageID];
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageList.Initialise: boolean;
const OPNAME = 'TLanguageList.Initialise';
begin
  Result := False;
  try
    Clear;
    Result := LoadEnglish;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TLanguageList.LoadEnglish: boolean;
const OPNAME = 'TLanguageList.LoadEnglish';
var LLanguage: TLanguage;
begin
  Result := False;
  try

    // Create the language.
    LLanguage := TLanguage.Create(FAppModules);
    LLanguage.LanguageID := 'ENG';
    LLanguage.Description := 'English';

    // Load the language text.
    ULanguageText_ENG_Buttons.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_Fields.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_FileAgentMessages.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_General.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_Rainfall.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_Planning.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_BSP.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_BSP_Other.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_HelpAbout.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_Menus.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_ModelAgentMessages.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_StudySelection.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_ViewData.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_Validation.LoadLanguageText(LLanguage.AddTextItem);
    ULanguageText_ENG_VNV.LoadLanguageText(LLanguage.AddTextItem);
    // Add the language to the list.
    FLanguages.AddObject('ENG', LLanguage);
    Result := True;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
