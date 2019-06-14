//
//
//  UNIT      : Contains  TRainfallToolBar and TRainfallMenuItemManager Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallMenuItemManager;

interface
uses

  UMenuItemManager,
  UGenericModelLinkClasses,
  UHelpContexts,
  UAbstractComponent,
  UAbstractObject,
  VCL.Dialogs,
  VCL.Menus,
  Classes,
  Windows;

type
  TRainfallMenuItemManager = class ( TMenuItemManager )
  protected
    procedure DisableAllMenus;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    procedure AddMenuItems; override;
    function  Initialise: boolean; override;
    function  StudyHasChanged: boolean; override;
    function  LanguageHasChanged: boolean; override;
    procedure Show; override;

  end;

implementation

uses
  SysUtils,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  CRainfallData                           : array[0..0] of string = ('Data');
  CHelpRainfallUserGuide                  : array[0..1] of string = ('Help','HelpRainfallUserGuide');
  CHelpRainfallTrainingMaterial           : array[0..1] of string = ('Help','HelpRainfallTrainingMaterial');
  CHelpRainfallUserGuideFile              : array[0..2] of string = ('Help','HelpRainfallUserGuide','HelpUserManual');
  CHelpRainfallPatchROutput               : array[0..2] of string = ('Help','HelpRainfallUserGuide','HelpRainfallPatchROutput');
  CHelpRainfallClassROutput               : array[0..2] of string = ('Help','HelpRainfallUserGuide','HelpRainfallClassROutput');
  CHelpRainfallSAWSNumbering              : array[0..2] of string = ('Help','HelpRainfallUserGuide','HelpRainfallSAWSNumbering');
  CHelpRainfallCLASSRAndPATCHRMethodology : array[0..2] of string = ('Help','HelpRainfallUserGuide','HelpRainfallCLASSRAndPATCHRMethodology');

procedure TRainfallMenuItemManager.AddMenuItems;
const OPNAME = 'TRainfallMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    AddMenuItemEntry(CRainfallData,  800);
    AddMenuItemEntry(CHelpRainfallUserGuide,                  230);
    AddMenuItemEntry(CHelpRainfallUserGuideFile,              240, CmeRainfallUserGuide);
    AddMenuItemEntry(CHelpRainfallPatchROutput,               250, CmeRainfallSummaryOfPatchROutput, nil);
    AddMenuItemEntry(CHelpRainfallClassROutput,               270, CmeRainfallSummaryOfClassROutput, nil);
    AddMenuItemEntry(CHelpRainfallSAWSNumbering,              290, CmeRainfallSAWSNumbering, nil);
    AddMenuItemEntry(CHelpRainfallCLASSRAndPATCHRMethodology, 310, CmeRainfallCLASSRAndPATCHRMethodology, nil);
    AddMenuItemEntry(CHelpRainfallTrainingMaterial,           240, CmeRainfallTrainingMaterial);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRainfallMenuItemManager.CreateMemberObjects;
const OPNAME = 'TRainfallMenuItemManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except  on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMenuItemManager.DestroyMemberObjects;
const OPNAME = 'TRainfallMenuItemManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMenuItemManager.DisableAllMenus;
const OPNAME = 'TRainfallMenuItemManager.DisableAllMenus';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMenuItemManager.Initialise: boolean;
const OPNAME = 'TRainfallMenuItemManager.Initialise';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMenuItemManager.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallMenuItemManager.LanguageHasChanged';
begin
  Result := false;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMenuItemManager.Show;
const OPNAME = 'TRainfallMenuItemManager.Show';
begin
  try
    inherited Show;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMenuItemManager.StudyHasChanged: boolean;
const OPNAME = 'TRainfallMenuItemManager.StudyHasChanged';
begin
  Result := false;
  try
    Result := true;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
