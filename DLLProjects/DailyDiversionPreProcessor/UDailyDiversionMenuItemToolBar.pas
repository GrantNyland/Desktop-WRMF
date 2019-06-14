//
//
//  UNIT      : Contains TDailyDiversionMenuItemToolBar Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UDailyDiversionMenuItemToolBar;


interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TDailyDiversionMenuItemToolBar = class(TChildToolBar)
  protected
    FCreateDailyDiversionBtn : TAbstractSpeedButton;
    FRenameDailyDiversionBtn : TAbstractSpeedButton;
    FDeleteDailyDiversionBtn : TAbstractSpeedButton;

    FImportDailyFlowDataFromCSVFileBtn : TAbstractSpeedButton;
    FClearDailyFlowDataFromCSVFileBtn : TAbstractSpeedButton;
    FImportFile14Btn : TAbstractSpeedButton;
    FClearFile14Btn : TAbstractSpeedButton;
    FImportDailyInstreamFlowFileBtn : TAbstractSpeedButton;
    FClearDailyInstreamFlowFileBtn : TAbstractSpeedButton;

    FGenerateFlowDiversionRelationBtn : TAbstractSpeedButton;
    FClearFlowDiversionRelationBtn : TAbstractSpeedButton;

    //FGenerateWRYMDataBtn: TAbstractSpeedButton;
//    FClearWRYMDataBtn: TAbstractSpeedButton;

    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure AssignHelpContext; override;
    procedure OnClickCreateDailyDiversion(Sender: TObject);
    procedure OnClickRenameDailyDiversion(Sender: TObject);
    procedure OnClickDeleteDailyDiversion(Sender: TObject);
    procedure OnClickImportDailyFlowDataFromCSVFile(Sender: TObject);
    procedure OnClickClearDailyFlowDataFromCSVFile(Sender: TObject);
    procedure OnClickImportDailyInstreamFlowFile(Sender: TObject);
    procedure OnClickClearDailyInstreamFlowFile(Sender: TObject);
    procedure OnClickGenerateFlowDiversionRelation(Sender: TObject);
    procedure OnClickClearFlowDiversionRelation(Sender: TObject);

    procedure OnClickGenerateWRYMData(Sender: TObject);
    procedure OnClickClearWRYMData(Sender: TObject);
    procedure OnClickImportFile14Btn(Sender: TObject);
    procedure OnClearFile14Btn(Sender : TObject);

  public
    function LanguageHasChanged: boolean; override;
    procedure SetCreateDailyDiversion(AEnabled: boolean);
    procedure SetRenameDailyDiversion(AEnabled: boolean);
    procedure SetDeleteDailyDiversion(AEnabled: boolean);
    procedure SetImportDailyFlowDataFromCSVFileBtn(AEnabled: boolean);
    procedure SetClearDailyFlowDataFromCSVFileBtn(AEnabled: boolean);
    procedure SetImportDailyInstreamFlowFileBtn(AEnabled: boolean);
    procedure SetClearDailyInstreamFlowFileBtn(AEnabled: boolean);
    procedure SetGenerateFlowDiversionRelationBtn(AEnabled: boolean);
    procedure SetClearFlowDiversionRelationBtn(AEnabled: boolean);
    procedure SetCImportFile14Btn(AEnabled: boolean);
    //procedure SetGenerateWRYMDataBtn(AEnabled: boolean);
    procedure SetClearFile14Btn(AEnabled: boolean);
//    procedure SetClearWRYMDataBtn(AEnabled: boolean);

    property CreateDailyDiversionBtn : TAbstractSpeedButton read FCreateDailyDiversionBtn;
    property RenameDailyDiversionBtn : TAbstractSpeedButton read FRenameDailyDiversionBtn;
    property DeleteDailyDiversionBtn : TAbstractSpeedButton read FDeleteDailyDiversionBtn;
    property ImportDailyFlowDataFromCSVFileBtn : TAbstractSpeedButton read FImportDailyFlowDataFromCSVFileBtn;
    property ClearDailyFlowDataFromCSVFileBtn : TAbstractSpeedButton read FClearDailyFlowDataFromCSVFileBtn;
    property ImportFile14Btn : TAbstractSpeedButton read FImportFile14Btn;
    property ClearFile14Btn : TAbstractSpeedButton read FClearFile14Btn; 
    property ImportDailyInstreamFlowFileBtn : TAbstractSpeedButton read FImportDailyInstreamFlowFileBtn;
    property ClearDailyInstreamFlowFileBtn : TAbstractSpeedButton read FClearDailyInstreamFlowFileBtn;
    property GenerateFlowDiversionRelationBtn : TAbstractSpeedButton read FGenerateFlowDiversionRelationBtn;
    property ClearFlowDiversionRelationBtn : TAbstractSpeedButton read FClearFlowDiversionRelationBtn;

    //property GenerateWRYMDataBtn: TAbstractSpeedButton read FGenerateWRYMDataBtn;
//    property ClearWRYMDataBtn: TAbstractSpeedButton read FClearWRYMDataBtn;

    procedure TabHasChanged(AGridTabSelected: boolean);


  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TDailyDiversionMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TDailyDiversionMenuItemToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateDailyDiversionBtn  := CreateButton('CreateDailyDiversion');
    FCreateDailyDiversionBtn.OnClick  := OnClickCreateDailyDiversion;
    FRenameDailyDiversionBtn  := CreateButton('RenameDailyDiversion');
    FRenameDailyDiversionBtn.OnClick  := OnClickRenameDailyDiversion;

    FDeleteDailyDiversionBtn  := CreateButton('DeleteDailyDiversion');
    FDeleteDailyDiversionBtn.OnClick := OnClickDeleteDailyDiversion;

    FImportDailyFlowDataFromCSVFileBtn  := CreateButton('ImportDailyFlowDataFromCSVFile');
    FImportDailyFlowDataFromCSVFileBtn.OnClick := OnClickImportDailyFlowDataFromCSVFile;
    FClearDailyFlowDataFromCSVFileBtn  := CreateButton('ClearDailyFlowDataFromCSVFile');
    FClearDailyFlowDataFromCSVFileBtn.OnClick := OnClickClearDailyFlowDataFromCSVFile;

    FImportFile14Btn  := CreateButton('ImportFile14');
    FClearFile14Btn   := CreateButton('ClearFile14');
    FImportDailyInstreamFlowFileBtn := CreateButton('ImportDailyInstreamFlowFile');
    FImportDailyInstreamFlowFileBtn.OnClick := OnClickImportDailyInstreamFlowFile;
    FClearDailyInstreamFlowFileBtn := CreateButton('ClearDailyInstreamFlowFile');
    FClearDailyInstreamFlowFileBtn.OnClick := OnClickClearDailyInstreamFlowFile;

{    FGenerateFlowDiversionRelationBtn := CreateButton('GenerateFlowDiversionRelation');
    FGenerateFlowDiversionRelationBtn.OnClick := OnClickGenerateFlowDiversionRelation;
    FClearFlowDiversionRelationBtn := CreateButton('ClearFlowDiversionRelation');
    FClearFlowDiversionRelationBtn.OnClick := OnClickClearFlowDiversionRelation;
 }
    FGenerateFlowDiversionRelationBtn := CreateButton('GenerateFlowDiversionRelation');
    FGenerateFlowDiversionRelationBtn.OnClick := OnClickGenerateFlowDiversionRelation;
    FClearFlowDiversionRelationBtn := CreateButton('ClearFlowDiversionRelation');
    FClearFlowDiversionRelationBtn.OnClick := OnClickClearFlowDiversionRelation;

    //FGenerateWRYMDataBtn := CreateButton('GenerateWRYMData');
    //FGenerateWRYMDataBtn.OnClick := OnClickGenerateWRYMData;
    FImportFile14Btn.OnClick := OnClickImportFile14Btn;
    FClearFile14Btn.OnClick := OnClearFile14Btn;
//    FClearWRYMDataBtn := CreateButton('ClearWRYMData');
//    FClearWRYMDataBtn.OnClick := OnClickClearWRYMData;

    FCreateDailyDiversionBtn.Hint := FAppModules.Language.GetString('ButtonHint.CreateDailyDiversion');
    FRenameDailyDiversionBtn.Hint := FAppModules.Language.GetString('ButtonHint.RenameDailyDiversion');
    FDeleteDailyDiversionBtn.Hint := FAppModules.Language.GetString('ButtonHint.DeleteDailyDiversion');
    FImportDailyFlowDataFromCSVFileBtn.Hint := FAppModules.Language.GetString('ButtonHint.ImportDailyFlowDataFromCSVFile');
    FClearDailyFlowDataFromCSVFileBtn.Hint := FAppModules.Language.GetString('ButtonHint.ClearDailyFlowDataFromCSVFile');
    FImportFile14Btn.Hint := FAppModules.Language.GetString('ButtonHint.ImportFile14');
    FClearFile14Btn.Hint := FAppModules.Language.GetString('ButtonHint.ClearFile14');
    FImportDailyInstreamFlowFileBtn.Hint :=FAppModules.Language.GetString('ButtonHint.ImportDailyInstreamFlowFile');
    FClearDailyInstreamFlowFileBtn.Hint :=FAppModules.Language.GetString('ButtonHint.ClearDailyInstreamFlowFile');
    FGenerateFlowDiversionRelationBtn.Hint :=FAppModules.Language.GetString('ButtonHint.GenerateFlowDiversionRelation');
    FClearFlowDiversionRelationBtn.Hint :=FAppModules.Language.GetString('ButtonHint.ClearFlowDiversionRelation');

    //FGenerateWRYMDataBtn.Hint :=FAppModules.Language.GetString('ButtonHint.GenerateWRYMData');
//    FClearWRYMDataBtn.Hint :=FAppModules.Language.GetString('ButtonHint.ClearWRYMData');

    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.AssignHelpContext;
const OPNAME = 'TDailyDiversionMenuItemToolBar.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDailyDiversionMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FCreateDailyDiversionBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FRenameDailyDiversionBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FDeleteDailyDiversionBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportDailyFlowDataFromCSVFileBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearDailyFlowDataFromCSVFileBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportFile14Btn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearFile14Btn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FImportDailyInstreamFlowFileBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearDailyInstreamFlowFileBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FGenerateFlowDiversionRelationBtn, True, False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FClearFlowDiversionRelationBtn, True, False, LButtonCount, LGaps);
    //SetButtonHorizontalPosition(FGenerateWRYMDataBtn, True, False, LButtonCount, LGaps);
//    SetButtonHorizontalPosition(FClearWRYMDataBtn, True, False, LButtonCount, LGaps);

    Width := FClearFlowDiversionRelationBtn.Left + FClearFlowDiversionRelationBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TDailyDiversionMenuItemToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FCreateDailyDiversionBtn.LanguageHasChanged;
    FRenameDailyDiversionBtn.LanguageHasChanged;
    FDeleteDailyDiversionBtn.LanguageHasChanged;
    FImportDailyFlowDataFromCSVFileBtn.LanguageHasChanged;
    FClearDailyFlowDataFromCSVFileBtn.LanguageHasChanged;
    FImportFile14Btn.LanguageHasChanged;
    FClearFile14Btn.LanguageHasChanged;
    FImportDailyInstreamFlowFileBtn.LanguageHasChanged;
    FClearDailyInstreamFlowFileBtn.LanguageHasChanged;
    FGenerateFlowDiversionRelationBtn.LanguageHasChanged;
    FClearFlowDiversionRelationBtn.LanguageHasChanged;
    //FGenerateWRYMDataBtn.LanguageHasChanged;
//    FClearWRYMDataBtn.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetCreateDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetCreateDailyDiversion';
begin
  try
    SetButtonEnabled(FCreateDailyDiversionBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetRenameDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetRenameDailyDiversion';
begin
  try
    SetButtonEnabled(FRenameDailyDiversionBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TDailyDiversionMenuItemToolBar.SetDeleteDailyDiversion(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetDeleteDailyDiversion';
begin
  try
    SetButtonEnabled(FDeleteDailyDiversionBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetImportDailyFlowDataFromCSVFileBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetImportDailyFlowDataFromCSVFileBtn';
begin
  try
    SetButtonEnabled(FImportDailyFlowDataFromCSVFileBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetClearDailyFlowDataFromCSVFileBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetClearDailyFlowDataFromCSVFileBtn';
begin
  try
    SetButtonEnabled(FClearDailyFlowDataFromCSVFileBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetImportDailyInstreamFlowFileBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetImportDailyInstreamFlowFileBtn';
begin
  try
    SetButtonEnabled(FImportDailyInstreamFlowFileBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetClearDailyInstreamFlowFileBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetClearDailyInstreamFlowFileBtn';
begin
  try
    SetButtonEnabled(FClearDailyInstreamFlowFileBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetGenerateFlowDiversionRelationBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetGenerateFlowDiversionRelationBtn';
begin
  try
    SetButtonEnabled(FGenerateFlowDiversionRelationBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetClearFlowDiversionRelationBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetClearFlowDiversionRelationBtn';
begin
  try
    SetButtonEnabled(FClearFlowDiversionRelationBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetCImportFile14Btn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetCImportFile14Btn';
begin
  try
    SetButtonEnabled(FImportFile14Btn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionMenuItemToolBar.SetClearFile14Btn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetClearFile14Btn';
begin
  try
    SetButtonEnabled(FClearFile14Btn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;



{procedure TDailyDiversionMenuItemToolBar.SetGenerateWRYMDataBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetGenerateWRYMDataBtn';
begin
  try
    SetButtonEnabled(FGenerateWRYMDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
{
procedure TDailyDiversionMenuItemToolBar.SetClearWRYMDataBtn(AEnabled: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.SetClearWRYMDataBtn';
begin
  try
    SetButtonEnabled(FClearWRYMDataBtn, AEnabled, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
procedure TDailyDiversionMenuItemToolBar.TabHasChanged(AGridTabSelected: boolean);
const OPNAME = 'TDailyDiversionMenuItemToolBar.TabHasChanged';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickCreateDailyDiversion(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickCreateDailyDiversion';
begin
  try
    FAppModules.Model.ProcessEvent(cmeCreateDailyDiversion, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDailyDiversionMenuItemToolBar.OnClickRenameDailyDiversion(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickRenameDailyDiversion';
begin
  try
    FAppModules.Model.ProcessEvent(cmeRenameDailyDiversion, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickDeleteDailyDiversion(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickDeleteDailyDiversion';
begin
  try
    FAppModules.Model.ProcessEvent(cmeDeleteDailyDiversion, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickImportDailyFlowDataFromCSVFile(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickImportDailyFlowDataFromCSVFile';
begin
  try
    FAppModules.Model.ProcessEvent(cmeImportDailyFlowDataFromCSVFile, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickClearDailyFlowDataFromCSVFile(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickClearDailyFlowDataFromCSVFile';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearDailyFlowDataFromCSVFile, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickImportDailyInstreamFlowFile(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickImportDailyInstreamFlowFile';
begin
  try
    FAppModules.Model.ProcessEvent(cmeImportDailyInstreamFlowFile, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickClearDailyInstreamFlowFile(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickClearDailyInstreamFlowFile';
begin
  try
    FAppModules.Model.ProcessEvent(cmeClearDailyInstreamFlowFile, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickGenerateFlowDiversionRelation(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickGenerateFlowDiversionRelation';
begin
  try
    FAppModules.Model.ProcessEvent(cmeGenerateFlowDiversionRelation, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickClearFlowDiversionRelation(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickClearFlowDiversionRelation';
begin
  try
    FAppModules.Model.ProcessEvent(cmeClearFlowDiversionRelation, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickGenerateWRYMData(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickGenerateWRYMData';
begin
  try
    FAppModules.Model.ProcessEvent(CmeGenerateWRYMData, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickClearWRYMData(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickClearWRYMData';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearWRYMData, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClickImportFile14Btn(Sender: TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClickImportFile14Btn';
begin
  try
    FAppModules.Model.ProcessEvent(cmeImportFile14, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionMenuItemToolBar.OnClearFile14Btn(Sender : TObject);
const OPNAME = 'TDailyDiversionMenuItemToolBar.OnClearFile14Btn';
begin
  try
    FAppModules.Model.ProcessEvent(CmeClearFile14, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
