//
//  UNIT      : Contains UCautionarySplashScreenValidator Class
//  AUTHOR    : Oagilwe Segola
//  DATE      : 2008/10/28
//  COPYRIGHT : Copyright © 2004 DWAF
//

unit UCautionarySplashScreenValidator;

interface

uses
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Types,
  Vcl.Grids,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UCautionarySplashScreenDialog,
  UAbstractYieldDataDialogValidator;

type

  TCautionarySplashScreenValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnShowScreenChkBoxClick(ASender : TObject);
    procedure DrawCell(ASender : TObject; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

    procedure RePopulateDataViewer;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function CautionarySplashScreenDialog: TCautionarySplashScreenDialog;
  end;

implementation
uses
  SysUtils,
  Windows,
  UErrorHandlingOperations;

{ TCautionarySplashScreenValidator }

procedure TCautionarySplashScreenValidator.CreateMemberObjects;
const OPNAME = 'TCautionarySplashScreenValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TCautionarySplashScreenDialog.Create(FPanelOwner,FAppModules);

    with CautionarySplashScreenDialog do
    begin
      IssueStringGrid.OnEnter    := OnEditControlEnter;
      IssueStringGrid.OnDrawCell := DrawCell;

      if(ShowScreenChkBox <> nil) then
      begin
        ShowScreenChkBox.OnEnter := OnEditControlEnter;
        ShowScreenChkBox.OnClick := OnShowScreenChkBoxClick;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.DestroyMemberObjects;
const OPNAME = 'TCautionarySplashScreenValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TCautionarySplashScreenValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TCautionarySplashScreenValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.RePopulateDataViewer;
const OPNAME = 'TCautionarySplashScreenValidator.RePopulateDataViewer';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.DrawCell(ASender : TObject; ACol, ARow: Integer; ARect: TRect;AState: TGridDrawState);
const OPNAME = 'TCautionarySplashScreenValidator.DrawCell';
begin
  try
    TStringGrid(ASender).Canvas.FillRect(ARect);
    DrawText(TStringGrid(ASender).Canvas.Handle,pchar(TStringGrid(ASender).Cells[ACol, ARow]),Length(TStringGrid(ASender).Cells[ACol, ARow]),
                           ARect, DT_CENTER OR DT_NOPREFIX OR DT_WORDBREAK );
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TCautionarySplashScreenValidator.OnShowScreenChkBoxClick(ASender: TObject);
const OPNAME = 'TCautionarySplashScreenValidator.OnShowScreenChkBoxClick';
var
  LShowAgain : boolean;
begin
  try
    LShowAgain := TCheckBox(ASender).Checked; 
    FAppModules.ViewIni.WriteString('TYieldModelManager','ShowScreenAgain',BoolToStr(LShowAgain,True));
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TCautionarySplashScreenValidator.CautionarySplashScreenDialog: TCautionarySplashScreenDialog;
const OPNAME = 'TCautionarySplashScreenValidator.CautionarySplashScreenDialog';
begin
  Result := Nil;
  try
    if(FPanel <> nil) then
      Result := TCautionarySplashScreenDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.ClearDataViewer;
const OPNAME = 'TCautionarySplashScreenValidator.ClearDataViewer';
var
  LRow : integer;
  LCol : integer;
begin
  inherited ClearDataViewer;
  try
    with CautionarySplashScreenDialog do
    begin
      for LRow := 1 to IssueStringGrid.RowCount - 1 do
      begin
        for LCol := 0 to IssueStringGrid.ColCount - 1 do
          IssueStringGrid.Cells[LCol, LRow] := '';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenValidator.Initialise: boolean;
const OPNAME = 'TCautionarySplashScreenValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenValidator.LanguageHasChanged: boolean;
const OPNAME = 'TCautionarySplashScreenValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCautionarySplashScreenValidator.PopulateDataViewer;
const OPNAME = 'TCautionarySplashScreenValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenValidator.SaveState: boolean;
const OPNAME = 'TCautionarySplashScreenValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCautionarySplashScreenValidator.StudyHasChanged: boolean;
const OPNAME = 'TCautionarySplashScreenValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
 