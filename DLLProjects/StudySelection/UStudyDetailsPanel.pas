//
//
//  UNIT      : Contains the class TStudyDetailsPanel.
//  AUTHOR    : Titi NGubane
//  DATE      : 2002/04/08
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudyDetailsPanel;

interface

uses
  Classes,
  Vcl.StdCtrls,
  Vcl.Graphics,
  UHelpContexts,
  UStudyObjects,
  UAbstractObject,
  UAbstractComponent;

type
  TStudyPanel = class(TAbstractPanel)
  protected
    FModelCaption             : TLabel;
    FModelLabel               : TLabel;
    FStudyCaption             : TLabel;
    FStudyLabel               : TLabel;
    FStudyNumber              : TLabel;
    FStudyNumberCaption       : TLabel;
    FDate                     : TLabel;
    FDateCaption              : TLabel;
    FClient                   : TLabel;
    FClientCaption            : TLabel;
    FConsultant               : TLabel;
    FConsultantCaption        : TLabel;
    FStudyDescription         : TLabel;
    FStudyDescriptionCaption  : TLabel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;

  public
    procedure PopulateStudy(AStudyDataObject: TStudyDataObject);
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
 end;

  TModelPanel = class(TAbstractPanel)
  protected
    FModelCaption            :TLabel;
    FModelLabel              :TLabel;
    FModelDescription        : TLabel;
    FModelDescriptionCaption : TLabel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure PopulateModel(AModelDataObject: TModelDataObject);
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
 end;

  TSubAreaPanel       = class(TAbstractPanel)
  protected
    FModelCaption        : TLabel;
    FModelLabel          : TLabel;
    FStudyCaption        : TLabel;
    FStudyLabel          : TLabel;
    FSubAreaCaption      : TLabel;
    FSubAreaLabel        : TLabel;
    FSubAreaDescrCaption : TLabel;
    FSubAreaDescr        : TLabel;

    FCoordsGroupBox        : TGroupBox;
    FCoordUnitsLabel       : TLabel;
    FTopLeftCoordLabel     : TLabel;
    FTopRightCoordLabel    : TLabel;
    FBottomLeftCoordLabel  : TLabel;
    FBottomRightCoordLabel : TLabel;
    FTopLeftCoordEdit      : TEdit;
    FTopRightCoordEdit     : TEdit;
    FBottomLeftCoordEdit   : TEdit;
    FBottomRightCoordEdit  : TEdit;
    FBtnUpdateFromGIS      : TButton;
    FBtnUpdateDB           : TButton;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure PopulateSubArea(ASubAreaDataObject: TSubAreaDataObject);
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    property TopLeftCoordEdit      : TEdit   read FTopLeftCoordEdit    ;
    property TopRightCoordEdit     : TEdit   read FTopRightCoordEdit   ;
    property BottomLeftCoordEdit   : TEdit   read FBottomLeftCoordEdit ;
    property BottomRightCoordEdit  : TEdit   read FBottomRightCoordEdit;
    property BtnUpdateFromGIS      : TButton read FBtnUpdateFromGIS    ;
    property BtnUpdateDB           : TButton read FBtnUpdateDB         ;
  end;

  TScenarioPanel       = class(TAbstractPanel)
  protected
    FModelCaption         :TLabel;
    FModelLabel           :TLabel;
    FStudyCaption         :TLabel;
    FStudyLabel           :TLabel;
    FSubAreaCaption       : TLabel;
    FSubAreaLabel         : TLabel;
    FScenarioCaption      : TLabel;
    FScenarioLabel        : TLabel;
    FScenarioDescrCaption : TLabel ;
    FScenarioDescr        : TLabel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure PopulateScenario(AScenarioDataObject: TScenarioDataObject);
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
  end;

  TStudyDetailsPanel = class(TAbstractPanel)
  private
  protected
    FStudyPanel    : TStudyPanel;
    FModelPanel    : TModelPanel;
    FSubAreaPanel  : TSubAreaPanel;
    FScenarioPanel : TScenarioPanel;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure ShowStudy(AStudyDataObject: TStudyDataObject);
    procedure ShowModel(AModelDataObject: TModelDataObject);
    procedure ShowSubArea(ASubAreaDataObject: TSubAreaDataObject);
    procedure ShowScenario(AScenarioDataObject: TScenarioDataObject);
    procedure ShowNothing;
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    property StudyPanel    : TStudyPanel    read FStudyPanel   ;
    property ModelPanel    : TModelPanel    read FModelPanel   ;
    property SubAreaPanel  : TSubAreaPanel  read FSubAreaPanel ;
    property ScenarioPanel : TScenarioPanel read FScenarioPanel;
  end;

implementation

uses
  math,
  SysUtils,
  Vcl.Controls,
  UErrorHandlingOperations;

procedure TStudyDetailsPanel.CreateMemberObjects;
const OPNAME = 'TStudyDetailsPanel.CreateMemberObjects';
begin
  try
    FStudyPanel := TStudyPanel.Create(self,FAppModules);
    FStudyPanel.Parent := Self;
    FStudyPanel.Align := alClient;
    FStudyPanel.Visible := False;

    FModelPanel := TModelPanel.Create(self,FAppModules);
    FModelPanel.Parent := Self;
    FModelPanel.Align := alClient;
    FModelPanel.Visible := False;

    FSubAreaPanel := TSubAreaPanel.Create(self,FAppModules);
    FSubAreaPanel.Parent := Self;
    FSubAreaPanel.Align := alClient;
    FSubAreaPanel.Visible := False;

    FScenarioPanel := TScenarioPanel.Create(self,FAppModules);
    FScenarioPanel.Parent := Self;
    FScenarioPanel.Align := alClient;
    FScenarioPanel.Visible := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.AssignHelpContext;
const OPNAME = 'TStudyDetailsPanel.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyDetailsPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyDetailsPanel.LanguageHasChanged: boolean;
const OPNAME = 'TStudyDetailsPanel.LanguageHasChanged';
begin
  Result := False;
  try
    FStudyPanel.LanguageHasChanged;
    FModelPanel.LanguageHasChanged;
    FSubAreaPanel.LanguageHasChanged;
    FScenarioPanel.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.ShowModel(AModelDataObject: TModelDataObject);
const OPNAME = 'TStudyDetailsPanel.ShowModel';
begin
  try
    FModelPanel.PopulateModel(AModelDataObject);
    FModelPanel.Visible := True;
    FModelPanel.BringToFront;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.ShowNothing;
const OPNAME = 'TStudyDetailsPanel.ShowNothing';
begin
  try
    FStudyPanel.Visible := False;
    FModelPanel.Visible := False;
    FSubAreaPanel.Visible := False;
    FScenarioPanel.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.ShowScenario(AScenarioDataObject: TScenarioDataObject);
const OPNAME = 'TStudyDetailsPanel.ShowScenario';
begin
  try
    FScenarioPanel.PopulateScenario(AScenarioDataObject);
    FScenarioPanel.Visible := True;
    FScenarioPanel.BringToFront;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.ShowStudy(AStudyDataObject: TStudyDataObject);
const OPNAME = 'TStudyDetailsPanel.ShowStudy';
begin
  try
    FStudyPanel.PopulateStudy(AStudyDataObject);
    FStudyPanel.Visible := True;
    FStudyPanel.BringToFront;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.ShowSubArea(ASubAreaDataObject: TSubAreaDataObject);
const OPNAME = 'TStudyDetailsPanel.ShowSubArea';
begin
  try
   // FSubAreaPanel.PopulateSubArea(ASubAreaDataObject);
   // FSubAreaPanel.Visible := True;
   // FSubAreaPanel.BringToFront;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyDetailsPanel.Resize;
const OPNAME = 'TStudyDetailsPanel.Resize';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TStudyPanel }

procedure TStudyPanel.AssignHelpContext;
const OPNAME = 'TStudyPanel.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.CreateMemberObjects;
const OPNAME = 'TStudyPanel.CreateMemberObjects';
begin
  try
    Height                                     := 100;

    FStudyCaption                              := TLabel.Create(self);
    FStudyCaption.Parent                       := self;
    FStudyCaption.Left                         := 20;
    FStudyCaption.Top                          := 20;
    FStudyCaption.Font.Style                   := [fsBold];

    FStudyLabel                                :=TLabel.Create(self);
    FStudyLabel.Parent                         := self;
    FStudyLabel.Left                           := 100;
    FStudyLabel.Top                            := 20;

    FStudyNumberCaption                        := TLabel.Create(self);
    FStudyNumberCaption.Parent                 := self;
    FStudyNumberCaption.Left                   := 20;
    FStudyNumberCaption.Top                    := 35;
    FStudyNumberCaption.Font.Style             := [fsBold];

    FStudyNumber                               := TLabel.Create(self);
    FStudyNumber.Parent                        := self;
    FStudyNumber.Left                          := 100;
    FStudyNumber.Top                           := 35;

    FModelCaption                              := TLabel.Create(self);
    FModelCaption.Parent                       := self;
    FModelCaption.Left                         := 20;
    FModelCaption.Top                          := 50;
    FModelCaption.Font.Style                   := [fsBold];

    FModelLabel                                := TLabel.Create(self);
    FModelLabel.Parent                         := self;
    FModelLabel.Left                           := 100;
    FModelLabel.Top                            := 50;

    FDateCaption                               := TLabel.Create(self);
    FDateCaption.Parent                        := self;
    FDateCaption.Left                          := 20;
    FDateCaption.Top                           := 65;
    FDateCaption.Font.Style                    := [fsBold];

    FDate                                      := TLabel.Create(self);
    FDate.Parent                               := self;
    FDate.Left                                 := 100;
    FDate.Top                                  := 65;

    FClientCaption                             := TLabel.Create(self);
    FClientCaption.Parent                      := self;
    FClientCaption.Left                        := 20;
    FClientCaption.Top                         := 80;
    FClientCaption.Font.Style                  := [fsBold];

    FClient                                    := TLabel.Create(self);
    FClient.Parent                             := self;
    FClient.Left                               := 100;
    FClient.Top                                := 80;

    FConsultantCaption                         := TLabel.Create(self);
    FConsultantCaption.Parent                  := self;
    FConsultantCaption.Left                    := 20;
    FConsultantCaption.Top                     := 95;
    FConsultantCaption.Font.Style              := [fsBold];

    FConsultant                                := TLabel.Create(self);
    FConsultant.Parent                         := self;
    FConsultant.Left                           := 100;
    FConsultant.Top                            := 95;

    FStudyDescriptionCaption                   := TLabel.Create(self);
    FStudyDescriptionCaption.Parent            := self;
    FStudyDescriptionCaption.Left              := 20;
    FStudyDescriptionCaption.Top               := 110;
    FStudyDescriptionCaption.Font.Style        := [fsBold];

    FStudyDescription                          := TLabel.Create(self);
    FStudyDescription.Parent                   := self;
    FStudyDescription.Left                     := 100;
    FStudyDescription.Top                      := 110;
    FStudyDescription.AutoSize                 := False;
    FStudyDescription.WordWrap                 := True;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TStudyPanel.LanguageHasChanged: boolean;
const OPNAME = 'TStudyPanel.LanguageHasChanged';
begin
  Result := False;
  try
   FStudyCaption.Caption               := FAppModules.Language.GetString('StudySelection.StudyCaption');
   FStudyNumberCaption.Caption         := FAppModules.Language.GetString('StudySelection.StudyNoCaption');
   FModelCaption.Caption               := FAppModules.Language.GetString('StudySelection.ModelCaption');
   FDateCaption.Caption                := FAppModules.Language.GetString('StudySelection.DateCaption');
   FClientCaption.Caption              := FAppModules.Language.GetString('StudySelection.ClientCaption');
   FConsultantCaption.Caption          := FAppModules.Language.GetString('StudySelection.ConsultantCaption');
   FStudyDescriptionCaption.Caption    := FAppModules.Language.GetString('StudySelection.DescriptionCaption');
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyPanel.PopulateStudy(AStudyDataObject: TStudyDataObject);
const OPNAME = 'TStudyPanel.PopulateStudy';
begin
  try
    FStudyLabel.Caption                 := ': ' + AStudyDataObject.StudyLabel;
    FStudyNumber.Caption                := ': ' + AStudyDataObject.StudyNumber;
//    FModelLabel.Caption                 := ': ' + AStudyDataObject.ModelLabel;
    FDate.Caption                       := ': ' + DateToStr(AStudyDataObject.StudyDate);
    FClient.Caption                     := ': ' + AStudyDataObject.Client;
    FConsultant.Caption                 := ': ' + AStudyDataObject.Consultant;
    FStudyDescription.Caption           := ': ' + AStudyDataObject.StudyDescr;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TStudyPanel.Resize;
const OPNAME = 'TStudyPanel.Resize';
begin
  inherited;
  try
    FStudyDescription.Width :=  self.Width - FStudyDescription.Left - 2;
    FStudyDescription.Height := max(10,self.Height - FStudyDescription.Top - 2);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TModelPanel }

procedure TModelPanel.AssignHelpContext;
const OPNAME = 'TModelPanel.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyDetailsModelPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelPanel.CreateMemberObjects;
const OPNAME = 'TModelPanel.CreateMemberObjects';
begin
  try
    Height                              := 100;
    FModelCaption                       := TLabel.Create(self);
    FModelCaption.Parent                := self;
    FModelCaption.Left                  := 20;
    FModelCaption.Top                   := 20;
    FModelCaption.Font.Style            := [fsBold];

    FModelLabel                         := TLabel.Create(self);
    FModelLabel.Parent                  := self;
    FModelLabel.Left                    := 100;
    FModelLabel.Top                     := 20;

    FModelDescriptionCaption            := TLabel.Create(self);
    FModelDescriptionCaption.Parent     := self;
    FModelDescriptionCaption.Left       :=  20;
    FModelDescriptionCaption.Top        := 35;
    FModelDescriptionCaption.Font.Style := [fsBold];

    FModelDescription                   := TLabel.Create(self);
    FModelDescription.Parent            := self;
    FModelDescription.Left              :=  100;
    FModelDescription.Top               := 35;
    FModelDescription.AutoSize           := False;
    FModelDescription.WordWrap          := True;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TModelPanel.LanguageHasChanged: boolean;
const OPNAME = 'TModelPanel.LanguageHasChanged';
begin
  Result := False;
  try
    FModelCaption.Caption            := FAppModules.Language.GetString('StudySelection.ModelCaption');
    FModelDescriptionCaption.Caption := FAppModules.Language.GetString('StudySelection.DescriptionCaption');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelPanel.PopulateModel(AModelDataObject: TModelDataObject);
const OPNAME = 'TModelPanel.PopulateModel';
begin
  try
    FModelLabel.Caption       := ': ' + AModelDataObject.ModelLabel;
    FModelDescription.Caption := ': ' + AModelDataObject.ModelDescr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TModelPanel.Resize;
const OPNAME = 'TModelPanel.Resize';
begin
  inherited;
  try
    FModelDescription.Width :=  self.Width - FModelDescription.Left - 2;
    FModelDescription.Height := max(10,self.Height - FModelDescription.Top - 2);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TSubAreaPanel }

procedure TSubAreaPanel.AssignHelpContext;
const OPNAME = 'TSubAreaPanel.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyDetailsSubAreaPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubAreaPanel.CreateMemberObjects;
const OPNAME = 'TSubAreaPanel.CreateMemberObjects';
begin
  try
    Height                          := 100;

    FStudyCaption                   :=TLabel.Create(self);
    FStudyCaption.Parent            := self;
    FStudyCaption.Left              := 20;
    FStudyCaption.Top               := 20;
    FStudyCaption.Font.Style        := [fsBold];

    FStudyLabel                     :=TLabel.Create(self);
    FStudyLabel.Parent              := self;
    FStudyLabel.Left                := 100;
    FStudyLabel.Top                 := 20;

    FModelCaption                   := TLabel.Create(self);
    FModelCaption.Parent            := self;
    FModelCaption.Left              := 20;
    FModelCaption.Top               := 35;
    FModelCaption.Font.Style        := [fsBold];

    FModelLabel                     := TLabel.Create(self);
    FModelLabel.Parent              := self;
    FModelLabel.Left                := 100;
    FModelLabel.Top                 := 35;

    FSubAreaCaption                 := TLabel.Create(self);
    FSubAreaCaption.Parent          := self;
    FSubAreaCaption.Left            := 20;
    FSubAreaCaption.Top             := 50;
    FSubAreaCaption.Font.Style      := [fsBold];

    FSubAreaLabel                   := TLabel.Create(self);
    FSubAreaLabel.Parent            := self;
    FSubAreaLabel.Left              := 100;
    FSubAreaLabel.Top               := 50;

    FSubAreaDescrCaption            := TLabel.Create(self);
    FSubAreaDescrCaption.Parent     := self;
    FSubAreaDescrCaption.Left       := 20;
    FSubAreaDescrCaption.Top        := 65;
    FSubAreaDescrCaption.Font.Style := [fsBold];

    FSubAreaDescr                   := TLabel.Create(self);
    FSubAreaDescr.Parent            := self;
    FSubAreaDescr.Left              := 100;
    FSubAreaDescr.Top               := 65;
    FSubAreaDescr.AutoSize          := False;
    FSubAreaDescr.WordWrap          := True;
//______________________________________________________________________________
    FCoordsGroupBox                 := TGroupBox.Create(self);
    FCoordsGroupBox.Parent          := self;
    FCoordsGroupBox.Height          := 75;
    FCoordsGroupBox.Align           := alBottom;

    FCoordUnitsLabel              := TLabel.Create(self);
    FCoordUnitsLabel.Parent       := FCoordsGroupBox;
    FCoordUnitsLabel.Left         := 430;
    FCoordUnitsLabel.Top          := 20;
    FCoordUnitsLabel.Width        := 60;
    FCoordUnitsLabel.Alignment    := taLeftJustify;

    FTopLeftCoordLabel              := TLabel.Create(self);
    FTopLeftCoordLabel.Parent       := FCoordsGroupBox;
    FTopLeftCoordLabel.Left         := 0;
    FTopLeftCoordLabel.Top          := 20;
    FTopLeftCoordLabel.Width        := 60;
    FTopLeftCoordLabel.Alignment    := taRightJustify;

    FTopRightCoordLabel             := TLabel.Create(self);
    FTopRightCoordLabel.Parent      := FCoordsGroupBox;
    FTopRightCoordLabel.Left        := 240;
    FTopRightCoordLabel.Top         := FTopLeftCoordLabel.Top;
    FTopRightCoordLabel.Width       := FTopLeftCoordLabel.Width;
    FTopRightCoordLabel.Alignment   := taRightJustify;

    FBottomLeftCoordLabel           := TLabel.Create(self);
    FBottomLeftCoordLabel.Parent    := FCoordsGroupBox;
    FBottomLeftCoordLabel.Left      := FTopLeftCoordLabel.Left;
    FBottomLeftCoordLabel.Top       := 48;
    FBottomLeftCoordLabel.Width     := FTopLeftCoordLabel.Width;
    FBottomLeftCoordLabel.Alignment := taRightJustify;

    FBottomRightCoordLabel          := TLabel.Create(self);
    FBottomRightCoordLabel.Parent   := FCoordsGroupBox;
    FBottomRightCoordLabel.Left     := FTopRightCoordLabel.Left;
    FBottomRightCoordLabel.Top      := FBottomLeftCoordLabel.Top;
    FBottomRightCoordLabel.Width    := FTopLeftCoordLabel.Width;
    FBottomRightCoordLabel.Alignment:= taRightJustify;

    FTopLeftCoordEdit               := TEdit.Create(self);
    FTopLeftCoordEdit.Parent        := FCoordsGroupBox;
    FTopLeftCoordEdit.Left          := 65;
    FTopLeftCoordEdit.Top           := 16;

    FTopRightCoordEdit              := TEdit.Create(self);
    FTopRightCoordEdit.Parent       := FCoordsGroupBox;
    FTopRightCoordEdit.Left         := 300;
    FTopRightCoordEdit.Top          := FTopLeftCoordEdit.Top;

    FBottomLeftCoordEdit            := TEdit.Create(self);
    FBottomLeftCoordEdit.Parent     := FCoordsGroupBox;
    FBottomLeftCoordEdit.Left       := FTopLeftCoordEdit.Left;
    FBottomLeftCoordEdit.Top        := 45;

    FBottomRightCoordEdit           := TEdit.Create(self);
    FBottomRightCoordEdit.Parent    := FCoordsGroupBox;
    FBottomRightCoordEdit.Top       := FBottomLeftCoordEdit.Top;
    FBottomRightCoordEdit.Left      := FTopRightCoordEdit.Left;

    FBtnUpdateFromGIS               := TButton.Create(self);
    FBtnUpdateFromGIS.Parent        := FCoordsGroupBox;
    FBtnUpdateFromGIS.Top           := FTopRightCoordEdit.Top;
    FBtnUpdateFromGIS.Height        := 21;
    FBtnUpdateFromGIS.Width         := 100;

    FBtnUpdateDB                    := TButton.Create(self);
    FBtnUpdateDB.Parent             := FCoordsGroupBox;
    FBtnUpdateDB.Top                := FBottomLeftCoordEdit.Top;
    FBtnUpdateDB.Height             := FBtnUpdateFromGIS.Height;
    FBtnUpdateDB.Width              := FBtnUpdateFromGIS.Width;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSubAreaPanel.LanguageHasChanged: boolean;
const OPNAME = 'TSubAreaPanel.LanguageHasChanged';
begin
  Result := False;
  try
    FStudyCaption.Caption            := FAppModules.Language.GetString('StudySelection.StudyCaption');
    FModelCaption.Caption            := FAppModules.Language.GetString('StudySelection.ModelCaption');
    FSubAreaCaption.Caption          := FAppModules.Language.GetString('StudySelection.SubAreaCaption');
    FSubAreaDescrCaption.Caption     := FAppModules.Language.GetString('StudySelection.DescriptionCaption');

    FCoordUnitsLabel.Caption         := FAppModules.Language.GetString('StudySelection.DecDegCoordUnits');
    FTopLeftCoordLabel.Caption      := FAppModules.Language.GetString('StudySelection.TopLeftCoordCaption');
    FTopRightCoordLabel.Caption     := FAppModules.Language.GetString('StudySelection.TopRightCoordCaption');
    FBottomLeftCoordLabel.Caption   := FAppModules.Language.GetString('StudySelection.BottomLeftCaption');
    FBottomRightCoordLabel.Caption  := FAppModules.Language.GetString('StudySelection.BottomRightCaption');
    FBtnUpdateFromGIS.Caption       := FAppModules.Language.GetString('StudySelection.BtnUpdateFromGISCaption');
    FBtnUpdateDB.Caption            := FAppModules.Language.GetString('StudySelection.BtnUpdateDBCaption');

    Result                           := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubAreaPanel.PopulateSubArea(ASubAreaDataObject: TSubAreaDataObject);
const OPNAME = 'TSubAreaPanel.PopulateSubArea';
begin
  try
    FStudyLabel.Caption              := ': ' + ASubAreaDataObject.Model.Study.StudyLabel;
    FModelLabel.Caption              := ': ' + ASubAreaDataObject.Model.ModelLabel;
    FSubAreaLabel.Caption            := ': ' + ASubAreaDataObject.SubAreaLabel;
    FSubAreaDescr.Caption            := ': ' + ASubAreaDataObject.SubAreaDescr;
    FTopLeftCoordEdit.Text           := FormatFloat('##0.00000',ASubAreaDataObject.TopLeftCoord);     //jkw - same as gisviewer component
    FTopRightCoordEdit.Text          := FormatFloat('##0.00000',ASubAreaDataObject.TopRightCoord);    //jkw - same as gisviewer component
    FBottomLeftCoordEdit.Text        := FormatFloat('##0.00000',ASubAreaDataObject.BottomLeftCoord);  //jkw - same as gisviewer component
    FBottomRightCoordEdit.Text       := FormatFloat('##0.00000',ASubAreaDataObject.BottomRightCoord); //jkw - same as gisviewer component
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSubAreaPanel.Resize;
const OPNAME = 'TSubAreaPanel.Resize';
begin
  inherited;
  try
    FSubAreaDescr.Width :=  self.Width - FSubAreaDescr.Left - 2;
    FSubAreaDescr.Height := max(10,self.Height - FSubAreaDescr.Top - 2);
    FBtnUpdateFromGIS.Left := FCoordsGroupBox.ClientWidth - FBtnUpdateFromGIS.Width - 5;
    FBtnUpdateDB.Left      := FBtnUpdateFromGIS.Left;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TScenarioPanel }

procedure TScenarioPanel.AssignHelpContext;
const OPNAME = 'TScenarioPanel.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyDetailsScenarioPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TScenarioPanel.CreateMemberObjects;
const OPNAME = 'TScenarioPanel.CreateMemberObjects';
begin
  try
    Height                            := 100;

    FModelCaption                     := TLabel.Create(self);
    FModelCaption.Parent              := self;
    FModelCaption.Left                := 20;
    FModelCaption.Top                 := 20;
    FModelCaption.Font.Style          := [fsBold];

    FModelLabel                       := TLabel.Create(self);
    FModelLabel.Parent                := self;
    FModelLabel.Left                  := 100;
    FModelLabel.Top                   := 20;

    FStudyCaption                     :=TLabel.Create(self);
    FStudyCaption.Parent              := self;
    FStudyCaption.Left                := 20;
    FStudyCaption.Top                 := 35;
    FStudyCaption.Font.Style          := [fsBold];

    FStudyLabel                       :=TLabel.Create(self);
    FStudyLabel.Parent                := self;
    FStudyLabel.Left                  := 100;
    FStudyLabel.Top                   := 35;

    FSubAreaCaption                   := TLabel.Create(self);
    FSubAreaCaption.Parent            := self;
    FSubAreaCaption.Left              := 20;
    FSubAreaCaption.Top               := 50;
    FSubAreaCaption.Font.Style        := [fsBold];

    FSubAreaLabel                     := TLabel.Create(self);
    FSubAreaLabel.Parent              := self;
    FSubAreaLabel.Left                := 100;
    FSubAreaLabel.Top                 := 50;

    FScenarioCaption                  := TLabel.Create(self);
    FScenarioCaption.Parent           := self;
    FScenarioCaption.Left             := 20;
    FScenarioCaption.Top              := 65;
    FScenarioCaption.Font.Style       := [fsBold];

    FScenarioLabel                    := TLabel.Create(self);
    FScenarioLabel.Parent             := self;
    FScenarioLabel.Left               := 100;
    FScenarioLabel.Top                := 65;

    FScenarioDescrCaption             := TLabel.Create(self);
    FScenarioDescrCaption.Parent      := self;
    FScenarioDescrCaption.Left        := 20;
    FScenarioDescrCaption.Top         := 80;
    FScenarioDescrCaption.Font.Style  := [fsBold];

    FScenarioDescr                    := TLabel.Create(self);
    FScenarioDescr.Parent             := self;
    FScenarioDescr.Left               := 100;
    FScenarioDescr.Top                := 80;
    FScenarioDescr.AutoSize           := False;
    FScenarioDescr.WordWrap           := True;

  except on E: Exception do HandleError(E, OPNAME) end;

end;

function TScenarioPanel.LanguageHasChanged: boolean;
const OPNAME = 'TScenarioPanel.LanguageHasChanged';
begin
  Result := False;
  try
    FStudyCaption.Caption            := FAppModules.Language.GetString('StudySelection.StudyCaption');
    FModelCaption.Caption            := FAppModules.Language.GetString('StudySelection.ModelCaption');
    FSubAreaCaption.Caption          := FAppModules.Language.GetString('StudySelection.SubAreaCaption');
    FScenarioCaption.Caption         := FAppModules.Language.GetString('StudySelection.ScenarioCaption');
    FScenarioDescrCaption.Caption    := FAppModules.Language.GetString('StudySelection.DescriptionCaption');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TScenarioPanel.PopulateScenario(AScenarioDataObject: TScenarioDataObject);
const OPNAME = 'TScenarioPanel.PopulateScenario';
begin
  try

    if ( AScenarioDataObject.SubArea.Model.ModelLabel = FAppModules.Language.GetString('Model.Rainfall') ) then
    begin
      FScenarioCaption.Caption := FAppModules.Language.GetString('StudySelection.ProjectGauge');//'Rainfall Zone';
//      FSubAreaCaption.Caption   := 'Project Gauge';
    end
    else
    begin
      FScenarioCaption.Caption := FAppModules.Language.GetString('StudySelection.ScenarioCaption');
      FSubAreaCaption.Caption   := FAppModules.Language.GetString('StudySelection.SubAreaCaption');
    end;

    FStudyLabel.Caption              := ': ' + AScenarioDataObject.SubArea.Model.Study.StudyLabel;
    FModelLabel.Caption              := ': ' + AScenarioDataObject.SubArea.Model.ModelLabel;
    FSubAreaLabel.Caption            := ': ' + AScenarioDataObject.SubArea.SubAreaLabel;
    FScenarioLabel.Caption           := ': ' + AScenarioDataObject.ScenarioLabel;
    FScenarioDescr.Caption           := ': ' + AScenarioDataObject.ScenarioDescr;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TScenarioPanel.Resize;
const OPNAME = 'TScenarioPanel.Resize';
begin
  inherited;
  try
    FScenarioDescr.Width :=  self.Width - FScenarioDescr.Left - 2;
    FScenarioDescr.Height := max(10,self.Height - FScenarioDescr.Top - 2);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
