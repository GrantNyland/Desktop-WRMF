{******************************************************************************}
{*  UNIT      : Contains the classes TVNVProgressDialog.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/10/24                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UVNVProgressDialog;

interface

uses
	VCL.Forms,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  Classes,
  VCL.XPMan,
  UAbstractComponent,
  VoaimsCom_TLB,
  SysUtils,
  VCL.ComCtrls,
  VCL.ListActns;

type
  TVNVProgressDialog = class(TAbstractForm)
	private
    FScrollBox         : TScrollBox;
    FProgressBar       : TProgressBar;
    FNrOfShapesCaption : TLabel;
    FNrOfShapesValue   : TLabel;
    FOfTotalCaption    : TLabel;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
	  procedure UpdateProgress (ACount : integer);
	  procedure SetMaximum (ATotal : integer);
  end;

implementation
uses
	VCL.Dialogs,
  Windows,
  StrUtils,
  VCL.Graphics,
  UConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TVNVProgressDialog                                                         *}
{******************************************************************************}

procedure TVNVProgressDialog.CreateMemberObjects;
const OPNAME = 'TVNVProgressDialog.CreateMemberObjects';
begin
	try
    BorderIcons  := [];
    Position     := poDesktopCenter;
    ClientHeight := 100;
    ClientWidth  := 250;

    FScrollBox := TScrollBox.Create(Self);
    FScrollBox.Parent     := Self;
    FScrollBox.Left       := 0;
    FScrollBox.Top        := 0;
    FScrollBox.Align      := alClient;
    FScrollBox.BevelInner := bvNone;

    FProgressBar      := TProgressBar.Create(Self);
    FProgressBar.Parent := FScrollBox;
    FProgressBar.Left   := 10;
    FProgressBar.Top    := 10;
    FProgressBar.Width  := 250;
    FProgressBar.Height := 20;
    FProgressBar.Max    := 100;
    FProgressBar.Position := 0;
    FProgressBar.Smooth   := TRUE;

    FNrOfShapesCaption            := TLabel.Create(Self);
    FNrOfShapesCaption.Parent     := FScrollBox;
    FNrOfShapesCaption.AutoSize   := FALSE;
    FNrOfShapesCaption.Top        := 40;
    FNrOfShapesCaption.Width      := 110;
    FNrOfShapesCaption.Left       := 10;

    FNrOfShapesValue            := TLabel.Create(Self);
    FNrOfShapesValue.Parent     := FScrollBox;
    FNrOfShapesValue.Top        := 40;
    FNrOfShapesValue.Width      := 40;
    FNrOfShapesValue.Left       := 125;
    FNrOfShapesValue.Caption    := '0';

    FOfTotalCaption            := TLabel.Create(Self);
    FOfTotalCaption.Parent     := FScrollBox;
    FOfTotalCaption.AutoSize   := FALSE;
    FOfTotalCaption.Top        := 40;
    FOfTotalCaption.Width      := 80;
    FOfTotalCaption.Left       := 170;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVProgressDialog.DestroyMemberObjects;
const OPNAME = 'TVNVProgressDialog.DestroyMemberObjects';
begin
	try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVProgressDialog.LanguageHasChanged: boolean;
const OPNAME = 'TVNVProgressDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
	try
    Caption                    := FAppModules.Language.GetString('VNV.RefreshShapes');
    FNrOfShapesCaption.Caption := FAppModules.Language.GetString('VNV.NrShapesUpdated');
    FOfTotalCaption.Caption    := FAppModules.Language.GetString('VNV.OfTotal');
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVProgressDialog.UpdateProgress (ACount : integer);
const OPNAME = 'TVNVProgressDialog.UpdateProgress';
begin
	try
    FProgressBar.Position := ACount;
    FNrOfShapesValue.Caption := IntToStr(ACount);
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TVNVProgressDialog.SetMaximum (ATotal : integer);
const OPNAME = 'TVNVProgressDialog.SetMaximum';
begin
	try
    FProgressBar.Max := ATotal;
    FOfTotalCaption.Caption := ' of ' + IntToStr(ATotal);
    Height := 100;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

