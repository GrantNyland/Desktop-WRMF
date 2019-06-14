//
//
//  UNIT      : Contains UListSelectionDialog utility.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/06/26
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UListSelectionDialog;

interface

uses
  Classes;

function SelectFromList(ACaption, ALabel: string; AList: TStrings; var ASelection: string): boolean;

implementation

uses
  SysUtils,
  Windows,
  vcl.Controls,
  vcl.ExtCtrls,
  vcl.StdCtrls,
  vcl.Buttons,
  vcl.Forms,
  UErrorHandlingOperations;

const
  C_LabelHeight  = 22;
  C_ButtonHeight = 28;
  C_ButtonWidth  = 70;
  C_ButtonGap    = 8;

function SelectFromList(ACaption, ALabel: string; AList: TStrings; var ASelection: string): boolean;
const OPNAME = 'UListSelectionDialog.SelectFromList';
var
  LListSelectionDialog: TForm;
  LListBox: TListBox;
  LLabelPanel, LButtonPanel: TPanel;
  LOKButton, LCancelButton: TBitBtn;
begin
  Result := False;
  try

    // Create the form.
    LListSelectionDialog := TForm.Create(nil);
    try

      // Set form properties.
      LListSelectionDialog.Cursor      := crHourGlass;
      LListSelectionDialog.BorderIcons := [biSystemMenu];
      LListSelectionDialog.BorderStyle := bsDialog;
      LListSelectionDialog.Caption     := ACaption;
      LListSelectionDialog.Position    := poScreenCenter;
      LListSelectionDialog.Height      := 200;
      LListSelectionDialog.Width       := 400;

      // Create the caption panel.
      if (ALabel <> '') then
      begin
        LLabelPanel := TPanel.Create(LListSelectionDialog);
        LLabelPanel.Parent  := LListSelectionDialog;
        LLabelPanel.Align   := alTop;
        LLabelPanel.Caption := ALabel;
        LLabelPanel.ClientHeight := C_LabelHeight;
      end;

      // Create the button panel.
      LButtonPanel := TPanel.Create(LListSelectionDialog);
      LButtonPanel.Parent  := LListSelectionDialog;
      LButtonPanel.Align   := alBottom;
      LButtonPanel.Caption := '';
      LButtonPanel.ClientHeight := C_ButtonHeight + C_ButtonGap * 2;

      // Create the OK button.
      LOKButton := TBitBtn.Create(LButtonPanel);
      LOKButton.Parent := LButtonPanel;
      LOKButton.Height := C_ButtonHeight;
      LOKButton.Width := C_ButtonWidth;
      LOKButton.Kind := bkOK;
      LOKButton.Top := C_ButtonGap;
      LOKButton.Left := (LButtonPanel.ClientWidth - C_ButtonGap) div 2 - C_ButtonWidth;

      // Create the cancel button.
      LCancelButton := TBitBtn.Create(LButtonPanel);
      LCancelButton.Parent := LButtonPanel;
      LCancelButton.Height := C_ButtonHeight;
      LCancelButton.Width := C_ButtonWidth;
      LCancelButton.Kind := bkCancel;
      LCancelButton.Top := C_ButtonGap;
      LCancelButton.Left := (LButtonPanel.ClientWidth + C_ButtonGap) div 2;

      // Create the list box.
      LListBox := TListBox.Create(LListSelectionDialog);
      LListBox.Parent := LListSelectionDialog;
      LListBox.Align  := alClient;
      LListBox.Items.AddStrings(AList);
      LListBox.ItemIndex := LListBox.Items.IndexOf(ASelection);

      // Show the splash screen.
      if (LListSelectionDialog.ShowModal = mrOK) then
      begin
        if (LListBox.ItemIndex >= 0) then
        begin
          ASelection := LListBox.Items[LListBox.ItemIndex];
          Result := True;
        end;
      end;

    // Clean up.
    finally
      LListSelectionDialog.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
