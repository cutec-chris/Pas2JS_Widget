{
  MIT License

  Copyright (c) 2018 Hélio S. Ribeiro and Anderson J. Gado da Silva

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit Pas2JS_IDE_Descriptor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  LazIDEIntf,
  ProjectIntf,
  CompOptsIntf,
  FormEditingIntf,
  PropEdits;

type

  { TPas2JSProject }

  TPas2JSProject = class(TProjectDescriptor)
  private
    FPas2JSBuilder: string;
    FPas2JSJSON: string;
    function GetOptions: TModalResult;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

  { TPas2JSWForm }

  TPas2JSWForm = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetUnitDirectives: string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TPas2JSWFrame }

  TPas2JSWFrame = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetUnitDirectives: string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TPas2JSWDataModule }

  TPas2JSWDataModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetUnitDirectives: string; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName, ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

procedure Register;

var
  VPas2JSProject: TPas2JSProject;
  VPas2JSWForm: TPas2JSWForm;    
  VPas2JSWFrame: TPas2JSWFrame;   
  VPas2JSWDataModule: TPas2JSWDataModule;

implementation

uses
  TypInfo,
  FileUtil,
  WebCtrls,
  FPJSON,
  Pas2JS_IDE_Options;

{ TPas2JSProject }

function TPas2JSProject.GetOptions: TModalResult;

  function ExtractConfig(AOptionsForm: TOptionsForm): string;
  var
    VJSONObject: TJSONObject;
    VJSONArray: TJSONArray;
    VIndex: NativeInt;
  begin
    VJSONObject := TJSONObject.Create([]);
    try
      VJSONObject.Add('Compiler', AOptionsForm.CompilerEdit.Text);
      VJSONObject.Add('Output', AOptionsForm.OutputEdit.Text);
      VJSONObject.Add('Template', AOptionsForm.TemplateEdit.Text);
      VJSONObject.Add('Browser', AOptionsForm.BrowserEdit.Text);
      VJSONArray := TJSONArray.Create([]);
      VJSONArray.Add('-Jirtl.js');
      VJSONArray.Add('-Tbrowser');
      VJSONArray.Add('-MDelphi');
      VJSONArray.Add('-Jc');
      for VIndex := 0 to (AOptionsForm.CustomOptionsMemo.Lines.Count - 1) do
      begin
        VJSONArray.Add(AOptionsForm.CustomOptionsMemo.Lines[VIndex]);
      end;
      VJSONObject.Add('CustomOptions', VJSONArray);
      Result := VJSONObject.FormatJSON();
    finally
      FreeAndNil(VJSONObject);
    end;
  end;

var
  VOptionsForm: TOptionsForm;
begin
  VOptionsForm := TOptionsForm.Create(Application);
  try
    Result := VOptionsForm.ShowModal;
    if (Result = mrOk) then
    begin
      FPas2JSBuilder := VOptionsForm.BuilderEdit.Text;
      FPas2JSJSON := ExtractConfig(VOptionsForm);
    end;
  finally
    FreeAndNil(VOptionsForm);
  end;
end;

constructor TPas2JSProject.Create;
begin
  inherited Create;
  FPas2JSBuilder := '';
  FPas2JSJSON := '';
  Name := 'Application(Pas2JS)';
end;

function TPas2JSProject.GetLocalizedName: string;
begin
  Result := 'Application(Pas2JS)';
end;

function TPas2JSProject.GetLocalizedDescription: string;
begin
  Result := 'Create a Pas2JS application';
end;

function TPas2JSProject.InitProject(AProject: TLazProject): TModalResult;

  function Project: TLazProjectFile;

    function Source: string;
    const
      LE = LineEnding;
    begin
      Result :=
        'program Project1; ' + LE +
        '' + LE +
        '{$mode delphi}{$H+}' + LE +
        '' + LE +
        'uses' + LE +
        '  Interfaces, Forms;' + LE +
        '' + LE +
        'begin' + LE +
        '  Application.Initialize;' + LE +
        '  Application.Run;' + LE +
        'end.' + LE + LE + LE +
        /// Pas2JS Config
        '{@PAS2JS_BEGIN}' + LE +
        FPas2JSJSON + LE +
        '{@PAS2JS_END}';
    end;

  begin
    Result := AProject.CreateProjectFile('project1.lpr');
    Result.IsPartOfProject := True;
    Result.SetSourceText(Source, True);
  end;

begin
  Result := inherited InitProject(AProject);
  Result := GetOptions;
  if (Result <> mrOk) then
  begin
    Exit;
  end;
  AProject.AddFile(Project, False);
  AProject.AddPackageDependency('pas2js_rtl');
  AProject.AddPackageDependency('pas2js_widget');
  AProject.Flags := AProject.Flags - [pfRunnable];
  AProject.LoadDefaultIcon;
  AProject.MainFileID := 0;
  AProject.LazCompilerOptions.SetAlternativeCompile(FPas2JSBuilder + ' ' + '$(ProjFile)', False);
  AProject.LazCompilerOptions.SyntaxMode := 'delphi';
  AProject.LazCompilerOptions.TargetFilename := 'project1';
  AProject.LazCompilerOptions.Win32GraphicApp := True;
end;

function TPas2JSProject.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(VPas2JSWForm, '', '', [nfIsPartOfProject, nfOpenInEditor, nfCreateDefaultSrc]);
  Result := mrOk;
end;

{ TPas2JSWForm }

constructor TPas2JSWForm.Create;
begin
  inherited Create;
  Name := 'WForm';
  ResourceClass := TWForm;
  UseCreateFormStatements := True;
end;

function TPas2JSWForm.GetUnitDirectives: string;
begin
  Result := '{$mode delphi}{$H+}';
end;

function TPas2JSWForm.GetInterfaceUsesSection: string;
begin
  Result := 'JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls';
end;

function TPas2JSWForm.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result :=
    'type' + LE +
    '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LE +
    '  private' + LE + LE +
    '  public' + LE +
    '    procedure Loaded; override;' + LE +
    '  end;' + LE + LE;

  if (DeclareClassVariable) then
  begin
    Result := Result +
      'var' + LE +
      '  ' + ResourceName + ': T' + ResourceName + ';' + LE + LE;
  end;
end;

function TPas2JSWForm.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  Result :=
    'procedure T' + ResourceName + '.Loaded;' + LE +
    'begin' + LE +
    '  inherited Loaded;' + LE +
    '  {$I ' + ExtractFileNameWithoutExt(Filename) + '.wfm}' + LE +
    'end;' + LE + LE;
end;

function TPas2JSWForm.GetLocalizedName: string;
begin
  Result := 'Web Form(Pas2JS)';
end;

function TPas2JSWForm.GetLocalizedDescription: string;
begin
  Result := 'Create a Pas2JS Web Form';
end;

{ TPas2JSWFrame }

constructor TPas2JSWFrame.Create;
begin
  inherited Create;
  Name := 'WFrame';
  ResourceClass := TWFrame;
  UseCreateFormStatements := False;
end;

function TPas2JSWFrame.GetUnitDirectives: string;
begin                                                                           
  Result := '{$mode delphi}{$H+}';
end;

function TPas2JSWFrame.GetInterfaceUsesSection: string;
begin
  Result := 'JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls';
end;
                       
function TPas2JSWFrame.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result :=
    'type' + LE +
    '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LE +
    '  private' + LE + LE +
    '  public' + LE +
    '    procedure Loaded; override;' + LE +
    '  end;' + LE + LE;
end;

function TPas2JSWFrame.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  Result :=
    'procedure T' + ResourceName + '.Loaded;' + LE +
    'begin' + LE +
    '  inherited Loaded;' + LE +
    '  {$I ' + ExtractFileNameWithoutExt(Filename) + '.wfm}' + LE +
    'end;' + LE + LE;
end;

function TPas2JSWFrame.GetLocalizedName: string;
begin
  Result := 'Web Frame(Pas2JS)';
end;

function TPas2JSWFrame.GetLocalizedDescription: string;
begin
  Result := 'Create a Pas2JS Web Fram';
end;

{ TPas2JSWDataModule }

constructor TPas2JSWDataModule.Create;
begin
  inherited Create;
  Name := 'WDataModule';
  ResourceClass := TWDataModule;
  UseCreateFormStatements := True;
end;

function TPas2JSWDataModule.GetUnitDirectives: string;
begin
  Result := '{$mode delphi}{$H+}';
end;

function TPas2JSWDataModule.GetInterfaceUsesSection: string;
begin
  Result := 'JS, Classes, SysUtils, Graphics, Controls, Forms, Dialogs, WebCtrls';
end;

function TPas2JSWDataModule.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result :=
    'type' + LE +
    '  T' + ResourceName + ' = class(' + ResourceClass.ClassName + ')' + LE +
    '  private' + LE + LE +
    '  public' + LE +
    '    procedure Loaded; override;' + LE +
    '  end;' + LE + LE;

  if (DeclareClassVariable) then
  begin
    Result := Result +
      'var' + LE +
      '  ' + ResourceName + ': T' + ResourceName + ';' + LE + LE;
  end;
end;

function TPas2JSWDataModule.GetImplementationSource(const Filename, SourceName, ResourceName: string): string;
const
  LE = LineEnding;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  Result :=
    'procedure T' + ResourceName + '.Loaded;' + LE +
    'begin' + LE +
    '  inherited Loaded;' + LE +
    '  {$I ' + ExtractFileNameWithoutExt(Filename) + '.wfm}' + LE +
    'end;' + LE + LE;
end;

function TPas2JSWDataModule.GetLocalizedName: string;
begin
  Result := 'Web Data Module(Pas2JS)';
end;

function TPas2JSWDataModule.GetLocalizedDescription: string;
begin
  Result := 'Create a Pas2JS Web Data Module';
end;

type

  { TIDEExtensionHandler }

  TIDEExtensionHandler = class
  protected
    function DoSaveEditorFile(Sender: TObject; {%H-}AFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; {%H-}TargetFilename: string): TModalResult;
  end;

{ TIDEExtensionHandler }

function TIDEExtensionHandler.DoSaveEditorFile(Sender: TObject; AFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;

  function FindCurrentModule: TComponent;
  var
    VIndex: longint;
    VComponent: TComponent;
  begin
    Result := nil;
    for VIndex := 0 to (FormEditingHook.DesignerCount - 1) do
    begin
      VComponent := FormEditingHook.Designer[VIndex].LookupRoot;
      if (Assigned(VComponent)) and
         (SameText(VComponent.UnitName, AFile.Unit_Name)) and
         (VComponent.InheritsFrom(TWForm)) or
         (VComponent.InheritsFrom(TWFrame)) or
         (VComponent.InheritsFrom(TWDataModule)) then
      begin
        Exit(VComponent);
      end;
    end;
  end;

var
  VCurrentModule: TComponent;
  VFile: TFilename;
  VStream: TMemoryStream;
  VData: string;
begin   
  Result := mrOk;
  if (SaveStep <> sefsAfterWrite) then
  begin
    Exit;
  end;
  VCurrentModule := FindCurrentModule;
  if (not (Assigned(VCurrentModule))) then
  begin
    Exit;
  end;
  VFile := ExtractFileNameWithoutExt(TargetFilename) + '.wfm';
  if (not FileExists(VFile)) then
  begin
    VData := '{@PAS2JS_BEGIN}{@PAS2JS_END}';
    VStream := TMemoryStream.Create;
    try
      VStream.WriteBuffer(VData[1], Length(VData));
      VStream.SaveToFile(VFile);
    finally
      FreeAndNil(VStream);
    end;
  end;
end;

procedure Register;
var
  {%H-}VIDEExtension: TIDEExtensionHandler;
begin
  VPas2JSWForm := TPas2JSWForm.Create;           
  VPas2JSWFrame := TPas2JSWFrame.Create;
  VPas2JSWDataModule := TPas2JSWDataModule.Create;

  RegisterProjectFileDescriptor(VPas2JSWForm);
  RegisterProjectFileDescriptor(VPas2JSWFrame);
  RegisterProjectFileDescriptor(VPas2JSWDataModule);

  FormEditingHook.RegisterDesignerBaseClass(TWForm);     
  FormEditingHook.RegisterDesignerBaseClass(TWFrame);
  FormEditingHook.RegisterDesignerBaseClass(TWDataModule);
  //FormEditingHook.StandardDesignerBaseClasses[3{DesignerBaseClassId_TForm}] := TWForm;
  //FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TFrame] := TWFrame;
  //FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TDataModule] := TWDataModule;

  VPas2JSProject := TPas2JSProject.Create;
  RegisterProjectDescriptor(VPas2JSProject);

  VIDEExtension := TIDEExtensionHandler.Create;
  LazarusIDE.AddHandlerOnSaveEditorFile(@VIDEExtension.DoSaveEditorFile);
end;

end.
