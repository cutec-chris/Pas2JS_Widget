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
unit Forms;

{$I pas2js_widget.inc}

interface

uses
  Classes,
  SysUtils,
  Types,
  JS,
  Web,
  Graphics,
  Controls;

type
  TFormType = (ftModalForm, ftWindow);
  TCloseAction = (caNone, caHide, caFree);
  TCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender: TObject; var CanClose: boolean) of object;

  TModalResult = low(integer)..high(integer);
  TModalResultProc = reference to procedure (Sender: TObject; ModalResult: TModalResult);

  { TCustomFrame }

  TCustomFrame = class(TCustomControl)
  protected
    procedure Changed; override;
    function CreateHandleElement: TJSHTMLElement; override;
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  TCustomFrameClass = class of TCustomFrame;

  { TCustomForm }

  TCustomForm = class(TCustomControl)
  private          
    FActiveControl: TWinControl; 
    FAlphaBlend: boolean;
    FAlphaBlendValue: byte;
    FChildForm: TCustomForm;
    FFormType: TFormType;
    FKeyPreview: boolean;   
    FModalResult: TModalResult;
    FModalResultProc: TModalResultProc;   
    FOverlay: TObject;
    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetActiveControl(AValue: TWinControl);
    procedure SetAlphaBlend(AValue: boolean);
    procedure SetAlphaBlendValue(AValue: byte);
    procedure SetModalResult(AValue: TModalResult);
  protected
    property Overlay: TObject read FOverlay write FOverlay;
    property ChildForm: TCustomForm read FChildForm write FChildForm;
  protected
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure DoClose(var CloseAction: TCloseAction); virtual;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoHide; virtual;
    procedure DoResize; override;
    procedure DoShow; virtual;
  protected
    function HandleEnter(AEvent: TJSFocusEvent): boolean; override;
    function HandleExit(AEvent: TJSEvent): boolean; override;
  protected
    procedure Changed; override;
    function CreateHandleElement: TJSHTMLElement; override;
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Close; virtual;
    function CloseQuery: boolean; virtual;
    procedure FocusControl(const AControl: TWinControl); virtual;
    procedure Hide; virtual;
    procedure Loaded; override;
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure ShowModal(AModalResultProc: TModalResultProc); virtual;
  public
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
    property AlphaBlend: boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: byte read FAlphaBlendValue write SetAlphaBlendValue;
    property FormType: TFormType read FFormType;
    property KeyPreview: boolean read FKeyPreview write FKeyPreview;
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;
  TCustomFormClass = class of TCustomForm;

  { TApplication }

  TApplication = class(TComponent)
  private
    FForms: TJSArray;
    FActiveForm: TCustomForm;
    FMainForm: TCustomForm;
    FStopOnException: boolean;
    FTerminated: boolean;
    FTitle: string;
    FOnResize: TNotifyEvent;
    FOnUnload: TNotifyEvent;
    function GetApplicatioName: string;
    function GetForm(const AIndex: NativeInt): TCustomForm;
    function GetFormCount: NativeInt;
    function GetFormIndex(const AForm: TCustomForm): NativeInt;
    function GetTitle: string;
    procedure SetTitle(AValue: string);
  protected
    procedure DoResize; virtual;
    procedure DoUnload; virtual;
    procedure LoadIcon; virtual;
  protected
    procedure RegisterForm(AForm: TCustomForm); virtual;
    procedure UnRegisterForm(AForm: TCustomForm); virtual;
    procedure RegisterHandleEvents; virtual;
    procedure UnRegisterHandleEvents; virtual;
  protected
    function HandleError(AEvent: TJSErrorEvent): boolean;
    function HandleResize(AEvent: TJSEvent): boolean;
    function HandleUnload(AEvent: TJSUIEvent): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateForm(AInstanceClass: TCustomFormClass; out AReference); virtual;
    procedure Initialize; virtual;
    procedure Run; virtual;
    procedure Terminate; virtual;   
    procedure UpdateMainForm(AForm: TCustomForm);
  public
    property ActiveForm: TCustomForm read FActiveForm write FActiveForm;
    property ApplicatioName: string read GetApplicatioName;
    property FormCount: NativeInt read GetFormCount;
    property FormIndex[const AForm: TCustomForm]: NativeInt read GetFormIndex;
    property Forms[const AIndex: NativeInt]: TCustomForm read GetForm;
    property MainForm: TCustomForm read FMainForm;
    property StopOnException: boolean read FStopOnException write FStopOnException;
    property Terminated: boolean read FTerminated;
    property Title: string read GetTitle write SetTitle;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnUnload: TNotifyEvent read FOnUnload write FOnUnload;
  end;

{ TODO: TScreen }

function Application: TApplication;

implementation

{$hints off}

procedure DefaultModalProc(Sender: TObject; ModalResult: TModalResult);
begin
  if (Assigned(Sender)) then
  begin
    Sender.Destroy;
    Sender := nil;
  end;
end;

{$hints on}

var
  VAppInstance: TApplication;

function Application: TApplication;
begin
  if not (Assigned(VAppInstance)) then
  begin
    VAppInstance := TApplication.Create(nil);
  end;
  Result := VAppInstance;
end;


type

  { TOverlay }

  TOverlay = class
  private
    FForm: TCustomForm;
    FHandleElement: TJSHTMLElement;
  public
    constructor Create(const AForm: TCustomForm); reintroduce;
    destructor Destroy; override;
  end;

{ TOverlay }

constructor TOverlay.Create(const AForm: TCustomForm);
begin
  FForm := AForm;
  if (Assigned(FForm)) then
  begin
    FHandleElement := TJSHTMLElement(Document.CreateElement('div'));
    with FHandleElement do
    begin
      /// Bounds
      Style.SetProperty('left', '0px');
      Style.SetProperty('top', '0px');
      Style.SetProperty('height', '100%');
      Style.SetProperty('width', '100%');
      /// Color
      Style.SetProperty('background', 'rgba(0, 0, 0, 0.6)');
      /// Position
      Style.SetProperty('position', 'absolute');
      /// Scroll
      Style.SetProperty('overflow', 'hidden');
    end;
    /// Register
    FForm.HandleElement.AppendChild(FHandleElement);
  end;
end;

destructor TOverlay.Destroy;
begin
  /// UnRegister
  if (Assigned(FForm)) then
  begin
    FForm.HandleElement.RemoveChild(FHandleElement);
  end;
  inherited Destroy;
end;

{ TCustomFrame }

procedure TCustomFrame.Changed;
begin
  inherited Changed;
  if (not IsUpdating) then
  begin
    with HandleElement do
    begin
      /// Focus highlight
      Style.SetProperty('outline', 'none');
      /// Scroll
      Style.SetProperty('overflow', 'auto');
    end;
  end;
end;

function TCustomFrame.CreateHandleElement: TJSHTMLElement;
begin
  Result := TJSHTMLElement(Document.CreateElement('div'));
end;

class function TCustomFrame.GetControlClassDefaultSize: TSize;
begin
  Result.Cx := 320;
  Result.Cy := 240;
end;

constructor TCustomFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginUpdate;
  try
    ParentFont := False;
    ParentShowHint := False;
    with GetControlClassDefaultSize do
    begin
      SetBounds(0, 0, Cx, Cy);
    end;
  finally
    EndUpdate;
  end;
end;

{ TCustomForm }

procedure TCustomForm.SetActiveControl(AValue: TWinControl);
begin
  if (FActiveControl <> AValue) then
  begin
    FActiveControl := AValue;
  end;
end;

procedure TCustomForm.SetAlphaBlend(AValue: boolean);
begin
  if (FAlphaBlend <> AValue) then
  begin
    FAlphaBlend := AValue;
    Changed;
  end;
end;

procedure TCustomForm.SetAlphaBlendValue(AValue: byte);
begin
  if (FAlphaBlendValue <> AValue) then
  begin
    FAlphaBlendValue := AValue;
    Changed;
  end;
end;

procedure TCustomForm.SetModalResult(AValue: TModalResult);
begin
  if (FModalResult <> AValue) then
  begin
    FModalResult := AValue;
    if (FModalResult <> mrNone) and (Assigned(FModalResultProc)) then
    begin
      Close;
    end;
  end;
end;

procedure TCustomForm.Activate;
begin
  if (Assigned(FOnActivate)) then
  begin
    FOnActivate(Self);
  end;
end;

procedure TCustomForm.Deactivate;
begin
  if (Assigned(FOnDeactivate)) then
  begin
    FOnDeactivate(Self);
  end;
end;

{$hints off}

procedure TCustomForm.DoClose(var CloseAction: TCloseAction);
begin
  if (Assigned(FOnDeactivate)) then
  begin
    FOnDeactivate(Self);
  end;
end;

{$hints on}

procedure TCustomForm.DoCreate;
begin
  if (Assigned(FOnCreate)) then
  begin
    FOnCreate(Self);
  end;
end;

procedure TCustomForm.DoDestroy;
begin
  if (Assigned(FOnDestroy)) then
  begin
    FOnDestroy(Self);
  end;
end;

procedure TCustomForm.DoHide;
begin
  if (Assigned(FOnHide)) then
  begin
    FOnHide(Self);
  end;
end;

procedure TCustomForm.DoResize;
begin
  inherited DoResize;
  if (Assigned(OnResize)) then
  begin
    FOnResize(Self);
  end;
end;

procedure TCustomForm.DoShow;
begin
  if (Assigned(FOnShow)) then
  begin
    FOnShow(Self);
  end;
end;

function TCustomForm.HandleEnter(AEvent: TJSFocusEvent): boolean;
var
  VControl: TWinControl;
begin
  Result := inherited HandleEnter(AEvent);
  if (Assigned(FChildForm)) and (FChildForm.FormType = ftModalForm) then
  begin
    FChildForm.Show;
  end
  else
  begin
    if (Assigned(FActiveControl)) then
    begin
      VControl := FActiveControl;
    end
    else
    begin
      VControl := FindFocusControl(nil, fsdFirst);
    end;
    FocusControl(VControl);
    Activate;
  end;
end;

function TCustomForm.HandleExit(AEvent: TJSEvent): boolean;
begin
  Result := inherited HandleExit(AEvent);
  Deactivate;
end;

procedure TCustomForm.Changed;
begin
  inherited Changed;
  if (not IsUpdating) then
  begin
    with HandleElement do
    begin
      /// Focus highlight
      Style.SetProperty('outline', 'none');
      /// AphaBlend/Opacity
      if (FAlphaBlend) then
      begin
        Style.SetProperty('opacity', FloatToStr(FAlphaBlendValue div 255));
      end
      else
      begin
        Style.RemoveProperty('opacity');
      end;
      /// Scroll
      Style.SetProperty('overflow', 'auto');
    end;
  end;
end;

function TCustomForm.CreateHandleElement: TJSHTMLElement;
begin
  Result := TJSHTMLElement(Document.CreateElement('div'));
end;

class function TCustomForm.GetControlClassDefaultSize: TSize;
begin
  Result.Cx := 320;
  Result.Cy := 240;
end;

constructor TCustomForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActiveControl := nil;
  FAlphaBlend := False;
  FAlphaBlendValue := 255;
  FChildForm := nil;
  FFormType := ftWindow;
  FKeyPreview := False;
  FModalResult := mrNone;
  FModalResultProc := nil;
  FOverlay := nil;
  BeginUpdate;
  try
    Color := clWhite;
    ParentFont := False;
    ParentShowHint := False;
    Visible := False;
    with GetControlClassDefaultSize do
    begin
      SetBounds(0, 0, Cx, Cy);
    end;
  finally
    EndUpdate;
  end;
end;

destructor TCustomForm.Destroy;
begin                   
  FActiveControl := nil;
  FChildForm := nil;
  inherited Destroy;
end;

procedure TCustomForm.AfterConstruction;
begin
  inherited AfterConstruction;
  Application.UpdateMainForm(Self);
  Application.RegisterForm(Self);
  Loaded;
  DoCreate;
end;

procedure TCustomForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Application.UnRegisterForm(Self);
  DoDestroy;
end;

procedure TCustomForm.Close;
var
  VAction: TCloseAction;   
  VIndex: NativeInt;
  VLastForm: TCustomForm;
  VOwnerForm: TCustomForm;
begin
  if (CloseQuery) then
  begin
    VAction := caHide;
    DoClose(VAction);
    if (VAction <> caNone) then
    begin
      if (Application.MainForm = Self) then
      begin
        Application.Terminate;
      end
      else
      begin
        Hide;  
        if (FFormType = ftModalForm) then
        begin
          if (Assigned(Owner)) and (Owner is TCustomForm) then
          begin    
            VOwnerForm := TCustomForm(Owner);
            VOwnerForm.ChildForm := nil;
            if (Assigned(VOwnerForm.Overlay)) then
            begin
              VOwnerForm.Overlay.Destroy;
              VOwnerForm.Overlay := nil;
            end; 
            VOwnerForm.Show;
          end;
          /// Execute Modal Proc
          if (Assigned(FModalResultProc)) then
          begin
            FModalResultProc(Self, FModalResult);
          end;
        end
        else
        begin
          /// Last active form
          for VIndex := (Application.FormCount - 1) downto 0 do
          begin
            VLastForm := Application.Forms[VIndex];
            if (Assigned(VLastForm)) and (VLastForm.Visible) and (VLastForm <> Self) then
            begin
              VLastForm.Show;
              Exit;
            end;
          end;
          if (Assigned(Application.MainForm)) then
          begin
            Application.MainForm.Show;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomForm.CloseQuery: boolean;
begin
  Result := True;
  if (Assigned(FOnCloseQuery)) then
  begin
    FOnCloseQuery(Self, Result);
  end;
end;

procedure TCustomForm.FocusControl(const AControl: TWinControl);
begin
  if (Assigned(AControl)) and (AControl.CanSetFocus) then
  begin
    AControl.SetFocus;
  end;
end;

procedure TCustomForm.Hide;
begin
  Visible := False;
  DoHide;
end;

procedure TCustomForm.Loaded;
begin
  inherited Loaded;
  /// Used only for forms (for controls created at runtime)
end;

procedure TCustomForm.Resize;
var
  VHeight: NativeInt;
  VLeft: NativeInt;
  VTop: NativeInt;
  VWidth: NativeInt;
  VWindowHeight: NativeInt;
  VWindowWidth: NativeInt;
begin
  VWindowWidth := Window.InnerWidth;
  VWindowHeight := Window.InnerHeight;
  case FFormType of
    ftModalForm:
    begin
      VWidth := Width;
      VHeight := Height;
      VLeft := (VWindowWidth - VWidth) div 2;
      VTop := (VWindowHeight - VHeight) div 2;
      SetBounds(VLeft, VTop, VWidth, VHeight);
    end;
    ftWindow:
    begin
      SetBounds(0, 0, VWindowWidth, VWindowHeight);
    end;
  end;
  DoResize;
end;

procedure TCustomForm.Show;
begin          
  Application.ActiveForm := Self;
  Application.Title := Caption;
  BeginUpdate;
  try
    Visible := True;
    Resize;
  finally
    EndUpdate;
  end;
  BringToFront;
  SetFocus;               
  DoShow;
end;

procedure TCustomForm.ShowModal(AModalResultProc: TModalResultProc);
Var
  VForm: TCustomForm;
begin
  if (not(Assigned(Owner))) then
  begin
   raise TJSError.New('Owner not found.');
  end;
  if (not(Owner is TCustomForm)) then
  begin
    raise TJSError.New('Invalid owner.');
  end;
  VForm := TCustomForm(Owner);
  if (Assigned(VForm.ChildForm)) then
  begin
    raise TJSError.New('Modal form already exists.');
  end;
  VForm.ChildForm := Self;
  VForm.Overlay := TOverlay.Create(VForm);
  FFormType := ftModalForm;
  FModalResult := mrNone;
  if (Assigned(AModalResultProc)) then
  begin
    FModalResultProc := AModalResultProc;
  end
  else
  begin
    FModalResultProc := @DefaultModalProc;
  end;
  Show;
end;

{ TApplication }

function TApplication.GetForm(const AIndex: NativeInt): TCustomForm;
begin
  Result := TCustomForm(FForms[AIndex]);
end;

function TApplication.GetApplicatioName: string;
begin
  Result := Window.Location.PathName;
end;

function TApplication.GetFormCount: NativeInt;
begin
  Result := FForms.Length;
end;

function TApplication.GetFormIndex(const AForm: TCustomForm): NativeInt;
begin
  Result := FForms.IndexOf(AForm);
end;

function TApplication.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TApplication.SetTitle(AValue: string);
begin
  if (FTitle <> AValue) then
  begin
    FTitle := AValue;
    Document.Title := FTitle;
  end;
end;

procedure TApplication.DoResize;
begin
  if (Assigned(FOnResize)) then
  begin
    FOnResize(Self);
  end;
end;

procedure TApplication.DoUnload;
begin
  if (Assigned(FOnUnload)) then
  begin
    FOnUnload(Self);
  end;
end;

procedure TApplication.LoadIcon;
Var
  VHRef: string;
begin
  /// Add an icon logo to the title bar
  with TJSHTMLElement(Document.Head.AppendChild(Document.CreateElement('link'))) do
  begin
    SetAttribute('rel', 'icon');
    SetAttribute('type', 'image/icon');
    SetAttribute('href',  TJSString(ApplicatioName).Replace('html', 'ico'));
  end;
end;

procedure TApplication.RegisterForm(AForm: TCustomForm);
begin
  if (Assigned(AForm)) then
  begin
    if (FForms.IndexOf(AForm) = -1) then
    begin
      FForms.Push(AForm);
      if (not Document.Body.Contains(AForm.HandleElement)) then
      begin
        Document.Body.AppendChild(AForm.HandleElement);
      end;
    end;
  end;
end;

procedure TApplication.UnRegisterForm(AForm: TCustomForm);
var
  VIndex: NativeInt;
begin
  if (Assigned(AForm)) then
  begin
    VIndex := FForms.IndexOf(AForm);
    if (VIndex >= 0) then
    begin
      FForms.Splice(VIndex, 1);
      if (Document.Body.Contains(AForm.HandleElement)) then
      begin
        Document.Body.RemoveChild(AForm.HandleElement);
      end;
    end;
  end;
end;

procedure TApplication.RegisterHandleEvents;
begin
  with Window do
  begin
    AddEventListener('error', @HandleError);
    AddEventListener('resize', @HandleResize);
    AddEventListener('unload', @HandleUnload);
  end;
end;

procedure TApplication.UnRegisterHandleEvents;
begin
  with Window do
  begin
    RemoveEventListener('error', @HandleError);
    RemoveEventListener('resize', @HandleResize);
    RemoveEventListener('unload', @HandleUnload);
  end;
end;

function TApplication.HandleError(AEvent: TJSErrorEvent): boolean;
const
  CLE = LineEnding;
  CError = 'Error Message: %s ' + CLE + 'Line Nro: %d ' + CLE + 'Column Nro: %d ' + CLE;
begin
  if (TJSString(TJSString(AEvent.Message).ToLowerCase).IndexOf('script error') > -1) then
  begin
    Window.Alert('Script Error: See Browser Console for Detail');
  end
  else
  begin
    Window.Alert(Format(CError, [AEvent.Message, AEvent.LineNo, AEvent.ColNo]))
  end;
  if (FStopOnException) then
  begin
    Terminate;
  end;
  AEvent.StopPropagation;
  Result := False;
end;

function TApplication.HandleResize(AEvent: TJSEvent): boolean;
var
  VForm: TCustomForm;
  VIndex: NativeInt;
begin
  AEvent.StopPropagation;
  DoResize();
  Result := True;
  /// Notify all of forms resize
  for VIndex := 0 to (FForms.Length - 1) do
  begin
    VForm := TCustomForm(FForms[VIndex]);
    if (Assigned(VForm)) and (VForm.Visible) then
    begin
      VForm.Resize;
    end;
  end;
end;

function TApplication.HandleUnload(AEvent: TJSUIEvent): boolean;
begin
  AEvent.StopPropagation;
  Result := True;
  try
    DoUnload();
  finally
    Terminate;
  end;
end;

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForms := TJSArray.New;
  FMainForm := nil;
  FStopOnException := True;
  FTerminated := False;
  FTitle := '';
end;

destructor TApplication.Destroy;
begin
  FForms.Length := 0;
  inherited Destroy;
end;

procedure TApplication.CreateForm(AInstanceClass: TCustomFormClass; out AReference);
begin
  try        
    AReference := AInstanceClass.Create(Self);
  except
    on E: Exception do
    begin
      { TODO: Exception? }
      AReference := nil;
    end;
  end;
end;

procedure TApplication.Initialize;
begin
end;

procedure TApplication.Run;
begin
  { TODO: Add a background to the application }
  RegisterHandleEvents;
  LoadIcon;
  if (Assigned(FMainForm)) then
  begin
    FMainForm.Show;
  end;
end;

procedure TApplication.Terminate;
var
  VForm: TCustomForm;
  VIndex: NativeInt;
begin
  if (not FTerminated) then
  begin
    UnRegisterHandleEvents;
    FTerminated := True;
    for VIndex := (FForms.Length - 1) downto 0 do
    begin
      VForm := TCustomForm(FForms[VIndex]);
      if (Assigned(VForm)) then
      begin
        VForm.Destroy;
        VForm := nil;
      end;
    end;
  end;
end;

procedure TApplication.UpdateMainForm(AForm: TCustomForm);
begin
  if (not Assigned(FMainForm)) then
  begin
    FMainForm := AForm;
    FActiveForm := AForm;
  end;
end;

end.
