unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.WebBrowser,
  SG.ScriptGate;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    Layout1: TLayout;
    Button1: TButton;
    Layout2: TLayout;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private var
    FScriptGate: TScriptGate;
  public
    procedure HelloDelphi(const iStr: String);
    procedure Add(const Msg: String; const A, B: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  SampleHTML =
    '<html>' +
    '<header>' +
      '<script type="text/JavaScript">' +
        'function helloJS(msg, msg2) { alert(msg + msg2); return "Hello Delphi ! I am JavaScript !"; }' + // Call from Delphi
      '</script>' +
    '</head>' +
    '<body>' +
      '<br><br>' + // Call Delphi Method
      '<a href="YourOrgScheme:HelloDelphi(''call by JS'')">Call Delphi noparam procedure</a>' +
      '<br><br>' +
      '<a href="YourOrgScheme:Add(''Calc: 30 + 12 = '', 30, 12)">Call Delphi procedure</a>' +
    '<body>' +
    '</html>';

procedure TForm1.FormCreate(Sender: TObject);
begin
  WebBrowser1.LoadFromStrings(SampleHTML, '/');

  // The method of the object specified by the first argument is
  // called from JavaScript.
  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'YourOrgScheme');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.CallScript(
    'helloJS("Hello JS ! ", "I am Delphi !")',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult);
    end
  );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FScriptGate.Eval(
    'document.getElementsByTagName("html")[0].outerHTML',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult);
    end
  );
end;

procedure TForm1.Add(const Msg: String; const A, B: Integer);
begin
  ShowMessage(Msg + (A + B).ToString);
end;

procedure TForm1.HelloDelphi(const iStr: String);
begin
  ShowMessage('Hello ! ' + iStr);
end;

end.
