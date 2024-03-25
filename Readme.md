#ScriptGate

##Overview

ScriptGate is a library that realizes mutual calling of JavaScript and Delphi code on TWebBrowser.

[日本語版はこちら](https://bitbucket.org/freeonterminate/scriptgate/src/master/Readme_ja.md)

##Environment

| Item        | Description                  |
|-------------|------------------------------|
| Environment | Delphi, RAD Studio           |
| Version     | 10.2.x Tokyo, 10.3 Rio, 10.4 , 11 , 12       |
| Framework   | FireMonkey                   |
| Support OS  | Windows, macOS, Android, iOS |

##History
2018/12/10 10.3 Rio is supported.
2024/03/25 D11,12(iOS) is supported by DelphiTeacher.

##Files
| Files                   | Description                          |
|-------------------------|--------------------------------------|
| README.txt              | Readme                               |
| LICENSE.txt             | License                              |
| SG.ScriptGate.pas       | ScriptGate Source                    |
| SG.ScriptGateHelper.pas | ScriptGate Helper Source             |
| SG.WebBrowserHelper.pas | ScriptGate TWebBrowser Helper Source |
| SG.ScriptGate.Win.pas   | ScriptGate Windows Source            |
| SG.ScriptGate.Mac.pas   | ScriptGate macOS Source              |
| SG.ScriptGate.iOS.pas   | ScriptGate iOS Source                |
| SG.ScriptGate.Android.pas                   | ScriptGate Android Source |
| SG.ScriptGate.Android.SGWebClientBridge.pas | SGWebClient Bridge Source |
| Android.JNI.SGWebClient.pas                 | SGWebClient JNI Source    |
| SGWebClient.jar                             | SGWebClient Jar File      |

##Usage

[For details, please click here (but, it is written in Japanese)](http://qiita.com/items/dacb1be7ad528d27803a)


1.  
Add the folder of the above file to the search path.  
Add SG.ScriptGate to the uses block.  

2.  
Add SGWebClient.jar to ProjectManager -> Target Platform -> Android -> Library.  

3.  
Call TScriptGate.Create to create a ScriptGate instance.  
The arguments of TScriptGate.Create are as follows.  
	1. An object that exposes methods to JavaScript  
	2. An associated instance of TWebBrowser  
	3. Scheme when calling from JavaScript  

###How to call JavaScript Method from Delphi
TScriptGate.CallScript  
argument  
	1. Write a JavaScript method call with a string.  
	2. An anonymous method that accepts the value evaluated for the JavaScript passed in the argument.  
　  
or  
　  
TScritGate.Eval  
argument  
	1. Specify any statement in JavaScript  
	2. An anonymous method that accepts the value evaluated for the JavaScript passed in the argument.  


###How to call Delphi's Method from JavaScript
It is SCHEME: DelphiMethod (argument) where JavaScript can be written.  
Here, SCHEME is the character string specified by the third argument of the constructor.  

##Example
###HTML / JavaScript
```html
<html> 
  <header> 
    <script type="text/JavaScript"> 
      function helloJS() { alert("Hello, JavaScript!"); return "Hello !!"; }
    </script> 
  </head> 

  <body> 
    <br><br> 
    <a href="delphi:HelloDelphi()">Call Delphi procedure</a> 
  <body> 
</html>;
```

###Delphi
```ObjectPascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Binding ScriptGate to WebBrowser and setting the scheme delphi
  // The scheme is also specified on the JavaScript side
  // Same as file:, JavaScript: etc.
  ScriptGate := TScriptGate.Create(Self, WebBrowser1, 'delphi');
end;

// Call helloJS () JavaScript.
// You can also retrieve the return value using an anonymous function.
procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.CallScript(
    'helloJS()',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult); // Show return value
    end
  );
end;

// Execute arbitrary JavaScript
// You can also retrieve the return value using an anonymous function.
procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.Eval(
    'document.getElementsByTagName("html")[0].outerHTML',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult); // Show return value
    end
  );
end;

// It is a method published in JavaScript and is called from JavaScript.
procedure TForm1.HelloDelphi;
begin
  ShowMessage('Hello, Delphi!');
end;

```

##Contact
freeonterminate@gmail.com  
http://twitter.com/pik  
      
#LICENSE
Copyright (c) 2017, 2018 HOSOKAWA Jun
Released under the MIT license  
http://opensource.org/licenses/mit-license.php
