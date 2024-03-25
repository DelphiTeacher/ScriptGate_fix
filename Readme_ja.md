#ScriptGate

##概要

ScriptGate は TWebBrowser 上の JavaScript と Delphi メソッドの相互呼び出しを実現します。  

##動作環境

| 項目       | 内容                         |
|------------|------------------------------|
| 環境       | Delphi, RAD Studio           |
| Version    | 10.2.x Tokyo, 10.3 Rio       |
| Framework  | FireMonkey                   |
| サポートOS | Windows, macOS, Android, iOS |

##履歴
2018/12/10 10.3 Rio に対応しました。

##ファイル

| Source Files            | Description                          |
|-------------------------|--------------------------------------|
| README.txt              | Readme                               |
| LICENSE.txt             | ライセンス                           |
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


##使用方法

[詳しくはこちらをご覧ください。](http://qiita.com/items/dacb1be7ad528d27803a)

1.  
上記のファイルを検索パスに追加し、SG.ScriptGate を uses します。  

2.  
SGWebClient.jar を ProjectManager -> Target Platform -> Android -> Library に追加します。  

3.  
TScriptGate.Create を呼んで ScriptGate インスタンスを作成します。  
TScriptGate.Create の引数は以下の通り。  
	1. JavaScript に対してメソッドを公開するオブジェクト  
	2. 関連付く TWebBrowser のインスタンス  
	3. JavaScript から呼び出す際のスキーム  


###Delphi から JavaScript の Method を呼び出す方法
TScriptGate.CallScript  
引数  
	1. JavaScript のメソッド呼び出しを文字列で記述します。  
	2. 引数に渡された JavaScript の式として評価された値を受け取る無名メソッドです。  
　  
もしくは  
　  
TScritGate.Eval  
引数  
	1. JavaScript の任意の文を指定します。  
	2. 引数に渡された JavaScript の式として評価された値を受け取る無名メソッドです。  


###JavaScript から Delphi の Method を呼び出す方法
JavaScript が書けるところで SCHEME:DelphiMethod(引数) とします。  
ここで SCHEME とはコンストラクタの第三引数で指定した文字列です。  


##例
HTML / JavaScript 側
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

Delphi 側
```ObjectPascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // ScriptGate と WebBrowser を結びつけて、スキーム delphi を設定する
  // スキームは JavaScript 側でも同じものを指定する
  // file: や JavaScript: といったものと同じ
  ScriptGate := TScriptGate.Create(Self, WebBrowser1, 'delphi');
end;

// JavaScript の helloJS() を呼び出します。
// 無名関数を使って戻値も取得できます。
procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.CallScript(
    'helloJS()',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult); // 戻値を表示
    end
  );
end;

// 任意の JavaScript を実行します。
// 無名関数を使って戻値も取得できます。
procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.Eval(
    'document.getElementsByTagName("html")[0].outerHTML',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult); // 戻値を表示
    end
  );
end;

// JavaScript に公開されるメソッドで、JavaScript から呼び出されます。
procedure TForm1.HelloDelphi;
begin
  ShowMessage('Hello, Delphi!');
end;
```

##連絡先
freeonterminate@gmail.com  
https://twitter.com/pik  
      
#ライセンス
Copyright (c) 2017, 2018 HOSOKAWA Jun
Released under the MIT license  
http://opensource.org/licenses/mit-license.php
