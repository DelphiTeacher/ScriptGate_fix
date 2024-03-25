unit SG.ScriptGate.iOS;

// (*
{$IFNDEF IOS}
{$WARNINGS OFF 1011}
interface
implementation
end.
{$ENDIF}
// *)

interface

procedure RegisterScriptGate;

implementation

uses
  SG.ScriptGate
  , SG.WebBrowserHelper
  , System.SysUtils
  , System.Rtti
  , Macapi.ObjectiveC
  , Macapi.Helpers
  , iOSapi.Foundation
  , iOSapi.UIKit
  , FMX.Platform
  , FMX.WebBrowser
  , iOSapi.WebKit
  , Macapi.ObjCRuntime
  , FMX.Helpers.iOS
  , FMX.WebBrowser.Delegate.Cocoa
  , FMX.WebBrowser.Delegate.iOS
  ;

type
  TOpenScriptGate = class(TScriptGate);

//  TSGWebViewDelegate = class (TBaseWebViewDelegate, WKUIDelegateSlim, WKNavigationDelegateSlim)
//  private var
//    [Weak] FOrgDelegate: WebUIDelegate;
//    [Weak] FScriptGate: TOpenScriptGate;
//  public
//    constructor Create(
//      const iOrgDelegate: WebUIDelegate;
//      const iScriptGate: TOpenScriptGate);
//    procedure webView(
//      webView: INativeWebView;
//      didFailLoadWithError: NSError); overload; cdecl;
//    function webView(
//      webView: INativeWebView;
//      shouldStartLoadWithRequest: NSURLRequest;
//      navigationType: UIWebViewNavigationType): Boolean; overload; cdecl;
//    procedure webViewDidFinishLoad(webView: INativeWebView); cdecl;
//    procedure webViewDidStartLoad(webView: INativeWebView); cdecl;
//  end;

{ TSGWebViewDelegate }
  /// <summary> Class delegate for reciving call backs from native UIWebView</summary>
  TSGWebViewDelegate = class(TBaseWebViewDelegate,
//                            WKUIDelegateSlim,
                            WKNavigationDelegateSlim
                            )
  private
    [Weak] FOrgDelegate: WebNavigationDelegate;
    [Weak] FScriptGate: TOpenScriptGate;

//    FWebBrowser: TCustomWebBrowser;
//    [unsafe] FURLSetter: ICustomBrowser;
  public
    { WKUIDelegate }
//    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
//    function webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
//      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
//    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
//    procedure webViewRunJavaScriptAlertPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
//      completionHandler: Pointer); cdecl;
//    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
//    procedure webViewRunJavaScriptConfirmPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
//      completionHandler: Pointer); cdecl;
//    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
//    procedure webViewRunJavaScriptTextInputPanelWithPrompt(webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
//      completionHandler: Pointer); cdecl;

    { WKNavigationDelegate }
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
      decisionHandler: Pointer); overload; cdecl;
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
      decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
  public
    /// <summary>Return a pointer to a native delegate</summary>
    constructor Create(
      const iOrgDelegate: WebNavigationDelegate;
      const iScriptGate: TOpenScriptGate);
    /// <summary>Setter for callback's receiver</summary>
//    procedure SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
  end;




  TScriptGateIOS = class(TScriptGateBase, IScriptGate)
  private var
    FWebView: INativeWebView;
    FDelegate: TSGWebViewDelegate;
    FScriptGate: TOpenScriptGate;
    FResultProc: TScriptGateResultProc;
  protected
    procedure WKWebViewBlockMethod1(param1: Pointer; error: NSError);
    procedure CallScript(
      const iScript: String;
      const iResultProc: TScriptGateResultProc); override;
  public
    constructor Create(const iScriptGate: TScriptGate);
    destructor Destroy; override;
  end;

  TScriptGateFactoryIOS = class(TScriptGateFactory)
  public
    function CreateScriptGate(
      const iScriptGate: TScriptGate): IScriptGate; override;
  end;

procedure RegisterScriptGate;
begin
  TPlatformServices.Current.AddPlatformService(
    IScriptGateFactory,
    TScriptGateFactoryIOS.Create);
end;

{ TScriptGateFactoryIOS }

function TScriptGateFactoryIOS.CreateScriptGate(
  const iScriptGate: TScriptGate): IScriptGate;
begin
  Result := TScriptGateIOS.Create(iScriptGate);
end;

{ TScriptGateIOS }

procedure TScriptGateIOS.CallScript(
  const iScript: String;
  const iResultProc: TScriptGateResultProc);
//var
//  Result: String;
begin
//  Result :=
//    NSStrToStr(
//      FWebView.evaluateJavaScript(StrToNSSTR(iScript),nil));
  FResultProc:=iResultProc;
  FWebView.evaluateJavaScript(StrToNSSTR(iScript),WKWebViewBlockMethod1);

//  if (Assigned(iResultProc)) then
//    iResultProc(Result);
end;

constructor TScriptGateIOS.Create(const iScriptGate: TScriptGate);
var
  Delegate: WebNavigationDelegate;
begin
  inherited Create;

  FScriptGate := TOpenScriptGate(iScriptGate);

  try
    FWebView := FScriptGate.WebBrowser.GetField<INativeWebView>('FWebView');
    Delegate := FScriptGate.WebBrowser.GetField<WebNavigationDelegate>('FNavigationDelegate');
  except
    FWebView := nil;
    Delegate := nil;
  end;

  if (FWebView = nil) or (Delegate = nil) then
    FScriptGate.CallErrorEvent(
      TScriptGate.TScriptGateFieldNotFound.Create(
        'Field not found in FMX.WebBrowser: FWebView, FDelegate'
      )
    )
  else
  begin
    FDelegate := TSGWebViewDelegate.Create(Delegate, FScriptGate);
    FWebView.setNavigationDelegate(FDelegate.GetObjectID);
  end;
end;

destructor TScriptGateIOS.Destroy;
begin
  FWebView := nil;
  FScriptGate := nil;

  FDelegate.DisposeOf;

  inherited;
end;

procedure TScriptGateIOS.WKWebViewBlockMethod1(param1: Pointer; error: NSError);
var
  Result: String;
begin
  Result :=
    NSStrToStr(TNSString.Wrap(param1));

  if (Assigned(FResultProc)) then
    FResultProc(Result);

end;

//{ TSGWebViewDelegate }
//
//procedure TSGWebViewDelegate.webView(
//  webView: INativeWebView;
//  didFailLoadWithError: NSError);
//begin
//  FOrgDelegate.webView(webView, didFailLoadWithError);
//end;
//
//constructor TSGWebViewDelegate.Create(
//  const iOrgDelegate: WebUIDelegate;
//  const iScriptGate: TOpenScriptGate);
//begin
//  inherited Create;
//
//  FOrgDelegate := iOrgDelegate;
//  FScriptGate := iScriptGate;
//end;
//
//function TSGWebViewDelegate.webView(
//  webView: INativeWebView;
//  shouldStartLoadWithRequest: NSURLRequest;
//  navigationType: UIWebViewNavigationType): Boolean;
//var
//  URL: String;
//begin
//  Result := True;
//  URL := NSStrToStr(shouldStartLoadWithRequest.URL.absoluteString);
//
//  FOrgDelegate.webView(webView, shouldStartLoadWithRequest, navigationType);
//
//  if (FScriptGate.CheckScheme(URL)) then
//  begin
//    Result := False;
//    FScriptGate.CallEvent(URL);
//  end;
//end;
//
//procedure TSGWebViewDelegate.webViewDidFinishLoad(webView: INativeWebView);
//begin
//  FOrgDelegate.webViewDidFinishLoad(webView);
//end;
//
//procedure TSGWebViewDelegate.webViewDidStartLoad(webView: INativeWebView);
//begin
//  FOrgDelegate.webViewDidStartLoad(webView);
//end;

{ TSGWebViewDelegate }

//constructor TSGWebViewDelegate.Create;
//begin
//  inherited Create;
//end;

constructor TSGWebViewDelegate.Create(
  const iOrgDelegate: WebNavigationDelegate;
  const iScriptGate: TOpenScriptGate);
begin
  inherited Create;

  FOrgDelegate := iOrgDelegate;
  FScriptGate := iScriptGate;
end;

//procedure TSGWebViewDelegate.SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
//begin
//  FWebBrowser := AWebBrowser;
//  FURLSetter := AURLSetter;
//end;

//function TSGWebViewDelegate.webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration;
//  navigationAction: WKNavigationAction; windowFeatures: WKWindowFeatures): WKWebView;
////var
////  Url: NSURL;
//begin
//  Result:=FOrgDelegate.webViewCreateWebViewWithConfiguration(webView,configuration,navigationAction,windowFeatures);
////  if navigationAction.targetFrame = nil then
////  begin
////    Url := navigationAction.request.URL;
////    SharedApplication.openURL(Url);
////  end
////  else if not navigationAction.targetFrame.isMainFrame then
////    webView.loadRequest(navigationAction.request);
////  Result := nil;
//end;

procedure TSGWebViewDelegate.webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
  decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
var
  URL: String;
begin
//  Result := True;
  URL := NSStrToStr(navigationAction.request.URL.absoluteString);
//  if FWebBrowser <> nil then
//  begin
//    FURLSetter.SetURL(NSStrToStr(navigationAction.request.URL.absoluteString));
//    FWebBrowser.ShouldStartLoading(FWebBrowser.URL);
//  end;
//  @LBlockImp := imp_implementationWithBlock(decisionHandler);
//  LBlockImp(WKNavigationActionPolicyAllow);
//  imp_removeBlock(@LBlockImp);

  FOrgDelegate.webViewDecidePolicyForNavigationAction(webView,navigationAction,decisionHandler);

  if (FScriptGate.CheckScheme(URL)) then
  begin
    //不允许跳转
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(WKNavigationActionPolicyCancel);
    imp_removeBlock(@LBlockImp);

    FScriptGate.CallEvent(URL);
  end;

end;

procedure TSGWebViewDelegate.webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
  decisionHandler: Pointer);
//var
//  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
//  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
//  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
//  imp_removeBlock(@LDecisionHandlerBlock);
  FOrgDelegate.webViewDecidePolicyForNavigationResponse(webView,navigationResponse,decisionHandler);
end;

procedure TSGWebViewDelegate.webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
//  if FWebBrowser <> nil then
//    FWebBrowser.FailLoadingWithError;
  FOrgDelegate.webViewDidFailNavigation(webView,navigation,error);
end;

procedure TSGWebViewDelegate.webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
//var
//  LURLString: NSString;
//  LURL: NSURL;
begin
//  if error.domain.isEqualToString(NSURLErrorDomain) and (error.code = NSURLErrorUnsupportedURL) then
//  begin
//    LURLString := TNSString.Wrap(error.userInfo.objectForKey(NSObjectToID(NSURLErrorFailingURLStringErrorKey)));
//    LURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(LURLString));
//    SharedApplication.openURL(LURL);
//  end
//  else if error.code = NSURLErrorTimedOut then
//    FWebBrowser.FailLoadingWithError;
  FOrgDelegate.webViewDidFailProvisionalNavigation(webView,navigation,error);
end;

procedure TSGWebViewDelegate.webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
//var
//  LCompletionHandlerBlock: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: Pointer; credential: Pointer); cdecl;
//  LAuthMethod: NSString;
begin
//  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
//  LAuthMethod := challenge.protectionSpace.authenticationMethod;
//  if LAuthMethod.isEqualToString(NSURLAuthenticationMethodDefault) or LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPBasic) or
//    LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPDigest) then
//  begin
//    AuthenticateForHost(webView.URL.host,
//      procedure(const ACredential: NSURLCredential)
//      begin
//        LCompletionHandlerBlock(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(ACredential));
//        imp_removeBlock(@LCompletionHandlerBlock);
//      end
//    );
//  end
//  else if LAuthMethod.isEqualToString(NSURLAuthenticationMethodServerTrust) then
//  begin
//    LCompletionHandlerBlock(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
//    imp_removeBlock(@LCompletionHandlerBlock);
//  end
//  else
//  begin
//    LCompletionHandlerBlock(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
//    imp_removeBlock(@LCompletionHandlerBlock);
//  end;
  FOrgDelegate.webViewDidReceiveAuthenticationChallenge(webView,challenge,completionHandler);
end;

procedure TSGWebViewDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation);
begin
//  if FWebBrowser <> nil then
//    FWebBrowser.StartLoading;
  FOrgDelegate.webViewDidStartProvisionalNavigation(webView,navigation);
end;

procedure TSGWebViewDelegate.webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation);
begin
//  if FURLSetter <> nil then
//    FURLSetter.SetURL(NSStrToStr(webView.URL.absoluteString));
//  if FWebBrowser <> nil then
//    FWebBrowser.FinishLoading;
  FOrgDelegate.webViewDidFinishNavigation(webView,navigation);
end;

//procedure TSGWebViewDelegate.webViewRunJavaScriptAlertPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
//  completionHandler: Pointer);
////var
////  LCompletionHandlerBlock: procedure; cdecl;
//begin
//  FOrgDelegate.webViewRunJavaScriptAlertPanelWithMessage(webView,message,frame,completionHandler);
////  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
////  JavaScriptInformationMessage(message,
////    procedure
////    begin
////      LCompletionHandlerBlock;
////      imp_removeBlock(@LCompletionHandlerBlock);
////    end
////  );
//end;
//
//procedure TSGWebViewDelegate.webViewRunJavaScriptConfirmPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
//  completionHandler: Pointer);
////var
////  LCompletionHandlerBlock: procedure(result: Boolean); cdecl;
//begin
//  FOrgDelegate.webViewRunJavaScriptConfirmPanelWithMessage(webView,message,frame);
////  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
////  JavaScriptConfirmationMessage(message,
////    procedure(const AResponse: Boolean)
////    begin
////      LCompletionHandlerBlock(AResponse);
////      imp_removeBlock(@LCompletionHandlerBlock);
////    end
////  );
//end;
//
//procedure TSGWebViewDelegate.webViewRunJavaScriptTextInputPanelWithPrompt(webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
//  completionHandler: Pointer);
////var
////  LCompletionHandlerBlock: procedure(result: Pointer); cdecl;
//begin
//  FOrgDelegate.webViewRunJavaScriptTextInputPanelWithPrompt(webView,prompt,defaultText,frame);
////  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
////  JavaScriptInputQuery(prompt, defaultText,
////    procedure(const AResponse: NSString)
////    begin
////      LCompletionHandlerBlock(NSObjectToID(AResponse));
////      imp_removeBlock(@LCompletionHandlerBlock);
////    end
////  );
//end;



initialization
  RegisterScriptGate;

end.

