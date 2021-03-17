import 'package:flutter/material.dart';
import 'dart:async';
import 'dart:io';
import 'package:webview_flutter/webview_flutter.dart';
import 'package:mobile/Pages/ConfirmationPage.dart';

class AreaWebView extends StatelessWidget {
  final String title;
  final String pageUrl;
  final Completer<WebViewController> _controller =
      Completer<WebViewController>();

  AreaWebView(this.title, this.pageUrl);

  void initState() {
    if (Platform.isAndroid) WebView.platform = SurfaceAndroidWebView();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(title),
        // This drop down menu demonstrates that Flutter widgets can be shown over the web view.
      ),
      // We're using a Builder here so we have a context that is below the Scaffold
      // to allow calling Scaffold.of(context) so we can show a snackbar.
      body: Builder(builder: (BuildContext context) {
        return WebView(
          initialUrl: pageUrl,
          javascriptMode: JavascriptMode.unrestricted,
          onWebViewCreated: (WebViewController webViewController) {
            _controller.complete(webViewController);
          },
          navigationDelegate: (NavigationRequest request) async {
            print("Request: " + request.url);
            return NavigationDecision.navigate;
          },
          gestureNavigationEnabled: true,
          onPageFinished: (url) {
            // print("UR L: " + url);
            if (url.startsWith(
                "https://area-junaifu-front.wonderful-goose-4.telebit.io/services")) {
              sleep(Duration(seconds: 6));
              Navigator.pushReplacement(
                  context,
                  MaterialPageRoute(
                      builder: (BuildContext context) => ConfirmationPage(
                          "Service subscribed successfully.", "/home")));
            }
          },
        );
      }),
    );
  }
}
