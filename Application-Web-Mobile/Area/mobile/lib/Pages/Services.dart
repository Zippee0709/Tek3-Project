import 'package:flutter/material.dart';
import 'package:mobile/Models/ServiceModel.dart';
import 'package:mobile/Pages/ConfirmationPage.dart';
import 'package:mobile/Service/AreaServices.dart';
import 'package:flutter_icons/flutter_icons.dart';
import 'package:mobile/Store/Store.dart';
import 'package:mobile/Tools/AreaWebView.dart';
import 'package:mobile/Tools/PopUp.dart';
import 'package:mobile/Service/CreateService.dart';
import 'package:flutter_webview_plugin/flutter_webview_plugin.dart';

class Services extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return FutureBuilderGetServices();
  }
}

class FutureBuilderGetServices extends State<Services> {
  final flutterWebViewPlugin = FlutterWebviewPlugin();

  @override
  void initState() {
    super.initState();
    flutterWebViewPlugin.onUrlChanged.listen((String url) {
      if (url == "https://area-junaifu-front.wonderful-goose-4.telebit.io/") {
        flutterWebViewPlugin.close();
        storeStringValue("firstTimeService", "ok");
        Navigator.pushReplacement(
            context,
            MaterialPageRoute(
                builder: (BuildContext context) => ConfirmationPage(
                    "You can subscribe to all our service now.", "/home")));
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    return FutureBuilder<dynamic>(
      future: getStringValue("firstTimeService"),
      builder: (context, snapshot) {
        if (snapshot.hasData) {
          return servicePage();
        }
        return WebviewScaffold(
          url: "https://area-junaifu-front.wonderful-goose-4.telebit.io/login",
          appBar: AppBar(
            title: const Text('Connexion Services'),
          ),
          withZoom: true,
          withLocalStorage: true,
          hidden: true,
        );
      },
    );
  }
}

Widget servicePage() {
  return FutureBuilder<dynamic>(
    future: fetchAllServices(),
    builder: (context, snapshot) {
      List<Widget> children;
      if (snapshot.hasData) {
        return DisplayServices(
            allServices: genInfoByName(snapshot.data["allServices"],
                snapshot.data["available"], context));
      } else if (snapshot.hasError) {
        children = <Widget>[
          Icon(
            Icons.error_outline,
            color: Colors.red,
            size: 60,
          ),
          Padding(
            padding: const EdgeInsets.only(top: 16),
            child: Text('Error : ${snapshot.error}'),
          )
        ];
      } else {
        children = <Widget>[
          SizedBox(
            child: CircularProgressIndicator(),
            width: 60,
            height: 60,
          ),
          const Padding(
            padding: EdgeInsets.only(top: 16),
            child: Text('Awaiting result...'),
          )
        ];
      }
      return Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: children,
        ),
      );
    },
  );
}

List<ServiceInfo> genInfoByName(List<dynamic> servicesName,
    List<dynamic> servicesNotSubscribed, BuildContext context) {
  // Parser les infos via le nom du service
  List<ServiceInfo> services = new List<ServiceInfo>();
  for (String name in servicesName) {
    if (name == "GitLab") {
      services.add(ServiceInfo(
        name: name,
        isAvailable: !(servicesNotSubscribed
            .where((service) => service == "GitLab")
            .isEmpty),
        icon: MaterialCommunityIcons.gitlab,
        color: Colors.orange,
        callback: () => {
          Navigator.of(context).push(MaterialPageRoute(
              builder: (BuildContext context) => AreaWebView(
                  "GitLab Authentification",
                  "https://gitlab.com/oauth/authorize?client_id=431aa4bc4e71c9e1937985be9021cbbf549b4fe79ba8e5e111d818d9bcc7151b&redirect_uri=https://area-junaifu-front.wonderful-goose-4.telebit.io/services?state=GitLab&response_type=code&state=GitLab&scope=api"))) // TODO: change link to FrontWeb OAuth2 of GitLab
        },
      ));
    } else if (name == "Trello") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "Trello")
              .isEmpty),
          icon: MaterialCommunityIcons.trello,
          color: Colors.blue,
          callback: () => {
                Navigator.of(context).push(MaterialPageRoute(
                    builder: (BuildContext context) => AreaWebView(
                        "Trello Authentification",
                        "https://trello.com/1/authorize?expiration=30days&name=AREA-EPITECH&scope=read,write,account&response_type=token&key=ff4c32d87be5508c24f6390892f7d7a5&return_url=https://area-junaifu-front.wonderful-goose-4.telebit.io/services?state=Trello")))
              }));
    } else if (name == "Discord") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "Discord")
              .isEmpty),
          icon: MaterialCommunityIcons.discord,
          color: Colors.purple,
          callback: () => {
                Navigator.of(context).push(MaterialPageRoute(
                    builder: (BuildContext context) => AreaWebView(
                        "Discord Authentification",
                        "https://discord.com/api/oauth2/authorize?client_id=815209494161260555&permissions=8&redirect_uri=https%3A%2F%2Farea-junaifu-front.wonderful-goose-4.telebit.io%2Fservices%3Fstate%3DDiscord&response_type=code&scope=bot%20identify%20email%20guilds%20guilds.join&response_type=code")))
              }));
    } else if (name == "Twilio") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "Twilio")
              .isEmpty),
          icon: MaterialIcons.sms,
          color: Colors.red,
          callback: () {
            showDialog(
              context: context,
              builder: (BuildContext context) => popUpCreateServicecDialog(
                  context,
                  "Twilio Authentification",
                  "Enter your phone number, it needs 10 numbers",
                  twilioCreateService),
            );
          }));
    } else if (name == "SendGrid") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "SendGrid")
              .isEmpty),
          icon: MaterialIcons.email,
          color: Colors.black,
          callback: () async {
            try {
              await sendgridCreateService();
              Navigator.pushReplacement(
                  context,
                  MaterialPageRoute(
                      builder: (BuildContext context) => ConfirmationPage(
                          "SendGrid successfully subscribed.", "/home")));
            } catch (error) {
              Text("Error occured");
            }
          }));
    } else if (name == "GitHub") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "GitHub")
              .isEmpty),
          icon: MaterialCommunityIcons.github_box,
          color: Colors.black,
          callback: () => {
                Navigator.of(context).push(MaterialPageRoute(
                    builder: (BuildContext context) => AreaWebView(
                        "GitHub Authentification",
                        "https://github.com/login/oauth/authorize?client_id=0123714cfdd44c79b26d&redirect_uri=https://area-junaifu-front.wonderful-goose-4.telebit.io/services?state=GitHub&response_type=code&scope=user,repo,admin:repo_hook,delete_repo"
                            "https://github.com/login/oauth/authorize?client_id=0123714cfdd44c79b26d&redirect_uri=https://area-junaifu-front.wonderful-goose-4.telebit.io/services?state=GitHub&response_type=code&scope=user,repo,admin:repo_hook,delete_repo")))
              } // TODO: change link to FrontWeb OAuth2 of GitHub
          ));
    } else if (name == "Slack") {
      services.add(ServiceInfo(
          name: name,
          isAvailable: !(servicesNotSubscribed
              .where((service) => service == "Slack")
              .isEmpty),
          icon: MaterialCommunityIcons.slack,
          color: Colors.black,
          callback: () => {
                Navigator.of(context).push(MaterialPageRoute(
                    builder: (BuildContext context) => AreaWebView(
                        "Slack Authentification",
                        "https://slack.com/oauth/v2/authorize?client_id=1797353003221.1797494244069&scope=incoming-webhook,app_mentions:read,chat:write,chat:write.customize,files:read,files:write&state=Slack&redirect_uri=https://area-junaifu-front.wonderful-goose-4.telebit.io/services")))
              } // TODO: change link to FrontWeb OAuth2 of Slack
          ));
    }
  }
  return services;
}

class DisplayServices extends StatefulWidget {
  // final List<String> servicesName;
  final List<ServiceInfo> allServices;
  DisplayServices({Key key, @required this.allServices}) : super(key: key);
  @override
  DisplayServicesState createState() => DisplayServicesState();
}

class DisplayServicesState extends State<DisplayServices> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      // Peut etre je dois centrer
      body: SingleChildScrollView(
        child: Column(
          children: [
            for (var elem in widget.allServices)
              ServiceCard(
                title: elem.name,
                isAvailable: elem.isAvailable,
                icon: elem.icon,
                color: elem.color,
                callback: elem.callback,
              ),
          ],
        ),
      ),
    );
  }
}

class ServiceCard extends StatefulWidget {
  final String title;
  final IconData icon;
  final Color color;
  final bool isAvailable;
  Function callback;

  // final bool isLinked;
  ServiceCard(
      {Key key,
      @required this.title,
      @required this.isAvailable,
      @required this.icon,
      @required this.color,
      @required this.callback})
      : super(key: key);

  @override
  ServiceCardState createState() => ServiceCardState();
}

class ServiceCardState extends State<ServiceCard> {
  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.all(10.0),
      height: 200,
      width: double.maxFinite,
      child: Card(
        elevation: 10,
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceEvenly,
          children: <Widget>[
            Container(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: <Widget>[
                  Container(
                    child: Text(
                      widget.title,
                      style: TextStyle(
                        color: Colors.black,
                        fontSize: 25,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ),
                  Container(
                    child: Icon(
                      widget.icon,
                      color: widget.color,
                      size: 100,
                    ),
                  ),
                ],
              ),
            ),
            Container(
              child: RaisedButton(
                onPressed: widget.isAvailable ? widget.callback : null,
                color: widget.isAvailable
                    ? Theme.of(context).accentColor
                    : Colors.greenAccent,
                elevation: 10,
                textColor: Colors.black,
                child: Text(
                  widget.isAvailable ? "Subscribe" : "Subscribed",
                  style: TextStyle(fontSize: 20),
                ),
              ),
            )
          ],
        ),
      ),
    );
  }
}

extension ColorExtension on String {
  toColor() {
    var hexColor = this.replaceAll("#", "");
    if (hexColor.length == 6) {
      hexColor = "FF" + hexColor;
    }
    if (hexColor.length == 8) {
      return Color(int.parse("0x$hexColor"));
    }
  }
}
