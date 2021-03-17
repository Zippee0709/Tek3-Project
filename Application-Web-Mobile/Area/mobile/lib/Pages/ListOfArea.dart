import 'package:flutter/material.dart';
import 'package:flutter_icons/flutter_icons.dart';
import 'package:mobile/Service/AreaServices.dart';
import 'package:mobile/Models/ServiceModel.dart';

class ListOfArea extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    // return _ListOfArea();
    return FutureBuilderGetUserServices();
  }
}

// Futurebuilder for get all services where user are subscribed
class FutureBuilderGetUserServices extends State<ListOfArea> {
  @override
  Widget build(BuildContext context) {
    return FutureBuilder<List<UserAreaInfo>>(
      future: fetchUserArea(),
      builder: (context, snapshot) {
        List<Widget> children;
        if (snapshot.hasData) {
          return DisplayUserServices(
              allArea: genAreaInfoByName(snapshot.data, context));
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
}

// Init les infos des Area
List<UserAreaInfo> genAreaInfoByName(
    List<UserAreaInfo> userArea, BuildContext context) {
  // Parser les infos via le nom du service
  List<UserAreaInfo> userFinalArea = new List<UserAreaInfo>();
  for (var area in userArea) {
    userFinalArea.add(new UserAreaInfo(
      areaId: area.areaId,
      action: genInfoByName(area.action, context),
      reaction: genInfoByName(area.reaction, context),
    ));
  }
  return userFinalArea;
}

// Init les infos d'un Area
AreaInfo genInfoByName(AreaInfo area, BuildContext context) {
  // Parser les infos via le nom du service
  AreaInfo newArea;
  if (area.name == "GitLab") {
    newArea = AreaInfo(
      name: area.name,
      icon: MaterialCommunityIcons.gitlab,
      color: Colors.orange,
      actionType: area.actionType,
    );
  } else if (area.name == "Trello") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialCommunityIcons.trello,
      color: Colors.blue,
      actionType: area.actionType,
    ));
  } else if (area.name == "Discord") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialCommunityIcons.discord,
      color: Colors.purple,
      actionType: area.actionType,
    ));
  } else if (area.name == "Twilio") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialIcons.sms,
      color: Colors.red,
      actionType: area.actionType,
    ));
  } else if (area.name == "SendGrid") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialIcons.email,
      color: Colors.black,
      actionType: area.actionType,
    ));
  } else if (area.name == "GitHub") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialCommunityIcons.github_box,
      color: Colors.black,
      actionType: area.actionType,
    ));
  } else if (area.name == "Slack") {
    newArea = (AreaInfo(
      name: area.name,
      icon: MaterialCommunityIcons.slack,
      color: Colors.black,
      actionType: area.actionType,
    ));
  }
  return newArea;
}

class DisplayUserServices extends StatefulWidget {
  final List<UserAreaInfo> allArea;
  DisplayUserServices({Key key, @required this.allArea}) : super(key: key);
  @override
  DisplayUserServicesState createState() =>
      DisplayUserServicesState(this.allArea);
}

class DisplayUserServicesState extends State<DisplayUserServices> {
  List<UserAreaInfo> allArea;

  DisplayUserServicesState(this.allArea);

  void refresh(String areaId) {
    setState(() {
      allArea.removeWhere((area) => area.areaId == areaId);
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: SingleChildScrollView(
          child: widget.allArea.length > 0
              ? Column(
                  children: [
                    for (var area in widget.allArea)
                      AreaWidget(area: area, refreshParent: refresh)
                  ],
                )
              : Container(
                  padding: EdgeInsets.only(top: 300),
                  child: Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: <Widget>[
                      Icon(
                        Icons.error_outline,
                        color: Colors.red,
                        size: 60,
                      ),
                      Text('User don\'t have area, please create area first'),
                    ],
                  ),
                )),
    );
  }
}

class AreaWidget extends StatefulWidget {
  final UserAreaInfo area;
  final void Function(String areaId) refreshParent;

  AreaWidget({Key key, @required this.area, @required this.refreshParent})
      : super(key: key);
  @override
  AreaWidgetState createState() => AreaWidgetState();
}

class AreaWidgetState extends State<AreaWidget> {
  @override
  Widget build(BuildContext context) {
    return Container(
      margin: EdgeInsets.all(5),
      height: 260,
      width: double.maxFinite,
      decoration: BoxDecoration(
        borderRadius: BorderRadius.circular(10),
        color: Colors.white,
      ),
      child: Stack(
        children: <Widget>[
          Card(
            elevation: 10,
            child: Column(
              // mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: <Widget>[
                Expanded(child: areaInfoCard(widget.area.action)),
                Icon(
                  MaterialCommunityIcons.arrow_down_circle_outline,
                  color: Colors.green,
                  size: 30,
                ),
                Expanded(child: areaInfoCard(widget.area.reaction)),
              ],
            ),
          ),
          Positioned(
            top: 0,
            right: 0,
            child: IconButton(
              icon: Icon(MaterialCommunityIcons.delete),
              tooltip: 'Delete area',
              onPressed: () async {
                await fetchDeleteArea(widget.area.areaId);
                widget.refreshParent(widget.area.areaId);
              },
            ),
          ),
        ],
      ),
    );
  }
}

Widget areaInfoCard(data) {
  return Column(
    children: <Widget>[
      Container(child: serviceInfoCard(data)),
      Container(
        padding: EdgeInsets.all(10),
        child: Text(
          data.actionType,
          style: TextStyle(color: Colors.black, fontSize: 20),
        ),
      ),
    ],
  );
}

Widget serviceInfoCard(data) {
  return Container(
    child: Row(
      mainAxisAlignment: MainAxisAlignment.center,
      children: <Widget>[
        Container(
          padding: EdgeInsets.all(10),
          child: Icon(data.icon, color: data.color, size: 40),
        ),
        Container(
          child: Text(
            data.name,
            style: TextStyle(
              color: Colors.black,
              fontSize: 25,
              fontWeight: FontWeight.bold,
            ),
          ),
        ),
        Container(
          padding: EdgeInsets.all(10),
          child: Icon(data.icon, color: Colors.transparent, size: 40),
        ),
      ],
    ),
  );
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
