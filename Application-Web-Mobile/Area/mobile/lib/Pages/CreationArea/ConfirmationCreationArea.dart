import 'package:flutter/material.dart';
import 'package:flutter_icons/flutter_icons.dart';
import 'package:mobile/Models/UserServiceModel.dart';
import 'package:mobile/Models/ServiceModel.dart';
import 'package:mobile/Pages/ConfirmationPage.dart';
import 'package:mobile/Service/AreaServices.dart';
import 'package:mobile/Service/DiscordService.dart';
import 'package:mobile/Service/DumbService.dart';
import 'package:mobile/Service/GitHubService.dart';
import 'package:mobile/Service/GitLabService.dart';
import 'package:mobile/Service/SendGridService.dart';
import 'package:mobile/Service/SlackService.dart';
import 'package:mobile/Tools/Snackbar.dart';
import 'package:smart_select/smart_select.dart';
import 'package:mobile/Service/TrelloService.dart';

class ConfirmationCreationArea extends StatefulWidget {
  final UserService firstSelectedService;
  final UserService secondSelectedService;
  final ServiceGroup selectedAction;
  final ServiceGroup selectedReaction;

  ConfirmationCreationArea(
      {Key key,
      @required this.firstSelectedService,
      @required this.secondSelectedService,
      @required this.selectedAction,
      @required this.selectedReaction})
      : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _ConfirmationCreationArea(this.firstSelectedService,
        this.secondSelectedService, this.selectedAction, this.selectedReaction);
  }
}

class _ConfirmationCreationArea extends State<ConfirmationCreationArea> {
  final UserService firstSelectedService;
  final UserService secondSelectedService;
  final ServiceGroup selectedAction;
  final ServiceGroup selectedReaction;

  _ConfirmationCreationArea(this.firstSelectedService,
      this.secondSelectedService, this.selectedAction, this.selectedReaction);

  Map<String, IconData> servicesIcon = {
    "Trello": MaterialCommunityIcons.trello,
    "GitLab": MaterialCommunityIcons.gitlab,
    "GitHub": MaterialCommunityIcons.github_box,
    "Discord": MaterialCommunityIcons.discord,
    "Slack": MaterialCommunityIcons.slack,
    "SendGrid": MaterialIcons.email,
    "Twilio": MaterialIcons.sms,
  };

  Map<String, dynamic> dataAction = {
    "TrelloBoard": null,
    "Trello": null,
    "GitLab": null,
    "GitHub": null,
    "GitHubUsername": null,
    "DiscordServer": null,
    "Discord": null,
    "Slack": null,
    "SendGrid": null,
    "Twilio": null,
  };

  Map<String, dynamic> dataReaction = {
    "TrelloBoard": null,
    "Trello": null,
    "GitLab": null,
    "GitHub": null,
    "GitHubUsername": null,
    "DiscordServer": null,
    "Discord": null,
    "Slack": null,
    "SendGridRecipient": null,
    "SendGridObject": null,
    "SendGrid": null,
    "Twilio": null,
  };

  Map<String, dynamic> setServiceCallback = {
    "Trello": trelloSetService,
    "GitLab": gitlabSetService,
    "GitHub": githubSetService,
    "Discord": discordSetService,
    "Slack": dumbSetService,
    "SendGrid": sendGridSetService,
    "Twilio": dumbSetService,
  };

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text("Settings AREA"),
        backgroundColor: Theme.of(context).primaryColor,
        centerTitle: true,
      ),
      backgroundColor: Colors.white,
      body: Builder(
        builder: (contextScaffold) => Center(
          child: ListView(shrinkWrap: true, children: <Widget>[
            Container(
              margin: EdgeInsets.all(10.0),
              child: Column(
                children: <Widget>[
                  actionCard(context),
                  reactionCard(context),
                  if (dataAction[selectedAction.group] != null &&
                      (dataReaction[selectedReaction.group] != null ||
                          selectedReaction.group == "Twilio"))
                    Container(
                        height: 80,
                        width: double.infinity,
                        padding: EdgeInsets.fromLTRB(10, 15, 10, 0),
                        child: RaisedButton(
                          textColor: Colors.white,
                          color: Theme.of(context).accentColor,
                          child: Row(
                              mainAxisAlignment: MainAxisAlignment.center,
                              children: <Widget>[
                                Text('Validate AREA',
                                    style: TextStyle(fontSize: 20)),
                                Icon(
                                  Icons.arrow_right_alt,
                                  size: 45,
                                ),
                              ]),
                          onPressed: () async {
                            try {
                              var actionResponse = await setServiceCallback[
                                  selectedAction.group](dataAction);
                              var reactionResponse = await setServiceCallback[
                                  selectedReaction.group](dataReaction);
                              setServicesLink(
                                  selectedAction.group,
                                  selectedAction.group == "Slack"
                                      ? dataAction["Slack"]["id"]
                                      : actionResponse["_id"],
                                  selectedAction.item,
                                  selectedReaction.group,
                                  selectedReaction.group == "Slack"
                                      ? dataReaction["Slack"]["id"]
                                      : reactionResponse["_id"],
                                  selectedReaction.item);
                              Navigator.pushReplacement(
                                  context,
                                  MaterialPageRoute(
                                      builder: (BuildContext context) =>
                                          ConfirmationPage(
                                              "Your AREA has been succesfully created.",
                                              "/home")));
                            } catch (error) {
                              print("ERROR OCCURED");
                              showError(contextScaffold, error.toString());
                            }
                          },
                        )),
                  SizedBox(
                    height: 40,
                  ),
                ],
              ),
            ),
          ]),
        ),
      ),
    );
  }

  Widget actionCard(BuildContext context) {
    return Card(
      elevation: 10,
      child: Column(children: <Widget>[
        SizedBox(
          height: 40,
        ),
        Text(
          "Action",
          style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
        ),
        Container(
          margin: EdgeInsets.only(top: 10.0),
          child: Row(
              crossAxisAlignment: CrossAxisAlignment.center,
              mainAxisAlignment: MainAxisAlignment.spaceAround,
              mainAxisSize: MainAxisSize.max,
              children: <Widget>[
                Icon(
                  servicesIcon[selectedAction.group],
                  size: 50,
                  color: Theme.of(context).accentColor,
                ),
                SizedBox(
                  width: 25,
                ),
                Text(
                  selectedAction.item,
                  style: TextStyle(color: Colors.black, fontSize: 20),
                  textAlign: TextAlign.center,
                ),
              ]),
        ),
        Divider(
          height: 20,
          color: Colors.black,
          thickness: 0.5,
        ),
        if (selectedAction.group == "Trello") ...trelloWidgets(true),
        if (selectedAction.group == "GitHub") ...githubWidgets(true),
        if (selectedAction.group == "GitLab") ...gitlabWidgets(true),
        if (selectedAction.group == "Slack") ...slackWidgets(true),
        SizedBox(
          height: 40,
        ),
      ]),
    );
  }

  Widget reactionCard(BuildContext context) {
    return Card(
      elevation: 10,
      child: Column(children: <Widget>[
        SizedBox(
          height: 40,
        ),
        Text(
          "Reaction",
          style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
        ),
        Container(
          margin: EdgeInsets.only(top: 10.0),
          child: Row(
              crossAxisAlignment: CrossAxisAlignment.center,
              mainAxisAlignment: MainAxisAlignment.spaceAround,
              mainAxisSize: MainAxisSize.max,
              children: <Widget>[
                Icon(
                  servicesIcon[selectedReaction.group],
                  size: 50,
                  color: Theme.of(context).accentColor,
                ),
                SizedBox(
                  width: 25,
                ),
                Text(
                  selectedReaction.item,
                  style: TextStyle(color: Colors.black, fontSize: 20),
                  textAlign: TextAlign.center,
                ),
              ]),
        ),
        Divider(
          height: 20,
          color: Colors.black,
          thickness: 0.5,
        ),
        if (selectedReaction.group == "Discord") ...discordWidgets(false),
        if (selectedReaction.group == "Trello") ...trelloWidgets(false),
        if (selectedReaction.group == "SendGrid") ...sendgridWidgets(false),
        if (selectedReaction.group == "GitHub") ...githubWidgets(false),
        if (selectedReaction.group == "GitLab") ...gitlabWidgets(false),
        if (selectedReaction.group == "Slack") ...slackWidgets(false),
        SizedBox(
          height: 40,
        ),
      ]),
    );
  }

  //                        TRELLO WIDGETS
  List<Widget> trelloWidgets(isAction) {
    return [
      dropdownBoxCustom(
          trelloGetBoards, "TrelloBoard", "Board", "Which board ?", isAction),
      if (isAction
          ? dataAction["TrelloBoard"] != null
          : dataReaction["TrelloBoard"] != null)
        dropdownBoxCustom(
            trelloGetListOfBoards,
            isAction ? selectedAction.group : selectedReaction.group,
            "List of board",
            "Which list from your board ?",
            isAction,
            isAction
                ? dataAction["TrelloBoard"]["id"]
                : dataReaction["TrelloBoard"]["id"]),
    ];
  }

  //                        GITHUB WIDGETS
  List<Widget> githubWidgets(isAction) {
    return [
      dropdownBoxCustom(
          githubGetUserRepo,
          isAction ? selectedAction.group : selectedReaction.group,
          "Repository",
          "Which repository ?",
          isAction),
    ];
  }

  //                        GITLAB WIDGETS
  List<Widget> gitlabWidgets(isAction) {
    return [
      dropdownBoxCustom(
          gitlabGetUserRepo,
          isAction ? selectedAction.group : selectedReaction.group,
          "Repository",
          "Which repository ?",
          isAction),
    ];
  }

  //                        DISCORD WIDGETS
  List<Widget> discordWidgets(isAction) {
    return [
      dropdownBoxCustom(discordGetServers, "DiscordServer", "Servers",
          "Which server ?", isAction, null, "serverId", "serverName"),
      if (isAction
          ? dataAction["DiscordServer"] != null
          : dataReaction["DiscordServer"] != null)
        dropdownBoxCustom(
            discordGetChannels,
            selectedReaction.group,
            "Channels",
            "Which channels ?",
            isAction,
            isAction
                ? dataAction["DiscordServer"]["id"]
                : dataReaction["DiscordServer"]["id"]),
    ];
  }

  //                        SendGrid WIDGETS
  List<Widget> sendgridWidgets(isAction) {
    return [
      textInputCustom(60.0, 'Enter the recipient here', 1, "SendGridRecipient"),
      if (dataReaction["SendGridRecipient"] != null)
        textInputCustom(60.0, 'Enter the object here', 1, "SendGridObject"),
      if (dataReaction["SendGridObject"] != null)
        textInputCustom(150.0, 'Enter the body here', 100, "SendGrid")
    ];
  }

  //                        SLACK WIDGETS
  List<Widget> slackWidgets(isAction) {
    return [
      dropdownBoxCustom(slackGetRegisteredTeam, "Slack", "Team", "Which team ?",
          isAction, null, "idService", "serverName"),
    ];
  }

  //                        CUSTOM WIDGET
  Widget textInputCustom(height, labelText, maxLine, serviceName) {
    return Padding(
      padding: EdgeInsets.fromLTRB(20, 20, 20, 0),
      child: Container(
        height: height,
        alignment: Alignment.center,
        padding: EdgeInsets.fromLTRB(0, 0, 0, 0),
        child: new Scrollbar(
          child: new SingleChildScrollView(
            scrollDirection: Axis.vertical,
            child: SizedBox(
              height: height,
              child: new TextField(
                maxLines: maxLine,
                decoration: new InputDecoration(
                  border: OutlineInputBorder(),
                  labelText: labelText,
                ),
                onChanged: (value) {
                  setState(() {
                    dataReaction[serviceName] = value;
                  });
                },
              ),
            ),
          ),
        ),
      ),
    );
  }

  //                        DROPDOWNBOXCUSTOM
  FutureBuilder dropdownBoxCustom(
      futureFunction, serviceName, title, placeholder, isAction,
      [functionParam, valueIndex, titleIndex]) {
    return FutureBuilder<List<dynamic>>(
      future: functionParam != null
          ? futureFunction(functionParam)
          : futureFunction(),
      builder: (BuildContext context, AsyncSnapshot<List<dynamic>> snapshot) {
        if (!snapshot.hasData) {
          return Center(
            child: CircularProgressIndicator(),
          );
        } else {
          return Column(children: <Widget>[
            SmartSelect.single(
                modalType: S2ModalType.bottomSheet,
                title: title,
                placeholder: placeholder,
                value: isAction
                    ? dataAction[serviceName]
                    : dataReaction[serviceName],
                choiceItems: S2Choice.listFrom(
                  source: snapshot.data,
                  value: (index, item) {
                    if (serviceName == "GitHub") {
                      isAction
                          ? dataAction["GitHubUsername"] =
                              item["owner"]["login"]
                          : dataReaction["GitHubUsername"] =
                              item["owner"]["login"];
                    }
                    return item[valueIndex ?? 'id'];
                  },
                  title: (index, item) => item[titleIndex ?? 'name'],
                ),
                choiceLayout: S2ChoiceLayout.wrap,
                choiceDirection: Axis.vertical,
                choiceBuilder: (ctx, state, choice) {
                  return Card(
                    margin: const EdgeInsets.fromLTRB(0, 5, 0, 5),
                    child: InkWell(
                      onTap: () => state.select(true),
                      child: SizedBox(
                        width: double.infinity,
                        height: 50,
                        child: Row(
                          mainAxisAlignment: MainAxisAlignment.start,
                          crossAxisAlignment: CrossAxisAlignment.center,
                          children: <Widget>[
                            Icon(
                                servicesIcon[isAction
                                    ? selectedAction.group
                                    : selectedReaction.group],
                                size: 35,
                                color: Theme.of(context).accentColor),
                            const SizedBox(width: 15),
                            Text(
                              state.title,
                              style: TextStyle(
                                color: Colors.black,
                                fontSize: 25,
                              ),
                            ),
                          ],
                        ),
                      ),
                    ),
                  );
                },
                tileBuilder: (context, state) {
                  return S2Tile.fromState(state,
                      enabled: !(serviceName == "GitHub" &&
                          !isAction &&
                          selectedReaction.item == "Create repo"),
                      isTwoLine: true,
                      value: state.valueTitle,
                      leading: Icon(
                        servicesIcon[isAction
                            ? selectedAction.group
                            : selectedReaction.group],
                        size: 55,
                        color: Theme.of(context).accentColor,
                      ));
                },
                onChange: (value) => setState(() {
                      if (serviceName == "GitHub") {
                        isAction
                            ? dataAction[serviceName] = {
                                "repoName": value.valueTitle,
                                "event": selectedAction.item,
                              }
                            : dataReaction[serviceName] = {
                                "repoName": value.valueTitle,
                                "event": selectedReaction.item
                              };
                      } else {
                        isAction
                            ? dataAction[serviceName] = {
                                "name": value.valueTitle,
                                "id": value.value
                              }
                            : dataReaction[serviceName] = {
                                "name": value.valueTitle,
                                "id": value.value
                              };
                      }
                    })),
          ]);
        }
      },
    );
  }
}
