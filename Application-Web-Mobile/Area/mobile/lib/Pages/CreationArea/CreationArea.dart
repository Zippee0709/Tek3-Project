import 'package:flutter/material.dart';
import 'package:mobile/Service/AreaServices.dart';
import 'package:smart_select/smart_select.dart';
import 'package:mobile/Models/ListActions.dart';
import 'package:mobile/Models/ListReactions.dart';
import 'package:flutter_icons/flutter_icons.dart';
import 'package:mobile/Models/UserServiceModel.dart';
import 'package:mobile/Pages/CreationArea/ConfirmationCreationArea.dart';
import 'package:mobile/Models/ServiceModel.dart';

class CreationArea extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return _CreationArea();
  }
}

class _CreationArea extends State<CreationArea> {
  // Future<List<String>> userSubscribedServices;
  S2SingleState firstSelectedService, secondSelectedService;
  S2SingleState actionSelected, reactionSelected;
  // Future<ListActions> servicesActions;
  List<S2Choice<UserServiceGroup>> userSubscribedServices;
  List<S2Choice<ServiceGroup>> servicesActions;
  List<S2Choice<ServiceGroup>> servicesReactions;
  // var servicesIcon = const <List, IconData>{
  //   const ["Trello"]: MaterialCommunityIcons.trello,
  // };

  Map<String, IconData> servicesIcon = {
    "Trello": MaterialCommunityIcons.trello,
    "GitLab": MaterialCommunityIcons.gitlab,
    "GitHub": MaterialCommunityIcons.github_box,
    "Discord": MaterialCommunityIcons.discord,
    "Slack": MaterialCommunityIcons.slack,
    "SendGrid": MaterialIcons.email,
    "Twilio": MaterialIcons.sms,
  };

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: Center(
        child: Container(
          margin: EdgeInsets.all(10.0),
          child: Column(
            children: <Widget>[
              Card(
                elevation: 10,
                child: Column(children: <Widget>[
                  SizedBox(
                    height: 40,
                  ),
                  Text(
                    "Create your own workflow !",
                    style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
                  ),
                  Container(
                    margin: EdgeInsets.only(top: 10.0),
                    child: Text(
                      "Know exactly what you want to build? Select the apps you want to connect to start your custom setup.",
                      style: TextStyle(color: Colors.grey),
                      textAlign: TextAlign.center,
                    ),
                  ),
                  FutureBuilder<List<UserService>>(
                    future: fetchUserServices(),
                    builder: (BuildContext context,
                        AsyncSnapshot<List<UserService>> snapshot) {
                      if (!snapshot.hasData) {
                        return Center(
                          child: CircularProgressIndicator(),
                        );
                      } else {
                        userSubscribedServices = [
                          for (var el in snapshot.data)
                            S2Choice<UserServiceGroup>(
                              value: UserServiceGroup(el, el.name),
                              title: el.name,
                            )
                        ];
                        return Column(children: <Widget>[
                          serviceSmartSelectBox(
                            0,
                            userSubscribedServices,
                            "Connect this app...",
                          ),
                          serviceSmartSelectBox(
                            1,
                            userSubscribedServices,
                            "with this one!",
                          ),
                        ]);
                      }
                    },
                  ),
                  if (firstSelectedService != null &&
                      secondSelectedService != null)
                    Column(
                      children: <Widget>[
                        FutureBuilder<ListActions>(
                          future: fetchActions(
                              firstSelectedService.value.item.name,
                              secondSelectedService.value.item.name),
                          builder: (BuildContext context,
                              AsyncSnapshot<ListActions> snapshot) {
                            if (!snapshot.hasData) {
                              return Center(
                                child: CircularProgressIndicator(),
                              );
                            } else {
                              servicesActions = [
                                for (var el in snapshot.data.firstActions)
                                  S2Choice<ServiceGroup>(
                                      value: ServiceGroup(
                                        el,
                                        firstSelectedService.value.group,
                                      ),
                                      title: el),
                                for (var el in snapshot.data.secondActions)
                                  S2Choice<ServiceGroup>(
                                    value: ServiceGroup(
                                      el,
                                      secondSelectedService.value.group,
                                    ),
                                    title: el,
                                  )
                              ];
                              return actionReactionSmartSelectBox(true,
                                  servicesActions, "When this happens...");
                            }
                          },
                        ),
                        FutureBuilder<ListReactions>(
                          future: fetchReactions(
                              firstSelectedService.value.item.name,
                              secondSelectedService.value.item.name),
                          builder: (BuildContext context,
                              AsyncSnapshot<ListReactions> snapshot) {
                            if (!snapshot.hasData) {
                              return Center(
                                child: CircularProgressIndicator(),
                              );
                            } else {
                              servicesReactions = [
                                for (var el in snapshot.data.firstReactions)
                                  S2Choice<ServiceGroup>(
                                    value: ServiceGroup(
                                        el, firstSelectedService.value.group),
                                    title: el,
                                  ),
                                for (var el in snapshot.data.secondReactions)
                                  S2Choice<ServiceGroup>(
                                    value: ServiceGroup(
                                        el, secondSelectedService.value.group),
                                    title: el,
                                  )
                              ];
                              return actionReactionSmartSelectBox(
                                  false, servicesReactions, "then do this!");
                            }
                          },
                        ),
                      ],
                    ),
                  if (reactionSelected != null && actionSelected != null)
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
                                Text('Make an AREA',
                                    style: TextStyle(fontSize: 20)),
                                Icon(
                                  Icons.arrow_right_alt,
                                  size: 45,
                                ),
                              ]),
                          onPressed: () {
                            Navigator.push(
                                context,
                                MaterialPageRoute(
                                    builder: (context) =>
                                        ConfirmationCreationArea(
                                          firstSelectedService:
                                              firstSelectedService.value.item,
                                          secondSelectedService:
                                              secondSelectedService.value.item,
                                          selectedAction: actionSelected.value,
                                          selectedReaction:
                                              reactionSelected.value,
                                        )));
                          },
                        )),
                  SizedBox(
                    height: 40,
                  ),
                ]),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget actionReactionSmartSelectBox(
      bool isAction, List<S2Choice<ServiceGroup>> choiceItems, String title) {
    return SmartSelect.single(
        modalType: S2ModalType.bottomSheet,
        title: title,
        placeholder: "Select an app",
        value: isAction
            ? actionSelected
            : reactionSelected, // HACK: Need to do this ternary, can't refacto
        choiceItems: choiceItems,
        choiceLayout: S2ChoiceLayout.wrap,
        choiceDirection: Axis.vertical,
        choiceBuilder: (ctx, state, choice) {
          return Card(
            margin: const EdgeInsets.fromLTRB(0, 5, 0, 5),
            // color: Theme.of(context).accentColor,
            child: InkWell(
              onTap: () => state.select(true),
              child: SizedBox(
                width: double.infinity,
                height: 50,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.start,
                  crossAxisAlignment: CrossAxisAlignment.center,
                  children: <Widget>[
                    Icon(servicesIcon[state.value.group],
                        size: 35, color: Theme.of(context).accentColor),
                    const SizedBox(width: 15),
                    Text(
                      state.value.item,
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
              isTwoLine: true,
              value: state.value?.item,
              leading: Icon(
                servicesIcon[state.value?.group],
                size: 55,
                color: Theme.of(context).accentColor,
              ));
        },
        onChange: (value) => setState(() {
              value = value;
              isAction
                  ? actionSelected = value
                  : reactionSelected =
                      value; // HACK: Need to do this ternary, can't refacto
            }));
  }

  Widget serviceSmartSelectBox(
      int type, List<S2Choice<UserServiceGroup>> choiceItems, String title) {
    return SmartSelect.single(
        modalType: S2ModalType.bottomSheet,
        title: title,
        placeholder: "Select an app",
        value: type == 0
            ? firstSelectedService
            : secondSelectedService, // HACK: Need to do this ternary, can't refacto
        choiceItems: choiceItems,
        choiceLayout: S2ChoiceLayout.wrap,
        choiceDirection: Axis.horizontal,
        choiceEmptyBuilder: (ctx, value) {
          return Container(
            padding: EdgeInsets.all(55),
            child: Text(
              "You don't have any services.\n" +
                  "Subscribe to services first ! ",
              style: TextStyle(fontSize: 20),
            ),
          );
        },
        choiceBuilder: (ctx, state, choice) {
          return Card(
            margin: const EdgeInsets.fromLTRB(0, 5, 0, 5),
            color: Theme.of(context).accentColor,
            child: InkWell(
              onTap: () => state.select(true),
              child: SizedBox(
                width: 100,
                height: 100,
                child: Center(
                  child: Column(
                    mainAxisAlignment: MainAxisAlignment.center,
                    crossAxisAlignment: CrossAxisAlignment.center,
                    children: <Widget>[
                      Icon(servicesIcon[state.value.group], size: 55),
                      const SizedBox(height: 5),
                      Text(
                        state.title,
                        style: TextStyle(
                            color: Colors.white,
                            fontSize: 20,
                            fontWeight: FontWeight.bold),
                      ),
                    ],
                  ),
                ),
              ),
            ),
          );
        },
        tileBuilder: (context, state) {
          return S2Tile.fromState(state,
              value: state.value?.item?.name,
              isTwoLine: true,
              leading: Icon(
                servicesIcon[state.value?.group],
                size: 55,
                color: Theme.of(context).accentColor,
              ));
        },
        onChange: (value) => setState(() {
              value = value;
              type == 0
                  ? firstSelectedService = value
                  : secondSelectedService =
                      value; // HACK: Need to do this ternary, can't refacto
            }));
  }
}
