import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Store/Store.dart';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Models/ListActions.dart';
import 'package:mobile/Models/ListReactions.dart';
import 'package:mobile/Models/UserServiceModel.dart';
import 'package:mobile/Models/ServiceModel.dart';

Future<dynamic> fetchAllServices() async {
  final userToken = await getStringValue("userToken");
  List<String> servicesName = List<String>();

  final http.Response response = await http.get(
    serverLink + '/service/getServices',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    var userServicesName = jsonDecode(response.body)["servicesList"][0]["user"]
        .map((x) => x["name"])
        .toList();
    var allServices = jsonDecode(response.body)["servicesList"][1]["server"];

    var serviceNotSubscribed = allServices
        .where((element) => !userServicesName.contains(element))
        .toList();

    return {"available": serviceNotSubscribed, "allServices": allServices};
  } else {
    throw Exception('Get Services failed');
  }
}

Future<List<UserService>> fetchUserServices() async {
  final userToken = await getStringValue("userToken");
  List<UserService> userServicesName = List<UserService>();

  final http.Response response = await http.get(
    serverLink + '/service/getServices',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    for (var userService in jsonDecode(response.body)["servicesList"][0]
        ["user"]) {
      userServicesName.add(UserService.fromJson(userService));
    }
    return userServicesName;
  } else {
    throw Exception('Get Services failed');
  }
}

Future<List<UserAreaInfo>> fetchUserArea() async {
  final userToken = await getStringValue("userToken");

  // Parse les données des tous les services dont l'user s'est abonnées
  List<UserService> userServices = await fetchUserServices();

  if (userServices.length <= 0 || userServices == null)
    throw Exception('User don\'t have subscribed service');

  List<UserAreaInfo> userListArea = new List<UserAreaInfo>();

  // Start to get area for each subscribed service
  for (var service in userServices) {
    final http.Response areaResponse = await http.get(
      serverLink + '/service/getServicesDetails/' + service.id,
      headers: <String, String>{
        'Content-Type': 'application/json; charset=UTF-8',
        'Authorization': 'Bearer ' + userToken,
      },
    );

    // Pas de else car l'user peut avoir un subscribed service avec des area
    // et un subscribed service sans area
    if (areaResponse.statusCode == 200) {
      // Start to save each area for a subscribed service
      for (var userArea in json.decode(areaResponse.body)["areaLinks"]) {
        userListArea.add(UserAreaInfo(
          areaId: userArea["id"],
          action: AreaInfo(
            name: userArea["serviceActionName"],
            actionType: userArea["serviceActionType"],
          ),
          reaction: AreaInfo(
            name: userArea["serviceReactionName"],
            actionType: userArea["serviceReactionType"],
          ),
        ));
      }
    }
  }
  return userListArea;
}

Future<String> fetchDeleteArea(String areaId) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.delete(
    serverLink + "/area/deleteArea/" + areaId,
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return 'Fetch delete area success';
  } else {
    throw Exception('Fetch delete area failed');
  }
}

Future<ListActions> fetchActions(
    String firstService, String secondService) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink +
        '/service/getServices/action/' +
        firstService +
        '/' +
        secondService,
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );
  var test = ListActions.fromJson(
      jsonDecode(response.body), firstService, secondService);
  if (response.statusCode == 200) {
    return test;
  } else {
    throw Exception('Fetch actions failed');
  }
}

Future<ListReactions> fetchReactions(
    String firstService, String secondService) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink +
        '/service/getServices/reaction/' +
        firstService +
        '/' +
        secondService,
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );
  var test = ListReactions.fromJson(
      jsonDecode(response.body), firstService, secondService);
  if (response.statusCode == 200) {
    return test;
  } else {
    throw Exception('Fetch actions failed');
  }
}

Future<List<String>> setServicesLink(serviceActionName, serviceActionId,
    actionType, serviceReactionName, serviceReactionId, reactionType,
    [title]) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.post(
    serverLink + '/area/setLink',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
    body: jsonEncode(<String, String>{
      "serviceActionName": serviceActionName,
      "serviceActionId": serviceActionId,
      "actionType": actionType,
      "serviceReactionName": serviceReactionName,
      "serviceReactionId": serviceReactionId,
      "reactionType": reactionType,
      "title": title ?? "Excalibur"
    }),
  );

  if (response.statusCode == 200) {
    return null;
  } else {
    throw Exception('Discord set service failed');
  }
}
