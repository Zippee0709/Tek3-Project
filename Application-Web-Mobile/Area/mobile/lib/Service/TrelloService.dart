import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> trelloSetService(data) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.post(
    serverLink + '/service/Trello/setService',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
    body: jsonEncode(<String, String>{
      "boardName": data["TrelloBoard"]["name"],
      "boardId": data["TrelloBoard"]["id"],
      "listName": data["Trello"]["name"],
      "listId": data["Trello"]["id"],
    }),
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Trello setService failed');
  }
}

Future<List<dynamic>> trelloGetBoards() async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/Trello/getBoards',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Get trello boards failed');
  }
}

Future<List<dynamic>> trelloGetListOfBoards(String boardId) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/Trello/getListOfBoard/' + boardId,
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Get list of boards failed');
  }
}
