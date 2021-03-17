import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> githubSetService(data) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.post(
    serverLink + '/service/GitHub/setService',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
    body: jsonEncode(<String, String>{
      "githubUserName": data["GitHub"]["repoName"],
      "githubRepoName": data["GitHubUsername"] ?? null,
      "events": data["GitHub"]["event"],
    }),
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('GitHub setService failed');
  }
}

Future<List<dynamic>> githubGetUserRepo() async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/GitHub/getUserRepo',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Get GitHub repository failed');
  }
}
