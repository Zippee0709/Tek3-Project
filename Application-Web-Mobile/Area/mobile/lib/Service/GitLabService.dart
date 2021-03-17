import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> gitlabSetService(data) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.post(
    serverLink + '/service/gitlab/setService',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
    body: jsonEncode(<String, String>{
      "projectId": data["GitLab"]["id"].toString(),
      "projectName": data["GitLab"]["name"],
    }),
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Gitlab setService failed');
  }
}

Future<List<dynamic>> gitlabGetUserRepo() async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/gitlab/getRepos',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Get gitlab repository failed');
  }
}
