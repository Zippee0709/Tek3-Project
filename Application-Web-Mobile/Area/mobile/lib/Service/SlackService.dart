import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<List<dynamic>> slackGetRegisteredTeam() async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/Slack/getRegisteredTeam',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Slack set service failed');
  }
}
