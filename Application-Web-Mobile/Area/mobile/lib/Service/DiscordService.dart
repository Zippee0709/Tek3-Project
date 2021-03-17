import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> discordSetService(data) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.post(
    serverLink + '/service/Discord/setService',
    headers: <String, String>{
      'Content-Type': 'application/json',
      'Authorization': 'Bearer ' + userToken,
    },
    body: jsonEncode(<String, String>{
      "serverName": data["DiscordServer"]["name"],
      "serverId": data["DiscordServer"]["id"],
      "channelName": data["Discord"]["name"],
      "channelId": data["Discord"]["id"],
    }),
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Discord set service failed');
  }
}

Future<List<dynamic>> discordGetChannels(serverId) async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/Discord/getChannels/' + serverId,
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('Get discord channels failed');
  }
}

Future<List<dynamic>> discordGetServers() async {
  final userToken = await getStringValue("userToken");

  final http.Response response = await http.get(
    serverLink + '/service/Discord/getServer',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
      'Authorization': 'Bearer ' + userToken,
    },
  );

  if (response.statusCode == 200) {
    return jsonDecode(response.body);
  } else {
    throw Exception('Get discord channels failed');
  }
}
