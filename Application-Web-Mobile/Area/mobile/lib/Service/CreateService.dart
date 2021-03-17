import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> twilioCreateService(phoneNumber) async {
  final userToken = await getStringValue("userToken");

  final http.Response response =
      await http.post(serverLink + '/service/createService',
          headers: <String, String>{
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + userToken,
          },
          body: jsonEncode(<String, String>{
            "nameServices": "Twilio",
            "accessToken": "empty",
            "refreshToken": "empty",
            "tokenType": "empty",
            "phoneNumber": phoneNumber,
          }));

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('SendGrid set service failed');
  }
}

Future<dynamic> sendgridCreateService() async {
  final userToken = await getStringValue("userToken");

  final http.Response response =
      await http.post(serverLink + '/service/createService',
          headers: <String, String>{
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + userToken,
          },
          body: jsonEncode(<String, String>{
            "nameServices": "SendGrid",
            "accessToken": "empty",
            "refreshToken": "empty",
            "tokenType": "empty",
          }));

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('SendGrid set service failed');
  }
}
