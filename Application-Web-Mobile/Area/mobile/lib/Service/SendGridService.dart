import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';
import 'package:mobile/Store/Store.dart';

Future<dynamic> sendGridSetService(data) async {
  final userToken = await getStringValue("userToken");

  final http.Response response =
      await http.post(serverLink + '/service/Sendgrid/setService',
          headers: <String, String>{
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + userToken,
          },
          body: jsonEncode(<String, String>{
            "object": data["SendGridObject"],
            "message": data["SendGrid"],
            "recipientEmail": data["SendGridRecipient"],
          }));

  if (response.statusCode == 200) {
    return jsonDecode(response.body)["success"];
  } else {
    throw Exception('SendGrid set service failed');
  }
}
