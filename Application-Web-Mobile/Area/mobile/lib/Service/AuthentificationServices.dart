import 'package:http/http.dart' as http;
import 'package:mobile/Models/UserModel.dart';
import 'dart:async';
import 'dart:convert';
import 'package:mobile/Service/ServerLink.dart';

Future<User> fetchLogin(String email, String password) async {
  final http.Response response = await http.post(
    serverLink + '/user/login',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
    },
    body: jsonEncode(<String, String>{
      'email': email,
      'password': password,
    }),
  );

  var decodedBody = jsonDecode(response.body);
  if (response.statusCode == 200) {
    return User.fromJson(decodedBody);
  } else {
    throw Exception('Login failed - ' + decodedBody["error"]);
  }
}

Future fetchRegister(String pseudo, String email, String password) async {
  final http.Response response = await http.post(
    serverLink + '/user/register',
    headers: <String, String>{
      'Content-Type': 'application/json; charset=UTF-8',
    },
    body: jsonEncode(<String, String>{
      'email': email,
      'password': password,
      'pseudo': pseudo,
    }),
  );

  if (response.statusCode != 200) {
    throw Exception('Register failed');
  }
}
