import 'package:shared_preferences/shared_preferences.dart';

void storeStringValue(String key, String value) async {
  final prefs = await SharedPreferences.getInstance();
  prefs.setString(key, value);
}

Future<String> getStringValue(String key) async {
  final prefs = await SharedPreferences.getInstance();
  final value = prefs.getString(key) ?? null;
  return value;
}

void removeStringValue(String key) async {
  final prefs = await SharedPreferences.getInstance();
  prefs.remove(key);
}
