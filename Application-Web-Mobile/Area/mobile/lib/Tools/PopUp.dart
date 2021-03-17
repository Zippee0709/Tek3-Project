import 'package:flutter/material.dart';
import 'package:mobile/Pages/ConfirmationPage.dart';

Widget popUpCreateServicecDialog(
    BuildContext context, title, labelText, callback) {
  TextEditingController textController = TextEditingController();
  bool haveError = false;

  return new AlertDialog(
    title: Text(title),
    content: TextField(
      controller: textController,
      decoration: new InputDecoration(
        border: OutlineInputBorder(),
        labelText: labelText,
        errorText: haveError ? "Phone number incorrect" : null,
      ),
    ),
    actions: <Widget>[
      new FlatButton(
        onPressed: () async {
          await callback(textController.text);
          Navigator.pushReplacement(
              context,
              MaterialPageRoute(
                  builder: (BuildContext context) => ConfirmationPage(
                      "Twilio successfully subscribed.", "/home")));
        },
        textColor: Theme.of(context).primaryColor,
        child: const Text('Submit'),
      ),
    ],
  );
}
