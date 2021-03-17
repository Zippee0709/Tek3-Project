import 'package:flutter/material.dart';

void showError(BuildContext context, String msg) {
  Scaffold.of(context).showSnackBar(
    SnackBar(
      content: Text(msg),
      backgroundColor: Theme.of(context).errorColor,
    ),
  );
}

void showInfo(BuildContext context, String msg) {
  Scaffold.of(context).showSnackBar(
    SnackBar(
      content: Text(msg),
      backgroundColor: Theme.of(context).accentColor,
    ),
  );
}

void showSuccess(BuildContext context, String msg) {
  Scaffold.of(context).showSnackBar(
    SnackBar(
      content: Text(msg),
      backgroundColor: Colors.green[800],
    ),
  );
}
