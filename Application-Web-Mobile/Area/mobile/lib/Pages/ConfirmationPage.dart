import 'package:flutter/material.dart';

class ConfirmationPage extends StatelessWidget {
  final String message;
  final String redirectPath;

  ConfirmationPage(this.message, this.redirectPath);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Color(0xFF12c06a),
      body: Builder(
        builder: (contextScaffold) => GestureDetector(
          onTap: () => Navigator.pushNamedAndRemoveUntil(
              context, redirectPath, (route) => false),
          child: Padding(
            padding: EdgeInsets.all(10),
            child: ListView(
              children: <Widget>[
                Padding(
                  padding: EdgeInsets.fromLTRB(
                      10, MediaQuery.of(context).size.height / 4, 10, 10),
                ),
                Column(
                  crossAxisAlignment: CrossAxisAlignment.center,
                  children: <Widget>[
                    Image.asset(
                      "assets/success.gif",
                      width: 300,
                      height: 300,
                    ),
                    Container(
                      transform: Matrix4.translationValues(0, -50, 0),
                      child: Text(
                        message,
                        style: TextStyle(fontSize: 20, color: Colors.white),
                      ),
                    ),
                    Text(
                      "Tap anywhere to continue.",
                      style: TextStyle(fontSize: 15, color: Colors.white),
                    ),
                  ],
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
