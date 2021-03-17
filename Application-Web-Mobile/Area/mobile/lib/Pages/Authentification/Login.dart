import 'package:flutter/material.dart';
import 'package:mobile/Pages/Authentification/Register.dart';
import 'package:mobile/Service/AuthentificationServices.dart';
import 'package:mobile/Tools/Snackbar.dart';
import 'package:mobile/Store/Store.dart';

class Login extends StatefulWidget {
  @override
  _Login createState() => _Login();
}

enum Field { email, password }

class _Login extends State<Login> {
  List<TextEditingController> _controllers = [
    for (int i = 0; i < 2; i++) TextEditingController()
  ];
  List<bool> _validates = [for (int i = 0; i < 2; i++) false];
  String errorMessage = "Error this field can't be empty";

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: Text('Log In'),
        ),
        body: Builder(
            builder: (contextScaffold) => Padding(
                padding: EdgeInsets.all(10),
                child: ListView(
                  children: <Widget>[
                    Image.asset('assets/area-logo.png',
                        width: 275, height: 275),
                    Container(
                      padding: EdgeInsets.all(10),
                      child: TextField(
                        controller: _controllers[Field.email.index],
                        decoration: InputDecoration(
                          prefixIcon: Icon(
                            Icons.alternate_email,
                            color: _validates[Field.email.index]
                                ? Theme.of(context).errorColor
                                : Theme.of(context).accentColor,
                          ),
                          border: OutlineInputBorder(),
                          labelText: 'E-mail address',
                          errorText: _validates[Field.email.index]
                              ? errorMessage
                              : null,
                        ),
                      ),
                    ),
                    Container(
                      padding: EdgeInsets.fromLTRB(10, 10, 10, 10),
                      child: TextField(
                        obscureText: true,
                        controller: _controllers[Field.password.index],
                        decoration: InputDecoration(
                          prefixIcon: Icon(
                            Icons.lock,
                            color: _validates[Field.password.index]
                                ? Theme.of(context).errorColor
                                : Theme.of(context).accentColor,
                          ),
                          border: OutlineInputBorder(),
                          labelText: 'Password',
                          errorText: _validates[Field.password.index]
                              ? errorMessage
                              : null,
                        ),
                      ),
                    ),
                    Container(
                        height: 65,
                        padding: EdgeInsets.fromLTRB(10, 15, 10, 0),
                        child: RaisedButton(
                          textColor: Colors.white,
                          color: Theme.of(context).accentColor,
                          child: Text('Log in', style: TextStyle(fontSize: 20)),
                          onPressed: () async {
                            if (!textEditingControllerHaveErrors()) {
                              try {
                                var user = await fetchLogin(
                                    _controllers[Field.email.index].text,
                                    _controllers[Field.password.index].text);
                                storeStringValue("userToken", user.token);
                              } catch (error) {
                                showError(contextScaffold, error.toString());
                                _controllers[Field.password.index].clear();
                                return;
                              }
                              Navigator.pushNamedAndRemoveUntil(
                                  context, '/home', (_) => false);
                            }
                          },
                        )),
                    FlatButton(
                      onPressed: () {
                        //TODO: écran d'oublie de mdp
                      },
                      textColor: Color(0xfff3ca20),
                      child: Text('Forgot password ?'),
                    ),
                    Container(
                        child: Row(
                      children: <Widget>[
                        Text('Not registered yet ?'),
                        FlatButton(
                          textColor: Color(0xfff3ca20),
                          child: Text(
                            'Register',
                            style: TextStyle(fontSize: 18),
                          ),
                          onPressed: () {
                            Navigator.push(
                                context,
                                MaterialPageRoute(
                                    builder: (context) => Register()));
                          },
                        )
                      ],
                      mainAxisAlignment: MainAxisAlignment.center,
                    ))
                  ],
                ))));
  }

  bool textEditingControllerHaveErrors() {
    setState(() => {
          for (int i = 0; i < _controllers.length; i++)
            {
              _controllers[i].text.isEmpty
                  ? _validates[i] = true
                  : _validates[i] = false
            }
        });

    if (_validates.contains(true)) {
      errorMessage = "Error this field can't be empty";
      return true;
    }
    return false;
  }
}
