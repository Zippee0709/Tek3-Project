import 'package:flutter/material.dart';
import 'package:mobile/Service/AuthentificationServices.dart';
import 'package:mobile/Tools/Snackbar.dart';
import 'package:mobile/Pages/ConfirmationPage.dart';

class Register extends StatefulWidget {
  @override
  _Register createState() => _Register();
}

enum Field { pseudo, email, password, confirmPassword }

class _Register extends State<Register> {
  List<TextEditingController> _controllers = [
    for (int i = 0; i < 4; i++) TextEditingController()
  ];
  List<bool> _validates = [for (int i = 0; i < 4; i++) false];
  String errorMessage;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(
          title: Text('Register'),
        ),
        body: Builder(
            builder: (contextScaffold) => Padding(
                padding: EdgeInsets.all(10),
                child: ListView(
                  children: <Widget>[
                    Image.asset('assets/area-logo.png',
                        width: 100, height: 100),
                    Container(
                      padding: EdgeInsets.all(10),
                      child: TextField(
                        controller: _controllers[Field.pseudo.index],
                        decoration: InputDecoration(
                          prefixIcon: Icon(
                            Icons.people,
                            color: _validates[Field.pseudo.index]
                                ? Theme.of(context).errorColor
                                : Theme.of(context).accentColor,
                          ),
                          border: OutlineInputBorder(),
                          labelText: 'Pseudo',
                          errorText: _validates[Field.pseudo.index]
                              ? errorMessage
                              : null,
                        ),
                      ),
                    ),
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
                      padding: EdgeInsets.fromLTRB(10, 10, 10, 10),
                      child: TextField(
                        obscureText: true,
                        controller: _controllers[Field.confirmPassword.index],
                        decoration: InputDecoration(
                            prefixIcon: Icon(
                              Icons.lock,
                              color: _validates[Field.confirmPassword.index]
                                  ? Theme.of(context).errorColor
                                  : Theme.of(context).accentColor,
                            ),
                            border: OutlineInputBorder(),
                            labelText: 'Confirm password',
                            errorText: _validates[Field.confirmPassword.index]
                                ? errorMessage
                                : null),
                      ),
                    ),
                    Container(
                        height: 65,
                        padding: EdgeInsets.fromLTRB(10, 15, 10, 0),
                        child: RaisedButton(
                          textColor: Colors.white,
                          color: Color(0xfff3ca20),
                          child:
                              Text('Register', style: TextStyle(fontSize: 20)),
                          onPressed: () async {
                            if (!textEditingControllerHaveErrors()) {
                              try {
                                await fetchRegister(
                                    _controllers[Field.pseudo.index].text,
                                    _controllers[Field.email.index].text,
                                    _controllers[Field.password.index].text);
                              } catch (error) {
                                showError(contextScaffold, error.toString());
                                return;
                              }
                              Navigator.pushReplacement(
                                  context,
                                  MaterialPageRoute(
                                      builder: (BuildContext context) =>
                                          ConfirmationPage(
                                              "A confirmation email has been sent.",
                                              "/login")));
                            }
                          },
                        )),
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

    if (_controllers[Field.password.index].text.length < 8) {
      errorMessage = "Use 8 characters or more for your password";
      _validates[Field.password.index] = true;
      return true;
    }

    if (_controllers[Field.password.index].text !=
        _controllers[Field.confirmPassword.index].text) {
      errorMessage = "The passwords didn't match. Please check again";
      _validates[Field.password.index] = true;
      _validates[Field.confirmPassword.index] = true;
      return true;
    }

    return false;
  }
}
