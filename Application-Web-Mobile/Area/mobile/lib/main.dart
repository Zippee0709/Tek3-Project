import 'package:flutter/material.dart';
import 'package:mobile/Pages/Authentification/Login.dart';
import 'package:mobile/Pages/Authentification/Register.dart';
import 'package:mobile/Pages/Services.dart';
import 'package:flutter_icons/flutter_icons.dart';
import 'package:mobile/Pages/ListOfArea.dart';
import 'package:mobile/Pages/CreationArea/CreationArea.dart';
import 'package:mobile/Store/Store.dart';
import 'package:flutter_dotenv/flutter_dotenv.dart' as DotEnv;

Future main() async {
  await DotEnv.load();
  final userToken = await getStringValue("userToken");
  runApp(new MyApp(userToken));
}

class MyApp extends StatelessWidget {
  final String userToken;

  MyApp(this.userToken);

  @override
  Widget build(BuildContext context) {
    return new MaterialApp(
      debugShowCheckedModeBanner: false,
      // title: "Area",
      home: userToken != null ? new View() : new Login(),
      theme: ThemeData(
        primaryColor: Color(0XFF000000),
        accentColor: Color(0XFFf3ca20),
        visualDensity: VisualDensity.adaptivePlatformDensity,
        fontFamily: "Nunito",
      ),
      routes: {
        "/login": (_) => new Login(),
        "/register": (_) => new Register(),
        "/services": (_) => new Services(),
        "/home": (_) => new View(),
      },
    );
  }
}

class View extends StatefulWidget {
  @override
  State<StatefulWidget> createState() {
    return new ViewState();
  }
}

class ViewState extends State<View> {
  int _selectedPage = 0;
  final _pageOptions = [
    CreationArea(),
    ListOfArea(),
    Services(),
  ];

  final items = [
    BottomNavigationBarItem(
      icon: Icon(Feather.plus_circle),
      label: "Create AREA",
    ),
    BottomNavigationBarItem(
      icon: Icon(Feather.layers),
      label: "List of AREA",
    ),
    BottomNavigationBarItem(
      icon: Icon(Feather.folder_plus),
      label: "Services",
    ),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: _pageOptions[_selectedPage],
      drawer: Drawer(
        child: ListView(
          padding: EdgeInsets.zero,
          children: <Widget>[
            DrawerHeader(
              child: null,
              decoration: BoxDecoration(
                color: Colors.white,
                image: new DecorationImage(
                  image: AssetImage("assets/area-logo.png"),
                  fit: BoxFit.contain,
                ),
              ),
            ),
            Divider(color: Colors.black),
            ListTile(
              title: Text('Log out'),
              leading: Icon(Icons.logout),
              onTap: () {
                removeStringValue("userToken");
                Navigator.pushNamedAndRemoveUntil(
                    context, '/login', (route) => false);
              },
            ),
          ],
        ),
      ),
      appBar: AppBar(
        title: Text(items[_selectedPage].label),
        backgroundColor: Theme.of(context).primaryColor,
        centerTitle: true,
        leading: Builder(
          builder: (context) => IconButton(
            icon: Icon(Icons.menu),
            onPressed: () => Scaffold.of(context).openDrawer(),
          ),
        ),
      ),
      bottomNavigationBar: BottomNavigationBar(
          type: BottomNavigationBarType.fixed,
          currentIndex: _selectedPage,
          selectedItemColor: Theme.of(context).accentColor,
          onTap: (int index) {
            setState(() {
              _selectedPage = index;
            });
          },
          items: items),
    );
  }
}
