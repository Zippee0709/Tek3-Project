import 'package:flutter/material.dart';

typedef void MyCallback();

class ServiceGroup {
  const ServiceGroup(this.item, this.group);

  final String item;
  final String group;
}

class ServiceInfo {
  final String name;
  final IconData icon;
  final Color color;
  final bool isAvailable;
  MyCallback callback;
  // final bool isLinked;
  // final String urlToOAuth2;
  ServiceInfo(
      {this.name, this.isAvailable, this.icon, this.color, this.callback});

  // ServiceInfo(
  //     {this.name, this.icon, this.color, this.isLinked, this.urlToOAuth2});
}

// subModel for /service/getServicesDetails
class AreaInfo {
  final String name;
  final IconData icon;
  final Color color;
  final String actionType;
  AreaInfo({this.name, this.icon, this.color, this.actionType});
}

// Model for /service/getServicesDetails
class UserAreaInfo {
  final String areaId;
  final AreaInfo action;
  final AreaInfo reaction;
  UserAreaInfo({this.areaId, this.action, this.reaction});
}

class Service {
  final String name;
  final IconData icon;
  final Color color;
  final bool isLinked;
  final String urlToOAuth2;

  Service({this.name, this.icon, this.color, this.isLinked, this.urlToOAuth2});
}
