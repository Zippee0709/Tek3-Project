class ListActions {
  final List<String> firstActions;
  final List<String> secondActions;

  ListActions({this.firstActions, this.secondActions});

  factory ListActions.fromJson(
      Map<String, dynamic> json, String firstService, String secondService) {
    var test = ListActions(
      firstActions: json["servicesAction"][firstService][0].cast<String>(),
      secondActions: json["servicesAction"][secondService][0].cast<String>(),
    );
    return test;
  }
}
