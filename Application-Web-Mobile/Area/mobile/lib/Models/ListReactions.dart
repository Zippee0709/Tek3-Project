class ListReactions {
  final List<String> firstReactions;
  final List<String> secondReactions;

  ListReactions({this.firstReactions, this.secondReactions});

  factory ListReactions.fromJson(
      Map<String, dynamic> json, String firstService, String secondService) {
    var test = ListReactions(
      firstReactions: json["servicesReaction"][firstService][0].cast<String>(),
      secondReactions:
          json["servicesReaction"][secondService][0].cast<String>(),
    );
    return test;
  }
}
