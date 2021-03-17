class UserService {
  final String id;
  final String name;
  final int nbrArea;

  UserService({this.id, this.name, this.nbrArea});

  factory UserService.fromJson(Map<String, dynamic> json) {
    return UserService(
      id: json['id'],
      name: json['name'],
      nbrArea: json['nbrArea'],
    );
  }
}

class UserServiceGroup {
  const UserServiceGroup(this.item, this.group);

  final UserService item;
  final String group;
}
