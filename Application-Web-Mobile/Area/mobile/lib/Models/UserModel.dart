class User {
  final String email;
  final String pseudo;
  final String token;

  User({this.email, this.pseudo, this.token});

  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      email: json['email'],
      pseudo: json['pseudo'],
      token: json['token'],
    );
  }
}
