pub type Approval {
  Yes
  No
  Perhaps
}

pub type Cuisine {
  Korean
  Turkish
}

pub type Genre {
  Crime
  Horror
  Romance
  Thriller
}

pub type Activity {
  BoardGame
  Chill
  Movie(Genre)
  Restaurant(Cuisine)
  Walk(Int)
}

pub fn rate_activity(activity: Activity) -> Approval {
  case activity {
    Restaurant(Korean) -> Yes
    Restaurant(Turkish) -> Perhaps
    Movie(Romance) -> Yes
    Movie(_) -> No
    Walk(kilometers) if kilometers > 11 -> Yes
    Walk(kilometers) if kilometers > 6 -> Perhaps
    Walk(_) -> No
    _ -> No
  }
}
