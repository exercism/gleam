pub fn is_leap_year(year: Int) -> Bool {
  year % 400 == 0 || year % 4 == 0 && year % 100 != 0
}
