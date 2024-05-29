import exercism/should
import exercism/test_runner
import wine_cellar.{Red, Rose, White, Wine}

const cellar = [
  Wine("Chardonnay", 2015, "Italy", White),
  Wine("Chardonnay", 2014, "France", White),
  Wine("Dornfelder", 2018, "Germany", Rose), Wine("Merlot", 2015, "France", Red),
  Wine("Riesling", 2017, "Germany", White),
  Wine("Pinot grigio", 2015, "Germany", White),
  Wine("Pinot noir", 2016, "France", Red),
  Wine("Pinot noir", 2013, "Italy", Red),
]

pub fn main() {
  test_runner.main()
}

pub fn wines_of_color_white_test() {
  cellar
  |> wine_cellar.wines_of_color(White)
  |> should.equal([
    Wine("Chardonnay", 2015, "Italy", White),
    Wine("Chardonnay", 2014, "France", White),
    Wine("Riesling", 2017, "Germany", White),
    Wine("Pinot grigio", 2015, "Germany", White),
  ])
}

pub fn wines_of_color_red_test() {
  cellar
  |> wine_cellar.wines_of_color(Red)
  |> should.equal([
    Wine("Merlot", 2015, "France", Red),
    Wine("Pinot noir", 2016, "France", Red),
    Wine("Pinot noir", 2013, "Italy", Red),
  ])
}

pub fn wines_of_color_rose_test() {
  cellar
  |> wine_cellar.wines_of_color(Rose)
  |> should.equal([Wine("Dornfelder", 2018, "Germany", Rose)])
}

pub fn wines_from_country_france_test() {
  cellar
  |> wine_cellar.wines_from_country("France")
  |> should.equal([
    Wine("Chardonnay", 2014, "France", White),
    Wine("Merlot", 2015, "France", Red),
    Wine("Pinot noir", 2016, "France", Red),
  ])
}

pub fn wines_from_country_germany_test() {
  cellar
  |> wine_cellar.wines_from_country("Germany")
  |> should.equal([
    Wine("Dornfelder", 2018, "Germany", Rose),
    Wine("Riesling", 2017, "Germany", White),
    Wine("Pinot grigio", 2015, "Germany", White),
  ])
}

pub fn wines_from_country_scotland_test() {
  cellar
  |> wine_cellar.wines_from_country("Scotland")
  |> should.equal([])
}

pub fn filter_white_germany_test() {
  cellar
  |> wine_cellar.filter(color: White, country: "Germany")
  |> should.equal([
    Wine("Riesling", 2017, "Germany", White),
    Wine("Pinot grigio", 2015, "Germany", White),
  ])
}

pub fn filter_red_france_test() {
  cellar
  |> wine_cellar.filter(country: "France", color: Red)
  |> should.equal([
    Wine("Merlot", 2015, "France", Red),
    Wine("Pinot noir", 2016, "France", Red),
  ])
}
