
# devtools::load_all()
try({
  avoncap::set_input("~/Data/avoncap")
  avoncap::input("nhs-extract")

  avoncap::all_files()


  # exact match on filename column of all_data()
  avoncap::most_recent_files("AvonCAPLRTDCentralDa")


  # or matches by lower case startWith on directory
  avoncap::most_recent_files("nhs-extract","deltave")


  avoncap::most_recent_files("metadata")

  avoncap::valid_inputs()
})
