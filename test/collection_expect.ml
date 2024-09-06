open Hbt.Collection

module Entity_construction = struct
  let pp_list pp_item ppf list =
    let open Format in
    let pp_sep fmt () = fprintf fmt ";@;<1 2>" in
    fprintf ppf "@[<hv>[@;<0 2>%a@;<0 0>]@]" (pp_print_list ~pp_sep pp_item) list

  let archive_uri = Uri.of_string "https://archive.org/"
  let created_at = Time.of_string "September 2, 2024"
  let archive_maybe_name = Some (Name.of_string "The Internet Archive")

  let%expect_test "bare" =
    let entity = Entity.make archive_uri created_at None Label_set.empty in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "September 2, 2024";
        updated_at = [];
        names = {};
        labels = {};
      }
      |}]

  let%expect_test "name and label" =
    let labels = Label_set.of_list [ Label.of_string "archives" ] in
    let entity = Entity.make archive_uri created_at archive_maybe_name labels in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "September 2, 2024";
        updated_at = [];
        names = {"The Internet Archive"};
        labels = {"archives"};
      }
      |}]

  let%expect_test "more labels" =
    let labels = Label_set.of_list [ Label.of_string "archives"; Label.of_string "books" ] in
    let entity = Entity.make archive_uri created_at archive_maybe_name labels in
    Entity.pp Format.std_formatter entity;
    [%expect
      {|
      {
        uri = https://archive.org/;
        created_at = "September 2, 2024";
        updated_at = [];
        names = {"The Internet Archive"};
        labels = {"archives"; "books"};
      }
      |}]

  let%expect_test "multiples" =
    let archive =
      let labels = Label_set.of_list [ Label.of_string "archives"; Label.of_string "search" ] in
      Entity.make archive_uri created_at archive_maybe_name labels
    in
    let google =
      let google_uri = Uri.of_string "https://www.google.com/" in
      let google_maybe_name = Some (Name.of_string "Google") in
      let labels = Label_set.of_list [ Label.of_string "search" ] in
      Entity.make google_uri created_at google_maybe_name labels
    in
    let entities = [ archive; google ] in
    pp_list Entity.pp Format.std_formatter entities;
    [%expect
      {|
      [
        {
          uri = https://archive.org/;
          created_at = "September 2, 2024";
          updated_at = [];
          names = {"The Internet Archive"};
          labels = {"archives"; "search"};
        };
        {
          uri = https://www.google.com/;
          created_at = "September 2, 2024";
          updated_at = [];
          names = {"Google"};
          labels = {"search"};
        }
      ]
      |}]
end
