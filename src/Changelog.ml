
open Common
open OASISTypes

let itp =
  Conf.create
    ~cli:"--itp"
    "int Bug number of the ITP for the package."
    Conf.ShortInput

let distribution =
  Conf.create
    ~cli:"--distribution"
    "str Distribution for the package."
    (Conf.Value "UNRELEASED")

let create ~ctxt t =
  let pkg_version = 
    OASISVersion.string_of_version t.pkg.version
  in
  let date =
    CalendarLib.Printer.Calendar.sprint "%a, %d %b %Y %T %z" (CalendarLib.Calendar.now ())
  in
  let distribution = Conf.get ~ctxt distribution in
  let closes =
    if Conf.is_set itp then
      Printf.sprintf " (Closes: #%s)" (Conf.get ~ctxt itp)
    else
      ""
  in
  let author =
    try
      Printf.sprintf "%s <%s>"
        (Sys.getenv "DEBFULLNAME")
        (Sys.getenv "DEBEMAIL")
    with Not_found ->
      failwith 
        "Set author using DEBFULLNAME and DEBEMAIL environment variables."
  in
    debian_with_fn 
      "changelog"
      (output_content
         (interpolate "\
$t.pkg.name ($pkg_version-1) $distribution; urgency=low

  * Initial release.$closes
  * Generated with oasis2debian v${Version.ver}.

 -- $author  ${date}"))


