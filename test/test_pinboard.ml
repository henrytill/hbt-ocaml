[@@@ocamlformat "wrap-fun-args = false"]

module Pinboard = Hbt.Pinboard

let pinboard_testable = Alcotest.testable Pinboard.pp Pinboard.equal

let small =
  {|<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Pinboard Bookmarks</TITLE>
<H1>Bookmarks</H1>
<DL><p><DT><A HREF="http://c-faq.com/decl/spiral.anderson.html" ADD_DATE="1653114361" PRIVATE="0" TOREAD="0" TAGS="c,c++">Clockwise/Spiral Rule</A>

<DT><A HREF="https://docs.microsoft.com/en-us/sysinternals/downloads/procmon" ADD_DATE="1606184699" PRIVATE="1" TOREAD="0" TAGS="windows-dev">Process Monitor - Windows Sysinternals | Microsoft Docs</A>
<DD>Monitor file system, Registry, process, thread and DLL activity in real-time.

<DT><A HREF="https://www.intel.com/content/www/us/en/developer/tools/oneapi/vtune-profiler.html" ADD_DATE="1649855530" PRIVATE="1" TOREAD="1" TAGS="performance,profiling,tools,toread">Fix Performance Bottlenecks with Intel® VTune™ Profiler</A>
</DL></p>
|}

let test_small () =
  let expected =
    [
      Pinboard.make
        ~href:"http://c-faq.com/decl/spiral.anderson.html"
        ~time:"1653114361"
        ~description:(Some "Clockwise/Spiral Rule")
        ~extended:None
        ~tag:[ "c"; "c++" ]
        ~hash:None
        ~shared:true
        ~toread:false;
      Pinboard.make
        ~href:"https://docs.microsoft.com/en-us/sysinternals/downloads/procmon"
        ~time:"1606184699"
        ~description:(Some "Process Monitor - Windows Sysinternals | Microsoft Docs")
        ~extended:
          (Some "Monitor file system, Registry, process, thread and DLL activity in real-time.")
        ~tag:[ "windows-dev" ]
        ~hash:None
        ~shared:false
        ~toread:false;
      Pinboard.make
        ~href:"https://www.intel.com/content/www/us/en/developer/tools/oneapi/vtune-profiler.html"
        ~time:"1649855530"
        ~description:
          (Some "Fix Performance Bottlenecks with Intel\194\174 VTune\226\132\162 Profiler")
        ~extended:None
        ~tag:[ "performance"; "profiling"; "tools" ]
        ~hash:None
        ~shared:false
        ~toread:true;
    ]
  in
  let expected = List.rev expected in
  let actual = Common.with_temp_file small @@ fun filename -> Pinboard.from_html filename in
  Alcotest.(check (list pinboard_testable)) "same list" expected actual

let tests = [ ("from_html", [ Alcotest.test_case "test_small" `Quick test_small ]) ]
