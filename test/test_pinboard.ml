module Pinboard = Backlogged.Pinboard

let pinboard_testable = Alcotest.testable Pinboard.pp Pinboard.equal

let small =
  {|<!DOCTYPE NETSCAPE-Bookmark-file-1>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Pinboard Bookmarks</TITLE>
<H1>Bookmarks</H1>
<DL><p><DT><A HREF="http://c-faq.com/decl/spiral.anderson.html" ADD_DATE="1653114361" PRIVATE="0" TOREAD="0" TAGS="c,c++">Clockwise/Spiral Rule</A>

<DT><A HREF="https://docs.microsoft.com/en-us/sysinternals/downloads/procmon" ADD_DATE="1606184699" PRIVATE="1" TOREAD="0" TAGS="windows-dev">Process Monitor - Windows Sysinternals | Microsoft Docs</A>
<DD>Monitor file system, Registry, process, thread and DLL activity in real-time.

<DT><A HREF="https://www.intel.com/content/www/us/en/developer/tools/oneapi/vtune-profiler.html#gs.x8oazh" ADD_DATE="1649855530" PRIVATE="1" TOREAD="1" TAGS="performance,profiling,tools,toread">Fix Performance Bottlenecks with Intel® VTune™ Profiler</A>
</DL></p>
|}

let test_small () =
  let expected =
    List.rev
      [
        Pinboard.make ~href:"http://c-faq.com/decl/spiral.anderson.html" ~time:"1653114361"
          ~shared:true ~toread:false ~tag:[ "c"; "c++" ] ~description:"Clockwise/Spiral Rule" ();
        Pinboard.make ~href:"https://docs.microsoft.com/en-us/sysinternals/downloads/procmon"
          ~time:"1606184699" ~shared:false ~toread:false ~tag:[ "windows-dev" ]
          ~description:"Process Monitor - Windows Sysinternals | Microsoft Docs"
          ~extended:"Monitor file system, Registry, process, thread and DLL activity in real-time."
          ();
        Pinboard.make
          ~href:
            "https://www.intel.com/content/www/us/en/developer/tools/oneapi/vtune-profiler.html#gs.x8oazh"
          ~time:"1649855530" ~shared:false ~toread:true
          ~tag:[ "performance"; "profiling"; "tools" ]
          ~description:"Fix Performance Bottlenecks with Intel® VTune™ Profiler" ();
      ]
  in
  let actual = Common.with_temp_file small @@ fun filename -> Pinboard.from_html filename in
  Alcotest.(check (list pinboard_testable)) "same list" expected actual

let tests = [ ("from_html", [ Alcotest.test_case "test_small" `Quick test_small ]) ]
