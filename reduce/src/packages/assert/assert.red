module assert;

% $Id: assert.red 6712 2024-02-09 11:59:12Z arthurcnorman $
% (c) 2010-2021 T. Sturm, Germany

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
%
%    * Redistributions of source code must retain the relevant
%      copyright notice, this list of conditions and the following
%      disclaimer.
%    * Redistributions in binary form must reproduce the above
%      copyright notice, this list of conditions and the following
%      disclaimer in the documentation and/or other materials provided
%      with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%

create!-package('(assert assertcheckfn assertproc), nil);

fluid '(assert_functionl!*);

global '(exlist!*);
global '(outl!*);
fluid '(curline!*);

fluid '(ifl!*);
fluid '(!*backtrace);
fluid '(!*comp);
fluid '(!*msg);
fluid '(lispsystem!*);


fluid '(assertstatistics!*);
fluid '(fname!*);

fluid '(!*backtrace);

% Translation time configuration:

switch assert;
% The switch assert is a main switch. When it is off, all assertions are ignored to the most
% possible extent at translation time:
% 1. For "asserted procedure" and "asserted inline procedure", no assertion-related code is
%    generated at all.
% 2. formassert for the ASSERT( ... ) statement introduces '(cond (nil nil)). Note that introducing
%    nil causes problems within progs.
% Even when assert is on, evaulation of assertions depends on further switches and commands,
% introduced below.

put('assert, 'simpfg, '((t (assert_onoff)) (nil (assert_onoff))));

procedure assert_onoff();
   % For autoloading
   ;

switch assert_procedures, assert_inline_procedures;
% When off, no assertion-related code is generated for "asserted procedure" and "asserted inline
% procedure" at all. When assert_inline_procedures is on (and the switch "assert" above, too),
% inline procedures are translated as expr procedures.

% Default settings for the switches depend on some environment variables. This allows automatic
% testing without modification of test files.

if getenv("REDUCE_ASSERT_LEVEL") = "1" then <<
   on1 'assert;
   off1 'assert_procedures;
   off1 'assert_inline_procedures
>> else if getenv("REDUCE_ASSERT_LEVEL") = "2" then <<
   on1 'assert;
   on1 'assert_procedures;
   off1 'assert_inline_procedures
>> else if getenv("REDUCE_ASSERT_LEVEL") = "3" then <<
   on1 'assert;
   on1 'assert_procedures;
   on1 'assert_inline_procedures
>> else <<
   off1 'assert
>>;

% Runtime configuration:

switch assertinstall;
% For asserted procedures, the assertions can be installed and uninstalled at runtime using
% assert_[un]install proc_1, .., proc_n  or  assert_[un]install_all()
% Technically, these commands exchange the function value of identifier (using copyd). Variants with
% and without assertions are kept on the property lists, and the latter is exactly the code that
% would habe been created without any assertions. The idea behind this is that assertion checking
% for some frequently called functions could slow down the computation too much.
%
% The switch assertinstall specifies the default choice for all functions. Turning it on is
% probably the only way to make sure assertions are active on all asserted procedures, because
% assert_install_all() can only know what has been loaded, and autoload is not assertion-aware. All
% requires some re-thinking.

switch evalassert;
% When active, the ASSERT( ... ) statement generates code "if !*evalassert then ...". The switch
% can thus be used to toggle the assertion test at runtime.

if getenv("REDUCE_ASSERT_LEVEL") member '("1" "2" "3") then <<
   on1 'assertinstall;
   on1 'evalassert
>> else <<
   off1 'assertinstall;
   off1 'evalassert
>>;

switch assertbreak, assertstatistics;
% When assertbreak is on, assertion violations raise an error and evaluation stops. When assertbreak
% is off, only a warning message is printed, and evaluation continues. With assertbreak off, it
% makes sense to turn on assertstatistics. Then, in addition to the warning messages, assertion
% violations are counted by procedure. The collected information can be printed at the end using
% assert_analyze().

if getenv("REDUCE_ASSERT_CONTINUE") = "1" then <<
   off1 'assertbreak;
   on1 'assertstatistics
>> else <<
   on1 'assertbreak;
   off1 'assertstatistics
>>;

procedure assert_check1(fn, origfn, argl, argtypel, restype);
   % This is the wrapper code executed when an insertion is installed. fn is the name of the
   % original function; origfn is an identifier having the original function as its function value;
   % argl is the list of arguments passed; argtypel is a list of types asserted for the arguments
   % in argl; restype is the type asserted for the result of the function call. Depending on the
   % swith !*assertstatistics, there is statictical information added to the fluid
   % assertstatistics!*, which is output and deleted when calling assert_analyze().
   begin scalar cfn, w, res, scargtypel, bad; integer n;
      if !*assertstatistics then <<
         w := atsoc(fn, assertstatistics!*);
         if w then
            cadr w := cadr w + 1
         else
            assertstatistics!* := (fn . {1, 0, 0}) . assertstatistics!*
      >>;
      scargtypel := argtypel;
      for each a in argl do <<
         n := n + 1;
         if (cfn := get(car scargtypel, 'assert_dyntypechk))
            and not apply(cfn, {a})
            and not(pairp a and flagp(car a, 'assert_ignore))
         then <<
            bad := t;
            assert_error(fn, argtypel, restype, n, argl, res)
         >>;
         scargtypel := cdr scargtypel
      >>;
      res := apply(origfn, argl);
      if (cfn := get(restype, 'assert_dyntypechk))
         and not apply(cfn, {res})
         and not(pairp res and flagp(car res, 'assert_ignore))
      then <<
         bad := t;
         assert_error(fn, argtypel, restype, 0, argl, res)
      >>;
      if !*assertstatistics and bad then <<
         w := cdr atsoc(fn, assertstatistics!*);
         cadr w := cadr w + 1
      >>;
      return res
   end;

procedure assert_error(fn, argtypel, restype, typeno, argl, res);
   % Subroutine of assert_check1 called in case of an assertion violation. fn is
   % the name of the original function; argtypel is a list of types asserted for
   % the arguments of the function call; restype is the type asserted for the
   % result of the function call; typeno is an integer denoting which argument
   % has violated an assertion, where 0 stands for the result; type is the
   % asserted type for arg; arg is the argument violating an assertion.
   % Depending on the switch !*assertbreak, either the computation is
   % interrupted with a rederr or computation continues and the error message is
   % printed as a warning. In the latter case lprim is used, which is controlled
   % by the switch !*msg.
   begin scalar w, stars, !*lower;
      if !*assertstatistics then <<
         w := cdr atsoc(fn, assertstatistics!*);
         caddr w := caddr w + 1
      >>;
      terpri();
      stars := if !*assertbreak then "***** " else "*** ";
      backtrace();
      terpri();
      prin2 fn;
      prin2t " being entered:";
      for i := 1 : length argl do <<
         prin2 "   ";
         prin2 mkid('a, i);
         prin2 ":   ";
         if i = typeno then
            prettyprint nth(argl, i)
         else
            print nth(argl, i)
      >>;
      if eqn(typeno, 0) then <<
         prin2 "returned:   ";
         prettyprint res
      >>;
      terpri();
      prin2 stars;
      prin2 "assertion ";
      prin2 assert_format(fn, argtypel, restype);
      prin2 " is violated by ";
      prin2 if eqn(typeno, 0) then "result" else mkid('a, typeno);
      if ifl!* then <<
         prin2 " ";
         prin2 "(at ";
         prin2 car ifl!*;
         prin2 ":";
         prin2 curline!*;
         prin2 ")"
      >>;
      terpri();
      if !*assertbreak then
         error1()
   end;

procedure assert_format(fn, argtypel, restype);
   % fn is the original function name; argtypel is the list of types asserted
   % for the arguments; restype is the type asserted for the result.
   % Reconstructs the assertion as a identifier for printing in diagnostic
   % messages.
   begin scalar ass;
      ass := '!! . '!' . ass;
      if restype then <<
         ass := nconc(explode restype, ass);
         ass := '!! . '!: . '!! . blank . ass
      >>;
      ass := '!! . '!) . ass;
      for each a in reverse argtypel do
         ass := '!! . '!, . '!! . blank . nconc(explode a, ass);
      ass := cddddr ass;
      ass := '!! . '!( . ass;
      ass := nconc(explode fn, ass);
      ass := '!! . '!` . ass;
      return compress ass
   end;

procedure assert_structstat();
   % The parser for struct. Returns a form that stores the type
   % checking function on the property list of the type.
   begin scalar type, cfn, typeflag, typecheckform;
      type := scan();
      typeflag := {'flag, mkquote {type}, ''assert_dyntype};
      scan();
      if flagp(cursym!*, 'delim) then <<
%%       if not !*assert then
%%          return nil;
         if !*msg then lprim {"struct", type, "is not checked"};
         return typeflag
      >>;
      if cursym!* neq 'checked and cursym!* neq 'asserted then
         rederr {"expecting 'asserted by' in struct but found", cursym!*};
      if scan() neq 'by then
         rederr {"expecting 'by' in struct but found", cursym!*};
      cfn := scan();
      if not flagp(scan(), 'delim) then
         rederr {"expecting end of struct but found", cursym!*};
%%       if not !*assert then
%%       return nil;
      typecheckform := {'put, mkquote type, ''assert_dyntypechk, mkquote cfn};
      return {'progn, typecheckform, typeflag}
   end;

put('struct, 'stat, 'assert_structstat);

procedure assert_dyntypep(s);
   idp s and flagp(s, 'assert_dyntype);

operator assert_analyze;

procedure assert_analyze();
   % Print and delete the statistical information collected in the fluid
   % assertstatistics!*. This works in both algebraic and symbolic mode.
   begin scalar headline, footline; integer s1, s2, s3;
      assertstatistics!* := sort(assertstatistics!*, function(lambda x, y; ordp(car y, car x)));
      for each pr in assertstatistics!* do <<
         s1 := s1 + cadr pr;
         s2 := s2 + caddr pr;
         s3 := s3 + cadddr pr
      >>;
      headline := '(function . (!#calls  !#bad! calls !#assertion! violations));
      footline := 'sum . {s1, s2, s3};
      assertstatistics!* := nil . headline . nil .
         reversip(nil . footline . nil . assertstatistics!*);
      for each pr in assertstatistics!* do <<
         if pr then <<
            prin2 car pr;
            for i := length explode2 car pr + length explode2 cadr pr : 23 do
               prin2 " ";
            prin2 cadr pr;
            for i := length explode2 caddr pr : 23 do prin2 " ";
            prin2 caddr pr;
            for i := length explode2 cadddr pr : 23 do prin2 " ";
            prin2t cadddr pr
         >> else <<
            for i := 1:72 do prin2 "-";
            terpri()
         >>
      >>;
      assertstatistics!* := nil
   end;

procedure assert_declarestat();
   % The parser for assert.
   begin scalar l;
      l := assert_stat!-parse();
      if not (!*assert and !*assert_procedures) then
         return nil;
      return assert_declarestat1 l
   end;

procedure assert_declarestat1(l);
   % Returns forms that define a suitable wrapper function, store relevant
   % information on the property list of the original function, and add the
   % original function to the global list assert_functionl!*.
   begin scalar fnx, progn, assertfn, noassertfn, argl, w1, w2, w3, w4, w5;
      integer i;
      fnx := explode car l;
      assertfn := intern compress nconc(explode 'assert!:, fnx);
      noassertfn := intern compress nconc(explode 'noassert!:, fnx);
      argl := for each x in cadr l collect mkid('a, i := i + 1);
      w1 := mkquote car l;
      w2 := mkquote noassertfn;
      w3 := 'list . argl;
      w4 := 'list . for each fn in cadr l collect mkquote fn;
      w5 := mkquote caddr l;
      progn := {'de, assertfn, argl, {'assert_check1, w1, w2, w3, w4, w5}} . progn;
      progn := {'put, w1, ''assert_assertfn, mkquote assertfn} . progn;
      progn := {'put, w1, ''assert_noassertfn, w2} . progn;
      progn := {'put, w1, ''assert_installed, nil} . progn;
      progn := {'cond, {
         {'not, {'member, w1, 'assert_functionl!*}},
         {'setq, 'assert_functionl!*, {'cons, w1, 'assert_functionl!*}}}} . progn;
      if !*assertinstall then <<
         progn := {'assert_install1, mkquote car l} . progn
      >>;
      return 'progn . reversip progn
   end;

procedure assert_stat!-parse();
   % Subroutine of assert_stat(). This is the actual parsing code.
   begin scalar fn, argtypel, restype;
      fn := scan();
      if scan() neq '!*colon!* then
         rederr {"expecting ':' in assert but found", cursym!*};
      argtypel := assert_stat1();
      if scan() neq 'difference or scan() neq 'greaterp then
         rederr {"expecting '->' in assert but found", cursym!*};
      restype := scan();
      if not flagp(scan(), 'delim) then
         rederr {"expecting end of assert but found", cursym!*};
      return {fn, argtypel, restype}
   end;

procedure assert_stat1();
   % Subroutine of assert_stat!-parse. Parses the tuple of argument
   % types left of the arrow.
   begin scalar argtypel;
      if scan() neq '!*lpar!* then
         rederr {"expecting '(' in assert but found", cursym!*};
      if scan() eq '!*rpar!* then
         return nil;
      repeat <<
         argtypel := cursym!* . argtypel;
         scan();
         if cursym!* neq '!*comma!* and cursym!* neq '!*rpar!* then
            rederr {"expecting ', ' or ')' in assert but found", cursym!*};
         if cursym!* eq '!*comma!* then
            scan()
      >> until cursym!* eq '!*rpar!*;
      return reversip argtypel
   end;

put('declare, 'stat, 'assert_declarestat);

procedure assert_install(fnl);
   % This is parsed as stat rlis, i.e., it takes a comma-separated list
   % fnl of arbirary length of arguments w/o parentesis. fnl is list of
   % identifiers that are functions for which an existing assertion is
   % installed.
   for each fn in fnl do assert_install1 fn;

put('assert_install, 'stat, 'rlis);

procedure assert_install1(fn);
   % fn is an identifier that is a single function for which an existing
   % assertion is installed.
   if get(fn, 'assert_installed) then
      lprim {"assert already installed for", fn}
   else if not eqcar(getd fn, 'expr) then
      lprim {fn, "is not an expr procedure - ignoring assert"}
   else <<
      copyd(get(fn, 'assert_noassertfn), fn);
      copyd(fn, get(fn, 'assert_assertfn));
      put(fn, 'assert_installed, t)
   >>;

procedure assert_uninstall(fnl);
   % This is parsed as stat rlis, i.e., it takes a comma-separated list
   % fnl of arbirary length of arguments w/o parentesis. fnl is list of
   % identifiers that are functions for which an installed assertion is
   % uninstalled.
   for each fn in fnl do assert_uninstall1 fn;

put('assert_uninstall, 'stat, 'rlis);

procedure assert_uninstall1(fn);
   % fn is an identifier that is a single function for which an
   % installed assertion is uninstalled.
   if not get(fn, 'assert_installed) then
      lprim {"assert not installed for", fn}
   else <<
      copyd(fn, get(fn, 'assert_noassertfn));
      put(fn, 'assert_installed, nil)
   >>;

operator assert_install_all;

procedure assert_install_all();
   % This is parsed as stat endstat, i.e., it takes no arguments but
   % also no empty pair of parenthesis. Installs assertions for the
   % functions in the global list assert_functionl!* of all functions
   % for which there are assertions defined.
   for each fn in assert_functionl!* do
      if not get(fn, 'assert_installed) then <<
         lprim {"assert_install", fn};
         assert_install1 fn
      >>;

operator assert_uninstall_all;

procedure assert_uninstall_all();
   % This is parsed as stat endstat, i.e., it takes no arguments but
   % also no empty pair of parenthesis. Uninstalls assertions for the
   % functions in the global list assert_functionl!* of all functions
   % for which ther are assertions defined.
   for each fn in assert_functionl!* do
      if get(fn, 'assert_installed) then <<
         lprim {"assert_uninstall", fn};
         assert_uninstall1 fn
      >>;

procedure formassert(u, vars, mode);
   if mode eq 'symbolic and !*assert then
      assert_assert(u, vars, mode)
   else
      '(cond (nil nil));

put('ASSERT, 'formfn, 'formassert);
put('assert, 'formfn, 'formassert);

procedure assert_assert(u, vars, mode);
   begin scalar a, m;
%%      a := assert_outl2string outl!*;
      a := u;
      m := {"assertion", mkquote cadr a, "violated in procedure", mkquote fname!*};
      if ifl!* then
         m := assert_sconcat {car ifl!*, ":", assert_at2str curline!*, ":"} . m;
      m := 'list . m;
      return {'cond, {{'and, '!*evalassert, {'not, formc(cadr u, vars, mode)}},
                      {'progn, {'cond, {'!*backtrace, {'backtrace}}},
                               {'cond, {'!*assertbreak, {'rederr, m}}, {t, {'lprim, m}}}}}}
   end;

procedure assert_outl2string(outl);
   id2string compress for each x in reverse cdr outl join
      if stringp x then assert_string2idl x else explode x;

procedure assert_string2idl(s);
   % This is an artificial case distinction because the tokenizer inserts " "
   % into outl!* to reperesent mandatory whitespace. If this string " " ever
   % occurs literally within an assertion, then it will not show up in the
   % output.
   if s = " " then
      {'!!, blank}
   else
      for each c in explode s join
         if eq(c, '!") or eq(c, blank) then {'!!, c} else {c};

% The following are copies of rltools/lto.red, because I do not want to depend
% on other modules here:

procedure assert_sconcat2(s1, s2);
   % List tools string concatenation 2. [s1] and [s2] are strings.
   % Returns a string. The returned string is the concatenation
   % [s1][s2].
   compress append(reversip cdr reversip explode s1, cdr explode s2);

procedure assert_sconcat(l);
   % List tools string concatenation. [l] is a list of strings.
   % Returns a string. The returned string is the concatenation of all
   % strings in [l].
   if l then
      if cdr l then
         assert_sconcat2(car l, assert_sconcat cdr l)
      else
         car l;

procedure assert_at2str(s);
   % List tools atom to string. [s] is an atom. Returns the print name
   % of the atom [s] as a string.
   compress('!" . reversip('!" . reversip explode s));

#if (or (not (getd 'id2string)) (flagp 'id2string 'rlisp))
% Note that CSL has had id2string since March 2014.

procedure id2string(id);
   symbol!-name id;

flag('(id2string), 'rlisp);

#endif

endmodule;

end;
