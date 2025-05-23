module inter; % Functions for interactive support.

% Author: Anthony C. Hearn.

% Copyright (c) 1993 RAND.  All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
%    * Redistributions of source code must retain the relevant copyright
%      notice, this list of conditions and the following disclaimer.
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in the
%      documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNERS OR
% CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%

% $Id: inter.red 6827 2024-06-25 10:57:14Z arthurcnorman $

fluid '(!*echo !*int trap!-time!* ifl!* curline!*);

global '(!$eof!$
         !$eol!$
         !*lessspace
         cursym!*
         curescaped!*
         cloc!*
         contl!*
         edit!*
         eof!*
         erfg!*
         flg!*
         ipl!*
         key!*
         ofl!*
         opl!*
         techo!*);

symbolic procedure pause;
   %Must appear at the top-most level;
   if null !*int then nil
    else if key!* = 'pause then pause1 nil
    else %typerr('pause,"lower level command");
         pause1 nil;   % Allow at lower level for now.

symbolic procedure pause1 bool;
   begin scalar x;
      if bool then
        if getd 'edit1 and erfg!* and cloc!* and yesp "Edit?"
          then return <<contl!* := nil;
           if ofl!* then <<lprim list(car ofl!*,'shut);
                           close cdr ofl!*;
                           opl!* := delete(ofl!*,opl!*);
                           ofl!* := nil>>;
           edit1(cloc!*,nil)>>
         else if flg!* then return (edit!* := nil);
      if null ifl!* or yesp "Cont?" then return nil;
      ifl!* := list(car ifl!*,cadr ifl!*,curline!*);
      if x := assoccar(car ifl!*,contl!*)
        then <<contl!* := delete(x,contl!*); close cadar x>>;
      contl!* := (ifl!* . cdr ipl!* . !*echo) . contl!*;
      ifl!* := ipl!* := nil;
      rds nil;
      !*echo := techo!*
   end;

symbolic procedure assoccar(u,v);
   % Returns element of v in which caar of that element = u.
   if null v then nil
    else if u=caaar v then car v
    else assoccar(u,cdr v);

symbolic procedure yesp u;
   begin scalar ifl,ofl,x,y;
        if ifl!*
          then <<ifl := ifl!* := list(car ifl!*,cadr ifl!*,curline!*);
                 rds nil>>;
        if ofl!* then <<ofl:= ofl!*; wrs nil>>;
        if null !*lessspace then terpri();
        if atom u then prin2 u else lpri u;
        prin2t " (Y or N)";
        if null !*lessspace then terpri();
        y := setpchar '!?;
        x := yesp1();
        setpchar y;
        if ofl then wrs cdr ofl;
        if ifl then rds cadr ifl;
        cursym!* := '!*semicol!*;
        curescaped!* := nil;
        return x
   end;

symbolic procedure yesp1;
   % Basic loop for reading response.
   begin scalar bool,x,y;
    a:  while (x := readch()) eq !$eol!$ do nil;
        % Assume an end-of-file means lost control and exit.
         if x eq !$eof!$ then eval '(bye)
         else if (y := x memq '(!y !Y)) or x memq '(!n !N)
          then return y % F.J. Wright.
         else if null bool then <<prin2t "Type Y or N"; bool := t>>;
        go to a
   end;

symbolic procedure cont;
   begin scalar fl,techo;
        if ifl!* then return nil   % CONT only active from terminal.
         else if null contl!* then rerror(rlisp,28,"No file open");
        fl := caar contl!*;
        ipl!* := fl . cadar contl!*;
        techo := cddar contl!*;
        contl!* := cdr contl!*;
        if car fl=caar ipl!* and cadr fl=cadar ipl!*
          then <<ifl!* := fl;
                 if fl then <<rds cadr fl; curline!* := caddr fl>>
                  else rds nil;
                 !*echo := techo>>
         else <<eof!* := 1; lprim list(fl,"not open"); error1()>>
   end;

deflist ('((cont endstat) (pause endstat) (retry endstat)),'stat);

flag ('(cont),'ignore);

% This nasty fragment of code returns a floating point value that
% is a CRUDE indication of the speed of the machine on which it is running.
% It uses around a second of CPU time the first time it is run, with that
% amount of time not depending on the nature of the host platform.

% The value returned ranges from a bit over 1.0 for the regular version of
% Reduce on a Raspberry Pi 1 (ie about the slowest machine in common use
% today) up to almost 20.0 for the fastest desktop systems of 2018. The idea
% of this is that it can be used with with!-timeout() to end up with
% a time limit that represents at least roughly the same amount of work
% whether run on a fast or slow computer.
% A copy of Reduce build with debugging options, or the CSL "bootstrapreduce"
% can be seriously slower - perhaps by up to a factor of ten. The measurement
% here should not be viewed as a benchmark that is valid in any respectable
% way for comparing performance on different systems, and the exact results
% returned will tend to vary somewhat from run to run. Despite all
% uncertainties it may help make code that wants to set time limits be at
% least roughly able to set ones based on how much work has to be done.

global '(machine_speed!*);
machine_speed!* := nil;

symbolic procedure machine_speed();
  if machine_speed!* then machine_speed!*
  else begin
    scalar n, t0, t1;
    n := 50;
    t1 := 0;
    while t1 < 20 do <<
      n := 2*n;
      t0 := time();
% At this stage in the Reduce source I do not yet have the infix operators
% "to", ".*" and ".+" available, so I construct a sample polynomial as a
% raw Lisp data-structure just using CONS. The idea here is that getting
% Reduce to expand (1+x)^40 will use a range of its functionality and so
% give me a more sensible result here than if I just used a counting loop
% to assess performance.
      for i := 1:n do exptf((('x . 1) . 1) . 1, 40);
      t1 := time() - t0 >>;
    return (machine_speed!* := float n / float t1)
  end;

% On my main machine the above function runs for around 63ms the first time
% it is used. I hope that is long enough that clock granularity will not
% render its result meaningless while not taking long enough to disrupt
% everything else.

% I will have a scheme that lets me impose a limit on the time taken by
% a calculation. This works by using a hook function that is called at the
% end of any garbage collection, and so its granularity is not very fine.
% Note in particular that if you have a very large amount of memory and/or
% the calculation involved does not allocate memory as it goes then any
% interruption may be dramatically delayed. However for many cases of
% general algebraic calculations with default allocation memory it will be
% broadly useful!
% It exits using "throw" rather than by raising an error because by doing
% that the guarded code can not use errorset to avoid the interruption.
% The time-limit is passed in units of milliseconds. I am allowed to nest
% uses of "with-timeout" however the inner ones have their time allocation
% capped at the residual of the limit set by any outer one.
%
% Sometimes a user will really want the limit to be an absolute time period,
% as in "give up if I have not got anywhere within an hour", while in
% other cases (perhaps especially in demonstration scripts) the limit will
% want to be normalised against machine seed. So I provide two versions here,
% one for each scenario. The second is called with!-normalized!-timeout and it
% takes its limit in abstract "time units" that are VERY ROUGHLY milliseconds
% on a Raspberry pi 1, and are of course then much smaller on faster systems.

% With CSL and the conservative garbage collector (which is becoming standard
% for CSL in early 2024) I have a lower-level resource!-limit function that
% I can use which does not have as much latency because it is able to
% check for a timeout what I hope will be seen as "reasonably" frequently.
% Even if that is in place I will use the gc hook scheme as well as a matter
% of braces and belt.

trap!-time!* := nil; % nil here means no trapping active.

% Note that when I detect a timeout I not only throw an exception, but I
% clear trap!-time!* so that unwinding and processing the timeout will not
% provoke a further activation of the mechanism.
% In CSL the gc-hook function is passed an argument that indicates something
% about the garbage collection that just happened. In PSL it does not have
% an argument.

% Usage is
%     with!-timeout(time_limit, form_to_evaluate);
% and the result should be (list eval(form)) if that happens quickly enough
% or atomic if there is a timeout. Rather like the behaviour of errorset.`


#if (member 'csl lispsystem!*)

symbolic procedure with!-timeout(n, u);
  resource!-limit(u, fix(n/1000));

% Time limit in arbitrary units such that (very roughly) slow and fast
% machines get to do about the same amount of work before being interrupted.
% I think of the argument as being very roughly "Raspberry Pi 1 milliseconds",
% so mid or high-range laptops or desktops will take less than that time by
% a factor of 10 or 20.

symbolic procedure with!-normalized!-timeout(n, u);
  resource!-limit(u, fix(n/(1000.0*machine_speed())));

#else   % Now an "anything but CSL" version

symbolic procedure aftergcsystemhook();
  if trap!-time!* and
    time() > trap!-time!* then <<
      trap!-time!* := nil;
      throw('!@timeout!@, '!@timeout!@) >>;

!*gc!-hook!* := 'aftergcsystemhook;

symbolic procedure with!-timeout(n, u);
  begin
    scalar ott := trap!-time!*;
    return begin
      scalar trap!-time!* := time() + fix n;
      if numberp ott and trap!-time!* > ott then trap!-time!* := ott;
      return eval list('catch, ''!@timeout!@, u)
    end
  end;

symbolic procedure with!-normalized!-timeout(n, u);
  begin
    scalar ott := trap!-time!*;
    return begin
      scalar trap!-time!* := time() + fix (n/machine_speed());
      if numberp ott and trap!-time!* > ott then trap!-time!* := ott;
      return eval list('catch, ''!@timeout!@, u)
    end
  end;

#endif % PSL etc

% A typical use of this would be:
%
%    with!-timeout(7000,               % allow 7 seconds.
%                  '(perform_some_calculation));
% or
%    with!-normalized!-timeout(70000, % 70000 "abstract time units"
%                                     % About as previous case on a mid-range
%                                     % computer of 2018.
%                             '(perform_some_calculation));
%
% These return an atom if the time limit was exceeded, and otherwise a list
% whose car is the value of the protected expression.

% Note that it will be VITAL that the protected expression be such that
% interrupting it can not leave global variables or data in a state that
% will mess up further computation. If the code binds fluids that will be OK
% because they will be restored in the process of handling the interrupt,
% but Reduce global state, such as that associated with kernel ordering, would
% NOT be restored, and if RPLACx operations are performed in ways that
% could leave data in a temporarily insanitary state then that will be
% dangerous. There could at least in principle be problems with global state
% within the Lisp system too, but at present I can not provide a clear
% statement of what is safe and what might not be. Well input or output
% stream selection certainly represents global state...
% For CSL see sandbox!-eval...

endmodule;

end;
