%PAKCS1.11 swi6 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule('Math').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Math.fib',fib,1,'Math.fib',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('Math.fact',fact,1,'Math.fact',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Math.fib'(_G42472,_G42473,_G42474,_G42475):-freeze(_G42474,'blocked_Math.fib'(_G42472,_G42473,_G42474,_G42475)).
'blocked_Math.fib'(_G42510,_G42986,_G42989,_G42992):-hnf(_G42510,_G43378,_G42989,_G43369),'blocked_Math.fib_or1_1'(_G43378,_G42986,_G43369,_G42992).

'blocked_Math.fib_or1_1'(_G43514,_G43515,_G43516,_G43517):-freeze(_G43516,'blocked_blocked_Math.fib_or1_1'(_G43514,_G43515,_G43516,_G43517)).
'blocked_blocked_Math.fib_or1_1'(0,0,_G43566,_G43566).
'blocked_blocked_Math.fib_or1_1'(1,1,_G43703,_G43703).
'blocked_blocked_Math.fib_or1_1'('FAIL'(_G43825),'FAIL'(_G43825),_G43832,_G43832):-nonvar(_G43825).
'blocked_Math.fib'(_G42510,_G43854,_G43857,_G43860):-makeShare(_G42510,_G43893),hnf('Prelude.+'('Math.fib'('Prelude.-'(_G43893,1)),'Math.fib'('Prelude.-'(_G43893,2))),_G43854,_G43857,_G43860).

'Math.fact'(_G44959,_G44960,_G44961,_G44962):-freeze(_G44961,'blocked_Math.fact'(_G44959,_G44960,_G44961,_G44962)).
'blocked_Math.fact'(_G44997,_G45325,_G45328,_G45331):-hnf(_G44997,_G45735,_G45328,_G45726),'blocked_Math.fact_or1_1'(_G45735,_G45325,_G45726,_G45331).

'blocked_Math.fact_or1_1'(_G45874,_G45875,_G45876,_G45877):-freeze(_G45876,'blocked_blocked_Math.fact_or1_1'(_G45874,_G45875,_G45876,_G45877)).
'blocked_blocked_Math.fact_or1_1'(0,1,_G45926,_G45926).
'blocked_blocked_Math.fact_or1_1'('FAIL'(_G46051),'FAIL'(_G46051),_G46058,_G46058):-nonvar(_G46051).
'blocked_Math.fact'(_G44997,_G46080,_G46083,_G46086):-makeShare(_G44997,_G46119),hnf('Prelude.*'(_G46119,'Math.fact'('Prelude.-'(_G46119,1))),_G46080,_G46083,_G46086).

:-costCenters(['']).




%%%%% Number of shared variables: 2
