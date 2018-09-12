% contradict

contradict(X,Y) :- X = not(Y);Y = not(X).

% contradict_element_list.

contradict_element_list(E,[H]) :-
			contradict(E,H).
contradict_element_list(E,[H|T]) :-
			contradict(E,H),!;
			contradict_element_list(E,T).

% contradict_list_list.

contradict_list_list([],[]).
contradict_list_list([H|T],L2) :-
		contradict_element_list(H,L2),!;
		contradict_list_list(T,L2).
% the legth of the list
length_list([],0).
length_list([_|L],N) :- length_list(L,N1), N is N1 + 1.

%remove_sublists(X,L): flatten the list if it has sublists.

remove_sublists(X,[X]) :- not(is_list(X)).
remove_sublists([],[]).
remove_sublists([X|Xs],Zs) :- remove_sublists(X,Y), remove_sublists(Xs,Ys), append(Y,Ys,Zs).

%reverse_list
inverse_list(L1,L2) :- inv_list(L1,L2,[]).

inv_list([],L2,L2) :- !.
inv_list([H|T],L2,L0) :- inv_list(T,L2,[H|L0]).

%negation_list
negation_list(L1,L3) :- neg_list(L1,L2,[]),inverse_list(L2,L3).

neg_list([],L2,L2) :- !.
neg_list([H|T],L2,L0) :- neg_list(T,L2,[not(H)|L0]).


% cases_without_preference: the set of all cases in the model

cases_without_preference(model_num(N),Cases_list) :-
			case_order(model_num(N),Case_order),
			remove_sublists(Case_order,Cases_list),!.


% member(X,L) means X is List L's member.

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).


% member_list(L1,L2): judge each element in L1 belongs to L2 or not.
% If so, return "true";
% if not, return "false", which means this element doesn't belong to L2.

member_list([X],L2) :-
		member(X,L2),!.
member_list([X|T],L2) :-
		member(X,L2),
		member_list(T,L2).

%------------------------------------------------------------------------------------
%the case model consists of cases and their order.

case_model(model_num(N)) :-
			case(model_num(N),case_num(_),_),
			case_order(model_num(N),_),!.

%----------------------------------------------------------------------------------
%case(case_num(N),C_list): I give each case in a case model a number N, so that we could distinguish these cases. The list shows elements in one case.

%case model 1 - copyright infringement

case(model_num(2),case_num(201),[not(pac),not(ite),not(pco),not(epr),not(avp),not(psa),not(ifg)]).
case(model_num(2),case_num(202),[pac,ite,pco,pec,not(ifg)]).
case(model_num(2),case_num(203),[pac,ite,pco,not(pec),not(epr),not(avp),not(psa),ifg,not(fpp)]).
case(model_num(2),case_num(204),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(205),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(206),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),not(lps),m3fti,not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(207),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(208),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(209),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(210),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,cpb,pbt]).
case(model_num(2),case_num(211),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,cpb,pbt]).
case(model_num(2),case_num(212),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(213),[pac,ite,pco,not(epr),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(214),[epr,not(pco),not(avp),not(psa),ifg,not(fpp)]).
case(model_num(2),case_num(215),[epr,not(pco),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(216),[epr,not(pco),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(217),[epr,not(pco),not(avp),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),not(lps),m3fti,not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(218),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(219),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(220),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(221),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,cpb,pbt]).
case(model_num(2),case_num(222),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,cpb,pbt]).
case(model_num(2),case_num(223),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(224),[epr,not(pco),not(avp),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(225),[pac,avp,not(pco),not(epr),not(psa),ifg,not(fpp)]).
case(model_num(2),case_num(226),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,ihe,not(ils),crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(227),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(228),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,ihe,not(ils),crc,not(hps),not(lps),m3fti,not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(229),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(230),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(231),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(232),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,cpb,pbt]).
case(model_num(2),case_num(233),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,cpb,pbt]).
case(model_num(2),case_num(234),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(235),[pac,avp,not(pco),not(epr),not(psa),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(236),[psa,not(pco),not(epr),not(avp),ifg,not(fpp)]).
case(model_num(2),case_num(237),[psa,not(pco),not(epr),not(avp),ifg,fpp,ihe,not(ils),crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(238),[psa,not(pco),not(epr),not(avp),ifg,fpp,ihe,not(ils),crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(239),[psa,not(pco),not(epr),not(avp),ifg,fpp,ihe,not(ils),crc,not(hps),not(lps),m3fti,not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(240),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,hps,not(lps),not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(241),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),lps,not(m3fti),not(l3fti),not(cdt),not(fin)]).
case(model_num(2),case_num(242),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),not(cdt),fin]).
case(model_num(2),case_num(243),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,cpb,pbt]).
case(model_num(2),case_num(244),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,cpb,pbt]).
case(model_num(2),case_num(245),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),l3fti,not(cdt),fin,not(cpb),not(pbt)]).
case(model_num(2),case_num(246),[psa,not(pco),not(epr),not(avp),ifg,fpp,not(ihe),ils,crc,not(hps),not(lps),not(m3fti),not(l3fti),cdt,fin,not(cpb),not(pbt)]).





case(model_num(1),case_num(101),[not(dmg)]).
case(model_num(1),case_num(102),[not(dut),dmg,not(unl),not(vrt),not(vst),not(vun)]).
case(model_num(1),case_num(103),[not(dut),dmg,unl,not(imp),not(ift),not(ila),not(ico)]).
case(model_num(1),case_num(104),[not(dut),dmg,unl,imp,not(cau)]).
case(model_num(1),case_num(105),[dut,dmg,unl,imp,cau,vrt,not(vst),not(vun),ift,not(ila),not(ico),not(jus),prp]).
case(model_num(1),case_num(106),[dut,dmg,unl,imp,cau,vrt,not(vst),not(vun),not(ift),ila,not(ico),not(jus),prp]).
case(model_num(1),case_num(107),[dut,dmg,unl,imp,cau,vrt,not(vst),not(vun),not(ift),not(ila),ico,not(jus),prp]).
case(model_num(1),case_num(108),[dut,dmg,unl,imp,cau,not(vrt),vst,not(vun),ift,not(ila),not(ico),not(jus),prp]).
case(model_num(1),case_num(109),[dut,dmg,unl,imp,cau,not(vrt),vst,not(vun),not(ift),ila,not(ico),not(jus),prp]).
case(model_num(1),case_num(110),[dut,dmg,unl,imp,cau,not(vrt),vst,not(vun),not(ift),not(ila),ico,not(jus),prp]).
case(model_num(1),case_num(111),[dut,dmg,unl,imp,cau,not(vrt),not(vst),vun,ift,not(ila),not(ico),not(jus),prp]).
case(model_num(1),case_num(112),[dut,dmg,unl,imp,cau,not(vrt),not(vst),vun,not(ift),ila,not(ico),not(jus),prp]).
case(model_num(1),case_num(113),[dut,dmg,unl,imp,cau,not(vrt),not(vst),vun,not(ift),not(ila),ico,not(jus),prp]).
case(model_num(1),case_num(114),[not(dut),dmg,not(unl),vrt,not(vst),jus]).
case(model_num(1),case_num(115),[not(dut),dmg,not(unl),not(vrt),vst,jus]).
case(model_num(1),case_num(116),[not(dut),dmg,unl,imp,cau,vst,not(prp)]).

cases_set(model_num(2),[case_num(201),case_num(202),case_num(203),case_num(204),case_num(205),case_num(206),case_num(207),case_num(208),case_num(209),case_num(210),case_num(211),case_num(212),case_num(213),case_num(214),case_num(215),case_num(216),case_num(217),case_num(218),case_num(219),case_num(220),case_num(221),case_num(222),case_num(223),case_num(224),case_num(225),case_num(226),case_num(227),case_num(228),case_num(229),case_num(230),case_num(231),case_num(232),case_num(233),case_num(234),case_num(235),case_num(236),case_num(237),case_num(238),case_num(239),case_num(240),case_num(241),case_num(242),case_num(243),case_num(244),case_num(245),case_num(246)]).

cases_set(model_num(1),[case_num(101),case_num(102),case_num(103),case_num(104),case_num(105),case_num(106),case_num(107),case_num(108),case_num(109),case_num(110),case_num(111),case_num(112),case_num(113),case_num(114),case_num(115),case_num(116)]).

%------------------------------------------------------------------------------------
%order(List): the list shows the case from stronger to weaker.

case_order(model_num(1),[case_num(101),case_num(102),case_num(103),case_num(104),[case_num(105),case_num(106),case_num(107),case_num(108),case_num(109),case_num(110),case_num(111),case_num(112),case_num(113)],[case_num(114),case_num(115),case_num(116)]]).

%------------------------------------------------------------------------------------
%order(List): the list shows the case from stronger to weaker.

case_order(model_num(2),[case_num(201),[case_num(202),case_num(203),case_num(214),case_num(225),case_num(236)],[case_num(204),case_num(205),case_num(207),case_num(208),case_num(215),case_num(216),case_num(218),case_num(219),case_num(226),case_num(227),case_num(229),case_num(230),case_num(237),case_num(238),case_num(240),case_num(241),case_num(206),case_num(209),case_num(210),case_num(211),case_num(212),case_num(213),case_num(217),case_num(220),case_num(221),case_num(222),case_num(223),case_num(224),case_num(228),case_num(231),case_num(232),case_num(233),case_num(234),case_num(235),case_num(239),case_num(242),case_num(243),case_num(244),case_num(245),case_num(246)]]).

%----------------------------------------------------------------------------------
% case order validity. duplicate_case is going to check if there is duplicate case in the list.

duplicate_case(Cases_list,[Case]) :- member(Case,Cases_list).
duplicate_case(Cases_list,[Case|Cases_set]) :-
				member(Case,Cases_list),
				duplicate_case(Cases_list,Cases_set).

% If there is a missing case, the length of these two list will be different. If there is a case which is not defined, the member_list will return false, because it is not a member of cases set.

case_order_validity(model_num(N)) :-
				cases_without_preference(model_num(N),Cases_list),
				cases_set(model_num(N),Cases_set),
				member_list(Cases_list,Cases_set),
				length_list(Cases_list,Length1),
				length_list(Cases_set,Length2),
				Length1 = Length2,
				duplicate_case(Cases_list,Cases_set),!.

% case model: check the model is valid or not.
% definiiton 1 - criteria 1 consistent

consistent_case(_,_,[]) :- !.
consistent_case(model_num(N),case(model_num(N),M,Case_M),[H|T]) :-
				case(model_num(N),M,Case_M),
				case(model_num(N),H,Case_H),
				not(not(Case_M) = Case_H),
				consistent_case(model_num(N),case(model_num(N),M,Case_M),T).

consistent_list(_,[]) :- !.
consistent_list(model_num(N),[H|T]) :-
			cases_without_preference(model_num(N),Cases_list),
			consistent_case(model_num(N),case(model_num(N),H,_),Cases_list),
			consistent_list(model_num(N),T).

case_model_consistent(model_num(N)) :-
				case_model(model_num(N)),
				case_order(model_num(N),Case_order),
				remove_sublists(Case_order,Cases_list),
				consistent_list(model_num(N),Cases_list),!.

% definition 1 - criteria 2 incompatible

incompatible_case(_,_,[]) :- !.
incompatible_case(model_num(N),M,[H|T]) :-
			M = H,
			incompatible_case(model_num(N),M,T);
			case(model_num(N),M,Case_M),
			case(model_num(N),H,Case_H),
			contradict_list_list(Case_M,Case_H),
			incompatible_case(model_num(N),M,T).

incompatible_list(_,[]) :- !.
incompatible_list(model_num(N),[H|T]) :-
			cases_without_preference(model_num(N),Cases_list),
			incompatible_case(model_num(N),H,Cases_list),
			incompatible_list(model_num(N),T).

case_model_incompatible(model_num(N)) :-
				case_order(model_num(N),Case_order),
				remove_sublists(Case_order,Cases_list),
				incompatible_list(model_num(N),Cases_list),!.

% definition 1 - criteria 3 different

different_case(_,_,[]) :- !.
different_case(model_num(N),M,[H|T]) :-
			M = H,
			different_case(model_num(N),M,T);
			case(model_num(N),M,Case_M),
			case(model_num(N),H,Case_H),
			not(Case_M = Case_H),
			different_case(model_num(N),M,T).

different_list(_,[]) :- !.
different_list(model_num(N),[H|T]) :-
			cases_without_preference(model_num(N),Cases_list),
			different_case(model_num(N),H,Cases_list),
			different_list(model_num(N),T).

case_model_different(model_num(N)) :-
				case_model(model_num(N)),
				case_order(model_num(N),Case_order),
				remove_sublists(Case_order,Cases_list),
				different_list(model_num(N),Cases_list),!.

%------------------------------------------------------------------------------------
%the case model consists of cases and their order.

case_model_valid(model_num(N)) :-
			case_model_consistent(model_num(N)),
			case_model_incompatible(model_num(N)),
			case_model_different(model_num(N)),
			case_order_validity(model_num(N)),!.

%-------------------------------------------------------------------------------------
% Definition 3
%coherent(X): judge the argument X is coherent in the model or not.

coherent_casemade(_,_,_) :- fail.
coherent_casemade(Premise,Conclusion,[Case_num|Other_cases]) :-
			case(_,Case_num,Case),
			append(Premise,Conclusion,Casemade),
			member_list(Casemade,Case),!;
			coherent_casemade(Premise,Conclusion,Other_cases).

coherent(argument(model_num(N),argu_num(_),Premise,Conclusion)) :-
				cases_without_preference(model_num(N),Cases_list),
				coherent_casemade(Premise,Conclusion,Cases_list).

%-------------------------------------------------------------------------------------
%Definition 4
%conclusive arguments

case_with_casemade(model_num(N),Premise,Conclusion,Case_num) :-
		case(model_num(N),Case_num,Case),
		append(Premise,Conclusion,Casemade),
		member_list(Premise,Case),
		member_list(Casemade,Case),!.

%either contain p,or not contain p

conclusive_case_check(model_num(N),Premise,Conclusion,Case_num) :-
		case_with_casemade(model_num(N),Premise,Conclusion,Case_num),!;
		case(model_num(N),Case_num,Case),
		not(member_list(Premise,Case)).

conclusive_cases_list_check(model_num(N),Premise,Conclusion,[Case_num]) :-
			conclusive_case_check(model_num(N),Premise,Conclusion,Case_num),!.
conclusive_cases_list_check(model_num(N),Premise,Conclusion,[Case_num|Other_cases]) :-
			conclusive_case_check(model_num(N),Premise,Conclusion,Case_num),
			conclusive_cases_list_check(model_num(N),Premise,Conclusion,Other_cases).

conclusive(argument(model_num(N),argu_num(_),Premise,Conclusion)) :-
				coherent(argument(model_num(N),argu_num(_),Premise,Conclusion)),
				cases_without_preference(model_num(N),Cases_list),
				conclusive_cases_list_check(model_num(N),Premise,Conclusion,Cases_list),!.

%-------------------------------------------------------------------------------------
%Definition 5
%presumptively_valid arguments


best_case_casemade_basic(model_num(_),Premise,Conclusion,Case_num,Best_case_casemade) :-
		case(model_num(_),Case_num,Case),
		append(Premise,Conclusion,Casemade),
		member_list(Casemade,Case),
		Best_case_casemade = Case_num.

best_case_casemade(model_num(_),_,_,_) :- !.
best_case_casemade(model_num(_),Premise,Conclusion,[Case_num|Other_cases],Best_case_casemade) :-
			best_case_casemade_basic(model_num(_),Premise,Conclusion,Case_num,Best_case_casemade),!;
			best_case_casemade(model_num(_),Premise,Conclusion,Other_cases,Best_case_casemade).



comparison_premise_sublist(model_num(N),Premise,[Case_num]) :-
			case(model_num(N),Case_num,Case),
			not(member_list(Premise,Case)),!.
comparison_premise_sublist(model_num(N),Premise,[Case_num|Other_cases]) :-
			is_list([Case_num|Other_cases]),
			case(model_num(N),Case_num,Case),
			not(member_list(Premise,Case)),
			comparison_premise_sublist(model_num(N),Premise,Other_cases).

comparison_premise_case(model_num(N),Premise,E) :-
			case(model_num(N),E,Case),
			not(member_list(Premise,Case)),!.

comparison_premise_basic(model_num(N),Premise,Case_num) :-
	comparison_premise_sublist(model_num(N),Premise,Case_num),!;
	comparison_premise_case(model_num(N),Premise,Case_num).

comparison_premise(_,Best_case_casemade,_,[Element]) :-
		Best_case_casemade = Element,!;
		member(Best_case_casemade,Element),!.
comparison_premise(model_num(N),Best_case_casemade,Premise,[Element|Other_cases]) :-
		Best_case_casemade = Element,!;
		member(Best_case_casemade,Element),!;
		comparison_premise_basic(model_num(N),Premise,Element),
		comparison_premise(model_num(N),Best_case_casemade,Premise,Other_cases).

presumptively_valid(argument(model_num(N),argu_num(_),Premise,Conclusion)) :-
					coherent(argument(model_num(N),argu_num(_),Premise,Conclusion)),
					case_order(model_num(N),Case_order),
					cases_without_preference(model_num(N),Cases_list),
					best_case_casemade(model_num(N),Premise,Conclusion,Cases_list,Best_case_casemade),
					comparison_premise(model_num(N),Best_case_casemade,Premise,Case_order),!.

%Attack

successful_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance) :-
					append(Premise,Defeating_circumstance,Premise_and_defeating),
					presumptively_valid(argument(model_num(N),argu_num(_),Premise,Conclusion)),
					not(presumptively_valid(argument(model_num(N),argu_num(_),Premise_and_defeating,Conclusion))).

rebutting_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance) :-
					successful_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance),
					negation_list(Conclusion,Neg_C),
					append(Premise,Defeating_circumstance,Premise_and_defeating),
					presumptively_valid(argument(model_num(N),argu_num(_),Premise_and_defeating,Neg_C)).

undercutting_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance) :-
					not(rebutting_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance)).

presumption(argument(model_num(N),argu_num(_),Premise,Conclusion)):-
					Premise = [],
					presumptively_valid(argument(model_num(N),argu_num(_),Premise,Conclusion)).

undermining_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance) :-
					presumption(argument(model_num(N),argu_num(_),Premise,Conclusion)),
					successful_attack(argument(model_num(N),argu_num(_),Premise,Conclusion),Defeating_circumstance).
