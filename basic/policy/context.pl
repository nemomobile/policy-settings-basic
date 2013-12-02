:- module(context, [update_contexts/1]).


% # Our predicates for the rule engine.
rules([update_contexts/1]).

% #
% # exported predicates
% #

update_contexts(ContextList) :-
    findall(X,context_variable(call,X),ContextList).	
	
% #############################################################################
% # com.nokia.policy.context (   variable = "call",                           #
% #                              value = [active|ringing|inactive] )          #   
% #############################################################################

% # call state(value) can be active, ringing or inactive
call_state(active) :- 
	telephony:has_active_call,!;
 	telephony:has_active_video_call,!;
	telephony:has_held_call,!.

call_state(ringing) :-
 	telephony:has_alerting_call,!;		% # incoming created call
 	telephony:has_outgoing_call,!.		% # outgoing created call

call_state(inactive) :- 
% #	\+call_state(active),	% # Commented out unnecessary checks. active and 
% # \+call_state(ringing),	% # ringing have already been checked as false.
	!.

% # call context predicate
context_variable(call, Entry) :-
	call_state(State),
	set_context_variable_and_value(call, State, Entry).


% #############################################################################
% #                        ### helper predicates ###                          #
% #############################################################################

set_context_variable_and_value(Variable, Value, Entry) :-
	Entry = [context, [variable, Variable], [value, Value]].
