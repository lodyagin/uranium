%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(proxy_checked,
          [recheck_good_proxies/5,
           craw/1
          ]).

:- use_module(library(readutil)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(logging/logging).
:- use_module(parser/general/dcg_entities).
:- use_module(library(http/dcg_basics)).
:- use_module(html/browsers/browsers).
:- use_module(library(ur_objects)).
:- use_module(library(ur_recorded_db)).
:- use_module(html/http_ops).
:- use_module(html/http_page).
:- use_module(library(time)).
:- use_module(logging/logging).

:- use_module(library(http/http_sgml_plugin),
              [type_major_props/3]). 

:- use_module(db/postgres/postgres).

:- use_module('Examples/utils/http_proxy_v',
	      [time_interval_pl_pg/2]).

recheck_good_proxies(DB_Key, Lim, Aux_Server, Min_Timeout, Max_Good_Resp) :-

   DB_Key = pg(Connection),
   
   % -1. Start the http server

   logged(http_server(mirror_request, [port(8996)]),
          error(socket_error('Address already in use'), _)),

   % 0. Make the DB connection

   db_size(DB_Key, DB_Size_Before),
   write_log(['full db size before:', DB_Size_Before],
             [logger(check_proxy), lf(1, before), lf(1)]),

   % 1. Recheck all known good proxies

   % 1.1 Define a recheck timeout
   (  pg_select(Connection, 
	     http_proxy_v(status^(:= almost_good), 
                          response_time^(Max_Good_Resp_I << max(_))
                          )
             ),
      (  Max_Good_Resp_I = null(_)
      -> Max_Good_Resp = 0
      ;  time_interval_pl_pg(Max_Good_Resp, Max_Good_Resp_I)
      )
   -> true
   ;  Max_Good_Resp = 0
   ),
   Good_Recheck_Timeout is max(Max_Good_Resp * 2, Min_Timeout),

   write_log(['Recheck known proxies with timeout', Good_Recheck_Timeout],
             [lf(1, before), lf(1)]),

   % 1.2. Rechecking
   db_reset(DB_Key, [check_time], status(almost_good)),
   check_proxy_db(DB_Key, Lim, Aux_Server, Good_Recheck_Timeout, 
	status(almost_good)).


craw(Args) :-

   memberchk(min_timeout - Min_Timeout, Args),
   memberchk(max_timeout - Max_Timeout, Args),   
   memberchk(db_name - Connection, Args),
   memberchk(mirror_http - Aux_Server, Args),

   assert(logging:logger(check_proxy, enabled)),
   DB_Key = pg(Connection),

   recheck_good_proxies(DB_Key, _, Aux_Server, Min_Timeout, Max_Good_Resp),
   
   % 2. Merge new DB
   write_log(['Downloading a fresh ip DB'],
             [lf(1, before), lf(1)]),

   download_ip_list(DB_Key),
  
   db_size(DB_Key, DB_Size_Now),
   format('~n^new db size: ~d~n', DB_Size_Now),

   % NB can be greater than Max_Timeout
   Max_Recheck_Timeout is max(Max_Timeout, Max_Good_Resp),
   
   Timeout_List = [Min_Timeout, Max_Recheck_Timeout],

   % 3. Check DB with different timeouts
   maplist(recheck_db(DB_Key, Aux_Server), Timeout_List).


recheck_db(DB_Key, Aux_Server, Timeout) :-

   write_log(['Check new entries with timeout', Timeout],
             [lf(1, before), lf(1)]),

   % Check the new part first
   check_proxy_db(DB_Key, _, Aux_Server, Timeout),

   % Reset all timed out entries with 
   % response_time (the old timeout) < Timeout

   DB_Key = pg(Connection),
   pg_begin(Connection),
   pg_update(Connection, 
             http_proxy_v(status ^ (:= timeout), 
                          response_time ^ (:= <(Timeout)), 
                          check_time ^ (null(_))
                          )
             ),
   pg_commit(Connection),

   write_log(['Recheck old entries with timeout', Timeout],
             [lf(1, before), lf(1)]),

   check_proxy_db(DB_Key, _, Aux_Server, Timeout).


%report(DB_Key) :-

%   db_iterate(DB_Key, status(almost_good), P), 
%   report(P),
%   fail 
%   ; 
%   true.

report(Proxy) :-

   obj_pretty_print(Proxy), 

   obj_field(Proxy, ip, IP),
   obj_field(Proxy, port, Port),
   obj_field(Proxy, response_time, Resp_Time),
   format('^good proxy: ~a:~d [resp ~f sec]~n', [IP, Port, Resp_Time]).

%
% (Re)Check proxies with check_time(+free)
%

check_proxy_db(DB_Key, Lim, Aux_Server, Timeout) :-

   check_proxy_db(DB_Key, Lim, Aux_Server, Timeout, true).

%
% (Re)Check only proxies matched criteria
% (... /\ check_time(+free) )
%

check_proxy_db(DB_Key, Lim, Aux_Server, Timeout, Query) :-

   Search_Query = Query /\ check_time(+free),

   db_iterate_replace(DB_Key,
                      check_proxy(Aux_Server, Timeout),
                      Search_Query, true, Lim).

check_proxy(Aux_Server, Timeout, Proxy_In, Proxy_Out, Count) :-

   obj_reset_fields([status, log, check_time, response_time],
                    Proxy_In, Proxy_Out, _),
        
   write_log(['checking proxy', Proxy_Out, '... '],
             [module(no), lf(0), logger(check_proxy)]),
   obj_field(Proxy_Out, ip, IP),
   obj_field(Proxy_Out, port, Port),
   obj_field(Proxy_Out, status, Status),
   obj_field(Proxy_Out, log, Log),
   obj_field(Proxy_Out, check_time, Check_Time),
   obj_field(Proxy_Out, response_time, Response_Time),

   get_time(Check_Time),
   
   Call_Time = Timeout,
   
   catch(
      call_with_time_limit(Call_Time,

         http_get(Aux_Server,
                  Reply_Codes,
                  [proxy(IP, Port), 
                   to(octets),
                   reply_header(Fields)
                  ] 
                 )
                           
      ),
      Exception,
      true
   ), 
   
   get_time(Get_Fin_Time),
   Response_Time is Get_Fin_Time - Check_Time,

   (   var(Reply_Codes)

   ->  (  Exception == time_limit_exceeded
       -> Status = timeout
       ;  Exception = error(socket_error(_), _)
       -> Status = connection_fail
       ;  Status = bad
       ),
       Log = Exception

   ;   Reply_Codes = redirect(_)

   ->  Status = bad,
       Log = Reply_Codes

       % define the content type
   ;   (   memberchk(content_type(Type_String), Fields)
       ->  (   type_major_props(Type_String, Type, _)
           ->  true 
           ;   Type = unknown
           )
       ;   Type = unknown
       ),

       (   Type == 'text/plain'
       ->  analize_reply(Reply_Codes, Reply, Status), 
	   Log = Reply

       ;   Status = bad, Log = codes(Reply_Codes)

       )
   ),

   write_log(Status, 
             [module(no), lf(1), logger(check_proxy)]),

   (  Status = almost_good
   -> report(Proxy_Out), Count = true
   ;  Count = false
   ).

analize_reply(Reply_Codes, Reply, Status) :-

   atom_codes(Reply_Atom, Reply_Codes),
   (  catch(term_to_atom(Reply_Term, Reply_Atom), _, fail)

   -> (  % NB must be synchronized 
         % with allowed_answer_header/1
         memberchk(protocol(http), Reply_Term),
         memberchk(peer(ip(_, _, _, _)), Reply_Term),
         memberchk(method(get), Reply_Term),
         memberchk(request_uri('/'), Reply_Term),
         memberchk(path('/'), Reply_Term),
         memberchk(http_version(_), Reply_Term)
      -> Status = almost_good
      ;  Status = questionary
      ),
      Reply = Reply_Term
   ;
      Reply = Reply_Atom, Status = bad
   ).


% proxies.my-proxy.com source

source_page('http://proxies.my-proxy.com/').

source_page(Url) :-

        between(2, 10, N),
        format(atom(Url),
               'http://proxies.my-proxy.com/proxy-list-~d.html',
               [N]).

download_ip_list(DB_Key) :-

        browser_headers(iceweasel, Headers),
        source_page(Url),
        get_time(Time_Stamp),
        catch(
              http_page(Url,
                        http_ops:http_get_html(Headers, _),
                        Page),
              _,
              fail),
        obj_field(Page, dom, DOM),
        find_ip(DOM, IP:Port),
        obj_construct(http_proxy_v,
                      [ip, port, source, dnl_time],
                      [IP, Port, Url, Time_Stamp],
                      Proxy_Obj),
        db_put_object(DB_Key, Proxy_Obj,
                      [ignore] % skip addition if present
                     ),
        fail
        ;
        true.

%
% On bt return all ip:port pairs found in DOM
%

find_ip([H], IP_Port) :-

        find_ip(H, IP_Port).

find_ip([H|T], IP_Port) :-

        find_ip(H, IP_Port),
        find_ip(T, IP_Port).

find_ip(element(_, _, List), IP_Port) :-

        member(M, List),
        find_ip(M, IP_Port).
        
find_ip(Atom, IP:Port) :-

        atom(Atom),
        atom_codes(Atom, Codes),
        phrase(ip_element(IP, Port), Codes).

ip_element(IP, Port) -->

        blanks, ipv4_and_port(IP, Port), blanks.

%%%%%%%%%%%%%%%%%%%%%%%%%
% The internal http server
%

mirror_request(Request) :-
        
   format('Content-type: text/plain~n~n'),
   include(allowed_answer_header, Request, Headers),
   writeq(Headers).

allowed_answer_header(H) :-

     H = protocol(_)
   ; H = peer(_)
   ; H = method(_)
   ; H = request_uri(_)
   ; H = path(_)
   ; H = http_version(_)
   .
