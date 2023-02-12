% Template for parsing html lists (<li> -> object)

:- module(find_all_list_rows,
					[find_all_list_rows/6,
					 reparse/1
					]).

:- use_module(u(vd)).
:- use_module(u(parser/html/html_page_parse)).
:- use_module(u(parser/html/html_page_find)).
:- use_module(u(parser/html/html_tag_li_v_parse)).


:- meta_predicate find_all_list_rows(+, +, 1, +, +, -).

find_all_list_rows(DB_Key, Page, Mod:Test_Pred, Class, Filter, NFound) :-

    % extract all <li> from the page
    atom_concat(DB_Key, '.all_list_items', DB2_Key),
    db_clear(DB2_Key),
    html_page_parse(DB2_Key,
                    Page,
                    [html_tag_li_v],
										Filter),

    dump_db(DB2_Key),

    % find lists
    atom_concat(DB_Key, '.matched_list_items', DB3_Key),
    db_clear(DB3_Key),
    html_page_find(DB2_Key, DB3_Key, html_tag_li_v,
                   % treat a list as a matched list
                   % if it contains these columns
                   Mod:Test_Pred),

    dump_db(DB3_Key),
		db_size(DB3_Key, NFound),

		atom_concat(DB_Key, '.orig', DB4_Key),

    % concat all found lists and make objects
    (
     db_recorded(DB3_Key, List_Item),
     html_tag_li_v_parse(List_Item, Class, Object0),
		 ( obj_field(Object0, fail, name, _) -> true
		 ; write_log(['Object ', Object0, ' has no a book name']), fail
		 ),
		 db_put_object(DB4_Key, fail, Object0, _),
		 obj_downcast(Object0, Object),
		 db_put_object(DB_Key, Object, _),
		 fail
		;
		 true
		).



reparse(DB_Key) :-
		atom_concat(DB_Key, '.orig', DB4_Key),

		db_clear(DB_Key),
		
    (
     db_recorded(DB4_Key, Object0),
		 obj_reset_fields([db_ref, db_key, db_class], Object0, Object1),
		 obj_downcast(Object1, Object),
		 db_put_object(DB_Key, Object, _),
		 fail
		;
		 true
		),

    dump_db(DB_Key).

