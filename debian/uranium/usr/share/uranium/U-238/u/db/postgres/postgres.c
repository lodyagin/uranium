/* File:      postgres.c
** Author(s): Hákun Skarðhamar
** Contact:   hakun.skardhamar@mail.dk
**
** Copyright (C) Hákun Skarðhamar, 2001
**
** This program is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free Software
** Foundation; either version 2 of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
** PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.
**
** You should have received a copy of the GNU Library General Public License along
** with oracle.pl; if not, write to the Free Software Foundation, Inc.,
** 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
*/


#include <SWI-Prolog.h>
#include <stdio.h>
#include "libpq-fe.h"
#include <stdlib.h>
#include <ctype.h>

struct connection {
	char	*name;
	PGconn	*con;
};

struct connection cons[50];

foreign_t
pgCONNECT(term_t connstring, term_t connection)
{
	int		i;
	PGconn		*conn;
	PGresult	*res;
	char *s1, *s2;

        if ( !PL_get_atom_chars(connstring, &s1) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
        if ( !PL_get_atom_chars(connection, &s2) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
	
	i=0;
   	while(i < 50 && cons[i].name != s2){i++;}
	
	if (i < 50) {
		printf("pgCONNECT/2: Connection %s allready open\n",s2);
		return 1;
	}
	
	i=0;
	while(i < 50 && cons[i].name != s2){i++;}
	if (i < 50) {
		return PL_warning("pgCONNECT/2: no more connections available");
        }

	i=0;
	while(i < 50 && cons[i].name != NULL){i++;}
	conn = cons[i].con;
	conn = PQconnectdb(s1);
	if (PQstatus(conn) == CONNECTION_BAD) {
		printf("pgCONNECT/2: CONNECTION_BAD\n");
                return 0;
	} else {
		i = 0;
		while(i<50 && cons[i].name){i++;}
		cons[i].name = s2;
		cons[i].con = conn;
		return 1;
	}
}

foreign_t
pgDISCONNECT(term_t connection)
{
	int 	i;
	PGconn	*conn;
	char *s;
	
        if ( !PL_get_atom_chars(connection, &s) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
	
	i=0;
   	while(i < 50 && cons[i].name != s){i++;}
	if (i == 50) {
		printf("pgCONNECT/2: No connection %s exist\n",s);
		return 0;
	}
	
	conn = cons[i].con;
	if (PQstatus(conn) == CONNECTION_BAD){
		return PL_warning("pgCONNECT/2: CONNECTION_BAD");
	} else {
		PQfinish(conn);
		cons[i].name = NULL;
		return 1;
	}
}

foreign_t
pgTYPES(term_t connection, term_t result) {
	PGconn		*conn;
	PGresult   	*res;
	term_t		el1 = PL_new_term_ref();
	term_t		el2 = PL_new_term_ref();
	term_t		term = PL_new_term_ref();
	term_t		list = PL_new_term_ref();
	int 		i;
	char		*c;
	char 		s[100],name[20];

        if ( !PL_get_atom_chars(connection, &c) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
	
	i=0;
	while(i < 50 && cons[i].name != c){i++;}
	if (i == 50) {
		printf("pgCONNECT/2: No connection %s exist\n",s);
		return 0;
	}
	else conn = cons[i].con;

	if (PQstatus(conn) == CONNECTION_BAD){
		return PL_warning("pgCONNECT/2: CONNECTION_BAD");
	} else {
	        sprintf(s,"select typname,typelem from pg_type where typtype = 'b' and typname ~ '^_'");
		res = PQexec(conn, s);

		if (PQresultStatus(res) == PGRES_TUPLES_OK) {
		/* PGRES_TUPLES_OK -- The query successfully executed */
			PL_put_nil(list);
			for (i = PQntuples(res)-1; i >= 0 ; i--) {
				strcpy(name,PQgetvalue(res,i,0));
				PL_put_atom_chars(el1, &name[1]);
				PL_put_integer(el2, atol(PQgetvalue(res,i,1)));
				PL_cons_functor(term, PL_new_functor(PL_new_atom("/"), 2), el1, el2);
				PL_cons_list(list, term, list);
			}
		}	
		PQclear(res);
		return PL_unify(result, list);			
	}
}

foreign_t
pgTABLE(term_t connection, term_t sql, term_t result)
{
	PGconn		*conn;
	PGresult   	*res;
	int 		functor, nFields, i, j;
	char 		s[100];
	char		*c, *tab;
	term_t		el1 = PL_new_term_ref();
	term_t		el2 = PL_new_term_ref();
	term_t		term = PL_new_term_ref();
	term_t		list = PL_new_term_ref();

        if ( !PL_get_atom_chars(connection, &c) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
        if ( !PL_get_atom_chars(sql, &tab) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
	
	i=0;
	while(i < 50 && cons[i].name != c){i++;}
	if (i == 50) {
		printf("pgCONNECT/2: No connection %s exist\n",s);
		return 0;
	}
	else conn = cons[i].con;

	if (PQstatus(conn) == CONNECTION_BAD){
		return PL_warning("pgCONNECT/2: CONNECTION_BAD");
	} else {
		sprintf(s,"select * from %s limit 1",tab);
		res = PQexec(conn, s);

		if (PQresultStatus(res) == PGRES_TUPLES_OK) {
		/* PGRES_TUPLES_OK -- The query successfully executed */
			PL_put_nil(list);
			nFields = PQnfields(res);
			for (i = nFields-1; i >= 0 ; i--) {
				PL_put_atom_chars(el1, PQfname(res, i));
				PL_put_integer(el2, PQftype(res,i));
				PL_cons_functor(term, PL_new_functor(PL_new_atom("/"), 2), el1, el2);
				PL_cons_list(list, term, list);
		}	
		PQclear(res);
		return PL_unify(result, list);			
		} else {
		PQclear(res);
		return 0;
		}
	}
}

foreign_t
pgSQL(term_t connection, term_t command, term_t result)
{
	PGconn		*conn;
	PGresult  	*res;
	int 		nFields, i, j, null;
	char		*c, *sql;
	term_t		el1 = PL_new_term_ref();
	term_t		row = PL_new_term_ref();
	term_t		rows = PL_new_term_ref();
	term_t		x = PL_new_term_ref();

        if ( !PL_get_atom_chars(connection, &c) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
        if ( !PL_get_chars(command, &sql, 
                           CVT_ALL | BUF_MALLOC | REP_UTF8) )
 		return PL_warning("pgCONNECT/2: instantiation fault");
	
	i=0;
	while(i < 50 && cons[i].name != c){i++;}
	if (i == 50) {
		printf("pgCONNECT/2: No connection %s exist\n",c);
		PL_free(sql);
		return 0;
	}
	else conn = cons[i].con;

	if (PQstatus(conn) == CONNECTION_BAD){
		return PL_warning("pgCONNECT/2: CONNECTION_BAD");
	} else {
		PL_put_nil(rows);
		res = PQexec(conn, sql);
		if (PQresultStatus(res) == PGRES_FATAL_ERROR) {
  		        printf("%s\n", PQerrorMessage(conn));
			PQclear(res);
			return PL_warning("pgCONNECT/2: PGRES_FATAL_ERROR");
		}
		if (PQresultStatus(res) == PGRES_NONFATAL_ERROR) {
			PQclear(res);	
			return PL_warning("pgCONNECT/2: PGRES_NONFATAL_ERROR");
		}
		if (PQresultStatus(res) == PGRES_BAD_RESPONSE) {
			PQclear(res);
			return PL_warning("pgCONNECT/2: PGRES_BAD_RESPONSE");
		}
		if (PQresultStatus(res) == PGRES_EMPTY_QUERY) {
		/* PGRES_EMPTY_QUERY -- The string sent to the backend was empty. */
			PQclear(res);
			return PL_warning("pgCONNECT/2: PGRES_EMPTY_QUERY");
		}
		if (PQresultStatus(res) == PGRES_COMMAND_OK) {
		/* PGRES_COMMAND_OK -- Successful completion of a command returning no data */
			PQclear(res);
			PL_free(sql);
			return PL_unify(result, rows);			
		}

		if (PQresultStatus(res) == PGRES_TUPLES_OK) {
		/* PGRES_TUPLES_OK -- The query successfully executed */
			nFields = PQnfields(res);
			for (i = PQntuples(res)-1; i >= 0 ; i--)
 			{
				PL_put_nil(row);
				for (j = nFields-1; j >= 0 ; j--) {
					null = PQgetisnull(res, i, j);
					if (null == 1) {
						PL_put_variable(x);
						PL_cons_functor(el1, PL_new_functor(PL_new_atom("null"),1), x);
					} else {
						PL_put_atom_chars(el1, PQgetvalue(res, i, j));
					}	
					PL_cons_list(row, el1, row);
				}	
				PL_cons_list(rows, row, rows);
			}
			PQclear(res);
			PL_free(sql);
			return PL_unify(result, rows);			
		}
		PQclear(res);
		PL_free(sql);
		return 0;
	}
}

install_t
install()
{ PL_register_foreign("pgDISCONNECT", 1, pgDISCONNECT, 0);
  PL_register_foreign("pgCONNECT", 2, pgCONNECT, 0);
  PL_register_foreign("pgTYPES", 2, pgTYPES, 0);
  PL_register_foreign("pgTABLE", 3, pgTABLE, 0);
  PL_register_foreign("pgSQL", 3, pgSQL, 0);
}


