/**

URI           = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":"
(
1*("//" [ userinfo "@" ] host [ ":" port ] *( "/" *pchar ))
/ ("/" [ 1*pchar *( "/" *pchar ) ])
/ (1*pchar *( "/" *pchar ))
)
[ "?" *( pchar / "/" / "?" ) ] 
[ "#" *( pchar / "/" / "?" ) ]

*/

/*

host |-> :- "$scheme:$hier_part(\?$query)?

*/